SELECTEDCOUNT <- "count"  # Change as needed
DIFFPERCENT <- 10       # Threshold: actual volume differs from predicted by more than ± DIFFPERCENT (e.g. 10%)
RUN_DISTRICT_MODEL <- FALSE  # Set to FALSE to skip admin_area_3-level (e.g. district or LGA) regression

#-------------------------------------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Mar 28
# Module: SERVICE UTILIZATION


# This script analyzes disruptions in essential health services using adjusted HMIS data (M2_adjusted_data.csv). 
# It has two main components:
#   1. Control Chart Analysis: Identifies whether deviations in service volumes are part of normal fluctuations 
#      or indicate significant disruptions.
#   2. Disruption Analysis: Quantifies the impact of these disruptions by measuring how service volumes changed 
#      during flagged periods.


# Ce code analyse les perturbations des services de santé essentiels à partir des données HMIS ajustées.
# Il comprend deux parties principales :
#   1. Analyse des cartes de contrôle : Détermine si les écarts dans les volumes de services relèvent de fluctuations normales 
#      ou signalent des perturbations importantes.
#   2. Analyse des perturbations : Quantifie l'impact des perturbations en mesurant les variations des volumes 
#      de services pendant les périodes signalées.

# ------------------------------------- KEY OUTPUTS ----------------------------------------------------------
# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.
# FILE: indicator_results.csv           # Indicator-level trends with control limits.
# FILE: M3_chartout.csv                 # Filtered dataset of flagged disruptions only.
# FILE: M3_disruptions_analysis.csv     # Outputs from the disruption analysis.

#-------------------------------------------------------------------------------------------------------------
# Load required libraries
library(lubridate)
library(zoo)
library(readxl)
library(fixest)  # For panel regressions (alternative to 'xtreg' in Stata)
library(stringr)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------
# STEP 1: CONTROL CHART ANALYSIS
#-------------------------------------------------------------------------------------------------------------
print("Loading data for control chart analysis...")
data_utilization <- read.csv("M2_adjusted_data_admin_area.csv")        # adjusted data aggregated at the lowest geo level, used for visualization
data <- read.csv("M1_output_outliers.csv")                             # data with outliers tagged 


print("Data loaded. Cleaning and filtering...")

data <- data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  filter(outlier_flag != 1) %>%
  drop_na(!!sym(SELECTEDCOUNT))

print("Grouping by panel (indicator + admin area)...")

data <- data %>%
  group_by(indicator_common_id, admin_area_2) %>%
  mutate(panelvar = cur_group_id()) %>%
  ungroup()

print("Aggregating data to province level...")

province_data <- data %>%
  group_by(panelvar, indicator_common_id, admin_area_1, admin_area_2, date) %>%
  summarise(count = sum(!!sym(SELECTEDCOUNT), na.rm = TRUE), .groups = "drop") %>%
  rename(count_original = count)

print("Filling missing months and metadata...")

province_data <- province_data %>%
  group_by(panelvar) %>%
  complete(date = seq(min(date), max(date), by = "month")) %>%
  ungroup() %>%
  group_by(panelvar) %>%
  fill(indicator_common_id, admin_area_1, admin_area_2, .direction = "downup") %>%
  ungroup()

print("Removing months with extremely low counts...")

province_data <- province_data %>%
  group_by(panelvar) %>%
  mutate(globalmean = mean(count_original, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(count = ifelse(count_original / globalmean < 0.5, NA, count_original))

print("Interpolating missing/removed values for modeling...")

province_data <- province_data %>%
  group_by(panelvar) %>%
  arrange(date) %>%
  mutate(count = zoo::na.approx(count, na.rm = FALSE, maxgap = Inf, rule = 2)) %>%
  ungroup()

print("Running robust control chart analysis for each panel...")

robust_control_chart <- function(panel_data, selected_count) {
  panel_data <- panel_data %>% mutate(month_factor = factor(month(date)))
  
  if (sum(!is.na(panel_data[[selected_count]])) >= 12) {
    mod <- rlm(as.formula(paste(selected_count, "~ month_factor + as.numeric(date)")), data = panel_data, maxit = 100)
    panel_data <- panel_data %>% mutate(count_predict = predict(mod, newdata = panel_data))
  } else {
    panel_data <- panel_data %>% mutate(count_predict = median(panel_data[[selected_count]], na.rm = TRUE))
  }
  
  panel_data <- panel_data %>%
    arrange(date) %>%
    mutate(count_smooth = zoo::rollmedian(count_predict, k = SMOOTH_K, fill = NA, align = "center"),
           count_smooth = ifelse(is.na(count_smooth), count_predict, count_smooth))
  
  # Compare against raw counts
  panel_data <- panel_data %>%
    mutate(residual = count_original - count_smooth)
  
  mad_resid <- mad(panel_data$residual, constant = 1, na.rm = TRUE)
  panel_data <- panel_data %>% mutate(robust_control = residual / (mad_resid + 1e-6))
  
  panel_data <- panel_data %>%
    mutate(tag_sharp = ifelse(!is.na(robust_control) & abs(robust_control) >= THRESHOLD, 1, 0))
  
  panel_data <- panel_data %>%
    mutate(
      mild_flag = ifelse(!is.na(robust_control) & abs(robust_control) >= 1 & abs(robust_control) < THRESHOLD, 1, 0),
      mild_cumulative = zoo::rollapply(mild_flag, width = 3, align = "right", fill = NA, FUN = sum, na.rm = TRUE),
      tag_sustained = ifelse(mild_cumulative >= 3 & abs(robust_control) >= 1.5, 1, 0)
    )
  
  # Tag "dips" in the data
  panel_data <- panel_data %>%
    mutate(dip_flag = ifelse(is.na(count_original) | count_original < DIP_THRESHOLD * count_smooth, 1, 0))   # Updated line with parameterized threshold
  
  dip_rle <- rle(panel_data$dip_flag)
  dip_tag <- inverse.rle(with(dip_rle, list(lengths = lengths, values = ifelse(values == 1 & lengths >= 3, 1, 0))))
  panel_data$tag_sustained_dip <- dip_tag
  
  panel_data <- panel_data %>%
    mutate(
      is_missing = is.na(count_original) | count_original == 0,
      missing_roll = zoo::rollapply(is_missing, width = 3, align = "right", fill = NA, FUN = sum, na.rm = TRUE),
      tag_missing = ifelse(missing_roll >= 2, 1, 0)
    )
  
  # Tag "rises" in the data
  panel_data <- panel_data %>%
    mutate(rise_flag = ifelse(!is.na(count_original) & count_original > RISE_THRESHOLD * count_smooth, 1, 0))
  
  rise_rle <- rle(panel_data$rise_flag)
  rise_tag <- inverse.rle(with(rise_rle, list(lengths = lengths, values = ifelse(values == 1 & lengths >= 3, 1, 0))))
  panel_data$tag_sustained_rise <- rise_tag
  
  
  
  # Final tagging
  
  panel_data <- panel_data %>%
    mutate(tagged = case_when(
      tag_sharp == 1 |
        tag_sustained == 1 |
        tag_sustained_dip == 1 |
        tag_sustained_rise == 1 |
        tag_missing == 1 ~ 1,
      TRUE ~ 0
    ))
  
  # NAs in 'tagged' are replaced with 0
  panel_data$tagged[is.na(panel_data$tagged)] <- 0
  
  return(panel_data)
}

# Run for all panels
panel_list <- unique(province_data$panelvar)

results_list <- list()
for(panel in panel_list) {
  print(paste0("Processing panel ID: ", panel))
  panel_data <- province_data %>% filter(panelvar == panel)
  panel_results <- robust_control_chart(panel_data, "count")
  results_list[[as.character(panel)]] <- panel_results
}

final_results <- bind_rows(results_list)

M3_chartout <- final_results
print("Control chart analysis complete")

#-------------------------------------------------------------------------------------------------------------
# STEP 2: DISRUPTION REGRESSION ANALYSIS (INDICATOR LEVEL + PROVINCE LEVEL + DISTRICT LEVEL)
#-------------------------------------------------------------------------------------------------------------
# Step 1: Load and Prepare Data
print("Loading and preparing data for disruption analysis...")
# Select only necessary columns from M3_chartout to avoid duplication

M3_chartout_selected <- final_results %>%
  select(date, indicator_common_id, admin_area_2, tagged)  # Keep only relevant columns

# Merge without duplicating columns
data_disruption <- data %>%
  left_join(M3_chartout_selected, by = c("date", "indicator_common_id", "admin_area_2")) %>%
  mutate(tagged = replace_na(tagged, 0))  # Ensure missing tagged values are set to 0

# Step 2: Run Panel Regressions
print("Running panel regressions...")
indicators <- unique(data_disruption$indicator_common_id)
districts <- unique(data_disruption$admin_area_3)
provinces <- unique(data_disruption$admin_area_2)


# Step 4a: Run Regression for Each Indicator -----------------------------------
print("Running regressions at the indicator level...")

indicator_results_list <- list()  # Store results for each indicator

for (indicator in indicators) {
  print(paste("Processing:", indicator))
  
  indicator_data <- data_disruption %>%
    filter(indicator_common_id == indicator) %>%
    drop_na(!!sym(SELECTEDCOUNT))
  
  if (nrow(indicator_data) == 0) {
    print(paste("Skipping", indicator, "- insufficient data"))
    next
  }
  
  model <- tryCatch(
    feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), 
          data = indicator_data, 
          cluster = as.formula(paste0("~", lowest_geo_level))),
    error = function(e) {
      print(paste("Regression failed for:", indicator, "Error:", e$message))
      return(NULL)
    }
  )
  
  if (!is.null(model) && !anyNA(coef(model))) {
    indicator_data <- indicator_data %>%
      mutate(expect_admin_area_1 = predict(model, newdata = indicator_data))
    
    
    
    
    disruption_effect <- coef(model)["tagged"]
    
    
    indicator_data <- indicator_data %>%
      mutate(expect_admin_area_1 = predict(model, newdata = indicator_data),
             expect_admin_area_1 = ifelse(tagged == 1,
                                          expect_admin_area_1 - disruption_effect,
                                          expect_admin_area_1))
    
    
    
    indicator_data <- indicator_data %>%
      group_by(date) %>%
      mutate(
        diff = expect_admin_area_1 - !!sym(SELECTEDCOUNT),
        diffmean = mean(diff, na.rm = TRUE),
        predictmean = mean(expect_admin_area_1, na.rm = TRUE),
        b_admin_area_1 = ifelse(tagged == 1, -1 * (diffmean / predictmean), NA_real_),
        b_trend_admin_area_1 = coef(model)["date"]
      ) %>%
      ungroup()
    
    # compute p-value for "tagged" effect
    if ("tagged" %in% names(coef(model))) {
      p_val_1 <- 2 * pt(abs(coef(model)["tagged"] / sqrt(diag(vcov(model)))["tagged"]),
                        df.residual(model), lower.tail = FALSE)
      indicator_data <- indicator_data %>%
        mutate(p_admin_area_1 = p_val_1)
    }
    
    # Store processed data in list
    indicator_results_list[[indicator]] <- indicator_data
  }
  
}

# Combine all indicator-level results
indicator_results_long <- bind_rows(indicator_results_list)

# Merge indicator-level results into main dataset
data_disruption <- data_disruption %>%
  left_join(indicator_results_long %>%
              select(facility_id, date, indicator_common_id, 
                     expect_admin_area_1, b_admin_area_1, b_trend_admin_area_1, p_admin_area_1),
            by = c("facility_id", "date", "indicator_common_id"))



print("Indicator-level regression complete.")



# Step 4b: Run Regression for Each Indicator × Province ------------------------
print("Running regressions at the province level...")

province_results_list <- list()  # Initialize list for storing results

for (indicator in indicators) {
  for (province in provinces) {
    print(paste("Processing:", indicator, "in province:", province))
    
    province_data <- data_disruption %>%
      filter(indicator_common_id == indicator, admin_area_2 == province) %>%
      drop_na(!!sym(SELECTEDCOUNT))
    
    if (nrow(province_data) < 1) {  
      print(paste("Skipping", indicator, "in", province, "- insufficient data"))
      next
    }
    
    # Run province-level regression dynamically
    model_province <- tryCatch(
      feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
            data = province_data,
            cluster = ~admin_area_3),
      error = function(e) {
        print(paste("Regression failed for:", indicator, "in", province, "Error:", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(model_province) && !anyNA(coef(model_province))) {
      province_data <- province_data %>%
        mutate(expect_admin_area_2 = predict(model_province, newdata = province_data))
      
      for (pmonth in unique(province_data$date[province_data$tagged == 1])) {
        coeff <- coef(model_province)["tagged"]
        province_data <- province_data %>%
          mutate(expect_admin_area_2 = ifelse(date == pmonth, expect_admin_area_2 - coeff, expect_admin_area_2))
      }
      
      province_data <- province_data %>%
        group_by(date) %>%
        mutate(
          diff = expect_admin_area_2 - !!sym(SELECTEDCOUNT),
          diffmean = mean(diff, na.rm = TRUE),
          predictmean = mean(expect_admin_area_2, na.rm = TRUE),
          b_admin_area_2 = ifelse(tagged == 1, -1 * (diffmean / predictmean), NA_real_)
        ) %>%
        ungroup()
      
      if ("tagged" %in% names(coef(model_province))) {
        p_val <- 2 * pt(abs(coef(model_province)["tagged"] / sqrt(diag(vcov(model_province)))["tagged"]), 
                        df.residual(model_province), lower.tail = FALSE)
        province_data <- province_data %>%
          mutate(p_admin_area_2 = p_val)
      }

            if ("date" %in% names(coef(model_province))) {
        province_data <- province_data %>%
          mutate(b_trend_admin_area_2 = coef(model_province)["date"])
      }
      
      # Store results
      province_results_list[[paste(indicator, province, sep = "_")]] <- province_data
    
    }
  }
}

# Combine all province-level results into one dataframe
province_results_long <- bind_rows(province_results_list)

# Merge province-level results into main dataset
data_disruption <- data_disruption %>%
  left_join(province_results_long %>% 
              select(facility_id, date, indicator_common_id, 
                     expect_admin_area_2, b_admin_area_2, p_admin_area_2, b_trend_admin_area_2),
            by = c("facility_id", "date", "indicator_common_id"))

print("Province-level regression complete.")




# Step 4b: Run Regression for Each Indicator × District ------------------------
if (RUN_DISTRICT_MODEL) {
  
  print("Running regressions at the district level...")
  
  district_results_list <- list()  # Store results for each indicator-district pair
  
  for (indicator in indicators) {
    for (district in districts) {
      print(paste("Processing:", indicator, "in district:", district))
      
      district_data <- data_disruption %>%
        filter(indicator_common_id == indicator, admin_area_3 == district) %>%
        drop_na(!!sym(SELECTEDCOUNT), tagged, date)
      
      if (nrow(district_data) < 10) {
        print(paste("Skipping", indicator, "in", district, "- insufficient data"))
        next
      }
      
      model_district <- tryCatch(
        feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
              data = district_data,
              cluster = as.formula(paste0("~", lowest_geo_level))),
        error = function(e) {
          print(paste("Regression failed for:", indicator, "in", district, "Error:", e$message))
          return(NULL)
        }
      )
      
      if (!is.null(model_district) && !anyNA(coef(model_district))) {
        district_data <- district_data %>%
          mutate(expect_admin_area_3 = predict(model_district, newdata = district_data))
        
        for (pmonth in unique(district_data$date[district_data$tagged == 1])) {
          coeff <- coef(model_district)["tagged"]
          district_data <- district_data %>%
            mutate(expect_admin_area_3 = ifelse(date == pmonth, expect_admin_area_3 - coeff, expect_admin_area_3))
        }
        
        district_data <- district_data %>%
          group_by(date) %>%
          mutate(
            diff = expect_admin_area_3 - !!sym(SELECTEDCOUNT),
            diffmean = mean(diff, na.rm = TRUE),
            predictmean = mean(expect_admin_area_3, na.rm = TRUE),
            b_admin_area_3 = ifelse(tagged == 1, -1 * (diffmean / predictmean), NA_real_)
          ) %>%
          ungroup()
        
        if ("tagged" %in% names(coef(model_district))) {
          p_admin_area_3 <- 2 * pt(
            abs(coef(model_district)["tagged"] / sqrt(diag(vcov(model_district)))["tagged"]),
            df.residual(model_district),
            lower.tail = FALSE
          )
          
          district_data <- district_data %>%
            mutate(p_admin_area_3 = p_admin_area_3)
        }
        
        district_results_list[[paste(indicator, district, sep = "_")]] <- district_data
      }
    }
  }
  
  district_results_long <- bind_rows(district_results_list)
  
  print("District-level regression analysis complete!")
  
  data_disruption <- data_disruption %>%
    left_join(
      district_results_long %>%
        select(facility_id, date, indicator_common_id, admin_area_3,
               expect_admin_area_3, b_admin_area_3, p_admin_area_3),
      by = c("facility_id", "date", "indicator_common_id", "admin_area_3")
    )
  
  print("District-level regression results merged into main dataset.")
}





#-------------------------------------------------------------------------------------------------------------
# STEP 3: PREPARE RESULTS FOR VISUALIZATION
#-------------------------------------------------------------------------------------------------------------
# Convert period_id first
data_disruption <- data_disruption %>%
  mutate(period_id = format(as.Date(date), "%Y%m"))

summary_disruption_admin1 <- data_disruption %>%
  group_by(admin_area_1, period_id, indicator_common_id) %>%
  mutate(num = n()) %>%
  summarise(
    count_original = mean(count, na.rm = TRUE),
    count_expect = mean(expect_admin_area_1, na.rm = TRUE),
    b = mean(b_admin_area_1, na.rm = TRUE),
    p = mean(p_admin_area_1, na.rm = TRUE),
    num_obs = mean(num),
    count_expect_sum = sum(expect_admin_area_1, na.rm = TRUE),
    count_sum = sum(count, na.rm = TRUE),
    diff_percent = 100 * (count_expect - count) / count_expect,
    diff_percent_sum = 100 * (count_expect_sum - count_sum) / count_expect_sum,
    count_expect_diff_sum = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect_sum, count_sum),
    count_expect_diff = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect, count),
    .groups = "drop"
  )

summary_disruption_admin2 <- data_disruption %>%
  group_by(admin_area_2, period_id, indicator_common_id) %>%
  mutate(num = n()) %>%
  summarise(
    count_original = mean(count, na.rm = TRUE),
    count_expect = mean(expect_admin_area_1, na.rm = TRUE),
    b = mean(b_admin_area_2, na.rm = TRUE),
    p = mean(p_admin_area_2, na.rm = TRUE),
    num_obs = mean(num),
    count_expect_sum = sum(expect_admin_area_2, na.rm = TRUE),
    count_sum = sum(count, na.rm = TRUE),
    diff_percent = 100 * (count_expect - count) / count_expect,
    diff_percent_sum = 100 * (count_expect_sum - count_sum) / count_expect_sum,
    count_expect_diff_sum = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect_sum, count_sum),
    count_expect_diff = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect, count),
    .groups = "drop"
  )


if (RUN_DISTRICT_MODEL) {
  summary_disruption_admin3 <- data_disruption %>%
    group_by(admin_area_3, period_id, indicator_common_id) %>%
    mutate(num = n()) %>%
    summarise(
      count_original = mean(count, na.rm = TRUE),
      count_expect = mean(expect_admin_area_1, na.rm = TRUE),
      b = mean(b_admin_area_3, na.rm = TRUE),
      p = mean(p_admin_area_3, na.rm = TRUE),
      num_obs = mean(num),
      count_expect_sum = sum(expect_admin_area_3, na.rm = TRUE),
      count_sum = sum(count, na.rm = TRUE),
      diff_percent = 100 * (count_expect - count) / count_expect,
      diff_percent_sum = 100 * (count_expect_sum - count_sum) / count_expect_sum,
      count_expect_diff_sum = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect_sum, count_sum),
      count_expect_diff = ifelse(abs(diff_percent) > DIFFPERCENT, count_expect, count),
      .groups = "drop"
    )
}


# Step 5: Save Outputs ----------------------------------------------------------
print("Saving results...")
write.csv(data, "M3_service_utilization.csv", row.names = FALSE)
write.csv(M3_chartout, "M3_chartout.csv", row.names = FALSE)
write.csv(summary_disruption_admin1, "M3_disruptions_analysis_admin_area_1.csv", row.names = FALSE) 
write.csv(summary_disruption_admin2, "M3_disruptions_analysis_admin_area_2.csv", row.names = FALSE) 
if (RUN_DISTRICT_MODEL) {
  write.csv(summary_disruption_admin3, "M3_disruptions_analysis_admin_area_3.csv", row.names = FALSE)
}