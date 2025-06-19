SELECTEDCOUNT <- "count_final_completeness"  #use count_final_none or count_final_completeness
VISUALIZATIONCOUNT <- "count_final_outliers" 

SMOOTH_K <- 7                        # Window size (in months) for rolling median smoothing of predicted counts.
                                     # Used in the control chart to reduce noise in trend estimation. MUST BE ODD

THRESHOLD <- 1.5                     # Threshold (in MAD units) for detecting sharp deviations in robust control chart.
                                     # If residual/MAD > THRESHOLD, the month is flagged as a sharp disruption.

DIP_THRESHOLD <- 0.90                # Threshold for dips: a month is flagged if actual count falls below
                                     # 90% of the smoothed expected volume (i.e., a ≥10% drop).
                                     # Set to 0.80 for a more conservative detection -> to flag big drops.

RISE_THRESHOLD <- 1 / DIP_THRESHOLD  # Threshold for rises: a month is flagged if actual count exceeds
                                     # ~111% of expected volume (i.e., a ≥10% rise). Mirrors the dip logic.

DIFFPERCENT <- 10                    # Difference threshold (in percent): if the actual volume differs from the predicted
                                     # volume by more than ±10%, use the predicted value in plotting disruptions.

RUN_DISTRICT_MODEL <- FALSE          # Set to TRUE to run regressions at the lowest geographic level (admin_area_3).
                                     # Set to FALSE for faster runtime.



PROJECT_DATA_HMIS <- "ethiopia_hmis_data.csv"
#-------------------------------------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 June 4
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
library(MASS)    # For the rlm() function >> robust regression
library(fixest)  # For panel regressions (alternative to 'xtreg' in Stata)
library(stringr)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------
# STEP 1: CONTROL CHART ANALYSIS
#-------------------------------------------------------------------------------------------------------------
print("Loading data for control chart analysis...")
raw_data <- read.csv(PROJECT_DATA_HMIS)
outlier_data <- read.csv("M1_output_outliers.csv")
data_utilization <- read.csv("M2_adjusted_data_admin_area.csv")        # adjusted data aggregated at the lowest geo level, used for visualization
data <- read.csv("M2_adjusted_data.csv")                               # data with outliers tagged 
data_adjusted <- data


print("Preparing data for province-level control chart analysis...")

data <- data %>%
  left_join(
    outlier_data[, c("facility_id", "indicator_common_id", "period_id", "outlier_flag")],
    by = c("facility_id", "indicator_common_id", "period_id")
  ) %>%
  mutate(
    outlier_flag = coalesce(outlier_flag, 0L),
    year = as.integer(substr(period_id, 1, 4)),
    month = as.integer(substr(period_id, 5, 6)),
    date = as.Date(sprintf("%04d-%02d-01", year, month)),
    is_missing = is.na(count_final_none),
    count_model = .data[[SELECTEDCOUNT]]
  ) %>%
  filter(outlier_flag != 1)


print("Assigning panel IDs...")
data <- data %>%
  group_by(indicator_common_id, admin_area_2) %>%
  mutate(panelvar = cur_group_id()) %>%
  ungroup()


print("Aggregating data to province level...")
province_data <- data %>%
  group_by(indicator_common_id, admin_area_2, date) %>%
  summarise(count_model = sum(count_model, na.rm = TRUE), .groups = "drop") %>%
  group_by(indicator_common_id, admin_area_2) %>%
  mutate(panelvar = cur_group_id()) %>%
  ungroup() %>%
  rename(count_original = count_model)

print("Filling missing months and metadata...")
province_data <- province_data %>%
  group_by(panelvar) %>%
  complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month")) %>%
  fill(indicator_common_id, admin_area_2, .direction = "downup") %>%
  ungroup()

print("Removing months with extremely low counts...")
province_data <- province_data %>%
  group_by(panelvar) %>%
  mutate(
    globalmean = mean(count_original, na.rm = TRUE),
    count = ifelse(count_original / globalmean < 0.5, NA_real_, count_original)
  ) %>%
  ungroup()

print("Interpolating missing/removed values for modeling...")
province_data <- province_data %>%
  group_by(panelvar) %>%
  arrange(date) %>%
  mutate(
    count = zoo::na.approx(count, na.rm = FALSE, maxgap = Inf, rule = 2)
  ) %>%
  ungroup()

print("Running robust control chart analysis for each panel...")

# Function control chart
robust_control_chart <- function(panel_data, selected_count) {
  panel_data <- panel_data %>%
    mutate(month_factor = factor(month(date)))
  
  # Count non-missing obs and unique dates
  n_obs <- sum(!is.na(panel_data[[selected_count]]))
  n_dates <- length(unique(panel_data$date[!is.na(panel_data[[selected_count]])]))
  
  # Model fallback logic
  if (n_obs >= 12 && n_dates > 12) {
    # Safe to use full model
    mod <- tryCatch({
      rlm(as.formula(paste(selected_count, "~ month_factor + as.numeric(date)")),
          data = panel_data, maxit = 100)
    }, error = function(e) {
      warning(paste("Full model failed, fallback to trend-only. Error:", e$message))
      NULL
    })
  } else if (n_obs >= 12) {
    # Use simpler model (trend only)
    mod <- tryCatch({
      rlm(as.formula(paste(selected_count, "~ as.numeric(date)")),
          data = panel_data, maxit = 100)
    }, error = function(e) {
      warning(paste("Trend-only model failed. Error:", e$message))
      NULL
    })
  } else {
    mod <- NULL
  }
  
  # Predict or fallback to median
  panel_data <- panel_data %>%
    mutate(count_predict = if (!is.null(mod)) {
      predict(mod, newdata = panel_data)
    } else {
      median(panel_data[[selected_count]], na.rm = TRUE)
    })
  
  # Smoothing
  panel_data <- panel_data %>%
    arrange(date) %>%
    mutate(
      count_smooth = zoo::rollmedian(count_predict, k = SMOOTH_K, fill = NA, align = "center"),
      count_smooth = ifelse(is.na(count_smooth), count_predict, count_smooth)
    )
  
  # Residuals and MAD-based control limits
  panel_data <- panel_data %>%
    mutate(
      residual = count_original - count_smooth,
      robust_control = residual / (mad(residual, constant = 1, na.rm = TRUE) + 1e-6),
      tag_sharp = ifelse(!is.na(robust_control) & abs(robust_control) >= THRESHOLD, 1, 0),
      mild_flag = ifelse(!is.na(robust_control) & abs(robust_control) >= 1 & abs(robust_control) < THRESHOLD, 1, 0),
      mild_cumulative = zoo::rollapply(mild_flag, width = 3, align = "right", fill = NA, FUN = sum, na.rm = TRUE),
      tag_sustained = ifelse(mild_cumulative >= 3 & abs(robust_control) >= 1.5, 1, 0),
      dip_flag = ifelse(is.na(count_original) | count_original < DIP_THRESHOLD * count_smooth, 1, 0)
    )
  
  # Dips
  dip_rle <- rle(panel_data$dip_flag)
  panel_data$tag_sustained_dip <- inverse.rle(with(dip_rle, list(
    lengths = lengths,
    values = ifelse(values == 1 & lengths >= 3, 1, 0)
  )))
  
  # Missing and rise tagging
  panel_data <- panel_data %>%
    mutate(
      is_missing = is.na(count_original) | count_original == 0,
      missing_roll = zoo::rollapply(is_missing, width = 3, align = "right", fill = NA, FUN = sum, na.rm = TRUE),
      tag_missing = ifelse(missing_roll >= 2, 1, 0),
      rise_flag = ifelse(!is.na(count_original) & count_original > RISE_THRESHOLD * count_smooth, 1, 0)
    )
  
  rise_rle <- rle(panel_data$rise_flag)
  panel_data$tag_sustained_rise <- inverse.rle(with(rise_rle, list(
    lengths = lengths,
    values = ifelse(values == 1 & lengths >= 3, 1, 0)
  )))
  
  # Final tagging
  panel_data <- panel_data %>%
    mutate(
      tagged = case_when(
        tag_sharp == 1 |
          tag_sustained == 1 |
          tag_sustained_dip == 1 |
          tag_sustained_rise == 1 |
          tag_missing == 1 ~ 1,
        TRUE ~ 0
      ),
      tagged = replace_na(tagged, 0)
    ) %>%
    group_by(admin_area_2) %>%
    mutate(
      last_6_months = ifelse(date >= max(date) - months(6), 1, 0),
      tagged = ifelse(last_6_months == 1, 1, tagged)
    ) %>%
    ungroup()
  
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
  dplyr::select(date, indicator_common_id, admin_area_2, tagged)


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
  
  n_clusters <- indicator_data %>%
    distinct(admin_area_3) %>%
    nrow()
  
  model <- tryCatch(
    if (n_clusters > 1) {
      feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
            data = indicator_data,
            cluster = ~admin_area_3)
    } else {
      feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
            data = indicator_data)
    },
    error = function(e) {
      print(paste("Regression failed for:", indicator, "Error:", e$message))
      return(NULL)
    }
  )
  
  if (!is.null(model) && !anyNA(coef(model))) {
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
    
    # Compute p-value if clustering applied or fallback to regular SE
    if ("tagged" %in% names(coef(model))) {
      tagged_se <- sqrt(diag(vcov(model)))["tagged"]
      p_val_1 <- 2 * pt(abs(disruption_effect / tagged_se), df.residual(model), lower.tail = FALSE)
      
      indicator_data <- indicator_data %>%
        mutate(p_admin_area_1 = p_val_1)
    }
    
    # Save output
    indicator_results_list[[indicator]] <- indicator_data
  }
}


# Combine all indicator-level results
indicator_results_long <- dplyr::bind_rows(indicator_results_list)

# Merge indicator-level results into main dataset
data_disruption <- data_disruption %>%
  dplyr::left_join(
    indicator_results_long %>%
      dplyr::select(
        facility_id, date, indicator_common_id, 
        expect_admin_area_1, b_admin_area_1, 
        b_trend_admin_area_1, p_admin_area_1
      ),
    by = c("facility_id", "date", "indicator_common_id")
  )

print("Indicator-level regression complete.")



# Step 4b: Run Regression for Each Indicator × Province ------------------------
print("Running regressions at the province level...")

province_results_list <- list()  # Initialize list for storing results

province_results_list <- list()

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
    
    # Determine whether clustering is valid
    n_clusters <- province_data %>% pull(admin_area_3) %>% n_distinct(na.rm = TRUE)
    
    # Build regression formula
    reg_formula <- as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged"))
    
    # Fit model: clustered if >1 cluster, unclustered otherwise
    model_province <- tryCatch(
      if (n_clusters > 1) {
        feols(reg_formula, data = province_data, cluster = ~admin_area_3)
      } else {
        feols(reg_formula, data = province_data)
      },
      error = function(e) {
        print(paste("Regression failed for:", indicator, "in", province, "Error:", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(model_province) && !anyNA(coef(model_province))) {
      province_data <- province_data %>%
        mutate(expect_admin_area_2 = predict(model_province, newdata = province_data))
      
      if ("tagged" %in% names(coef(model_province)) &&
          any(province_data$tagged == 1, na.rm = TRUE)) {
        coeff <- coef(model_province)["tagged"]
        province_data <- province_data %>%
          mutate(expect_admin_area_2 = ifelse(tagged == 1, expect_admin_area_2 - coeff, expect_admin_area_2))
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
        p_val <- 2 * pt(abs(coef(model_province)["tagged"] /
                              sqrt(diag(vcov(model_province)))["tagged"]),
                        df.residual(model_province), lower.tail = FALSE)
        province_data <- province_data %>%
          mutate(p_admin_area_2 = p_val)
      }
      
      if ("date" %in% names(coef(model_province))) {
        province_data <- province_data %>%
          mutate(b_trend_admin_area_2 = coef(model_province)["date"])
      }
      
      province_results_list[[paste(indicator, province, sep = "_")]] <- province_data
    }
  }
}


# Combine all province-level results into one dataframe
province_results_long <- dplyr::bind_rows(province_results_list)

# Merge province-level results into main dataset
data_disruption <- data_disruption %>%
  dplyr::left_join(
    province_results_long %>%
      dplyr::select(
        facility_id, date, indicator_common_id, 
        expect_admin_area_2, b_admin_area_2, 
        p_admin_area_2, b_trend_admin_area_2
      ),
    by = c("facility_id", "date", "indicator_common_id")
  )

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
              cluster = ~admin_area_3),
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
  
  district_results_long <- dplyr::bind_rows(district_results_list)
  
  print("District-level regression analysis complete!")
  
  data_disruption <- data_disruption %>%
    dplyr::left_join(
      district_results_long %>%
        dplyr::select(
          facility_id, date, indicator_common_id, admin_area_3,
          expect_admin_area_3, b_admin_area_3, p_admin_area_3
        ),
      by = c("facility_id", "date", "indicator_common_id", "admin_area_3")
    )
  
  print("District-level regression results merged into main dataset.")
}





#-------------------------------------------------------------------------------------------------------------
# STEP 3: PREPARE RESULTS FOR VISUALIZATION
#-------------------------------------------------------------------------------------------------------------
# Convert period_id first
admin_area_1_lookup <- raw_data %>%
  dplyr::distinct(facility_id, admin_area_1)

data_disruption <- data_disruption %>%
  dplyr::left_join(admin_area_1_lookup, by = "facility_id") %>%
  dplyr::mutate(period_id = as.integer(format(as.Date(date), "%Y%m")))



summary_disruption_admin1 <- data_disruption %>%
  group_by(admin_area_1, period_id, indicator_common_id) %>%
  summarise(
    count_original     = mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
    count_expect       = mean(expect_admin_area_1, na.rm = TRUE),
    b                  = mean(b_admin_area_1, na.rm = TRUE),
    p                  = mean(p_admin_area_1, na.rm = TRUE),
    num_obs            = n(),
    count_expect_sum   = sum(expect_admin_area_1, na.rm = TRUE),
    count_sum          = sum(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
    diff_percent       = 100 * (mean(expect_admin_area_1, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
      mean(expect_admin_area_1, na.rm = TRUE),
    diff_percent_sum   = 100 * (count_expect_sum - count_sum) / count_expect_sum,
    #count_expected_if_above_diff_threshold
    count_expected_if_above_diff_threshold = ifelse(
      abs(100 * (count_expect_sum - count_sum) / count_expect_sum) > DIFFPERCENT,
      count_expect_sum, count_sum),
    count_expect_diff = ifelse(
      abs(100 * (mean(expect_admin_area_1, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
            mean(expect_admin_area_1, na.rm = TRUE)) > DIFFPERCENT,
      mean(expect_admin_area_1, na.rm = TRUE), mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)
    ),
    .groups = "drop"
  )


summary_disruption_admin2 <- data_disruption %>%
  group_by(admin_area_2, period_id, indicator_common_id) %>%
  summarise(
    count_original     = mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
    count_expect       = mean(expect_admin_area_2, na.rm = TRUE),
    b                  = mean(b_admin_area_2, na.rm = TRUE),
    p                  = mean(p_admin_area_2, na.rm = TRUE),
    num_obs            = n(),
    count_expect_sum   = sum(expect_admin_area_2, na.rm = TRUE),
    count_sum          = sum(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
    diff_percent       = 100 * (mean(expect_admin_area_2, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
      mean(expect_admin_area_2, na.rm = TRUE),
    diff_percent_sum   = 100 * (count_expect_sum - count_sum) / count_expect_sum,
    #count_expected_if_above_diff_threshold
    #count_expect_diff_sum
    count_expected_if_above_diff_threshold = ifelse(
      abs(100 * (count_expect_sum - count_sum) / count_expect_sum) > DIFFPERCENT,
      count_expect_sum, count_sum),
    count_expect_diff = ifelse(
      abs(100 * (mean(expect_admin_area_2, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
            mean(expect_admin_area_2, na.rm = TRUE)) > DIFFPERCENT,
      mean(expect_admin_area_2, na.rm = TRUE), mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)
    ),
    .groups = "drop"
  )



if (RUN_DISTRICT_MODEL) {
  summary_disruption_admin3 <- data_disruption %>%
    group_by(admin_area_3, period_id, indicator_common_id) %>%
    summarise(
      count_original     = mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
      count_expect       = mean(expect_admin_area_3, na.rm = TRUE),
      b                  = mean(b_admin_area_3, na.rm = TRUE),
      p                  = mean(p_admin_area_3, na.rm = TRUE),
      num_obs            = n(),
      count_expect_sum   = sum(expect_admin_area_3, na.rm = TRUE),
      count_sum          = sum(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE),
      diff_percent       = 100 * (mean(expect_admin_area_3, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
        mean(expect_admin_area_3, na.rm = TRUE),
      diff_percent_sum   = 100 * (count_expect_sum - count_sum) / count_expect_sum,
      #count_expect_diff_sum
      #count_expected_if_above_diff_threshold
      count_expected_if_above_diff_threshold = ifelse(
        abs(100 * (count_expect_sum - count_sum) / count_expect_sum) > DIFFPERCENT,
        count_expect_sum, count_sum),
      count_expect_diff = ifelse(
        abs(100 * (mean(expect_admin_area_3, na.rm = TRUE) - mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)) /
              mean(expect_admin_area_3, na.rm = TRUE)) > DIFFPERCENT,
        mean(expect_admin_area_3, na.rm = TRUE), mean(!!sym(VISUALIZATIONCOUNT), na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

# Step 5: Save Outputs ----------------------------------------------------------
print("Saving results...")
write.csv(data_adjusted, "M3_service_utilization.csv", row.names = FALSE)

M3_chartout_export <- M3_chartout_selected %>%
  mutate(
    period_id = as.integer(format(date, "%Y%m")),
    year = as.integer(format(date, "%Y")),
    quarter = as.integer((as.integer(format(date, "%m")) - 1) %/% 3 + 1),
    quarter_id = sprintf("%d%02d", year, quarter)
  ) %>%
  dplyr::select(admin_area_2, 
                indicator_common_id, 
                period_id, 
                quarter_id, 
                year, 
                tagged)



write.csv(M3_chartout_export, "M3_chartout.csv", row.names = FALSE)


# Export summary disruptions at national level (admin_area_1)
summary_disruption_admin1_export <- summary_disruption_admin1 %>%
  mutate(
    year = as.integer(period_id %/% 100),
    quarter = ((period_id %% 100 - 1) %/% 3) + 1,
    quarter_id = sprintf("%d%02d", year, quarter)
  ) %>%
  dplyr::select(admin_area_1, 
                indicator_common_id, 
                period_id, 
                quarter_id, 
                year, 
                count_sum, 
                count_expected_if_above_diff_threshold)

write.csv(summary_disruption_admin1_export, "M3_disruptions_analysis_admin_area_1.csv", row.names = FALSE)

summary_disruption_admin2_export <- summary_disruption_admin2 %>%
  mutate(
    year = as.integer(period_id %/% 100),
    quarter = ((period_id %% 100 - 1) %/% 3) + 1,
    quarter_id = sprintf("%d%02d", year, quarter)
  ) %>%
  dplyr::select(admin_area_2, 
                indicator_common_id, 
                period_id, quarter_id, 
                year, 
                count_sum, 
                count_expected_if_above_diff_threshold)

write.csv(summary_disruption_admin2_export, "M3_disruptions_analysis_admin_area_2.csv", row.names = FALSE)

# Summary Disruptions - Admin Area 3 (Optional)
if (RUN_DISTRICT_MODEL) {
  summary_disruption_admin3_export <- summary_disruption_admin3 %>%
    mutate(
      year = as.integer(period_id %/% 100),
      quarter = ((period_id %% 100 - 1) %/% 3) + 1,
      quarter_id = sprintf("%d%02d", year, quarter)
    ) %>%
    dplyr::select(admin_area_3, 
                  indicator_common_id, 
                  period_id, 
                  quarter_id, 
                  year, 
                  count_sum, 
                  count_expected_if_above_diff_threshold)
  
  write.csv(summary_disruption_admin3_export, "M3_disruptions_analysis_admin_area_3.csv", row.names = FALSE)
}

