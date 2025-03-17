SELECTEDCOUNT <- "count"  # Change as needed

#-------------------------------------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Feb 10
# Module: SERVICE UTILIZATION & DISRUPTION ANALYSIS

# This script analyzes disruptions in essential health services using adjusted HMIS data (M2_adjusted_data.csv).
# It runs regressions at (1) the indicator level, (2) indicator × province level, and (3) the indicator × district level.

# Ce code analyse les perturbations des services de santé essentiels à partir des données HMIS ajustées.
# Il exécute des régressions pour chaque indicateur aux niveaux national, régional et au niveau des districts sanitaires.

# ------------------------------------- KEY OUTPUTS ----------------------------------------------------------
# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.
# FILE: indicator_results.csv           # Indicator-level trends with control limits.
# FILE: M3_chartout.csv                 # Filtered dataset of flagged disruptions only.
# FILE: M3_disruptions_analysis.csv     # Outputs from the disruption analysis.

#-------------------------------------------------------------------------------------------------------------
# Load required libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)
library(stats)   # For time series decomposition
library(fixest)  # For panel regressions (alternative to 'xtreg' in Stata)
library(stringr)
#-------------------------------------------------------------------------------------------------------------
# STEP 1: CONTROL CHART ANALYSIS
#-------------------------------------------------------------------------------------------------------------
print("Loading data for control chart analysis...")
data <- read.csv("M1_output_outliers.csv")                  ##data <- fread("M2_adjusted_data.csv", showProgress = TRUE)

print("Loading disruption data...")
disruption_data <- read_excel("Disruption Database.xlsx")

geo_cols <- names(data)[grepl("^admin_area_", names(data))]
print(paste("Detected geo_cols:", paste(geo_cols, collapse = ", ")))

# Create 'date' column from 'year' and 'month'
data <- data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Remove outliers before processing
data <- data %>%
  filter(outlier_flag != 1) %>%  # Exclude flagged outliers
  drop_na(!!sym(SELECTEDCOUNT))  # Drop missing volume data

# Control Chart Analysis Function
control_chart_analysis <- function(cleaned_data, geo_cols, selected_count) {
  print("Starting control chart analysis...")
  
  
  # Generate `panelvar` (group ID for indicator & province)
  print("Creating panel variable...")
  cleaned_data <- cleaned_data %>%
    group_by(indicator_common_id, admin_area_2) %>%
    mutate(panelvar = cur_group_id()) %>%
    ungroup()
  
  # Aggregate at `admin_area_2` Level while keeping `admin_area_1`
  print("Aggregating data at province level...")
  province_data <- cleaned_data %>%
    group_by(panelvar, indicator_common_id, admin_area_1, admin_area_2, date) %>%
    summarise(!!sym(selected_count) := sum(!!sym(selected_count), na.rm = TRUE), .groups = "drop")
  
  # Expand missing months within each `panelvar`
  print("Expanding missing months for each panel...")
  province_data <- province_data %>%
    group_by(panelvar) %>%
    complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month"),
             fill = setNames(list(NA), selected_count)) %>%
    ungroup()
  
  
  # Fill missing province and indicator names
  print("Filling missing province and indicator names...")
  province_data <- province_data %>%
    group_by(panelvar) %>%
    complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month"),
             fill = setNames(list(NA), selected_count)) %>%
    fill(indicator_common_id, admin_area_1, admin_area_2, .direction = "downup") %>%  # Fill missing attributes
    ungroup()
  
  print("Filling complete.")
  
  # Drop months with low volume (less than 50% of the global mean)
  print("Dropping months with abnormally low volume...")
  province_data <- province_data %>%
    group_by(panelvar) %>%
    mutate(globalmean = mean(!!sym(selected_count), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!sym(selected_count) := ifelse(!!sym(selected_count) / globalmean < 0.5, NA, !!sym(selected_count)))
  
  print(paste("After filtering low-volume months:", nrow(province_data), "rows"))
  
  # Proceed with control chart processing
  panel_list <- unique(province_data$panelvar)
  results_list <- list()
  
  for (panel in panel_list) {
    print(paste("Processing panel:", panel))
    panel_data <- province_data %>% filter(panelvar == panel)
    
    min_date <- min(panel_data$date, na.rm = TRUE)
    max_date <- max(panel_data$date, na.rm = TRUE)
    
    # Expand missing months within the time range
    panel_data <- panel_data %>%
      complete(date = seq(min_date, max_date, by = "month"), fill = setNames(list(NA), selected_count))
    
    # Interpolate missing values
    panel_data <- panel_data %>%
      mutate(!!sym(selected_count) := zoo::na.approx(!!sym(selected_count), na.rm = FALSE, maxgap = Inf))
    
    # Regression for deseasonalization
    if (sum(!is.na(panel_data[[selected_count]])) >= 12) {
      print(paste("Running regression for panel", panel))
      panel_data <- panel_data %>%
        mutate(
          count_predict = tryCatch(
            predict(lm(!!sym(selected_count) ~ factor(month(date)) + as.numeric(date),
                       data = panel_data, na.action = na.omit),
                    newdata = panel_data),
            error = function(e) NA  # If error, return NA
          )
        )
    } else {
      print(paste("Not enough data for regression in panel", panel, "- using median"))
      panel_data <- panel_data %>%
        mutate(count_predict = median(!!sym(selected_count), na.rm = TRUE))
    }
    
    # # Rolling Mean Smoothing
    # panel_data <- panel_data %>%
    #   mutate(
    #     count_smooth = zoo::rollmean(count_predict, k = min(12, n()), 
    #                                  fill = median(count_predict, na.rm = TRUE), align = "center"),
    #     residual = !!sym(selected_count) - count_smooth,
    #     sd_residual = ifelse(sd(residual, na.rm = TRUE) > 0, sd(residual, na.rm = TRUE), 1),
    #     control = residual / sd_residual
    #   )
    
    # Lead-Lag Mean Smoothing (style)
    for (i in 1:12) {
      panel_data <- panel_data %>%
        mutate(!!paste0("vf", i) := lead(count_predict, i),
               !!paste0("vl", i) := lag(count_predict, i))
    }
    
    panel_data <- panel_data %>%
      mutate(count_smooth = rowMeans(select(., starts_with("vf"), starts_with("vl"), "count_predict"), na.rm = TRUE)) %>%
      select(-starts_with("vf"), -starts_with("vl"))
    
    # Compute residuals and control values
    panel_data <- panel_data %>%
      mutate(
        residual = !!sym(selected_count) - count_smooth,
        sd_residual = ifelse(sd(residual, na.rm = TRUE) > 0, sd(residual, na.rm = TRUE), 1),
        control = residual / sd_residual
      )
    
    # For the last 6 months -> replace count_smooth with NA
    last_6_months <- max(panel_data$date, na.rm = TRUE) - months(6)
    panel_data <- panel_data %>%
      mutate(count_smooth = ifelse(date >= last_6_months, NA, count_smooth))
    
    # Tag unusual timepoints 
    panel_data <- panel_data %>%
      mutate(
        tag = case_when(
          abs(control) >= 2 ~ 1,  # Unusual deviation
          abs(!!sym(selected_count) - count_smooth) / count_smooth < 0.05 ~ 0,  # Normal fluctuations
          abs(control) <= 0.5 ~ 0,  # Within normal range
          TRUE ~ NA_real_
        )
      ) %>%
      mutate(tag = as.numeric(tag))  # Ensure tag is numeric
    
    # Initialize tagged1 and tagged2
    panel_data <- panel_data %>%
      mutate(tagged1 = 0, tagged2 = 0)
    
    # Propagate anomalies forward within each panel
    panel_data <- panel_data %>%
      arrange(panelvar, date) %>%
      mutate(
        tagged1 = case_when( #fwd tagging
          tag == 1 ~ 1,  
          lag(tagged1, default = 0) == 1 & control > 0 ~ 1, 
          TRUE ~ 0
        )
      ) %>%
      arrange(panelvar, desc(date)) %>%
      mutate(
        tagged2 = case_when(
          tag == 1 ~ 1,  
          lead(tagged2, default = 0) == 1 & control > 0 ~ 1,
          TRUE ~ 0  
        )
      ) %>%
      mutate(tagged = ifelse(tagged1 == 1 | tagged2 == 1, 1, 0))  # Final tagged column
    results_list[[as.character(panel)]] <- panel_data
    
  }
  
  final_results <- bind_rows(results_list)
  
  print("Applying taggedmean logic for systemic disruptions...")
  final_results <- final_results %>%
    mutate(tagged = as.numeric(tagged)) %>%
    group_by(date) %>%
    mutate(taggedmean = sum(tagged, na.rm = TRUE) / n_distinct(panelvar)) %>%  # Proportion of groups tagged
    ungroup() %>%
    mutate(tagged = ifelse(taggedmean > 0.3, 1, tagged))
  
  
  print("Control chart analysis complete.")
  return(final_results)
}

# Merge Control Chart Results with Disruption Database
merge_disruption_data <- function(control_chart_results, disruption_data) {
  print("Merging with disruption database...")
  
  # Fix country spelling for Guinea
  disruption_data <- disruption_data %>%
    mutate(admin_area_1 = ifelse(country_keep == "Guinea", "Guinée", country_keep)) %>%
    mutate(
      start_date = as.Date(sprintf("%d-%02d-01", startdate_year, startdate_month)),
      end_date = as.Date(sprintf("%d-%02d-01", enddate_year, enddate_month))
    ) %>%
    select(admin_area_1, admin_area_2 = province_keep, indicator_common_id = indicatortype_keep, 
           start_date, end_date, disruption_reason)
  
  print("Disruption database processed.")
  
  # Perform Left Join but Preserve Original Data
  merged_data <- control_chart_results %>%
    left_join(disruption_data, by = "admin_area_1") %>%
    mutate(
      # Apply 'ALL' Logic for Provinces
      admin_area_2 = case_when(
        is.na(admin_area_2.y) ~ admin_area_2.x,  # If no match, keep original
        admin_area_2.y == "ALL" ~ admin_area_2.x,  # Apply to all provinces
        TRUE ~ admin_area_2.y  # If specific match, use it
      ),
      # Apply 'ALL' Logic for Indicators
      indicator_common_id = case_when(
        is.na(indicator_common_id.y) ~ indicator_common_id.x,  # If no match, keep original
        indicator_common_id.y == "ALL" ~ indicator_common_id.x,  # Apply to all indicators
        TRUE ~ indicator_common_id.y  # If specific match, use it
      )
    ) %>%
    select(-admin_area_2.x, -admin_area_2.y, -indicator_common_id.x, -indicator_common_id.y)  # Remove redundant columns
  
  print("Applied 'ALL' logic for provinces and indicators.")
  
  merged_data <- merged_data %>%
    mutate(
      disruption_applies = case_when(
        is.na(start_date) ~ 0,  # If no disruption, do nothing
        date >= start_date & date <= end_date ~ 1,  # Apply only if within the correct period
        TRUE ~ 0  # Otherwise, ignore the disruption
      ),
      tagged = case_when(
        disruption_applies == 1 ~ 1,  # Apply tag only if disruption applies
        TRUE ~ tagged  # Keep original tagging from control chart
      ),
      disruption_reason = case_when(
        disruption_applies == 1 ~ disruption_reason,  # Keep disruption reason only where it applies
        TRUE ~ NA_character_  # Otherwise, set to NA
      )
    ) %>%
    select(-start_date, -end_date, -disruption_applies)  # Remove unnecessary columns
  
  print("Final merging complete. All time periods preserved.")
  
  #Ensure Proper Column Order
  merged_data <- merged_data %>%
    relocate(admin_area_1, admin_area_2, .before = count) %>%  # Move admin_area_1 & admin_area_2 to the start
    relocate(indicator_common_id, .before = count)  # Keep indicator_common_id in place
  
  print("Column cleanup completed.")
  
  return(merged_data)
}



# Run Control Chart Analysis - Merge Disruption Data
control_chart_results <- control_chart_analysis(data, geo_cols, SELECTEDCOUNT)
final_results <- merge_disruption_data(control_chart_results, disruption_data)

# Save final outputs
write_csv(final_results, "M3_chartout.csv")
print("Final results saved.")



M3_chartout <- final_results


#-------------------------------------------------------------------------------------------------------------
# STEP 2: DISRUPTION REGRESSION ANALYSIS (FACILITY + DISTRICT + PROVINCE)
#-------------------------------------------------------------------------------------------------------------
# Step 1: Load and Prepare Data
print("Loading and preparing data for disruption analysis...")
# Select only necessary columns from M3_chartout to avoid duplication
M3_chartout_selected <- M3_chartout %>%
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

# Identify the lowest available geographic level for clustering
geo_cols <- colnames(data_disruption) %>% str_subset("^admin_area_[0-9]+$")
lowest_geo_level <- if (length(geo_cols) > 0) {
  geo_cols[which.max(as.numeric(str_extract(geo_cols, "[0-9]+")))]
} else {
  stop("No geographic levels detected in the dataset!")
}
print(paste("Using geographic level for clustering:", lowest_geo_level))


# Create empty lists to store results
results_list <- list()                #Step 4a
district_results_list <- list()       #Step 4b
province_results_list <- list()       #Step 4c

# Step 4a: Run Regression for Each Indicator -----------------------------------
print("Running regressions at the indicator level...")

for (indicator in indicators) {
  print(paste("Processing:", indicator))
  
  indicator_data <- data_disruption %>%
    filter(indicator_common_id == indicator) %>%
    drop_na(!!sym(SELECTEDCOUNT))
  
  if (nrow(indicator_data) == 0) {
    print(paste("Skipping", indicator, "- insufficient data"))
    next
  }
  
  model <- feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), 
                 data = indicator_data, 
                 cluster = as.formula(paste0("~", lowest_geo_level)))
  
  indicator_data <- indicator_data %>%
    mutate(expect_admin_area_1 = predict(model, newdata = indicator_data))
  
  for (pmonth in unique(indicator_data$date[indicator_data$tagged == 1])) {
    coeff <- coef(model)["tagged"]
    indicator_data <- indicator_data %>%
      mutate(expect_admin_area_1 = ifelse(date == pmonth, expect_admin_area_1 - coeff, expect_admin_area_1))
  }
  
  indicator_data <- indicator_data %>%
    mutate(
      b_admin_area_1 = ifelse(tagged == 1, -1 * (mean(expect_admin_area_1 - !!sym(SELECTEDCOUNT), na.rm = TRUE) / mean(expect_admin_area_1, na.rm = TRUE)), NA_real_),
      b_trend_admin_area_1 = coef(model)["date"]
    ) %>%
    select(facility_id, date, indicator_common_id, expect_admin_area_1, b_admin_area_1, b_trend_admin_area_1)
  
  results_list[[indicator]] <- indicator_data
}

results_long <- bind_rows(results_list)

# Merge national-level results into main dataset
data_disruption <- data_disruption %>%
  left_join(results_long,
            by = c("facility_id", "date", "indicator_common_id"))

# # Step 4b: Run Regression for Each Indicator × District ------------------------
# print("Running regressions at the district level...")
# for (indicator in indicators) {
#   for (district in districts) {
#     print(paste("Processing:", indicator, "in district:", district))
#     
#     # Filter data for this indicator and district
#     district_data <- data_disruption %>%
#       filter(indicator_common_id == indicator, !!sym(lowest_geo_level) == district) %>%
#       drop_na(!!sym(SELECTEDCOUNT))
#     
#     if (nrow(district_data) < 50) {
#       print(paste("Skipping", indicator, "in", district, "- insufficient data"))
#       next
#     }
#     
#     # Run district-level regression dynamically
#     model_district <- tryCatch(
#       feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
#             data = district_data,
#             cluster = as.formula(paste0("~", lowest_geo_level))),
#       error = function(e) NULL
#     )
#     
#     if (!is.null(model_district) && !anyNA(coef(model_district))) {
#       district_data <- district_data %>%
#         mutate(expect_district = predict(model_district, newdata = district_data))
#       
#       # Adjust for pandemic months
#       for (pmonth in unique(district_data$date[district_data$tagged == 1])) {
#         coeff <- coef(model_district)["tagged"]
#         district_data <- district_data %>%
#           mutate(expect_district = ifelse(date == pmonth, expect_district - coeff, expect_district))
#       }
#       
#       # Compute b_admin_area dynamically
#       district_data <- district_data %>%
#         group_by(date) %>%
#         mutate(
#           diff = expect_district - !!sym(SELECTEDCOUNT),
#           diffmean = mean(diff, na.rm = TRUE),
#           predictmean = mean(expect_district, na.rm = TRUE),
#           b_admin_area = ifelse(tagged == 1, -1 * (diffmean / predictmean), NA_real_)
#         ) %>%
#         ungroup()
#       
#       # Compute p-value for the effect of "tagged"
#       if ("tagged" %in% names(coef(model_district))) {
#         p_admin_area <- 2 * pt(abs(coef(model_district)["tagged"] / sqrt(diag(vcov(model_district)))["tagged"]),
#                                df.residual(model_district), lower.tail = FALSE)
#         district_data <- district_data %>%
#           mutate(p_admin_area = p_admin_area)
#       }
#       
#       # Store results
#       district_results_list[[paste(indicator, district, sep = "_")]] <- district_data %>%
#         select(date, indicator_common_id, !!sym(lowest_geo_level), expect_district, b_admin_area, p_admin_area)
#     }
#   }
# }
# print("District-level regression analysis complete!")
# 
# # Combine district results
# district_results_long <- bind_rows(district_results_list)

# # Merge district-level results into main dataset using admin_area_3
# data_disruption <- data_disruption %>%
#   left_join(district_results_long,
#             by = c("facility_id", "date", "indicator_common_id", "admin_area_3"),
#             suffix = c("", "_admin_area_3"))  # Prevents column name conflicts

# print("District-level regression analysis complete!")

# Step 4c: Run Regression for Each Indicator × Province ------------------------
print("Running regressions at the province level...")
for (indicator in indicators) {
  for (province in provinces) {
    print(paste("Processing:", indicator, "in province:", province))

    province_data <- data_disruption %>%
      filter(indicator_common_id == indicator, admin_area_2 == province) %>%
      drop_na(!!sym(SELECTEDCOUNT))

    if (nrow(province_data) < 10) {
      print(paste("Skipping", indicator, "in", province, "- insufficient data"))
      next
    }

    # Run province-level regression dynamically
    model_province <- tryCatch(
      feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")),
            data = province_data,
            cluster = as.formula(paste0("~", lowest_geo_level))),
      error = function(e) NULL
    )

    if (!is.null(model_province) && !anyNA(coef(model_province))) {
      province_data <- province_data %>%
        mutate(expect_admin_area_2 = predict(model_province, newdata = province_data))
      
      # Adjust for pandemic months
      for (pmonth in unique(province_data$date[province_data$tagged == 1])) {
        coeff <- coef(model_province)["tagged"]
        province_data <- province_data %>%
          mutate(expect_admin_area_2 = ifelse(date == pmonth, expect_admin_area_2 - coeff, expect_admin_area_2))
      }
      
      # Compute b_admin_area_2 correctly
      province_data <- province_data %>%
        group_by(date) %>%
        mutate(
          diff = expect_admin_area_2 - !!sym(SELECTEDCOUNT),
          diffmean = mean(diff, na.rm = TRUE),
          predictmean = mean(expect_admin_area_2, na.rm = TRUE),
          b_admin_area_2 = ifelse(tagged == 1, -1 * (diffmean / predictmean), NA_real_)
        ) %>%
        ungroup()
      
      # Compute p-value
      if ("tagged" %in% names(coef(model_province))) {
        p_admin_area_2 <- 2 * pt(abs(coef(model_province)["tagged"] / sqrt(diag(vcov(model_province)))["tagged"]),
                                 df.residual(model_province), lower.tail = FALSE)
        province_data <- province_data %>%
          mutate(p_admin_area_2 = p_admin_area_2)
      }
    }}
}

# Combine province results
province_results_long <- bind_rows(province_results_list)

# Merge province-level results into main dataset using admin_area_2
data_disruption <- data_disruption %>%
  left_join(province_results_long,
            by = c("date", "indicator_common_id", "admin_area_2"),
            suffix = c("", "_admin_area_2"))  # Prevents column name conflicts

print("Province-level regression analysis complete!")

# # Step 5: Save Output ----------------------------------------------------------
# print("Saving results...")
# write.csv(M3_chartout, "M3_chartout.csv", row.names = FALSE)
# #write.csv(data, "M3_service_utilization.csv", row.names = FALSE)
# write.csv(data_disruption, "M3_disruptions_analysis.csv", row.names = FALSE)
