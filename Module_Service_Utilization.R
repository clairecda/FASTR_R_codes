SELECTEDCOUNT <- "count_final_both"  # Change as needed

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
print("Loading adjusted data for control chart analysis...")
#data <- read.csv("M2_adjusted_data.csv")
data <- fread("M2_adjusted_data.csv", showProgress = TRUE)
# Detect geographic columns
geo_cols <- names(data)[grepl("^admin_area_", names(data))]
print(paste("Detected geo_cols:", paste(geo_cols, collapse = ", ")))

print("Creating 'date' column from 'year' and 'month'...")
data$date <- as.Date(with(data, sprintf("%d-%02d-01", year, month)))

# Optimized Control Chart Analysis Function
control_chart_analysis <- function(cleaned_data, geo_cols, selected_count) {
  print("Starting optimized control chart analysis...")
  
  # Ensure 'date' column exists
  if (!"date" %in% colnames(cleaned_data)) {
    print("Creating 'date' column from 'year' and 'month'...")
    cleaned_data <- cleaned_data %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-")))
  }
  
  print(paste("Initial dataset dimensions:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "columns"))
  
  # Aggregate at `admin_area_2` (Province) Level
  province_data <- cleaned_data %>%
    group_by(indicator_common_id, admin_area_2, date) %>%
    summarise(!!sym(selected_count) := sum(!!sym(selected_count), na.rm = TRUE), .groups = "drop")
  
  print(paste("Dataset after province-level aggregation:", nrow(province_data), "rows"))
  
  # List of indicators
  indicator_list <- unique(province_data$indicator_common_id)
  results_list <- list()
  
  for (indicator in indicator_list) {
    print(paste("Processing indicator:", indicator))
    
    indicator_data <- province_data %>%
      filter(indicator_common_id == indicator)
    
    # Process at Province Level
    province_list <- unique(indicator_data$admin_area_2)
    province_results <- list()
    
    for (province in province_list) {
      print(paste("Processing province:", province))
      
      province_data_subset <- indicator_data %>%
        filter(admin_area_2 == !!province)
      
      min_date <- min(province_data_subset$date, na.rm = TRUE)
      max_date <- max(province_data_subset$date, na.rm = TRUE)
      
      print(paste("Province", province, "time range:", min_date, "to", max_date))
      
      # Expand missing months within the time range
      print(paste("Expanding missing months for", province))
      province_data_subset <- province_data_subset %>%
        complete(date = seq(min_date, max_date, by = "month"), fill = setNames(list(NA), selected_count))
      print(paste("Finished expanding months for", province))
      
      # Interpolate missing values
      print(paste("Interpolating missing values for", province))
      province_data_subset <- province_data_subset %>%
        mutate(!!sym(selected_count) := zoo::na.approx(!!sym(selected_count), na.rm = FALSE, maxgap = Inf))
      print(paste("Interpolation completed for", province))
      
      # Run lm() if there are enough data points
      if (sum(!is.na(province_data_subset[[selected_count]])) >= 12) {
        print(paste("Running lm() regression for", province))
        province_data_subset <- province_data_subset %>%
          mutate(
            count_predict = predict(
              lm(!!sym(selected_count) ~ factor(month(date)) + as.numeric(date), 
                 data = province_data_subset, na.action = na.omit),
              newdata = province_data_subset
            )
          )
        print(paste("lm() regression complete for", province))
      } else {
        province_data_subset <- province_data_subset %>%
          mutate(count_predict = median(!!sym(selected_count), na.rm = TRUE))
        print(paste("Skipped lm() for", province, "due to insufficient data, using median instead"))
      }
      
      # Rolling Mean Smoothing (handle short time frames)
      print(paste("Applying rolling mean smoothing for", province))
      province_data_subset <- province_data_subset %>%
        mutate(
          count_smooth = zoo::rollmean(count_predict, k = min(12, n()), fill = median(count_predict, na.rm = TRUE), align = "center"),
          residual = !!sym(selected_count) - count_smooth,
          sd_residual = ifelse(sd(residual, na.rm = TRUE) > 0, sd(residual, na.rm = TRUE), 1),
          control = residual / sd_residual,
          tag = ifelse(abs(control) >= 2, 1, 0)
        )
      print(paste("Rolling mean smoothing completed for", province))
      
      # Propagate anomalies forward
      print(paste("Propagating anomalies forward for", province))
      province_data_subset <- province_data_subset %>%
        arrange(date) %>%
        mutate(tag = accumulate(replace_na(tag, 0), `|`))
      print(paste("Anomaly propagation completed for", province))
      
      province_results[[province]] <- province_data_subset
    }
    
    # Combine all province results for the indicator
    results_list[[indicator]] <- bind_rows(province_results)
  }
  
  # Combine all indicator results
  final_results <- bind_rows(results_list)
  print("Control chart analysis complete.")
  return(final_results)
}

# Generate Indicator-Level Control Limits
generate_indicator_results <- function(cleaned_data, selected_count) {
  print("Generating indicator-level results with control limits...")
  
  indicator_results <- cleaned_data %>%
    filter(!is.na(!!sym(selected_count)) & !is.na(count_smooth)) %>%
    group_by(indicator_common_id, admin_area_2, date) %>%
    summarise(
      total_count = sum(!!sym(selected_count), na.rm = TRUE),
      predicted_count = sum(count_smooth, na.rm = TRUE),
      sd_total = ifelse(n() > 1, sd(!!sym(selected_count), na.rm = TRUE), NA),  # Compute SD only if data exists
      UCL = total_count + 3 * sd_total,
      LCL = total_count - 3 * sd_total,
      period_id = format(date, "%Y%m"),
      .groups = "drop"
    ) %>%
    mutate(
      UCL = ifelse(is.na(UCL), total_count, UCL),  # Ensure UCL is not NA
      LCL = ifelse(is.na(LCL), total_count, LCL)   # Ensure LCL is not NA
    )
  
  print("Indicator-level control limits generated.")
  return(indicator_results)
}


# Run Control Chart Analysis
control_chart_results <- control_chart_analysis(data, geo_cols, SELECTEDCOUNT)

# Generate Aggregated Control Limits
indicator_results <- generate_indicator_results(control_chart_results, SELECTEDCOUNT)


# Extract Flagged Disruptions
M3_chartout <- control_chart_results %>%
  filter(tag == 1) %>%
  select(date, indicator_common_id, admin_area_2, tag) %>%
  distinct()


#-------------------------------------------------------------------------------------------------------------
# STEP 2: DISRUPTION REGRESSION ANALYSIS (FACILITY + DISTRICT + PROVINCE)
#-------------------------------------------------------------------------------------------------------------
# Step 1: Load and Prepare Data
print("Loading and preparing data for disruption analysis...")
data_disruption <- data %>%
  left_join(M3_chartout, by = c("date", "indicator_common_id", "admin_area_2")) %>%
  mutate(tagged = replace_na(tag, 0))  # Directly using chartout tagged disruptions

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
