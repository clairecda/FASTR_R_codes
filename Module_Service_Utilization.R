# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 22
# Module: SERVICE UTILIZATION



# DATA: guinea_imported_dataset.csv

# ------------------------------------- PARAMETERS -----------------------------

# ------------------------------------------------------------------------------
# Outlier Analysis Parameters
outlier_params <- list(
  outlier_pc_threshold = 0.8,  # Threshold for proportional contribution to flag outliers
  count_threshold = 100        # Minimum count to consider for outlier adjustment
)

geo_level <- "admin_area_3"    # Desired geographic level (district) for grouping when adjusting outliers (e.g., "admin_area_3").
# If this level is not available in the dataset, the function will fall back to the lowest 
# available level in `geo_cols` (e.g., "admin_area_2").

# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE: output_outliers.csv             # Detailed facility-level data with identified outliers and adjusted volumes.

# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)  # Ensure lubridate is loaded for floor_date

# Define Functions -----------------------------------------------------------
load_and_preprocess_data <- function(file_path) {
  print("Loading and preprocessing data...")
  data <- read.csv(file_path) %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      panelvar = paste(indicator_common_id, facility_id, sep = "_")
    )
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  return(list(data = data, geo_cols = geo_cols))
}


# PART 1 OUTLIERS ------------------------------------------------------------------
outlier_analysis <- function(data, geo_cols, outlier_params) {
  print("Performing outlier analysis...")
  
  # Step 1: Calculate Median Volume
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(median_volume = median(count, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Calculate MAD and Identify Outliers
  print("Calculating MAD and identifying outliers...")
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(
      mad_volume = ifelse(!is.na(count), mad(count[count >= median_volume], na.rm = TRUE), NA),
      mad_residual = ifelse(!is.na(mad_volume) & mad_volume > 0, abs(count - median_volume) / mad_volume, NA),
      moderate = ifelse(!is.na(mad_residual) & mad_residual > 10 & mad_residual <= 20, 1, 0),  # note - this might not be necessary - not used
      severe = ifelse(!is.na(mad_residual) & mad_residual > 20, 1, 0), # note - this might not be necessary - not used
      outlier_mad = ifelse(moderate == 1 | severe == 1, 1, 0)
    ) %>%
    ungroup()
  
  # Step 3: Calculate Proportional Contribution using Parameter
  print("Calculating proportional contribution...")
  data <- data %>%
    group_by(facility_id, indicator_common_id, year) %>%
    mutate(
      pc = count / sum(count, na.rm = TRUE),
      outlier_pc = ifelse(!is.na(pc) & pc > outlier_params$outlier_pc_threshold, 1, 0)
    ) %>%
    ungroup()
  
  # Step 4: Combine Outlier Flags
  data <- data %>%
    mutate(outlier = ifelse((outlier_mad == 1 | outlier_pc == 1) & count > outlier_params$count_threshold, 1, 0))
  
  # Step 5: Adjust Outliers with Geographic Level
  print("Adjusting outliers with specified geographic level...")
  
  if (!geo_level %in% geo_cols) {
    warning(paste(geo_level, "is not present in the dataset. Falling back to the lowest available level."))
    geo_level <- tail(geo_cols, 1)  # Use the lowest available geographic level
  }
  
  data <- data %>%
    group_by(across(all_of(c(geo_level, "indicator_common_id")))) %>%
    mutate(
      count_adjust = ifelse(outlier == 1, median(count, na.rm = TRUE), count),
      deviance = ifelse(count_adjust > 0, (count - count_adjust) / count_adjust, NA_real_), 
      volIM = mean(count[!outlier], na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Step 6: Generate Dataset for Reporting Top Outliers
  print("Generating dataset for reporting top outliers...")
  top_outliers_data <- data %>%
    filter(outlier == 1) %>%
    mutate(percent_change = (count - count_adjust) / count_adjust) %>%
    group_by(indicator_common_id, facility_id) %>%
    summarise(
      max_percent_change = max(percent_change, na.rm = TRUE),
      mean_volIM = mean(volIM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(max_percent_change))  # Sort by descending percent change
  
  
  # Return all relevant datasets
  return(list(
    outlier_data = data,
    top_outliers_data = top_outliers_data))
}


# ------------------- Main Execution-------------------------------------------------------------------------------------

inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols


# Outlier Analysis
print("Running outlier analysis...")
outlier_results    <- outlier_analysis(data, geo_cols, outlier_params)
outlier_data_main  <- outlier_results$outlier_data

# -------------------------------- SAVE DATA OUTPUTS --------------------------------------------------------------------------------------------------

print("Saving all data outputs from outlier analysis...")
write.csv(outlier_results$outlier_data, "output_outliers.csv", row.names = FALSE)             # Facility-level outlier data
write.csv(outlier_results$top_outliers_data, "top_outliers_data.csv", row.names = FALSE)      # Top outliers summary
