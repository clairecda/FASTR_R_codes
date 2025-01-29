# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Jan 29

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged counts with 12-month rolling averages.
#   2. Completeness: Substitutes rolling averages where completeness issues are detected.

outlier_data <- read_csv("M1_output_outliers.csv")
completeness_data <- read_csv("M1_completeness_long_format.csv")


# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv    # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# Load Required Libraries -----------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(zoo)  # For rolling averages


# Define Functions ------------------------------------------------------------------------------------------

# Function to Apply Adjustments -----------------------------------------------------------------------------
apply_adjustments <- function(data, outlier_data, completeness_data, geo_cols,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Convert to data.table for performance
  setDT(data)
  setDT(outlier_data)
  setDT(completeness_data)
  
  # Merge Outlier and Completeness Flags
  data <- merge(data, outlier_data[, .(facility_id, indicator_common_id, year, month, outlier_flag)], 
                by = c("facility_id", "indicator_common_id", "year", "month"), all.x = TRUE)
  data[, outlier_flag := fifelse(is.na(outlier_flag), 0, outlier_flag)]
  
  data <- merge(data, completeness_data[, .(facility_id, indicator_common_id, year, month, completeness_flag)], 
                by = c("facility_id", "indicator_common_id", "year", "month"), all.x = TRUE)
  data[, completeness_flag := fifelse(is.na(completeness_flag), 1, completeness_flag)]
  
  # Initialize working count column
  data[, count_working := as.numeric(count)]
  
  # Outlier Adjustment --------------------------------------------------------------------------------------
  if (adjust_outliers) {
    cat(" -> Adjusting outliers...\n")
    
    if ("facility_type" %in% names(data) & any(!is.na(data$facility_type))) {
      # Option 1: Use group-based mean if facility_type exists
      cat(" --> Using facility_type grouping for outlier adjustment...\n")
      data[, avg_non_outlier := mean(count[outlier_flag == 0], na.rm = TRUE), 
           by = c(geo_cols, "indicator_common_id", "facility_type", "year", "month")]
      data[outlier_flag == 1, count_working := avg_non_outlier]
    } else {
      # Option 2: Use Rolling Average for Outliers
      cat(" --> Using rolling average for outlier adjustment...\n")
      data[, rolling_avg_outlier := frollapply(
        count * (outlier_flag == 0), 
        n = 12, 
        FUN = function(x) mean(x[x > 0], na.rm = TRUE), 
        fill = NA, align = "center"), 
        by = .(facility_id, indicator_common_id)]
      data[outlier_flag == 1 & !is.na(rolling_avg_outlier), count_working := rolling_avg_outlier]
    }
  }
  
  # Completeness Adjustment --------------------------------------------------------------------------------
  if (adjust_completeness) {
    cat(" -> Adjusting completeness issues...\n")
    
    data[, rolling_avg_completeness := frollapply(
      count_working * (outlier_flag == 0), 
      n = 12, 
      FUN = function(x) mean(x[x > 0], na.rm = TRUE), 
      fill = NA, align = "center"), 
      by = .(facility_id, indicator_common_id)]
    
    data[completeness_flag == 0 & !is.na(rolling_avg_completeness), count_working := rolling_avg_completeness]
  }
  
  return(data)
}

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments_scenarios <- function(data, outlier_data, completeness_data, geo_cols) {
  join_cols <- c(geo_cols, "facility_id", "indicator_common_id", "year", "month")
  
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  results <- lapply(names(scenarios), function(name) {
    cat("Processing scenario:", name, "\n")
    adjustment <- scenarios[[name]]
    
    data_adjusted <- apply_adjustments(
      data = copy(data),
      outlier_data = outlier_data,
      completeness_data = completeness_data,
      geo_cols = geo_cols,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    )[, .SD, .SDcols = c(join_cols, "count_working")]
    
    setnames(data_adjusted, "count_working", paste0("count_final_", name))
    return(data_adjusted)
  })
  
  df_adjusted <- Reduce(function(x, y) merge(x, y, by = join_cols, all.x = TRUE), results)
  
  df_final <- merge(data, df_adjusted, by = join_cols, all.x = TRUE)
  
  return(df_final)
}

# ------------------- Main Execution ------------------------------------------------------------------------
print("Running adjustments analysis...")

# Run Adjustment Scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  data = data,
  outlier_data = outlier_data,
  completeness_data = completeness_data,
  geo_cols = c("admin_area_1", "admin_area_2", "admin_area_3")
)

# Save Outputs
write.csv(adjusted_data_final, "M2_adjusted_data.csv", row.names = FALSE)

print("Adjustments completed and saved.")

