# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Feb 6

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged outliers with 12-month rolling averages (excluding outliers).
#   2. Completeness: Replaces missing count with 12-month rolling averages (excluding outliers).


# DATA: guinea_imported_dataset_v2.csv

# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv    # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# Load Required Libraries -----------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(zoo)  # For rolling averages

data <- read.csv("guinea_imported_dataset_v2.csv")
outlier_data <- read.csv("M1_output_outliers.csv")
completeness_data <- read.csv("M1_completeness_long_format.csv")

# Define Functions ------------------------------------------------------------------------------------------

# Function to Apply Adjustments -----------------------------------------------------------------------------
apply_adjustments <- function(data, outlier_data, completeness_data, geo_cols,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Convert to data.table for performance
  setDT(data)
  setDT(outlier_data)
  setDT(completeness_data)
  
  # Start with completeness_data to ensure all facility-month combinations exist
  data <- merge(completeness_data[, .(facility_id, indicator_common_id, year, month, completeness_flag)], 
                data[, .(facility_id, indicator_common_id, year, month, count)], 
                by = c("facility_id", "indicator_common_id", "year", "month"), 
                all.x = TRUE)  # Preserve all rows from completeness_data
  
  # Set missing counts to NA (ensures completeness rows without observed data are correctly marked)
  data[, count := fifelse(is.na(count), NA_real_, count)]
  
  # Merge Outlier Flags & Replace NAs with 0
  data <- merge(data, outlier_data[, .(facility_id, indicator_common_id, year, month, outlier_flag)], 
                by = c("facility_id", "indicator_common_id", "year", "month"), 
                all.x = TRUE)
  data[, outlier_flag := fifelse(is.na(outlier_flag), 0, outlier_flag)]
  
  
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
    
    data[, rolling_avg_completeness := rollapplyr(
      count_working, width = 12, FUN = function(x) {
        valid_x <- x[!is.na(x) & x > 0]  # Remove NAs and zeros
        if (length(valid_x) >= 6) {
          return(mean(valid_x, na.rm = TRUE))
        } else {
          return(NA_real_)
        }
      }, fill = NA, partial = TRUE, align = "center"),
      by = .(facility_id, indicator_common_id)]
    
    data[completeness_flag == 0 & is.na(count_working) & !is.na(rolling_avg_completeness), 
         count_working := rolling_avg_completeness]
  }
  
  return(data)
}

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments_scenarios <- function(data, outlier_data, completeness_data) {
  join_cols <- c("facility_id", "indicator_common_id", "year", "month")
  
  # Detect geo columns dynamically from `completeness_data`
  geo_cols <- grep("^admin_area_[0-9]+$", colnames(completeness_data), value = TRUE)
  
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  results <- list()  # Store scenario results
  
  for (name in names(scenarios)) {
    cat("Processing scenario:", name, "\n")
    
    adjustment <- scenarios[[name]]
    
    data_adjusted <- apply_adjustments(
      data = copy(data),  # Ensure data is copied properly
      outlier_data = outlier_data,
      completeness_data = completeness_data,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    )[, c(join_cols, "count_working"), with = FALSE]  # Select only necessary columns
    
    setnames(data_adjusted, "count_working", paste0("count_final_", name))
    results[[name]] <- data_adjusted
  }
  
  # Merge all scenario results together
  df_adjusted <- Reduce(function(x, y) merge(x, y, by = join_cols, all.x = TRUE, all.y = TRUE), results)
  
  # Ensure completeness rows are kept, even when merging
  df_final <- merge(data, df_adjusted, by = join_cols, all.x = TRUE, all.y = TRUE)
  
  # Convert to data.table (Fixing `:=` error)
  setDT(df_final)
  
  # Add geo columns from completeness_data (without duplicates)
  geo_data <- unique(completeness_data[, c("facility_id", geo_cols), with = FALSE])
  df_final <- merge(df_final, geo_data, by = "facility_id", all.x = TRUE)
  
  # Ensure period_id and quarter_id are correctly computed
  df_final[, period_id := as.integer(paste0(year, sprintf("%02d", month)))]
  df_final[, quarter_id := as.integer(paste0(year, ceiling(month / 3)))]

  df_final <- df_final[, !c("count", "indicator_label", "mapped_raw_indicator_ids"), with = FALSE]
  
  return(df_final)
}


# ------------------- Main Execution ------------------------------------------------------------------------
print("Running adjustments analysis...")
data <- read.csv("guinea_imported_dataset_v2.csv")
geo_cols <- colnames(data) %>% str_subset("^admin_area_[0-9]+$")

# Drop geo columns + period_id + quarter_id from `data`
data <- data %>% select(-all_of(geo_cols), -period_id, -quarter_id)


# Run Adjustment Scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  data = data,
  outlier_data = outlier_data,
  completeness_data = completeness_data
)

# Save Outputs
write.csv(adjusted_data_final, "M2_adjusted_data.csv", row.names = FALSE)

print("Adjustments completed and saved.")

