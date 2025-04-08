EXCLUDED_FROM_ADJUSTMENT <- c("indicator_a", "indicator_b", "indicator_c")

# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Apr 7

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged outliers with 12-month rolling averages (excluding outliers).
#   2. Completeness: Replaces missing count with 12-month rolling averages (excluding outliers).

# Ce script ajuste dynamiquement les données brutes pour :
#   1. Les valeurs aberrantes : Remplace les valeurs identifiées comme aberrantes par une moyenne mobile sur 12 mois (hors valeurs aberrantes).
#   2. L'exhaustivité : Remplace les valeurs manquantes par une moyenne mobile sur 12 mois (hors valeurs aberrantes).

# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv              # Dataset including facility-level adjusted volumes for all adjustment scenarios.
# FILE: M2_adjusted_data_admin_area.csv   # Dataset including admin-level adjusted volumes for all adjustment scenarios.
# FILE: M2_adjusted_data_national.csv     # Dataset including national-level adjusted volumes for all adjustment scenarios.


# Load Required Libraries -----------------------------------------------------------------------------------
library(data.table)  # For fast data processing & merging
library(zoo)         # For rolling averages
library(stringr)     # For `str_subset()`
library(dplyr)

outlier_data <- read.csv("M1_output_outliers.csv")
completeness_data <- read.csv("M1_completeness_long_format.csv")

# Define Functions ------------------------------------------------------------------------------------------

# Function to Apply Adjustments -----------------------------------------------------------------------------
apply_adjustments <- function(data, outlier_data, completeness_data, geo_cols,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Convert to data.table for performance
  # setDT(data)
  setDT(outlier_data)
  setDT(completeness_data)
  
  # # Start with completeness_data to ensure all facility-month combinations exist
  data <- merge(completeness_data, outlier_data[, .(facility_id, indicator_common_id, year, month, outlier_flag)], 
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
apply_adjustments <- function(completeness_data, outlier_data, adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  setDT(outlier_data)
  setDT(completeness_data)
  
  # Merge outlier info
  data <- merge(
    completeness_data,
    outlier_data[, .(facility_id, indicator_common_id, year, month, outlier_flag)],
    by = c("facility_id", "indicator_common_id", "year", "month"),
    all.x = TRUE
  )
  data[, outlier_flag := fifelse(is.na(outlier_flag), 0, outlier_flag)]
  data[, count_working := as.numeric(count)]
  
  # ----------------------------------------
  # Outlier Adjustment
  # ----------------------------------------
  if (adjust_outliers) {
    cat(" -> Adjusting outliers with rolling average...\n")
    data[, rolling_avg_outlier := frollapply(
      count * (outlier_flag == 0),
      n = 12,
      FUN = function(x) mean(x[x > 0], na.rm = TRUE),
      fill = NA, align = "center"
    ), by = .(facility_id, indicator_common_id)]
    
    data[outlier_flag == 1 & !is.na(rolling_avg_outlier), count_working := rolling_avg_outlier]
  }
  
  # ----------------------------------------
  # Completeness Adjustment
  # ----------------------------------------
  if (adjust_completeness) {
    cat(" -> Adjusting missing data with rolling + fallback methods...\n")
    
    # 1. Rolling average
    data[, rolling_avg_completeness := rollapplyr(
      count_working,
      width = 12,
      FUN = function(x) {
        valid_x <- x[!is.na(x) & x > 0]
        if (length(valid_x) >= 6) mean(valid_x, na.rm = TRUE) else NA_real_
      },
      fill = NA, partial = TRUE, align = "center"
    ), by = .(facility_id, indicator_common_id)]
    
    # 2. Fallback mean per facility-indicator
    data[, fallback_mean := mean(count_working[!is.na(count_working) & count_working > 0], na.rm = TRUE),
         by = .(facility_id, indicator_common_id)]
    
    # Apply fallback logic
    data[is.na(count_working) & !is.na(rolling_avg_completeness), count_working := rolling_avg_completeness]
    data[is.na(count_working) & !is.na(fallback_mean), count_working := fallback_mean]
  }
  
  return(data)
}

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments_scenarios <- function(completeness_data, outlier_data) {
  cat("Applying adjustments across scenarios...\n")
  
  join_cols <- c("facility_id", "indicator_common_id", "year", "month")
  
  # Detect geo columns dynamically from completeness_data
  geo_cols <- colnames(completeness_data) %>% str_subset("^admin_area_[0-9]+$")
  
  # Properly extract geo_cols without `with = FALSE`
  geo_data <- completeness_data %>%
    select(c("facility_id", all_of(geo_cols))) %>%
    distinct()
  
  # Define adjustment scenarios
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
      completeness_data = completeness_data,
      outlier_data = outlier_data,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    ) %>%
      select(all_of(join_cols), count_working)  # Select only necessary columns
    
    # Revert adjustments for excluded indicators
    data_adjusted <- data_adjusted %>%
      mutate(count_working = ifelse(
        indicator_common_id %in% EXCLUDED_FROM_ADJUSTMENT,
        count,
        count_working
      ))
    
    
    colnames(data_adjusted)[colnames(data_adjusted) == "count_working"] <- paste0("count_final_", name)
    results[[name]] <- data_adjusted
  }
  
  # Merge all scenario results together
  df_adjusted <- Reduce(function(x, y) merge(x, y, by = join_cols, all.x = TRUE, all.y = TRUE), results)
  
  # Convert to data.table
  setDT(df_adjusted)
  
  # Ensure period_id and quarter_id are correctly computed
  df_adjusted[, period_id := as.integer(paste0(year, sprintf("%02d", month)))]
  df_adjusted[, quarter_id := as.integer(paste0(year, sprintf("%02d", (month - 1) %/% 3 + 1)))]
  

  df_adjusted <- df_adjusted %>%
    left_join(geo_data, by = "facility_id")
  
  # Attach outlier_flag and replace NAs with 0
  df_adjusted <- df_adjusted %>%
    left_join(
      outlier_data %>% select(all_of(join_cols), outlier_flag),
      by = join_cols
    ) %>%
    mutate(outlier_flag = ifelse(is.na(outlier_flag), 0, outlier_flag))
  
  return(df_adjusted)
}

# ------------------- Main Execution ------------------------------------------------------------------------
print("Running adjustments analysis...")

# Run Adjustment Scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  outlier_data = outlier_data,
  completeness_data = completeness_data
)

geo_cols <- colnames(adjusted_data_final) %>% str_subset("^admin_area_[0-9]+$")

# Aggregate data at the lowest available admin area level
adjusted_data_admin_area_final <- adjusted_data_final %>%
  group_by(across(all_of(geo_cols)),indicator_common_id, year, month, period_id, quarter_id) %>%
  summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate data at annual level
adjusted_data_national_final <- adjusted_data_final %>%  
  group_by(admin_area_1, indicator_common_id, year, month, period_id, quarter_id) %>%
  summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  )

# Save Outputs
write.csv(adjusted_data_final, "M2_adjusted_data.csv", row.names = FALSE)
write.csv(adjusted_data_admin_area_final, "M2_adjusted_data_admin_area.csv", row.names = FALSE)
write.csv(adjusted_data_national_final, "M2_adjusted_data_national.csv", row.names = FALSE)

print("Adjustments completed and saved.")

