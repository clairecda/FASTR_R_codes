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

raw_data <- read.csv("guinea_data_updated.csv")
outlier_data <- read.csv("M1_output_outliers.csv")
completeness_data <- read.csv("M1_output_completeness.csv")

# Define Functions ------------------------------------------------------------------------------------------

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments <- function(raw_data, completeness_data, outlier_data,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  setDT(raw_data)
  setDT(outlier_data)
  setDT(completeness_data)
  
  # Merge completeness and outlier flags
  data_adj <- merge(
    completeness_data,
    outlier_data[, .(facility_id, indicator_common_id, period_id, outlier_flag)],
    by = c("facility_id", "indicator_common_id", "period_id"),
    all.x = TRUE
  )
  data_adj[, outlier_flag := fifelse(is.na(outlier_flag), 0, outlier_flag)]
  
  # Merge in raw count
  data_adj <- merge(
    data_adj,
    raw_data[, .(facility_id, indicator_common_id, period_id, count)],
    by = c("facility_id", "indicator_common_id", "period_id"),
    all.x = TRUE
  )
  
  # Initialize working count column
  data_adj[, count_working := as.numeric(count)]
  
  # Outlier Adjustment
  if (adjust_outliers) {
    cat(" -> Adjusting outliers with rolling average...\n")
    data_adj[, rolling_avg_outlier := frollapply(
      count * (outlier_flag == 0),
      n = 12,
      FUN = function(x) mean(x[x > 0], na.rm = TRUE),
      fill = NA, align = "center"
    ), by = .(facility_id, indicator_common_id)]
    
    data_adj[outlier_flag == 1 & !is.na(rolling_avg_outlier), count_working := rolling_avg_outlier]
  }
  
  # Completeness Adjustment
  if (adjust_completeness) {
    cat(" -> Adjusting missing data with rolling + fallback methods...\n")
    
    data_adj[, rolling_avg_completeness := rollapplyr(
      count_working,
      width = 12,
      FUN = function(x) {
        valid_x <- x[!is.na(x) & x > 0]
        if (length(valid_x) >= 6) mean(valid_x, na.rm = TRUE) else NA_real_
      },
      fill = NA, partial = TRUE, align = "center"
    ), by = .(facility_id, indicator_common_id)]
    
    data_adj[, fallback_mean := mean(count_working[!is.na(count_working) & count_working > 0], na.rm = TRUE),
             by = .(facility_id, indicator_common_id)]
    
    data_adj[is.na(count_working) & !is.na(rolling_avg_completeness), count_working := rolling_avg_completeness]
    data_adj[is.na(count_working) & !is.na(fallback_mean), count_working := fallback_mean]
  }
  
  return(data_adj)
}

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments_scenarios <- function(raw_data, completeness_data, outlier_data) {
  cat("Applying adjustments across scenarios...\n")
  
  join_cols <- c("facility_id", "indicator_common_id", "period_id")
  
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  results <- list()
  
  for (name in names(scenarios)) {
    cat("Processing scenario:", name, "\n")
    adjustment <- scenarios[[name]]
    
    data_adjusted <- apply_adjustments(
      raw_data = raw_data,
      completeness_data = completeness_data,
      outlier_data = outlier_data,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    ) %>%
      select(all_of(join_cols), count, count_working) %>%
      mutate(count_working = ifelse(
        indicator_common_id %in% EXCLUDED_FROM_ADJUSTMENT,
        count,
        count_working
      ))
    
    colnames(data_adjusted)[colnames(data_adjusted) == "count_working"] <- paste0("count_final_", name)
    results[[name]] <- data_adjusted %>% select(-count)
  }
  
  df_adjusted <- Reduce(function(x, y) merge(x, y, by = join_cols, all.x = TRUE, all.y = TRUE), results)
  
  
  return(df_adjusted)
}

# ------------------- Main Execution ------------------------------------------------------------------------
print("Running adjustments analysis...")

# Run Adjustment Scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  raw_data = raw_data,
  completeness_data = completeness_data,
  outlier_data = outlier_data
)

# Extract geo columns from raw_data
geo_cols <- colnames(raw_data) %>% str_subset("^admin_area_[0-9]+$")

# Join geography into adjusted data
adjusted_data_geo <- adjusted_data_final %>%
  left_join(
    raw_data %>%
      select(facility_id, all_of(geo_cols)) %>%
      distinct(),
    by = "facility_id"
  )

# Aggregate at lowest available admin area level
adjusted_data_admin_area_final <- adjusted_data_geo %>%
  group_by(across(all_of(geo_cols)), indicator_common_id, period_id) %>%
  summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate at national level (admin_area_1)
adjusted_data_national_final <- adjusted_data_geo %>%
  group_by(admin_area_1, indicator_common_id, period_id) %>%
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

