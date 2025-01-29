# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Jan 24

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged counts with 12-month rolling averages.
#   2. Completeness: Substitutes rolling averages where completeness issues are detected.

outlier_data <- read_csv("M1_output_outliers.csv")
completeness_data <- read_csv("M1_completeness_long_format.csv")


# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv    # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# Load Required Libraries -----------------------------------------------------------------------------------
library(tidyverse)
library(zoo)  # For rolling averages


# Define Functions ------------------------------------------------------------------------------------------

# PART 1 - Apply Adjustments --------------------------------------------------------------------------------
apply_adjustments <- function(data, outlier_data, completeness_data, geo_cols,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Step 1: Merge outlier flags into the main data
  data_merged <- data %>%
    left_join(outlier_data %>%
                select(facility_id, indicator_common_id, year, month, outlier_flag, everything()),
              by = c("facility_id", "indicator_common_id", "year", "month"))
  
  # Step 2: Merge completeness flags into the main data
  data_merged <- data_merged %>%
    left_join(completeness_data %>%
                select(facility_id, indicator_common_id, year, month, completeness_flag),
              by = c("facility_id", "indicator_common_id", "year", "month"))
  
  # Step 3: Create working count column
  data_merged <- data_merged %>%
    mutate(count_working = count)  # Start with the raw count
  
  # Step 4: Apply Outlier Adjustments
  if (adjust_outliers) {
    cat(" -> Adjusting outliers...\n")
    
    if("facility_type" %in% colnames(data_merged)) {
      # Option 1: Group by facility_type (if it exists) and compute mean of non-outliers
      data_merged <- data_merged %>%
        group_by(across(all_of(c(geo_cols, "indicator_common_id", "facility_type", "year", "month")))) %>%
        mutate(
          avg_non_outlier = mean(count[is.na(outlier_flag) | outlier_flag == 0], na.rm = TRUE),
          volume_adjust = if_else(!is.na(outlier_flag) & outlier_flag == 1, avg_non_outlier, count_working)
        ) %>%
        ungroup()
    }
    
    # Option 2: If facility_type is missing, use rolling average
    data_merged <- data_merged %>%
      group_by(facility_id, indicator_common_id) %>%
      arrange(year, month) %>%
      mutate(
        rolling_avg = rollapply(count, width = 12, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
        volume_adjust = if_else((!"facility_type" %in% colnames(data_merged) | is.na(facility_type)) &
                                  !is.na(outlier_flag) & outlier_flag == 1 & !is.na(rolling_avg),
                                rolling_avg, volume_adjust)
      ) %>%
      ungroup()
  }
  
  # Step 5: Apply Completeness Adjustments
  if (adjust_completeness) {
    cat(" -> Adjusting for completeness issues with rolling averages...\n")
    
    data_merged <- data_merged %>%
      group_by(facility_id, indicator_common_id) %>%
      arrange(year, month) %>%
      mutate(
        rolling_avg_clean = rollapply(volume_adjust[!is.na(completeness_flag) & completeness_flag == 1], 
                                      width = 12, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
        count_working = if_else(!is.na(completeness_flag) & completeness_flag == 0 & !is.na(rolling_avg_clean),
                                rolling_avg_clean, volume_adjust)
      ) %>%
      ungroup()
  }
  
  # Final Adjusted Count
  data_merged <- data_merged %>% mutate(count_final = count_working)
  
  return(data_merged)
}

# PART 2 - Apply Adjustments for Different Scenarios --------------------------------------------------------
apply_adjustments_scenarios <- function(data, outlier_data, completeness_data, geo_cols) {
  join_cols <- c(geo_cols, "facility_id", "indicator_common_id", "year", "month")
  
  # Define adjustment scenarios
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  # Apply adjustments for each scenario
  results <- lapply(names(scenarios), function(name) {
    adjustment <- scenarios[[name]]
    cat("Processing scenario:", name, "\n")
    data_adjusted <- apply_adjustments(
      data = data,
      outlier_data = outlier_data,
      completeness_data = completeness_data,
      geo_cols = geo_cols,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    ) %>%
      select(all_of(join_cols), count_final) %>%
      rename_with(~ paste0("count_final_", name), "count_final")
    return(data_adjusted)
  })
  
  # Merge results into one dataset
  df_adjusted <- Reduce(function(x, y) left_join(x, y, by = join_cols), results)
  
  # Join back with the original columns in `data` while maintaining the order
  df_final <- data %>%
    left_join(df_adjusted, by = join_cols)
  
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

