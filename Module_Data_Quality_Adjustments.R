# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Jan 24

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged counts with 12-month rolling averages.
#   2. Completeness: Substitutes rolling averages where completeness issues are detected.


outlier_data <- M1_output_outliers.csv
completeness_data <- M1_completeness_long_format.csv

# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv    # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# Load Required Libraries -----------------------------------------------------------------------------------
library(tidyverse)
library(zoo)  # For rolling averages


# Define Functions ------------------------------------------------------------------------------------------
# Function to Apply Adjustments
apply_adjustments <- function(data,
                              outlier_data,
                              completeness_data,
                              geo_cols,
                              adjust_outliers = FALSE,
                              adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Step 1: Merge outlier flags into the main data
  data_merged <- data %>%
    left_join(
      outlier_data %>%
        select(
          facility_id, indicator_common_id, year, month,
          outlier_flag
        ),
      by = c("facility_id", "indicator_common_id", "year", "month")
    )
  
  # Step 2: Merge completeness flags into the main data
  data_merged <- data_merged %>%
    left_join(
      completeness_data %>%
        select(
          facility_id, indicator_common_id, year, month,
          completeness_flag
        ),
      by = c("facility_id", "indicator_common_id", "year", "month")
    )
  
  # Step 3: Add a 12-month rolling average for each indicator at the facility level
  data_merged <- data_merged %>%
    group_by(facility_id, indicator_common_id) %>%
    arrange(year, month) %>%
    mutate(
      rolling_avg = rollapply(
        count,
        width = 12,
        FUN = mean,
        fill = NA,
        align = "centre",
        na.rm = TRUE
      ),
      rolling_avg = ifelse(is.na(rolling_avg), mean(count, na.rm = TRUE), rolling_avg)  # Fallback to mean
    ) %>%
    ungroup()
  
  # Step 4: Create a working column `count_working` initialized with raw `count`
  data_merged <- data_merged %>%
    mutate(count_working = count)  # Start with the raw count
  
  # Step 5: Apply outlier adjustments if required
  if (adjust_outliers) {
    cat(" -> Adjusting outliers with rolling averages...\n")
    data_merged <- data_merged %>%
      mutate(
        count_working = if_else(
          !is.na(outlier_flag) & outlier_flag == 1 & !is.na(rolling_avg),
          rolling_avg,  # Replace outlier with rolling average
          count_working  # Otherwise, keep the original count
        )
      )
  }
  
  # Step 6: Apply completeness adjustments if required
  if (adjust_completeness) {
    cat(" -> Adjusting for completeness issues with rolling averages...\n")
    data_merged <- data_merged %>%
      mutate(
        count_working = if_else(
          !is.na(completeness_flag) & completeness_flag == 0 & !is.na(rolling_avg),
          rolling_avg,  # Replace where completeness is flagged as incomplete
          count_working  # Otherwise, keep the original count
        )
      )
  }
  
  # Step 7: Rename the final adjusted column
  data_merged <- data_merged %>%
    mutate(count_final = count_working)
  
  return(data_merged)
}

# Function to Apply Adjustments for Different Scenarios
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

# ------------------- Main Execution ----------------------------------------------------------------------------

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

