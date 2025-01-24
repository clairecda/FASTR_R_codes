

# CB - R code FASTR PROJECT
# Module: DYNAMIC ADJUSTMENT
# Last edit: 2025 Jan 24

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged counts with 12-month rolling averages.
#   2. Completeness: Substitutes rolling averages where completeness issues are detected.



# DATA: sierraleone_imported_dataset.csv

# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv    # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# Load Required Libraries -----------------------------------------------------------------------------------
library(tidyverse)
library(zoo)  # For rolling averages

# Inputs: Read directly from the environment
outlier_data <- outlier_data_main
completeness_data <- completeness_results

# Ensure `data`, `outlier_data`, and `completeness_data` are available in the environment.

# Define Functions ------------------------------------------------------------------------------------------
# Define Adjustment Function
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
        align = "right",
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
  
  # Step 8: Debugging Logs
  cat("Summary of adjustments applied:\n")
  print(data_merged %>%
          summarise(
            total_outliers_flagged = sum(outlier_flag == 1, na.rm = TRUE),
            total_completeness_issues = sum(completeness_flag == 0, na.rm = TRUE),
            adjustments_applied = sum(count != count_final, na.rm = TRUE)
          ))
  
  # Step 9: Return the merged dataset with adjustments
  return(data_merged)
}

# Define Adjustment Scenarios
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
  df_merged <- Reduce(function(x, y) left_join(x, y, by = join_cols), results)
  
  return(df_merged)
}


# ------------------- Main Execution ----------------------------------------------------------------------------

print("Running adjustments analysis...")

adjusted_data <- apply_adjustments_scenarios(
  data = data,
  outlier_data = outlier_data,
  completeness_data = completeness_data,
  geo_cols = geo_cols
)

# Save Outputs
print("Saving adjusted data...")
write.csv(adjusted_data, "M2_adjusted_data.csv", row.names = FALSE)

print("Adjustments completed and saved.")

