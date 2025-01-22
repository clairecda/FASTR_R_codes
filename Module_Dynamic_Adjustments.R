# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 Jan 16

# DATA: sierraleone_imported_dataset.csv
# ------------------------------------- KEY OUTPUT(S) ------------------------------------------------------------------------
# FILE: adjusted_data.csv             # Dataset including facility-level adjusted volumes for all adjustment scenarios.



# -------- Dynamic Adjustments (Work-in-Progress) ----------------------------------------------------------------------------

# This script includes functions to dynamically adjust raw data for:
#   1. Outliers: Corrects extreme values that may distort analyses by replacing them with adjusted counts.
#   2. Completeness: Scales up reported counts to account for underreporting. the completeness adjustment implemented
#      in this code aligns conceptually with WHO recommendations, https://www.who.int/publications/i/item/9789240063938
#      for addressing incomplete reporting. 
#      The implementation simplifies the adjustment by assuming that non-reporting facilities provide services at the same
#      level as reporting facilities (k = 1).
#      https://www.who.int/publications/i/item/9789240063938
#
# WHO Formula for completeness adjustment:
#   p_adjusted = p_reported + p_reported * (1 / c - 1) * k
#   - p_adjusted: Adjusted service outputs.
#   - p_reported: Reported service outputs.
#   - c: Reporting completeness as a fraction (e.g., 80% = 0.8).
#   - k: Adjustment factor, reflecting service provision in non-reporting facilities:
#       - k = 0: Non-reporting facilities provide no services.
#       - 0 < k < 1: Non-reporting facilities provide fewer services than reporting ones.
#       - k = 1: Non-reporting facilities provide services at the same rate as reporting ones.

# Simplified formula in this code:
#   p_adjusted = p_reported / c
#   - This assumes k = 1, meaning non-reporting facilities provide services at the same rate as reporting ones.
#   - Completeness percentages are merged into the dataset, converted to fractions, and used to scale up
#     reported counts proportionally.
#
# Lit review in progress - review later

# Load Required Libraries ----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(scales)

# Define Functions -----------------------------------------------------------------------------------------------------------------
load_and_preprocess_data <- function(file_path) {
  print("Loading and preprocessing data...")
  data <- read.csv(file_path) %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      panelvar = paste(indicator_common_id, facility_id, sep = "_")
    )  # Add date column here
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  return(list(data = data, geo_cols = geo_cols))
}

# PART 1 OUTLIERS
outlier_analysis <- function(data, geo_cols) {
  print("Performing outlier analysis...")
  
  # Step 1: Calculate Median Volume
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(median_volume = median(count, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Calculate MAD and Identify Outliers
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(
      mad_volume = ifelse(!is.na(count), mad(count[count >= median_volume], na.rm = TRUE), NA),
      mad_residual = ifelse(!is.na(mad_volume) & mad_volume > 0, abs(count - median_volume) / mad_volume, NA),
      moderate = ifelse(!is.na(mad_residual) & mad_residual > 10 & mad_residual <= 20, 1, 0),
      severe = ifelse(!is.na(mad_residual) & mad_residual > 20, 1, 0),
      outlier_mad = ifelse(moderate == 1 | severe == 1, 1, 0)
    ) %>%
    ungroup()
  
  # Step 3: Calculate Proportional Contribution
  data <- data %>%
    group_by(facility_id, indicator_common_id, year) %>%
    mutate(
      pc = count / sum(count, na.rm = TRUE),
      outlier_pc = ifelse(!is.na(pc) & pc > 0.8, 1, 0)
    ) %>%
    ungroup()
  
  # Step 4: Combine Outlier Flags
  data <- data %>%
    mutate(outlier = ifelse((outlier_mad == 1 | outlier_pc == 1) & count > 100, 1, 0))
  
  # Step 5: Adjust Outliers
  print("Adjusting outliers...")
  data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id")))) %>%
    mutate(
      count_adjust = ifelse(outlier == 1, median(count, na.rm = TRUE), count),
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
    group_by(indicator_common_id) %>%
    slice_max(order_by = max_percent_change, n = 5, with_ties = FALSE) %>%
    ungroup()
  
  # Step 7: Calculate Monthly Volume Increase/Decrease
  print("Calculating monthly volume changes...")
  volume_increase_data <- data %>%
    group_by(
      admin_area_1, 
      indicator_common_id, 
      month = floor_date(date, "month")  # Ensure 'date' column exists
    ) %>%
    summarise(
      total_volume = sum(count, na.rm = TRUE),
      adjusted_volume = sum(count_adjust, na.rm = TRUE),
      percent_change = 100 * (total_volume - adjusted_volume) / adjusted_volume,
      .groups = "drop"
    )
  
  # Step 8: Create heatmap data dynamically with geographic flexibility
  print("Creating heatmap data...")
  heatmap_data <- data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id) %>%
    summarise(
      total_volume = sum(count, na.rm = TRUE),
      adjusted_volume = sum(count_adjust, na.rm = TRUE),
      percent_change = 100 * (total_volume - adjusted_volume) / adjusted_volume,
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = all_of(geo_cols),
      names_from = indicator_common_id,
      values_from = percent_change,
      values_fill = 0
    ) %>%
    mutate(avg_indicators = rowMeans(select(., -all_of(geo_cols)), na.rm = TRUE)) %>%
    select(all_of(geo_cols), avg_indicators, everything())
  
  # Return all relevant datasets
  return(list(
    outlier_data = data,
    top_outliers_data = top_outliers_data,
    volume_increase_data = volume_increase_data,
    heatmap_data = heatmap_data
  ))
}

# PART 2 COMPLETENESS
completeness_analysis <- function(data, geo_cols) {
  print("Performing completeness analysis (facility-month >0) with dynamic expected months...")
  
  # Step 1: Identify the min and max month for EACH year in the dataset
  year_month_range <- data %>%
    group_by(year) %>%
    summarise(
      min_month = min(month, na.rm = TRUE),
      max_month = max(month, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 2: Build a table of all (year, month) combinations from min_month..max_month for each year
  all_year_months <- year_month_range %>%
    rowwise() %>%
    mutate(
      month_seq = list(seq(from = min_month, to = max_month))
    ) %>%
    unnest(month_seq) %>%
    rename(month = month_seq) %>%
    ungroup()
  
  # Step 3: Cross with all facilities
  all_facilities <- data %>%
    distinct(facility_id)
  
  complete_month_grid <- all_facilities %>%
    crossing(all_year_months)
  
  # Step 4: LEFT JOIN raw data so missing months become NA
  expanded_data <- complete_month_grid %>%
    left_join(data, by = c("facility_id", "year", "month"))
  
  # Step 5: Convert year-month to a Date and add period_id (yyyymm)
  expanded_data <- expanded_data %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      period_id = as.integer(paste0(year, sprintf("%02d", month)))
    ) %>%
    filter(
      !is.na(facility_id),
      !is.na(indicator_common_id),
      !is.na(date)
    )
  
  # Step 6: Summarize monthly reported units
  facility_month_data <- expanded_data %>%
    group_by(
      facility_id, 
      indicator_common_id, 
      year, 
      month, 
      period_id,
      across(all_of(geo_cols))
    ) %>%
    summarise(
      monthly_reported_units = sum(count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      completeness_flag = ifelse(monthly_reported_units > 0, 1, 0)
    )
  
  # Step 7: Count "expected months" for each facility-year
  year_monthly_reported_units_by_facility <- expanded_data %>%
    group_by(facility_id, year) %>%
    summarise(
      expected_facility_months = n_distinct(month),
      .groups = "drop"
    )
  
  # Step 8: Summarize to yearly level
  yearly_summary <- facility_month_data %>%
    group_by(
      facility_id, 
      indicator_common_id, 
      year, 
      across(all_of(geo_cols))
    ) %>%
    summarise(
      reported_facility_months = sum(completeness_flag, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    left_join(
      year_monthly_reported_units_by_facility, 
      by = c("facility_id", "year")
    ) %>%
    mutate(
      completeness_percentage = reported_facility_months / expected_facility_months
    )
  
  # Step 9: Aggregate by geographies
  geography_aggregate <- yearly_summary %>%
    group_by(
      across(all_of(geo_cols)), 
      indicator_common_id, 
      year
    ) %>%
    summarise(
      total_expected_facility_months = sum(expected_facility_months, na.rm = TRUE),
      total_reported_facility_months = sum(reported_facility_months, na.rm = TRUE),
      completeness_percentage = total_reported_facility_months / total_expected_facility_months,
      .groups = "drop"
    )
  
  # Step 10: Return results
  list(
    facility_month_data = facility_month_data,
    yearly_summary       = yearly_summary,
    geography_aggregate  = geography_aggregate
  )
}

# PART 3 ADJUSTMENTS
apply_adjustments <- function(data,
                              outlier_data,
                              completeness_data,
                              geo_cols,
                              adjust_outliers = FALSE,
                              adjust_completeness = FALSE) {
  cat("Applying dynamic adjustments...\n")
  
  # Step 1: Merge “outlier flags” and “count_adjust” into the main data
  data_merged <- data %>%
    left_join(
      outlier_data %>%
        select(
          facility_id, indicator_common_id, year, month,
          outlier, count_adjust
        ),
      by = c("facility_id", "indicator_common_id", "year", "month")
    )
  
  # Step 2: Merge completeness info (assuming completeness_data is at facility-year-indicator level)
  data_merged <- data_merged %>%
    left_join(
      completeness_data %>%
        select(
          facility_id, indicator_common_id, year,
          completeness_percentage
        ),
      by = c("facility_id", "indicator_common_id", "year")
    )
  
  # Step 3: Create a working column “count_working” that starts as the raw “count”
  data_merged <- data_merged %>%
    mutate(count_working = count)  # Keep an original reference to the raw count
  
  # Step 4: If adjust_outliers is TRUE, replace raw counts with outlier-adjusted counts
  if (adjust_outliers) {
    cat(" -> Replacing raw counts with outlier-adjusted counts...\n")
    data_merged <- data_merged %>%
      mutate(
        count_working = if_else(
          !is.na(count_adjust),
          count_adjust,     # If we have an adjusted value for outliers
          count_working     # Otherwise keep the original
        )
      )
  }
  
  # Step 5: If adjust_completeness is TRUE, scale “count_working” by fraction_reported
  #         fraction_reported = completeness_percentage # Fixed error here
  if (adjust_completeness) {
    cat(" -> Scaling counts by completeness fraction...\n")
    data_merged <- data_merged %>%
      mutate(
        fraction_reported = completeness_percentage,
        count_working = if_else(
          !is.na(fraction_reported) & fraction_reported > 0,
          count_working / fraction_reported,  # Scale up if fraction < 1
          count_working                       # If fraction is NA or 0, keep as-is
        )
      )
  }
  
  # Step 6: Rename the final adjusted column to something consistent, e.g. “count_final”
  data_merged <- data_merged %>%
    mutate(count_final = count_working)
  
  # Step 7: Print a summary of the differences
  diff_summary <- data_merged %>%
    mutate(diff = count_final - count) %>%
    summarise(
      n_changed_rows = sum(diff != 0, na.rm = TRUE), #number of rows where adjustments were applied
      mean_diff      = mean(diff, na.rm = TRUE), #average difference between original and adjusted counts
      max_diff       = max(diff, na.rm = TRUE) #maximum difference applied during adjustments
    )
  # Print a summary of the differences between the original and adjusted counts
  cat("Summary of changes from original 'count' to 'count_final':\n")
  cat("  Rows changed:", diff_summary$n_changed_rows, "\n")
  cat("  Mean diff:   ", round(diff_summary$mean_diff, 2), "\n")
  cat("  Max diff:    ", round(diff_summary$max_diff, 2), "\n")
  
  # Step 8: Return the merged dataset with the new “count_final” column
  return(data_merged)
}


# This function applies adjustments for multiple scenarios:
# 1. None: No adjustments.
# 2. Outliers: Only outlier adjustments.
# 3. Completeness: Only completeness scaling.
# 4. Both: Combination of outlier corrections and completeness scaling.
apply_adjustments_scenarios <- function(data,
                                        outlier_data,
                                        completeness_data,
                                        geo_cols) {
  join_cols <- c(geo_cols, "facility_id", "indicator_common_id", "year", "month", "period_id")
  
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  results <- lapply(names(scenarios), function(name) {
    adjustment <- scenarios[[name]]
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
  
  # Merge all four results into one wide dataset
  df_merged <- Reduce(function(x, y) left_join(x, y, by = join_cols), results)
  
  # Add the `date` column to the merged dataset
  df_merged <- df_merged %>%
    mutate(date = as.Date(paste(year, month, "1", sep = "-")))
  
  return(df_merged)
}



# Main Execution ---------------------------------------------------------------------------------
inputs <- load_and_preprocess_data("sierraleone_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols
# Main Execution ---------------------------------------------------------------------------------

print("Running outlier analysis...")
outlier_data <- outlier_analysis(data, geo_cols)

print("Running completeness analysis...")
completeness_results <- completeness_analysis(outlier_data$outlier_data, geo_cols)

print("Running adjustments analysis...")
adjusted_data <- apply_adjustments_scenarios(
  data              = data,
  outlier_data      = outlier_data$outlier_data,   # or outlier_data if named differently
  completeness_data = completeness_results$yearly_summary,
  geo_cols          = geo_cols
)


# ADJUSTED DATA // National Trends Visualization --------------------------------------------------
generate_adjusted_national_results <- function(adjusted_data) {
  print("Generating national-level results for adjusted data...")
  
  # Extract unique indicators from the dataset
  unique_indicators <- unique(adjusted_data$indicator_common_id)
  
  # Process each indicator separately to generate scenario-based trends
  national_results <- lapply(unique_indicators, function(indicator) {
    # Filter data for the current indicator
    indicator_data <- adjusted_data %>%
      filter(indicator_common_id == indicator) %>%
      pivot_longer(
        cols = starts_with("count_final"),  # Transform different adjustment scenarios into long format
        names_to = "scenario",
        values_to = "total_volume"
      ) %>%
      # Recode scenario names to make them more descriptive
      mutate(
        scenario = recode(
          scenario,
          "count_final_none" = "Unadjusted",
          "count_final_outliers" = "Outlier Adjusted",
          "count_final_completeness" = "Completeness Adjusted",
          "count_final_both" = "Both Adjusted"
        )
      ) %>%
      # Group by date and adjustment scenario to aggregate total volumes
      group_by(date, scenario) %>%
      summarise(
        total_volume = sum(total_volume, na.rm = TRUE),  # Aggregate total volume across all facilities
        .groups = "drop"
      ) %>%
      # Add the indicator column for identification
      mutate(
        indicator = indicator
      )
    
    return(indicator_data)
  }) %>%
    # Combine results for all indicators into a single dataset
    bind_rows()
  
  return(national_results)
}

plot_adjusted_national_grid <- function(national_data) {
  # Create a combined grid plot for all indicators and scenarios
  ggplot(national_data, aes(x = date, y = total_volume, color = scenario)) +
    geom_line(size = 0.8) +  # Plot the trend lines for each scenario
    facet_wrap(~indicator, scales = "free_y") +  # Create one subplot per indicator
    labs(
      title = "Adjusted volumes by adjustment scenario & indicator",
      subtitle = "Changes in service volumes over time for different adjustment scenarios",
      x = "Year",
      y = "Total Volume",
      caption = "'Unadjusted' (raw data), 'Outlier Adjusted' (extreme values corrected), 'Completeness Adjusted' (scaled for underreporting), and 'Both Adjusted' (combined adjustments)."
    ) +
    scale_color_manual(
      values = c(
        "Unadjusted" = "#0099cc",  # Light blue for unadjusted data
        "Outlier Adjusted" = "#bb99ff",  # Purple for outlier-adjusted data
        "Completeness Adjusted" = "#ffe6b3",  # Orange for completeness-adjusted data
        "Both Adjusted" = "#ff8080"  # Red for both adjustments
      ),
      breaks = c(
        "Unadjusted",
        "Outlier Adjusted",
        "Completeness Adjusted",
        "Both Adjusted"
      ),
      labels = c(
        "Unadjusted",
        "Outlier Adjusted",
        "Completeness Adjusted",
        "Both Adjusted"
      )
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),  # Style facet labels
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      legend.position = "bottom"  # Move legend to the bottom of the plot
    )
}

# Generate and plot adjusted national trends
adjusted_national_results <- generate_adjusted_national_results(adjusted_data)
print("Plotting adjusted national grid...")
adjusted_national_plot <- plot_adjusted_national_grid(adjusted_national_results)
print(adjusted_national_plot)



# -------------------------------------------------------------------------------------------------
# Save Outputs ------------------------------------------------------------------------------------
print("Saving adjusted data...")
write.csv(adjusted_data, "adjusted_data.csv", row.names = FALSE)
write.csv(adjusted_national_results, "adjusted_national_results.csv", row.names = FALSE)

print("Saving all plots...")
ggsave("adjusted_data.png", plot = adjusted_national_plot, width = 12, height = 8)

print("Adjustment analysis completed and outputs saved.")