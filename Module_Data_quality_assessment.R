# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 02

# DATA: sierraleone_imported_dataset.csv

# FILE: output_outliers.csv           # Detailed facility-level data with identified outliers and adjusted volumes.
# FILE: top_outliers_data.csv         # Top outliers based on percentage change in volume, highlighting extreme deviations.
# FILE: volume_increase_data.csv      # Monthly summary of volume changes due to outlier adjustments.
# FILE: output_consistency.csv        # Results of consistency analysis, including ratio calculations and consistency flags.
# FILE: completeness_summary.csv      # Aggregated completeness data at various geographic levels (e.g., national, regional).
# FILE: completeness_long_format.csv  # Facility-level completeness data in a detailed long format, including reported and expected months.
# FILE: completeness_aggregate.csv    # Aggregated completeness data summarizing facility-month reporting across geographic levels.
# FILE: facility_dqa.csv              # Facility-level results from DQA analysis, including completeness, outlier, and consistency flags.
# FILE: dqa_summary.csv               # Summary of DQA results by geography and indicator, with aggregated scores and flags.
# FILE: overall_dqa.csv               # Geographic-level summary of overall data quality, including aggregated DQA scores and metrics.
# FILE: adjusted_data.csv             # Dataset including facility-level adjusted volumes for all adjustment scenarios.
# FILE: adjusted_national_results.csv # National-level dataset summarizing adjusted service volumes for each scenario.


# IMAGE: outliers_heatmap.png         # Heatmap visualization showing the percent change due to outliers by administrative region and indicator.
# IMAGE: completeness_heatmap.png     # Heatmap visualization representing the completeness percentage by administrative region and indicator.
# IMAGE: bar_chart_volume_change.png  # Bar chart showing monthly percentage changes in volume due to outlier adjustments, by region and indicator.
# IMAGE: adjusted_data.png            # Grid plot visualizing national-level trends for all indicators under n=4 adjustment scenarios.
# IMAGE: dqa_heatmap_results.png      # Heatmap visualization showing percentage of facilities meeting DQA criteria aggregated by region (level 2) and year.

# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)

# Define Functions -----------------------------------------------------------
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

# PART 2 CONSISTENCY
consistency_analysis <- function(data, geo_cols) {
  print("Performing consistency analysis...")
  
  # Step 1: Identify Available Indicators
  print("Checking available indicators...")
  indicators <- unique(data$indicator_common_id)
  print(paste("Available indicators:", paste(indicators, collapse = ", ")))
  
  # Define required pairs for consistency checks
  required_pairs <- list(
    anc = c("anc1", "anc4"),
    delivery = c("delivery", "bcg"),
    pnc = c("delivery", "pnc1"),
    penta = c("penta1", "penta3")
  )
  
  # Filter out missing indicator pairs based on available data
  pairs_to_check <- list()
  for (key in names(required_pairs)) {
    if (all(required_pairs[[key]] %in% indicators)) {
      pairs_to_check[[key]] <- required_pairs[[key]]
    } else {
      print(paste("Skipping consistency check for", key, "due to missing indicators."))
    }
  }
  
  # If no valid pairs are available, return an empty data frame
  if (length(pairs_to_check) == 0) {
    print("No valid indicator pairs available for consistency analysis.")
    return(data.frame())
  }
  
  # Step 2: Exclude Outliers
  print("Excluding outliers...")
  data <- data %>%
    mutate(volume = ifelse(outlier == 1, NA, count))
  
  # Step 3: Aggregate Data
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id", "year")))) %>%
    summarise(
      volume = sum(volume, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot data to wide format for ratio calculations
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols, "year"),
      names_from = "indicator_common_id",
      values_from = "volume",
      values_fill = list(volume = 0)
    )
  
  # Debug: Print wide data column names
  print("Wide data column names:")
  print(colnames(wide_data))
  
  # Step 4: Calculate Consistency Ratios
  print("Calculating consistency ratios...")
  for (pair_name in names(pairs_to_check)) {
    pair <- pairs_to_check[[pair_name]]
    col1 <- pair[1] # First indicator
    col2 <- pair[2] # Second indicator
    
    # Check if both columns exist in wide_data
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      ratio_name <- paste0("ratio_", col1) # Name for the ratio
      sratio_name <- paste0("sratio_", col1) # Name for the binary flag
      
      # Add ratio and flag dynamically
      wide_data <- wide_data %>%
        mutate(
          !!ratio_name := if_else(
            !!sym(col2) > 0,
            !!sym(col1) / !!sym(col2),
            NA_real_
          ),
          !!sratio_name := case_when(
            pair_name == "delivery" ~ (!!sym(ratio_name) >= 0.7 & !!sym(ratio_name) <= 1.3),
            TRUE ~ !!sym(ratio_name) > 1
          )
        )
    } else {
      print(paste("Skipping pair:", col1, "and", col2, "- columns not found"))
    }
  }
  
  # Reshape Data Back to Long Format
  print("Reshaping data back to long format...")
  long_data <- wide_data %>%
    pivot_longer(
      cols = starts_with("ratio_"),
      names_to = "ratio_type",
      values_to = "consistency_ratio"
    ) %>%
    mutate(
      sconsistency = if_else(str_starts(ratio_type, "sratio_"), 1, 0)
    ) %>%
    select(any_of(c(geo_cols, "year", "ratio_type", "consistency_ratio", "sconsistency")))
  
  return(long_data)
}

# PART 3 COMPLETENESS
completeness_analysis <- function(data, geo_cols) {
  print("Performing completeness analysis...")
  
  # Step 1: Ensure 'date' and 'year' columns exist
  data <- data %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      panelvar = paste(indicator_common_id, facility_id, sep = "_")
    ) %>%
    filter(!is.na(date) & !is.na(panelvar))  # Keep rows with valid dates and panelvars
  
  # Step 2: Generate full facility-year combinations
  print("Generating expected facility-year combinations...")
  facilities <- unique(data$facility_id)
  indicators <- unique(data$indicator_common_id)
  years <- unique(data$year)
  
  all_combinations <- expand.grid(
    facility_id = facilities,
    indicator_common_id = indicators,
    year = years,
    KEEP.OUT.ATTRS = FALSE
  )
  
  # Step 3: Count reported facility-months per year for each facility-indicator
  print("Counting reported facility-months per facility-indicator...")
  monthly_reporting <- data %>%
    group_by(facility_id, indicator_common_id, year) %>%
    summarise(reported_facility_months = n(), .groups = "drop")  # Count facility-months reported
  
  # Step 4: Merge expected combinations and add geographic columns
  print("Merging expected combinations and adding geographic information...")
  yearly_data <- all_combinations %>%
    left_join(monthly_reporting, by = c("facility_id", "indicator_common_id", "year")) %>%
    mutate(
      reported_facility_months = replace_na(reported_facility_months, 0),  # Replace missing values with 0
      expected_facility_months = 12,  # Expect 12 months per year
      completeness_percentage = (reported_facility_months / expected_facility_months) * 100
    )
  
  geo_info <- data %>%
    select(facility_id, all_of(geo_cols)) %>%
    distinct()
  
  yearly_data <- yearly_data %>%
    left_join(geo_info, by = "facility_id")
  
  # Step 5: Aggregate completeness over geography
  print("Aggregating completeness over geography...")
  aggregate_data <- yearly_data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id, year) %>%
    summarise(
      total_expected_facility_months = sum(expected_facility_months),
      total_reported_facility_months = sum(reported_facility_months),
      completeness_percentage = (total_reported_facility_months / total_expected_facility_months) * 100,
      .groups = "drop"
    )
  
  # Step 6: Reorder columns in long_format
  print("Reordering columns in long_format...")
  long_format <- yearly_data %>%
    select(all_of(geo_cols), everything())  # Ensure geo_cols are at the beginning
  
  return(list(summary = aggregate_data, long_format = long_format))
}

# PART 4 DQA
# CB ... DQA - combining completeness, outliers, and consistency - is consistently applied at the annual level: True?
dqa_analysis <- function(completeness_data, consistency_data, outlier_data, geo_cols) {
  print("Performing updated DQA analysis...")
  
  # Step 1: Map all ratio types to indicators in consistency_data
  ratio_mapping <- tibble(
    ratio_type = c("ratio_anc1", "ratio_delivery", "ratio_pnc1", "ratio_penta1"),
    indicator_common_id = c("anc1", "delivery", "pnc1", "penta1")
  )
  
  consistency_data <- consistency_data %>%
    left_join(ratio_mapping, by = "ratio_type")
  
  # Step 2: Deduplicate Inputs
  completeness_data <- completeness_data %>%
    distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE)
  
  consistency_data <- consistency_data %>%
    distinct(across(all_of(c(geo_cols, "year", "indicator_common_id"))), .keep_all = TRUE)
  
  outlier_data <- outlier_data %>%
    distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE)
  
  # Step 3: Merge Completeness, Consistency, and Outliers
  print("Merging completeness, consistency, and outlier data...")
  merged_data <- completeness_data %>%
    left_join(
      outlier_data %>% select(all_of(geo_cols), facility_id, year, indicator_common_id, outlier_flag = outlier),
      by = c(geo_cols, "facility_id", "year", "indicator_common_id")
    ) %>%
    left_join(
      consistency_data %>% select(all_of(geo_cols), year, indicator_common_id, sconsistency),
      by = c(geo_cols, "year", "indicator_common_id")
    )
  
  # Step 4: Enhanced DQA Flags
  print("Calculating enhanced DQA flags...")
  merged_data <- merged_data %>%
    mutate(
      dqa_temp = case_when(
        completeness_percentage >= 90 &  # Flexible completeness threshold
          outlier_flag == 0 & 
          (is.na(sconsistency) | sconsistency == 1) ~ 1,
        TRUE ~ 0
      )
    )
  
  # Step 5: Aggregate DQA Results
  print("Aggregating DQA results...")
  dqa_summary <- merged_data %>%
    group_by(across(all_of(c(geo_cols, "year", "indicator_common_id")))) %>%
    summarise(
      dqa_score = mean(dqa_temp, na.rm = TRUE),  # Proportion of facilities passing DQA
      total_flags = sum(dqa_temp, na.rm = TRUE),  # Count of facilities passing DQA
      completeness_avg = mean(completeness_percentage, na.rm = TRUE),  # Avg completeness
      outlier_count = sum(outlier_flag, na.rm = TRUE),  # Outlier flags
      consistency_failures = sum(ifelse(is.na(sconsistency), 0, 1 - sconsistency), na.rm = TRUE),  # Count of failed consistency checks
      .groups = "drop"
    )
  
  # Step 6: Overall DQA Results at Geography-Year Level
  print("Calculating overall DQA scores...")
  overall_dqa <- dqa_summary %>%
    group_by(across(all_of(c(geo_cols, "year")))) %>%
    summarise(
      overall_dqa_score = mean(dqa_score, na.rm = TRUE),
      total_indicators = n(),
      completeness_avg = mean(completeness_avg, na.rm = TRUE),
      total_outlier_count = sum(outlier_count, na.rm = TRUE),
      total_consistency_failures = sum(consistency_failures, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Return Updated Results
  return(list(
    facility_dqa = merged_data,
    dqa_summary = dqa_summary,
    overall_dqa = overall_dqa
  ))
}

# PART 5: Dynamic Adjustments (Work-in-Progress) ---------------------------------------
# This section includes functions to dynamically adjust raw data for:
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
  #         fraction_reported = completeness_percentage / 100
  if (adjust_completeness) {
    cat(" -> Scaling counts by completeness fraction...\n")
    data_merged <- data_merged %>%
      mutate(
        fraction_reported = completeness_percentage / 100,
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



# Main Execution --------------------------------------------------------------
inputs <- load_and_preprocess_data("sierraleone_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols
# Main Execution --------------------------------------------------------------

print("Running outlier analysis...")
outlier_data <- outlier_analysis(data, geo_cols)

print("Running consistency analysis...")
consistency_data <- consistency_analysis(outlier_data$outlier_data, geo_cols)

print("Running completeness analysis...")
completeness_results <- completeness_analysis(outlier_data$outlier_data, geo_cols)

print("Running DQA analysis...")
dqa_results <- dqa_analysis(
  completeness_data = completeness_results$long_format,
  consistency_data = consistency_data,
  outlier_data = outlier_data$outlier_data,
  geo_cols = geo_cols)

print("Running adjustments analysis...")
adjusted_data <- apply_adjustments_scenarios(
  data              = data,
  outlier_data      = outlier_data$outlier_data,   # or outlier_data if named differently
  completeness_data = completeness_results$long_format,
  geo_cols          = geo_cols
)


# Visualization  --------------------------------------------------------------
#SETUP
viz_colors <- c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")

# Outliers Heatmap (PART1)------------------------------------------------
# Create the heatmap
heatmap_outliers <- outlier_data$heatmap_data %>%
  pivot_longer(
    cols = -all_of(geo_cols), # Exclude all geographic columns dynamically
    names_to = "indicator", 
    values_to = "percent_change"
  ) %>%
  mutate(
    indicator = factor(indicator, levels = c("avg_indicators", sort(setdiff(unique(indicator), "avg_indicators"))))
  ) %>%
  ggplot(aes(x = indicator, y = admin_area_2, fill = percent_change)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#1a9641", mid = "#ffffbf", high = "#d7191c",
    midpoint = 0, na.value = "grey50"
  ) +
  labs(
    title = "Percent Change of Volume Due to Outliers",
    x = "Indicator",
    y = "Administrative Area",
    fill = "% Change"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid = element_blank() # Remove gridlines
  )
print(heatmap_outliers)


# Volume Increase Due to Outliers ------------------------
bar_chart <- ggplot(outlier_data$volume_increase_data, aes(x = month, y = percent_change, fill = admin_area_1)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~indicator_common_id, scales = "free_y") +
  labs(
    title = "Volume Change Due to Outliers",
    x = "Month",
    y = "% Change",
    fill = "Administrative Area"
  ) +
  theme_minimal()
print(bar_chart)


# Completeness Heatmap (PART3)------------------------------------------------
print("Creating completeness heatmap...")
heatmap_plot <- ggplot(completeness_results$summary, aes(x = indicator_common_id, y = admin_area_2, fill = completeness_percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = viz_colors,
    values = rescale(c(0, 25, 50, 90, 100)),  # Adjust thresholds
    limits = c(0, 100),                       # Ensure the scale is [0, 100]
    name = "Completeness (%)"
  ) +
  labs(
    title = "Completeness by Indicator and Region",
    x = "Indicator",
    y = "Region",
    fill = "Completeness (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(heatmap_plot)


# DQA Heatmap (Part 4)
generate_dqa_heatmap <- function(dqa_summary, geo_col = "admin_area_2", year_col = "year") {
  print("Generating DQA heatmap data...")
  
  # Step 1: Aggregate DQA Results
  dqa_aggregated <- dqa_summary %>%
    group_by(!!sym(geo_col), !!sym(year_col)) %>%
    summarise(
      adequate_quality_percentage = mean(dqa_score, na.rm = TRUE) * 100,  # Convert to percentage
      .groups = "drop"
    )
  
  print("DQA heatmap data aggregation complete.")
  
  # Step 2: Generate Heatmap Plot
  heatmap_plot <- ggplot(dqa_aggregated, aes_string(x = year_col, y = geo_col, fill = "adequate_quality_percentage")) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
      values = scales::rescale(c(0, 25, 50, 75, 100)),
      limits = c(0, 100),
      name = "Facilities Meeting\nDQA Criteria (%)"
    ) +
    labs(
      title = "Data Quality Assessment",
      subtitle = "Percentage of facilities meeting DQA criteria, aggregated by region and year",
      x = "Year",
      y = "Administrative Region (Level 2)",
      fill = "Adequate Quality (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "italic"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid = element_blank()
    )
  
  print("DQA plot created.")
  
  return(list(
    heatmap_data = dqa_aggregated,
    heatmap_plot = heatmap_plot
  ))
}

dqa_heatmap_results <- generate_dqa_heatmap(dqa_results$dqa_summary)
heatmap_dqa <- dqa_heatmap_results$heatmap_plot  # Access the plot for further use
print(heatmap_dqa)



# ADJUSTED DATA // National Trends Visualization (PART 5) -------------------------------------------------------------
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
      title = "Adjusted Data Trends by Scenario",
      subtitle = "Visualizing changes in service volumes over time for different adjustment scenarios",
      x = "Year",
      y = "Total Volume",
      caption = "'Unadjusted' (raw data), 'Outlier Adjusted' (extreme values corrected), 'Completeness Adjusted' (scaled for underreporting), and 'Both Adjusted' (combined adjustments)."
    ) +
    scale_color_manual(
      values = c(
        "Unadjusted" = "lightblue",  # Light blue for unadjusted data
        "Outlier Adjusted" = "blue",  # Blue for outlier-adjusted data
        "Completeness Adjusted" = "orange",  # Orange for completeness-adjusted data
        "Both Adjusted" = "red"  # Red for both adjustments
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
print("Saving all data outputs from outlier analysis...")
write.csv(outlier_data$outlier_data, "output_outliers.csv", row.names = FALSE)
write.csv(outlier_data$top_outliers_data, "top_outliers_data.csv", row.names = FALSE)
write.csv(outlier_data$volume_increase_data, "volume_increase_data.csv", row.names = FALSE)

print("Saving all data outputs from consistency analysis...")
write.csv(consistency_data, "output_consistency.csv", row.names = FALSE)

print("Saving all data outputs from conpleteness analysis...")
write.csv(completeness_results$summary, "completeness_summary.csv", row.names = FALSE)
write.csv(completeness_results$long_format, "completeness_long_format.csv", row.names = FALSE)


print("Saving all data outputs from DQA analysis...")
write.csv(dqa_results$facility_dqa, "facility_dqa.csv", row.names = FALSE)
write.csv(dqa_results$dqa_summary, "dqa_summary.csv", row.names = FALSE)
write.csv(dqa_results$overall_dqa, "overall_dqa.csv", row.names = FALSE)


print("Saving adjusted data...")
write.csv(adjusted_data, "adjusted_data.csv", row.names = FALSE)
write.csv(adjusted_national_results, "adjusted_national_results.csv", row.names = FALSE)

print("Saving all plots...")
ggsave("outliers_heatmap.png", plot = heatmap_outliers, width = 12, height = 8)
ggsave("completeness_heatmap.png", plot = heatmap_plot, width = 12, height = 8)
ggsave("bar_chart_volume_change.png", plot = bar_chart, width = 12, height = 8)
ggsave("dqa_heatmap_results.png", plot = heatmap_dqa, width = 12, height = 8)
ggsave("adjusted_data.png", plot = adjusted_national_plot, width = 12, height = 8)

print("DQA analysis completed and outputs saved.")