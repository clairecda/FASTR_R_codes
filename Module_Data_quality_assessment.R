# CB - R code FASTR PROJECT
# Last edit: 2024 Dec 23


#DATA: sierraleone_imported_dataset.csv


# FILE: output_outliers.csv
# FILE: top_outliers_data.csv
# FILE: volume_increase_data.csv
# FILE: output_consistency.csv
# FILE: completeness_summary.csv
# FILE: completeness_long_format.csv
# FILE: completeness_aggregate.csv
# FILE: dqa_data.csv
# FILE: dqa_summary.csv


# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)


# Define Functions -----------------------------------------------------------
load_and_preprocess_data <- function(file_path) {
  print("Loading and preprocessing data...")
  data <- read.csv(file_path)
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  data <- data %>%
    mutate(date = ymd(paste(year, month, "1", sep = "-")))
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
    pnc = c("delivery", "pnc1")
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
  date_summary <- data %>%
    group_by(indicator_common_id, year) %>%
    summarise(
      date_min = min(date, na.rm = TRUE),
      date_max = max(date, na.rm = TRUE),
      .groups = "drop"
    )
  
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id", "year")))) %>%
    summarise(
      volume = sum(volume, na.rm = TRUE),
      volIM = sum(volIM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(date_summary, by = c("indicator_common_id", "year"))
  
  # Clean and prepare indicator types
  aggregated_data <- aggregated_data %>%
    mutate(indictemp = str_replace_all(indicator_common_id, " ", "_")) %>%
    filter(!str_detect(indictemp, "\\+")) %>%
    drop_na(any_of(tail(geo_cols, 1)))
  
  # Step 4: Calculate Consistency Ratios
  print("Calculating consistency ratios...")
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols, "year"),
      names_from = indictemp,
      values_from = c(volume, volIM),
      names_sep = "_",
      values_fill = list(volume = 0, volIM = 0)
    )
  
  # Add consistency calculations dynamically based on pairs_to_check
  if (!is.null(pairs_to_check$anc)) {
    wide_data <- wide_data %>%
      mutate(
        ratio_anc1 = if_else(volume_anc4 > 0, volume_anc1 / volume_anc4, NA_real_),
        sratio_anc1 = if_else(!is.na(ratio_anc1) & ratio_anc1 > 1, 1, 0)
      )
  } else {
    wide_data <- wide_data %>%
      mutate(sratio_anc1 = NA_real_)
  }
  
  if (!is.null(pairs_to_check$delivery)) {
    wide_data <- wide_data %>%
      mutate(
        ratio_delivery = if_else(volume_delivery > 0, volume_bcg / volume_delivery, NA_real_),
        sratio_delivery = if_else(
          !is.na(ratio_delivery) & ratio_delivery >= 0.7 & ratio_delivery <= 1.3, 1, 0
        )
      )
  } else {
    wide_data <- wide_data %>%
      mutate(sratio_delivery = NA_real_)
  }
  
  if (!is.null(pairs_to_check$pnc)) {
    wide_data <- wide_data %>%
      mutate(
        ratio_pnc1 = if_else(volume_delivery > 0, volume_pnc1 / volume_delivery, NA_real_),
        sratio_pnc1 = if_else(
          !is.na(ratio_pnc1) & ratio_pnc1 >= 0.7 & ratio_pnc1 <= 1.3, 1, 0
        )
      )
  } else {
    wide_data <- wide_data %>%
      mutate(sratio_pnc1 = NA_real_)
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
      sconsistency = case_when(
        ratio_type == "ratio_anc1" ~ sratio_anc1,
        ratio_type == "ratio_delivery" ~ sratio_delivery,
        ratio_type == "ratio_pnc1" ~ sratio_pnc1,
        TRUE ~ NA_real_
      )
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
dqa_analysis <- function(completeness_data, consistency_data, geo_cols) {
  print("Performing DQA analysis...")
  
  # Step 1: Check if Consistency Data is Available
  if (nrow(consistency_data) == 0) {
    print("Skipping DQA analysis: Consistency analysis was not performed.")
    return(list(
      dqa_data = data.frame(),
      dqa_summary = data.frame(message = "DQA analysis skipped: No consistency data available.")
    ))
  }
  
  # Step 2: Map Consistency Data to Indicators
  consistency_mapping <- tibble(
    ratio_type = c("ratio_anc1", "ratio_delivery", "ratio_pnc1"),
    indicator_common_id = c("anc1", "delivery", "pnc1")
  )
  
  consistency_data <- consistency_data %>%
    inner_join(consistency_mapping, by = "ratio_type")
  
  # Step 3: Merge Completeness with Consistency
  merged_data <- completeness_data %>%
    left_join(
      consistency_data %>%
        select(any_of(c(geo_cols, "year", "indicator_common_id", "sconsistency"))),
      by = c(geo_cols, "year", "indicator_common_id")
    )
  
  # Step 4: Apply DQA Rules
  print("Applying DQA rules")
  merged_data <- merged_data %>%
    mutate(
      dqa_flag = case_when(
        indicator_common_id %in% c("opd", "penta1", "anc1") & completeness_percentage == 100 ~ 1,
        indicator_common_id == "penta1" & sconsistency == 0 ~ 0,
        indicator_common_id == "anc1" & sconsistency == 0 ~ 0,
        TRUE ~ 0
      )
    )
  
  # Step 5: Summarize DQA Results
  print("Summarizing DQA results")
  dqa_summary <- merged_data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id) %>%
    summarise(
      avg_dqa_flag = mean(dqa_flag, na.rm = TRUE),
      total_flags = sum(dqa_flag, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(dqa_data = merged_data, dqa_summary = dqa_summary))
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
dqa_results <- dqa_analysis( completeness_results$long_format, consistency_data, geo_cols)

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
    values = rescale(c(0, 25, 50, 90, 100)),  # Adjust thresholds for your data
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


# Save Outputs
print("Saving all data outputs from outlier analysis...")
write.csv(outlier_data$outlier_data, "output_outliers.csv", row.names = FALSE)
write.csv(outlier_data$top_outliers_data, "top_outliers_data.csv", row.names = FALSE)
write.csv(outlier_data$volume_increase_data, "volume_increase_data.csv", row.names = FALSE)

print("Saving all data outputs from consistency analysis...")
write.csv(consistency_data, "output_consistency.csv", row.names = FALSE)

print("Saving all data outputs from conpleteness analysis...")
write.csv(completeness_results$summary, "completeness_summary.csv", row.names = FALSE)
write.csv(completeness_results$long_format, "completeness_long_format.csv", row.names = FALSE)
write.csv(completeness_results$aggregate, "completeness_aggregate.csv", row.names = FALSE)

print("Saving all data outputs from DQA analysis...")
write.csv(dqa_results$dqa_data, "dqa_data.csv", row.names = FALSE)
write.csv(dqa_results$dqa_summary, "dqa_summary.csv", row.names = FALSE)

print("DQA analysis completed and outputs saved.")
