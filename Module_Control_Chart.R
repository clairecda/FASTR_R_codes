# CB - R code FASTR PROJECT - Control Chart Analysis
# Last edit: 2024 Dec 24

# DATA: sierraleone_imported_dataset.csv

# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.
# FILE: national_results.csv            # National-level trends with control limits for each indicator.
# IMAGE: national_grid_plot.png         # Grid plot of national-level trends for all indicators, with control limits.

# Description of current approach:
# - De-seasonalized volumes are calculated by subtracting monthly averages (volume_predict = total_volume - monthly_mean).
# - A 12-month rolling average (rollmean()) is used to smooth the data and highlight trends.
# - Next step: Implement regression-based deseasonalization to replicate the Stata script.


# Required Libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)  # For interpolation

# Define Functions -----------------------------------------------------------
# PART 1 Load and Preprocess Data
load_and_preprocess_data <- function(file_path) {
  print("Loading and preprocessing data...")
  data <- read.csv(file_path)
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  data <- data %>%
    mutate(date = ymd(paste(year, month, "1", sep = "-")))
  return(list(data = data, geo_cols = geo_cols))
}

# PART 2 Outlier Analysis
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

# PART 3 Control Chart Analysis Function --------------------------------------------
control_chart_analysis <- function(cleaned_data, geo_cols) {
  print("Performing control chart analysis...")
  
  # Step 1: Aggregate data by indicator, province, and time
  aggregated_data <- cleaned_data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id, date) %>%
    summarise(total_volume = sum(count_adjust, na.rm = TRUE), .groups = "drop")
  
  # Step 2: Create panel variable for grouping
  aggregated_data <- aggregated_data %>%
    mutate(panelvar = paste(indicator_common_id, admin_area_1, sep = "_"))
  
  # Step 3: Fill missing dates and interpolate missing volumes
  complete_data <- aggregated_data %>%
    group_by(panelvar) %>%
    complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month")) %>%
    fill(total_volume, .direction = "downup") %>%
    ungroup()
  
  # Step 4: Deseasonalize and calculate control limits
  deseasonalized_data <- complete_data %>%
    mutate(month = month(date)) %>%
    group_by(panelvar, month) %>%
    mutate(monthly_mean = mean(total_volume, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(volume_predict = total_volume - monthly_mean) %>%
    group_by(panelvar) %>%
    mutate(
      volume_smooth = rollmean(volume_predict, k = 12, fill = NA),  # 12-month rolling average
      residual = volume_predict - volume_smooth,
      sd_residual = sd(residual, na.rm = TRUE),
      control = residual / sd_residual,
      tag = ifelse(abs(control) >= 2, 1, 0)  # Anomalies if control >= 2 SD
    ) %>%
    ungroup()
  
  return(deseasonalized_data)
}

# PART 4 National Aggregation
generate_national_results <- function(cleaned_data) {
  print("Generating national-level results with control limits...")
  unique_indicators <- unique(cleaned_data$indicator_common_id)
  
  national_results <- lapply(unique_indicators, function(indicator) {
    indicator_data <- cleaned_data %>%
      filter(indicator_common_id == indicator) %>%
      group_by(date) %>%
      summarise(
        total_volume = sum(count_adjust, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate control limits
    mean_volume <- mean(indicator_data$total_volume, na.rm = TRUE)
    sd_volume <- sd(indicator_data$total_volume, na.rm = TRUE)
    indicator_data <- indicator_data %>%
      mutate(
        indicator = indicator,
        UCL = mean_volume + 3 * sd_volume,
        LCL = mean_volume - 3 * sd_volume
      )
  }) %>%
    bind_rows()
  
  return(national_results)
}


# PART 5 Visualizations
plot_national_grid <- function(national_data) {
  # Create a combined grid plot for all indicators
  ggplot(national_data, aes(x = date, y = total_volume)) +
    geom_line(size = 1.2, color = "black") +
    geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "blue") +
    facet_wrap(~indicator, scales = "free_y") +  # Create one subplot per indicator
    labs(
      title = "Control chart",
      subtitle = "Change in the volumes of service over time - with control limits (UCL and LCL)",
      x = "Year",
      y = "Total Volume",
      caption = "Upper Control Limit = red dashed line, Lower Control Limit = blue dashed line"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),  # Adjust facet labels
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    )
}

# Main Script -----------------------------------------------------------------
inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

# Perform outlier analysis
print("Running outlier analysis...")
outlier_results <- outlier_analysis(data, geo_cols)
cleaned_data <- outlier_results$outlier_data

# Perform control chart analysis
print("Running control chart analysis...")
control_chart_results <- control_chart_analysis(cleaned_data, geo_cols)

# Generate national results
print("Generating national results...")
national_results <- generate_national_results(cleaned_data)

# Visualize all indicators in a single grid plot
print("Visualizing all indicators in a single grid plot...")
national_grid_plot <- plot_national_grid(national_results)
print(national_grid_plot)

# Save Outputs
print("Saving plot and results...")
ggsave("national_grid_plot.png", plot = national_grid_plot, width = 12, height = 8)
write.csv(control_chart_results, "control_chart_results.csv", row.names = FALSE)
write.csv(national_results, "national_results.csv", row.names = FALSE)

