# CB - R code FASTR PROJECT - Control Chart Analysis
# Last edit: 2025 Jan 10

# DATA: guinea_imported_dataset.csv

# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.


# FILE: indicator_results.csv           # Indicator-level trends with control limits.
# IMAGE: indicator_grid_plot.png        # Grid plot of indicator-level trends with control limits.

# NOTES/VALUE COLUMNS:
# `control_chart_results` = results of the control chart analysis at the facility level.
#   - `count_adjust`: Observed count of health services after outlier adjustment.
#   - `count_predict`: Deseasonalized predicted count, derived using regression-based methods.
#   - `residual`: Deviation of `count_adjust` from `count_predict`.
#   - `sd_residual`: Standard deviation of residuals for anomaly detection.
#   - `control`: Standardized residual, calculated as `residual / sd_residual`.
#   - `tag`: Binary flag for anomalies (`1` = anomaly, `0` = normal).


# `indicator_results` contains aggregated results of the control chart analysis at the national (admin_area_1) & indicator level. Key columns include:
#   - `total_count`: Aggregated observed count across all facilities for the specific indicator and time period.
#   - `predicted_count`: Aggregated deseasonalized count across all facilities for the specific indicator and time period.
#   - `UCL` (Upper Control Limit): Upper threshold for identifying anomalies, calculated as the mean + 3 SD of `total_count`.
#   - `LCL` (Lower Control Limit): Lower threshold for identifying anomalies, calculated as the mean - 3 SD of `total_count` (with no negative values).

# VISUALIZATION:
# The grid plot visualizes indicator-level trends:
#   - Black line (Total Count): The observed count of services provided, adjusted for outliers.
#   - Green dashed line (Predicted Count): The deseasonalized count derived from regression-based methods.
#   - Red points: Detected anomalies.
#   - Red dashed line (UCL): Upper Control Limit.
#   - Blue dashed line (LCL): Lower Control Limit.


# Required Libraries ------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(stats)   # For ts() and stl()

# Define Functions --------------------------------------------------------------------------------------------------
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
  
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(
      median_count = median(count, na.rm = TRUE),
      mad_count = ifelse(!is.na(count), mad(count[count >= median_count], na.rm = TRUE), NA),
      mad_residual = ifelse(!is.na(mad_count) & mad_count > 0, abs(count - median_count) / mad_count, NA),
      outlier = ifelse(!is.na(mad_residual) & mad_residual > 20, 1, 0)
    ) %>%
    ungroup()
  
  data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id")))) %>%
    mutate(count_adjust = ifelse(outlier == 1, median(count, na.rm = TRUE), count)) %>%
    ungroup()
  
  return(data)
}

# PART 3: Control Chart Analysis (Aligned with Stata Methodology)
control_chart_analysis <- function(cleaned_data, geo_cols) {
  print("Performing control chart analysis with iteration over indicators and regions...")
  
  # Identify the latest available date
  latest_date <- max(cleaned_data$date, na.rm = TRUE)
  
  # Ensure continuous time series for each panel only up to the latest available date
  cleaned_data <- cleaned_data %>%
    group_by(indicator_common_id, across(all_of(geo_cols))) %>%
    complete(
      date = seq(min(date, na.rm = TRUE), latest_date, by = "month")  # Use latest_date here
    ) %>%
    mutate(
      # Fill missing values
      count_adjust = if_else(is.na(count_adjust), 0, count_adjust),
      year = year(date),
      month = month(date),
      period_id = if_else(!is.na(date), format(date, "%Y%m"), NA_character_)
    ) %>%
    ungroup()
  # Verify
  print(colnames(cleaned_data))  # Ensure indicator_common_id is still present
  
  # Proceed with deseasonalization and smoothing...
  deseasonalized_data <- cleaned_data %>%
    group_by(indicator_common_id, admin_area_3) %>%
    nest() %>%
    mutate(
      model = map(data, ~ tryCatch(
        lm(count_adjust ~ factor(month(.x$date)) + as.numeric(date), data = .x),
        error = function(e) NULL
      )),
      deseasonalized = map2(data, model, ~ if (!is.null(.y)) {
        mutate(.x, count_predict = predict(.y, newdata = .x))
      } else {
        seasonal_means <- .x %>%
          group_by(month = month(date)) %>%
          summarise(seasonal_mean = mean(count_adjust, na.rm = TRUE)) %>%
          ungroup()
        mutate(.x, count_predict = seasonal_means$seasonal_mean[match(.x$month, seasonal_means$month)])
      })
    ) %>%
    select(-data, -model) %>%
    unnest(deseasonalized)
  
  # Smooth predictions using a moving average
  deseasonalized_data <- deseasonalized_data %>%
    group_by(indicator_common_id, admin_area_3) %>%
    mutate(
      count_smooth = if (n() < 12) {
        zoo::rollmean(count_predict, k = n(), fill = NA, align = "center")
      } else {
        zoo::rollmean(count_predict, k = 12, fill = NA, align = "center")
      },
      count_smooth = zoo::na.approx(count_smooth, na.rm = FALSE, maxgap = Inf),
      count_smooth = ifelse(is.na(count_smooth), 0, count_smooth)
    ) %>%
    ungroup()
  
  # Calculate residuals
  deseasonalized_data <- deseasonalized_data %>%
    group_by(indicator_common_id, admin_area_3) %>%
    mutate(
      residual = count_adjust - count_smooth,
      sd_residual = sd(residual, na.rm = TRUE),
      control = residual / sd_residual,
      tag = ifelse(abs(control) >= 2, 1, 0)
    ) %>%
    ungroup()
  
  return(deseasonalized_data)
}

# PART 4: Indicator-Level Aggregation with Control Limits
generate_indicator_results <- function(cleaned_data) {
  print("Generating indicator-level results with control limits...")
  
  # Aggregate results by date and indicator
  indicator_results <- cleaned_data %>%
    filter(!is.na(count_adjust) & !is.na(count_predict)) %>%  # Only use valid data
    group_by(indicator_common_id, date) %>%
    summarise(
      total_count = sum(count_adjust, na.rm = TRUE),
      predicted_count = sum(count_smooth, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(indicator_common_id) %>%
    mutate(
      UCL = mean(total_count, na.rm = TRUE) + 3 * sd(total_count, na.rm = TRUE), #Upper Control Limit
      LCL = mean(total_count, na.rm = TRUE) - 3 * sd(total_count, na.rm = TRUE), #Lower Control Limit
      period_id = format(date, "%Y%m")
    ) %>%
    ungroup()
  
  return(indicator_results)
}

# PART 5: Visualization (national view)
plot_indicator_grid <- function(indicator_data) {
  ggplot(indicator_data, aes(x = date)) +
    geom_line(aes(y = total_count), size = 0.8, color = "black") +  # Raw counts
    geom_line(aes(y = predicted_count), size = 0.8, color = "#1a9641") +  # Predicted counts
    geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "#d7191c") +  # Upper Control Limit
    geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "#0099cc") +  # Lower Control Limit
    facet_wrap(~indicator_common_id, scales = "free_y") +
    labs(
      title = "Control Chart",
      subtitle = "Black line: Observed counts of health services with outlier adjustments applied. Green line: Predicted (deseasonalized) counts based on regression.",
      x = "Date",
      y = "Counts",
      caption = "Red dashed line = UCL. Blue dashed line = LCL."
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 9)
    )
}

# Main Script ---------------------------------------------------------------------------------------------------------
inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

print("Performing outlier analysis...")
outlier_results <- outlier_analysis(data, geo_cols)
print("Performing control chart analysis using iteration...")
control_chart_results <- control_chart_analysis(outlier_results, geo_cols)
print("Generating indicator-level results...")
indicator_results <- generate_indicator_results(control_chart_results)
print("Plotting indicator control charts...")
indicator_grid_plot <- plot_indicator_grid(indicator_results)
print(indicator_grid_plot)

# print("Saving results...")
ggsave("indicator_grid_plot.png", plot = indicator_grid_plot, width = 14, height = 10)
write.csv(control_chart_results, "control_chart_results.csv", row.names = FALSE)
write.csv(indicator_results, "indicator_results.csv", row.names = FALSE)