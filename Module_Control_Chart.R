# CB - R code FASTR PROJECT - Control Chart Analysis
# Last edit: 2025 Jan 06

# DATA: sierraleone_imported_dataset.csv

# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.
# FILE: indicator_results.csv           # Indicator-level trends with control limits.
# IMAGE: indicator_grid_plot.png        # Grid plot of indicator-level trends with control limits.


# NOTES
# `control_chart_results` contains the results of the control chart analysis at the facility level. See breakdown of key columns below:

# - `date`: The time period for which the data is recorded.
# - `admin_area_1`, `admin_area_2`, `admin_area_3`: Geographic levels (national, province, district).
# - `indicator_common_id`: The name of the health indicator (e.g., ANC visits, deliveries).
# - `total_count`: The actual observed count of health services provided at the specific facility for the given date.
# - `panelvar`: A unique identifier for the indicator and geographic region, used for analysis (e.g., "anc1_Sierra Leone").
# - `count_predict`: The predicted (deseasonalized) count derived from the STL decomposition trend component.
# - `residual`: The difference between the observed count (`total_count`) and the predicted count (`count_predict`), capturing deviations from the expected value.
# - `seasonal`: The seasonal component extracted by STL decomposition for the facility.
# - `sd_residual`: The standard deviation of the residuals, calculated at the facility level, used for anomaly detection.
# - `control`: The standardized residual, calculated as `residual / sd_residual`, used to determine if a value is anomalous.
# - `tag`: A binary anomaly flag, where `1` indicates an anomaly (when `control` exceeds ±2), and `0` indicates normal behavior.


# `indicator_results` contains the aggregated results of the control chart analysis at the national (admin_area_1) & indicator level. See breakdown of key columns below:
# - `date`: The time period for which the data is aggregated (monthly).
# - `total_count`: The aggregated observed count of health services provided across all facilities for the specific indicator and time period.
# - `mean_predict`: The aggregated predicted (deseasonalized) count derived from the STL decomposition trend component, averaged across all facilities.
# - `tag`: A binary anomaly flag at the national or indicator level, where `1` indicates an anomaly (if any facility reported an anomaly during the period), and `0` indicates normal behavior.
# - `indicator_common_id`
# - `period_id`: A string identifier for the time period, formatted as `YYYYMM`.
# - `UCL` (Upper Control Limit): The upper control limit for the aggregated `total_count`. Values above this limit are considered potential anomalies.
# - `LCL` (Lower Control Limit): The lower control limit for the aggregated `total_count`. Values below this limit are considered potential anomalies.

# Required Libraries ------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(tidyr)   # For complete() and fill()
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

# PART 3 Control Chart Analysis with STL Decomposition
control_chart_analysis_stl <- function(cleaned_data, geo_cols) {
  print("Performing control chart analysis with STL decomposition...")
  
  # Aggregate the cleaned data by geography, indicator, and date
  aggregated_data <- cleaned_data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id, date) %>%
    summarise(total_count = sum(count_adjust, na.rm = TRUE), .groups = "drop")
  
  # Perform STL decomposition for each panel (indicator and geographic area)
  stl_results <- aggregated_data %>%
    group_by(panelvar = paste(indicator_common_id, admin_area_1, sep = "_")) %>%
    group_split() %>%
    map_dfr(function(panel_data) {
      tryCatch({
        # Ensure the time series has no missing dates
        complete_data <- panel_data %>%
          complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month")) %>%
          fill(total_count, .direction = "downup")
        
        # Check if there is enough data for STL decomposition
        if (nrow(complete_data) >= 24) {
          # Perform STL decomposition
          ts_data <- ts(
            complete_data$total_count,
            frequency = 12,
            start = c(year(min(complete_data$date)), month(min(complete_data$date)))
          )
          stl_decomp <- stl(ts_data, s.window = "periodic")
          
          # Extract deseasonalized trend and calculate residuals
          trend <- as.numeric(stl_decomp$time.series[, "trend"])
          seasonal <- as.numeric(stl_decomp$time.series[, "seasonal"])
          residuals <- as.numeric(stl_decomp$time.series[, "remainder"])
          
          # Add results back to the data
          complete_data <- complete_data %>%
            mutate(
              count_predict = trend,
              residual = residuals,
              seasonal = seasonal
            )
          
          return(complete_data)
        } else {
          message("Skipping panelvar: ", unique(panel_data$panelvar), " - Not enough data for STL.")
          return(tibble())
        }
      }, error = function(e) {
        message("Skipping panelvar: ", unique(panel_data$panelvar), " - Error: ", e$message)
        return(tibble())
      })
    })
  
  # Calculate standardized residuals and anomaly tags
  stl_results <- stl_results %>%
    group_by(panelvar) %>%
    mutate(
      sd_residual = sd(residual, na.rm = TRUE),
      control = residual / sd_residual,  # Standardized residuals
      tag = ifelse(abs(control) >= 2, 1, 0)  # Flag anomalies with ±2 SD
    ) %>%
    ungroup()
  
  return(stl_results)
}

# PART 4 Indicator-Level Aggregation with Control Limits
generate_indicator_results <- function(cleaned_data) {
  print("Generating indicator-level results with control limits...")
  
  unique_indicators <- unique(cleaned_data$indicator_common_id)
  
  indicator_results <- lapply(unique_indicators, function(indicator) {
    indicator_data <- cleaned_data %>%
      filter(indicator_common_id == indicator) %>%
      group_by(date) %>%
      summarise(
        total_count = sum(total_count, na.rm = TRUE),       # Aggregated actual counts
        mean_predict = sum(count_predict, na.rm = TRUE),   # Summed predicted counts (reflect national trend)
        tag = max(tag, na.rm = TRUE),                      # Anomaly flag: if any facility is flagged, mark as 1
        .groups = "drop"
      ) %>%
      mutate(
        indicator_common_id = indicator,
        period_id = format(date, "%Y%m")  # Period ID for reference
      )
    
    # Calculate control limits based on `total_count`
    mean_count <- mean(indicator_data$total_count, na.rm = TRUE)
    sd_count <- sd(indicator_data$total_count, na.rm = TRUE)
    indicator_data <- indicator_data %>%
      mutate(
        UCL = mean_count + 3 * sd_count,  # Upper Control Limit
        LCL = mean_count - 3 * sd_count  # Lower Control Limit
      )
    
    return(indicator_data)
  }) %>%
    bind_rows()
  
  print("Indicator-level results generated successfully.")
  return(indicator_results)
}

# PART 5 Visualizations
plot_indicator_grid <- function(indicator_data) {
  ggplot(indicator_data, aes(x = date, y = total_count)) +
    geom_line(size = 0.9, color = "black") +  # Actual counts
    geom_line(aes(y = mean_predict), color = "blue", linetype = "dashed") +  # Predicted counts
    geom_point(data = indicator_data %>% filter(tag == 1), aes(x = date, y = total_count), color = "red", size = 0.9) +
    geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "blue") +
    facet_wrap(~indicator_common_id, scales = "free_y") +
    labs(
      title = "Deseasonalized Counts by Indicator with Anomalies",
      subtitle = "STL-based Deseasonalization with Anomaly Detection",
      x = "Date",
      y = "Total Count",
      caption = "Red dashed line = Upper Control Limit, Blue dashed line = Lower Control Limit. Dashed blue line = Predicted Count. Red points = Anomalies detected"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 9),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10, face = "italic")
    )
}

# Main Script ---------------------------------------------------------------------------------------------------------
inputs <- load_and_preprocess_data("sierraleone_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

print("Performing outlier analysis...")
outlier_results <- outlier_analysis(data, geo_cols)

print("Performing control chart analysis with STL decomposition...")
control_chart_results <- control_chart_analysis_stl(outlier_results, geo_cols)

print("Generating indicator-level results...")
indicator_results <- generate_indicator_results(control_chart_results)

print("Plotting indicator control charts...")
indicator_grid_plot <- plot_indicator_grid(indicator_results)
print(indicator_grid_plot)

print("Saving results...")
ggsave("indicator_grid_plot.png", plot = indicator_grid_plot, width = 14, height = 10)
write.csv(control_chart_results, "control_chart_results.csv", row.names = FALSE)
write.csv(indicator_results, "indicator_results.csv", row.names = FALSE)
