# CB - R code FASTR PROJECT - Control Chart Analysis
# Last edit: 2025 Jan 03

# DATA: sierraleone_imported_dataset.csv

# FILE: control_chart_results.csv       # Facility-level control chart analysis results with flags for anomalies.
# FILE: indicator_results.csv           # Indicator-level trends with control limits.
# IMAGE: indicator_grid_plot.png        # Grid plot of indicator-level trends with control limits.

# NOTES

# OUTPUTS:
# 1. `control_chart_results.csv`: Facility-level dataset with:
#    - Adjusted counts (`count_adjust`), deseasonalized values, and anomaly flags (`tag`).
# 2. `indicator_results.csv`: National-level aggregated trends with control limits and anomaly flags.
# 3. `indicator_grid_plot.png`: Grid plot of deseasonalized trends for all indicators.

# KEY VARIABLES:
# - `indicator_common_id`: Name of the health indicator (e.g., ANC visits, deliveries).
# - `admin_area_1`, `admin_area_2`, `admin_area_3`: Geographic levels (national, province, district).
# - `count`: Original count of health services provided.
# - `count_adjust`: Cleaned count (outliers replaced with medians).
# - `count_predict`: Predicted (deseasonalized) count using regression.
# - `residual`: Difference between actual and predicted counts.
# - `control`: Standardized residuals for anomaly detection.
# - `UCL`, `LCL`: Upper and lower control limits for aggregated trends.

# Required Libraries ------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)  # For rolling statistics

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

# PART 3 Control Chart Analysis with Regression-Based Deseasonalization
control_chart_analysis <- function(cleaned_data, geo_cols) {
  print("Performing control chart analysis with regression-based deseasonalization...")
  
  aggregated_data <- cleaned_data %>%
    group_by(across(all_of(geo_cols)), indicator_common_id, date) %>%
    summarise(total_count = sum(count_adjust, na.rm = TRUE), .groups = "drop")
  
  complete_data <- aggregated_data %>%
    group_by(panelvar = paste(indicator_common_id, admin_area_1, sep = "_")) %>%
    complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month")) %>%
    fill(total_count, .direction = "downup") %>%
    ungroup()
  
  print("Applying regression-based deseasonalization...")
  deseasonalized_data <- complete_data %>%
    mutate(month = month(date), date_numeric = as.numeric(date)) %>%
    group_by(panelvar) %>%
    group_split() %>%
    map_dfr(function(panel_data) {
      tryCatch({
        model <- lm(total_count ~ poly(date_numeric, 2) + factor(month), data = panel_data)
        panel_data <- panel_data %>%
          mutate(
            count_predict = predict(model, newdata = panel_data),
            residual = total_count - count_predict
          )
        return(panel_data)
      }, error = function(e) {
        message("Skipping panelvar: ", unique(panel_data$panelvar), " - Error: ", e$message)
        return(tibble())
      })
    })
  
  deseasonalized_data <- deseasonalized_data %>%
    group_by(panelvar) %>%
    mutate(
      period_id = format(date, "%Y%m"),
      sd_residual = sd(residual, na.rm = TRUE),
      control = residual / sd_residual,
      tag = ifelse(abs(control) >= 2, 1, 0)
    ) %>%
    ungroup()
  
  return(deseasonalized_data)
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
        total_count = sum(count_predict, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        indicator_common_id = indicator,
        period_id = format(date, "%Y%m")
      )
    
    # Calculate control limits
    mean_count <- mean(indicator_data$total_count, na.rm = TRUE)
    sd_count <- sd(indicator_data$total_count, na.rm = TRUE)
    indicator_data <- indicator_data %>%
      mutate(
        UCL = mean_count + 3 * sd_count,
        LCL = mean_count - 3 * sd_count,
        tag = ifelse(total_count > UCL | total_count < LCL, 1, 0)
      )
    
    return(indicator_data)
  }) %>%
    bind_rows()
  
  print("Indicator-level results generated successfully.")
  return(indicator_results)
}

# PART 5 Visualizations
plot_indicator_grid <- function(indicator_data) {
  if (nrow(indicator_data) == 0) {
    stop("The input data is empty. Ensure 'generate_indicator_results' produced valid results.")
  }
  
  ggplot(indicator_data, aes(x = date, y = total_count)) +
    geom_line(size = 0.9, color = "black") +
    geom_hline(aes(yintercept = UCL), linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = LCL), linetype = "dashed", color = "blue") +
    facet_wrap(~indicator_common_id, scales = "free_y") +  # Updated to use indicator_common_id
    labs(
      title = "Control charts by indicator",
      subtitle = "Trends in deseasonalized counts with control limits",
      x = "Date",
      y = "Total Count",
      caption = "Red dashed line = Upper Control Limit, Blue dashed line = Lower Control Limit"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, face = "italic")
    )
}

# Main Script ---------------------------------------------------------------------------------------------------------
inputs <- load_and_preprocess_data("sierraleone_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

print("Performing outlier analysis...")
outlier_results <- outlier_analysis(data, geo_cols)

print("Performing control chart analysis...")
control_chart_results <- control_chart_analysis(outlier_results, geo_cols)

print("Generating indicator-level results...")
indicator_results <- generate_indicator_results(control_chart_results)

print("Plotting indicator control charts...")
indicator_grid_plot <- plot_indicator_grid(indicator_results)
print(indicator_grid_plot)

print("Saving results...")
ggsave("indicator_grid_plot.png", plot = indicator_grid_plot, width = 14, height = 10)
write.csv(control_chart_results, "control_chart_results.csv", row.names = FALSE)
write.csv(indicator_results, "indicator_results.csv", row.names = FALSE) 