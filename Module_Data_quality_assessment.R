# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 18
# Module: DATA QUALITY ASSESSMENT

# DATA: guinea_imported_dataset.csv

# ------------------------------------- PARAMETERS -----------------------------

# ------------------------------------------------------------------------------

# Outlier Analysis Parameters
outlier_params <- list(
  outlier_pc_threshold = 0.8,  # Threshold for proportional contribution to flag outliers
  count_threshold = 100        # Minimum count to consider for outlier adjustment
)

# Consistency Analysis Parameters 
consistency_params <- list(
  consistency_pairs = list(
    pair_delivery = c("bcg", "delivery"),   # BCG / Delivery
    pair_penta = c("penta1", "penta3"),     # Penta1 / Penta3
    pair_anc = c("anc1", "anc4")            # ANC1 / ANC4
    # Removed pair_pnc - it WAS part of the stata analysis - it is not mentioned in the methodology? 
  ),
  consistency_ranges = list(
    pair_delivery = c(lower = 0.7, upper = 1.3),  # BCG / Delivery within 0.7 to 1.3
    pair_penta = c(lower = 1, upper = Inf),       # Penta1 / Penta3 > 1
    pair_anc = c(lower = 1, upper = Inf)          # ANC1 / ANC4 > 1
    # Removed pair_pnc consistency range - ?? it WAS part of the stata analysis - it is not mentioned in the methodology? 
  )
)

# DQA Analysis Parameters # priority vs non priority - is this the best approach?
dqa_params <- list(
  priority_indicators = c("opd", "penta1", "anc1"),  # List of priority indicators
  non_priority_indicators = c("penta3", "anc4"),     # Explicitly defined non-priority indicators (replace with NULL to select all indicators that are not in the priority list)
  dqa_rules = list(
    priority = list(
      completeness = 1,
      outlier_flag = 0,
      sconsistency = 1
    ),
    non_priority = list(
      completeness = 1,
      outlier_flag = 0
      # sconsistency not required for non-priority indicators #cb: ?? confirm that the approach is correct
    )
  )
)

# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE: output_outliers.csv           # Detailed facility-level data with identified outliers and adjusted volumes.
# FILE: completeness_long_format.csv  # Facility-level completeness data in a detailed long format, including reported and expected months.
# FILE: output_consistency_geo.csv    # District-level consistency results
# FILE: output_consistency_facility.csv # Facility-level consistency results
# FILE: dqa_summary.csv               # Summary of DQA results by admin_area_2 and indicators (% of facilities meeting DQA criteria).
# FILE: facility_dqa.csv              # Facility-level results from DQA analysis.



# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)  # Ensure lubridate is loaded for floor_date

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

# Function to Adjust DQA Parameters for Flexibility
adjust_dqa_parameters <- function(dqa_params, consistency_params, data) {
  # Adjust priority indicators
  existing_priority <- dqa_params$priority_indicators[dqa_params$priority_indicators %in% unique(data$indicator_common_id)]
  missing_priority <- setdiff(dqa_params$priority_indicators, existing_priority)
  
  if(length(missing_priority) > 0) {
    warning(paste("The following priority indicators are missing from the data and will be excluded:", 
                  paste(missing_priority, collapse = ", ")))
    dqa_params$priority_indicators <- existing_priority
  }
  
  # Handle non_priority_indicators
  if(is.null(dqa_params$non_priority_indicators)) {
    # Automatically assign all indicators not in priority as non-priority
    dqa_params$non_priority_indicators <- setdiff(unique(data$indicator_common_id), dqa_params$priority_indicators)
    message("Non-priority indicators have been automatically assigned as all indicators not listed as priority.")
  } else {
    # Ensure that explicitly defined non-priority indicators exist in the data
    existing_non_priority <- dqa_params$non_priority_indicators[dqa_params$non_priority_indicators %in% unique(data$indicator_common_id)]
    missing_non_priority <- setdiff(dqa_params$non_priority_indicators, existing_non_priority)
    
    if(length(missing_non_priority) > 0) {
      warning(paste("The following non-priority indicators are missing from the data and will be excluded:", 
                    paste(missing_non_priority, collapse = ", ")))
      dqa_params$non_priority_indicators <- existing_non_priority
    }
    
    # Assign remaining indicators not in priority or explicitly non-priority
    auto_assigned_non_priority <- setdiff(unique(data$indicator_common_id), 
                                          c(dqa_params$priority_indicators, dqa_params$non_priority_indicators))
    
    if(length(auto_assigned_non_priority) > 0) {
      message(paste("Automatically assigning the following indicators as non-priority:", 
                    paste(auto_assigned_non_priority, collapse = ", ")))
      dqa_params$non_priority_indicators <- c(dqa_params$non_priority_indicators, auto_assigned_non_priority)
    }
  }
  
  # Adjust consistency_params based on available indicators
  consistency_pairs_names <- names(consistency_params$consistency_pairs)
  
  # Identify valid consistency pairs where both indicators exist
  valid_consistency_pairs <- consistency_params$consistency_pairs[sapply(consistency_params$consistency_pairs, 
                                                                         function(pair) all(pair %in% unique(data$indicator_common_id)))]
  
  # Update consistency_params with valid pairs only
  consistency_params$consistency_pairs <- valid_consistency_pairs
  consistency_params$consistency_ranges <- consistency_params$consistency_ranges[names(consistency_params$consistency_ranges) %in% names(valid_consistency_pairs)]
  
  if(length(consistency_params$consistency_pairs) < length(consistency_pairs_names)) {
    removed_pairs <- setdiff(consistency_pairs_names, names(consistency_params$consistency_pairs))
    warning(paste("Removed the following consistency pairs due to missing indicators:", 
                  paste(removed_pairs, collapse = ", ")))
  }
  
  return(list(dqa_params = dqa_params, consistency_params = consistency_params))
}

# PART 1 OUTLIERS
outlier_analysis <- function(data, geo_cols, outlier_params) {
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
  
  # Step 3: Calculate Proportional Contribution using Parameter
  data <- data %>%
    group_by(facility_id, indicator_common_id, year) %>%
    mutate(
      pc = count / sum(count, na.rm = TRUE),
      outlier_pc = ifelse(!is.na(pc) & pc > outlier_params$outlier_pc_threshold, 1, 0)
    ) %>%
    ungroup()
  
  # Step 4: Combine Outlier Flags
  data <- data %>%
    mutate(outlier = ifelse((outlier_mad == 1 | outlier_pc == 1) & count > outlier_params$count_threshold, 1, 0))
  
  # Step 5: Adjust Outliers
  print("Adjusting outliers...")
  data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id")))) %>%
    mutate(
      count_adjust = ifelse(outlier == 1, median(count, na.rm = TRUE), count),
      deviance = ifelse(count_adjust > 0, (count - count_adjust) / count_adjust, NA_real_),
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

# PART 2-A: Consistency Analysis - Facility Level ----------------------------
facility_consistency_analysis <- function(data, geo_cols_facility = "facility_id", consistency_params) {
  print("Performing facility-level consistency analysis...")
  
  # Extract required pairs and ranges from parameters
  required_pairs <- consistency_params$consistency_pairs
  consistency_ranges <- consistency_params$consistency_ranges
  
  # Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier == 1, NA, count))
  
  # Aggregate data at the facility level
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols_facility, "indicator_common_id", "year")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols_facility, "year"),
      names_from = "indicator_common_id",
      values_from = "count",
      values_fill = list(count = 0)
    )
  
  # Process each pair in isolation
  pair_results <- list()
  
  for (pair_name in names(required_pairs)) {
    pair <- required_pairs[[pair_name]]
    col1 <- pair[1]
    col2 <- pair[2]
    ratio_name <- paste0(pair_name, "_ratio")
    sratio_name <- paste0(pair_name, "_sratio")
    
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      # Retrieve consistency range for the current pair
      range <- consistency_ranges[[pair_name]]
      lower_bound <- range["lower"]
      upper_bound <- range["upper"]
      
      pair_data <- wide_data %>%
        mutate(
          !!ratio_name := if_else(!!sym(col2) > 0, !!sym(col1) / !!sym(col2), NA_real_),
          !!sratio_name := case_when(
            !is.na(!!sym(ratio_name)) & 
              !!sym(ratio_name) > 1 ~ 1, # ANC1/ANC4 and Penta1/Penta3
            !is.na(!!sym(ratio_name)) & 
              !!sym(ratio_name) >= lower_bound & !!sym(ratio_name) <= upper_bound ~ 1, # BCG/Delivery
            !is.na(!!sym(ratio_name)) ~ 0,
            TRUE ~ NA_real_
          )
        ) %>%
        select(all_of(c(geo_cols_facility, "year", ratio_name, sratio_name)))
      
      pair_results[[pair_name]] <- pair_data
    }
  }
  
  combined_data <- reduce(pair_results, full_join, by = c(geo_cols_facility, "year"))
  
  ratio_columns <- grep("_ratio$", colnames(combined_data), value = TRUE)
  sratio_columns <- grep("_sratio$", colnames(combined_data), value = TRUE)
  
  long_ratio_data <- combined_data %>%
    select(all_of(c(geo_cols_facility, "year", ratio_columns))) %>%
    pivot_longer(
      cols = ends_with("_ratio"),
      names_to = "ratio_type",
      values_to = "consistency_ratio"
    )
  
  long_sratio_data <- combined_data %>%
    select(all_of(c(geo_cols_facility, "year", sratio_columns))) %>%
    pivot_longer(
      cols = ends_with("_sratio"),
      names_to = "sratio_type",
      values_to = "sconsistency"
    ) %>%
    mutate(
      ratio_type = gsub("_sratio$", "_ratio", sratio_type)
    ) %>%
    select(-sratio_type)
  
  long_data <- long_ratio_data %>%
    left_join(long_sratio_data, by = c(geo_cols_facility, "year", "ratio_type")) %>%
    mutate(sconsistency = as.integer(sconsistency))
  
  # Join geographic columns back
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  if (length(geo_cols) > 0) {
    long_data <- data %>%
      select(all_of(c(geo_cols, geo_cols_facility))) %>%
      distinct() %>%
      left_join(long_data, by = geo_cols_facility)
  }
  
  return(long_data)
}

# PART 2-B: Consistency Analysis - Geo Level ----------------------------------
geo_consistency_analysis <- function(data, geo_cols, consistency_params) {
  print("Performing geo-level consistency analysis...")
  
  # Extract required pairs and ranges from parameters
  required_pairs <- consistency_params$consistency_pairs
  consistency_ranges <- consistency_params$consistency_ranges
  
  # Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier == 1, NA, count))
  
  # Aggregate data at geographic level
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id", "year")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols, "year"),
      names_from = "indicator_common_id",
      values_from = "count",
      values_fill = list(count = 0)
    )
  
  # Process each pair in isolation
  pair_results <- list()
  
  for (pair_name in names(required_pairs)) {
    pair <- required_pairs[[pair_name]]
    col1 <- pair[1]
    col2 <- pair[2]
    ratio_type <- pair_name
    ratio_name <- "consistency_ratio"
    sconsistency_name <- "sconsistency"
    
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      # Retrieve consistency range for the current pair
      range <- consistency_ranges[[pair_name]]
      lower_bound <- range["lower"]
      upper_bound <- range["upper"]
      
      pair_data <- wide_data %>%
        mutate(
          consistency_ratio = if_else(!!sym(col2) > 0, !!sym(col1) / !!sym(col2), NA_real_),
          sconsistency = case_when(
            pair_name != "pair_delivery" & !is.na(consistency_ratio) & consistency_ratio > 1 ~ 1,  # ANC1/ANC4 and Penta1/Penta3
            pair_name == "pair_delivery" & !is.na(consistency_ratio) & consistency_ratio >= lower_bound & consistency_ratio <= upper_bound ~ 1,  # BCG/Delivery
            !is.na(consistency_ratio) ~ 0,
            TRUE ~ NA_real_
          ),
          ratio_type = pair_name  # Assign the ratio type
        ) %>%
        select(all_of(c(geo_cols, "year", "ratio_type", "consistency_ratio", "sconsistency")))
      
      pair_results[[pair_name]] <- pair_data
    }
  }
  
  # Combine results from all pairs
  combined_data <- bind_rows(pair_results)
  
  # Ensure `sconsistency` is converted to integer
  combined_data <- combined_data %>%
    mutate(sconsistency = as.integer(sconsistency))
  
  return(combined_data)
}

# PART 3 COMPLETENESS
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

# PART 4 DQA (Strict, Facility-Level Consistency)
dqa_analysis_strict_facility_consistency <- function(
    completeness_data, 
    consistency_data,   # Facility-level sconsistency
    outlier_data, 
    geo_cols,
    dqa_params,
    consistency_params
) {
  print("Performing strict DQA analysis (facility-level consistency)...")
  
  # Step 1: Define priority and non-priority indicators
  priority_indicators <- dqa_params$priority_indicators
  non_priority_indicators <- dqa_params$non_priority_indicators
  
  # Step 2: Map ratio types to corresponding indicators correctly
  ratio_mapping <- tibble::tibble(
    ratio_type = paste0(names(consistency_params$consistency_pairs), "_ratio"),
    indicator_common_id = sapply(consistency_params$consistency_pairs, function(x) x[1])
  )
  
  
  # Step 3: Merge completeness, consistency, and outlier data
  merged_data <- completeness_data %>%
    distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE) %>%
    left_join(
      outlier_data %>%
        distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE) %>%
        select(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id", "outlier_flag"))),
      by = c(geo_cols, "facility_id", "year", "indicator_common_id")
    ) %>%
    left_join(
      consistency_data %>%
        left_join(ratio_mapping, by = "ratio_type") %>%
        distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id", "ratio_type"))), .keep_all = TRUE) %>%
        select(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id", "sconsistency", "ratio_type"))),
      by = c(geo_cols, "facility_id", "year", "indicator_common_id")
    )
  
  # Step 4: Create DQA criteria using Parameters
  merged_data <- merged_data %>%
    mutate(
      dqa_temp = case_when(
        # Priority indicators: Strict rules
        indicator_common_id %in% priority_indicators &
          completeness_flag == dqa_params$dqa_rules$priority$completeness & 
          outlier_flag == dqa_params$dqa_rules$priority$outlier_flag &
          sconsistency == dqa_params$dqa_rules$priority$sconsistency ~ 1,
        
        # Non-priority indicators: Simpler rules
        indicator_common_id %in% non_priority_indicators &
          completeness_flag == dqa_params$dqa_rules$non_priority$completeness &
          outlier_flag == dqa_params$dqa_rules$non_priority$outlier_flag ~ 1,
        
        # All other cases fail
        TRUE ~ 0
      )
    )
  
  # Step 5: Aggregate DQA results for summary
  dqa_summary <- merged_data %>%
    group_by(admin_area_2, year, indicator_common_id) %>%
    summarise(
      dqa_pass_rate = mean(dqa_temp, na.rm = TRUE),  # Calculate % of facilities passing DQA test
      facility_count = n(),                          # Count facilities in each group
      .groups = "drop"
    )
  
  # Step 6: Return results as a list
  return(list(
    dqa_results = merged_data %>% select(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id", "ratio_type", "dqa_temp"))),
    dqa_summary = dqa_summary
  ))
}

# -----------------------------------------------------------------------------
# Main Execution 
# -----------------------------------------------------------------------------
inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

# Adjust DQA Parameters for Flexibility
adjusted_params <- adjust_dqa_parameters(dqa_params, consistency_params, data)
dqa_params <- adjusted_params$dqa_params
consistency_params <- adjusted_params$consistency_params

# Outlier Analysis
print("Running outlier analysis...")
outlier_results    <- outlier_analysis(data, geo_cols, outlier_params)
outlier_data_main  <- outlier_results$outlier_data

# Completeness Analysis
print("Running completeness analysis...")
completeness_results <- completeness_analysis(outlier_data_main, geo_cols)
facility_month_data  <- completeness_results$facility_month_data %>%
  mutate(completeness = completeness_flag)

# Select completeness facility-month output for monthly-level DQA:
completeness_data <- facility_month_data

# Consistency Analysis (Facility-Level)
print("Running consistency analysis (facility-level)...")
facility_consistency_results <- facility_consistency_analysis(
  outlier_data_main, 
  geo_cols_facility = "facility_id", 
  consistency_params = consistency_params
)

# Consistency Analysis (Geo-Level)
print("Running consistency analysis (geo-level)...")
geo_consistency_results <- geo_consistency_analysis(
  outlier_data_main, 
  geo_cols = geo_cols, 
  consistency_params = consistency_params
)

# Prepare outlier_data for DQA (rename 'outlier' to 'outlier_flag')
outlier_data_for_dqa <- outlier_data_main %>%
  rename(outlier_flag = outlier)

# Run DQA (Strict, Facility-Level)
print("Running DQA analysis...")
dqa_results <- dqa_analysis_strict_facility_consistency(
  completeness_data = completeness_data,
  consistency_data  = facility_consistency_results, 
  outlier_data      = outlier_data_for_dqa,
  geo_cols          = geo_cols,
  dqa_params        = dqa_params,
  consistency_params = consistency_params  # Ensure this is passed
)

# -------------------------------- SAVE DATA OUTPUTS --------------------------------------------------

print("Saving all data outputs from outlier analysis...")
write.csv(outlier_results$outlier_data, "output_outliers.csv", row.names = FALSE)            # Facility-level outlier data
write.csv(outlier_results$top_outliers_data, "top_outliers_data.csv", row.names = FALSE)      # Top outliers summary
write.csv(outlier_results$volume_increase_data, "volume_increase_data.csv", row.names = FALSE) # Monthly volume changes
write.csv(outlier_results$heatmap_data, "heatmap_data.csv", row.names = FALSE)                # Heatmap data

print("Saving all data outputs from consistency analysis...")
write.csv(geo_consistency_results, "output_consistency_geo.csv", row.names = FALSE)          # Geo-level consistency results
write.csv(facility_consistency_results, "output_consistency_facility.csv", row.names = FALSE) # Facility-level consistency results

print("Saving all data outputs from completeness analysis...")
write.csv(completeness_results$geography_aggregate, "completeness_geo_aggregated.csv", row.names = FALSE) # Geo-level completeness
write.csv(completeness_results$facility_month_data, "completeness_long_format.csv", row.names = FALSE)     # Facility-month completeness
write.csv(completeness_results$yearly_summary, "completeness_yearly_summary.csv", row.names = FALSE)     # Facility-month completeness by year

print("Saving all data outputs from DQA analysis...")
write.csv(dqa_results$dqa_results, "facility_dqa.csv", row.names = FALSE)                                # Facility-level DQA results
write.csv(dqa_results$dqa_summary, "dqa_summary.csv", row.names = FALSE)                                 # Summary by admin_area_2 DQA results

print("DQA Analysis completed. All outputs saved.")