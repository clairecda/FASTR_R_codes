OUTLIER_PROPORTION_THRESHOLD <- 0.8  # Proportion threshold for outlier detection
MINIMUM_COUNT_THRESHOLD <- 100      # Minimum count threshold for consideration
GEOLEVEL <- "admin_area_3" 
DQA_INDICATORS <- c("opd", "penta1", "anc1") # Specify which indicators are subjected to DQA (default: opd, penta1, anc1)

#------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 24
# Module: DATA QUALITY ASSESSMENT

# This script is designed to handle datasets where the available indicators are limited 
# and insufficient to conduct a consistency analysis. In such cases, the script defaults 
# to performing the DQA without including consistency checks.


# DATA: guinea_imported_dataset.csv

# ------------------------------------- PARAMETERS -----------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------
# Outlier Analysis Parameters
outlier_params <- list(
  outlier_pc_threshold = OUTLIER_PROPORTION_THRESHOLD,  # Threshold for proportional contribution to flag outliers
  count_threshold = MINIMUM_COUNT_THRESHOLD        # Minimum count to consider for outlier adjustment
)

geo_level <- GEOLEVEL  # Desired geographic level (district) for grouping when adjusting outliers (e.g., "admin_area_3").
# If this level is not available in the dataset, the function will fall back to the lowest 
# available level in `geo_cols` (e.g., "admin_area_2").

# Consistency Analysis Parameters 
consistency_params <- list(
  consistency_pairs = list(
    pair_delivery = c("bcg", "delivery"),   # BCG / Delivery
    pair_penta = c("penta1", "penta3"),     # Penta1 / Penta3
    pair_anc = c("anc1", "anc4")            # ANC1 / ANC4
  ),
  consistency_ranges = list(
    pair_delivery = c(lower = 0.7, upper = 1.3),  # BCG / Delivery within 0.7 to 1.3
    pair_penta = c(lower = 1, upper = Inf),       # Penta1 / Penta3 > 1
    pair_anc = c(lower = 1, upper = Inf)          # ANC1 / ANC4 > 1
  )
)

# DQA Rules
dqa_rules <- list(
  completeness = 1,   # Completeness must be flagged as 1
  outlier_flag = 0,   # Outliers must not be flagged
  sconsistency = 1    # Consistency must be flagged as 1
)


# ------------------------------------- KEY OUTPUTS ----------------------------------------------------------
# FILE: M1_output_outliers.csv             # Detailed facility-level data with identified outliers and adjusted volumes.
# FILE: M1_completeness_long_format.csv    # Facility-level completeness data in a detailed long format, including reported and expected months.
# FILE: M1_output_consistency_geo.csv      # District-level consistency results - use in visualizer
# FILE: M1_facility_dqa.csv                # Facility-level results from DQA analysis.

# Load Required Libraries ------------------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)  # Ensure lubridate is loaded for floor_date

# Define Functions ------------------------------------------------------------------------------------------
load_and_preprocess_data <- function(file_path) {
  print("Loading and preprocessing data...")
  data <- read.csv(file_path) %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      panelvar = paste(indicator_common_id, facility_id, sep = "_")
    )
  geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))]
  return(list(data = data, geo_cols = geo_cols))
}

# Function to validate consistency pairs
validate_consistency_pairs <- function(consistency_params, data) {
  print("Validating consistency pairs based on available indicators...")
  
  consistency_pairs_names <- names(consistency_params$consistency_pairs)
  
  # Identify valid consistency pairs
  valid_consistency_pairs <- consistency_params$consistency_pairs[sapply(
    consistency_params$consistency_pairs, 
    function(pair) all(pair %in% unique(data$indicator_common_id))
  )]
  
  # Retain only valid ranges
  consistency_params$consistency_pairs <- valid_consistency_pairs
  consistency_params$consistency_ranges <- consistency_params$consistency_ranges[names(valid_consistency_pairs)]
  
  if (length(valid_consistency_pairs) < length(consistency_pairs_names)) {
    removed_pairs <- setdiff(consistency_pairs_names, names(valid_consistency_pairs))
    warning(paste("The following consistency pairs were removed due to missing indicators:", 
                  paste(removed_pairs, collapse = ", ")))
  }
  
  return(consistency_params)
}


# PART 1 OUTLIERS ----------------------------------------------------------------------------------------------
outlier_analysis <- function(data, geo_cols, outlier_params) {
  print("Performing outlier analysis...")
  
  # Step 1: Calculate Median Volume
  print("Calculating median volume per facility and indicator...")
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(median_volume = median(count, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Calculate MAD and Identify Outliers
  print("Calculating MAD and identifying outliers...")
  data <- data %>%
    group_by(facility_id, indicator_common_id) %>%
    mutate(
      mad_volume = ifelse(!is.na(count), mad(count[count >= median_volume], na.rm = TRUE), NA),
      mad_residual = ifelse(!is.na(mad_volume) & mad_volume > 0, abs(count - median_volume) / mad_volume, NA),
      outlier_mad = ifelse(!is.na(mad_residual) & mad_residual > 10, 1, 0)  # Flag outliers based on MAD
    ) %>%
    ungroup()
  
  # Step 3: Calculate Proportional Contribution and Identify Outliers
  print("Calculating proportional contribution and flagging outliers...")
  data <- data %>%
    group_by(facility_id, indicator_common_id, year) %>%
    mutate(
      pc = count / sum(count, na.rm = TRUE),  # Calculate proportional contribution
      outlier_pc = ifelse(!is.na(pc) & pc > outlier_params$outlier_pc_threshold, 1, 0)  # Flag based on threshold
    ) %>%
    ungroup()
  
  # Step 4: Combine Outlier Flags
  print("Combining MAD and proportional contribution outlier flags...")
  data <- data %>%
    mutate(
      outlier_flag = ifelse(
        (outlier_mad == 1 | outlier_pc == 1) & count > outlier_params$count_threshold, 1, 0
      )  # Combine MAD and proportional contribution
    )
  
  # Step 5: Output Only Outlier Data
  print("Returning dataset with outliers flagged...")
  outlier_data <- data %>%
    select(facility_id, indicator_common_id, year, month, count, median_volume, 
           mad_volume, mad_residual, pc, outlier_flag, geo_cols)
  
  return(outlier_data)
}


# PART 2-A: Consistency Analysis - Facility Level --------------------------------------------------------------
facility_consistency_analysis <- function(data, geo_cols_facility = "facility_id", consistency_params) {
  print("Performing facility-level consistency analysis...")
  
  # Extract required pairs and ranges from parameters
  required_pairs <- consistency_params$consistency_pairs
  consistency_ranges <- consistency_params$consistency_ranges
  
  # Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier_flag == 1, NA, count))
  
  # Aggregate data at the facility level
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols_facility, "indicator_common_id", "year"))), across(all_of(geo_cols))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols_facility, "year", geo_cols),
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
    
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      # Retrieve consistency range for the current pair
      range <- consistency_ranges[[pair_name]]
      lower_bound <- range["lower"]
      upper_bound <- range["upper"]
      
      pair_data <- wide_data %>%
        mutate(
          ratio_type = pair_name,
          consistency_ratio = if_else(!!sym(col2) > 0, !!sym(col1) / !!sym(col2), NA_real_),
          sconsistency = case_when(
            !is.na(consistency_ratio) & consistency_ratio >= lower_bound & consistency_ratio <= upper_bound ~ 1,
            !is.na(consistency_ratio) ~ 0,
            TRUE ~ NA_real_
          )
        ) %>%
        select(geo_cols_facility, year, geo_cols, ratio_type, sconsistency)
      
      pair_results[[pair_name]] <- pair_data
    }
  }
  
  # Combine results
  combined_data <- bind_rows(pair_results)
  
  return(combined_data)
}

# PART 2-B: Consistency Analysis - Geo Level -------------------------------------------------------------------
geo_consistency_analysis <- function(data, geo_cols, consistency_params) {
  print("Performing geo-level consistency analysis...")
  
  # Extract required pairs and ranges from parameters
  required_pairs <- consistency_params$consistency_pairs
  consistency_ranges <- consistency_params$consistency_ranges
  
  # Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier_flag == 1, NA, count))
  
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

# PART 2-C: Expand Consistency outputs For DQA -------------------------------------------------------------------
expand_consistency_results <- function(consistency_data, completeness_data) {
  print("Expanding consistency results to include all months...")
  
  # Extract unique year-month combinations from completeness_data
  unique_months <- completeness_data %>%
    distinct(year, month)  # Get unique year-month combinations
  
  # Deduplicate consistency_data before expanding
  consistency_data <- consistency_data %>%
    distinct(facility_id, across(everything()))  # Ensure no duplicates
  
  # Cross-join unique months with consistency results
  expanded_consistency <- consistency_data %>%
    left_join(unique_months, by = character())  # Expand across months
  
  return(expanded_consistency)
}


# PART 3-A COMPLETENESS ----------------------------------------------------------------------------------------
completeness_analysis <- function(data, geo_cols) {
  print("Performing completeness analysis with smoothing for trailing missing data...")
  
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
      period_id = as.integer(paste0(year, sprintf("%02d", month))),
      negdate = as.integer(date) * -1  # Generate negative date for sorting
    )
  
  # Step 6: Mark completeness (only NA is considered incomplete)
  expanded_data <- expanded_data %>%
    mutate(
      completeness_flag = ifelse(!is.na(count), 1, 0)  # NA is incomplete, 0 is fine
    )
  
  # Step 7: Smoothing the reporting time frame
  smoothed_data <- expanded_data %>%
    group_by(panelvar = paste(indicator_common_id, facility_id, sep = "_")) %>%
    arrange(panelvar, date) %>%
    mutate(
      first_occurrence = cumsum(completeness_flag),  # Identify first occurrence of completeness
      num1 = ifelse(first_occurrence == 0, row_number(), NA_integer_)  # Count rows before the first occurrence
    ) %>%
    ungroup() %>%
    filter(is.na(num1) | num1 <= 12)  # Keep data within 12 months before the first occurrence
  
  # Step 8: Summarize monthly reported units
  facility_month_data <- smoothed_data %>%
    group_by(
      facility_id, 
      indicator_common_id, 
      year, 
      month, 
      period_id,
      across(all_of(geo_cols))
    ) %>%
    summarise(
      reported_facility_months = sum(completeness_flag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      completeness_flag = ifelse(reported_facility_months > 0, 1, 0)
    )
  
  # Step 9: Return results
  return(facility_month_data)
}


# PART 4 DQA ----------------------------------------------------------------------------------------------------
# 1. dqa_with_consistency: Includes consistency checks
# 2. dqa_without_consistency: Excludes consistency checks

# DQA Function Including Consistency Checks
dqa_with_consistency <- function(
    completeness_data, 
    consistency_data, 
    outlier_data, 
    geo_cols,
    dqa_rules
) {
  print("Performing DQA analysis with consistency checks...")
  
  # Filter only specified indicators for DQA
  completeness_data <- completeness_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  # Deduplicate datasets
  completeness_data <- completeness_data %>%
    distinct(facility_id, year, month, indicator_common_id, completeness_flag, !!!syms(geo_cols))
  
  consistency_data <- consistency_data %>%
    distinct(facility_id, year, month, ratio_type, sconsistency, !!!syms(geo_cols))
  
  outlier_data <- outlier_data %>%
    distinct(facility_id, year, month, indicator_common_id, outlier_flag, !!!syms(geo_cols))
  
  # Merge completeness and outlier data
  merged_data <- completeness_data %>%
    left_join(
      outlier_data,
      by = c("facility_id", "year", "month", "indicator_common_id", geo_cols)
    ) %>%
    mutate(
      completeness_pass = ifelse(completeness_flag == dqa_rules$completeness, 1, 0),
      outlier_pass = ifelse(outlier_flag == dqa_rules$outlier_flag, 1, 0)
    )
  
  # Aggregate indicator results
  indicator_results <- merged_data %>%
    group_by(facility_id, year, month, !!!syms(geo_cols)) %>%
    summarise(
      total_indicator_points = sum(completeness_pass + outlier_pass, na.rm = TRUE),  # Max 2 points per indicator
      .groups = "drop"
    )
  
  # Aggregate consistency results
  consistency_results <- consistency_data %>%
    group_by(facility_id, year, month, !!!syms(geo_cols)) %>%
    summarise(
      total_consistency_points = sum(sconsistency, na.rm = TRUE),  # Max 1 point per pair
      .groups = "drop"
    )
  
  # Merge indicator and consistency results
  dqa_results <- indicator_results %>%
    left_join(consistency_results, by = c("facility_id", "year", "month", geo_cols)) %>%
    mutate(
      total_points = total_indicator_points + total_consistency_points,
      max_points = 2 * length(DQA_INDICATORS) + length(unique(consistency_data$ratio_type)),
      dqa_score = ifelse(total_points == max_points, 1, 0)
    )
  
  return(dqa_results)
}

# DQA Function Excluding Consistency Checks
dqa_without_consistency <- function(
    completeness_data, 
    outlier_data, 
    geo_cols,
    dqa_rules
) {
  print("Performing DQA analysis without consistency checks...")
  
  # Filter only specified indicators for DQA
  completeness_data <- completeness_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  # Deduplicate datasets
  completeness_data <- completeness_data %>%
    distinct(
      facility_id, year, month, indicator_common_id, completeness_flag, !!!syms(geo_cols)
    )
  
  outlier_data <- outlier_data %>%
    distinct(
      facility_id, year, month, indicator_common_id, outlier_flag, !!!syms(geo_cols)
    )
  
  # Merge completeness and outlier data
  merged_data <- completeness_data %>%
    left_join(
      outlier_data,
      by = c("facility_id", "year", "month", "indicator_common_id", geo_cols)
    ) %>%
    mutate(
      completeness_pass = ifelse(completeness_flag == dqa_rules$completeness, 1, 0),
      outlier_pass = ifelse(outlier_flag == dqa_rules$outlier_flag, 1, 0)
    )
  
  # Aggregate results
  dqa_results <- merged_data %>%
    group_by(facility_id, year, month, !!!syms(geo_cols)) %>%
    summarise(
      total_indicator_points = sum(completeness_pass + outlier_pass, na.rm = TRUE),
      max_points = 2 * length(DQA_INDICATORS),  # Max points for indicators
      dqa_score = ifelse(total_indicator_points == max_points, 1, 0),
      .groups = "drop"
    )
  
  return(dqa_results)
}

# ------------------- Main Execution ----------------------------------------------------------------------------

inputs <- load_and_preprocess_data("sierraleone_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

# Validate Consistency Pairs
consistency_params <- validate_consistency_pairs(consistency_params, data)

# Run Outlier Analysis
print("Running outlier analysis...")
outlier_data_main <- outlier_analysis(data, geo_cols, outlier_params)


# Run Completeness Analysis
print("Running completeness analysis...")
completeness_results <- completeness_analysis(outlier_data_main, geo_cols)

# Run Consistency Analysis (if applicable)
if (length(consistency_params$consistency_pairs) > 0) {
  print("Running facility-level consistency analysis...")
  
  # Facility-Level Consistency Analysis
  facility_consistency_results <- facility_consistency_analysis(
    data = outlier_data_main, 
    geo_cols_facility = "facility_id", 
    consistency_params = consistency_params
  )
  
  # Expand Consistency Results Across All Months
  print("Expanding consistency results across months...")
  facility_consistency_results <- expand_consistency_results(
    consistency_data = facility_consistency_results,
    completeness_data = completeness_results
  )
  
  print("Running geo-level consistency analysis...")
  
  # Geo-Level Consistency Analysis
  geo_consistency_results <- geo_consistency_analysis(
    data = outlier_data_main, 
    geo_cols = geo_cols, 
    consistency_params = consistency_params
  )
} else {
  print("No valid consistency pairs found. Skipping consistency analysis...")
  facility_consistency_results <- NULL
  geo_consistency_results <- NULL
}


facility_consistency_results <- facility_consistency_results %>%
  left_join(
    completeness_results %>%
      distinct(facility_id, !!!syms(geo_cols)),
    by = "facility_id",
    relationship = "many-to-many"
  ) %>%
  rename(
    year = year.x,
    admin_area_1 = admin_area_1.x,
    admin_area_2 = admin_area_2.x,
    admin_area_3 = admin_area_3.x
  ) %>%
  select(-c(year.y, admin_area_1.y, admin_area_2.y, admin_area_3.y))  # Remove duplicate columns



# RUN Data Quality Assessment (DQA)
if (!is.null(facility_consistency_results)) {
  print("Joining completeness, outlier, and expanded consistency data...")
  dqa_results <- dqa_with_consistency(
    completeness_data = completeness_results,
    consistency_data = facility_consistency_results,
    outlier_data = outlier_data_main,
    geo_cols = geo_cols,
    dqa_rules = dqa_rules
  )
} else {
  print("Performing DQA analysis without consistency checks...")
  dqa_results <- dqa_without_consistency(
    completeness_data = completeness_results,
    outlier_data = outlier_data_main,
    geo_cols = geo_cols,
    dqa_rules = dqa_rules
  )
}


# -------------------------------- SAVE DATA OUTPUTS ------------------------------------------------------------

print("Saving results from outlier analysis...")
write.csv(outlier_data_main, "M1_output_outliers.csv", row.names = FALSE)                          # Facility-level outlier data

if (length(consistency_params$consistency_pairs) > 0) {
  print("Saving all data outputs from consistency analysis...")
  write.csv(geo_consistency_results, "M1_output_consistency_geo.csv", row.names = FALSE)           # Geo-level consistency results
  write.csv(facility_consistency_results, "M1_output_consistency_facility.csv", row.names = FALSE) # Facility-level consistency results
}


print("Saving results from completeness analysis...")
write.csv(completeness_results, "M1_completeness_long_format.csv", row.names = FALSE)              # Facility-month completeness

print("Saving results from DQA analysis...")
write.csv(dqa_results, "M1_facility_dqa.csv", row.names = FALSE)                                   # Facility-level DQA results

print("DQA Analysis completed. All outputs saved.")
