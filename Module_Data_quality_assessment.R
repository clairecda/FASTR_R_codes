OUTLIER_PROPORTION_THRESHOLD <- 0.8  # Proportion threshold for outlier detection
MINIMUM_COUNT_THRESHOLD <- 100      # Minimum count threshold for consideration
GEOLEVEL <- "admin_area_3" 
DQA_INDICATORS <- c("penta1") # Specify which indicators are subjected to DQA (default: opd, penta1, anc1)

#------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 29
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
    mutate(period_id = as.integer(paste0(year, sprintf("%02d", month))),) %>%
    select(facility_id, geo_cols, indicator_common_id, year, month, period_id, count, median_volume, 
           mad_volume, mad_residual, pc, outlier_flag)
  
  return(outlier_data)
}


# PART 2-A Consistency Analysis - Geo Level -----------------------------------------------------------------
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
    group_by(across(all_of(c(geo_cols, "indicator_common_id", "year", "month")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols, "year", "month"),
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
        )  %>%
        select(all_of(c(geo_cols, "year", "month", "ratio_type", "consistency_ratio", "sconsistency")))
      
      pair_results[[pair_name]] <- pair_data
    }
  }
  
  # Combine results from all pairs
  combined_data <- bind_rows(pair_results)
  
  # Ensure `sconsistency` is converted to integer
  combined_data <- combined_data %>%
    mutate(sconsistency = as.integer(sconsistency))
  
  # Add `range` field showing the start and end month for each year
  combined_data <- combined_data %>%
    group_by(across(all_of(c(geo_cols, "year", "ratio_type")))) %>%
    mutate(
      start_month = min(month, na.rm = TRUE),
      end_month = max(month, na.rm = TRUE),
      range = paste0(year, sprintf("%02d", start_month), " - ", year, sprintf("%02d", end_month))
    ) %>%
    ungroup()
  
  return(combined_data)
}

expand_geo_consistency_to_facilities <- function(facility_metadata, geo_consistency_results) {
  print("Expanding geo-level consistency results to facility level...")
  
  # Step 1: Start with a list of all facility IDs and their admin_area_3
  facility_list <- facility_metadata %>%
    select(facility_id, admin_area_3) %>%
    distinct()  # Ensure each facility appears only once
  
  # Step 2: Expand geo consistency results to all facilities by joining on `admin_area_3`
  facility_consistency_results <- facility_list %>%
    left_join(geo_consistency_results, by = "admin_area_3", relationship = "many-to-many")  # Allow expected many-to-many join
  
  return(facility_consistency_results)
}


# PART 3 COMPLETENESS ----------------------------------------------------------------------------------------
completeness_analysis <- function(data, geo_cols, facility_metadata) {
  print("Performing completeness analysis with smoothing...")
  
  # Step 1: Generate a full grid of all possible year-month combinations for each facility and indicator
  all_year_months <- data %>%
    group_by(year) %>%
    summarise(
      min_month = min(month, na.rm = TRUE),
      max_month = max(month, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(month_seq = list(seq(from = min_month, to = max_month))) %>%
    unnest(month_seq) %>%
    rename(month = month_seq) %>%
    select(-min_month, -max_month)
  
  all_facilities_indicators <- data %>% distinct(facility_id, indicator_common_id)
  
  # Step 2: Generate full facility-month-indicator coverage
  complete_month_grid <- all_facilities_indicators %>%
    crossing(all_year_months)
  
  # Drop geo_cols temporarily to avoid missing values for added rows
  data_no_geo <- data %>% select(-all_of(geo_cols))
  
  # Step 3: Join with data to get expanded dataset
  expanded_data <- complete_month_grid %>%
    left_join(data_no_geo, by = c("facility_id", "indicator_common_id", "year", "month")) %>%
    mutate(
      date = as.Date(paste(year, month, "1", sep = "-")),
      period_id = as.integer(paste0(year, sprintf("%02d", month))),
      negdate = as.integer(date) * -1  # Negative date for sorting
    )
  
  # Step 4: Identify completeness - Keep all months, even if not reported
  expanded_data <- expanded_data %>%
    mutate(
      completeness_flag = ifelse(!is.na(count) & count > 0, 1, 0)  # Keep 0s where missing
    )
  
  # Step 5: Smoothing reporting timeframe
  smoothed_data <- expanded_data %>%
    group_by(facility_id, indicator_common_id) %>%  
    arrange(date) %>%
    mutate(
      first_valid_report = cumsum(completeness_flag) > 0,
      last_valid_report = rev(cumsum(rev(completeness_flag)) > 0),  # Reverse cumulative sum
      
      # Compute trailing months since last report without removing any rows
      trailing_months = ifelse(completeness_flag == 1, 0, NA_integer_),
      trailing_months = zoo::na.locf(trailing_months, fromLast = TRUE, na.rm = FALSE),
      trailing_months = ifelse(is.na(trailing_months), 12, trailing_months + 1),
      
      # Ensure all months are retained for completeness assessment
      include_flag = 1  
    ) %>%
    ungroup()
  
  # Step 6: Summarize completeness at facility-month level
  facility_month_data <- smoothed_data %>%
    group_by(
      facility_id, 
      indicator_common_id, 
      year, 
      month, 
      period_id
    ) %>%
    summarise(
      reported_facility_months = sum(completeness_flag, na.rm = TRUE),  # Count of reported months
      completeness_rate = mean(completeness_flag, na.rm = TRUE),        # Proportion completeness per facility-month
      .groups = "drop"
    ) %>%
    mutate(
      completeness_flag = ifelse(reported_facility_months > 0, 1, 0)    # Final completeness assessment
    )
  
  # Step 7: Fix the many-to-many join issue
  facility_metadata_unique <- data %>%
    select(facility_id, all_of(geo_cols)) %>%
    distinct(facility_id, .keep_all = TRUE)
  
  # Step 8: Rejoin geo_cols
  facility_month_data <- facility_month_data %>%
    left_join(facility_metadata_unique, by = "facility_id")
  
  # Step 9: Return the summarized completeness data
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
  print("Performing DQA analysis with strict consistency checks...")
  
  # Step 1: Filter only relevant indicators for DQA
  completeness_data <- completeness_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  # Step 2: Merge Completeness & Outlier Data at Facility-Month-Indicator Level
  merged_data <- completeness_data %>%
    left_join(
      outlier_data,
      by = c("facility_id", "year", "month", "indicator_common_id", geo_cols)
    ) %>%
    mutate(
      outlier_flag = replace_na(outlier_flag, 0),  # Treat NA as "Not an Outlier"
      completeness_pass = ifelse(completeness_flag == dqa_rules$completeness, 1, 0),
      outlier_pass = ifelse(outlier_flag == dqa_rules$outlier_flag, 1, 0)
    )
  
  # Step 3: Aggregate at Facility-Month Level (Total Points for Completeness & Outlier)
  dqa_facility_month <- merged_data %>%
    group_by(facility_id, year, month, !!!syms(geo_cols)) %>%
    summarise(
      total_indicator_points = sum(completeness_pass + outlier_pass, na.rm = TRUE),  # Max 2 points per indicator
      max_points = 2 * length(DQA_INDICATORS),  # Maximum possible points
      dqa_outlier_completeness = ifelse(total_indicator_points == max_points, 1, 0),  # Pass if all indicators pass
      .groups = "drop"
    )
  
  # Step 4: Merge with Consistency Data (Now at Facility-Month Level)
  dqa_data <- dqa_facility_month %>%
    left_join(consistency_expanded, by = c("facility_id", "year", "month", geo_cols)) %>%
    mutate(across(starts_with("pair_"), ~replace_na(.x, 0)))  # Fill missing consistency scores with 0
  
  # Step 5: Compute Final DQA Score Based on All Conditions
  consistency_cols <- setdiff(names(consistency_expanded), c("facility_id", "year", "month", geo_cols))
  
  dqa_data <- dqa_data %>%
    mutate(
      all_pairs_pass = ifelse(rowSums(select(., all_of(consistency_cols)) == 1) == length(consistency_cols), 1, 0),
      dqa_score = ifelse(
        dqa_outlier_completeness == 1 & all_pairs_pass == 1, 1, 0
      ),
      period_id = as.integer(paste0(year, sprintf("%02d", month)))  # Ensure YYYYMM format
    ) %>%
   
    select(all_of(geo_cols), facility_id, year, month, period_id, dqa_score) %>%
    #distinct()  # Ensure only one row per facility-month
  
  return(dqa_data)
}


# DQA Function Excluding Consistency Checks
dqa_without_consistency <- function(
    completeness_data, 
    outlier_data, 
    geo_cols,
    dqa_rules
) {
  print("Performing DQA analysis without consistency checks...")
  
  # Step 1: Filter only specified indicators for DQA
  completeness_data <- completeness_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% DQA_INDICATORS)
  
  # Step 2: Merge completeness and outlier data (Fix: Fill `NA` outlier_flag with `0`)
  merged_data <- completeness_data %>%
    left_join(
      outlier_data,
      by = c("facility_id", "year", "month", "indicator_common_id", geo_cols)
    ) %>%
    mutate(
      outlier_flag = replace_na(outlier_flag, 0),  # Ensure `NA` means "Not an Outlier"
      completeness_pass = ifelse(completeness_flag == dqa_rules$completeness, 1, 0),
      outlier_pass = ifelse(outlier_flag == dqa_rules$outlier_flag, 1, 0)
    )
  
  # Step 3: Aggregate results
  dqa_results <- merged_data %>%
    group_by(facility_id, year, month, !!!syms(geo_cols)) %>%
    summarise(
      total_indicator_points = sum(completeness_pass + outlier_pass, na.rm = TRUE),
      max_points = 2 * length(DQA_INDICATORS),  # Max points per indicator
      dqa_score = ifelse(total_indicator_points == max_points, 1, 0),
      .groups = "drop"
    )
  
  return(dqa_results)
}


# ------------------- Main Execution ----------------------------------------------------------------------------

inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
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

  print("Running consistency analysis...")
  geo_consistency_results <- geo_consistency_analysis(
    data = outlier_data_main, 
    geo_cols = geo_cols, 
    consistency_params = consistency_params
  )

} else {
  print("No valid consistency pairs found. Skipping consistency analysis...")
  geo_consistency_results <- NULL
}


# Expend consistency - join to facility
facility_consistency_results <- expand_geo_consistency_to_facilities(
  facility_metadata = data,  # Contains `facility_id` and `admin_area_3`
  geo_consistency_results = geo_consistency_results  # Geo-level consistency results
)

# Ensure consistency data has one row per facility-month by pivoting ratio pairs
consistency_expanded <- facility_consistency_results %>%
  select(facility_id, year, month, ratio_type, sconsistency) %>%  # Keep only necessary columns
  pivot_wider(
    names_from = ratio_type,   
    values_from = sconsistency, 
    values_fill = list(sconsistency = 0)
  ) %>%
  distinct(facility_id, year, month, .keep_all = TRUE)

# Restore geo_cols AFTER pivoting to avoid duplication issues
consistency_expanded <- consistency_expanded %>%
  left_join(
    facility_consistency_results %>% select(facility_id, year, month, all_of(geo_cols)) %>% distinct(),
    by = c("facility_id", "year", "month")
  )


# RUN Data Quality Assessment (DQA)
if (!is.null(facility_consistency_results)) {
  print("Joining completeness, outlier, and expanded consistency data...")
  dqa_results <- dqa_with_consistency(
    completeness_data = completeness_results,
    consistency_data = consistency_expanded,
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
