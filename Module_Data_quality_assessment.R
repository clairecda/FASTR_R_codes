OUTLIER_PROPORTION_THRESHOLD <- 0.8  # Proportion threshold for outlier detection
MINIMUM_COUNT_THRESHOLD <- 100       # Minimum count threshold for consideration
MADS <- 10                           # Number of MADs
GEOLEVEL <- "admin_area_4"           # Admin level used to join facilities to corresponding geo-consistency
DQA_INDICATORS <- c("penta1", "anc1", "opd")

#-------------------------------------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Feb 13
# Module: DATA QUALITY ASSESSMENT

# This script is designed to evaluate the reliability of HMIS data by
# examining three key components: outliers, completeness, and consistency.  

# Ce script est conçu pour évaluer la fiabilité des données du HMIS en analysant 
# trois éléments clés : la détection des valeurs aberrantes, l’évaluation de la complétude et la mesure de la cohérence.



# DATA: nigeria_imported_data.csv

# ------------------------------------- PARAMETERS -----------------------------------------------------------
# Outlier Analysis Parameters
outlier_params <- list(
  outlier_pc_threshold = OUTLIER_PROPORTION_THRESHOLD,  # Threshold for proportional contribution to flag outliers
  count_threshold = MINIMUM_COUNT_THRESHOLD             # Minimum count to consider for outlier adjustment
)


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
default_dqa_ind <- DQA_INDICATORS


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
library(zoo)
library(stringr)
library(dplyr)       
library(tidyr)


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
      outlier_mad = ifelse(!is.na(mad_residual) & mad_residual > MADS, 1, 0)  # Flag outliers based on MAD
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
  
  # Step 5: Select relevant columns for output
  print("Selecting relevant columns for output...")
  outlier_data <- data %>%
    select(facility_id, all_of(geo_cols), indicator_common_id, year, month, count, 
           median_volume, mad_volume, mad_residual, outlier_mad, pc, outlier_flag)
  
  # Step 6: Bring back period_id and quarter_id from original data
  print("Merging period_id and quarter_id back into outlier data...")
  outlier_data <- outlier_data %>%
    left_join(data %>% select(facility_id, indicator_common_id, year, month, period_id, quarter_id),
              by = c("facility_id", "indicator_common_id", "year", "month"))
  
  return(outlier_data)
}


# PART 2-A Consistency Analysis - Geo Level -----------------------------------------------------------------
geo_consistency_analysis <- function(data, geo_cols, geo_level, consistency_params) {
  # Extract required pairs and ranges from parameters
  required_pairs <- consistency_params$consistency_pairs
  consistency_ranges <- consistency_params$consistency_ranges
  
  # Define all possible geographic levels
  geo_levels <- c("admin_area_1", "admin_area_2", "admin_area_3", "admin_area_4", "admin_area_5", "admin_area_6", "admin_area_7", "admin_area_8")
  
  # Identify the geographic columns up to the selected geo_level
  relevant_geo_cols <- geo_levels[seq_len(match(geo_level, geo_levels, nomatch = length(geo_levels)))]
  
  # Ensure only existing columns in data are selected
  relevant_geo_cols <- intersect(relevant_geo_cols, geo_cols)
  
  # Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier_flag == 1, NA, count))
  
  # Aggregate data at the selected geographic level
  aggregated_data <- data %>%
    group_by(across(all_of(c(relevant_geo_cols, "indicator_common_id", "year", "month", "period_id", "quarter_id")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(relevant_geo_cols, "year", "month", "period_id", "quarter_id"),
      names_from = "indicator_common_id",
      values_from = "count",
      values_fill = list(count = 0)
    )
  
  print("Checking available indicators in dataset...")
  print(unique(data$indicator_common_id))
  
  # Process each pair in isolation
  pair_results <- list()
  
  for (pair_name in names(required_pairs)) {
    pair <- required_pairs[[pair_name]]
    col1 <- pair[1]
    col2 <- pair[2]
    
    # Ensure both indicators exist in `wide_data`
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      print(paste("Processing pair:", pair_name, "-", col1, "/", col2))
      
      # Retrieve consistency range for the current pair
      if (pair_name %in% names(consistency_ranges)) {
        range <- consistency_ranges[[pair_name]]
        
        # Extract lower and upper bounds correctly
        if (is.list(range)) {
          lower_bound <- range$lower
          upper_bound <- range$upper
        } else if (is.vector(range) && !is.null(names(range))) {
          lower_bound <- as.numeric(range["lower"])
          upper_bound <- as.numeric(range["upper"])
        } else {
          warning(paste("Unexpected range structure for:", pair_name))
          lower_bound <- NA_real_
          upper_bound <- NA_real_
        }
      } else {
        warning(paste("No range found for pair:", pair_name))
        lower_bound <- NA_real_
        upper_bound <- NA_real_
      }
      
      # Compute consistency ratios and flag consistency
      pair_data <- wide_data %>%
        mutate(
          ratio_type = pair_name,
          consistency_ratio = if_else(
            !!sym(col2) > 0, 
            !!sym(col1) / !!sym(col2), 
            NA_real_
          ),
          sconsistency = case_when(
            !is.na(consistency_ratio) & consistency_ratio >= lower_bound & consistency_ratio <= upper_bound ~ 1,
            !is.na(consistency_ratio) ~ 0,
            TRUE ~ NA_integer_
          )
        ) %>%
        select(all_of(c(relevant_geo_cols, "year", "month", "period_id", "quarter_id", "ratio_type", "consistency_ratio", "sconsistency")))
      
      pair_results[[pair_name]] <- pair_data
    } else {
      print(paste("Skipping pair - missing columns:", col1, col2))
    }
  }
  
  # Combine results from all pairs
  combined_data <- bind_rows(pair_results)
  
  # Ensure `sconsistency` is converted to integer
  combined_data <- combined_data %>%
    mutate(sconsistency = as.integer(sconsistency))
  
  return(combined_data)
}
expand_geo_consistency_to_facilities <- function(facility_metadata, geo_consistency_results, geo_level) {
  print(paste("Expanding geo-level consistency results using:", geo_level, "..."))
  # Detect all available geographic levels in `facility_metadata`
  available_geo_levels <- grep("^admin_area_[0-9]+$", colnames(facility_metadata), value = TRUE)
  
  # Ensure the chosen geo_level exists, fallback to the lowest available level if missing
  if (!(geo_level %in% available_geo_levels)) {
    geo_level <- tail(sort(available_geo_levels), 1)  # Pick the lowest available (highest number)
    print(paste("Chosen geo_level not found! Falling back to:", geo_level))
  } else {
    print(paste("Using user-specified geo_level:", geo_level))
  }
  # Step 1: Extract facility list with the specified geographic level
  facility_list <- facility_metadata %>%
    select(facility_id, all_of(geo_level)) %>%
    distinct()

  
  # Step 2: Expand geo consistency results by duplicating values across all facilities in the same area
  facility_consistency_results <- facility_list %>%
    left_join(geo_consistency_results, by = geo_level, relationship = "many-to-many")
  
  print("Successfully expanded geo-level consistency results to all facilities")
  return(facility_consistency_results)
}

# PART 3 COMPLETENESS ----------------------------------------------------------------------------------------
completeness_analysis <- function(data, geo_cols) {
  print("Performing completeness analysis for each indicator separately with smoothing...")
  
  # List to store completeness data for each indicator
  completeness_list <- list()
  
  # Get unique indicators
  indicators <- unique(data$indicator_common_id)
  
  for (indicator in indicators) {
    print(paste("Processing:", indicator))
    
    # Filter data for the current indicator
    indicator_data <- data %>% filter(indicator_common_id == indicator)
    
    # Get the count of unique facilities reporting this indicator
    unique_facilities <- indicator_data %>% distinct(facility_id) %>% nrow()
    print(paste("Number of facilities reporting for", indicator, ":", unique_facilities))
    
    # Find first and last record dates for this indicator
    first_record <- indicator_data %>%
      summarise(first_date = min(as.Date(paste(year, month, "1", sep = "-")), na.rm = TRUE)) %>%
      pull(first_date)
    
    last_record <- indicator_data %>%
      summarise(last_date = max(as.Date(paste(year, month, "1", sep = "-")), na.rm = TRUE)) %>%
      pull(last_date)
    
    print(paste("First record for", indicator, ":", first_record))
    print(paste("Last record for", indicator, ":", last_record))
    
    # Step 1: Generate a full grid of year-month combinations ONLY for this indicator's reporting period
    all_year_months <- indicator_data %>%
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
    
    # Step 2: Get the facilities that reported for this indicator
    reporting_facilities <- indicator_data %>% distinct(facility_id)
    
    # Step 3: Expand only for these facilities and their reporting period
    complete_month_grid <- reporting_facilities %>%
      crossing(all_year_months) %>%
      mutate(indicator_common_id = indicator)
    
    # Drop geo_cols temporarily to avoid missing values for added rows
    indicator_data_no_geo <- indicator_data %>% select(-all_of(geo_cols))
    
    # Step 4: Left join with data to retain all facility-months within reporting period
    expanded_data <- complete_month_grid %>%
      left_join(indicator_data_no_geo, by = c("facility_id", "indicator_common_id", "year", "month")) %>%
      mutate(
        date = as.Date(paste(year, month, "1", sep = "-")),
        period_id = as.integer(paste0(year, sprintf("%02d", month))),
        quarter_id = as.integer(paste0(year, sprintf("%02d", (month - 1) %/% 3 + 1))),
        negdate = as.integer(date) * -1
      )
    
    # Step 5: Apply smoothing per facility (prevents premature incompleteness marking)
    smoothed_data <- expanded_data %>%
      group_by(facility_id) %>%
      arrange(date) %>%
      mutate(
        first_valid_report = cumsum(!is.na(count) & count > 0) > 0,
        last_valid_report = rev(cumsum(rev(!is.na(count) & count > 0)) > 0),
        
        # Compute trailing months since last report
        trailing_months = ifelse(!is.na(count) & count > 0, 0, NA_integer_),
        trailing_months = zoo::na.locf(trailing_months, fromLast = TRUE, na.rm = FALSE),
        trailing_months = ifelse(is.na(trailing_months), 12, trailing_months + 1),
        
        # Mark completeness only for valid reporting periods
        completeness_flag = ifelse(first_valid_report & last_valid_report, ifelse(is.na(count), 0, 1), NA_real_)
      ) %>%
      ungroup()
    
    # Step 6: Summarize completeness at facility-month level
    facility_month_data <- smoothed_data %>%
      group_by(
        facility_id, 
        indicator_common_id, 
        year, 
        month, 
        period_id,
        quarter_id
      ) %>%
      summarise(
        count = sum(count, na.rm = TRUE),
        reported_facility_months = sum(completeness_flag, na.rm = TRUE),  # Count of reported months
        completeness_rate = mean(completeness_flag, na.rm = TRUE),        # Proportion completeness per facility-month
        .groups = "drop"
      ) %>%
      mutate(
        completeness_flag = ifelse(reported_facility_months > 0, 1, 0)  # Ensure completeness_flag is set
      )
    
    # Store results for this indicator
    completeness_list[[indicator]] <- facility_month_data
  }
  
  # Step 7: Combine all indicators into a single dataset
  final_completeness_data <- bind_rows(completeness_list)
  
  # Step 8: Fix the many-to-many join issue for facility metadata
  facility_metadata_unique <- data %>%
    select(facility_id, all_of(geo_cols)) %>%
    distinct(facility_id, .keep_all = TRUE)
  
  # Step 9: Rejoin geo_cols
  final_completeness_data <- final_completeness_data %>%
    left_join(facility_metadata_unique, by = "facility_id")
  
  # Step 10: Return the summarized completeness data
  return(final_completeness_data)
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
    filter(indicator_common_id %in% dqa_indicators_to_use)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% dqa_indicators_to_use)
  
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
      max_points = 2 * length(dqa_indicators_to_use),  # Maximum possible points
      completeness_outlier_score = total_indicator_points / max_points,  # NEW: Normalized completeness/outlier score
      dqa_outlier_completeness = ifelse(total_indicator_points == max_points, 1, 0),  # Binary pass/fail
      .groups = "drop"
    )
  
  # Step 4: Merge with Consistency Data (Now at Facility-Month Level)
  dqa_data <- dqa_facility_month %>%
    left_join(consistency_expanded %>% select(facility_id, year, month, starts_with("pair_")), 
              by = c("facility_id", "year", "month")) %>%
    mutate(across(starts_with("pair_"), ~replace_na(.x, 0)))   # Fill missing consistency scores with 0
  
  # Step 4B: Drop "pair_delivery" if it exists
  if ("pair_delivery" %in% colnames(dqa_data)) {
    dqa_data <- dqa_data %>% select(-pair_delivery)
  }
  
  # Step 5: Compute Final DQA Score Based on All Conditions
  consistency_cols <- setdiff(names(consistency_expanded), c("facility_id", "year", "month", geo_cols, "pair_delivery"))
  
  dqa_data <- dqa_data %>%
    mutate(
      total_consistency_pass = rowSums(select(., all_of(consistency_cols)) == 1, na.rm = TRUE),  # Count consistency passes
      max_consistency_checks = length(consistency_cols),  # Total consistency checks available
      consistency_score = ifelse(max_consistency_checks > 0, total_consistency_pass / max_consistency_checks, NA),  # Normalized
      
      all_pairs_pass = ifelse(total_consistency_pass == max_consistency_checks, 1, 0),  # Binary pass/fail for consistency
      
      dqa_mean = (completeness_outlier_score + consistency_score) / 2,
      dqa_score = ifelse(dqa_outlier_completeness == 1 & all_pairs_pass == 1, 1, 0),  # Binary DQA score (1=pass, 0=fail)
      
      period_id = as.integer(paste0(year, sprintf("%02d", month))),
      quarter_id = as.integer(paste0(year, sprintf("%02d", (month - 1) %/% 3 + 1)))
    ) %>%
    
    select(all_of(geo_cols), facility_id, year, month, period_id, quarter_id,
           completeness_outlier_score, consistency_score, dqa_mean, dqa_score)  # Include all new scores
  
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
    filter(indicator_common_id %in% dqa_indicators_to_use)
  
  outlier_data <- outlier_data %>%
    filter(indicator_common_id %in% dqa_indicators_to_use)
  
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
      max_points = 2 * length(dqa_indicators_to_use),  # Max points per indicator
      dqa_score = ifelse(total_indicator_points == max_points, 1, 0),
      .groups = "drop"
    )
  
  return(dqa_results)
}

# ------------------- Main Execution ----------------------------------------------------------------------------
inputs <- load_and_preprocess_data("nigeria_imported_data.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols


# Validate Consistency Pairs
consistency_params <- validate_consistency_pairs(consistency_params, data)


# Dynamically set DQA_INDICATORS based on available indicators
dqa_indicators_to_use <- intersect(DQA_INDICATORS, unique(data$indicator_common_id))

# Ensure DQA_INDICATORS is empty if none of the key indicators exist
if (length(dqa_indicators_to_use) == 0) {
  dqa_indicators_to_use <- character(0)  # Empty vector
}

print(paste("DQA indicators selected:", ifelse(length(dqa_indicators_to_use) > 0, paste(dqa_indicators_to_use, collapse = ", "), "None found")))

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
    geo_level = GEOLEVEL,
    consistency_params = consistency_params
  )

} else {
  print("No valid consistency pairs found. Skipping consistency analysis...")
  geo_consistency_results <- NULL
}


# Expend consistency - join to facility
facility_consistency_results <- expand_geo_consistency_to_facilities(
  facility_metadata = completeness_results,  
  geo_consistency_results = geo_consistency_results,  
  geo_level = GEOLEVEL  
)

# Ensure consistency data has one row per facility-month by pivoting ratio pairs
consistency_expanded <- facility_consistency_results %>%
  select(facility_id, year, month, ratio_type, sconsistency) %>%  # Keep only necessary columns
  pivot_wider(
    names_from = ratio_type,   
    values_from = sconsistency, 
    values_fill = list(sconsistency = 0)
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%  # Ensure no remaining NAs
  distinct(facility_id, year, month, .keep_all = TRUE)

# Restore geo_cols AFTER pivoting to avoid duplication issues
consistency_expanded <- consistency_expanded %>%
  left_join(
    facility_consistency_results %>%
      select(facility_id, year, month, any_of(geo_cols)) %>% distinct(),  # Use any_of() instead of all_of()
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
