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
library(data.table)

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
# Function to generate full time series per indicator
generate_full_series_per_indicator <- function(outlier_data, indicator_id, timeframe) {
  print(paste("Processing indicator:", indicator_id))  # Track progress
  
  # Subset data for the given indicator
  indicator_subset <- outlier_data[indicator_common_id == indicator_id, .(facility_id, indicator_common_id, year, month, count)]
  print(paste("Subset data size for", indicator_id, ":", nrow(indicator_subset)))
  
  # Get first and last reporting month for this indicator
  time_range <- timeframe[indicator_common_id == indicator_id]
  first_month <- time_range$first_month
  last_month <- time_range$last_month
  
  print(paste("Time range for", indicator_id, ":", first_month, "to", last_month))
  
  # Generate full month sequence
  month_seq <- seq(from = first_month, to = last_month, by = "1 month")
  
  # Create a full facility-month grid
  complete_grid <- CJ(
    facility_id = unique(indicator_subset$facility_id),
    date = month_seq
  )[, `:=` (
    indicator_common_id = indicator_id,
    year = year(date),
    month = month(date),
    period_id = sprintf("%04d%02d", year(date), month(date)),  # YYYYMM format
    quarter_id = sprintf("%04d%02d", year(date), ((month(date) - 1) %/% 3) + 1)
  )]
  
  print(paste("Generated complete grid for", indicator_id, "with", nrow(complete_grid), "rows"))
  
  # Ensure `indicator_subset` has a `date` column before merging
  indicator_subset[, date := as.Date(paste(year, month, "1", sep = "-"))]
  
  # Merge with original data to retain counts
  complete_data <- merge(
    complete_grid, indicator_subset,
    by = c("facility_id", "indicator_common_id", "year", "month", "date"),
    all.x = TRUE
  )[, .(facility_id, indicator_common_id, year, month, count, date, period_id, quarter_id)]
  
  print(paste("Merged data size for", indicator_id, ":", nrow(complete_data)))
  
  return(complete_data)
}


# Main processing function
process_completeness <- function(outlier_data_main) {
  print("Starting completeness processing...")
  setDT(outlier_data_main)  # Convert to data.table
  
  # Identify first and last reported month for each indicator
  indicator_timeframe <- outlier_data_main[, .(
    first_month = min(as.Date(paste(year, month, "1", sep = "-")), na.rm = TRUE),
    last_month = max(as.Date(paste(year, month, "1", sep = "-")), na.rm = TRUE)
  ), by = indicator_common_id]
  
  print(paste("Identified timeframes for", nrow(indicator_timeframe), "indicators"))

  
  # Deduplicate to ensure one row per facility
  geo_lookup <- unique(outlier_data_main[, c("facility_id", geo_cols), with = FALSE])
  
  
  print(paste("Geo data extracted for", nrow(geo_lookup), "facilities"))
  
  # Process each indicator separately to optimize memory usage
  completeness_list <- lapply(unique(outlier_data_main$indicator_common_id), function(ind) {
    print(paste("Starting processing for indicator:", ind))
    
    # Generate full time series for the indicator
    complete_data <- generate_full_series_per_indicator(outlier_data_main, ind, indicator_timeframe)
    
    print(paste("Applying completeness tagging for", ind))
    
    # Apply completeness tagging
    complete_data <- complete_data[order(facility_id, date)]  # Order data
    
    complete_data[, has_reported := !is.na(count), by = facility_id]  # Identify reporting months
    complete_data[, first_report_idx := cumsum(has_reported) > 0, by = facility_id]  # First report
    complete_data[, last_report_idx := rev(cumsum(rev(has_reported)) > 0), by = facility_id]  # Last report
    
    complete_data[, completeness_flag := fifelse(
      !first_report_idx, 2, fifelse(
        has_reported, 1, fifelse(
          first_report_idx & last_report_idx, 0, 2
        )
      )
    ), by = facility_id]
    
    print(paste("Completeness tagging done for", ind, "- Removing flagged rows..."))
    
    # Attach geo info: Find the admin area for each facility
    complete_data <- merge(complete_data, geo_lookup, by = "facility_id", all.x = TRUE)
    
    # Remove rows where completeness_flag == 2
    result <- complete_data[completeness_flag != 2, c("facility_id", "indicator_common_id", "year", "month", "count", "completeness_flag", "period_id", "quarter_id", geo_cols), with = FALSE]
    
    print(paste("Final dataset size for", ind, ":", nrow(result)))
    
    return(result)
  })
  
  # Combine results efficiently
  print("Combining all indicator datasets...")
  completeness_long <- rbindlist(completeness_list, use.names = TRUE, fill = TRUE)
  if ("facility_id.1" %in% colnames(completeness_long)) {
    completeness_long <- completeness_long[, !("facility_id.1"), with = FALSE]
  }
  print("Completeness processing finished!")
  return(completeness_long)
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
completeness_results <- process_completeness(outlier_data_main)


geo_cols_filtered <- setdiff(geo_cols, "facility_id")

# Extract unique facilities and their geo/admin_area columns
facility_metadata <- completeness_results %>%
  select(any_of(c("facility_id", geo_cols_filtered))) %>%
  distinct()

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


if (!is.null(geo_consistency_results) && !"facility_id" %in% colnames(geo_consistency_results)) {
  print("Expanding geo-level consistency results to facilities...")
  facility_consistency_results <- expand_geo_consistency_to_facilities(
    facility_metadata = facility_metadata,  
    geo_consistency_results = geo_consistency_results,  
    geo_level = GEOLEVEL  
  )
} else {
  print("Geo consistency results already at facility level. Skipping expansion...")
  facility_consistency_results <- geo_consistency_results
}


consistency_expanded <- facility_consistency_results %>%
  pivot_wider(
    id_cols = c(facility_id, year, month, any_of(geo_cols)),
    names_from = ratio_type,   
    values_from = sconsistency, 
    values_fill = list(sconsistency = 0)
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%  # Ensure no remaining NAs
  distinct(facility_id, year, month, .keep_all = TRUE)  # Ensure unique rows



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
