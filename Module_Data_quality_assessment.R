# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 18
# Module: DATA QUALITY ASSESSMENT

# DATA: guinea_imported_dataset.csv

# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE: output_outliers.csv           # Detailed facility-level data with identified outliers and adjusted volumes.
# FILE: completeness_long_format.csv  # Facility-level completeness data in a detailed long format, including reported and expected months.
# FILE: output_consistency_geo.csv    # Geo-level consistency results
# FILE: output_consistency_facility.csv # Facility-level consistency results
# FILE: dqa_summary.csv               # Summary of DQA results by admin_area_2 and priority indicator (% of facilities meeting DQA criteria).
# FILE: facility_dqa.csv              # Facility-level results from DQA analysis.
     

# ------------------------------------- FOR R testing ------------------------------------------------------------------------------------------
# FILE: top_outliers_data.csv         # Top outliers based on percentage change in volume, highlighting extreme deviations.
# FILE: volume_increase_data.csv      # Monthly summary of volume changes due to outlier adjustments.
# FILE: completeness_summary.csv      # Aggregated completeness data at various geographic levels (e.g., national, regional).
# FILE: adjusted_national_results.csv # National-level dataset summarizing adjusted service volumes for each scenario.
# FILE: completeness_aggregate.csv    # Aggregated completeness data summarizing facility-month reporting across geographic levels.


# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)

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
facility_consistency_analysis <- function(data, geo_cols_facility = "facility_id") {
  print("Performing facility-level consistency analysis...")
  
  # Define required pairs for consistency checks # Add code block to allow for more pairs
  required_pairs <- list(
    pair_anc = c("anc1", "anc4"),
    pair_delivery = c("delivery", "bcg"),
    pair_pnc = c("delivery", "pnc1"),
    pair_penta = c("penta1", "penta3")
  )
  
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
      pair_data <- wide_data %>%
        mutate(
          !!ratio_name := if_else(!!sym(col2) > 0, !!sym(col1) / !!sym(col2), NA_real_),
          !!sratio_name := case_when(
            pair_name == "pair_delivery" ~ (!!sym(ratio_name) >= 0.7 & !!sym(ratio_name) <= 1.3),
            TRUE ~ !!sym(ratio_name) > 1
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
geo_consistency_analysis <- function(data, geo_cols) {
  print("Performing geo-level consistency analysis...")
  
  # Step 1: Define required pairs for consistency checks
  required_pairs <- list(
    pair_anc = c("anc1", "anc4"),
    pair_delivery = c("delivery", "bcg"),
    pair_pnc = c("delivery", "pnc1"),
    pair_penta = c("penta1", "penta3")
  )
  
  # Step 2: Exclude Outliers
  data <- data %>%
    mutate(count = ifelse(outlier == 1, NA, count))
  
  # Step 3: Aggregate data at geographic level
  aggregated_data <- data %>%
    group_by(across(all_of(c(geo_cols, "indicator_common_id", "year")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Step 4: Pivot to wide format
  wide_data <- aggregated_data %>%
    pivot_wider(
      id_cols = c(geo_cols, "year"),
      names_from = "indicator_common_id",
      values_from = "count",
      values_fill = list(count = 0)
    )
  
  # Step 5: Process each pair in isolation
  pair_results <- list()
  
  for (pair_name in names(required_pairs)) {
    pair <- required_pairs[[pair_name]]
    col1 <- pair[1]
    col2 <- pair[2]
    ratio_name <- paste0(pair_name, "_ratio")
    sratio_name <- paste0(pair_name, "_sratio")
    
    if (all(c(col1, col2) %in% colnames(wide_data))) {
      pair_data <- wide_data %>%
        mutate(
          consistency_ratio = if_else(!!sym(col2) > 0, !!sym(col1) / !!sym(col2), NA_real_),
          sconsistency = case_when(
            pair_name == "pair_delivery" ~ (consistency_ratio >= 0.7 & consistency_ratio <= 1.3),
            TRUE ~ consistency_ratio > 1
          )
        ) %>%
        mutate(ratio_type = pair_name) %>% # Directly assign the ratio type
        select(all_of(c(geo_cols, "year", "ratio_type", "consistency_ratio", "sconsistency")))
      
      pair_results[[pair_name]] <- pair_data
    }
  }
  
  # Step 6: Combine results from all pairs
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
  
  # Step 2: Build a table of all (year, month) combos from min_month..max_month for each year
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
    geo_cols
) {
  print("Performing strict DQA analysis (facility-level consistency)...")
  
  # Step 1: Define relevant indicators for DQA
  relevant_indicators <- c("opd", "penta1", "anc1")  # List of priority indicators
  
  # Map ratio types to corresponding indicators
  ratio_mapping <- tibble::tibble(
    ratio_type = c("pair_anc_ratio", "pair_delivery_ratio", "pair_pnc_ratio", "pair_penta_ratio"),
    indicator_common_id = c("anc1", "delivery", "pnc1", "penta1")
  )
  
  # Merge completeness, consistency, and outlier data
  merged_data <- completeness_data %>%
    filter(indicator_common_id %in% relevant_indicators) %>%  # Filter relevant indicators
    distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE) %>%
    left_join(
      outlier_data %>%
        distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id"))), .keep_all = TRUE) %>%
        select(all_of(geo_cols), facility_id, year, indicator_common_id, outlier_flag),
      by = c(geo_cols, "facility_id", "year", "indicator_common_id")
    ) %>%
    left_join(
      consistency_data %>%
        left_join(ratio_mapping, by = "ratio_type") %>%
        distinct(across(all_of(c(geo_cols, "facility_id", "year", "indicator_common_id", "ratio_type"))), .keep_all = TRUE) %>%
        select(all_of(geo_cols), facility_id, year, indicator_common_id, sconsistency, ratio_type),
      by = c(geo_cols, "facility_id", "year", "indicator_common_id")
    )
  
  # Step 2: Create DQA criteria
  merged_data <- merged_data %>%
    mutate(
      dqa_temp = case_when(
        # OPD has no consistency requirement
        indicator_common_id == "opd" &
          completeness == 1 & 
          outlier_flag == 0 ~ 1,
        
        # Penta1 & ANC1 require sconsistency == 1 or NA?
        indicator_common_id %in% c("penta1", "anc1") &
          completeness == 1 &
          outlier_flag == 0 &
          (is.na(sconsistency) | sconsistency == 1) ~ 1,
        
        # Irrelevant indicators are automatically set to 0
        TRUE ~ 0
      )
    )
  
  # Step 3: Aggregate DQA results for summary
  dqa_summary <- merged_data %>%
    group_by(admin_area_2, year, indicator_common_id) %>%
    summarise(
      dqa_pass_rate = mean(dqa_temp, na.rm = TRUE),  # Calculate % of facilities passing DQA test
      facility_count = n(),                          # Count facilities in each group
      .groups = "drop"
    )
  
  # Step 4: Return results as a list
  return(list(
    dqa_results = merged_data %>% select(all_of(geo_cols), facility_id, year, indicator_common_id, ratio_type, dqa_temp),
    dqa_summary = dqa_summary
  ))
}

# -----------------------------------------------------------------------------
# Main Execution --------------------------------------------------------------
# -----------------------------------------------------------------------------
inputs <- load_and_preprocess_data("guinea_imported_dataset.csv")
data <- inputs$data
geo_cols <- inputs$geo_cols

# Outlier Analysis
print("Running outlier analysis...")
outlier_results    <- outlier_analysis(data, geo_cols)
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
facility_consistency_results <- facility_consistency_analysis(outlier_data_main) # Call facility-level consistency analysis

# Consistency Analysis (Geo-Level)
print("Running consistency analysis (geo-level)...")
geo_consistency_results <- geo_consistency_analysis(outlier_data_main, geo_cols)

# Prepare outlier_data for DQA (rename 'outlier' to 'outlier_flag')
outlier_data_for_dqa <- outlier_data_main %>%
  rename(outlier_flag = outlier)

# Run DQA (Strict, Facility-Level)
print("Running DQA (strict, facility-level consistency)...")
dqa_results <- dqa_analysis_strict_facility_consistency(
  completeness_data = completeness_data,
  consistency_data  = facility_consistency_results, 
  outlier_data      = outlier_data_for_dqa,
  geo_cols          = geo_cols
)


# -----------------------------------------------------------------------------
# Visualization  --------------------------------------------------------------
# -----------------------------------------------------------------------------
viz_colors <- c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")

# PART 1 - VIZ
# A - Outliers Heatmap
heatmap_outliers <- outlier_results$heatmap_data %>%
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
    title = "Percent change of volume due to outliers",
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


# B - Volume Increase Due to Outliers
bar_chart <- ggplot(outlier_results$volume_increase_data, aes(x = month, y = percent_change, fill = admin_area_1)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~indicator_common_id, scales = "free_y") +
  labs(
    title = "Volume change due to outliers",
    x = "Month",
    y = "% Change",
    fill = "Administrative area"
  ) +
  theme_minimal()
print(bar_chart)

# PART 2 - VIZ
# Consistency Heatmap
print("Creating consistency heatmap...")

# Step 1: Summarize consistency benchmarks by geographic area
consistency_summary <- geo_consistency_results %>%
  group_by(admin_area_2, ratio_type) %>%
  summarise(
    percent_meeting_benchmark = mean(sconsistency, na.rm = TRUE) * 100,  # Calculate % meeting consistency
    .groups = "drop"
  )

# Step 2: Pivot data for heatmap
consistency_heatmap_data <- consistency_summary %>%
  pivot_wider(
    names_from = ratio_type,
    values_from = percent_meeting_benchmark,
    values_fill = 0  # Fill missing with 0 if no data
  )

# Step 3: Create the heatmap plot
consistency_heatmap <- consistency_summary %>%
  ggplot(aes(x = ratio_type, y = admin_area_2, fill = percent_meeting_benchmark)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
    values = rescale(c(0, 25, 50, 75, 100)),
    limits = c(0, 100),
    name = "Percent Meeting\nConsistency (%)"
  ) +
  labs(
    title = "Consistency Benchmarks by Administrative Area",
    subtitle = "Percent of administrative areas meeting consistency benchmarks",
    x = "Consistency Benchmark",
    y = "Administrative Area",
    fill = "Consistency (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Step 4: Print the heatmap
print(consistency_heatmap)

# Completeness Heatmap
print("Creating completeness heatmap...")
heatmap_plot <- ggplot(completeness_results$geography_aggregate, aes(x = indicator_common_id, y = admin_area_2, fill = completeness_percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = viz_colors,
    values = rescale(c(0, 0.25, 0.50, 0.90, 1)),  # Adjust thresholds
    limits = c(0, 1),               
    name = "Completeness (%)",
    labels = scales::percent_format(accuracy = 1)  # Format labels as percentages
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

# PART 3 - VIZ
generate_dqa_heatmap <- function(dqa_results, geo_col = "admin_area_2", year_col = "year") {
  print("Aggregating DQA results at admin_area_2 level...")
  
  # Access the dqa_results dataframe from the input list
  dqa_data <- dqa_results$dqa_results
  
  # Step 1: Aggregate data to calculate % of facilities meeting DQA criteria
  dqa_aggregated <- dqa_data %>%
    group_by(!!sym(geo_col), !!sym(year_col)) %>%
    summarise(
      total_facilities = n(),
      adequate_quality = sum(dqa_temp == 1, na.rm = TRUE),
      adequate_quality_percentage = 100 * adequate_quality / total_facilities,
      .groups = "drop"
    )
  
  print("Data aggregation complete. Preparing heatmap...")
  
  # Step 2: Generate Heatmap Plot
  heatmap_plot_DQA <- ggplot(dqa_aggregated, aes_string(x = year_col, y = geo_col, fill = "adequate_quality_percentage")) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
      values = scales::rescale(c(0, 25, 50, 75, 100)),
      limits = c(0, 100),
      name = "Facilities meeting\nDQA criteria (%)"
    ) +
    labs(
      title = "Data Quality Assessment",
      subtitle = "Percentage of facilities meeting DQA criteria, aggregated by region and year",
      x = "Year",
      y = "Administrative Region (Level 2)",
      fill = "Adequate quality (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "italic"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid = element_blank()
    )
  
  print("DQA heatmap plot created. Displaying now...")
  print(heatmap_plot_DQA)
  
  # Step 3: Return aggregated data and the plot
  return(list(
    heatmap_data = dqa_aggregated,
    heatmap_plot_DQA = heatmap_plot_DQA
  ))
}
# Generate DQA heatmap
heatmap_dqa <- generate_dqa_heatmap(dqa_results)





# -------------------------------- SAVE DATA OUTPUTS --------------------------------------------------
print("Saving all data outputs from outlier analysis...")
write.csv(outlier_results$outlier_data, "output_outliers.csv", row.names = FALSE)            # Facility-level outlier data
write.csv(outlier_results$top_outliers_data, "top_outliers_data.csv", row.names = FALSE)      # Top outliers summary
write.csv(outlier_results$volume_increase_data, "volume_increase_data.csv", row.names = FALSE) # Monthly volume changes

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


# -------------------------------- SAVE VISUALIZATIONS --------------------------------------------------
print("Saving all plots...")
ggsave("heatmap_outliers.png", plot = heatmap_outliers, width = 12, height = 8, dpi = 300)    # 1) Save Outliers Heatmap
ggsave("bar_chart_outliers.png", plot = bar_chart, width = 12, height = 8, dpi = 300)         # 2) Save Volume Increase Due to Outliers
ggsave("consistency_heatmap.png", plot = consistency_heatmap, width = 12, height = 8, dpi = 300)  # 3) Save Consistency Heatmap
ggsave("completeness_heatmap.png", plot = heatmap_plot, width = 12, height = 8, dpi = 300)        # 4) Save Completeness Heatmap
ggsave("heatmap_DQA.png", plot = heatmap_dqa$heatmap_plot_DQA, width = 12, height = 8, dpi = 300) # 5) Save DQA Heatmap

print("DQA Analysis completed. All outputs and plots saved.")
