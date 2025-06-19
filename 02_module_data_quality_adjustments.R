
PROJECT_DATA_HMIS <- "hmis_ghana.csv"

# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 June 10

# This script dynamically adjusts raw data for:
#   1. Outliers: Replaces flagged outliers with 12-month rolling averages (excluding outliers).
#   2. Completeness: Replaces missing count with 12-month rolling averages (excluding outliers).

# Ce script ajuste dynamiquement les données brutes pour :
#   1. Les valeurs aberrantes : Remplace les valeurs identifiées comme aberrantes par une moyenne mobile sur 12 mois (hors valeurs aberrantes).
#   2. L'exhaustivité : Remplace les valeurs manquantes par une moyenne mobile sur 12 mois (hors valeurs aberrantes).

# -------------------------- KEY OUTPUT ----------------------------------------------------------------------
# FILE: M2_adjusted_data.csv              # Dataset including facility-level adjusted volumes for all adjustment scenarios.
# FILE: M2_adjusted_data_admin_area.csv   # Dataset including admin-level adjusted volumes for all adjustment scenarios.
# FILE: M2_adjusted_data_national.csv     # Dataset including national-level adjusted volumes for all adjustment scenarios.


# Load Required Libraries -----------------------------------------------------------------------------------
library(data.table)  # For fast data processing & merging
library(zoo)         # For rolling averages
library(stringr)     # For `str_subset()`
library(dplyr)
library(lubridate)

EXCLUDED_FROM_ADJUSTMENT <- c("u5_deaths", "maternal_deaths")

raw_data <- read.csv(PROJECT_DATA_HMIS)
outlier_data <- read.csv("M1_output_outliers.csv")
completeness_data <- read.csv("M1_output_completeness.csv")

# Define Functions ------------------------------------------------------------------------------------------
geo_cols <- colnames(raw_data)[grepl("^admin_area_[0-9]+$", colnames(raw_data))]

# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments <- function(raw_data, completeness_data, outlier_data,
                              adjust_outliers = FALSE, adjust_completeness = FALSE) {
  message("Running adjustments...")
  
  setDT(raw_data)
  setDT(outlier_data)
  setDT(completeness_data)
  
  # Merge
  data_adj <- merge(completeness_data,
                    outlier_data[, .(facility_id, indicator_common_id, period_id, outlier_flag)],
                    by = c("facility_id", "indicator_common_id", "period_id"),
                    all.x = TRUE)
  data_adj[, outlier_flag := fifelse(is.na(outlier_flag), 0, outlier_flag)]
  
  data_adj <- merge(data_adj,
                    raw_data[, .(facility_id, indicator_common_id, period_id, count)],
                    by = c("facility_id", "indicator_common_id", "period_id"),
                    all.x = TRUE)
  
  data_adj[, count_working := as.numeric(count)]
  
  # ---------------- Outlier Adjustment ----------------
  if (adjust_outliers) {
    message(" -> Adjusting outliers using enhanced logic with fallback to same month last year...")
    
    data_adj[, date := as.Date(paste0(substr(period_id, 1, 4), "-", substr(period_id, 6, 7), "-01"))]
    setorder(data_adj, facility_id, indicator_common_id, date)
    
    data_adj[, count_working := as.numeric(count)]
    data_adj[, adj_method := NA_character_]
    data_adj[, adjust_note := NA_character_]
    
    data_adj <- data_adj[, {
      result <- copy(.SD)
      n <- .N
      for (i in seq_len(n)) {
        if (outlier_flag[i] != 1) next
        current_date <- date[i]
        
        # Identify valid (non-outlier, >0) values
        valid_idx <- which(outlier_flag == 0 & count > 0)
        
        if (i <= 6) {
          # First 6 months: forward average
          fwd_idx <- valid_idx[valid_idx > i & date[valid_idx] <= current_date %m+% months(12)]
          vals <- count[fwd_idx][1:6]
          if (length(vals) == 6) {
            result$count_working[i] <- mean(vals)
            result$adj_method[i] <- "roll6_forward"
            next
          }
        } else if (i >= n - 5) {
          # Last 6 months: backward average
          back_idx <- valid_idx[valid_idx < i & date[valid_idx] >= current_date %m-% months(12)]
          vals <- rev(count[back_idx])[1:6]
          if (length(vals) == 6) {
            result$count_working[i] <- mean(vals)
            result$adj_method[i] <- "roll6_backward"
            next
          }
        } else {
          # Middle: centered average
          before_idx <- valid_idx[valid_idx < i & date[valid_idx] >= current_date %m-% months(12)]
          after_idx  <- valid_idx[valid_idx > i & date[valid_idx] <= current_date %m+% months(12)]
          vals <- c(tail(count[before_idx], 3), head(count[after_idx], 3))
          if (length(vals) == 6) {
            result$count_working[i] <- mean(vals)
            result$adj_method[i] <- "roll6_center"
            next
          }
        }
        
        # Fallback: same month last year
        last_year_date <- current_date %m-% months(12)
        fallback_row <- which(date == last_year_date)
        if (length(fallback_row) == 1 &&
            isTRUE(outlier_flag[fallback_row] == 0) &&
            isTRUE(count[fallback_row] > 0)) {
          result$count_working[i] <- count[fallback_row]
          result$adj_method[i] <- "same_month_last_year"
          result$adjust_note[i] <- paste("used", format(last_year_date, "%b-%Y"))
          next
        }
        
        
        # Final fallback: unadjusted
        result$adj_method[i] <- "unadjusted"
        result$adjust_note[i] <- "no valid replacement"
      }
      result
    }, by = .(facility_id, indicator_common_id)]
    
    adj_debug <- data_adj[outlier_flag == 1, .N, by = adj_method][order(adj_method)]
    message("   -> Outlier adjustment method counts:")
    print(adj_debug)
  }
  
  # ---------------- Completeness Adjustment ----------------
  if (adjust_completeness) {
    message(" -> Adjusting missing data with 12-month rolling average (min 6 valid) + fallback mean...")
    
    # Step 1: Create valid values mask (much faster than repeated filtering in function)
    data_adj[, valid_count := fifelse(is.na(count_working) | count_working <= 0, NA_real_, count_working)]
    
    # Step 2: Use data.table's fast rolling mean instead of zoo's rollapplyr
    data_adj[, rolling_avg_temp := frollmean(valid_count, 12, na.rm = TRUE, align = "center"), 
             by = .(facility_id, indicator_common_id)]
    
    # Step 3: Count valid values in each 12-month window to enforce minimum 6 requirement
    data_adj[, valid_count_in_window := frollapply(valid_count, 12, 
                                                   function(x) sum(!is.na(x)), 
                                                   align = "center"), 
             by = .(facility_id, indicator_common_id)]
    
    # Step 4: Only keep rolling average if we have at least 6 valid values
    data_adj[, rolling_avg_completeness := fifelse(valid_count_in_window >= 6, 
                                                   rolling_avg_temp, 
                                                   NA_real_)]
    
    # Step 5: Calculate fallback mean (unchanged - already efficient)
    data_adj[, fallback_mean := mean(count_working[!is.na(count_working) & count_working > 0], na.rm = TRUE),
             by = .(facility_id, indicator_common_id)]
    
    # Step 6: Apply adjustments in order of preference
    data_adj[is.na(count_working) & !is.na(rolling_avg_completeness), count_working := rolling_avg_completeness]
    data_adj[is.na(count_working) & is.na(rolling_avg_completeness) & !is.na(fallback_mean), count_working := fallback_mean]
    
    # Step 7: Clean up temporary columns to save memory
    data_adj[, c("valid_count", "rolling_avg_temp", "valid_count_in_window") := NULL]
  }
  
  return(data_adj)
}


# Function to Apply Adjustments Across Scenarios ------------------------------------------------------------
apply_adjustments_scenarios <- function(raw_data, completeness_data, outlier_data) {
  cat("Applying adjustments across scenarios...\n")
  
  # Ensure all inputs are data.tables
  setDT(raw_data)
  setDT(completeness_data)
  setDT(outlier_data)
  
  join_cols <- c("facility_id", "indicator_common_id", "period_id")
  
  scenarios <- list(
    none = list(adjust_outliers = FALSE, adjust_completeness = FALSE),
    outliers = list(adjust_outliers = TRUE, adjust_completeness = FALSE),
    completeness = list(adjust_outliers = FALSE, adjust_completeness = TRUE),
    both = list(adjust_outliers = TRUE, adjust_completeness = TRUE)
  )
  
  results <- list()
  
  for (name in names(scenarios)) {
    cat("Processing scenario:", name, "\n")
    adjustment <- scenarios[[name]]
    
    data_adjusted <- apply_adjustments(
      raw_data = raw_data,
      completeness_data = completeness_data,
      outlier_data = outlier_data,
      adjust_outliers = adjustment$adjust_outliers,
      adjust_completeness = adjustment$adjust_completeness
    )
    
    # Keep only needed columns
    data_adjusted <- data_adjusted[, .(facility_id, indicator_common_id, period_id, count, count_working)]
    
    # Apply exclusion logic
    data_adjusted[indicator_common_id %in% EXCLUDED_FROM_ADJUSTMENT, count_working := count]
    
    # Report excluded indicators
    excluded_inds <- unique(data_adjusted[indicator_common_id %in% EXCLUDED_FROM_ADJUSTMENT, indicator_common_id])
    if (length(excluded_inds) > 0) {
      cat(" -> Indicators excluded from adjustment in scenario '", name, "':\n  ", paste(excluded_inds, collapse = ", "), "\n", sep = "")
    }
    
    
    # Rename working count column
    setnames(data_adjusted, "count_working", paste0("count_final_", name))
    
    # Drop raw count column
    data_adjusted[, count := NULL]
    
    results[[name]] <- data_adjusted
  }
  
  # Merge all scenario outputs
  df_adjusted <- Reduce(function(x, y) merge(x, y, by = join_cols, all = TRUE), results)
  
  return(df_adjusted)
}

# ------------------- Main Execution ------------------------------------------------------------------------

print("Running adjustments analysis...")

# Apply adjustment scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  raw_data = raw_data,
  completeness_data = completeness_data,
  outlier_data = outlier_data
)

# Create metadata lookups
geo_lookup <- raw_data %>%
  dplyr::select(facility_id, dplyr::starts_with("admin_area_")) %>%
  dplyr::distinct()

period_lookup <- completeness_data %>%
  dplyr::distinct(period_id, quarter_id, year)

# Merge metadata into facility-level adjusted data
setDT(adjusted_data_final)
setDT(geo_lookup)
setDT(period_lookup)
adjusted_data_export <- merge(merge(adjusted_data_final, geo_lookup, by = "facility_id"), 
                              period_lookup, by = "period_id")

# Detect admin area columns
geo_cols <- grep("^admin_area_[0-9]+$", names(adjusted_data_export), value = TRUE)
geo_admin_area_sub <- setdiff(geo_cols, "admin_area_1")

# Re-order columns 
adjusted_data_export <- adjusted_data_export %>%
  dplyr::select(
    facility_id,
    dplyr::all_of(geo_admin_area_sub),
    period_id, 
    quarter_id, 
    year, 
    indicator_common_id,
    dplyr::everything()
  )

message("Detected admin area columns: ", paste(geo_cols, collapse = ", "))
message("Using for subnational aggregation: ", paste(geo_admin_area_sub, collapse = ", "))

# Subnational admin area output
adjusted_data_admin_area_final <- adjusted_data_export %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(geo_admin_area_sub)), indicator_common_id, period_id) %>%
  dplyr::summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(period_lookup, by = "period_id") %>%
  dplyr::select(
    dplyr::all_of(geo_admin_area_sub),
    period_id, 
    quarter_id, 
    year, 
    indicator_common_id,
    dplyr::everything()
  )

# National-level output (admin_area_1 only)
adjusted_data_national_final <- adjusted_data_export %>%
  dplyr::group_by(admin_area_1, indicator_common_id, period_id) %>%
  dplyr::summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(period_lookup, by = "period_id") %>%
  dplyr::select(
    admin_area_1, 
    period_id, 
    quarter_id, 
    year, 
    indicator_common_id,
    dplyr::everything()
  )

# Clean facility-level output before saving (drop admin_area_1 to match schema expectations)
adjusted_data_export_clean <- adjusted_data_export %>%
  dplyr::select(-admin_area_1)

# Save all outputs
write.csv(adjusted_data_export_clean,      "M2_adjusted_data.csv",              row.names = FALSE)
write.csv(adjusted_data_admin_area_final,  "M2_adjusted_data_admin_area.csv",   row.names = FALSE)
write.csv(adjusted_data_national_final,    "M2_adjusted_data_national.csv",     row.names = FALSE)

print("Adjustments completed and all outputs saved.")

# ------------------- Generate SQL CREATE TABLE Statements for M2 Outputs ------------------------

# Function to generate CREATE TABLE SQL
generate_sql_schema <- function(table_name, columns) {
  sql_lines <- paste0("  ", columns)
  sql <- c(
    paste0("CREATE TABLE ", table_name, " ("),
    paste(sql_lines, collapse = ",\n"),
    ");\n"
  )
  return(paste(sql, collapse = "\n"))
}

# Get SQL-ready admin area columns
geo_columns_export_sql <- paste0(geo_admin_area_sub, " TEXT NOT NULL")
geo_columns_national_sql <- "admin_area_1 TEXT NOT NULL"

# Facility-level SQL columns
adjusted_facility_sql_cols <- c(
  "facility_id TEXT NOT NULL",
  geo_columns_export_sql,
  "period_id INTEGER NOT NULL",
  "quarter_id INTEGER NOT NULL",
  "year INTEGER NOT NULL",
  "indicator_common_id TEXT NOT NULL",
  "count_final_none NUMERIC",
  "count_final_outliers NUMERIC",
  "count_final_completeness NUMERIC",
  "count_final_both NUMERIC"
)

# Subnational-level SQL columns
adjusted_admin_sql_cols <- c(
  geo_columns_export_sql,
  "period_id INTEGER NOT NULL",
  "quarter_id INTEGER NOT NULL",
  "year INTEGER NOT NULL",
  "indicator_common_id TEXT NOT NULL",
  "count_final_none NUMERIC",
  "count_final_outliers NUMERIC",
  "count_final_completeness NUMERIC",
  "count_final_both NUMERIC"
)

# National-level SQL columns
adjusted_national_sql_cols <- c(
  geo_columns_national_sql,
  "period_id INTEGER NOT NULL",
  "quarter_id INTEGER NOT NULL",
  "year INTEGER NOT NULL",
  "indicator_common_id TEXT NOT NULL",
  "count_final_none NUMERIC",
  "count_final_outliers NUMERIC",
  "count_final_completeness NUMERIC",
  "count_final_both NUMERIC"
)

# Build SQL schema
sql_output <- c(
  generate_sql_schema("m2_adjusted_data", adjusted_facility_sql_cols),
  generate_sql_schema("m2_adjusted_data_admin_area", adjusted_admin_sql_cols),
  generate_sql_schema("m2_adjusted_data_national", adjusted_national_sql_cols)
)

# Write SQL schema to file
writeLines(sql_output, "M2_sql_schema_output.txt")
