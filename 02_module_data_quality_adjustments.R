
PROJECT_DATA_HMIS <- "hmis_ghana.csv"

# CB - R code FASTR PROJECT
# Module: DATA QUALITY ADJUSTMENT
# Last edit: 2025 May 5

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
    
    data_adj[, rolling_avg_completeness := rollapplyr(
      count_working, 12,
      FUN = function(x) {
        valid <- x[!is.na(x) & x > 0]
        if (length(valid) >= 6) mean(valid) else NA_real_
      },
      fill = NA, partial = TRUE, align = "center"
    ), by = .(facility_id, indicator_common_id)]
    
    data_adj[, fallback_mean := mean(count_working[!is.na(count_working) & count_working > 0], na.rm = TRUE),
             by = .(facility_id, indicator_common_id)]
    
    data_adj[is.na(count_working) & !is.na(rolling_avg_completeness), count_working := rolling_avg_completeness]
    data_adj[is.na(count_working) & is.na(rolling_avg_completeness) & !is.na(fallback_mean), count_working := fallback_mean]
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

# Step 1: Apply adjustment scenarios
adjusted_data_final <- apply_adjustments_scenarios(
  raw_data = raw_data,
  completeness_data = completeness_data,
  outlier_data = outlier_data
)

# Step 2: Metadata lookups
geo_lookup <- raw_data %>%
  dplyr::distinct(facility_id, admin_area_1, admin_area_2, admin_area_3)

period_lookup <- raw_data %>%
  dplyr::distinct(period_id, quarter_id, year)

# Step 3: Facility-level output (with metadata)
adjusted_data_export <- adjusted_data_final %>%
  as.data.frame() %>%
  dplyr::left_join(geo_lookup, by = "facility_id") %>%
  dplyr::left_join(period_lookup, by = "period_id") %>%
  dplyr::select(
    facility_id, admin_area_3, admin_area_2, admin_area_1,
    period_id, quarter_id, year, indicator_common_id,
    dplyr::everything()
  )

# Step 4: Admin area (level 2 and 3) output
geo_cols <- grep("^admin_area_[0-9]+$", names(adjusted_data_export), value = TRUE)

adjusted_data_admin_area_final <- adjusted_data_export %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(geo_cols)), indicator_common_id, period_id) %>%
  dplyr::summarise(
    count_final_none = sum(count_final_none, na.rm = TRUE),
    count_final_outliers = sum(count_final_outliers, na.rm = TRUE),
    count_final_completeness = sum(count_final_completeness, na.rm = TRUE),
    count_final_both = sum(count_final_both, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(period_lookup, by = "period_id") %>%
  dplyr::select(
    dplyr::all_of(geo_cols),
    period_id, quarter_id, year, indicator_common_id,
    dplyr::everything()
  )

# Step 5: National-level output (admin_area_1 only)
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
    admin_area_1, period_id, quarter_id, year, indicator_common_id,
    dplyr::everything()
  )

# Step 6: Remove admin_area_1 before saving facility-level output
adjusted_data_export_clean <- adjusted_data_export %>%
  dplyr::select(-admin_area_1)

# Step 7: Save outputs
write.csv(adjusted_data_export_clean,        "M2_adjusted_data.csv",              row.names = FALSE)
write.csv(adjusted_data_admin_area_final,    "M2_adjusted_data_admin_area.csv",   row.names = FALSE)
write.csv(adjusted_data_national_final,      "M2_adjusted_data_national.csv",     row.names = FALSE)

print("Adjustments completed and saved.")


