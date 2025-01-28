#PLACEHOLDER FOR PARAMETERS - UI


# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 27
# Module: Coverage Estimates

# Description:
# This script replicates the Stata Coverage Estimates module (National). It processes the
# annual volume data (after outlier adjustment), applies population projections, integrates survey data
# (MICS/DHS), calculates coverage estimates, selects optimal denominators based
# on minimal error compared to surveys, and exports the results for each country.


# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE(S): ....


# ------------------------------ Load Required Libraries -------------------------
library(tidyverse)  # For data manipulation and visualization
library(haven)      # For reading Stata .dta files -- remove once the data load in platform
library(zoo)        # For carryforward functionality
library(stringr)    # For string manipulation

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- adjusted_data_final  # Using data directly from the environment
wpp_data_path        <- "~/Desktop/FASTR/Coverage_Analysis/UNWPP/WPP.dta"
mics_data_path       <- "~/Desktop/FASTR/Coverage_Analysis/MICS/MICS.dta"
dhs_data_path        <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS.dta"

# ------------------------------ Define Parameters --------------------------------
# Denominator Adjustment Factors
adjustment_factors <- list(
  preg_loss = 0.03,        # Default pregnancy loss
  twin_rate = 0.015,       # Default twin rate
  stillbirth = 0.02,       # Default stillbirth rate
  nmr = 0.03,              # Default neonatal mortality rate
  pnmr = 0.02,             # Default postnatal mortality rate
  imr = 0.05               # Default infant mortality rate
)

# Coverage Estimation Parameters
coverage_params <- list(
  indicators = c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")
)

# List of survey variables to carry forward
survey_vars <- c(
  "avgsurvey_anc1", "avgsurvey_anc4", "avgsurvey_delivery",
  "avgsurvey_bcg", "avgsurvey_penta1", "avgsurvey_penta3",
  "postnmr", "avgsurvey_imr", "avgsurvey_nmr", "micsstill"
)

# ------------------------------ Define Functions --------------------------------
# PART 1 - Adjust Names for Merging ----------------------------------------------
adjust_names_for_merging <- function(data, column, replacements) {
  data[[column]] <- recode(data[[column]], !!!replacements)
  return(data)
}

# PART 2 - Map Adjusted Volumes to Indicators -------------------------------------
map_adjusted_volumes <- function(data) {
  expected_indicators <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")
  
  # Add missing indicators with NA values if they do not exist
  missing_indicators <- setdiff(expected_indicators, unique(data$indicator_common_id))
  if (length(missing_indicators) > 0) {
    warning("The following indicators are missing from the dataset: ", 
            paste(missing_indicators, collapse = ", "))
    for (indicator in missing_indicators) {
      data <- data %>%
        mutate(!!paste0("count", indicator) := NA_real_)
    }
  }
  
  # Map the existing indicators
  data <- data %>%
    mutate(
      countanc1 = if_else(indicator_common_id == "anc1", count_final_outliers, NA_real_),
      countanc4 = if_else(indicator_common_id == "anc4", count_final_outliers, NA_real_),
      countdelivery = if_else(indicator_common_id == "delivery", count_final_outliers, NA_real_),
      countbcg = if_else(indicator_common_id == "bcg", count_final_outliers, NA_real_),
      countpenta1 = if_else(indicator_common_id == "penta1", count_final_outliers, NA_real_),
      countpenta3 = if_else(indicator_common_id == "penta3", count_final_outliers, NA_real_),
      countnmr = if_else(indicator_common_id == "nmr", count_final_outliers, NA_real_),
      countimr = if_else(indicator_common_id == "imr", count_final_outliers, NA_real_)
    )
  
  return(data)
}


# PART 3 - Extend Survey Data for Missing Years -----------------------------------
extend_survey_data <- function(survey_data, min_year = 2000, max_year = 2025, prefix) {
  survey_data <- survey_data %>%
    filter(year >= min_year & year <= max_year)  # Ensure year range is valid
  full_year_range <- seq(min_year, max_year)
  
  survey_data %>%
    group_by(admin_area_1) %>%
    complete(year = full_year_range) %>%
    arrange(admin_area_1, year) %>%
    mutate(
      across(
        starts_with(prefix),
        ~ zoo::na.locf(.x, na.rm = FALSE),  # Forward fill
        .names = "{.col}_carry"
      )
    ) %>%
    ungroup()
}


# PART 4 - Create Survey Averages and Datasource Mapping --------------------------
create_survey_averages <- function(data) {
  data <- data %>%
    mutate(
      # DHS prioritized over MICS
      avgsurvey_anc1 = coalesce(dhsanc1, micsanc1),
      avgsurvey_anc4 = coalesce(dhsanc4, micsanc4),
      avgsurvey_delivery = coalesce(dhsdelivery, micsdelivery),
      avgsurvey_bcg = coalesce(dhsbcg, micsbcg),
      avgsurvey_penta1 = coalesce(dhspenta1, micspenta1),
      avgsurvey_penta3 = coalesce(dhspenta3, micspenta3),
      avgsurvey_nmr = coalesce(dhsnmr, micsnmr),
      avgsurvey_imr = coalesce(dhsimr, micsimr),
      
      # Postnatal mortality rate
      postnmr = avgsurvey_imr - avgsurvey_nmr,
      
      # Data source tracking
      datasource_anc1 = if_else(!is.na(dhsanc1), "DHS", "MICS"),
      datasource_delivery = if_else(!is.na(dhsdelivery), "DHS", "MICS"),
      datasource_bcg = if_else(!is.na(dhsbcg), "DHS", "MICS"),
      datasource_penta1 = if_else(!is.na(dhspenta1), "DHS", "MICS"),
      datasource_penta3 = if_else(!is.na(dhspenta3), "DHS", "MICS")
    )
  return(data)
}

# PART 5 - Carry Forward Survey Data ----------------------------------------------
carry_forward_survey_data <- function(data, survey_vars) {
  # Ensure the function processes only existing survey variables
  existing_vars <- intersect(survey_vars, colnames(data))
  
  if (length(existing_vars) == 0) {
    warning("No survey variables found to carry forward.")
    return(data)
  }
  
  data %>%
    arrange(admin_area_1, year) %>%  # Ensure data is sorted for carryforward
    group_by(admin_area_1) %>%      # Group by admin area for proper carryforward
    mutate(
      across(
        all_of(existing_vars),
        ~ zoo::na.locf(.x, na.rm = FALSE),  # Forward fill missing values
        .names = "{.col}_carry"
      ),
      across(
        all_of(existing_vars),
        ~ zoo::na.locf(.x, na.rm = FALSE, fromLast = TRUE),  # Backward fill if needed
        .names = "{.col}_carry"
      )
    ) %>%
    mutate(
      # Assign fallback of 0.5 if still missing after carryforward
      across(
        ends_with("_carry"),
        ~ if_else(is.na(.x), 0.5, .x)
      )
    ) %>%
    ungroup()
}


# PART 6.1 - Calculate HMIS derived-denominators ------------------------------------------------
calculate_hmis_denominators <- function(data, adjustment_factors) {
  for (indicator in c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3")) {
    count_col <- paste0("count", indicator)
    carry_col <- paste0(indicator, "carry")
    denom_col_base <- paste0("d", indicator)
    
    # Skip if the required columns are missing
    if (!count_col %in% colnames(data)) {
      message(paste("Missing count column for indicator:", indicator))
      next
    }
    if (!carry_col %in% colnames(data)) {
      message(paste("Missing carry column for indicator:", indicator))
      next
    }
    
    if (indicator == "anc1") {
      # ANC1 denominators
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$preg_loss) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$preg_loss) /
              (1 - (adjustment_factors$twin_rate / 2)) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$nmr),
            NA_real_
          )
        )
    } else if (indicator == "delivery") {
      # Delivery denominators
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$imr),
            NA_real_
          )
        )
    } else if (indicator == "bcg") {
      # BCG denominators
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$nmr) *
              (1 - adjustment_factors$pnmr),
            NA_real_
          )
        )
    } else if (indicator == "penta1") {
      # Penta1 denominators
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth) /
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth) /
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$pnmr),
            NA_real_
          )
        )
    } else if (indicator == "penta3") {
      # Penta3 denominators
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]) & !is.na(.data[[carry_col]]) & .data[[carry_col]] > 0,
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss),
            NA_real_
          )
        )
    }
  }
  
  return(data)
}

# PART 6.2 - Calculate WPP-derived denominators ------------------------------------
calculate_wpp_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      dwpp_pregnancy = if_else(!is.na(wppCBR) & !is.na(wpptotpop), 
                               (wppCBR / 1000) * wpptotpop / (1 + adjustment_factors$twin_rate), 
                               NA_real_),
      dwpp_dpt = wpptotu1pop,
      dwpp_mcv = wpptotu5pop
    )
  return(data)
}


# PART 7 - Calculate Coverage for Each Indicator Based on Denominators ---------------
calculate_coverage <- function(data) {
  data <- data %>%
    mutate(
      cov_anc1_livebirth = if_else(
        !is.na(countanc1) & !is.na(danc1_livebirth) & danc1_livebirth != 0, 
        (countanc1 / danc1_livebirth) * 100, 
        NA_real_
      ),
      cov_anc1_dpt = if_else(
        !is.na(countanc1) & !is.na(danc1_dpt) & danc1_dpt != 0,
        (countanc1 / danc1_dpt) * 100, 
        NA_real_
      ),
      cov_delivery_pregnancy = if_else(
        !is.na(countdelivery) & !is.na(ddelivery_pregnancy) & ddelivery_pregnancy != 0, 
        (countdelivery / ddelivery_pregnancy) * 100, 
        NA_real_
      ),
      cov_delivery_dpt = if_else(
        !is.na(countdelivery) & !is.na(ddelivery_dpt) & ddelivery_dpt != 0,
        (countdelivery / ddelivery_dpt) * 100, 
        NA_real_
      ),
      cov_bcg_pregnancy = if_else(
        !is.na(countbcg) & !is.na(dbcg_pregnancy) & dbcg_pregnancy != 0, 
        (countbcg / dbcg_pregnancy) * 100, 
        NA_real_
      ),
      cov_bcg_dpt = if_else(
        !is.na(countbcg) & !is.na(dbcg_dpt) & dbcg_dpt != 0, 
        (countbcg / dbcg_dpt) * 100, 
        NA_real_
      ),
      cov_penta1_pregnancy = if_else(
        !is.na(countpenta1) & !is.na(dpenta1_pregnancy) & dpenta1_pregnancy != 0, 
        (countpenta1 / dpenta1_pregnancy) * 100, 
        NA_real_
      ),
      cov_penta1_livebirth = if_else(
        !is.na(countpenta1) & !is.na(dpenta1_livebirth) & dpenta1_livebirth != 0, 
        (countpenta1 / dpenta1_livebirth) * 100, 
        NA_real_
      ),
      cov_penta3_pregnancy = if_else(
        !is.na(countpenta3) & !is.na(dpenta3_pregnancy) & dpenta3_pregnancy != 0, 
        (countpenta3 / dpenta3_pregnancy) * 100, 
        NA_real_
      )
    )
  return(data)
}



# PART 7.2 - Extrapolate Coverage Trends for Missing Years -----------------------------------
extrapolate_coverage_trends <- function(data) {
  data %>%
    group_by(admin_area_1) %>%
    arrange(year) %>%
    mutate(
      # Use lag for intermediate years, then fill missing at the start and end
      projected_cov_anc1 = zoo::na.locf(
        zoo::na.locf(
          if_else(is.na(cov_anc1_livebirth), lag(cov_anc1_livebirth), cov_anc1_livebirth), 
          na.rm = FALSE
        ), 
        fromLast = TRUE, na.rm = FALSE
      ),
      
      projected_cov_delivery = zoo::na.locf(
        zoo::na.locf(
          if_else(is.na(cov_delivery_pregnancy), lag(cov_delivery_pregnancy), cov_delivery_pregnancy), 
          na.rm = FALSE
        ), 
        fromLast = TRUE, na.rm = FALSE
      ),
      projected_cov_bcg = zoo::na.locf(
        zoo::na.locf(
          if_else(is.na(cov_bcg_pregnancy), lag(cov_bcg_pregnancy), cov_bcg_pregnancy), 
          na.rm = FALSE
        ), 
        fromLast = TRUE, na.rm = FALSE
      ),
      projected_cov_penta1 = zoo::na.locf(
        zoo::na.locf(
          if_else(is.na(cov_penta1_pregnancy), lag(cov_penta1_pregnancy), cov_penta1_pregnancy), 
          na.rm = FALSE
        ), 
        fromLast = TRUE, na.rm = FALSE
      ),
      projected_cov_penta3 = zoo::na.locf(
        zoo::na.locf(
          if_else(is.na(cov_penta3_pregnancy), lag(cov_penta3_pregnancy), cov_penta3_pregnancy), 
          na.rm = FALSE
        ), 
        fromLast = TRUE, na.rm = FALSE
      )
    ) %>%
    ungroup()
}

# PART 8 - Combine HMIS-Based and Survey-Based Coverage --------------------------------------
combine_coverage_sources <- function(data) {
  data %>%
    mutate(
      final_cov_anc1 = coalesce(cov_anc1_livebirth, anc1carry, projected_cov_anc1),
      final_cov_delivery = coalesce(cov_delivery_pregnancy, deliverycarry, projected_cov_delivery),
      final_cov_bcg = coalesce(cov_bcg_pregnancy, bcgcarry, projected_cov_bcg),
      final_cov_penta1 = coalesce(cov_penta1_pregnancy, penta1carry, projected_cov_penta1),
      final_cov_penta3 = coalesce(cov_penta3_pregnancy, penta3carry, projected_cov_penta3)
    )
}

# PART 9 - Select Best Denominator for Each Indicator -------------------------------------------------
select_best_denominator <- function(data, indicators) {
  for (indicator in indicators) {
    # Define coverage and error columns for each indicator
    coverage_cols <- grep(paste0("^cov_", indicator), colnames(data), value = TRUE)
    if (length(coverage_cols) == 0) {
      print(paste("No coverage columns found for indicator:", indicator))
      next
    }
    
    # Create error columns for coverage estimates
    for (coverage_col in coverage_cols) {
      error_col <- paste0("error_", gsub("cov_", "", coverage_col))
      data <- data %>%
        mutate(
          !!error_col := if_else(
            !is.na(.data[[coverage_col]]) & !is.na(.data[[paste0(indicator, "carry")]]),
            (.data[[coverage_col]] - .data[[paste0(indicator, "carry")]])^2, 
            NA_real_
          )
        )
    }
    
    # Select the best denominator based on minimal error (squared error)
    error_cols <- grep(paste0("^error_", indicator), colnames(data), value = TRUE)
    if (length(error_cols) > 0) {
      data <- data %>%
        mutate(
          best_error = pmin(!!!syms(error_cols), na.rm = TRUE),
          best_source = case_when(
            !!sym(error_cols[1]) == best_error ~ coverage_cols[1],
            !!sym(error_cols[2]) == best_error ~ coverage_cols[2],
            !!sym(error_cols[3]) == best_error ~ coverage_cols[3],
            TRUE ~ NA_character_
          )
        )
    }
  }
  return(data)
}



# ------------------------------ Main Execution -----------------------------------
# 1. Load and Map Adjusted Volumes
adjusted_volume <- map_adjusted_volumes(adjusted_volume_data)
hmis_countries <- unique(adjusted_volume$admin_area_1)        # Identify Relevant Countries from HMIS Data

# 2. Aggregate HMIS Data to Annual Level
annual_hmis <- adjusted_volume %>%
  group_by(admin_area_1, year) %>%
  summarise(
    across(starts_with("count"), ~ if_else(all(is.na(.)), NA_real_, sum(., na.rm = TRUE))),  # Proper handling of NAs
    nummonth = sum(!is.na(month)),  # Only count non-missing months
    .groups = "drop"
  )

# 3. Adjust Names for Consistency
name_replacements <- c("Guinea" = "Guinée", "Sierra Leone" = "SierraLeone")

# 4. Apply Filtering to Ensure Only Relevant Countries are Included
mics_data_filtered <- read_dta(mics_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)  # Keep only HMIS countries

dhs_data_filtered <- read_dta(dhs_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)  # Keep only HMIS countries

wpp_data_filtered <- read_dta(wpp_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)  # Keep only HMIS countries

# 5. Extend Survey Data
mics_data_extended <- extend_survey_data(mics_data_filtered, prefix = "mics")
dhs_data_extended <- extend_survey_data(dhs_data_filtered, prefix = "dhs")
wpp_data_extended <- extend_survey_data(wpp_data_filtered, prefix = "wpp")

# 6. Merge and Process Data
data <- annual_hmis %>%
  full_join(mics_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(dhs_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(wpp_data_extended, by = c("admin_area_1", "year")) %>%
  filter(year >= 2000 & year <= 2025) %>%  # Restrict to the desired year range
  create_survey_averages() %>%
  carry_forward_survey_data(survey_vars)

# 7. Combine Carry Variables
data <- data %>%
  mutate(
    anc1carry = coalesce(dhsanc1_carry, micsanc1_carry),
    anc4carry = coalesce(dhsanc4_carry, micsanc4_carry),
    deliverycarry = coalesce(dhsdelivery_carry, micsdelivery_carry),
    bcgcarry = coalesce(dhsbcg_carry, micsbcg_carry),
    penta1carry = coalesce(dhspenta1_carry, micspenta1_carry),
    penta3carry = coalesce(dhspenta3_carry, micspenta3_carry),
    nmrcarry = coalesce(dhsnmr_carry, micsnmr_carry),
    imrcarry = coalesce(dhsimr_carry, micsimr_carry)
  )

# 8. Calculate Denominators
data <- calculate_hmis_denominators(data, adjustment_factors)
data <- calculate_wpp_denominators(data, adjustment_factors)

# 9. Calculate Coverage for Each Indicator
data <- calculate_coverage(data)

# 10. Extrapolate Coverage Trends for Missing Years
data <- extrapolate_coverage_trends(data)

# 11. Combine HMIS-Based and Survey-Based Coverage
data <- combine_coverage_sources(data)

# 12. Select the Best Denominator for Each Indicator
data <- select_best_denominator(data, indicators = c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3"))


