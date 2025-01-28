#PLACEHOLDER FOR PARAMETERS - UI


#------------------------------------------------------------------------------
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
  data %>%
    mutate(
      !!sym(column) := recode(!!sym(column), !!!replacements)
    )
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
  # Dynamically detect available indicators
  available_indicators <- intersect(c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr"), 
                                    colnames(data) %>% str_remove("^count"))
  
  # Check for missing required carry columns
  carry_columns <- paste0(available_indicators, "carry")
  missing_carry <- setdiff(carry_columns, colnames(data))
  if (length(missing_carry) > 0) {
    warning("The following carry columns are missing: ", paste(missing_carry, collapse = ", "))
  }
  
  # Calculate denominators dynamically based on available indicators
  for (indicator in available_indicators) {
    count_col <- paste0("count", indicator)
    carry_col <- paste0(indicator, "carry")
    denom_col_base <- paste0("d", indicator)
    
    # Generate specific denominators for each indicator
    if (indicator == "anc1") {
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$preg_loss) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$preg_loss) /
              (1 - (adjustment_factors$twin_rate / 2)) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0("dmcv_", indicator) := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$preg_loss) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$imr),
            NA_real_
          )
        )
    } else if (indicator == "delivery") {
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss),
            NA_real_
          ),
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 + adjustment_factors$twin_rate) *
              (1 - adjustment_factors$stillbirth) *
              (1 - adjustment_factors$imr),
            NA_real_
          )
        )
    } else if (indicator == "bcg") {
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth),
            NA_real_
          ),
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$nmr) *
              (1 - adjustment_factors$pnmr),
            NA_real_
          )
        )
    } else if (indicator == "penta1") {
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth) /
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_livebirth") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth) /
              (1 - adjustment_factors$nmr),
            NA_real_
          ),
          !!paste0(denom_col_base, "_dpt") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)),
            NA_real_
          ),
          !!paste0(denom_col_base, "_mcv") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) *
              (1 - adjustment_factors$pnmr),
            NA_real_
          )
        )
    } else if (indicator == "penta3") {
      data <- data %>%
        mutate(
          !!paste0(denom_col_base, "_pregnancy") := if_else(
            !is.na(.data[[count_col]]),
            (.data[[count_col]] / (.data[[carry_col]] / 100)) /
              (1 - adjustment_factors$preg_loss) /
              (1 + adjustment_factors$twin_rate) /
              (1 - adjustment_factors$stillbirth) /
              (1 - adjustment_factors$nmr),
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

# 4. Apply filtering to ensure only relevant countries are included
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


# 3. Extend Survey Data
mics_data_extended <- extend_survey_data(mics_data_filtered, prefix = "mics")
dhs_data_extended <- extend_survey_data(dhs_data_filtered, prefix = "dhs")
wpp_data_extended <- extend_survey_data(wpp_data_filtered, prefix = "wpp")

# 4. Merge and Process Data
data <- annual_hmis %>%
  full_join(mics_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(dhs_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(wpp_data_extended, by = c("admin_area_1", "year")) %>%
  filter(year >= 2000 & year <= 2025) %>%  # Restrict to the desired year range
  create_survey_averages() %>%
  carry_forward_survey_data(survey_vars)

# Combine carry variables into unified columns
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


# 6. Calculate Denominators
data <- calculate_hmis_denominators(data, adjustment_factors)
data <- calculate_wpp_denominators(data, adjustment_factors)

