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
  full_year_range <- seq(min_year, max_year) # Define complete year range
  
  survey_data %>%
    group_by(admin_area_1) %>%
    complete(year = full_year_range) %>% # Ensure full year coverage
    arrange(admin_area_1, year) %>%
    fill(starts_with(prefix), .direction = "downup") %>% # Fill forward and backward
    mutate(across(starts_with(prefix), ~ if_else(is.na(.x), 0.5, .x))) %>% # Assign fallback of 0.5 for missing values
    filter(year >= min_year) %>% # Explicitly filter years before 2000
    ungroup()
}


# PART 4 - Create Survey Averages and Datasource Mapping --------------------------
create_survey_averages <- function(data) {
  data <- data %>%
    mutate(
      # Create survey averages (note = DHS takes priority over MICS)
      avgsurvey_anc1 = coalesce(dhsanc1, micsanc1),
      avgsurvey_anc4 = coalesce(dhsanc4, micsanc4),
      avgsurvey_delivery = coalesce(dhsdelivery, micsdelivery),
      avgsurvey_bcg = coalesce(dhsbcg, micsbcg),
      avgsurvey_penta1 = coalesce(dhspenta1, micspenta1),
      avgsurvey_penta3 = coalesce(dhspenta3, micspenta3),
      avgsurvey_nmr = coalesce(dhsnmr, micsnmr),
      avgsurvey_imr = coalesce(dhsimr, micsimr),
      
      # Calculate postnatal mortality
      postnmr = avgsurvey_imr - avgsurvey_nmr,
      
      # Assign data source
      datasource_anc1 = if_else(!is.na(dhsanc1), "DHS", "MICS"),
      datasource_delivery = if_else(!is.na(dhsdelivery), "DHS", "MICS"),
      datasource_bcg = if_else(!is.na(dhsbcg), "DHS", "MICS"),
      datasource_penta1 = if_else(!is.na(dhspenta1), "DHS", "MICS"),
      datasource_penta3 = if_else(!is.na(dhspenta3), "DHS", "MICS")
    )
  return(data)
}

# PART 5.A - Carry Forward Survey Data ---------------------------------------------
carry_forward_survey_data <- function(data, survey_vars) {
  existing_vars <- intersect(survey_vars, colnames(data))
  if (length(existing_vars) == 0) {
    warning("No survey variables found to carry forward.")
    return(data)
  }
  
  data %>%
    arrange(admin_area_1, year) %>%
    group_by(admin_area_1) %>%
    mutate(across(
      all_of(existing_vars),
      ~ zoo::na.locf(.x, na.rm = FALSE), 
      .names = "{sub('^avgsurvey_', '', .col)}carry"
    )) %>%
    mutate(across(all_of(existing_vars), ~ if_else(is.na(.x), 0.5, .x))) %>%
    ungroup()
}


# PART 5.B - Create Datasource Columns ---------------------------------------------
create_datasource_columns <- function(data) {
  data %>%
    mutate(
      datasource_anc1 = if_else(!is.na(dhsanc1), "DHS", if_else(!is.na(micsanc1), "MICS", NA_character_)),
      datasource_anc4 = if_else(!is.na(dhsanc4), "DHS", if_else(!is.na(micsanc4), "MICS", NA_character_)),
      datasource_delivery = if_else(!is.na(dhsdelivery), "DHS", if_else(!is.na(micsdelivery), "MICS", NA_character_)),
      datasource_bcg = if_else(!is.na(dhsbcg), "DHS", if_else(!is.na(micsbcg), "MICS", NA_character_)),
      datasource_penta1 = if_else(!is.na(dhspenta1), "DHS", if_else(!is.na(micspenta1), "MICS", NA_character_)),
      datasource_penta3 = if_else(!is.na(dhspenta3), "DHS", if_else(!is.na(micspenta3), "MICS", NA_character_)),
      datasource_nmr = if_else(!is.na(dhsnmr), "DHS", if_else(!is.na(micsnmr), "MICS", "Calculated")),
      datasource_imr = if_else(!is.na(dhsimr), "DHS", if_else(!is.na(micsimr), "MICS", "Calculated"))
    )
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

# 2. Adjust Names for Consistency
name_replacements <- c("Guinea" = "Guinée", "Sierra Leone" = "SierraLeone")
wpp_data <- read_dta(wpp_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements)
mics_data <- read_dta(mics_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements)
dhs_data <- read_dta(dhs_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements)

# 3. Extend Survey Data
mics_data_extended <- extend_survey_data(mics_data, prefix = "mics")
dhs_data_extended <- extend_survey_data(dhs_data, prefix = "dhs")

# 4. Merge and Process Data
data <- adjusted_volume %>%
  left_join(mics_data_extended, by = c("admin_area_1", "year")) %>%
  left_join(dhs_data_extended, by = c("admin_area_1", "year")) %>%
  left_join(wpp_data, by = c("admin_area_1", "year")) %>%
  create_survey_averages() %>%
  carry_forward_survey_data(survey_vars)

# 5. Map Data Sources
data <- create_datasource_columns(data)

# 6. Calculate Denominators
data <- calculate_hmis_denominators(data, adjustment_factors)
data <- calculate_wpp_denominators(data, adjustment_factors)

