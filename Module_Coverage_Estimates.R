#PLACEHOLDER FOR PARAMETERS - UI


# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 29
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
library(haven)      # For reading Stata .dta files -- !!!!! REMOVE THIS AND ALL DATA LOADING once the data load in platform
library(zoo)        # For carryforward functionality
library(stringr)    # For string manipulation

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- read_csv("M2_adjusted_data.csv")
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


# PART 6 - Calculate HMIS AND WPP-derived-denominators -------------------------------
# Function to calculate denominators for ANC1
calculate_anc1_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      danc1_pregnancy = if_else(
        indicator_common_id == "anc1" & !is.na(countanc1) & !is.na(micsanc1_carry),
        countanc1 / (micsanc1_carry / 100),
        NA_real_
      ),
      danc1_livebirth = if_else(
        indicator_common_id == "anc1" & !is.na(countanc1) & !is.na(anc1carry),
        (countanc1 / (anc1carry / 100)) * (1 - adjustment_factors$preg_loss) * 
          (1 + adjustment_factors$twin_rate) * (1 - adjustment_factors$stillbirth),
        NA_real_
      ),
      danc1_dpt = if_else(
        indicator_common_id == "anc1" & !is.na(countanc1) & !is.na(anc1carry),
        (countanc1 / (anc1carry / 100)) * (1 - adjustment_factors$preg_loss) /
          (1 - (adjustment_factors$twin_rate / 2)) * (1 - adjustment_factors$stillbirth) * 
          (1 - adjustment_factors$nmr),
        NA_real_
      ),
      danc1_mcv = if_else(
        indicator_common_id == "anc1" & !is.na(countanc1) & !is.na(anc1carry),
        (countanc1 / (anc1carry / 100)) * (1 - adjustment_factors$preg_loss) *
          (1 + adjustment_factors$twin_rate) * (1 - adjustment_factors$stillbirth) * 
          (1 - adjustment_factors$imr),
        NA_real_
      )
    )
  return(data)
}

# Function to calculate denominators for Delivery
calculate_delivery_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      ddelivery_pregnancy = if_else(
        indicator_common_id == "delivery" & !is.na(countdelivery) & !is.na(deliverycarry),
        (countdelivery / (deliverycarry / 100)) / (1 - adjustment_factors$preg_loss),
        NA_real_
      ),
      ddelivery_livebirth = if_else(
        indicator_common_id == "delivery" & !is.na(countdelivery) & !is.na(deliverycarry),
        (countdelivery / (deliverycarry / 100)) * (1 + adjustment_factors$twin_rate) * 
          (1 - adjustment_factors$stillbirth),
        NA_real_
      ),
      ddelivery_dpt = if_else(
        indicator_common_id == "delivery" & !is.na(countdelivery) & !is.na(deliverycarry),
        (countdelivery / (deliverycarry / 100)) * (1 + adjustment_factors$twin_rate) * 
          (1 - adjustment_factors$stillbirth) * (1 - adjustment_factors$nmr),
        NA_real_
      ),
      ddelivery_mcv = if_else(
        indicator_common_id == "delivery" & !is.na(countdelivery) & !is.na(deliverycarry),
        (countdelivery / (deliverycarry / 100)) * (1 + adjustment_factors$twin_rate) * 
          (1 - adjustment_factors$stillbirth) * (1 - adjustment_factors$imr),
        NA_real_
      )
    )
  return(data)
}

# Function to calculate denominators for BCG
calculate_bcg_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      dbcg_pregnancy = if_else(
        indicator_common_id == "bcg" & !is.na(countbcg) & !is.na(bcgcarry),
        (countbcg / (bcgcarry / 100)) / (1 - adjustment_factors$preg_loss) /
          (1 + adjustment_factors$twin_rate) / (1 - adjustment_factors$stillbirth),
        NA_real_
      ),
      dbcg_livebirth = if_else(
        indicator_common_id == "bcg" & !is.na(countbcg) & !is.na(bcgcarry),
        (countbcg / (bcgcarry / 100)),
        NA_real_
      ),
      dbcg_dpt = if_else(
        indicator_common_id == "bcg" & !is.na(countbcg) & !is.na(bcgcarry),
        (countbcg / (bcgcarry / 100)) * (1 - adjustment_factors$nmr),
        NA_real_
      ),
      dbcg_mcv = if_else(
        indicator_common_id == "bcg" & !is.na(countbcg) & !is.na(bcgcarry),
        (countbcg / (bcgcarry / 100)) * (1 - adjustment_factors$nmr) * 
          (1 - adjustment_factors$pnmr),
        NA_real_
      )
    )
  return(data)
}

# Function to calculate denominators for Penta1
calculate_penta1_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      dpenta1_pregnancy = if_else(
        indicator_common_id == "penta1" & !is.na(countpenta1) & !is.na(penta1carry),
        (countpenta1 / (penta1carry / 100)) / (1 - adjustment_factors$preg_loss) /
          (1 + adjustment_factors$twin_rate) / (1 - adjustment_factors$stillbirth) / 
          (1 - adjustment_factors$nmr),
        NA_real_
      ),
      dpenta1_livebirth = if_else(
        indicator_common_id == "penta1" & !is.na(countpenta1) & !is.na(penta1carry),
        (countpenta1 / (penta1carry / 100)) / (1 + adjustment_factors$twin_rate) / 
          (1 - adjustment_factors$stillbirth) / (1 - adjustment_factors$nmr),
        NA_real_
      ),
      dpenta1_dpt = if_else(
        indicator_common_id == "penta1" & !is.na(countpenta1) & !is.na(penta1carry),
        (countpenta1 / (penta1carry / 100)),
        NA_real_
      ),
      dpenta1_mcv = if_else(
        indicator_common_id == "penta1" & !is.na(countpenta1) & !is.na(penta1carry),
        (countpenta1 / (penta1carry / 100)) * (1 - adjustment_factors$pnmr),
        NA_real_
      )
    )
  return(data)
}

# Function to calculate denominators for Penta3
# ??? no denominators from Penta3??

# Function to calculate denominators for WPP (World Population Prospects)
calculate_wpp_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      # dwpp_pregnancy: Estimated pregnancies based on crude birth rate and total population
      dwpp_pregnancy = if_else(
        !is.na(wppCBR) & !is.na(wpptotpop) & !is.na(nummonth) & nummonth > 0,
        (wppCBR / 1000) * wpptotpop / (1 + adjustment_factors$twin_rate) * (12 / nummonth),
        NA_real_
      ),
      
      # dwpp_livebirth: Adjusted for number of months reported
      dwpp_livebirth = if_else(
        !is.na(wpplivebirth) & !is.na(nummonth) & nummonth > 0,
        wpplivebirth * (12 / nummonth),
        NA_real_
      ),
      
      # dwpp_dpt: Estimated eligible infants for DPT1, adjusted for reporting months
      dwpp_dpt = if_else(
        !is.na(wpptotu1pop) & !is.na(nummonth) & nummonth > 0,
        wpptotu1pop * (12 / nummonth),
        NA_real_
      ),
      
      # dwpp_mcv: Estimated eligible infants for MCV, adjusted for reporting months
      dwpp_mcv = if_else(
        !is.na(wpptotu5pop) & !is.na(nummonth) & nummonth > 0,
        wpptotu5pop * (12 / nummonth),
        NA_real_
      )
    )
  
  return(data)
}

# Main function to call all of the above functions
calculate_all_denominators <- function(data, adjustment_factors) {
  data <- calculate_anc1_denominators(data, adjustment_factors)
  data <- calculate_delivery_denominators(data, adjustment_factors)
  data <- calculate_bcg_denominators(data, adjustment_factors)
  data <- calculate_penta1_denominators(data, adjustment_factors)
  data <- calculate_wpp_denominators(data, adjustment_factors)
  return(data)
}

# PART 7 - Calculate Coverage for Each Indicator Based on Denominators ---------------
calculate_coverage_dynamic <- function(data) {
  # Define indicators and denominator types
  indicators <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3")
  denominator_types <- c("pregnancy", "livebirth", "dpt", "mcv")
  
  # Ensure unique column names to prevent duplicate errors
  colnames(data) <- make.unique(colnames(data))
  
  # Create an empty tibble to store coverage results
  coverage_results <- tibble()
  
  # Loop through each indicator and denominator type
  for (indicator in indicators) {
    for (denominator in denominator_types) {
      # Construct column names dynamically
      denominator_col <- paste0("d", indicator, "_", denominator)
      count_col <- paste0("count", indicator)
      
      # Check if the denominator column exists
      if (denominator_col %in% colnames(data)) {
        # Compute coverage only where numerator and denominator are available
        coverage_subset <- data %>%
          filter(!is.na(.data[[count_col]]) & !is.na(.data[[denominator_col]]) & .data[[denominator_col]] > 0) %>%
          mutate(
            coverage = (.data[[count_col]] / .data[[denominator_col]]) * 100,
            indicator_common_id = indicator,
            denominator_type = denominator
          ) %>%
          select(admin_area_1, year, indicator_common_id, coverage, denominator_type)
        
        # Append the results to coverage_results
        coverage_results <- bind_rows(coverage_results, coverage_subset)
      }
    }
  }
  
  return(coverage_results)
}

# PART 8 - Select Best Denominator ---------------------------------------------------
select_best_denominator <- function(coverage_long, data) {
  # Add reference values directly without joining
  debug_denominator <- coverage_long %>%
    mutate(
      reference_value = case_when(
        indicator_common_id == "anc1" ~ data$anc1carry,
        indicator_common_id == "anc4" ~ data$anc4carry,
        indicator_common_id == "delivery" ~ data$deliverycarry,
        indicator_common_id == "penta1" ~ data$penta1carry,
        indicator_common_id == "penta3" ~ data$penta3carry,
        indicator_common_id == "bcg" ~ data$bcgcarry,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      squared_error = if_else(!is.na(reference_value), (coverage - reference_value)^2, NA_real_)
    )
  
  # Print out unique counts before filtering to debug duplicates
  print(debug_denominator %>%
          count(admin_area_1, year, indicator_common_id, denominator_type) %>%
          filter(n > 1))
  
  # Select the best denominator with the lowest squared error
  best_denominator <- debug_denominator %>%
    group_by(admin_area_1, year, indicator_common_id) %>%
    slice_min(order_by = squared_error, with_ties = FALSE) %>%
    select(admin_area_1, year, indicator_common_id, coverage, denominator_type, squared_error) %>%
    ungroup()
  
  return(best_denominator)
}



# ------------------------------ Main Execution -----------------------------------
# 1. Load and Map Adjusted Volumes
adjusted_volume <- map_adjusted_volumes(adjusted_volume_data)
hmis_countries <- unique(adjusted_volume$admin_area_1)        # Identify Relevant Countries from HMIS Data

# 2. Aggregate HMIS Data to Annual Level
annual_hmis <- adjusted_volume %>%
  group_by(admin_area_1, year, indicator_common_id) %>%  # Ensure indicator_common_id is kept
  summarise(
    # Summing the count columns while ignoring NA values
    across(starts_with("count"), ~ if_else(all(is.na(.)), NA_real_, sum(., na.rm = TRUE))),
    
    # Calculating the number of months that are not missing
    nummonth = sum(!is.na(month)),
    
    .groups = "drop"  # Drop the grouping
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
data <- calculate_all_denominators(data, adjustment_factors)

# # 9. Calculate Coverage for Each Indicator
coverage_long <- calculate_coverage_dynamic(data)

#rapid check 
coverage_long %>% filter(coverage_long$indicator_common_id == "penta1" & year == 2022)

# Select Best Denominator 
best_coverage <- select_best_denominator(coverage_long, data)
# extrapolate



