SELECTED_COUNT_VARIABLE <- "count_final_both"  # Options: "count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both"
VALID_COUNT_VARIABLES <- c("count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both")

CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))  # Dynamically get current year
MIN_YEAR <- 2000  # Set a fixed minimum year for filtering


library(haven)      # For reading Stata .dta files -- !!!!! REMOVE THIS AND ALL DATA LOADING once the data load in platform
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
library(zoo)        # For carryforward functionality
library(data.table) # For coverage calculations

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- read.csv("M2_adjusted_data.csv")
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

# List of survey variables to carry forward - as per stata 
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
  
  # Map the existing indicators dynamically using SELECTED_COUNT_VARIABLE
  data <- data %>%
    mutate(
      countanc1 = if_else(indicator_common_id == "anc1", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countanc4 = if_else(indicator_common_id == "anc4", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countdelivery = if_else(indicator_common_id == "delivery", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countbcg = if_else(indicator_common_id == "bcg", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countpenta1 = if_else(indicator_common_id == "penta1", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countpenta3 = if_else(indicator_common_id == "penta3", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countnmr = if_else(indicator_common_id == "nmr", !!sym(SELECTED_COUNT_VARIABLE), NA_real_),
      countimr = if_else(indicator_common_id == "imr", !!sym(SELECTED_COUNT_VARIABLE), NA_real_)
    )
  
  return(data)
}


# PART 3 - Extend Survey Data for Missing Years -----------------------------------
extend_survey_data <- function(survey_data, min_year = MIN_YEAR, max_year = CURRENT_YEAR, prefix) {
  survey_data <- survey_data %>%
    filter(year >= min_year & year <= max_year)  # Ensure year range is valid
  
  full_year_range <- seq(min_year, max_year)
  
  survey_data %>%
    group_by(admin_area_1) %>%
    complete(year = full_year_range) %>%
    arrange(admin_area_1, year) %>%
    mutate(
      across(
        matches(paste0("^avgsurvey_", prefix)),  # ONLY forward-fill avgsurvey_* variables
        ~ zoo::na.locf(.x, na.rm = FALSE),
        .names = "{.col}_carry"
      )
    ) %>%
    ungroup()
}


# PART 4 - Create Survey Averages and Datasource Mapping --------------------------
create_survey_averages <- function(data) {
  data <- data %>%
    mutate(
      # DHS prioritized over MICS (if DHS is available, use it; otherwise, use MICS)
      avgsurvey_anc1 = coalesce(dhsanc1, micsanc1), #coalesce works row-wise
      avgsurvey_anc4 = coalesce(dhsanc4, micsanc4),
      avgsurvey_delivery = coalesce(dhsdelivery, micsdelivery),
      avgsurvey_bcg = coalesce(dhsbcg, micsbcg),
      avgsurvey_penta1 = coalesce(dhspenta1, micspenta1),
      avgsurvey_penta3 = coalesce(dhspenta3, micspenta3),
      avgsurvey_nmr = coalesce(dhsnmr, micsnmr),
      avgsurvey_imr = coalesce(dhsimr, micsimr),
      
      # Postnatal mortality rate (IMR - NMR)
      postnmr = avgsurvey_imr - avgsurvey_nmr,
      
      # Map the data source directly based on which value was used
      datasource_anc1 = if_else(!is.na(dhsanc1) & dhsanc1 == avgsurvey_anc1, "DHS", 
                                if_else(!is.na(micsanc1) & micsanc1 == avgsurvey_anc1, "MICS", NA_character_)),
      
      datasource_anc4 = if_else(!is.na(dhsanc4) & dhsanc4 == avgsurvey_anc4, "DHS", 
                                if_else(!is.na(micsanc4) & micsanc4 == avgsurvey_anc4, "MICS", NA_character_)),
      
      datasource_delivery = if_else(!is.na(dhsdelivery) & dhsdelivery == avgsurvey_delivery, "DHS", 
                                    if_else(!is.na(micsdelivery) & micsdelivery == avgsurvey_delivery, "MICS", NA_character_)),
      
      datasource_bcg = if_else(!is.na(dhsbcg) & dhsbcg == avgsurvey_bcg, "DHS", 
                               if_else(!is.na(micsbcg) & micsbcg == avgsurvey_bcg, "MICS", NA_character_)),
      
      datasource_penta1 = if_else(!is.na(dhspenta1) & dhspenta1 == avgsurvey_penta1, "DHS", 
                                  if_else(!is.na(micspenta1) & micspenta1 == avgsurvey_penta1, "MICS", NA_character_)),
      
      datasource_penta3 = if_else(!is.na(dhspenta3) & dhspenta3 == avgsurvey_penta3, "DHS", 
                                  if_else(!is.na(micspenta3) & micspenta3 == avgsurvey_penta3, "MICS", NA_character_)),
      
      datasource_nmr = if_else(!is.na(dhsnmr) & dhsnmr == avgsurvey_nmr, "DHS", 
                               if_else(!is.na(micsnmr) & micsnmr == avgsurvey_nmr, "MICS", NA_character_)),
      
      datasource_imr = if_else(!is.na(dhsimr) & dhsimr == avgsurvey_imr, "DHS", 
                               if_else(!is.na(micsimr) & micsimr == avgsurvey_imr, "MICS", NA_character_))
    )
  
  return(data)
}

# PART 5.1 - Carry Forward Survey Data --------------------------------------------
carry_forward_survey_data <- function(data) {
  # Identify ONLY the expected survey variables
  survey_vars <- c(
    "avgsurvey_anc1", "avgsurvey_anc4", "avgsurvey_delivery",
    "avgsurvey_bcg", "avgsurvey_penta1", "avgsurvey_penta3",
    "avgsurvey_nmr", "avgsurvey_imr", "postnmr"
  )
  
  # Ensure we only process these variables
  existing_vars <- intersect(survey_vars, colnames(data))
  
  if (length(existing_vars) == 0) {
    warning("No survey variables found to carry forward.")
    return(data)
  }
  
  data %>%
    arrange(admin_area_1, year) %>%  # Ensure data is sorted correctly
    group_by(admin_area_1) %>%       # Group by admin area for correct carryforward
    mutate(
      across(
        all_of(existing_vars),  # Ensure only avgsurvey_* variables are carried forward
        ~ zoo::na.locf(.x, na.rm = FALSE)
      )
    ) %>%
    mutate(
      across(
        all_of(existing_vars),
        ~ if_else(is.na(.x), 0.5, .x)
      )
    ) %>%
    ungroup()
  
  return(data)
}

# PART 5.2 - Assign Carried Survey Data -------------------------------------------
assign_carried_survey_data <- function(data) {
  # Define the mapping of avgsurvey_* to _carry variables
  carry_vars <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")
  
  # Dynamically create carry variables
  for (var in carry_vars) {
    data <- data %>%
      mutate(!!paste0(var, "carry") := .data[[paste0("avgsurvey_", var)]])
  }
  
  return(data)
}

# PART 6 - Calculate HMIS AND WPP-derived-denominators -------------------------------
# Function to calculate denominators for ANC1
calculate_anc1_denominators <- function(data, adjustment_factors) {
  data <- data %>%
    mutate(
      danc1_pregnancy = if_else(
        indicator_common_id == "anc1" & !is.na(countanc1) & !is.na(anc1carry),
        countanc1 / (anc1carry / 100),
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
# ??? no denominators from Penta3 ??

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
calculate_coverage <- function(data) {
  # Step 1: Create the Denominator Table
  
  # Select valid denominator columns
  denominator_cols <- names(data)[str_detect(names(data), "^d.*_(pregnancy|livebirth|dpt)$")]
  
  # Pivot denominator columns into long format
  denominator_long <- data %>%
    select(admin_area_1, year, all_of(denominator_cols)) %>%
    pivot_longer(
      cols = all_of(denominator_cols),
      names_to = "denominator",
      values_to = "denominator_value"
    ) %>%
    drop_na(denominator_value) %>%
    distinct() %>%
    mutate(
      denominator_type = case_when(
        str_detect(denominator, "_pregnancy$") ~ "pregnancy",
        str_detect(denominator, "_livebirth$") ~ "livebirth",
        str_detect(denominator, "_dpt$") ~ "dpt",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      indicator_to_match_on = case_when(
        denominator_type == "pregnancy" ~ list(c("anc1", "anc4")),
        denominator_type == "livebirth" ~ list(c("delivery", "bcg")),
        denominator_type == "dpt" ~ list(c("penta1", "penta3")),
        TRUE ~ list(NA_character_)
      )
    ) %>%
    unnest(indicator_to_match_on)
  
  # Step 2: Create the Numerator Table
  
  # Select valid numerator columns (exclude "count" and "count_final")
  num_cols <- names(data)[str_detect(names(data), "^count(?!$|.*final)")]
  num_cols <- setdiff(num_cols, "count")
  
  # Pivot numerators into long format
  numerator_long <- data %>%
    select(admin_area_1, year, all_of(num_cols)) %>%
    pivot_longer(
      cols = all_of(num_cols),
      names_to = "numerator_col",
      values_to = "numerator"
    ) %>%
    mutate(
      indicator_common_id = str_replace(numerator_col, "^count", ""),
      numerator_col = NULL
    ) %>%
    drop_na(numerator)
  
  # Step 3: Join Numerator & Denominator
  
  # Ensure that numerator_long is expanded properly for each year-indicator combo
  expanded_numerator <- numerator_long %>%
    rename(indicator_to_match_on = indicator_common_id)
  
  # Full join on admin_area_1, year, and indicator_to_match_on
  coverage_data <- full_join(denominator_long, expanded_numerator, 
                             by = c("admin_area_1", "year", "indicator_to_match_on"))
  
  # Step 4: Calculate Coverage
  coverage_data <- coverage_data %>%
    mutate(
      coverage = case_when(
        denominator_type == "pregnancy" & indicator_to_match_on %in% c("anc1", "anc4") ~ (numerator / denominator_value) * 100,
        denominator_type == "livebirth" & indicator_to_match_on %in% c("delivery", "bcg") ~ (numerator / denominator_value) * 100,
        denominator_type == "dpt" & indicator_to_match_on %in% c("penta1", "penta3") ~ (numerator / denominator_value) * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    drop_na(coverage) #tbc
  
  return(coverage_data)
}


# PART 8 - Select Best Denominator ---------------------------------------------------
# Step 1: Extract reference values
extract_reference_values <- function(data) {
  # Find all "carry" columns
  carry_cols <- grep("carry$", names(data), value = TRUE)
  
  if (length(carry_cols) == 0) {
    stop("No '_carry' columns found in data! Check column names.")
  }
  
  # Pivot 'carry' columns into long format
  carry_values <- data %>%
    select(admin_area_1, year, all_of(carry_cols)) %>%
    pivot_longer(cols = all_of(carry_cols), names_to = "indicator_to_match_on", values_to = "reference_value") %>%
    mutate(indicator_to_match_on = gsub("carry$", "", indicator_to_match_on)) %>%
    drop_na(reference_value)
  
  return(carry_values)
}
# Step 2: Merge survey estimates
merge_survey_estimates <- function(coverage_long, carry_values) {
  # Expand carry_values to match all relevant indicator-year pairs
  expanded_carry_values <- coverage_long %>%
    select(admin_area_1, year, indicator_to_match_on) %>%
    distinct() %>%  # Ensure no duplicate rows
    left_join(carry_values, by = c("admin_area_1", "year", "indicator_to_match_on")) %>%
    drop_na(reference_value)  # Drop missing reference values
  
  # Ensure no duplicate reference values
  expanded_carry_values <- expanded_carry_values %>%
    distinct(admin_area_1, year, indicator_to_match_on, reference_value)
  
  # Now merge with coverage_long
  merged_data <- left_join(
    coverage_long, expanded_carry_values,
    by = c("admin_area_1", "year", "indicator_to_match_on")
  )
  
  # Compute Squared Error (Distance from Reference Survey Value)
  merged_data <- merged_data %>%
    mutate(
      squared_error = ifelse(
        !is.na(reference_value) & !is.na(coverage),
        (coverage - reference_value)^2,
        NA_real_
      )
    )
  
  return(merged_data)
}
# Step 3: Select best denominator
select_best_denominator <- function(merged_data) {
  best_denominator <- merged_data %>%
    filter(!is.na(squared_error)) %>%  # Remove rows where squared_error is NA
    group_by(admin_area_1, year, indicator_to_match_on) %>%
    slice_min(squared_error, with_ties = FALSE) %>%  # Select row with min squared_error
    ungroup() %>%
    rename(indicator_common_id = indicator_to_match_on) %>%  # Rename column
    select(admin_area_1, year, indicator_common_id, coverage, denominator, denominator_value, denominator_type, squared_error)
  
  return(best_denominator)
}



# ------------------------------ Main Execution -----------------------------------
# 1. Load and Map Adjusted Volumes
adjusted_volume <- map_adjusted_volumes(adjusted_volume_data)
hmis_countries <- unique(adjusted_volume$admin_area_1)        # Identify Relevant Countries from HMIS Data

# 2. Aggregate HMIS Data to Annual Level
annual_hmis <- adjusted_volume %>%
  group_by(admin_area_1, year, indicator_common_id) %>%  # Keep key identifiers
  summarise(
    # Sum valid count columns (excluding count_final_x)
    across(
      starts_with("count"), 
      ~ if_else(all(is.na(.)), NA_real_, sum(., na.rm = TRUE)), 
      .names = "{.col}"
    ),
    
    # Correctly count the number of unique non-missing months (is that for each indicator ?? or overall)
    nummonth = n_distinct(month[!is.na(month)]),  
    
    .groups = "drop"
  ) %>%
  select(-starts_with("count_final")) 

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


# 6. Merge All Data Sources (Post-Filtering)
data <- annual_hmis %>%
  full_join(mics_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(dhs_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(wpp_data_extended, by = c("admin_area_1", "year")) %>%
  filter(year >= MIN_YEAR & year <= CURRENT_YEAR) %>%
  create_survey_averages()

# 5. Apply Carry Forward Survey Data
data <- carry_forward_survey_data(data)

# 6. Assign Carried Survey Data 
data <- assign_carried_survey_data(data)

# 7. Calculate Denominators
data <- calculate_all_denominators(data, adjustment_factors)

# 8. Calculate Coverage for Each Indicator
coverage_long <- calculate_coverage(data)

# 9. Select Best Denominator (Choose the denominator with the smallest error compared to surveys)
carry_values <- extract_reference_values(data)
merged_coverage <- merge_survey_estimates(coverage_long, carry_values)
best_coverage <- select_best_denominator(merged_coverage)