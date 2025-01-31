SELECTED_COUNT_VARIABLE <- "count_final_both"  # Options: "count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both"
VALID_COUNT_VARIABLES <- c("count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both")

CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))  # Dynamically get current year
MIN_YEAR <- 2000  # Set a fixed minimum year for filtering

PREGNANCY_LOSS_RATE <- 0.03      
TWIN_RATE <- 0.015               
STILLBIRTH_RATE <- 0.02          
NEONATAL_MORTALITY_RATE <- 0.03  
POSTNEONATAL_MORTALITY_RATE <- 0.02  
INFANT_MORTALITY_RATE <- 0.05   



library(haven)      # For reading Stata .dta files -- !!!!! REMOVE THIS AND ALL DATA LOADING once the xtra datasets load in platform

# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 31
# Module: Coverage Estimates

# Description:
# This script replicates the Stata Coverage Estimates module (National). It processes the
# annual volume data (after outlier adjustment), applies population projections, integrates survey data
# (MICS/DHS), calculates coverage estimates, selects optimal denominators based
# on minimal error compared to surveys, and exports the results for each country.


# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE(S): M4_Coverage_estimation.csv #long format with source column to differentiate what type of coverate estimate it is


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
  
  data <- data %>%
    arrange(admin_area_1, year, indicator_common_id)
  
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
  
  data <- data %>%
    arrange(admin_area_1, year) %>%  # Ensure data is sorted correctly
    group_by(admin_area_1) %>%       # Group by admin area for correct carryforward
    complete(year = full_seq(year, 1)) %>% # Ensure all sequential years are present
    mutate(
      across(
        all_of(existing_vars),       # Ensure only avgsurvey_* variables are carried forward
        ~ zoo::na.locf(.x, na.rm = FALSE)
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
calculate_denominators <- function(data, adjustment_factors) {
  
  # Define indicator names and their required columns
  indicator_vars <- list(
    anc1 = c("countanc1", "anc1carry"),
    delivery = c("countdelivery", "deliverycarry"),
    bcg = c("countbcg", "bcgcarry"),
    penta1 = c("countpenta1", "penta1carry")
  )
  
  # Identify available columns in data
  available_vars <- names(data)
  
  # Safe mutate function to avoid crashes
  safe_mutate <- function(var_name, formula) {
    if (all(indicator_vars[[var_name]] %in% available_vars)) {
      return(formula)
    } else {
      return(NA_real_)
    }
  }
  
  # Apply calculations safely
  data <- data %>%
    mutate(
      # ANC1 Denominators
      danc1_pregnancy = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                                    countanc1 / (anc1carry / 100),
                                                    NA_real_)),
      
      danc1_livebirth = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                                    (countanc1 / (anc1carry / 100)) * 
                                                      (1 - PREGNANCY_LOSS_RATE) * 
                                                      (1 + TWIN_RATE) * 
                                                      (1 - STILLBIRTH_RATE),
                                                    NA_real_)),
      
      danc1_dpt = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                              (countanc1 / (anc1carry / 100)) * 
                                                (1 - PREGNANCY_LOSS_RATE) /
                                                (1 - (TWIN_RATE / 2)) * 
                                                (1 - STILLBIRTH_RATE) * 
                                                (1 - NEONATAL_MORTALITY_RATE),
                                              NA_real_)),
      
      danc1_mcv = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                              (countanc1 / (anc1carry / 100)) * 
                                                (1 - PREGNANCY_LOSS_RATE) *
                                                (1 + TWIN_RATE) * 
                                                (1 - STILLBIRTH_RATE) *
                                                (1 - INFANT_MORTALITY_RATE),
                                              NA_real_)),
      
      # Delivery Denominators
      ddelivery_pregnancy = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                            (countdelivery / (deliverycarry / 100)) / 
                                                              (1 - PREGNANCY_LOSS_RATE),
                                                            NA_real_)),
      
      ddelivery_livebirth = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                            (countdelivery / (deliverycarry / 100)) * 
                                                              (1 + TWIN_RATE) * 
                                                              (1 - STILLBIRTH_RATE),
                                                            NA_real_)),
      
      ddelivery_dpt = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                      (countdelivery / (deliverycarry / 100)) * 
                                                        (1 + TWIN_RATE) * 
                                                        (1 - STILLBIRTH_RATE) * 
                                                        (1 - NEONATAL_MORTALITY_RATE),
                                                      NA_real_)),
      
      ddelivery_mcv = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                      (countdelivery / (deliverycarry / 100)) * 
                                                        (1 + TWIN_RATE) * 
                                                        (1 - STILLBIRTH_RATE) * 
                                                        (1 - INFANT_MORTALITY_RATE),
                                                      NA_real_)),
      
      # BCG Denominators
      dbcg_pregnancy = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                                  (countbcg / (bcgcarry / 100)) / 
                                                    (1 - PREGNANCY_LOSS_RATE) /
                                                    (1 + TWIN_RATE) /
                                                    (1 - STILLBIRTH_RATE),
                                                  NA_real_)),
      
      dbcg_livebirth = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                                  countbcg / (bcgcarry / 100),
                                                  NA_real_)),
      
      dbcg_dpt = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                            (countbcg / (bcgcarry / 100)) * (1 - NEONATAL_MORTALITY_RATE),
                                            NA_real_)),
      
      dbcg_mcv = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                            (countbcg / (bcgcarry / 100)) * (1 - NEONATAL_MORTALITY_RATE) * 
                                              (1 - POSTNEONATAL_MORTALITY_RATE),
                                            NA_real_)),
      
      # Penta1 Denominators
      dpenta1_pregnancy = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                        (countpenta1 / (penta1carry / 100)) / 
                                                          (1 - PREGNANCY_LOSS_RATE) /
                                                          (1 + TWIN_RATE) /
                                                          (1 - STILLBIRTH_RATE) /
                                                          (1 - NEONATAL_MORTALITY_RATE),
                                                        NA_real_)),
      
      dpenta1_livebirth = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                        (countpenta1 / (penta1carry / 100)) / 
                                                          (1 + TWIN_RATE) / 
                                                          (1 - STILLBIRTH_RATE) / 
                                                          (1 - NEONATAL_MORTALITY_RATE),
                                                        NA_real_)),
      
      dpenta1_dpt = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                  countpenta1 / (penta1carry / 100),
                                                  NA_real_)),
      
      dpenta1_mcv = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                  (countpenta1 / (penta1carry / 100)) * (1 - POSTNEONATAL_MORTALITY_RATE),
                                                  NA_real_))
    )
  
  # Adjust the most recent year's coverage if `nummonth` is available and < 12
  if ("nummonth" %in% available_vars) {
    data <- data %>%
      group_by(admin_area_1) %>%
      mutate(
        most_recent_year = max(year, na.rm = TRUE),
        coverage_adjustment = if_else(year == most_recent_year & nummonth < 12, 12 / nummonth, 1),
        
        # Adjust denominators for the most recent year
        danc1_pregnancy = if_else(year == most_recent_year, danc1_pregnancy * coverage_adjustment, danc1_pregnancy),
        danc1_livebirth = if_else(year == most_recent_year, danc1_livebirth * coverage_adjustment, danc1_livebirth),
        ddelivery_pregnancy = if_else(year == most_recent_year, ddelivery_pregnancy * coverage_adjustment, ddelivery_pregnancy),
        ddelivery_livebirth = if_else(year == most_recent_year, ddelivery_livebirth * coverage_adjustment, ddelivery_livebirth),
        dbcg_pregnancy = if_else(year == most_recent_year, dbcg_pregnancy * coverage_adjustment, dbcg_pregnancy),
        dbcg_livebirth = if_else(year == most_recent_year, dbcg_livebirth * coverage_adjustment, dbcg_livebirth),
        dpenta1_pregnancy = if_else(year == most_recent_year, dpenta1_pregnancy * coverage_adjustment, dpenta1_pregnancy),
        dpenta1_livebirth = if_else(year == most_recent_year, dpenta1_livebirth * coverage_adjustment, dpenta1_livebirth)
      ) %>%
      ungroup() %>%
      select(-most_recent_year, -coverage_adjustment)
  }
  
  return(data)
}

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
  data <- calculate_denominators(data, adjustment_factors)  # Apply all indicator-based denominator calculations
  data <- calculate_wpp_denominators(data, adjustment_factors)  # Apply WPP-based denominators
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
# Step 1: Extract reference values for each year independently
extract_reference_values <- function(data) {
  # Find all "carry" columns
  carry_cols <- grep("carry$", names(data), value = TRUE)
  
  if (length(carry_cols) == 0) {
    stop("No '_carry' columns found in data! Check column names.")
  }
  
  # Pivot 'carry' columns into long format per year
  carry_values <- data %>%
    select(admin_area_1, year, all_of(carry_cols)) %>%
    pivot_longer(cols = all_of(carry_cols), 
                 names_to = "indicator_to_match_on", 
                 values_to = "reference_value") %>%
    mutate(indicator_to_match_on = gsub("carry$", "", indicator_to_match_on)) %>%
    drop_na(reference_value) %>%
    arrange(admin_area_1, year, indicator_to_match_on)  # Ensure year-wise processing
  
  return(carry_values)
}

# Step 2: Merge survey estimates for each year independently
merge_survey_estimates <- function(coverage_long, carry_values) {
  # Expand carry_values to match all relevant indicator-year pairs
  expanded_carry_values <- coverage_long %>%
    select(admin_area_1, year, indicator_to_match_on) %>%
    distinct() %>%  # Ensure no duplicate rows
    left_join(carry_values, by = c("admin_area_1", "year", "indicator_to_match_on")) %>%
    drop_na(reference_value)  # Drop missing reference values
  
  # Ensure no duplicate reference values within the same year
  expanded_carry_values <- expanded_carry_values %>%
    distinct(admin_area_1, year, indicator_to_match_on, reference_value)
  
  # Now merge with coverage_long for each year independently
  merged_data <- left_join(
    coverage_long, expanded_carry_values,
    by = c("admin_area_1", "year", "indicator_to_match_on")
  ) %>%
    arrange(admin_area_1, year, indicator_to_match_on)  # Ensure order is year-wise
  
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

# Step 3: Select best denominator for each year independently
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

# PART 9 - Extrapolate  Estimates ----------------------------------------------------
# Function to compute coverage delta
detect_coverage_delta <- function(best_coverage) {
  coverage_table <- best_coverage %>%
    select(admin_area_1, year, indicator_common_id, coverage, denominator, denominator_value, denominator_type) %>%
    rename(
      best_coverage = coverage,
      best_denominator = denominator,
      best_denominator_value = denominator_value,
      best_denominator_type = denominator_type
    ) %>%
    arrange(admin_area_1, indicator_common_id, year) %>%
    group_by(admin_area_1, indicator_common_id) %>%
    mutate(
      coverage_delta = if_else(
        !is.na(best_coverage) & !is.na(lag(best_coverage)), 
        best_coverage - lag(best_coverage), 
        NA_real_
      ),
      # Ensure missing deltas don't block propagation
      coverage_delta = ifelse(is.na(coverage_delta), 0, coverage_delta)
    ) %>%
    ungroup()
  
  return(coverage_table)
}

# Function to calculate avg survey projections based on coverage deltas and reference values
calculate_avgsurveyprojection <- function(coverage_table, carry_values) {
  
  # Merge coverage_table with carry_values (reference values) using 'indicator_common_id' and 'year'
  coverage_table <- left_join(coverage_table, carry_values, by = c("admin_area_1", "year", "indicator_common_id" = "indicator_to_match_on"))
  
  # Ensure the data is ordered by year for each admin area and indicator
  coverage_table <- coverage_table %>%
    arrange(admin_area_1, indicator_common_id, year)
  
  # Calculate the avg survey projection using reference_value and coverage_delta
  coverage_table <- coverage_table %>%
    group_by(admin_area_1, indicator_common_id) %>%
    mutate(
      avgsurveyprojection = reference_value + cumsum(coverage_delta) # Add coverage_delta to reference_value over time
    ) %>%
    ungroup()
  
  return(coverage_table)
}

# PART 10 - Prepare Results (combine estimates) --------------------------------------
prepare_combined_coverage_data <- function(data_survey, annual_hmis, coverage_table_with_projection, best_coverage) {
  
  # 1. Transform official estimates from data_survey
  official_estimate_long <- data_survey %>%
    select(admin_area_1, year, starts_with("avgsurvey_"), starts_with("datasource_")) %>%
    pivot_longer(cols = starts_with("avgsurvey_"), 
                 names_to = "indicator_common_id", 
                 values_to = "coverage") %>%
    mutate(source = "official_estimate",
           # Clean indicator names by removing "avgsurvey_" prefix
           indicator_common_id = gsub("avgsurvey_", "", indicator_common_id)) %>%
    filter(!indicator_common_id %in% c("nmr", "imr")) %>%
    select(admin_area_1, year, indicator_common_id, coverage, source)
  
  # 2. Transform annual HMIS data
  annual_hmis_long <- annual_hmis %>%
    pivot_longer(cols = starts_with("count"), 
                 names_to = "indicator_common_id", 
                 values_to = "count") %>%
    mutate(indicator_common_id = gsub("count", "", indicator_common_id))  # Clean indicator names
  
  # 3. Transform coverage_table_with_projection
  coverage_projection_long <- coverage_table_with_projection %>%
    select(admin_area_1, year, indicator_common_id, avgsurveyprojection, reference_value) %>%
    pivot_longer(cols = c("avgsurveyprojection", "reference_value"), 
                 names_to = "source", 
                 values_to = "coverage")
  
  # 4. Transform best_coverage
  best_coverage_long <- best_coverage %>%
    select(admin_area_1, year, indicator_common_id, coverage) %>%
    mutate(source = "cov")
  
  # 5. Transform annual_hmis_long and rename for merging
  annual_hmis_long <- annual_hmis_long %>%
    select(admin_area_1, year, indicator_common_id, count) %>%
    mutate(source = "count", coverage = count) %>%
    select(-count)
  
  # 6. Merge all datasets together
  combined_data <- bind_rows(coverage_projection_long, 
                             best_coverage_long, 
                             annual_hmis_long, 
                             official_estimate_long) %>%
    arrange(admin_area_1, year, indicator_common_id, source) %>%
    filter(source != "reference_value")  # Remove reference_value rows
  
  return(combined_data)
}

# ------------------------------ Main Execution -----------------------------------
# 1. Load and Map Adjusted Volumes
print("load HMIS adjusted volume...")
adjusted_volume <- map_adjusted_volumes(adjusted_volume_data)
hmis_countries <- unique(adjusted_volume$admin_area_1)        # Identify Relevant Countries from HMIS Data

# 2. Aggregate HMIS Data to Annual Level
print("aggregate HMIS adjusted volume to annual level...")
annual_hmis <- adjusted_volume %>%
  select(admin_area_1, year, indicator_common_id, count, month) %>%  
  group_by(admin_area_1, year, indicator_common_id) %>%
  summarise(
    count = sum(count, na.rm = TRUE),  # Sum counts across all months
    .groups = "drop"
  )

# Step 2: Count number of unique months per `admin_area_1` and `year`
nummonth_data <- adjusted_volume %>%
  select(admin_area_1, year, month) %>%
  distinct() %>%  # Keep only unique month entries per area-year
  group_by(admin_area_1, year) %>%
  summarise(nummonth = n_distinct(month[!is.na(month)]), .groups = "drop")  # Count unique months

annual_hmis <- annual_hmis %>%
  pivot_wider(
    names_from = indicator_common_id,  
    values_from = count,  
    names_prefix = "count",  # Prefix column names with "count"
    values_fill = list(count = 0)  # Fill missing values with 0
  )

annual_hmis <- annual_hmis %>%
  left_join(nummonth_data, by = c("admin_area_1", "year"))


annual_hmis <- annual_hmis %>%
  arrange(admin_area_1, year)

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
print("Extend survey data...")
mics_data_extended <- extend_survey_data(mics_data_filtered, prefix = "mics")
dhs_data_extended <- extend_survey_data(dhs_data_filtered, prefix = "dhs")
wpp_data_extended <- extend_survey_data(wpp_data_filtered, prefix = "wpp")


# 6. Merge All Data Sources (Post-Filtering)
print("Combine surveys...")
data <- annual_hmis %>%
  full_join(mics_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(dhs_data_extended, by = c("admin_area_1", "year")) %>%
  full_join(wpp_data_extended, by = c("admin_area_1", "year")) %>%
  filter(year >= MIN_YEAR & year <= CURRENT_YEAR) %>%
  create_survey_averages()

# 7. Apply Carry Forward Survey Data
print("Carry fwd surveys...")
data_survey <- carry_forward_survey_data(data)

# 8. Assign Carried Survey Data 
data <- assign_carried_survey_data(data_survey)

# 9. Calculate Denominators
print("Calculate denominators...")
data <- calculate_all_denominators(data, adjustment_factors)

# 10. Calculate Coverage for Each Indicator
print("Calculate coverage for each indicator")
coverage_long <- calculate_coverage(data)

# 11. Select Best Denominator (Choose the denominator with the smallest error compared to surveys)
print("Select best denominator...")
carry_values <- extract_reference_values(data)
merged_coverage <- merge_survey_estimates(coverage_long, carry_values)
best_coverage <- select_best_denominator(merged_coverage)

# 12. Projection
print("Extrapolate coverage estimates...")
coverage_table <- detect_coverage_delta(best_coverage)
coverage_table_with_projection <- calculate_avgsurveyprojection(coverage_table, carry_values)

# 13. Call function to generate the combined dataset
combined_data <- prepare_combined_coverage_data(data_survey, annual_hmis, coverage_table_with_projection, best_coverage)

# 14. Export the cleaned dataset
print("Save the results..")
write.csv(combined_data, "M4_Coverage_Estimation.csv", row.names = FALSE)

