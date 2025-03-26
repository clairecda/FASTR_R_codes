SELECTED_COUNT_VARIABLE <- "count_final_both"  # Options: "count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both"

CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))  # Dynamically get current year
MIN_YEAR <- 2000  # Set a fixed minimum year for filtering

PREGNANCY_LOSS_RATE <- 0.03
TWIN_RATE <- 0.015       
STILLBIRTH_RATE <- 0.02
NEONATAL_MORTALITY_RATE <- 0.03
POSTNEONATAL_MORTALITY_RATE <- 0.02
INFANT_MORTALITY_RATE <- 0.05   

# CB - R code FASTR PROJECT
# Last edit: 2025 March 24
# Module: Coverage Estimates

# Description:
# This script processes the annual volume data (adjusted for completeness and outliers),  
# applies population projections, integrates survey data (MICS/DHS),  
# calculates coverage estimates, selects optimal denominators based on minimal error compared to surveys,  
# and exports the results for the country.  

# Ce code traite les données de volume annuel (ajustées pour la complétude et les valeurs aberrantes),  
# applique des projections démographiques, intègre les données d'enquêtes (MICS/DHS),  
# calcule les estimations de couverture, sélectionne les dénominateurs optimaux en fonction de l'erreur minimale  
# par rapport aux enquêtes, et exporte les résultats pour le pays.  


# ------------------------------------- KEY OUTPUTS --------------------------------------------------------------------------------------------
# FILE(S): M4_Coverage_estimation.csv #long format with source column to differentiate what type of coverate estimate it is


# ------------------------------ Load Required Libraries -------------------------
library(dplyr)       # For `mutate()`, `group_by()`, `summarise()`, `filter()`, `arrange()`
library(tidyr)       # For `pivot_longer()`, `pivot_wider()`, `complete()`
library(zoo)         # For `na.locf()` in `carry_forward_survey_data()`
library(stringr)     # For `str_subset()` to detect `geo_cols`
library(haven)       # For reading `.dta` Stata files

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- read.csv("M2_adjusted_data_national.csv")
wpp_data_path        <- "~/Desktop/FASTR/Coverage_Analysis/UNWPP/WPP.dta"
mics_data_path       <- "~/Desktop/FASTR/Coverage_Analysis/MICS/MICS.dta"
dhs_data_path        <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS.dta"

# ------------------------------ Define Parameters --------------------------------
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
  
  # Ensure all expected indicators exist
  missing_indicators <- setdiff(expected_indicators, unique(data$indicator_common_id))
  if (length(missing_indicators) > 0) {
    warning("The following indicators are missing from the dataset: ", paste(missing_indicators, collapse = ", "))
  }
  
  # Keep `indicator_common_id` and dynamically rename `SELECTED_COUNT_VARIABLE` to `count`
  data <- data %>%
    mutate(count = !!sym(SELECTED_COUNT_VARIABLE)) %>%  # Assign selected count variable to "count"
    select(admin_area_1, year, month, indicator_common_id, count) %>%  # Keep only necessary columns
    arrange(admin_area_1, year, month, indicator_common_id)
  
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
  
  for (var in existing_vars) {
    original_var <- paste0(var, "_original")
    
    if (!(original_var %in% colnames(data))) {  # Only create if not already existing
      data[[original_var]] <- data[[var]]
    }
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
calculate_denominators <- function(data) {
  
  # Define indicator names and their required columns
  indicator_vars <- list(
    anc1 = c("countanc1", "anc1carry"),
    delivery = c("countdelivery", "deliverycarry"),
    bcg = c("countbcg", "bcgcarry"),
    penta1 = c("countpenta1", "penta1carry")
  )
  
  # Identify available columns in data
  available_vars <- names(data)
  
  # Safe mutate function to prevent errors when required variables are missing
  safe_mutate <- function(var_name, formula) {
    if (all(indicator_vars[[var_name]] %in% available_vars)) {
      return(if_else(!is.na(data[[indicator_vars[[var_name]][1]]]) & 
                       !is.na(data[[indicator_vars[[var_name]][2]]]), formula, NA_real_))
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
                                                      (1 - (TWIN_RATE / 2)) *
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
                                                (1 - (TWIN_RATE / 2)) *
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
  # Adjust denominators for all years where `nummonth` < 12
  if ("nummonth" %in% available_vars) {
    data <- data %>%
      group_by(admin_area_1, year) %>%
      mutate(
        # Ensure `nummonth` is valid (never 0 or NA)
        coverage_adjustment = if_else(is.na(nummonth) | nummonth == 0, 1, nummonth / 12),
        
        # Apply adjustment to all numeric denominators starting with "d"
        across(where(is.numeric) & starts_with("d"), 
               ~ if_else(!is.na(.x), .x * coverage_adjustment, NA_real_))
      ) %>%
      ungroup() %>%
      select(-coverage_adjustment)
  }
  
  
  return(data)
}

# Function to calculate denominators for WPP (World Population Prospects)
calculate_wpp_denominators <- function(data) {
  # Ensure `nummonth` is valid
  data <- data %>%
    mutate(nummonth = if_else(is.na(nummonth) | nummonth == 0, 12, nummonth)) 
  
  # Apply WPP-based denominator calculations while preserving NA logic
  data <- data %>%
    mutate(
      dwpp_pregnancy = if_else(!is.na(wppCBR) & !is.na(wpptotpop),
                               (wppCBR / 1000) * wpptotpop / (1 + TWIN_RATE), NA_real_),
      
      dwpp_livebirth = if_else(!is.na(wpplivebirth), wpplivebirth, NA_real_),
      
      dwpp_dpt = if_else(!is.na(wpptotu1pop), wpptotu1pop, NA_real_),
      
      dwpp_mcv = if_else(!is.na(wpptotu5pop), wpptotu5pop, NA_real_)
    ) %>%
    # Adjust all non-missing denominators dynamically
    mutate(across(starts_with("dwpp"), ~ if_else(!is.na(.x), .x * (nummonth / 12), NA_real_)))
  
  
  return(data)
}

# Main function to call all of the above functions
calculate_all_denominators <- function(data) {
  data <- calculate_denominators(data)  # Apply all indicator-based denominator calculations
  data <- calculate_wpp_denominators(data)  # Apply WPP-based denominators
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
        denominator_type == "livebirth" & indicator_to_match_on %in% c("delivery", "bcg") ~ (numerator / denominator_value)* 100,
        denominator_type == "dpt" & indicator_to_match_on %in% c("penta1", "penta3") ~ (numerator / denominator_value)* 100,
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
    rename(indicator_common_id = indicator_to_match_on) %>% 
    arrange(admin_area_1, year, indicator_common_id)  # Ensure order is year-wise
  
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

compare_denominators <- function(merged_data) {
  ranked_denominators <- merged_data %>%
    filter(!is.na(squared_error)) %>%  # Keep only valid comparisons
    group_by(admin_area_1, year, indicator_common_id) %>%
    arrange(squared_error) %>%  # Sort by smallest error first
    mutate(rank = row_number()) %>%  # Rank denominators by error
    ungroup() %>%
    
    select(admin_area_1, year, indicator_common_id, coverage,
           denominator, denominator_value, squared_error, rank) %>%
    mutate(denominator = paste0(denominator))  # Ensure clarity
  
  return(ranked_denominators)
}

select_best_denominator <- function(merged_data) {
  best_denominator <- merged_data %>%
    filter(!is.na(squared_error)) %>%  # Remove rows where squared_error is NA
    group_by(admin_area_1, year, indicator_common_id) %>%
    slice_min(squared_error, with_ties = FALSE) %>%  # Select row with min squared_error
    ungroup() %>%
    
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

# Add function to compute coverage delta with ALL denominators included
detect_coverage_delta_all <- function(merged_data) {
  merged_data %>%
    arrange(admin_area_1, indicator_common_id, denominator, year) %>%
    group_by(admin_area_1, indicator_common_id, denominator) %>%
    mutate(
      coverage_delta = if_else(
        !is.na(coverage) & !is.na(lag(coverage)),
        coverage - lag(coverage),
        NA_real_
      ),
      coverage_delta = if_else(is.na(coverage_delta), 0, coverage_delta)
    ) %>%
    ungroup()
}

# Function to calculate avg survey projection for each denominator separately
calculate_avgsurveyprojection_all <- function(coverage_table, carry_values) {
  # Remove existing reference_value to avoid name clash
  coverage_table <- coverage_table %>%
    select(-reference_value)
  
  # Rename for join
  carry_values <- carry_values %>%
    rename(indicator_common_id = indicator_to_match_on)
  
  # Merge
  merged <- left_join(
    coverage_table,
    carry_values,
    by = c("admin_area_1", "year", "indicator_common_id")
  )
  
  # Check
  if (!"reference_value" %in% names(merged)) {
    stop("reference_value not found after merge! Join failed.")
  }
  
  # Projection logic
  merged <- merged %>%
    arrange(admin_area_1, indicator_common_id, denominator, year) %>%
    group_by(admin_area_1, indicator_common_id, denominator) %>%
    mutate(
      avgsurveyprojection = if (all(is.na(reference_value))) {
        NA_real_
      } else {
        first(reference_value) + cumsum(coverage_delta)
      },
      projection_source = paste0("avgsurveyprojection_", denominator)
    ) %>%
    ungroup()
  
  return(merged)
}

# PART 10 - Prepare Results (combine estimates) --------------------------------------
prepare_combined_coverage_data <- function(data_survey, coverage_table_with_projection) {

  # 1. Process official estimates from data_survey
  original_estimate_long <- data_survey %>%
    select(admin_area_1, year, ends_with("_original")) %>%
    pivot_longer(cols = ends_with("_original"),
                 names_to = "indicator_common_id",
                 values_to = "coverage") %>%
    mutate(source = "original_estimate",
           indicator_common_id = gsub("avgsurvey_", "", indicator_common_id),
           indicator_common_id = gsub("_original", "", indicator_common_id)) %>%
    select(admin_area_1, year, indicator_common_id, coverage, source)

  # 2. Transform coverage_table_with_projection
  coverage_projection_long <- coverage_table_with_projection %>%
    select(admin_area_1, year, indicator_common_id, avgsurveyprojection) %>%
    pivot_longer(cols = c("avgsurveyprojection"),
                 names_to = "source",
                 values_to = "coverage")

  # 3. Transform best_coverage
  best_coverage_long <- best_coverage %>%
    select(admin_area_1, year, indicator_common_id, coverage) %>%
    mutate(source = "cov")

  # 4. Merge all datasets together
  combined_data <- bind_rows(
    coverage_projection_long,
    best_coverage_long,
    original_estimate_long
  ) %>%
    arrange(admin_area_1, year, indicator_common_id, source)

  # 5. Pivot to wide format
  combined_data_wide <- combined_data %>%
    pivot_wider(
      id_cols = c(admin_area_1, year, indicator_common_id),
      names_from = source,
      values_from = coverage,
      names_prefix = "coverage_",
      values_fill = NA
    ) %>%
    mutate(
      coverage_original_estimate = coverage_original_estimate / 100,
      coverage_avgsurveyprojection = coverage_avgsurveyprojection / 100,
      coverage_cov = coverage_cov / 100
    ) %>%
    filter(!(is.na(coverage_original_estimate) & is.na(coverage_avgsurveyprojection) & is.na(coverage_cov)))

  return(combined_data_wide)
}

prepare_combined_coverage_data_all <- function(data_survey, coverage_data, ranked_denominators) {
  
  # 1. Process original survey estimates
  original_estimates <- data_survey %>%
    select(admin_area_1, year, ends_with("_original")) %>%
    pivot_longer(
      cols = ends_with("_original"),
      names_to = "indicator_common_id",
      values_to = "coverage_original_estimate"
    ) %>%
    mutate(
      indicator_common_id = gsub("avgsurvey_", "", indicator_common_id),
      indicator_common_id = gsub("_original", "", indicator_common_id)
    )
  
  # 2. Get all unique denominators per indicator
  unique_denoms <- coverage_data %>%
    distinct(indicator_common_id, denominator)
  
  # 3. Expand each survey row across all denominators
  expanded_originals <- original_estimates %>%
    left_join(unique_denoms, by = "indicator_common_id")  # removed relationship arg
  
  # 4. Pull projections and HMIS coverage
  projections <- coverage_data %>%
    select(admin_area_1, year, indicator_common_id, denominator, avgsurveyprojection)
  
  hmis_cov <- coverage_data %>%
    select(admin_area_1, year, indicator_common_id, denominator, coverage) %>%
    rename(coverage_cov = coverage)
  
  # 5. Bring in rank
  ranks <- ranked_denominators %>%
    select(admin_area_1, year, indicator_common_id, denominator, rank)
  
  # 6. Join all together
  final <- expanded_originals %>%
    left_join(projections, by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    left_join(hmis_cov,    by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    left_join(ranks,       by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    rename(coverage_avgsurveyprojection = avgsurveyprojection) %>%
    
    # 7. Apply transformations
    mutate(
      coverage_original_estimate = coverage_original_estimate / 100,
      coverage_avgsurveyprojection = coverage_avgsurveyprojection / 100,
      coverage_cov = coverage_cov / 100
    ) %>%
    
    # 8. Filter incomplete rows
    filter(!(is.na(coverage_original_estimate) & is.na(coverage_avgsurveyprojection) & is.na(coverage_cov))) %>%
    
    # 9. Final columns
    select(admin_area_1, year, indicator_common_id, denominator,
           coverage_original_estimate, coverage_avgsurveyprojection, coverage_cov, rank)
  
  return(final)
}

# ------------------------------ Main Execution -----------------------------------
# 1. Load and Map Adjusted Volumes
print("load HMIS adjusted volume...")
adjusted_volume <- map_adjusted_volumes(adjusted_volume_data)
hmis_countries <- unique(adjusted_volume$admin_area_1)        # Identify Relevant Countries from HMIS Data
print(hmis_countries)

# 2. Count number of unique months per `admin_area_1` and `year`
nummonth_data <- adjusted_volume %>%
  select(admin_area_1, year, month) %>%
  distinct() %>%  # Keep only unique month entries per area-year
  group_by(admin_area_1, year) %>%
  summarise(nummonth = n_distinct(month[!is.na(month)]), .groups = "drop")  # Count unique months


# 2. Aggregate HMIS Data to Annual Level
print("aggregate HMIS adjusted volume to annual level...")
annual_hmis <- adjusted_volume %>%
  group_by(admin_area_1, year, indicator_common_id) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = indicator_common_id,  
    values_from = count,  
    names_prefix = "count",  # 
    values_fill = list(count = 0)
  ) %>%  
  left_join(nummonth_data, by = c("admin_area_1", "year")) %>%
  arrange(admin_area_1, year)


# 3. Adjust Names for Consistency
name_replacements <- c("Guinea" = "Guinée", "Sierra Leone" = "SierraLeone", "Nigeria" = "ng Federal Government")

# 4. Apply Filtering to Ensure Only Relevant Countries are Included
mics_data_filtered <- read_dta(mics_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries) %>%  # Keep only HMIS countries
  filter(year != 2024)  # Remove year 2024

dhs_data_filtered <- read_dta(dhs_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries) %>%  # Keep only HMIS countries
  filter(year != 2024)  # Remove year 2024

wpp_data_filtered <- read_dta(wpp_data_path) %>%
  rename(admin_area_1 = country) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries) %>%  # Keep only HMIS countries
  filter(year != 2024)  # Remove year 2024

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
data <- calculate_all_denominators(data)

# 10. Calculate Coverage for Each Indicator
print("Calculate coverage for each indicator")
coverage_long <- calculate_coverage(data)

# 11. Select Best Denominator (Choose the denominator with the smallest error compared to surveys)
print("Select best denominator...")
carry_values <- extract_reference_values(data)

merged_data <- merge_survey_estimates(coverage_long, carry_values)
best_coverage <- select_best_denominator(merged_data)
comparative_coverage <- compare_denominators(merged_data)


# 12. Projection
print("Extrapolate coverage estimates...")
coverage_table <- detect_coverage_delta(best_coverage)
all_coverage <- detect_coverage_delta_all(merged_data)  # Compute deltas for all denominators

coverage_table_with_projection <- calculate_avgsurveyprojection(coverage_table, carry_values)
all_coverage <- calculate_avgsurveyprojection_all(all_coverage, carry_values)  # Compute projections (with ALL denominators included)



# 13. Call function to generate the combined dataset
#combined_data <- prepare_combined_coverage_data(data_survey, coverage_table_with_projection)
combined_coverage_data <- prepare_combined_coverage_data_all(data_survey, all_coverage, comparative_coverage)


# 14. Carry back survey projection to official survey points 
# Identify indicators where ALL values of coverage_cov are missing
indicators_to_remove <- combined_coverage_data %>%
  group_by(indicator_common_id) %>%
  summarize(all_missing = all(is.na(coverage_cov))) %>%
  filter(all_missing) %>%
  pull(indicator_common_id)

# Remove these indicators from the dataset
combined_coverage_data <- combined_coverage_data %>%
  filter(!indicator_common_id %in% indicators_to_remove) %>%
  ungroup() 

# Compute adjusted survey estimates (one per indicator-year)
adjusted_survey <- combined_coverage_data %>%
  group_by(admin_area_1, indicator_common_id, year) %>%
  summarise(
    coverage_original_estimate = first(coverage_original_estimate),
    coverage_avgsurveyprojection = first(coverage_avgsurveyprojection),
    .groups = "drop"
  ) %>%
  group_by(indicator_common_id) %>%
  mutate(
    first_year_projection = min(year[!is.na(coverage_avgsurveyprojection)], na.rm = TRUE),
    last_year_original = max(year[!is.na(coverage_original_estimate)], na.rm = TRUE),
    
    coverage_original_estimate = case_when(
      year == first_year_projection & is.na(coverage_original_estimate) & !is.na(coverage_avgsurveyprojection) ~ coverage_avgsurveyprojection,
      year == last_year_original  & !is.na(coverage_original_estimate) & !is.na(coverage_avgsurveyprojection) ~ coverage_avgsurveyprojection,
      TRUE ~ coverage_original_estimate
    )
  ) %>%
  select(admin_area_1, year, indicator_common_id, adjusted_survey = coverage_original_estimate)

# Join the cleaned survey estimates back into the full dataset
combined_coverage_data <- combined_coverage_data %>%
  select(-coverage_original_estimate) %>%
  left_join(adjusted_survey, by = c("admin_area_1", "year", "indicator_common_id")) %>%
  rename(coverage_official_estimate = adjusted_survey)


# 15. Export the cleaned dataset
print("Save the results..")
write.csv(combined_coverage_data, "M4_coverage_estimation.csv", row.names = FALSE)

print("Coverage estimate analysis - country-wide - completed!")
## ---------------------------------------------------------------------------------------------------------------- ##
print("Start the coverage estimate analysis at the admin_area_2 level")

province_name_replacements <- c(
  "ab Abia State" = "Abia", "ad Adamawa State" = "Adamawa", "ak Akwa-Ibom State" = "Akwa Ibom",
  "an Anambra state" = "Anambra", "ba Bauchi State" = "Bauchi", "be Benue State" = "Benue",
  "bo Borno State" = "Borno", "by Bayelsa State" = "Bayelsa", "cr Cross River State" = "Cross River",
  "de Delta State" = "Delta", "eb Ebonyi State" = "Ebonyi", "ed Edo State" = "Edo",
  "ek Ekiti State" = "Ekiti", "en Enugu State" = "Enugu", "fc Federal Capital Territory" = "FCT Abuja",
  "go Gombe State" = "Gombe", "im Imo State" = "Imo", "ji Jigawa State" = "Jigawa",
  "kd Kaduna State" = "Kaduna", "ke Kebbi State" = "Kebbi", "kn Kano State" = "Kano",
  "ko Kogi State" = "Kogi", "kt Katsina State" = "Katsina", "kw Kwara State" = "Kwara",
  "la Lagos State" = "Lagos", "na Nasarawa State" = "Nasarawa", "ni Niger State" = "Niger",
  "og Ogun State" = "Ogun", "on Ondo State" = "Ondo", "os Osun State" = "Osun",
  "oy Oyo State" = "Oyo", "pl Plateau State" = "Plateau", "ri Rivers State" = "Rivers",
  "so Sokoto State" = "Sokoto", "ta Taraba State" = "Taraba", "yo Yobe State" = "Yobe",
  "za Zamfara State" = "Zamfara"
)

# ------------------------------ Part 1: Key Functions ------------------------------------
print("Load all the key functions for the analysis...")
adjust_names_for_merging <- function(data, column, replacements) {
  data[[column]] <- recode(data[[column]], !!!replacements)
  return(data)
}
extend_survey_data_province <- function(survey_data, min_year = MIN_YEAR, max_year = CURRENT_YEAR, prefix) {
  full_year_range <- seq(min_year, max_year)
  
  for (var in c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")) {
    survey_var <- paste0("dhs", var)
    new_col <- paste0("avgsurvey_", var)
    if (survey_var %in% colnames(survey_data)) {
      survey_data[[new_col]] <- survey_data[[survey_var]]
    }
  }
  
  survey_data %>%
    filter(year >= min_year & year <= max_year) %>%
    group_by(admin_area_1, admin_area_2) %>%
    complete(year = full_year_range) %>%
    arrange(admin_area_1, admin_area_2, year) %>%
    mutate(
      across(
        starts_with("avgsurvey_"),
        ~ zoo::na.locf(.x, na.rm = FALSE),
        .names = "{.col}_carry"
      )
    ) %>%
    ungroup()
}
carry_forward_survey_data_province <- function(data) {
  survey_vars <- c(
    "avgsurvey_anc1", "avgsurvey_anc4", "avgsurvey_delivery",
    "avgsurvey_bcg", "avgsurvey_penta1", "avgsurvey_penta3",
    "avgsurvey_nmr", "avgsurvey_imr", "postnmr"
  )
  
  existing_vars <- intersect(survey_vars, colnames(data))
  if (length(existing_vars) == 0) {
    warning("No survey variables found to carry forward.")
    return(data)
  }
  
  for (var in existing_vars) {
    original_var <- paste0(var, "_original")
    if (!(original_var %in% colnames(data))) {
      data[[original_var]] <- data[[var]]
    }
  }
  
  data %>%
    arrange(admin_area_1, admin_area_2, year) %>%
    group_by(admin_area_1, admin_area_2) %>%
    complete(year = full_seq(year, 1)) %>%
    mutate(
      across(
        all_of(existing_vars),
        ~ zoo::na.locf(.x, na.rm = FALSE)
      )
    ) %>%
    ungroup()
}
assign_carried_survey_data_province <- function(data) {
  carry_vars <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")
  
  for (var in carry_vars) {
    colname <- paste0("avgsurvey_", var)
    if (colname %in% colnames(data)) {
      data[[paste0(var, "carry")]] <- data[[colname]]
    }
  }
  return(data)
}

calculate_denominators_province <- function(data) {
  safe_calc <- function(expr) tryCatch(expr, error = function(e) NA_real_)
  
  # Calculate denominators based on ANC1 (e.g. pregnancies, live births, DPT proxy, MCV proxy)
  if (all(c("countanc1", "anc1carry") %in% names(data))) {
    data <- data %>%
      mutate(
        danc1_pregnancy = safe_calc(countanc1 / (anc1carry / 100)),
        danc1_livebirth = safe_calc(
          danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE)
        ),
        danc1_dpt = safe_calc(
          danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) / (1 - (TWIN_RATE / 2)) * 
            (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE)
        ),
        danc1_mcv = safe_calc(
          danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) * (1 + TWIN_RATE) * 
            (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE)
        )
      )
  }
  
  # Calculate denominators based on delivery counts
  if (all(c("countdelivery", "deliverycarry") %in% names(data))) {
    data <- data %>%
      mutate(
        ddelivery_pregnancy = safe_calc((countdelivery / (deliverycarry / 100)) / (1 - PREGNANCY_LOSS_RATE)),
        ddelivery_livebirth = safe_calc((countdelivery / (deliverycarry / 100)) / (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE)),
        ddelivery_dpt = safe_calc((countdelivery / (deliverycarry / 100)) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE)),
        ddelivery_mcv = safe_calc((countdelivery / (deliverycarry / 100)) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE))
      )
  }
  
  # If 'nummonth' exists, adjust denominators for incomplete reporting
  if ("nummonth" %in% names(data)) {
    data <- data %>%
      mutate(adjustment_factor = if_else(is.na(nummonth) | nummonth == 0, 1, nummonth / 12)) %>%
      mutate(across(starts_with("d"), ~ if_else(!is.na(.x), .x * adjustment_factor, .x))) %>%
      select(-adjustment_factor)
  }
  
  return(data)
}
calculate_hmis_coverage_province <- function(data) {
  # Step 1: Identify relevant denominator columns (exclude MCV)
  denominator_cols <- names(data)[str_detect(names(data), "^d[a-z0-9]+_(pregnancy|livebirth|dpt)$")]
  
  if (length(denominator_cols) == 0) {
    warning("No denominator columns found.")
    return(data)
  }
  
  # Step 2: Define valid indicator-denominator mappings
  denom_indicator_map <- list(
    pregnancy = c("anc1", "anc4"),
    livebirth = c("delivery", "bcg"),
    dpt = c("penta1", "penta3")
  )
  
  # Step 3: Reshape denominator data to long format and map indicators
  denominator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(denominator_cols)) %>%
    pivot_longer(cols = all_of(denominator_cols),
                 names_to = "denominator", values_to = "denominator_value") %>%
    drop_na(denominator_value) %>%
    mutate(
      denominator_type = case_when(
        str_detect(denominator, "_pregnancy$") ~ "pregnancy",
        str_detect(denominator, "_livebirth$") ~ "livebirth",
        str_detect(denominator, "_dpt$") ~ "dpt",
        TRUE ~ NA_character_
      )
    ) %>%
    rowwise() %>%
    mutate(indicator_common_id = list(denom_indicator_map[[denominator_type]])) %>%
    unnest(indicator_common_id) %>%
    ungroup()
  
  # Step 4: Prepare numerator data
  numerator_cols <- names(data)[str_detect(names(data), "^count[a-z0-9]+$")]
  
  if (length(numerator_cols) == 0) {
    warning("No numerator columns found.")
    return(data)
  }
  
  numerator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(numerator_cols)) %>%
    pivot_longer(
      cols = all_of(numerator_cols),
      names_to = "numerator",
      values_to = "numerator_value"
    ) %>%
    mutate(indicator_common_id = str_remove(numerator, "^count")) %>%
    drop_na(numerator_value)
  
  # Step 5: Merge + calculate coverage
  coverage_data <- inner_join(
    denominator_long,
    numerator_long,
    by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id")
  ) %>%
    mutate(
      coverage = ifelse(
        denominator_value > 0,
        100 * numerator_value / denominator_value,
        NA_real_
      ),
      coverage_col = paste0("cov_", indicator_common_id, "_", str_remove(denominator, "^d"))
    )
  
  # Step 6: Spread and merge back
  coverage_wide <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, coverage_col, coverage) %>%
    pivot_wider(names_from = coverage_col, values_from = coverage)
  
  data <- left_join(data, coverage_wide, by = c("admin_area_1", "admin_area_2", "year"))
  
  return(data)
}

extract_reference_values_province <- function(data) {
  carry_cols <- grep("carry$", names(data), value = TRUE)
  if (length(carry_cols) == 0) stop("No '_carry' columns found in data!")
  
  data %>%
    select(admin_area_1, admin_area_2, year, all_of(carry_cols)) %>%
    pivot_longer(cols = all_of(carry_cols),
                 names_to = "indicator_to_match_on",
                 values_to = "reference_value") %>%
    mutate(indicator_to_match_on = gsub("carry$", "", indicator_to_match_on)) %>%
    drop_na(reference_value) %>%
    arrange(admin_area_1, admin_area_2, year, indicator_to_match_on)
}
extract_all_denominator_coverage_province <- function(data) {
  # Define mapping from denominator type to indicators (based on Stata logic)
  denom_indicator_map <- list(
    pregnancy = c("anc1", "anc4"),
    livebirth = c("delivery", "bcg"),
    dpt = c("penta1", "penta3")
  )
  
  # Get all denominator columns that match the valid suffixes
  denom_cols <- names(data)[stringr::str_detect(names(data), "^d[a-z0-9]+_(pregnancy|livebirth|dpt)$")]
  
  # Pivot longer and tag with denominator type
  denominator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(denom_cols)) %>%
    pivot_longer(cols = all_of(denom_cols),
                 names_to = "denominator",
                 values_to = "denominator_value") %>%
    drop_na(denominator_value) %>%
    mutate(
      denominator_type = case_when(
        str_ends(denominator, "_pregnancy") ~ "pregnancy",
        str_ends(denominator, "_livebirth") ~ "livebirth",
        str_ends(denominator, "_dpt") ~ "dpt"
      )
    ) %>%
    rowwise() %>%
    mutate(indicator_to_match_on = list(denom_indicator_map[[denominator_type]])) %>%
    unnest(indicator_to_match_on) %>%
    ungroup()
  
  # Attach numerators
  numerator_long <- data %>%
    select(admin_area_1, admin_area_2, year, starts_with("count")) %>%
    pivot_longer(cols = starts_with("count"),
                 names_to = "numerator",
                 values_to = "numerator_value") %>%
    mutate(indicator_to_match_on = str_remove(numerator, "^count")) %>%
    filter(!is.na(numerator_value))
  
  # Merge and calculate coverage
  long <- inner_join(
    denominator_long,
    numerator_long,
    by = c("admin_area_1", "admin_area_2", "year", "indicator_to_match_on")
  ) %>%
    mutate(
      coverage = ifelse(
        denominator_value > 0,
        100 * numerator_value / denominator_value,
        NA_real_
      )
    ) %>%
    select(admin_area_1, admin_area_2, year, indicator_to_match_on,
           denominator, denominator_value, coverage)
  
  return(long)
}
merge_survey_estimates_province <- function(coverage_long, reference_values) {
  coverage_long %>%
    left_join(reference_values,
              by = c("admin_area_1", "admin_area_2", "year", "indicator_to_match_on")) %>%
    rename(indicator_common_id = indicator_to_match_on) %>%
    mutate(
      squared_error = ifelse(!is.na(reference_value) & !is.na(coverage),
                             (coverage - reference_value)^2,
                             NA_real_)
    )
}
rank_denominators_by_error_province <- function(merged_data) {
  merged_data %>%
    filter(!is.na(squared_error)) %>%
    group_by(admin_area_1, admin_area_2, year, indicator_common_id) %>%
    arrange(squared_error) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    select(admin_area_1, admin_area_2, year, indicator_common_id,
           coverage, denominator, denominator_value, squared_error, rank)
}
detect_coverage_delta_all_province <- function(merged_data) {
  merged_data %>%
    arrange(admin_area_1, admin_area_2, indicator_common_id, denominator, year) %>%
    group_by(admin_area_1, admin_area_2, indicator_common_id, denominator) %>%
    mutate(
      coverage_delta = if_else(
        !is.na(coverage) & !is.na(lag(coverage)),
        coverage - lag(coverage),
        NA_real_
      ),
      coverage_delta = if_else(is.na(coverage_delta), 0, coverage_delta)
    ) %>%
    ungroup()
}
calculate_avgsurveyprojection_all_province <- function(coverage_table, carry_values) {
  # Clean duplicate columns
  coverage_table <- coverage_table %>% select(-reference_value)
  carry_values <- carry_values %>% rename(indicator_common_id = indicator_to_match_on)
  
  merged <- left_join(
    coverage_table,
    carry_values,
    by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id")
  )
  
  if (!"reference_value" %in% names(merged)) {
    stop("reference_value not found after merge! Join failed.")
  }
  
  merged %>%
    arrange(admin_area_1, admin_area_2, indicator_common_id, denominator, year) %>%
    group_by(admin_area_1, admin_area_2, indicator_common_id, denominator) %>%
    mutate(
      # Use only the first available reference as anchor
      anchor_value = reference_value[which(!is.na(reference_value))[1]],
      avgsurveyprojection = ifelse(
        is.na(anchor_value),
        NA_real_,
        anchor_value + cumsum(coverage_delta)
      ),
      projection_source = paste0("avgsurveyprojection_", denominator)
    ) %>%
    ungroup()
}

prepare_combined_coverage_data_province <- function(data_survey, coverage_data, ranked_data) {
  
  # 1. Process DHS original estimates
  official_estimates <- data_survey %>%
    select(admin_area_1, admin_area_2, year, ends_with("_original")) %>%
    pivot_longer(
      cols = ends_with("_original"),
      names_to = "indicator_common_id",
      values_to = "coverage_official_estimate"
    ) %>%
    mutate(
      indicator_common_id = str_remove(indicator_common_id, "^avgsurvey_"),
      indicator_common_id = str_remove(indicator_common_id, "_original$")
    )
  
  # 2. Get all unique indicator-denominator pairs
  unique_denoms <- coverage_data %>%
    distinct(indicator_common_id, denominator)
  
  # 3. Expand survey rows across all denominator options
  expanded_officials <- official_estimates %>%
    left_join(unique_denoms, by = "indicator_common_id", relationship = "many-to-many")
  
  # 4. Pull HMIS coverage and projections
  hmis_cov <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, indicator_common_id, denominator, coverage) %>%
    rename(coverage_cov = coverage)
  
  projections <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, indicator_common_id, denominator, avgsurveyprojection) %>%
    rename(coverage_avgsurveyprojection = avgsurveyprojection)
  
  ranks <- ranked_data %>%
    select(admin_area_1, admin_area_2, year, indicator_common_id, denominator, rank)
  
  # 5. Join all together
  final <- expanded_officials %>%
    left_join(projections, by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id", "denominator")) %>%
    left_join(hmis_cov,    by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id", "denominator")) %>%
    left_join(ranks,       by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id", "denominator"))
  
  # 6. Normalize and clean
  final %>%
    mutate(across(
      c(coverage_official_estimate, coverage_avgsurveyprojection, coverage_cov),
      ~ .x / 100
    )) %>%
    filter(!(is.na(coverage_official_estimate) & is.na(coverage_avgsurveyprojection) & is.na(coverage_cov))) %>%
    arrange(admin_area_1, admin_area_2, indicator_common_id, year, denominator)
}

# ------------------------------ Part 2: Load Input Data ------------------------------------
print("Load data for coverage estimate (admin_area_2) level analysis ")
# Load adjusted HMIS service volumes
adjusted_volume_data_province <- read.csv("M2_adjusted_data_admin_area.csv")

# Load DHS province-level survey estimates
dhs_data_province_path <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS Province.dta"

# ------------------------------ Part 3: Prepare HMIS Aggregated Data ------------------------------------
print("Prepare HMIS data for analysis...")
# Identify countries included in the HMIS dataset
hmis_countries <- unique(adjusted_volume_data_province$admin_area_1)
print(hmis_countries)

# Filter and reshape adjusted volume data
volume_data <- adjusted_volume_data_province %>%
  filter(year >= MIN_YEAR & year <= CURRENT_YEAR)

expected_indicators <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr")
missing_indicators <- setdiff(expected_indicators, unique(volume_data$indicator_common_id))
if (length(missing_indicators) > 0) {
  warning("The following indicators are missing from the dataset: ", paste(missing_indicators, collapse = ", "))
}

adjusted_volume_province <- volume_data %>%
  mutate(count = .data[[SELECTED_COUNT_VARIABLE]]) %>%
  select(admin_area_1, admin_area_2, year, month, indicator_common_id, count) %>%
  arrange(admin_area_1, admin_area_2, year, month, indicator_common_id)

# Count available months of data per province-year
nummonth_data <- adjusted_volume_province %>%
  distinct(admin_area_1, admin_area_2, year, month) %>%
  group_by(admin_area_1, admin_area_2, year) %>%
  summarise(nummonth = n_distinct(month, na.rm = TRUE), .groups = "drop")

# Aggregate HMIS volumes to annual level
print("Aggregating HMIS adjusted volume to annual level...")

annual_hmis_province <- adjusted_volume_province %>%
  group_by(admin_area_1, admin_area_2, year, indicator_common_id) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = indicator_common_id,
    values_from = count,
    names_prefix = "count",
    values_fill = list(count = 0)
  ) %>%
  left_join(nummonth_data, by = c("admin_area_1", "admin_area_2", "year")) %>%
  arrange(admin_area_1, admin_area_2, year)
# ------------------------------ Part 4: Prepare DHS Survey Data ------------------------------------
print("Prepare the DHS survey data for analysis...")
# Load and clean DHS survey estimates
dhs_data_province <- read_dta(dhs_data_province_path) %>%
  rename(admin_area_1 = country, admin_area_2 = province) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  # Rename short province names to match long-form province names in HMIS data
  mutate(admin_area_2 = recode(admin_area_2, !!!setNames(names(province_name_replacements), province_name_replacements))) %>%
  filter(admin_area_1 %in% hmis_countries, year != 2024)


# Extend and carry forward survey values
print("Extend survey data...")
dhs_data_province_extended <- extend_survey_data_province(dhs_data_province, prefix = "")
dhs_data_province_carried <- carry_forward_survey_data_province(dhs_data_province_extended)
dhs_data_province_carried <- assign_carried_survey_data_province(dhs_data_province_carried)

# ------------------------------ Part 5: Merge + Compute Denominators & Coverage ------------------------------------
print("Merge survey values with HMIS, calculate denominators, calculate coverage values for each numerator-denominator combination...")
# Merge survey with HMIS and assign reference values
annual_hmis_province <- annual_hmis_province %>%
  left_join(dhs_data_province_carried, by = c("admin_area_1", "admin_area_2", "year")) %>%
  assign_carried_survey_data_province()

# Calculate denominators
annual_hmis_province <- calculate_denominators_province(annual_hmis_province)

# Compute coverage values for each numerator-denominator combination
annual_hmis_province <- calculate_hmis_coverage_province(annual_hmis_province)

# ------------------------------ Part 6: Compare Coverage vs Survey Estimates ------------------------------------
print("Compare coverage VS survey estimates, rank denominators based on lowest squared error vs survey...")
# Extract reference survey values for each indicator
ref_vals_province <- extract_reference_values_province(annual_hmis_province)

# Calculate coverage using all denominator options
long_cov_province <- extract_all_denominator_coverage_province(annual_hmis_province)

# Merge HMIS coverage with survey reference values and calculate error
merged_province <- merge_survey_estimates_province(long_cov_province, ref_vals_province)

# Rank denominators based on lowest squared error vs survey
ranked_denominators_province <- rank_denominators_by_error_province(merged_province)

# ------------------------------ Part 7: Project Survey Coverage Forward ------------------------------------
print("Project survey coverage fwd...")
ranked_with_deltas_province <- detect_coverage_delta_all_province(ranked_denominators_province)
ranked_with_deltas_province <- left_join(
  ranked_with_deltas_province,
  ref_vals_province %>%
    rename(indicator_common_id = indicator_to_match_on),
  by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id")
)
projected_coverage_province <- calculate_avgsurveyprojection_all_province(
  ranked_with_deltas_province,
  ref_vals_province
)


# ------------------------------ Part 8: Prepare results for visualization ----------------------------------
# Same format as national results
combined_coverage_data_province <- prepare_combined_coverage_data_province(
  data_survey = dhs_data_province_carried,
  coverage_data = projected_coverage_province,
  ranked_data = ranked_denominators_province
)

# Long format - need to calculate mean(coverage_cov) and present time period min year - max year on plot
coverage_estimate_hmis_province <- combined_coverage_data_province %>%
  filter(!is.na(coverage_cov), rank == 1) %>%  # keep best denominator only
  select(admin_area_1, admin_area_2, year, indicator_common_id, coverage_cov)



print("Save the results..")
#write.csv(combined_coverage_data_province, "M4_coverage_estimation_admin_area_2.csv", row.names = FALSE) # most likely not used in visualization
write.csv(coverage_estimate_hmis_province, "M4_coverage_estimation_admin_area_2.csv", row.names = FALSE)

print("Coverage estimate analysis - admin_area_2-level - completed!")