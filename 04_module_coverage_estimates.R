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
survey_data_unified <- read.csv("survey_data_unified.csv")
population_estimates_only <- read.csv("population_estimates_only.csv")

# ------------------------------ Define Parameters --------------------------------
# Coverage Estimation Parameters
coverage_params <- list(
  indicators = c(
    "anc1", "anc4", "delivery",
    "bcg", "penta1", "penta3",
    "measles1", "measles2", "rota1", "rota2", "polio1", "polio2", "polio3",
    "nmr", "imr"
  )
)

# List of survey variables to carry forward (for forward-fill and projections)
survey_vars <- c(
  "avgsurvey_anc1", "avgsurvey_anc4", "avgsurvey_delivery",
  "avgsurvey_bcg", "avgsurvey_penta1", "avgsurvey_penta3",
  "avgsurvey_measles1", "avgsurvey_measles2",
  "avgsurvey_rota1", "avgsurvey_rota2",
  "avgsurvey_polio1", "avgsurvey_polio2", "avgsurvey_polio3",
  "postnmr", "avgsurvey_imr", "avgsurvey_nmr"
)

# ------------------------------ Define Functions --------------------------------
# PART 1 - Adjust Names for Merging ----------------------------------------------
adjust_names_for_merging <- function(data, column, replacements) {
  data[[column]] <- recode(data[[column]], !!!replacements)
  return(data)
}

# PART 2 - Map Adjusted Volumes to Indicators -------------------------------------
map_adjusted_volumes <- function(data) {
  expected_indicators <- c(
    "anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr",
    "measles1", "measles2", "rota1", "rota2", "polio1", "polio2", "polio3"
  )
  
  # Ensure all expected indicators exist
  missing_indicators <- setdiff(expected_indicators, unique(data$indicator_common_id))
  if (length(missing_indicators) > 0) {
    warning("The following indicators are missing from the dataset: ", paste(missing_indicators, collapse = ", "))
  }
  
  # Rename selected count variable and keep relevant columns
  data <- data %>%
    mutate(count = !!sym(SELECTED_COUNT_VARIABLE)) %>%
    select(admin_area_1, year, month, indicator_common_id, count) %>%
    arrange(admin_area_1, year, month, indicator_common_id)
  
  return(data)
}

# PART 3 - Extend Survey Data for Missing Years -----------------------------------
extend_survey_data <- function(survey_data, min_year = MIN_YEAR, max_year = CURRENT_YEAR) {
  full_year_range <- seq(min_year, max_year)
  
  survey_data %>%
    filter(year >= min_year & year <= max_year) %>%
    group_by(admin_area_1, indicator_common_id, source) %>%
    group_split() %>%
    lapply(function(group_df) {
      complete_df <- tidyr::complete(group_df, year = full_year_range)
      complete_df <- complete_df %>%
        arrange(year) %>%
        mutate(
          survey_value_carry = zoo::na.locf(survey_value, na.rm = FALSE),
          admin_area_1 = unique(group_df$admin_area_1),
          indicator_common_id = unique(group_df$indicator_common_id),
          source = unique(group_df$source)
        )
      return(complete_df)
    }) %>%
    bind_rows()
}


# PART 4 - Create Survey Averages and Datasource Mapping --------------------------
print("preparing the coverage data and unwpp data -->>> pivot to wide...")
prepare_survey_data <- function(survey_extended) {
  survey_wide <- survey_extended %>%
    filter(admin_area_2 == "NATIONAL") %>%
    select(admin_area_1, year, indicator_common_id, source, survey_value_carry) %>%
    mutate(
      source = case_when(
        str_detect(tolower(source), "dhs") ~ "dhs",
        str_detect(tolower(source), "mics") ~ "mics",
        TRUE ~ tolower(source)
      )
    ) %>%
    pivot_wider(
      names_from = c(source, indicator_common_id),
      values_from = survey_value_carry,
      names_glue = "{indicator_common_id}_{source}",
      values_fn = \(x) mean(x, na.rm = TRUE)
    )
  
  coalesce_cols <- function(df, cols) {
    cols <- cols[cols %in% names(df)]
    if (length(cols) == 0) return(NA_real_)
    do.call(coalesce, df[cols])
  }
  
  survey_wide$avgsurvey_anc1     <- coalesce_cols(survey_wide, c("anc1_dhs", "anc1_mics"))
  survey_wide$avgsurvey_anc4     <- coalesce_cols(survey_wide, c("anc4_dhs", "anc4_mics"))
  survey_wide$avgsurvey_delivery <- coalesce_cols(survey_wide, c("delivery_dhs", "delivery_mics"))
  survey_wide$avgsurvey_bcg      <- coalesce_cols(survey_wide, c("bcg_dhs", "bcg_mics"))
  survey_wide$avgsurvey_penta1   <- coalesce_cols(survey_wide, c("penta1_dhs", "penta1_mics"))
  survey_wide$avgsurvey_penta3   <- coalesce_cols(survey_wide, c("penta3_dhs", "penta3_mics"))
  survey_wide$avgsurvey_measles1 <- coalesce_cols(survey_wide, c("measles1_dhs", "measles1_mics"))
  survey_wide$avgsurvey_measles2 <- coalesce_cols(survey_wide, c("measles2_dhs", "measles2_mics"))
  survey_wide$avgsurvey_rota1    <- coalesce_cols(survey_wide, c("rota1_dhs", "rota1_mics"))
  survey_wide$avgsurvey_rota2    <- coalesce_cols(survey_wide, c("rota2_dhs", "rota2_mics"))
  survey_wide$avgsurvey_polio1   <- coalesce_cols(survey_wide, c("polio1_dhs", "polio1_mics"))
  survey_wide$avgsurvey_polio2   <- coalesce_cols(survey_wide, c("polio2_dhs", "polio2_mics"))
  survey_wide$avgsurvey_polio3   <- coalesce_cols(survey_wide, c("polio3_dhs", "polio3_mics"))
  survey_wide$avgsurvey_nmr      <- coalesce_cols(survey_wide, c("nmr_dhs", "nmr_mics"))
  survey_wide$avgsurvey_imr      <- coalesce_cols(survey_wide, c("imr_dhs", "imr_mics"))
  survey_wide$postnmr            <- survey_wide$avgsurvey_imr - survey_wide$avgsurvey_nmr
  
  return(survey_wide)
}

prepare_unwpp_data <- function(unwpp_data) {
  unwpp_data %>%
    filter(admin_area_2 == "NATIONAL") %>%
    mutate(source = tolower(source)) %>%
    select(admin_area_1, year, indicator_common_id, survey_value, source) %>%
    pivot_wider(
      names_from = c(indicator_common_id, source),
      values_from = survey_value,
      names_glue = "{indicator_common_id}_{source}",
      values_fn = mean  # average if duplicate
    )
}


# PART 5.1 - Carry Forward Survey Data --------------------------------------------
carry_forward_survey_data <- function(data) {
  survey_vars <- c(
    "avgsurvey_anc1", "avgsurvey_anc4", "avgsurvey_delivery",
    "avgsurvey_bcg", "avgsurvey_penta1", "avgsurvey_penta3",
    "avgsurvey_measles1", "avgsurvey_measles2",
    "avgsurvey_rota1", "avgsurvey_rota2",
    "avgsurvey_polio1", "avgsurvey_polio2", "avgsurvey_polio3",
    "avgsurvey_nmr", "avgsurvey_imr", "postnmr"
  )
  
  existing_vars <- intersect(survey_vars, colnames(data))
  if (length(existing_vars) == 0) {
    warning("No survey variables found to carry forward.")
    return(data)
  }
  
  # Add original backup columns
  for (var in existing_vars) {
    original_var <- paste0(var, "_original")
    if (!(original_var %in% colnames(data))) {
      data[[original_var]] <- data[[var]]
    }
  }
  
  # Build complete year range per country
  data_filled <- data %>%
    select(admin_area_1, year, all_of(existing_vars)) %>%
    distinct() %>%
    group_by(admin_area_1) %>%
    tidyr::complete(year = full_seq(year, 1)) %>%
    arrange(admin_area_1, year) %>%
    mutate(across(all_of(existing_vars), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
    ungroup()
  
  # Rejoin other columns (e.g. from HMIS or UNWPP)
  other_cols <- setdiff(names(data), names(data_filled))
  data_out <- left_join(data_filled, data[, c("admin_area_1", "year", other_cols)], by = c("admin_area_1", "year"))
  
  return(data_out)
}

# PART 5.2 - Assign Carried Survey Data -------------------------------------------
assign_carried_survey_data <- function(data) {
  carry_vars <- c(
    "anc1", "anc4", "delivery", "bcg", "penta1", "penta3",
    "measles1", "measles2", "rota1", "rota2",
    "polio1", "polio2", "polio3",
    "nmr", "imr", "postnmr"
  )
  
  # Assign *_carry columns using avgsurvey_* values
  for (var in carry_vars) {
    avgsurvey_var <- paste0("avgsurvey_", var)
    carry_col <- paste0(var, "carry")
    
    if (avgsurvey_var %in% colnames(data)) {
      data[[carry_col]] <- data[[avgsurvey_var]]
    }
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
                                                    countanc1 / (anc1carry),
                                                    NA_real_)),
      
      
      danc1_livebirth = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                                    (countanc1 / (anc1carry)) * 
                                                      (1 - PREGNANCY_LOSS_RATE) * 
                                                      (1 - (TWIN_RATE / 2)) *
                                                      (1 - STILLBIRTH_RATE),
                                                    NA_real_)),
      
      danc1_dpt = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                              (countanc1 / (anc1carry)) * 
                                                (1 - PREGNANCY_LOSS_RATE) /
                                                (1 - (TWIN_RATE / 2)) * 
                                                (1 - STILLBIRTH_RATE) * 
                                                (1 - NEONATAL_MORTALITY_RATE),
                                              NA_real_)),
      
      danc1_mcv = safe_mutate("anc1", if_else(!is.na(countanc1) & !is.na(anc1carry),
                                              (countanc1 / (anc1carry)) * 
                                                (1 - PREGNANCY_LOSS_RATE) *
                                                (1 - (TWIN_RATE / 2)) *
                                                (1 - STILLBIRTH_RATE) *
                                                (1 - INFANT_MORTALITY_RATE),
                                              NA_real_)),
      
      # Delivery Denominators
      ddelivery_pregnancy = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                            (countdelivery / (deliverycarry)) / 
                                                              (1 - PREGNANCY_LOSS_RATE),
                                                            NA_real_)),
      
      ddelivery_livebirth = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                            (countdelivery / (deliverycarry)) *
                                                              (1 + TWIN_RATE) *
                                                              (1 - STILLBIRTH_RATE),
                                                            NA_real_)),
      
      ddelivery_dpt = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                      (countdelivery / (deliverycarry)) * 
                                                        (1 + TWIN_RATE) * 
                                                        (1 - STILLBIRTH_RATE) * 
                                                        (1 - NEONATAL_MORTALITY_RATE),
                                                      NA_real_)),
      
      ddelivery_mcv = safe_mutate("delivery", if_else(!is.na(countdelivery) & !is.na(deliverycarry),
                                                      (countdelivery / (deliverycarry)) * 
                                                        (1 + TWIN_RATE) * 
                                                        (1 - STILLBIRTH_RATE) * 
                                                        (1 - INFANT_MORTALITY_RATE),
                                                      NA_real_)),
      
      # BCG Denominators
      dbcg_pregnancy = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                                  (countbcg / (bcgcarry)) / 
                                                    (1 - PREGNANCY_LOSS_RATE) /
                                                    (1 + TWIN_RATE) /
                                                    (1 - STILLBIRTH_RATE),
                                                  NA_real_)),
      
      dbcg_livebirth = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                                  countbcg / (bcgcarry),
                                                  NA_real_)),
      
      dbcg_dpt = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                            (countbcg / (bcgcarry)) * (1 - NEONATAL_MORTALITY_RATE),
                                            NA_real_)),
      
      dbcg_mcv = safe_mutate("bcg", if_else(!is.na(countbcg) & !is.na(bcgcarry),
                                            (countbcg / (bcgcarry)) * (1 - NEONATAL_MORTALITY_RATE) * 
                                              (1 - POSTNEONATAL_MORTALITY_RATE),
                                            NA_real_)),
      
      # Penta1 Denominators
      dpenta1_pregnancy = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                        (countpenta1 / (penta1carry)) / 
                                                          (1 - PREGNANCY_LOSS_RATE) /
                                                          (1 + TWIN_RATE) /
                                                          (1 - STILLBIRTH_RATE) /
                                                          (1 - NEONATAL_MORTALITY_RATE),
                                                        NA_real_)),
      
      dpenta1_livebirth = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                        (countpenta1 / (penta1carry)) / 
                                                          (1 + TWIN_RATE) / 
                                                          (1 - STILLBIRTH_RATE) / 
                                                          (1 - NEONATAL_MORTALITY_RATE),
                                                        NA_real_)),
      
      dpenta1_dpt = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                  countpenta1 / (penta1carry),
                                                  NA_real_)),
      
      dpenta1_mcv = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                  (countpenta1 / (penta1carry)) * (1 - POSTNEONATAL_MORTALITY_RATE),
                                                  NA_real_)),
      
      # add Penta1-based denominators for additional vaccines
      dpenta1_measles1 = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                       (countpenta1 / (penta1carry)) * (1 - POSTNEONATAL_MORTALITY_RATE),
                                                       NA_real_)),
      
      dpenta1_measles2 = safe_mutate("penta1", if_else(!is.na(countpenta1) & !is.na(penta1carry),
                                                       (countpenta1 / (penta1carry)) * (1 - 2 * POSTNEONATAL_MORTALITY_RATE),
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
  data <- data %>%
    mutate(nummonth = if_else(is.na(nummonth) | nummonth == 0, 12, nummonth)) %>%
    mutate(
      # Pregnancy estimate based on crude birth rate and total pop
      dwpp_pregnancy = if_else(!is.na(crudebr_unwpp) & !is.na(poptot_mics),
                               (crudebr_unwpp / 1000) * poptot_mics / (1 + TWIN_RATE), NA_real_),
      
      # Live births from crude birth rate and population
      dwpp_livebirth = if_else(!is.na(crudebr_unwpp) & !is.na(poptot_mics),
                               (crudebr_unwpp / 1000) * poptot_mics, NA_real_),
      
      # DPT1 denominator (U1 population)
      dwpp_dpt = if_else(!is.na(totu1pop_unwpp), totu1pop_unwpp, NA_real_),
      
      # MCV1 denominator (U1 minus NMR)
      dwpp_measles1 = if_else(!is.na(totu1pop_unwpp) & !is.na(nmrcarry),
                              totu1pop_unwpp * (1 - (nmrcarry / 100)), NA_real_),
      
      # MCV2 denominator (U1 minus NMR and 2×PNMR)
      dwpp_measles2 = if_else(!is.na(totu1pop_unwpp) & !is.na(nmrcarry) & !is.na(postnmr),
                              totu1pop_unwpp * (1 - (nmrcarry / 100)) * (1 - (2 * postnmr / 100)), NA_real_)
    ) %>%
    # Scale all denominators by reporting completeness
    mutate(across(starts_with("dwpp"), ~ if_else(!is.na(.x), .x * (nummonth / 12), NA_real_)))
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
  denominator_cols <- names(data)[str_detect(names(data), "^d.*_(pregnancy|livebirth|dpt|measles1|measles2)$")]
  
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
        str_detect(denominator, "_measles1$") ~ "measles1",
        str_detect(denominator, "_measles2$") ~ "measles2",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      indicator_to_match_on = case_when(
        str_detect(denominator, "_pregnancy$") ~ list(c("anc1", "anc4")),
        str_detect(denominator, "_livebirth$") ~ list(c("delivery", "bcg")),
        str_detect(denominator, "_dpt$") ~ list(c("penta1", "penta3")),
        str_detect(denominator, "_measles1$") ~ list("measles1"),
        str_detect(denominator, "_measles2$") ~ list("measles2"),
        TRUE ~ list(NA_character_)
      )
    ) %>%
    unnest(indicator_to_match_on)
  
  # Step 2: Create the Numerator Table
  
  # Select valid numerator columns
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
  coverage_data <- full_join(
    denominator_long, 
    rename(numerator_long, indicator_to_match_on = indicator_common_id),
    by = c("admin_area_1", "year", "indicator_to_match_on")
  )
  
  # Step 4: Calculate Coverage
  coverage_data <- coverage_data %>%
    mutate(
      coverage = (numerator / denominator_value)
    ) %>%
    drop_na(coverage)
  
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
  

  unique_denoms <- coverage_data %>%
    distinct(indicator_common_id, denominator)
  

  expanded_originals <- original_estimates %>%
    left_join(unique_denoms, by = "indicator_common_id")  # removed relationship arg
  

  projections <- coverage_data %>%
    select(admin_area_1, year, indicator_common_id, denominator, avgsurveyprojection)
  
  hmis_cov <- coverage_data %>%
    select(admin_area_1, year, indicator_common_id, denominator, coverage) %>%
    rename(coverage_cov = coverage)
  

  ranks <- ranked_denominators %>%
    select(admin_area_1, year, indicator_common_id, denominator, rank)
  

  final <- expanded_originals %>%
    left_join(projections, by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    left_join(hmis_cov,    by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    left_join(ranks,       by = c("admin_area_1", "year", "indicator_common_id", "denominator")) %>%
    rename(coverage_avgsurveyprojection = avgsurveyprojection) %>%
    
    

    filter(!(is.na(coverage_original_estimate) & is.na(coverage_avgsurveyprojection) & is.na(coverage_cov))) %>%
    

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
name_replacements <- c(
  "Guinea" = "Guinée",
  "Sierra Leone" = "SierraLeone",
  "Nigeria" = "ng Federal Government"
)

# 4. Filter survey and population data for national-level only
survey_data_filtered <- survey_data_unified %>%
  filter(admin_area_2 == "NATIONAL") %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)

population_data_filtered <- population_estimates_only %>%
  filter(admin_area_2 == "NATIONAL") %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)

# 5. Extend Survey Data
print("Extend survey data...")
survey_extended <- extend_survey_data(survey_data_filtered)
population_extended <- extend_survey_data(population_data_filtered)



# 6. Merge All Data Sources (Post-Filtering)
print("Combine surveys...")
survey_prepared <- prepare_survey_data(survey_extended)
unwpp_prepared <- prepare_unwpp_data(population_extended)

data <- annual_hmis %>%
  full_join(survey_prepared, by = c("admin_area_1", "year")) %>%
  full_join(unwpp_prepared, by = c("admin_area_1", "year"))

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
combined_data <- prepare_combined_coverage_data(data_survey, coverage_table_with_projection)
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
