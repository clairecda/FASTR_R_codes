SELECTED_COUNT_VARIABLE <- "count_final_both"  # Options: "count_final_none", "count_final_outlier", "count_final_completeness", "count_final_both"

CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))  # Dynamically get current year
MIN_YEAR <- 2000  # Set a fixed minimum year for filtering

PREGNANCY_LOSS_RATE <- 0.03
TWIN_RATE <- 0.015       
STILLBIRTH_RATE <- 0.02
NEONATAL_MORTALITY_RATE <- 0.03
POSTNEONATAL_MORTALITY_RATE <- 0.02
INFANT_MORTALITY_RATE <- 0.05   

# ------------------------------ Load Required Libraries -------------------------
library(dplyr)       # For `mutate()`, `group_by()`, `summarise()`, `filter()`, `arrange()`
library(tidyr)       # For `pivot_longer()`, `pivot_wider()`, `complete()`
library(zoo)         # For `na.locf()` in `carry_forward_survey_data()`
library(stringr)     # For `str_subset()` to detect `geo_cols`
library(haven)       # For reading `.dta` Stata files

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- read.csv("M2_adjusted_data_national.csv")
adjusted_volume_data_subnational <- read.csv("M2_adjusted_data_admin_area.csv", fileEncoding = "UTF-8")
survey_data_unified <- read.csv("survey_data_unified.csv", fileEncoding = "UTF-8")
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

name_replacements <- c(
  "Guinea" = "Guinée",
  "Sierra Leone" = "SierraLeone",
  "Nigeria" = "ng Federal Government"
)

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
  "za Zamfara State" = "Zamfara",
  
  #guinea
  "DSV Conakry" = "Conakry",
  "IRS Faranah" = "Faranah",
  "IRS Kankan" = "Kankan",
  "IRS Kindia" = "Kindia",
  "IRS Mamou" = "Mamou",
  "IRS Boké" = "Boké",
  "IRS Labé" = "Labé",
  "IRS Nzérékoré" = "N'Zérékoré"
)

# ------------------------------ Define Functions --------------------------------
#Part 1 - prepare hmis data
process_hmis_adjusted_volume <- function(adjusted_volume_data, count_col = SELECTED_COUNT_VARIABLE, province_name_replacements = NULL) {
  expected_indicators <- c(
    "anc1", "anc4", "delivery", "bcg", "penta1", "penta3", "nmr", "imr",
    "measles1", "measles2", "rota1", "rota2", "polio1", "polio2", "polio3"
  )
  
  message("Loading and mapping adjusted HMIS volume...")
  
  has_admin2 <- "admin_area_2" %in% names(adjusted_volume_data)
  
  # Apply province name replacements if admin_area_2 is present and replacements are provided
  if (has_admin2 && !is.null(province_name_replacements)) {
    adjusted_volume_data <- adjusted_volume_data %>%
      mutate(admin_area_2 = recode(admin_area_2, !!!province_name_replacements))
  }
  
  group_vars <- if (has_admin2) c("admin_area_1", "admin_area_2", "year") else c("admin_area_1", "year")
  
  adjusted_volume <- adjusted_volume_data %>%
    mutate(count = .data[[count_col]]) %>%
    select(any_of(c("admin_area_1", "admin_area_2", "year", "month", "indicator_common_id", "count"))) %>%
    arrange(across(any_of(c("admin_area_1", "admin_area_2", "year", "month", "indicator_common_id"))))
  
  missing <- setdiff(expected_indicators, unique(adjusted_volume$indicator_common_id))
  if (length(missing) > 0) {
    warning("The following indicators are not available in the HMIS data: ", paste(missing, collapse = ", "))
  }
  
  hmis_countries <- unique(adjusted_volume$admin_area_1)
  message("HMIS data for country: ", paste(hmis_countries, collapse = ", "))
  
  nummonth_data <- adjusted_volume %>%
    distinct(across(all_of(c(group_vars, "month")))) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(nummonth = n_distinct(month, na.rm = TRUE), .groups = "drop")
  
  message("Aggregating HMIS volume to annual level...")
  
  annual_hmis <- adjusted_volume %>%
    group_by(across(all_of(c(group_vars, "indicator_common_id")))) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = indicator_common_id,
      values_from = count,
      names_prefix = "count",
      values_fill = list(count = 0)
    ) %>%
    left_join(nummonth_data, by = group_vars) %>%
    arrange(across(all_of(group_vars)))
  
  list(
    annual_hmis = annual_hmis,
    hmis_countries = hmis_countries
  )
}
#Part 2 - prepare survey data
process_survey_data <- function(survey_data, name_replacements, hmis_countries,
                                min_year = MIN_YEAR, max_year = CURRENT_YEAR) {
  indicators <- c("anc1", "anc4", "delivery", "bcg", "penta1", "penta3",
                  "measles1", "measles2", "rota1", "rota2",
                  "polio1", "polio2", "polio3", "nmr", "imr")
  
  is_national <- all(unique(survey_data$admin_area_2) == "NATIONAL")
  
  survey_data <- survey_data %>%
    mutate(admin_area_1 = dplyr::recode(admin_area_1, !!!name_replacements)) %>%
    filter(admin_area_1 %in% hmis_countries)
  
  survey_filtered <- if (is_national) {
    survey_data %>% filter(admin_area_2 == "NATIONAL")
  } else {
    survey_data %>% filter(admin_area_2 != "NATIONAL")
  }
  
  full_years <- seq(min_year, max_year)
  
  survey_extended <- survey_filtered %>%
    filter(year %in% full_years) %>%
    group_by(across(any_of(c("admin_area_1", if (!is_national) "admin_area_2" else NULL))), indicator_common_id, source) %>%
    tidyr::complete(year = full_years) %>%
    arrange(across(any_of(c("admin_area_1", if (!is_national) "admin_area_2" else NULL))), indicator_common_id, source, year) %>%
    mutate(survey_value_carry = zoo::na.locf(survey_value, na.rm = FALSE)) %>%
    ungroup()
  
  survey_wide <- survey_extended %>%
    mutate(source = case_when(
      str_detect(tolower(source), "dhs")  ~ "dhs",
      str_detect(tolower(source), "mics") ~ "mics",
      TRUE                                ~ tolower(source)
    )) %>%
    select(all_of(c("admin_area_1", if (!is_national) "admin_area_2" else NULL)), year, indicator_common_id, source, survey_value_carry) %>%
    pivot_wider(
      names_from  = c(source, indicator_common_id),
      values_from = survey_value_carry,
      names_glue  = "{indicator_common_id}_{source}",
      values_fn   = mean
    )
  
  for (ind in indicators) {
    dhs_col <- paste0(ind, "_dhs")
    mics_col <- paste0(ind, "_mics")
    avg_col <- paste0("avgsurvey_", ind)
    
    dhs_exists  <- dhs_col %in% names(survey_wide)
    mics_exists <- mics_col %in% names(survey_wide)
    
    if (is_national) {
      if (dhs_exists || mics_exists) {
        survey_wide[[avg_col]] <- coalesce(
          if (dhs_exists) survey_wide[[dhs_col]] else NULL,
          if (mics_exists) survey_wide[[mics_col]] else NULL
        )
      }
    } else {
      if (dhs_exists) {
        survey_wide[[avg_col]] <- survey_wide[[dhs_col]]
      }
    }
  }
  
  if (all(c("avgsurvey_imr", "avgsurvey_nmr") %in% names(survey_wide))) {
    survey_wide <- survey_wide %>%
      mutate(postnmr = avgsurvey_imr - avgsurvey_nmr)
  } else {
    survey_wide$postnmr <- NA_real_
  }
  
  carry_group <- if (is_national) c("admin_area_1") else c("admin_area_1", "admin_area_2")
  
  survey_carried <- survey_wide %>%
    group_by(across(all_of(carry_group))) %>%
    tidyr::complete(year = full_seq(year, 1)) %>%
    arrange(across(all_of(carry_group)), year) %>%
    mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
    ungroup()
  
  for (ind in c(indicators, "postnmr")) {
    avg_col   <- paste0("avgsurvey_", ind)
    carry_col <- paste0(ind, "carry")
    if (avg_col %in% colnames(survey_carried)) {
      survey_carried[[carry_col]] <- survey_carried[[avg_col]]
    }
  }
  
  return(survey_carried)
}

#Part 2b - prepare unwpp data
process_national_population_data <- function(population_data,
                                             name_replacements,
                                             hmis_countries) {
  population_data %>%
    filter(admin_area_2 == "NATIONAL") %>%
    mutate(admin_area_1 = dplyr::recode(admin_area_1, !!!name_replacements)) %>%
    filter(admin_area_1 %in% hmis_countries) %>%
    mutate(source = tolower(source)) %>%
    select(admin_area_1, year, indicator_common_id, survey_value, source) %>%
    pivot_wider(
      names_from  = c(indicator_common_id, source),
      values_from = survey_value,
      names_glue  = "{indicator_common_id}_{source}",
      values_fn   = mean
    )
}

#Part 3 - calculate denominators
calculate_denominators <- function(hmis_data, survey_data, population_data = NULL) {
  has_admin_area_2 <- "admin_area_2" %in% names(hmis_data)
  
  if (has_admin_area_2) {
    # Subnational (province-level): no population data used
    data <- hmis_data %>%
      full_join(survey_data, by = c("admin_area_1", "admin_area_2", "year"))
  } else {
    # National-level: use population data
    data <- hmis_data %>%
      full_join(survey_data,     by = c("admin_area_1", "year")) %>%
      full_join(population_data, by = c("admin_area_1", "year"))
  }
  
  # Define indicator requirements
  indicator_vars <- list(
    anc1 = c("countanc1", "anc1carry"),
    delivery = c("countdelivery", "deliverycarry"),
    penta1 = c("countpenta1", "penta1carry")
  )
  
  # Add BCG for national only
  if (!has_admin_area_2) {
    indicator_vars$bcg <- c("countbcg", "bcgcarry")
  }
  
  available_vars <- names(data)
  
  safe_mutate <- function(var_name, formula) {
    required_vars <- indicator_vars[[var_name]]
    if (all(required_vars %in% available_vars)) formula else NA_real_
  }
  
  data <- data %>%
    mutate(
      danc1_pregnancy = safe_mutate("anc1", countanc1 / anc1carry),
      danc1_livebirth = safe_mutate("anc1", danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) * (1 - (TWIN_RATE / 2)) * (1 - STILLBIRTH_RATE)),
      danc1_dpt       = safe_mutate("anc1", danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) / (1 - (TWIN_RATE / 2)) * (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE)),
      danc1_mcv       = safe_mutate("anc1", danc1_pregnancy * (1 - PREGNANCY_LOSS_RATE) * (1 - (TWIN_RATE / 2)) * (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE)),
      
      ddelivery_pregnancy = safe_mutate("delivery", (countdelivery / deliverycarry) / (1 - PREGNANCY_LOSS_RATE)),
      ddelivery_livebirth = safe_mutate("delivery", (countdelivery / deliverycarry) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE)),
      ddelivery_dpt       = safe_mutate("delivery", (countdelivery / deliverycarry) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE)),
      ddelivery_mcv       = safe_mutate("delivery", (countdelivery / deliverycarry) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE)),
      
      dpenta1_dpt = safe_mutate("penta1", countpenta1 / penta1carry),
      dpenta1_mcv = safe_mutate("penta1", (countpenta1 / penta1carry) * (1 - POSTNEONATAL_MORTALITY_RATE)),
      dpenta1_measles1 = safe_mutate("penta1", (countpenta1 / penta1carry) * (1 - POSTNEONATAL_MORTALITY_RATE)),
      dpenta1_measles2 = safe_mutate("penta1", (countpenta1 / penta1carry) * (1 - 2 * POSTNEONATAL_MORTALITY_RATE))
      
    )
  
  if (!has_admin_area_2) {
    data <- data %>% mutate(
      dbcg_pregnancy = safe_mutate("bcg", (countbcg / bcgcarry) / (1 - PREGNANCY_LOSS_RATE) / (1 + TWIN_RATE) / (1 - STILLBIRTH_RATE)),
      dbcg_livebirth = safe_mutate("bcg", countbcg / bcgcarry),
      dbcg_dpt = safe_mutate("bcg", (countbcg / bcgcarry) * (1 - NEONATAL_MORTALITY_RATE)),
      dbcg_mcv = safe_mutate("bcg", (countbcg / bcgcarry) * (1 - NEONATAL_MORTALITY_RATE) * (1 - POSTNEONATAL_MORTALITY_RATE))
    )
    
    # Add WPP-based denominators for national only
    data <- data %>% mutate(nummonth = if_else(is.na(nummonth) | nummonth == 0, 12, nummonth)) %>%
      mutate(
        dwpp_pregnancy = if_else(!is.na(crudebr_unwpp) & !is.na(poptot_mics),
                                 (crudebr_unwpp / 1000) * poptot_mics / (1 + TWIN_RATE), NA_real_),
        
        dwpp_livebirth = if_else(!is.na(crudebr_unwpp) & !is.na(poptot_mics),
                                 (crudebr_unwpp / 1000) * poptot_mics, NA_real_),
        
        dwpp_dpt = if_else(!is.na(totu1pop_unwpp), totu1pop_unwpp, NA_real_),
        
        dwpp_measles1 = if_else(!is.na(totu1pop_unwpp) & !is.na(nmrcarry),
                                totu1pop_unwpp * (1 - (nmrcarry / 100)), NA_real_),
        
        dwpp_measles2 = if_else(!is.na(totu1pop_unwpp) & !is.na(nmrcarry) & !is.na(postnmr),
                                totu1pop_unwpp * (1 - (nmrcarry / 100)) * (1 - (2 * postnmr / 100)), NA_real_),
        
        across(starts_with("dwpp"), ~ if_else(!is.na(.x), .x * (nummonth / 12), NA_real_))
      )
  }
  
  if ("nummonth" %in% available_vars) {
    group_keys <- if (has_admin_area_2) c("admin_area_1", "admin_area_2", "year") else c("admin_area_1", "year")
    
    data <- data %>%
      group_by(across(all_of(group_keys))) %>%
      mutate(
        coverage_adjustment = if_else(is.na(nummonth) | nummonth == 0, 1, nummonth / 12),
        across(where(is.numeric) & starts_with("d"), ~ if_else(!is.na(.x), .x * coverage_adjustment, NA_real_))
      ) %>%
      ungroup() %>%
      select(-coverage_adjustment)
  }
  
  return(data)
}

#Part 4 - calculate coverage and compare all denominators
evaluate_coverage_by_denominator <- function(data) {
  has_admin_area_2 <- "admin_area_2" %in% names(data)
  geo_keys <- if (has_admin_area_2) c("admin_area_1", "admin_area_2", "year") else c("admin_area_1", "year")
  
  # Numerators
  numerator_long <- data %>%
    select(all_of(geo_keys), starts_with("count")) %>%
    pivot_longer(
      cols = -all_of(geo_keys),
      names_to = "numerator_col",
      values_to = "numerator"
    ) %>%
    filter(numerator_col != "count") %>%
    mutate(indicator_common_id = str_remove(numerator_col, "^count")) %>%
    select(-numerator_col)
  
  # Denominators
  denom_pattern <- if (has_admin_area_2) {
    "^d(anc1|delivery|penta1)_(pregnancy|livebirth|dpt|mcv|measles1|measles2)$"
  } else {
    "^d(anc1|delivery|penta1|bcg|wpp)_(pregnancy|livebirth|dpt|mcv|measles1|measles2)$"
  }
  
  denominator_long <- data %>%
    select(all_of(geo_keys), matches(denom_pattern)) %>%
    pivot_longer(
      cols = -all_of(geo_keys),
      names_to = "denominator",
      values_to = "denominator_value"
    ) %>%
    mutate(
      denominator_type = case_when(
        str_detect(denominator, "_pregnancy$") ~ "pregnancy",
        str_detect(denominator, "_livebirth$") ~ "livebirth",
        str_detect(denominator, "_delivery$") ~ "delivery",
        str_detect(denominator, "_dpt$") ~ "penta",
        str_detect(denominator, "_measles1$") ~ "measles1",
        str_detect(denominator, "_measles2$") ~ "measles2",
        TRUE ~ NA_character_
      ),
      indicator_common_id = case_when(
        denominator_type == "pregnancy" ~ list(c("anc1", "anc4")),
        denominator_type == "delivery"  ~ list("delivery"),
        denominator_type == "livebirth" ~ list(c("bcg", "skilled")),
        denominator_type == "penta"     ~ list(c("penta1", "penta2", "penta3", "opv1", "opv2", "opv3", "pcv1", "pcv2", "pcv3", "rota1", "rota2", "ipv1", "ipv2")),
        denominator_type == "measles1"  ~ list("measles1"),
        denominator_type == "measles2"  ~ list("measles2"),
        TRUE ~ list(NA_character_)
      )
    ) %>%
    unnest(indicator_common_id)
  
  # Join numerator and denominator
  coverage_data <- full_join(
    numerator_long,
    denominator_long,
    by = c(geo_keys, "indicator_common_id")
  ) %>%
    mutate(coverage = numerator / denominator_value) %>%
    drop_na(coverage)
  
  # Reference values
  carry_cols <- grep("carry$", names(data), value = TRUE)
  carry_values <- data %>%
    select(all_of(geo_keys), all_of(carry_cols)) %>%
    pivot_longer(
      cols = all_of(carry_cols),
      names_to = "indicator_common_id",
      names_pattern = "(.*)carry$",
      values_to = "reference_value"
    ) %>%
    drop_na(reference_value)
  
  coverage_with_error <- left_join(
    coverage_data,
    carry_values,
    by = c(geo_keys, "indicator_common_id")
  ) %>%
    mutate(squared_error = (coverage - reference_value)^2)
  
  ranked <- coverage_with_error %>%
    filter(!is.na(squared_error)) %>%
    group_by(across(all_of(geo_keys)), indicator_common_id) %>%
    arrange(squared_error) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  best <- ranked %>%
    filter(rank == 1) %>%
    select(all_of(geo_keys), indicator_common_id,
           coverage, reference_value, denominator, denominator_type, squared_error)
  
  list(
    full_ranking = ranked,
    best_only = best
  )
}

#Part 5 - run projections
project_coverage_from_all <- function(ranked_coverage) {
  message("Projecting survey coverage forward using HMIS deltas...")
  
  if (!"reference_value" %in% names(ranked_coverage)) {
    stop("ERROR!! 'reference_value' column not found in ranked_coverage.")
  }
  
  has_admin_area_2 <- "admin_area_2" %in% names(ranked_coverage)
  geo_keys <- if (has_admin_area_2) {
    c("admin_area_1", "admin_area_2", "indicator_common_id", "denominator")
  } else {
    c("admin_area_1", "indicator_common_id", "denominator")
  }
  
  ranked_with_delta <- ranked_coverage %>%
    arrange(across(all_of(c(geo_keys, "year")))) %>%
    group_by(across(all_of(geo_keys))) %>%
    mutate(
      coverage_delta = if_else(
        !is.na(coverage) & !is.na(lag(coverage)),
        coverage - lag(coverage),
        0
      )
    ) %>%
    ungroup()
  
  all_projected <- ranked_with_delta %>%
    group_by(across(all_of(geo_keys))) %>%
    arrange(year) %>%
    mutate(
      avgsurveyprojection = first(reference_value) + cumsum(coverage_delta),
      projection_source = paste0("avgsurveyprojection_", denominator)
    ) %>%
    ungroup()
  
  return(all_projected)
}



# ------------------------------ Main Execution -----------------------------------
# 1 - prepare the hmis data
hmis_processed <- process_hmis_adjusted_volume(adjusted_volume_data)
hmis_processed_subnational <- process_hmis_adjusted_volume(
  adjusted_volume_data_subnational,
  count_col = SELECTED_COUNT_VARIABLE,
  province_name_replacements = province_name_replacements
)



# 2 - prepare the survey data
survey_processed_national <- process_survey_data(
  survey_data = survey_data_unified %>% filter(admin_area_2 == "NATIONAL"),
  name_replacements = name_replacements,
  hmis_countries = hmis_processed$hmis_countries
)


survey_processed_province <- process_survey_data(
  survey_data = survey_data_unified %>% filter(admin_area_2 != "NATIONAL"),
  name_replacements = name_replacements,
  hmis_countries = hmis_processed_subnational$hmis_countries
)

national_population_processed <- process_national_population_data(
  population_data = population_estimates_only,
  name_replacements = name_replacements,
  hmis_countries = hmis_processed$hmis_countries
)

# 3 - calculate the denominators
denominators_national <- calculate_denominators(
  hmis_data = hmis_processed$annual_hmis,
  survey_data = survey_processed_national,
  population_data = national_population_processed
)

denominators_province <- calculate_denominators(
  hmis_data = hmis_processed_subnational$annual_hmis,
  survey_data = survey_processed_province
)

# 4 - calculate coverage and compare the denominators
national_coverage_eval <- evaluate_coverage_by_denominator(denominators_national)
subnational_coverage_eval <- evaluate_coverage_by_denominator(denominators_province)

# 5 - project survey coverage forward using HMIS deltas
national_coverage_projected <- project_coverage_from_all(national_coverage_eval$full_ranking)
subnational_coverage_projected <- project_coverage_from_all(subnational_coverage_eval$full_ranking)
