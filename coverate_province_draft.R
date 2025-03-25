# ------------------------------ Setup ----------------------------------------
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(haven)

# ------------------------------ Constants ------------------------------------
SELECTED_COUNT_VARIABLE <- "count_final_both"
CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))
MIN_YEAR <- 2000

# Adjustment factors for coverage calculations
PREGNANCY_LOSS_RATE <- 0.03
TWIN_RATE <- 0.015       
STILLBIRTH_RATE <- 0.02
NEONATAL_MORTALITY_RATE <- 0.03
POSTNEONATAL_MORTALITY_RATE <- 0.02
INFANT_MORTALITY_RATE <- 0.05   

# Country and province name replacements for merging
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
  "za Zamfara State" = "Zamfara"
)

# ------------------------------ Part 1: Key Functions ------------------------------------

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

calculate_hmis_coverage_province <- function(data) {
  # Step 1: Denominator table
  denominator_cols <- names(data)[str_detect(names(data), "^d.*_(pregnancy|livebirth|dpt|mcv)$")]
  
  denominator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(denominator_cols)) %>%
    pivot_longer(
      cols = all_of(denominator_cols),
      names_to = "denominator",
      values_to = "denominator_value"
    ) %>%
    drop_na(denominator_value) %>%
    mutate(
      denominator_type = case_when(
        str_detect(denominator, "_pregnancy$") ~ "pregnancy",
        str_detect(denominator, "_livebirth$") ~ "livebirth",
        str_detect(denominator, "_dpt$") ~ "dpt",
        str_detect(denominator, "_mcv$") ~ "mcv",
        TRUE ~ NA_character_
      ),
      indicator_to_match_on = case_when(
        denominator_type == "pregnancy" ~ list(c("anc1", "anc4")),
        denominator_type == "livebirth" ~ list(c("delivery", "bcg")),
        denominator_type == "dpt" ~ list(c("penta1", "penta3")),
        denominator_type == "mcv" ~ list(c("anc1", "anc4", "delivery")),
        TRUE ~ list(NA_character_)
      )
    ) %>%
    unnest(indicator_to_match_on)
  
  # Step 2: Numerator table
  num_cols <- names(data)[str_detect(names(data), "^count(?!$|.*final)")]
  num_cols <- setdiff(num_cols, "count")
  
  numerator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(num_cols)) %>%
    pivot_longer(
      cols = all_of(num_cols),
      names_to = "numerator_col",
      values_to = "numerator"
    ) %>%
    mutate(
      indicator_to_match_on = str_remove(numerator_col, "^count")
    ) %>%
    drop_na(numerator)
  
  # Step 3: Merge and calculate coverage
  coverage_data <- full_join(
    denominator_long,
    numerator_long,
    by = c("admin_area_1", "admin_area_2", "year", "indicator_to_match_on")
  ) %>%
    mutate(
      coverage = ifelse(
        !is.na(numerator) & !is.na(denominator_value) & denominator_value != 0,
        (numerator / denominator_value) * 100,
        NA_real_
      ),
      coverage_col = paste0("cov_", indicator_to_match_on, "_", str_remove(denominator, "^d"))
    )
  
  # Step 4: Spread back out
  coverage_wide <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, coverage_col, coverage) %>%
    pivot_wider(names_from = coverage_col, values_from = coverage)
  
  # Step 5: Join back to original dataset
  data <- left_join(data, coverage_wide, by = c("admin_area_1", "admin_area_2", "year"))
  
  return(data)
}

calculate_hmis_coverage_province <- function(data) {
  # Step 1: Denominator table
  denominator_cols <- names(data)[str_detect(names(data), "^d.*_(pregnancy|livebirth|dpt|mcv)$")]
  
  denominator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(denominator_cols)) %>%
    pivot_longer(
      cols = all_of(denominator_cols),
      names_to = "denominator",
      values_to = "denominator_value"
    ) %>%
    drop_na(denominator_value) %>%
    mutate(
      denominator_type = case_when(
        str_detect(denominator, "_pregnancy$") ~ "pregnancy",
        str_detect(denominator, "_livebirth$") ~ "livebirth",
        str_detect(denominator, "_dpt$") ~ "dpt",
        str_detect(denominator, "_mcv$") ~ "mcv",
        TRUE ~ NA_character_
      ),
      indicator_to_match_on = case_when(
        denominator_type == "pregnancy" ~ list(c("anc1", "anc4")),
        denominator_type == "livebirth" ~ list(c("delivery", "bcg")),
        denominator_type == "dpt" ~ list(c("penta1", "penta3")),
        denominator_type == "mcv" ~ list(c("anc1", "anc4", "delivery")),
        TRUE ~ list(NA_character_)
      )
    ) %>%
    unnest(indicator_to_match_on)
  
  # Step 2: Numerator table
  num_cols <- names(data)[str_detect(names(data), "^count(?!$|.*final)")]
  num_cols <- setdiff(num_cols, "count")
  
  numerator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(num_cols)) %>%
    pivot_longer(
      cols = all_of(num_cols),
      names_to = "numerator_col",
      values_to = "numerator"
    ) %>%
    mutate(
      indicator_common_id = str_remove(numerator_col, "^count"),
      numerator_col = NULL
    ) %>%
    drop_na(numerator)
  
  # Step 3: Merge and calculate coverage
  coverage_data <- full_join(
    denominator_long,
    numerator_long,
    by = c("admin_area_1", "admin_area_2", "year", "indicator_to_match_on")
  ) %>%
    mutate(
      coverage = ifelse(
        !is.na(numerator) & !is.na(denominator_value) & denominator_value != 0,
        (numerator / denominator_value) * 100,
        NA_real_
      ),
      coverage_col = paste0("cov_", indicator_to_match_on, "_", str_remove(denominator, "^d"))
    )
  
  # Step 4: Spread back out
  coverage_wide <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, coverage_col, coverage) %>%
    pivot_wider(names_from = coverage_col, values_from = coverage)
  
  # Step 5: Join back to original dataset
  data <- left_join(data, coverage_wide, by = c("admin_area_1", "admin_area_2", "year"))
  
  return(data)
}


rank_denominator_options_province <- function(data, indicators = c("anc1", "anc4", "delivery", "penta1", "penta3", "bcg")) {
  results <- list()
  
  for (ind in indicators) {
    cov_cols <- names(data)[grepl(paste0("^cov_", ind, "_"), names(data))]
    carry_var <- paste0(ind, "carry")
    
    # Skip indicator if no relevant coverage columns or carry var
    if (length(cov_cols) == 0 || !(carry_var %in% names(data))) next
    
    temp <- data %>%
      select(admin_area_1, admin_area_2, year, all_of(cov_cols), !!carry_var := .data[[carry_var]]) %>%
      pivot_longer(
        cols = all_of(cov_cols),
        names_to = "denominator_source",
        values_to = "coverage"
      ) %>%
      mutate(
        indicator_common_id = ind,
        squared_error = (coverage - .data[[carry_var]])^2
      ) %>%
      group_by(admin_area_1, admin_area_2, denominator_source) %>%
      mutate(avg_error = mean(squared_error, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(admin_area_1, admin_area_2, year, indicator_common_id) %>%
      mutate(rank = rank(avg_error, ties.method = "min")) %>%
      ungroup()
    
    results[[ind]] <- temp
  }
  
  bind_rows(results)
}

calculate_coverage_province <- function(data) {
  denom_cols <- names(data)[grepl("^d[a-z0-9_]+", names(data))]
  
  for (denom in denom_cols) {
    indicator_match <- stringr::str_extract(denom, "(?<=^d)[a-z0-9]+")
    numerator_col <- paste0("count", indicator_match)
    
    if (numerator_col %in% names(data)) {
      # Extract suffix of the denominator (e.g., "pregnancy", "dpt", "mcv")
      denom_suffix <- sub(paste0("^d", indicator_match, "_"), "", denom)
      
      # Construct clean coverage column name
      coverage_col <- paste0("cov_", indicator_match, "_", denom_suffix)
      
      # Compute coverage using standard vectorized assignment
      data[[coverage_col]] <- ifelse(
        !is.na(data[[numerator_col]]) & !is.na(data[[denom]]) & data[[denom]] != 0,
        (data[[numerator_col]] / data[[denom]]) * 100,
        NA_real_
      )
    }
  }
  
  return(data)
}

# ------------------------------ Part 2: Load Data ------------------------------------

adjusted_volume_data_province <- read.csv("M2_adjusted_data_admin_area.csv")
dhs_data_province_path <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS Province.dta"

# ------------------------------ Part 3: Prepare HMIS Data ------------------------------------

hmis_countries <- unique(adjusted_volume_data_province$admin_area_1)
print(hmis_countries)

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

nummonth_data <- adjusted_volume_province %>%
  distinct(admin_area_1, admin_area_2, year, month) %>%
  group_by(admin_area_1, admin_area_2, year) %>%
  summarise(nummonth = n_distinct(month, na.rm = TRUE), .groups = "drop")

message("Aggregating HMIS adjusted volume to annual level...")

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
  arrange(admin_area_1, admin_area_2, year) %>%
  adjust_names_for_merging("admin_area_2", province_name_replacements)

# ------------------------------ Part 4: Prepare DHS Data ------------------------------------

dhs_data_province <- read_dta(dhs_data_province_path) %>%
  rename(admin_area_1 = country, admin_area_2 = province) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  adjust_names_for_merging("admin_area_2", province_name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries, year != 2024)

print("Extend survey data...")
dhs_data_province_extended <- extend_survey_data_province(dhs_data_province, prefix = "")
dhs_data_province_carried <- carry_forward_survey_data_province(dhs_data_province_extended)
dhs_data_province_carried <- assign_carried_survey_data_province(dhs_data_province_carried)


# ------------------------------ Part 5: Calculate denominators ------------------------------------
annual_hmis_province <- annual_hmis_province %>%
  left_join(dhs_data_province_carried, by = c("admin_area_1", "admin_area_2", "year")) %>%
  assign_carried_survey_data_province()

annual_hmis_province <- calculate_denominators_province(annual_hmis_province)
annual_hmis_province <- calculate_hmis_coverage_province(annual_hmis_province)

coverage_ranked_province <- rank_denominator_options_province(annual_hmis_province)


# ------------------------------ Part 5: Calculate coverage  ----------------------------------------
annual_hmis_province <- calculate_coverage_province(annual_hmis_province)


# ------------------------------ Part 6: Calculate year-over-year deltas -----------------------------

coverage_delta_columns <- names(annual_hmis_province)[grepl("^cov_", names(annual_hmis_province))]

for (col in coverage_delta_columns) {
  delta_col <- paste0("delta_", col)
  annual_hmis_province <- annual_hmis_province %>%
    group_by(admin_area_1, admin_area_2) %>%
    arrange(year) %>%
    mutate(!!delta_col := .data[[col]] - lag(.data[[col]])) %>%
    ungroup()
}

# ------------------------------ Part 7: Project forward survey coverage using HMIS deltas -----------------------------

projected <- list()
carry_indicators <- c("anc1", "anc4", "delivery")  # Can add more

for (indicator in carry_indicators) {
  carry_col <- paste0("avgsurvey_", indicator)
  
  for (denom in c("livebirth", "dpt", "mcv", "pregnancy")) {
    cov_col <- paste0("cov_", indicator, "_", if (indicator %in% c("delivery")) paste0("ddelivery_", denom) else paste0("danc1_", denom))
    delta_col <- paste0("delta_", cov_col)
    proj_col <- paste0("proj_", cov_col)
    
    if (all(c(carry_col, cov_col, delta_col) %in% colnames(annual_hmis_province))) {
      annual_hmis_province <- annual_hmis_province %>%
        group_by(admin_area_1, admin_area_2) %>%
        arrange(year) %>%
        mutate(
          !!proj_col := ifelse(!is.na(.data[[carry_col]]), .data[[carry_col]], NA_real_)
        )
      
      for (i in 2:nrow(annual_hmis_province)) {
        if (is.na(annual_hmis_province[[proj_col]][i]) && !is.na(annual_hmis_province[[proj_col]][i - 1]) && !is.na(annual_hmis_province[[delta_col]][i])) {
          annual_hmis_province[[proj_col]][i] <- annual_hmis_province[[proj_col]][i - 1] + annual_hmis_province[[delta_col]][i]
        }
      }
      
      projected[[proj_col]] <- annual_hmis_province[[proj_col]]
    }
  }
}

