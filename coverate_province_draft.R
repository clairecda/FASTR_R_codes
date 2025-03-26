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
  # Step 1: Denominator table
  denominator_cols <- names(data)[str_detect(names(data), "^d[a-z0-9]+_(pregnancy|livebirth|dpt|mcv)$")]
  
  denominator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(denominator_cols)) %>%
    pivot_longer(
      cols = everything(),
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
      )
    ) %>%
    mutate(indicator_common_id = str_extract(denominator, "(?<=^d)[a-z0-9]+"))
  
  # Step 2: Numerator table
  numerator_cols <- names(data)[str_detect(names(data), "^count(?!$|.*final)")]
  numerator_cols <- setdiff(numerator_cols, "count")  # safety
  
  if (length(numerator_cols) == 0) {
    warning("No numerator columns found — skipping coverage calculation.")
    return(data)
  }
  
  numerator_long <- data %>%
    select(admin_area_1, admin_area_2, year, all_of(numerator_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "numerator",
      values_to = "numerator_value"
    ) %>%
    mutate(indicator_common_id = str_remove(numerator, "^count")) %>%
    drop_na(numerator_value)
  
  # Step 3: Merge and calculate coverage (only where both exist)
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
  
  # Step 4: Spread wide
  coverage_wide <- coverage_data %>%
    select(admin_area_1, admin_area_2, year, coverage_col, coverage) %>%
    pivot_wider(names_from = coverage_col, values_from = coverage)
  
  # Step 5: Merge back
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
  # Get all denominator columns
  denom_cols <- names(data)[stringr::str_detect(names(data), "^d[a-z0-9]+_(pregnancy|livebirth|dpt|mcv)$")]
  
  # Pivot longer
  long <- data %>%
    select(admin_area_1, admin_area_2, year, starts_with("count"), all_of(denom_cols)) %>%
    pivot_longer(
      cols = all_of(denom_cols),
      names_to = "denominator",
      values_to = "denominator_value"
    ) %>%
    mutate(
      indicator_to_match_on = stringr::str_extract(denominator, "(?<=^d)[a-z0-9]+")
    ) %>%
    rowwise() %>%
    mutate(
      numerator = get(paste0("count", indicator_to_match_on)),
      coverage = ifelse(!is.na(numerator) & !is.na(denominator_value) & denominator_value != 0,
                        100 * numerator / denominator_value,
                        NA_real_)
    ) %>%
    ungroup() %>%
    select(admin_area_1, admin_area_2, year, indicator_to_match_on, denominator, denominator_value, coverage)
  
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
  # Clean any conflicting column before merge
  coverage_table <- coverage_table %>%
    select(-reference_value)
  
  carry_values <- carry_values %>%
    rename(indicator_common_id = indicator_to_match_on)
  
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
      avgsurveyprojection = if (all(is.na(reference_value))) {
        NA_real_
      } else {
        first(reference_value) + cumsum(coverage_delta)
      },
      projection_source = paste0("avgsurveyprojection_", denominator)
    ) %>%
    ungroup()
}




# ------------------------------ Part 2: Load Input Data ------------------------------------

# Load adjusted HMIS service volumes
adjusted_volume_data_province <- read.csv("M2_adjusted_data_admin_area.csv")

# Load DHS province-level survey estimates
dhs_data_province_path <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS Province.dta"

# ------------------------------ Part 3: Prepare HMIS Aggregated Data ------------------------------------

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

# ------------------------------ Part 4: Prepare DHS Survey Data ------------------------------------

# Load and clean DHS survey estimates
dhs_data_province <- read_dta(dhs_data_province_path) %>%
  rename(admin_area_1 = country, admin_area_2 = province) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  adjust_names_for_merging("admin_area_2", province_name_replacements) %>%
  filter(admin_area_1 %in% hmis_countries, year != 2024)

# Extend and carry forward survey values
print("Extend survey data...")
dhs_data_province_extended <- extend_survey_data_province(dhs_data_province, prefix = "")
dhs_data_province_carried <- carry_forward_survey_data_province(dhs_data_province_extended)
dhs_data_province_carried <- assign_carried_survey_data_province(dhs_data_province_carried)

# ------------------------------ Part 5: Merge + Compute Denominators & Coverage ------------------------------------

# Merge survey with HMIS and assign reference values
annual_hmis_province <- annual_hmis_province %>%
  left_join(dhs_data_province_carried, by = c("admin_area_1", "admin_area_2", "year")) %>%
  assign_carried_survey_data_province()

# Calculate denominators
annual_hmis_province <- calculate_denominators_province(annual_hmis_province)

# Compute coverage values for each numerator-denominator combination
annual_hmis_province <- calculate_hmis_coverage_province(annual_hmis_province)

# # ------------------------------ Part 6: Compare Coverage vs Survey Estimates ------------------------------------
# 
# # Extract reference survey values for each indicator
# ref_vals_province <- extract_reference_values_province(annual_hmis_province)
# 
# # Calculate coverage using all denominator options
# long_cov_province <- extract_all_denominator_coverage_province(annual_hmis_province)
# 
# # Merge HMIS coverage with survey reference values and calculate error
# merged_province <- merge_survey_estimates_province(long_cov_province, ref_vals_province)
# 
# # Rank denominators based on lowest squared error vs survey
# ranked_denominators_province <- rank_denominators_by_error_province(merged_province)
# 
# # ------------------------------ Part 7: Project Survey Coverage Forward ------------------------------------
# 
# ranked_with_deltas_province <- detect_coverage_delta_all_province(ranked_denominators_province)
# ranked_with_deltas_province <- left_join(
#   ranked_with_deltas_province,
#   ref_vals_province %>%
#     rename(indicator_common_id = indicator_to_match_on),
#   by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id")
# )
# projected_coverage_province <- calculate_avgsurveyprojection_all_province(
#   ranked_with_deltas_province,
#   ref_vals_province
# )

