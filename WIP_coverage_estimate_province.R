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
# Last edit: 2025 Feb 8
# Module: Coverage Estimates Province

# ------------------------------ Load Required Libraries -------------------------
library(dplyr)       # For `mutate()`, `group_by()`, `summarise()`, `filter()`, `arrange()`
library(tidyr)       # For `pivot_longer()`, `pivot_wider()`, `complete()`
library(zoo)         # For `na.locf()` in `carry_forward_survey_data()`
library(stringr)     # For `str_subset()` to detect `geo_cols`
library(haven)       # For reading `.dta` Stata files

# ------------------------------ Define File Paths -------------------------------
# Input Datasets
adjusted_volume_data <- read.csv("M2_adjusted_data_admin_area.csv")               #aggregated data from module 2
dhs_data_path        <- "~/Desktop/FASTR/Coverage_Analysis/DHS/DHS Province.dta"  #province-level DHS survey


hmis_countries <- unique(adjusted_volume_data$admin_area_1)        # Identify Relevant Countries from HMIS Data
print(hmis_countries)

# Function to adjust names for consistency
adjust_names_for_merging <- function(data, column, replacements) {
  data <- data %>%
    mutate(!!sym(column) := case_when(
      !!sym(column) %in% names(replacements) ~ replacements[!!sym(column)],
      TRUE ~ !!sym(column)
    ))
  return(data)
}



# Adjust Names for Consistency
name_replacements <- c(
  "Guinea" = "Guinée",
  "Sierra Leone" = "SierraLeone"
)

province_replacements <- c(
  "Chattogram" = "Chittagong",
  "Barishal" = "Barisal",
  "Dykundi" = "Daykundi",
  "Hirat" = "Herat",
  "Kunar" = "Kunarha",
  "Sar-e-Pul" = "Sar-E-Pul",
  "Benishangul Gumuz" = "Beneshangul-Gumuz",
  "Northwestern" = "North-Western",
  "Nzerekore" = "NZerekore",
  "Extreme Nord" = "Extreme-Nord",
  "Nord Kivu" = "Nord-Kivu",
  "Sud Kivu" = "Sud-Kivu",
  "Kasai Occident" = "Kasai Central",
  "Gambella" = "Gambela",
  "Oromiya" = "Oromia",
  "SNNP" = "SNNPR",
  "Rivercess" = "River Cess",
  "Montserrado" = "Montserrado and Monrovia",
  "Nkhata Bay" = "Nkhata-Bay",
  
  #GUINEA.....................................
  "BokÃ©" = "IRS Boké",
  "LabÃ©" = "IRS Labé",
  "N'ZÃ©rÃ©korÃ©" = "IRS N'zérékoré",
  "Faranah" = "IRS Faranah",
  "Kindia" = "IRS Kindia",
  "Conakry" = "DSV Conakry",
  "Kankan" = "IRS Kankan",
  "Mamou" = "IRS Mamou"
  
)

# clean up names in the dhs dataset
dhs_data_filtered_province <- read_dta(dhs_data_path) %>%
  rename(admin_area_1 = country, admin_area_2 = province) %>%
  adjust_names_for_merging("admin_area_1", name_replacements) %>%
  adjust_names_for_merging("admin_area_2", province_replacements) %>%
  filter(admin_area_1 %in% hmis_countries)  # Keep only HMIS countries


# Aggregate data to the province level (sum service volumes)
adjusted_volume_province <- adjusted_volume_data %>%
  group_by(admin_area_1, admin_area_2, indicator_common_id, year, month, period_id, quarter_id) %>%
  summarise(
    count = sum(!!sym(SELECTED_COUNT_VARIABLE), na.rm = TRUE),
    .groups = "drop"
  )

adjusted_volume_province_wide <- adjusted_volume_province %>%
  pivot_wider(
    names_from = indicator_common_id, 
    values_from = count, 
    names_prefix = "count_"
  )

# Merge HMIS aggregated data with DHS survey data at province level
merged_data_province <- adjusted_volume_province_wide %>%
  left_join(dhs_data_filtered_province, by = c("admin_area_1", "admin_area_2", "year"))

# Generate survey averages
merged_data_province <- merged_data_province %>%
  mutate(
    avgsurvey_anc1 = dhsanc1,
    avgsurvey_anc4 = dhsanc4,
    avgsurvey_delivery = dhsdelivery
  )

# Carry forward
merged_data_province <- merged_data_province %>%
  group_by(admin_area_1, admin_area_2) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    anc1carry = zoo::na.locf(avgsurvey_anc1, na.rm = FALSE),
    anc4carry = zoo::na.locf(avgsurvey_anc4, na.rm = FALSE),
    deliverycarry = zoo::na.locf(avgsurvey_delivery, na.rm = FALSE)
  ) %>%
  ungroup()

# Replace missing with 70% coverage
merged_data_province <- merged_data_province %>%
  mutate(
    anc1carry = ifelse(is.na(anc1carry), 70.0, anc1carry),
    anc4carry = ifelse(is.na(anc4carry), 70.0, anc4carry),
    deliverycarry = ifelse(is.na(deliverycarry), 70.0, deliverycarry)
  )



calculate_denominators_province <- function(data) {
  data <- data %>%
    mutate(
      # Denominators based on ANC1 volume
      danc1_livebirth = (count_anc1 / (anc1carry / 100)) * (1 - PREGNANCY_LOSS_RATE) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE),
      danc1_dpt = (count_anc1 / (anc1carry / 100)) * (1 - PREGNANCY_LOSS_RATE) / (1 - (TWIN_RATE / 2)) * (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE),
      dmcv_anc1 = (count_anc1 / (anc1carry / 100)) * (1 - PREGNANCY_LOSS_RATE) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE),
      
      # Denominators based on Delivery volume
      ddelivery_pregnancy = (count_delivery / (deliverycarry / 100)) / (1 - PREGNANCY_LOSS_RATE),
      ddelivery_dpt = (count_delivery / (deliverycarry / 100)) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - NEONATAL_MORTALITY_RATE),
      ddelivery_mcv = (count_delivery / (deliverycarry / 100)) * (1 + TWIN_RATE) * (1 - STILLBIRTH_RATE) * (1 - INFANT_MORTALITY_RATE)
    )
  
  return(data)
}

# RUN - CALCULATE DENOMINATORS ----------------------------------------------------------------------
merged_data_province <- calculate_denominators_province(merged_data_province)

# FUNCTION - CALCULATE COVERAGE ---------------------------------------------------------------------
calculate_coverage_province <- function(data) {
  data <- data %>%
    mutate(
      # ANC Coverage
      cov_anc1 = (count_anc1 / danc1_livebirth) * 100,
      cov_anc4 = (count_anc4 / danc1_livebirth) * 100,
      
      # Delivery Coverage
      cov_delivery = (count_delivery / ddelivery_pregnancy) * 100,
      
      # BCG Coverage
      cov_bcg = (count_bcg / danc1_livebirth) * 100,
      
      # Penta Coverage
      cov_penta1 = (count_penta1 / danc1_dpt) * 100,
      cov_penta3 = (count_penta3 / danc1_dpt) * 100
    )
  
  return(data)
}

merged_data_province <- calculate_coverage_province(merged_data_province)

# FUNCTION - SELECT BEST DENOMINATIOR ---------------------------------------------------------------
select_best_denominator_and_pivot <- function(data) {
  # Generate default carry-forward values for missing indicators
  data <- data %>%
    mutate(
      bcgcarry = 70,
      penta1carry = 70,
      penta3carry = 70
    )
  
  # Define coverage indicators
  indicators <- c("anc1", "anc4", "delivery", "penta1", "penta3", "bcg")
  
  for (var in indicators) {
    data <- data %>%
      mutate(
        # Compute squared error between coverage estimate and survey carry values
        !!paste0("error_", var) := (get(paste0("cov_", var)) - get(paste0(var, "carry")))^2
      ) %>%
      group_by(admin_area_1, admin_area_2, year) %>%
      mutate(
        min_error = min(get(paste0("error_", var)), na.rm = TRUE)
      ) %>%
      filter(get(paste0("error_", var)) == min_error) %>%
      ungroup()
  }
  
  # Keep only relevant columns
  data <- data %>%
    select(admin_area_1, admin_area_2, year, starts_with("cov_"), starts_with("source_"), starts_with("datasource"))
  
  # Pivot to long format, keeping indicator names in `indicator_common_id`
  data_long <- data %>%
    pivot_longer(
      cols = starts_with("cov_"),
      names_to = "indicator_common_id",
      values_to = "coverage"
    ) %>%
    mutate(
      indicator_common_id = str_replace(indicator_common_id, "cov_", "")  # Remove prefix for clean indicator names
    )
  
  return(data_long)
}

denominator_long <- select_best_denominator_and_pivot(merged_data_province)

denominator_long <- denominator_long %>%
  group_by(admin_area_1, admin_area_2, indicator_common_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(coverage_delta = coverage - lag(coverage)) %>%
  ungroup()


fill_missing_survey_data <- function(data) {
  data <- data %>%
    group_by(admin_area_1, admin_area_2, indicator_common_id) %>%
    arrange(year, .by_group = TRUE) %>%
    
    # Identify last non-missing survey value
    mutate(last_nonmissing_year = max(year[!is.na(coverage)], na.rm = TRUE)) %>%
    
    # Flag the first missing year after last survey data
    mutate(flag_project = ifelse(year == last_nonmissing_year + 1, 1, 0)) %>%
    
    # Apply HMIS delta to fill missing survey years
    mutate(
      projected_coverage = ifelse(year >= last_nonmissing_year, 
                                  ifelse(!is.na(coverage), coverage, lag(coverage) + lag(coverage_delta)), 
                                  coverage)
    ) %>%
    
    # Drop temp columns
    select(-last_nonmissing_year, -flag_project) %>%
    ungroup()
  
  return(data)
}

# Apply function
coverage_results <- fill_missing_survey_data(denominator_long)


# Compute average coverage per province for each indicator
coverage_results_province <- coverage_results %>%
  group_by(indicator_common_id, admin_area_2) %>%
  summarise(avg_coverage = mean(coverage, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_coverage))



# Create histograms for each indicator and print them
for (indicator in unique(avg_coverage$indicator_common_id)) {
  df <- avg_coverage %>%
    filter(indicator_common_id == indicator) %>%
    arrange(desc(avg_coverage))  # Sort by coverage (high to low)
  
  print(
    ggplot(df, aes(x = reorder(admin_area_2, -avg_coverage), y = avg_coverage)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +  # Flip axes for better readability
      labs(
        title = paste("Average Coverage for", indicator),
        x = "Admin Area 2 (Province)",
        y = "Average Coverage (%)"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  )
}

