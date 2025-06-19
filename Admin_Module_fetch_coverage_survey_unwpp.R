# Author: cb
# Last updated: 22 April 2025


# FETCH SURVEY DATA FOR COVERAGE ANALYSIS
# FETCH FROM DHS AND MICS
# FETCH POPULATION DATA FROM UNWPP AND WB

readRenviron("~/Desktop/modules/.Renviron")


# ----------------------------------------
# Load required libraries
# ----------------------------------------
library(rdhs)   # For DHS API
library(rsdmx)  # For MICS SDMX API
library(httr)
library(jsonlite)
library(dplyr)  # For data manipulation
library(tidyr)
library(stringr)
library(haven)
library(countrycode)
library(data.table)
library(RCurl)

# ----------------------------------------
# Set DHS indicator codes
# ----------------------------------------

dhs_indicators_base <- c(
  # ANC and maternal
  "RH_ANCP_W_SKP",  # ANC1 from skilled provider
  "RH_ANCN_W_N01",  # Exactly 1 ANC visit
  "RH_ANCN_W_N4P",  # ANC4+
  "RH_DELP_C_DHF",  # Institutional delivery
  "RH_PCMN_W_MOT",  # PNC_mother
  "RH_ANCC_W_IRN",  # Iron supplementation during ANC
  
  # IPTp (SP/Fansidar use)
  "ML_IPTP_W_SPF",  # IPTp1
  "ML_IPTP_W_2SP",  # IPTp2+
  "ML_IPTP_W_3SP",  # IPTp3+
  
  # Family planning
  "FP_SRCM_W_TOT",  # mCPR
  
  # Mortality and fertility
  "CM_PNMR_C_NSB",  # Stillbirth rate
  "CM_ECMT_C_IMR",  # IMR
  "CM_ECMR_C_NNR",  # NMR
  "FE_FRTY_W_NPG",  # Women of reproductive age
  "FE_FRTR_W_CBR",  # Crude birth rate
  "FE_FRTR_W_TFR",  # Total fertility rate
  
  # Child vaccinations
  "CH_VAC1_C_BCG",  # BCG
  "CH_VAC1_C_DP1",  # Penta1
  "CH_VAC1_C_DP3",  # Penta3
  "CH_VAC1_C_OP1",  # Polio1
  "CH_VAC1_C_OP2",  # Polio2
  "CH_VAC1_C_OP3",  # Polio3
  "CH_VAC1_C_MSL",  # Measles1
  "CH_VAC1_C_MS2",  # Measles2
  "CH_VAC1_C_RT1",  # Rotavirus 1
  "CH_VAC1_C_RT2",  # Rotavirus 2
  
  # Child health and nutrition
  "CN_MIAC_C_MMN",  # Micronutrient powder (MNP)
  "CN_MIAC_C_DWM",  # Deworming
  "CN_MIAC_C_VAS",  # Vitamin A
  "CH_DIAT_C_ORZ"   # ORS and Zinc
)

# ----------------------------------------
# Set DHS country ISO codes
# ----------------------------------------

dhs_countries <- c(
  "AF", "BD", "GH", "LB", "CM", "CD", "ET", "GN", "HT", "MD",
  "KE", "MW", "ML", "NG", "SN", "SL", "TD", "UG", "ZM", "GU",
  "TJ", "LR", "MG", "SO"
)



# ----------------------------------------
# Pull DHS national and subnational data
# ----------------------------------------
dhs_national_list <- lapply(dhs_countries, function(iso) {
  tryCatch({
    dhs_data(
      indicatorIds = unique(dhs_indicators_base),
      countryIds = iso,
      breakdown = "national",
      f = "json"
    ) %>%
      mutate(LevelRank = as.character(LevelRank))
  }, error = function(e) {
    message("No NATIONAL DHS data for ", iso, 
            " [Indicators: ", paste(dhs_indicators_base, collapse = ", "), "] → Skipping...")
    NULL
  })
})

dhs_subnational_list <- lapply(dhs_countries, function(iso) {
  tryCatch({
    dhs_data(
      indicatorIds = unique(dhs_indicators_base),
      countryIds = iso,
      breakdown = "subnational",
      f = "json"
    ) %>%
      mutate(LevelRank = as.character(LevelRank))
  }, error = function(e) {
    message("No SUBNATIONAL DHS data for ", iso, 
            " [Indicators: ", paste(dhs_indicators_base, collapse = ", "), "] → Skipping...")
    NULL
  })
})


# ----------------------------------------
# Combine results
# ----------------------------------------

dhs_national <- bind_rows(Filter(Negate(is.null), dhs_national_list))
dhs_subnational <- bind_rows(Filter(Negate(is.null), dhs_subnational_list))




# ----------------------------------------
# Pull MICS data from SDMX API
# ----------------------------------------
mics_indicators <- c(
  "CME_MRM0", "CME_MRY0T4", "DM_POP_GRT", "DM_POP_TOT", "DM_POP_U5",
  "IM_BCG", "IM_HEPBB", "IM_HPV", "IM_HIB3", "IM_DTP1", "IM_DTP3",
  "MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_IPTP", "MNCH_MMR",
  "MNCH_ORS", "MNCH_ORSZINC",  
  "MNCH_PNCMOM", "NT_ANT_BAZ_NE3"
)


mics_url <- paste0(
  "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GLOBAL_DATAFLOW,1.0/.",
  paste(mics_indicators, collapse = "+"),
  "..?startPeriod=2010&endPeriod=2024"
)
mics_sdmx <- readSDMX(mics_url)
mics_data <- as.data.frame(mics_sdmx)


# ----------------------------------------
# Pull UNWPP data via API
# ----------------------------------------
headers <- c("Authorization" = Sys.getenv("UNWPP_TOKEN"))

# --- Get location list ---
base_url <- "https://population.un.org/dataportalapi/api/v1/locations/?pageSize=100&pageNumber="
all_pages <- list()
for (page in 1:3) {
  url <- paste0(base_url, page)
  message("Fetching page ", page)
  
  res <- GET(url, add_headers(Authorization = headers[["Authorization"]]))
  if (status_code(res) != 200) stop("Request failed on page ", page)
  
  parsed <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  all_pages[[page]] <- parsed$data
}


locations_df <- bind_rows(all_pages)

target_iso2 <- c("AF", "BD", "GH", "LR", "CM", "CD", "ET", "GN", "HT", 
                 "MG", "KE", "MW", "ML", "NG", "SO", "SN", "SL", "UG", 
                 "ZM", "TJ", "LB", "MD", "GU", "TD")

target_locations <- locations_df %>%
  filter(iso2 %in% target_iso2) %>%
  select(id, iso2, iso3, name)

# --- List of indicator IDs and short names ---
indicator_map <- tibble::tibble(
  indicator_id = c(2, 22, 24, 41, 46, 47, 49, 55),
  short = c("CPModP", "IMR", "U5MR", "womenrepage", "totu5pop", "totu1pop", "poptot", "CBR")
)

# --- Fetch function ---
fetch_wpp_data <- function(loc_id, indicator_id, start_year = 2020, end_year = 2025) {
  indicator_id <- as.character(indicator_id)
  loc_id <- as.character(loc_id)

  url <- paste0(
    "https://population.un.org/dataportalapi/api/v1/data/indicators/", indicator_id,
    "/locations/", loc_id,
    "/start/", start_year, "/end/", end_year, "?pagingInHeader=true&format=json"
  )

  response <- tryCatch({
    getURL(url, .opts = list(httpheader = headers, followlocation = TRUE))
  }, error = function(e) return(NULL))

  if (is.null(response) || nchar(response) < 10) return(NULL)

  parsed <- tryCatch(fromJSON(response, flatten = TRUE), error = function(e) return(NULL))

  if (!is.null(parsed) && is.data.frame(parsed)) {
    return(parsed)
  } else {
    return(NULL)
  }
}



# --- Main loop ---
all_data <- list()
counter <- 1

for (row in 1:nrow(indicator_map)) {
  ind_id <- indicator_map$indicator_id[row]
  ind_short <- indicator_map$short[row]
  
  message("\n Fetching indicator ", ind_id, " (", ind_short, ")")
  
  for (i in 1:nrow(target_locations)) {
    loc <- target_locations[i, ]
    message("  → ", loc$name)
    
    dat <- fetch_wpp_data(loc$id, ind_id)
    
    if (!is.null(dat) && nrow(dat) > 0) {
      df <- as.data.frame(dat)
      df$indicator_id <- ind_id
      df$indicatorshortname <- ind_short
      df$country <- loc$name
      df$iso3 <- loc$iso3
      all_data[[counter]] <- df
      counter <- counter + 1
    } else {
      message("No data returned.")
    }
  }
}

# Combine result
unwpp_raw <- bind_rows(all_data)


# ----------------------------------------
# Cleaning functions
# ----------------------------------------

# GENERAL HELPER

standardize_admin_area_1 <- function(df) {
  df %>%
    mutate(
      admin_area_1 = case_when(
        admin_area_1 %in% c("Democratic Republic of the Congo", "Congo Democratic Republic", "Congo - Kinshasa") ~ "DRC",
        admin_area_1 == "Sierra Leone" ~ "SierraLeone",
        TRUE ~ admin_area_1
      )
    )
}

# ----------------------------------------
#
#       DHS
#
# ----------------------------------------

clean_dhs_national <- function(df) {
  df %>%
    filter(!is.na(Value), IsPreferred == 1) %>%
    transmute(
      admin_area_1 = CountryName,
      admin_area_2 = "NATIONAL",
      disaggregation = CharacteristicLabel,
      year = SurveyYear,
      indicator_id = tolower(IndicatorId),
      survey_value = Value,
      source = "DHS National",
      source_detail = SurveyId,
      survey_type = "household",
      indicator_common_id = case_when(
        # Vaccines
        indicator_id == "ch_vac1_c_bcg"  ~ "bcg",
        indicator_id == "ch_vac1_c_dp1"  ~ "penta1",
        indicator_id == "ch_vac1_c_dp3"  ~ "penta3",
        indicator_id == "ch_vac1_c_op1"  ~ "polio1",
        indicator_id == "ch_vac1_c_op2"  ~ "polio2",
        indicator_id == "ch_vac1_c_op3"  ~ "polio3",
        indicator_id == "ch_vac1_c_msl"  ~ "measles1",
        indicator_id == "ch_vac1_c_ms2"  ~ "measles2",
        indicator_id == "ch_vac1_c_rt1"  ~ "rota1",
        indicator_id == "ch_vac1_c_rt2"  ~ "rota2",
        
        # Maternal health
        indicator_id == "rh_delp_c_dhf"  ~ "delivery",
        indicator_id == "rh_ancn_w_n4p"  ~ "anc4",
        indicator_id == "rh_ancn_w_n01"  ~ "anc1_old",
        indicator_id == "rh_ancp_w_skp"  ~ "anc1",
        indicator_id == "rh_pcmn_w_mot"  ~ "pnc1",
        indicator_id == "rh_ancc_w_irn"  ~ "iron",
        
        # IPTp
        indicator_id == "ml_iptp_w_spf"  ~ "iptp1",
        indicator_id == "ml_iptp_w_2sp"  ~ "iptp2",
        indicator_id == "ml_iptp_w_3sp"  ~ "iptp3",
        
        # Child health and nutrition
        indicator_id == "cn_miac_c_mmn"  ~ "mnp",
        indicator_id == "cn_miac_c_dwm"  ~ "deworm",
        indicator_id == "cn_miac_c_vas"  ~ "vita",
        indicator_id == "ch_diat_c_orz"  ~ "orszinc",
        
        # Other
        indicator_id == "fp_srcm_w_tot"  ~ "fp",
        indicator_id == "cm_pnmr_c_nsb"  ~ "still",
        indicator_id == "cm_ecmt_c_imr"  ~ "imr",
        indicator_id == "cm_ecmr_c_nnr"  ~ "nmr",
        indicator_id == "fe_frty_w_npg"  ~ "womenrepage",
        indicator_id == "fe_frtr_w_cbr"  ~ "crudebr",
        indicator_id == "fe_frtr_w_tfr"  ~ "tfr",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(indicator_common_id))
}

clean_dhs_subnational <- function(df) {
  df %>%
    filter(
      !is.na(Value),
      IsPreferred == 1,
      ByVariableLabel == "Two years preceding the survey"
    ) %>%
    mutate(
      admin_area_2 = str_remove(CharacteristicLabel, "^\\.*\\s*"),
      region_unique_id = CharacteristicId,
      indicator_id = tolower(IndicatorId),
      indicator_common_id = case_when(
        # Vaccines
        indicator_id == "ch_vac1_c_bcg"  ~ "bcg",
        indicator_id == "ch_vac1_c_dp1"  ~ "penta1",
        indicator_id == "ch_vac1_c_dp3"  ~ "penta3",
        indicator_id == "ch_vac1_c_op1"  ~ "polio1",
        indicator_id == "ch_vac1_c_op2"  ~ "polio2",
        indicator_id == "ch_vac1_c_op3"  ~ "polio3",
        indicator_id == "ch_vac1_c_msl"  ~ "measles1",
        indicator_id == "ch_vac1_c_ms2"  ~ "measles2",
        indicator_id == "ch_vac1_c_rt1"  ~ "rota1",
        indicator_id == "ch_vac1_c_rt2"  ~ "rota2",
        
        # Maternal health
        indicator_id == "rh_delp_c_dhf"  ~ "delivery",
        indicator_id == "rh_ancn_w_n4p"  ~ "anc4",
        indicator_id == "rh_ancn_w_n01"  ~ "anc1_old",
        indicator_id == "rh_ancp_w_skp"  ~ "anc1",
        indicator_id == "rh_pcmn_w_mot"  ~ "pnc1",
        indicator_id == "rh_ancc_w_irn"  ~ "iron",
        
        # IPTp
        indicator_id == "ml_iptp_w_spf"  ~ "iptp1",
        indicator_id == "ml_iptp_w_2sp"  ~ "iptp2",
        indicator_id == "ml_iptp_w_3sp"  ~ "iptp3",
        
        # Child health and nutrition
        indicator_id == "cn_miac_c_mmn"  ~ "mnp",
        indicator_id == "cn_miac_c_dwm"  ~ "deworm",
        indicator_id == "cn_miac_c_vas"  ~ "vita",
        indicator_id == "ch_diat_c_orz"  ~ "orszinc",
        
        # Other
        indicator_id == "fp_srcm_w_tot"  ~ "fp",
        indicator_id == "cm_pnmr_c_nsb"  ~ "still",
        indicator_id == "cm_ecmt_c_imr"  ~ "imr",
        indicator_id == "cm_ecmr_c_nnr"  ~ "nmr",
        indicator_id == "fe_frty_w_npg"  ~ "womenrepage",
        indicator_id == "fe_frtr_w_cbr"  ~ "crudebr",
        indicator_id == "fe_frtr_w_tfr"  ~ "tfr",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(indicator_common_id)) %>%
    transmute(
      admin_area_1 = CountryName,
      admin_area_2,
      region_unique_id,
      year = SurveyYear,
      indicator_id,
      survey_value = Value,
      source = "DHS Sub-national",
      source_detail = SurveyId,
      survey_type = "household",
      indicator_common_id
    )
}

# ----------------------------------------
#
#       MICS
#
# ----------------------------------------
clean_mics <- function(df) {
  df %>%
    filter(SEX %in% c("_T", NA)) %>%  # Keep totals (correct for all indicators)
    mutate(
      year = as.integer(TIME_PERIOD),
      value = if_else(UNIT_MULTIPLIER == "3", as.numeric(OBS_VALUE) * 1000, as.numeric(OBS_VALUE)),
      admin_area_1 = REF_AREA,
      indicator_id = INDICATOR,
      survey_value = value,
      source = "MICS",
      source_detail = DATA_SOURCE,
      survey_type = "household",
      indicator_common_id = case_when(
        # Mortality
        indicator_id == "CME_MRM0" ~ "nmr",          # Neonatal mortality rate
        indicator_id == "CME_MRY0T4" ~ "imr",        # Infant mortality rate (0-4 years)
        
        # Population estimates
        indicator_id == "DM_POP_GRT" ~ "popgrowth",  # Population growth rate
        indicator_id == "DM_POP_TOT" ~ "poptot",     # Total population
        indicator_id == "DM_POP_U5" ~ "popu5",       # Under-5 population
        
        # Immunization
        indicator_id == "IM_BCG" ~ "bcg",            # BCG vaccine
        indicator_id == "IM_HEPBB" ~ "hepb",         # Hepatitis B
        indicator_id == "IM_HPV" ~ "hpv",            # HPV vaccine
        indicator_id == "IM_HIB3" ~ "hib3",          # Hib3
        indicator_id == "IM_DTP1" ~ "penta1",        # DTP1 (Penta1)
        indicator_id == "IM_DTP3" ~ "penta3",        # DTP3 (Penta3)
        
        # Maternal and newborn care
        indicator_id == "MNCH_ANC1" ~ "anc1",        # ANC1
        indicator_id == "MNCH_ANC4" ~ "anc4",        # ANC4+
        indicator_id == "MNCH_INSTDEL" ~ "delivery", # Institutional delivery
        indicator_id == "MNCH_PNCMOM" ~ "pnc1",      # PNC (mother)
        indicator_id == "MNCH_IPTP" ~ "iptp3",       # IPTp3+ doses
        indicator_id == "MNCH_MMR" ~ "mmr",          # Maternal mortality ratio
        
        # Diarrhoea treatment
        indicator_id == "MNCH_ORS" ~ "ors",          # ORS only
        indicator_id == "MNCH_ORSZINC" ~ "orszinc",  # ORS and Zinc
        
        # Family planning and nutrition
        indicator_id == "MNCH_DEMAND_FP" ~ "fp",     # Family planning demand satisfied
        indicator_id == "NT_ANT_BAZ_NE3" ~ "iron"    # Iron supplementation (example code)
        
        # Fallback
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(survey_value), !is.na(indicator_common_id)) %>%
    select(admin_area_1, year, indicator_id, indicator_common_id,
           survey_value, source, source_detail, survey_type)
}

# ----------------------------------------
#
#       UNWPP
#
# ----------------------------------------
clean_unwpp <- function(df) {
  base <- df %>%
    filter(
      variant == "Median",
      !is.na(value)
    )
  
  # Crude birth rate
  crudebr <- base %>%
    filter(indicatorDisplayName == "Crude birth rate (births per 1,000 population)") %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "crudebr")
  
  # Total number of live births
  livebirth <- base %>%
    filter(indicatorDisplayName == "Total number of live births by sex") %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "livebirth")
  
  # Total population (all ages)
  poptot <- base %>%
    filter(indicatorDisplayName == "Total population by sex",
           sex == "Both sexes"
    ) %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "poptot")
  
  # Infant mortality rate
  imr <- base %>%
    filter(indicatorDisplayName == "Infant mortality rate (IMR)",
           sex == "Both sexes"
    ) %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "imr")
  
  # Under-five mortality rate
  u5mr <- base %>%
    filter(
      indicatorDisplayName == "Under-five mortality rate (U5MR)",
      sex == "Both sexes"
    ) %>%
    # group_by(country, iso3, timeLabel) %>%
    # summarise(
    #   value = mean(value, na.rm = TRUE),  # or use first(value) if values are identical
    #   .groups = "drop"
    # ) %>%
    mutate(
      indicator_id = "Under-five mortality rate (U5MR)",
      indicator_common_id = "u5mr"
    )
  
  
  # Female population 15–49
  womenrepage <- base %>%
    filter(indicatorDisplayName == "Female population of reproductive age (15-49 years)",
           sex %in% c("Female", NA),
           ageLabel == "15-49") %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "womenrepage")
  
  # mCPR (modern contraceptive prevalence)
  mcpr <- base %>%
    filter(indicatorDisplayName == "CP Modern",
           category == "All women") %>%
    mutate(indicator_id = indicatorDisplayName, indicator_common_id = "mcpr")
  
  # U1: age = 0
  totu1pop <- base %>%
    filter(indicatorDisplayName == "Annual population by 1-year age groups and by sex",
           ageLabel == "0",
           sex == "Both sexes") %>%
    group_by(country, iso3, timeLabel) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(indicator_id = "Annual population age 0", indicator_common_id = "totu1pop")
  
  # U5: ages 0–4
  totu5pop <- base %>%
    filter(indicatorDisplayName == "Annual population by 1-year age groups and by sex",
           ageLabel %in% as.character(0:4),
           sex == "Both sexes") %>%
    group_by(country, iso3, timeLabel) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(indicator_id = "Annual population age 0-4", indicator_common_id = "totu5pop")
  
  # Combine all
  bind_rows(crudebr, livebirth, poptot, imr, u5mr, womenrepage, mcpr, totu1pop, totu5pop) %>%
    mutate(
      admin_area_1 = countrycode(iso3, "iso3c", "country.name"),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      value_type = "number",
      source = "UNWPP",
      source_detail = "UNWPP",
      survey_type = "modeled"
    ) %>%
    select(
      admin_area_1, admin_area_2, year,
      indicator_id, indicator_common_id,
      survey_value = value, value_type,
      source, source_detail, survey_type
    )
}

# ----------------------------------------
# Clean and standardize survey datasets
# ----------------------------------------

dhs_national_tidy <- clean_dhs_national(dhs_national) %>%
  standardize_admin_area_1()

dhs_subnational_tidy <- clean_dhs_subnational(dhs_subnational) %>%
  standardize_admin_area_1()

mics_tidy <- clean_mics(mics_data) %>%
  filter(admin_area_1 %in% c("AFG", "BGD", "CMR", "COD", "ETH", "GHA", "GIN", "GUM", "HTI", 
                             "KEN", "LBN", "MDG", "MLI", "MWI", "NGA", "SEN", "SLE", "TCD", 
                             "TJK", "UGA", "ZMB", "MDA", "LBR", "SOM")) %>%
  mutate(admin_area_1 = countrycode(admin_area_1, origin = "iso3c", destination = "country.name")) %>%
  standardize_admin_area_1()

# Clean UNWPP data
unwpp_tidy <- clean_unwpp(unwpp_raw) %>%
  standardize_admin_area_1()


# ----------------------------------------
# Final: Unified dataset with indicator_type
# ----------------------------------------

all_data <- bind_rows(
  dhs_national_tidy,
  dhs_subnational_tidy,
  mics_tidy,
  unwpp_tidy
) %>%
  mutate(
    admin_area_2 = ifelse(is.na(admin_area_2), "NATIONAL", admin_area_2)
  )

# Indicator classification
percent_indicators <- c(
  "anc1", "anc4", "delivery", "anc1_old", "fp",
  "penta1", "penta3", "bcg", "polio1", "polio2", "polio3", "pnc1",
  "measles1", "measles2", "rota1", "rota2"
)

number_indicators <- c("still")

rate_indicators <- c("imr", "nmr", "tfr", "crudebr")

population_estimate_indicators <- c(
  "poptot", "popu5", "totu1pop", "totu5pop", "livebirth", "womenrepage"
)

# Label types
all_data_labeled <- all_data %>%
  mutate(
    indicator_type = case_when(
      indicator_common_id %in% percent_indicators ~ "percent",
      indicator_common_id %in% number_indicators ~ "number",
      indicator_common_id %in% rate_indicators ~ "rate",
      indicator_common_id == "womenrepage" & source == "DHS National" ~ "survey_count",
      indicator_common_id %in% population_estimate_indicators ~ "population_estimate",
      TRUE ~ NA_character_
    ),
    survey_value = if_else(indicator_type == "percent", survey_value / 100, survey_value)
  ) %>%
  filter(!is.na(indicator_type)) %>%
  select(
    admin_area_1, admin_area_2, year,
    indicator_id, indicator_common_id,
    indicator_type,
    survey_value, source, source_detail, survey_type
  )


# ----------------------------------------
# Separate outputs
# ----------------------------------------

# Keep only MICS + UNWPP population estimates
population_estimates_only <- all_data_labeled %>%
  filter(indicator_type == "population_estimate" & source %in% c("UNWPP", "MICS"))

# All other survey outputs (coverage, rates, counts, etc.)
survey_data_only <- all_data_labeled %>%
  filter(!(indicator_type == "population_estimate" & source %in% c("UNWPP", "MICS")))

# ----------------------------------------
# Write outputs (with fwrite for efficiency)
# ----------------------------------------
# Step 1: Read your manually updated Nigeria backup
nigeria_only <- read.csv("survey_data_nigeria_backup.csv")

missing_nigeria <- anti_join(nigeria_only, survey_data_only,
                             by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id"))
if (nrow(missing_nigeria) > 0) {
  survey_data_only <- bind_rows(survey_data_only, missing_nigeria)
}


ghana_only <- read.csv("survey_data_ghana_backup.csv")

missing_ghana <- anti_join(ghana_only, survey_data_only,
                           by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id"))

if (nrow(missing_ghana) > 0) {
  survey_data_only <- bind_rows(survey_data_only, missing_ghana)
}



 
# Write the outputs...
fwrite(survey_data_only, "survey_data_unified.csv")
fwrite(population_estimates_only, "population_estimates_only.csv")
