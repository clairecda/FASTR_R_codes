# Author: cb
# Last updated: 22 April 2025


# FETCH SURVEY DATA FOR COVERAGE ANALYSIS
# FETCH FROM DHS AND MICS
# FETCH POPULATION DATA FROM UNWPP AND WB

readRenviron("~/Desktop/FASTR/R codes/.Renviron")  


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
dhs_indicators <- c(
  "RH_DELP_C_DHF",  # Institutional delivery
  "RH_ANCP_W_SKP",  # Skilled delivery
  "RH_ANCN_W_N4P",  # ANC4
  "FP_SRCM_W_TOT",  # mCPR
  "CH_VAC1_C_DP1",  # Penta1
  "CH_VAC1_C_DP3",  # Penta3
  "CH_VAC1_C_BCG",  # BCG
  "CM_PNMR_C_NSB",  # Stillbirth rate
  "CM_ECMT_C_IMR",  # IMR
  "CM_ECMR_C_NNR",  # NMR
  "FE_FRTY_W_NPG",  # Women of reproductive age
  "FE_FRTR_W_CBR",  # Crude birth rate
  "FE_FRTR_W_TFR"   # Total fertility rate
)

# ----------------------------------------
# Set DHS country ISO codes
# ----------------------------------------
dhs_countries <- c(
  "AF", "BD", "GH", "LB", "CM", "CD", "ET", "GN", "HT", "MD",
  "KE", "MW", "ML", "NG", "SN", "SL", "TD", "UG", "ZM", "GU", "TJ"
)

# ----------------------------------------
# Pull DHS data via API
# ----------------------------------------
dhs_national <- dhs_data(
  indicatorIds = dhs_indicators,
  countryIds = dhs_countries,
  breakdown = "national",
  f = "json"
)

dhs_subnational <- dhs_data(
  indicatorIds = dhs_indicators,
  countryIds = dhs_countries,
  breakdown = "subnational",
  f = "json"
)

# ----------------------------------------
# Pull MICS data from SDMX API
# ----------------------------------------
mics_indicators <- c(
  "CME_MRM0", "CME_MRY0T4", "DM_POP_GRT", "DM_POP_TOT", "DM_POP_U5",
  "IM_BCG", "IM_HEPBB", "IM_HPV", "IM_HIB3", "IM_DTP1", "IM_DTP3",
  "MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_IPTP", "MNCH_MMR",
  "MNCH_PNCMOM", "NT_ANT_BAZ_NE3"
)

mics_url <- paste0(
  "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GLOBAL_DATAFLOW,1.0/.",
  paste(mics_indicators, collapse = "+"),
  "..?startPeriod=2010&endPeriod=2024"
)
mics_sdmx <- readSDMX(mics_url)
mics_data <- as.data.frame(mics_sdmx)

# # ----------------------------------------
# # Pull World Bank WPP data via API
# # ----------------------------------------
# wb_indicators <- c("SP.POP.TOTL", "SP.DYN.CBRT.IN")
# wb_countries <- c("AF", "BD", "GH", "LB", "CM", "CD", "ET", "GN", "HT", "MD",
#                   "KE", "MW", "ML", "NG", "SN", "SL", "TD", "UG", "ZM", "GU", "TJ")
# 
# fetch_wb_data <- function(indicator, countries, start_year, end_year) {
#   url <- paste0(
#     "https://api.worldbank.org/v2/country/",
#     paste(countries, collapse = ";"),
#     "/indicator/", indicator,
#     "?date=", start_year, ":", end_year,
#     "&format=json&per_page=10000"
#   )
#   res <- GET(url)
#   json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
#   if (length(json) < 2) return(NULL)
#   json[[2]]
# }
# 
# wb_data_list <- lapply(wb_indicators, function(ind) {
#   fetch_wb_data(ind, wb_countries, 2010, 2024)
# })
# names(wb_data_list) <- wb_indicators

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
                 "ZM", "TJ")

target_locations <- locations_df %>%
  filter(iso2 %in% target_iso2) %>%
  select(id, iso2, iso3, name)

# --- List of indicator IDs and short names ---
indicator_map <- tibble::tibble(
  indicator_id = c(2, 22, 24, 41, 46, 47, 49, 55),
  short = c("CPModP", "IMR", "U5MR", "womenrepage", "totu5pop", "totu1pop", "totpop", "CBR")
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
clean_dhs_national <- function(df) {
  df %>%
    filter(!is.na(Value)) %>%
    transmute(
      admin_area_1 = CountryName,
      admin_area_2 = 'NATIONAL',
      disaggregation = CharacteristicLabel,
      year = SurveyYear,
      indicator_id = tolower(IndicatorId),
      survey_value = Value,
      source = "DHS National",
      source_detail = SurveyId,
      survey_type = "household",
      indicator_common_id = case_when(
        indicator_id == "ch_vac1_c_bcg"  ~ "bcg",
        indicator_id == "ch_vac1_c_dp1"  ~ "penta1",
        indicator_id == "ch_vac1_c_dp3"  ~ "penta3",
        indicator_id == "rh_delp_c_dhf"  ~ "delivery",
        indicator_id == "rh_ancn_w_n4p"  ~ "anc4",
        indicator_id == "rh_ancp_w_skp"  ~ "anc1",
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

recall_order <- c("Two years preceding the survey", "Three years preceding the survey", "Five years preceding the survey", "Ten years preceding the survey", "")

clean_dhs_subnational <- function(df) {
  df %>%
    filter(!is.na(Value)) %>%
    mutate(
      ByVariableLabel = factor(ByVariableLabel, levels = recall_order),
      admin_area_2 = str_remove(CharacteristicLabel, "^\\.*\\s*"),
      region_unique_id = CharacteristicId,
      indicator_id = tolower(IndicatorId),
      indicator_common_id = case_when(
        indicator_id == "ch_vac1_c_bcg"  ~ "bcg",
        indicator_id == "ch_vac1_c_dp1"  ~ "penta1",
        indicator_id == "ch_vac1_c_dp3"  ~ "penta3",
        indicator_id == "rh_delp_c_dhf"  ~ "delivery",
        indicator_id == "rh_ancn_w_n4p"  ~ "anc4",
        indicator_id == "rh_ancp_w_skp"  ~ "anc1",
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
    group_by(CountryName, admin_area_2, SurveyYear, indicator_common_id) %>%
    arrange(ByVariableLabel) %>%
    slice(1) %>%
    ungroup() %>%
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

clean_mics <- function(df) {
  df %>%
    filter(SEX != "M") %>%
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
        indicator_id == "CME_MRM0" ~ "nmr",
        indicator_id == "CME_MRY0" ~ "imr",
        indicator_id == "CME_SBR" ~ "still",
        indicator_id == "IM_DTP1" ~ "penta1",
        indicator_id == "IM_DTP3" ~ "penta3",
        indicator_id == "MNCH_INSTDEL" ~ "delivery",
        indicator_id == "MNCH_DEMAND_FP" ~ "fp",
        indicator_id == "MNCH_ANC4" ~ "anc4",
        indicator_id == "MNCH_ANC1" ~ "anc1",
        indicator_id == "IM_BCG" ~ "bcg",
        indicator_id == "MNCH_PNCMOM" ~ "pnc1",
        indicator_id == "DM_POP_GRT" ~ "popgrowth",
        indicator_id == "DM_POP_TOT" ~ "poptot",
        indicator_id == "DM_POP_U5" ~ "popu5",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(survey_value), !is.na(indicator_common_id)) %>%
    select(admin_area_1, year, indicator_id, indicator_common_id, survey_value, source, source_detail, survey_type)
}

clean_unwpp <- function(df) {
  df %>%
    filter(
      !is.na(value),
      variant == "Median"  # TBD - Should we keep the median variant - more??
    ) %>%
    mutate(
      indicator_id = indicatorDisplayName,
      indicator_common_id = case_when(
        indicator_id == "Crude birth rate (births per 1,000 population)" ~ "crudebr",
        indicator_id == "Total number of live births by sex" ~ "livebirth",
        indicator_id == "Annual population by 5-year age groups and by sex" ~ "totu5pop",
        indicator_id == "Annual population by 1-year age groups and by sex" ~ "totu1pop",
        indicator_id == "Total population by sex" ~ "totpop",
        indicator_id == "Infant mortality rate (IMR)" ~ "imr",
        indicator_id == "Under-five mortality rate (U5MR)" ~ "u5mr",
        indicator_id == "Female population of reproductive age (15-49 years)" ~ "womenrepage",
        indicator_id == "CP Modern" ~ "mcpr",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(indicator_common_id)) %>%
    transmute(
      admin_area_1 = countrycode(iso3, origin = "iso3c", destination = "country.name"),
      year = as.integer(timeLabel),
      indicator_id,
      indicator_common_id,
      survey_value = value,
      sex,
      age = ageLabel,
      source = "UNWPP",
      source_detail = source,
      survey_type = "modeled"
    )
}

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
# Clean and standardize survey datasets
# ----------------------------------------

dhs_national_tidy <- clean_dhs_national(dhs_national) %>%
  standardize_admin_area_1()

dhs_subnational_tidy <- clean_dhs_subnational(dhs_subnational) %>%
  standardize_admin_area_1()

mics_tidy <- clean_mics(mics_data) %>%
  filter(admin_area_1 %in% c("AFG", "BDG", "CMR", "COD", "ETH", "GHA", "GIN", "GUM", "HTI", 
                             "KEN", "LBN", "MDG", "MLI", "MWI", "NGA", "SEN", "SLE", "TCD", 
                             "TJK", "UGA", "ZMB")) %>%
  mutate(admin_area_1 = countrycode(admin_area_1, origin = "iso3c", destination = "country.name")) %>%
  standardize_admin_area_1()

# Clean UNWPP data
unwpp_tidy <- clean_unwpp(unwpp_raw) %>%
  standardize_admin_area_1()

# # Clean World Bank WPP data
# wpp_pop_total <- clean_wpp(wb_data_list[["SP.POP.TOTL"]], "SP.POP.TOTL", "poptot")
# wpp_cbr <- clean_wpp(wb_data_list[["SP.DYN.CBRT.IN"]], "SP.DYN.CBRT.IN", "crudebr")
# ----------------------------------------
# Final bind and selection
# ----------------------------------------
all_survey <- bind_rows(
  dhs_national_tidy,
  dhs_subnational_tidy,
  mics_tidy
) %>%
  select(
    admin_area_1, admin_area_2, year,
    indicator_common_id, survey_value,
    source, source_detail, survey_type
  )



# ----------------------------------------
# Write outputs with fwrite (faster)
# ----------------------------------------
fwrite(unwpp_tidy, "unwpp_tidy.csv")
fwrite(unwpp_raw, "unwpp_raw.csv")
fwrite(all_survey, "all_survey.csv")
