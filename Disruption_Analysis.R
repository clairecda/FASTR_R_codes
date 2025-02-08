# CB - R code FASTR PROJECT
# WIP - DISRUPTION ANALYSIS
# Last edit: 2025 Feb 8

# This script analyzes disruptions in essential health services using adjusted HMIS data (M2_adjusted_data.csv).
# It runs regressions at both the indicator and indicator × district levels.

# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(fixest)   # For panel regressions (alternative to xtreg in Stata)
library(stringr)

# Step 1: Load M2 Adjusted Data ------------------------------------------------
print("Loading adjusted data...")
data <- read.csv("M2_adjusted_data.csv")

# Convert date column (assuming 'period_id' is in YYYYMM format)
data <- data %>%
  mutate(
    year = as.integer(substr(period_id, 1, 4)),  
    month = as.integer(substr(period_id, 5, 6)), 
    date = as.Date(paste(year, month, "01", sep = "-")) 
  )

# Step 2: Identify Pandemic Months -----------------------------------------------
print("Identifying pandemic months...")
pandemic_start <- as.Date("2020-03-01")  
pandemic_end <- as.Date("2021-12-01")    

# Generate pandemic dummy variables
data <- data %>%
  mutate(tagged = ifelse(date >= pandemic_start & date <= pandemic_end, 1, 0))

# Step 3: Set Up Panel Data -----------------------------------------------------
print("Setting up panel data...")
# Identify the lowest available geographic level for clustering
geo_cols <- colnames(data) %>% str_subset("^admin_area_[0-9]+$")
lowest_geo_level <- if (length(geo_cols) > 0) {
  geo_cols[which.max(as.numeric(str_extract(geo_cols, "[0-9]+")))]
} else {
  stop("No geographic levels detected in the dataset!")
}
print(paste("Using geographic level for clustering:", lowest_geo_level))

# Get list of unique indicators and districts
indicators <- unique(data$indicator_common_id)
districts <- unique(data$admin_area_2)  # Assuming `admin_area_2` is the district level

# Create empty lists to store results
results_list <- list()
district_results_list <- list()

# Step 4a: Run Regression for Each Indicator -----------------------------------
print("Running regressions at the indicator level...")

for (indicator in indicators) {
  print(paste("Processing:", indicator))
  
  indicator_data <- data %>%
    filter(indicator_common_id == indicator) %>%
    drop_na(count_final_both)
  
  if (nrow(indicator_data) == 0) {
    print(paste("Skipping", indicator, "- insufficient data"))
    next
  }
  
  model <- feols(count_final_both ~ date + factor(month) + tagged, 
                 data = indicator_data, 
                 cluster = as.formula(paste0("~", lowest_geo_level)))
  
  indicator_data <- indicator_data %>%
    mutate(expect_national = predict(model, newdata = indicator_data))
  
  for (pmonth in unique(indicator_data$date[indicator_data$tagged == 1])) {
    coeff <- coef(model)["tagged"]
    indicator_data <- indicator_data %>%
      mutate(expect_national = ifelse(date == pmonth, expect_national - coeff, expect_national))
  }
  
  indicator_data <- indicator_data %>%
    mutate(
      b_national = ifelse(tagged == 1, -1 * (mean(expect_national - count_final_both, na.rm = TRUE) / mean(expect_national, na.rm = TRUE)), NA_real_),
      b_trend_national = coef(model)["date"]
    ) %>%
    select(facility_id, date, indicator_common_id, expect_national, b_national, b_trend_national)
  
  results_list[[indicator]] <- indicator_data
}

results_long <- bind_rows(results_list)

# Merge national-level results into main dataset
data <- data %>%
  left_join(results_long, by = c("facility_id", "date", "indicator_common_id"))

# Step 4b: Run Regression for Each Indicator × District ------------------------
print("Running regressions at the district level...")

for (indicator in indicators) {
  for (district in districts) {
    print(paste("Processing:", indicator, "in district:", district))
    
    district_data <- data %>%
      filter(indicator_common_id == indicator, admin_area_2 == district) %>%
      drop_na(count_final_both)
    
    if (nrow(district_data) < 20) {
      print(paste("Skipping", indicator, "in", district, "- insufficient data"))
      next
    }
    
    model <- feols(count_final_both ~ date + factor(month) + tagged, 
                   data = district_data, 
                   cluster = as.formula(paste0("~", lowest_geo_level)))
    
    district_data <- district_data %>%
      mutate(expect_district = predict(model, newdata = district_data))
    
    for (pmonth in unique(district_data$date[district_data$tagged == 1])) {
      coeff <- coef(model)["tagged"]
      district_data <- district_data %>%
        mutate(expect_district = ifelse(date == pmonth, expect_district - coeff, expect_district))
    }
    
    district_data <- district_data %>%
      mutate(
        b_district = ifelse(tagged == 1, -1 * (mean(expect_district - count_final_both, na.rm = TRUE) / mean(expect_district, na.rm = TRUE)), NA_real_),
        b_trend_district = coef(model)["date"]
      ) %>%
      select(facility_id, date, indicator_common_id, admin_area_2, expect_district, b_district, b_trend_district)
    
    district_results_list[[paste(indicator, district, sep = "_")]] <- district_data
  }
}

district_results_long <- bind_rows(district_results_list)

# Merge district-level results into main dataset with suffixes
data <- data %>%
  left_join(district_results_long, by = c("facility_id", "date", "indicator_common_id", "admin_area_2"))

print("Disrtict-level regression analysis complete!")

# Step 4c: Run Regression for Each Indicator × Province ------------------------
# Store results for province-level regressions
province_results_list <- list()

# Get unique provinces (admin_area_2)
provinces <- unique(data$admin_area_2)

print("Running regressions at the province level...")

for (indicator in indicators) {
  for (province in provinces) {
    print(paste("Processing:", indicator, "in province:", province))
    
    province_data <- data %>%
      filter(indicator_common_id == indicator, admin_area_2 == province) %>%
      drop_na(count_final_both)
    
    if (nrow(province_data) < 20) {
      print(paste("Skipping", indicator, "in", province, "- insufficient data"))
      next
    }
    
    # Run province-level regression
    model_province <- tryCatch(
      feols(count_final_both ~ date + factor(month) + tagged, 
            data = province_data, 
            cluster = as.formula(paste0("~", lowest_geo_level))),
      error = function(e) NULL
    )
    
    if (!is.null(model_province) && !anyNA(coef(model_province))) {
      province_data <- province_data %>%
        mutate(expect_province = predict(model_province, newdata = province_data)) %>%
        mutate(
          b_province = ifelse(tagged == 1, -1 * (mean(expect_province - count_final_both, na.rm = TRUE) / mean(expect_province, na.rm = TRUE)), NA_real_),
          b_trend_province = coef(model_province)["date"]
        ) %>%
        select(facility_id, date, indicator_common_id, admin_area_1, expect_province, b_province, b_trend_province)
      
      province_results_list[[paste(indicator, province, sep = "_")]] <- province_data
    }
  }
}

# Combine province results
province_results_long <- bind_rows(province_results_list)

# Merge back into the main dataset, ensuring province-level results have clear suffixes
data <- data %>%
  left_join(province_results_long, by = c("facility_id", "date", "indicator_common_id", "admin_area_1"))

print("Province-level regression analysis complete!")



# Step 5: Save Output ----------------------------------------------------------
print("Saving results...")
write.csv(data, "M3_disruptions_analysis.csv", row.names = FALSE)
print("Analysis complete!")
