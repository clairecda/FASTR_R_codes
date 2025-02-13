SELECTEDCOUNT <- "count_final_both"  # Change as needed

#-------------------------------------------------------------------------------------------------------------
# CB - R code FASTR PROJECT
# Last edit: 2025 Feb 10
# Module: SERVICE UTILIZATION

# This script analyzes disruptions in essential health services using adjusted HMIS data (M2_adjusted_data.csv).
# It runs regressions at (1) the indicator level, (2) indicator × province level, and (3) the indicator × district level.

# Ce code analyse les perturbations des services de santé essentiels à partir des données HMIS ajustées.
# Il exécute des régressions pour chaque indicateur aux niveaux national, régional et au niveau des districts sanitaires.
# ------------------------------------- KEY OUTPUTS ----------------------------------------------------------
# FILE: M3_service_utilization.csv             # Adjusted volumes aggregated at the lowest geo level
# FILE: M3_disruptions_analysis.csv            # Outputs from the disruption analysis


#-------------------------------------------------------------------------------------------------------------
# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(fixest)   # For panel regressions (alternative to 'xtreg' in Stata)
library(stringr)


# Step 1: Load M2 Adjusted Data ------------------------------------------------------------------------------
print("Loading adjusted data...")

data <- read.csv("M2_adjusted_data_admin_area.csv")        # adjusted data aggregated at the lowest geo level, used for visualization
data_disruption <- read.csv("M2_adjusted_data.csv")        # facility-level adjusted data - used in disruption analysis


# Process data: Convert date and drop unwanted columns
data_disruption <- data_disruption %>%
  mutate(
    year = as.integer(substr(period_id, 1, 4)),  
    month = as.integer(substr(period_id, 5, 6)), 
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  select(-setdiff(names(.)[grepl("^count_final_", names(.))], SELECTEDCOUNT))  # Drop unwanted columns

# Get list of unique indicators and districts
indicators <- unique(data_disruption$indicator_common_id)
districts <- unique(data_disruption$admin_area_3)
provinces <- unique(data_disruption$admin_area_2)

# Step 2: Identify Pandemic Months -----------------------------------------------
print("Identifying pandemic months...")
pandemic_start <- as.Date("2020-03-01")  
pandemic_end <- as.Date("2021-12-01")    

# Generate pandemic dummy variables
data_disruption <- data_disruption %>%
  mutate(tagged = ifelse(date >= pandemic_start & date <= pandemic_end, 1, 0))

# Step 3: Set Up Panel Data -----------------------------------------------------
print("Setting up panel data...")

# Identify the lowest available geographic level for clustering
geo_cols <- colnames(data_disruption) %>% str_subset("^admin_area_[0-9]+$")
lowest_geo_level <- if (length(geo_cols) > 0) {
  geo_cols[which.max(as.numeric(str_extract(geo_cols, "[0-9]+")))]
} else {
  stop("No geographic levels detected in the dataset!")
}
print(paste("Using geographic level for clustering:", lowest_geo_level))


# Create empty lists to store results
results_list <- list()                #Step 4a
district_results_list <- list()       #Step 4b
province_results_list <- list()       #Step 4c

# Step 4a: Run Regression for Each Indicator -----------------------------------
print("Running regressions at the indicator level...")

for (indicator in indicators) {
  print(paste("Processing:", indicator))
  
  indicator_data <- data_disruption %>%
    filter(indicator_common_id == indicator) %>%
    drop_na(!!sym(SELECTEDCOUNT))
  
  if (nrow(indicator_data) == 0) {
    print(paste("Skipping", indicator, "- insufficient data"))
    next
  }
  
  model <- feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), 
                 data = indicator_data, 
                 cluster = as.formula(paste0("~", lowest_geo_level)))
  
  indicator_data <- indicator_data %>%
    mutate(expect_admin_area_1 = predict(model, newdata = indicator_data))
  
  for (pmonth in unique(indicator_data$date[indicator_data$tagged == 1])) {
    coeff <- coef(model)["tagged"]
    indicator_data <- indicator_data %>%
      mutate(expect_admin_area_1 = ifelse(date == pmonth, expect_admin_area_1 - coeff, expect_admin_area_1))
  }
  
  indicator_data <- indicator_data %>%
    mutate(
      b_admin_area_1 = ifelse(tagged == 1, -1 * (mean(expect_admin_area_1 - !!sym(SELECTEDCOUNT), na.rm = TRUE) / mean(expect_admin_area_1, na.rm = TRUE)), NA_real_),
      b_trend_admin_area_1 = coef(model)["date"]
    ) %>%
    select(facility_id, date, indicator_common_id, expect_admin_area_1, b_admin_area_1, b_trend_admin_area_1)
  
  results_list[[indicator]] <- indicator_data
}

results_long <- bind_rows(results_list)

# Merge national-level results into main dataset
data_disruption <- data_disruption %>%
  left_join(results_long,
            by = c("facility_id", "date", "indicator_common_id"))



# # Step 4b: Run Regression for Each Indicator × District ------------------------
# print("Running regressions at the district level...")
# 
# for (indicator in indicators) {
#   for (district in districts) {
#     print(paste("Processing:", indicator, "in district:", district))
#     
#     district_data <- data_disruption %>%
#       filter(indicator_common_id == indicator, admin_area_3 == district) %>%
#       drop_na(!!sym(SELECTEDCOUNT))
#     
#     # Ensure enough data before running regression
#     if (nrow(district_data) < 30 || length(unique(district_data[[SELECTEDCOUNT]])) < 2) {
#       print(paste("Skipping:", indicator, "in", district, "- insufficient variation in data"))
#       next
#     }
#     
#     # Run district-level regression dynamically with error handling
#     model_district <- tryCatch(
#       feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), 
#             data = district_data, 
#             cluster = as.formula(paste0("~", lowest_geo_level))),
#       error = function(e) NULL
#     )
#     
#     # Check if model is valid before proceeding
#     if (is.null(model_district) || anyNA(coef(model_district)) || anyNA(vcov(model_district))) {
#       print(paste("Skipping district-level regression for:", indicator, "in", district, "- model issue"))
#       next
#     }
#     
#     # Compute expected values with NA handling
#     district_data <- district_data %>%
#       mutate(expect_district = ifelse(is.na(predict(model_district, newdata = district_data)), NA_real_, predict(model_district, newdata = district_data))) %>%
#       mutate(
#         b_district = ifelse(tagged == 1, -1 * (mean(expect_district - !!sym(SELECTEDCOUNT), na.rm = TRUE) / mean(expect_district, na.rm = TRUE)), NA_real_),
#         b_trend_district = coef(model_district)["date"]
#       ) %>%
#       select(facility_id, date, indicator_common_id, admin_area_3, expect_district, b_district, b_trend_district)  
#     
#     district_results_list[[paste(indicator, district, sep = "_")]] <- district_data
#   }
# }
# 
# 
# # Combine district results
# district_results_long <- bind_rows(district_results_list)
# 
# # Merge district-level results into main dataset using admin_area_3
# data_disruption <- data_disruption %>%
#   left_join(district_results_long, 
#             by = c("facility_id", "date", "indicator_common_id", "admin_area_3"), 
#             suffix = c("", "_admin_area_3"))  # Prevents column name conflicts
# 
# print("District-level regression analysis complete!")


# Step 4c: Run Regression for Each Indicator × Province ------------------------
print("Running regressions at the province level...")
for (indicator in indicators) {
  for (province in provinces) {
    print(paste("Processing:", indicator, "in province:", province))
    
    province_data <- data_disruption %>%
      filter(indicator_common_id == indicator, admin_area_2 == province) %>%
      drop_na(!!sym(SELECTEDCOUNT))
    
    if (nrow(province_data) < 20) {
      print(paste("Skipping", indicator, "in", province, "- insufficient data"))
      next
    }
    
    # Run province-level regression dynamically
    model_province <- tryCatch(
      feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), 
            data = province_data, 
            cluster = as.formula(paste0("~", lowest_geo_level))),
      error = function(e) NULL
    )
    
    if (!is.null(model_province) && !anyNA(coef(model_province))) {
      province_data <- province_data %>%
        mutate(expect_admin_area_2 = predict(model_province, newdata = province_data)) %>%
        mutate(
          b_admin_area_2 = ifelse(tagged == 1, -1 * (mean(expect_admin_area_2 - !!sym(SELECTEDCOUNT), na.rm = TRUE) / mean(expect_admin_area_2, na.rm = TRUE)), NA_real_),
          b_trend_admin_area_2 = coef(model_province)["date"]
        ) %>%
        select(facility_id, date, indicator_common_id, admin_area_2, expect_admin_area_2, b_admin_area_2, b_trend_admin_area_2)
      
      province_results_list[[paste(indicator, province, sep = "_")]] <- province_data
    }
  }
}

# Combine province results
province_results_long <- bind_rows(province_results_list)

# Merge province-level results into main dataset using admin_area_2
data_disruption <- data_disruption %>%
  left_join(province_results_long, 
            by = c("facility_id", "date", "indicator_common_id", "admin_area_2"), 
            suffix = c("", "_admin_area_2"))  # Prevents column name conflicts


print("Province-level regression analysis complete!")


# Step 5: Save Output ----------------------------------------------------------
print("Saving results...")
write.csv(data, "M3_service_utilization.csv", row.names = FALSE)
write.csv(data_disruption, "M3_disruptions_analysis.csv", row.names = FALSE)

print("Analysis complete!")