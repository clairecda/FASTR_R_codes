# Load Required Libraries -----------------------------------------------------
library(tidyverse)
library(scales)

# PART 0: MODULE SETUP --------------------------------------------------------
# Step 1: Load Data -----------------------------------------------------------
print("Step 1: Loading data...")
data <- read.csv("/Users/claireboulange/Desktop/FASTR/Data received/example_imported_data_bangladesh.csv")
geo_cols <- colnames(data)[grepl("^admin_area_", colnames(data))] #detect admin_area_* columns dynamically
print(head(data))
# Define visualisation colour palette
viz_colors <- c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")

# Step 2: Create Date Column --------------------------------------------------
print("Step 2: Creating date column from year and month...")
data <- data %>%
  mutate(date = ymd(paste(year, month, "1", sep = "-")))

# PART 1: OUTLIER DETECTION ---------------------------------------------------
# Step 3: Calculate Median Volume ---------------------------------------------
print("Step 3: Calculating median volume per facility and indicator...")
data <- data %>%
  group_by(facility_id, indicator_common_id) %>%
  mutate(median_volume = median(count, na.rm = TRUE)) %>%
  ungroup()

# Step 4: Calculate MAD and Identify Outliers ---------------------------------
print("Step 4: Calculating MAD and identifying outliers...")
data <- data %>%
  group_by(facility_id, indicator_common_id) %>%
  mutate(
    mad_volume = ifelse(!is.na(count), mad(count[count >= median_volume], na.rm = TRUE), NA),
    mad_residual = ifelse(!is.na(mad_volume) & mad_volume > 0, abs(count - median_volume) / mad_volume, NA),
    moderate = ifelse(!is.na(mad_residual) & mad_residual > 10 & mad_residual <= 20, 1, 0),
    severe = ifelse(!is.na(mad_residual) & mad_residual > 20, 1, 0),
    outlier_mad = ifelse(moderate == 1 | severe == 1, 1, 0)
  ) %>%
  ungroup()

# Step 5: Calculate Proportional Contribution ---------------------------------
print("Step 5: Calculating proportional contribution and flagging outliers...")
data <- data %>%
  group_by(facility_id, indicator_common_id, year) %>%
  mutate(
    pc = count / sum(count, na.rm = TRUE),
    outlier_pc = ifelse(!is.na(pc) & pc > 0.8, 1, 0)
  ) %>%
  ungroup()

# Step 6: Combine Outlier Flags -----------------------------------------------
print("Step 6: Combining outlier flags...")
data <- data %>%
  mutate(outlier = ifelse((outlier_mad == 1 | outlier_pc == 1) & count > 100, 1, 0))

# Step 7: Adjust Outliers -----------------------------------------------------
print("Step 7: Adjusting outliers using the median of non-outliers...")
data <- data %>%
  group_by(across(all_of(c(geo_cols, "indicator_common_id")))) %>%
  mutate(
    count_adjust = ifelse(outlier == 1, median(count, na.rm = TRUE), count),
    volIM = mean(count[!outlier], na.rm = TRUE)
  ) %>%
  ungroup()

# Step 8: Add Rolling Year ----------------------------------------------------
print("Step 8: Adding rolling year variable...")
data <- data %>%
  mutate(
    rollingyear = floor((max(data$date, na.rm = TRUE) - date) / 365.25)
  ) %>%
  select(any_of(c(geo_cols, "facility_id", "indicator_common_id", "date", "year", "month", 
                  "count", "median_volume", "mad_volume", "mad_residual", 
                  "outlier", "count_adjust", "volIM", "rollingyear")))

# Step 9: Generate Dataset for Reporting Top Outliers -------------------------
print("Step 9: Generating dataset for reporting top outliers...")

top_outliers_data <- data %>%
  filter(outlier == 1) %>%
  mutate(percent_change = 100 * (count - count_adjust) / count_adjust) %>%
  group_by(indicator_common_id, facility_id) %>%
  summarise(
    max_percent_change = max(percent_change, na.rm = TRUE),
    mean_volIM = mean(volIM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(indicator_common_id) %>%
  slice_max(order_by = max_percent_change, n = 5, with_ties = FALSE) %>%
  ungroup()


# PART 1 - VISUALISATION -------------------------------------------------------
# Heatmap - % Change Increase Due to Outliers ------------------------
print("Creating heatmap for % change due to outliers...")
# Create heatmap data dynamically with geographic flexibility
heatmap_data <- data %>%
  group_by(across(all_of(geo_cols)), indicator_common_id) %>%  # Group by dynamic geographic columns and indicator
  summarise(
    total_volume = sum(count, na.rm = TRUE),
    adjusted_volume = sum(count_adjust, na.rm = TRUE),
    percent_change = 100 * (total_volume - adjusted_volume) / adjusted_volume,
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = all_of(geo_cols),         # Use all detected geographic columns as IDs
    names_from = indicator_common_id,  # Create columns for each indicator
    values_from = percent_change,      # Values for percent change
    values_fill = 0                    # Fill missing values with 0
  ) %>%
  mutate(avg_indicators = rowMeans(select(., -all_of(geo_cols)), na.rm = TRUE)) %>%  # Calculate averages dynamically
  select(all_of(geo_cols), avg_indicators, everything())  # Reorder columns to place geographic columns first


# Create the heatmap
heatmap_plot <- heatmap_data %>%
  pivot_longer(
    cols = -all_of(geo_cols), # Exclude all geographic columns dynamically
    names_to = "indicator", 
    values_to = "percent_change"
  ) %>%
  mutate(
    indicator = factor(indicator, levels = c("avg_indicators", sort(setdiff(unique(indicator), "avg_indicators"))))
  ) %>%
  ggplot(aes(x = indicator, y = admin_area_2, fill = percent_change)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#1a9641", mid = "#ffffbf", high = "#d7191c",
    midpoint = 0, na.value = "grey50"
  ) +
  labs(
    title = "Percent Change of Volume Due to Outliers",
    x = "Indicator",
    y = "Administrative Area",
    fill = "% Change"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid = element_blank() # Remove gridlines
  )

# Bar Chart - Volume Increase Due to Outliers ------------------------
print("Creating bar chart for volume increase due to outliers...")
volume_increase_data <- data %>%
  group_by(admin_area_1, indicator_common_id, month = floor_date(date, "month")) %>%
  summarise(
    total_volume = sum(count, na.rm = TRUE),
    adjusted_volume = sum(count_adjust, na.rm = TRUE),
    percent_change = 100 * (total_volume - adjusted_volume) / adjusted_volume,
    .groups = "drop"
  )

bar_chart <- ggplot(volume_increase_data, aes(x = month, y = percent_change, fill = admin_area_1)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~indicator_common_id, scales = "free_y") +
  labs(
    title = "Volume Change Due to Outliers",
    x = "Month",
    y = "% Change",
    fill = "Administrative Area"
  ) +
  theme_minimal()

# Scatter Plot - Top Five Outlier Values -----------------------------
print("Creating scatter plot for top five outliers...")
scatter_plot <- ggplot(top_outliers_data, aes(x = reorder(facility_id, -max_percent_change), 
                                              y = max_percent_change, color = indicator_common_id)) +
  geom_point(size = 5) +
  geom_segment(aes(xend = facility_id, yend = 0), color = "grey") +
  labs(
    title = "Top Five Outlier Values by Indicator",
    x = "Facility ID",
    y = "% Change from Adjusted Volume",
    color = "Indicator",
    caption = "Top 5 facilities per indicator are shown."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_plot)
print(bar_chart)
print(scatter_plot)

# PART 1 - SAVE ALL OUTPUTS ----------------------------------------------------
print("Saving all data outputs from Part 1...")
write.csv(data, "data_output_part1.csv", row.names = FALSE)
write.csv(top_outliers_data, "top_outliers_data.csv", row.names = FALSE)
write.csv(heatmap_data, "heatmap_data_with_averages.csv", row.names = FALSE)
write.csv(volume_increase_data, "volume_increase_data.csv", row.names = FALSE)


# # Save Plots
print("Saving all plots...")
ggsave("heatmap_with_averages.png", plot = heatmap_plot, width = 10, height = 7)
ggsave("bar_chart_volume_change.png", plot = bar_chart, width = 10, height = 7)
ggsave("scatter_top_outliers.png", plot = scatter_plot, width = 10, height = 7)

print("Outliers analysis completed")

# PART 2: CONSISTENCY ANALYSIS -------------------------------------------------
# Step 1: Load Adjusted Data ---------------------------------------------------
print("Step 1: Loading adjusted data from Part 1...")
data <- read.csv("data_output_part1.csv")

# Step 2: Drop Outliers --------------------------------------------------------
print("Step 2: Excluding outlier values and recalculating dates...")
# Replace outlier values with NA
data <- data %>%
  mutate(volume = ifelse(outlier == 1, NA, count))


# Recalculate min and max dates for each year and indicator
date_summary <- data %>%
  group_by(indicator_common_id, year) %>%
  summarise(
    date_min = min(date, na.rm = TRUE),
    date_max = max(date, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Aggregate Data -------------------------------------------------------
print("Step 3: Aggregating data by geographic and time variables...")
aggregated_data <- data %>%
  group_by(across(all_of(c(geo_cols, "indicator_common_id", "year")))) %>%
  summarise(
    volume = sum(volume, na.rm = TRUE),  # Sum volume excluding outliers
    volIM = sum(volIM, na.rm = TRUE),    # Sum imputed volume
    .groups = "drop"
  ) %>%
  left_join(date_summary, by = c("indicator_common_id", "year"))  # Add recalculated dates

# Clean and prepare indicator types
aggregated_data <- aggregated_data %>%
  mutate(indictemp = str_replace_all(indicator_common_id, " ", "_")) %>%
  filter(!str_detect(indictemp, "\\+")) %>%  # Drop rows where indictemp contains a '+' symbol
  drop_na(any_of(tail(geo_cols, 1)))  # Drop rows missing the last geographic level (if it exists)


# Create a panel variable based on geographic and time variables
aggregated_data <- aggregated_data %>%
  mutate(panel_id = apply(select(., any_of(c(geo_cols, "year"))), 1, paste, collapse = "_"))

# Reshape data to wide format
print("Step 3b: Reshaping data to wide format...")
# Create a wide data frame  
wide_data <- aggregated_data %>%  
  mutate(  
    geo_name = sapply(strsplit(panel_id, "_"), function(x) paste(head(x, -1), collapse = "_")),  
    year = as.integer(sapply(strsplit(panel_id, "_"), tail, 1)),  
    indicator_abb = indictemp  
  ) %>%  
  pivot_wider(  
    id_cols = c(geo_name, year),  
    names_from = indicator_abb,  
    values_from = c(volume, volIM),  
    names_sep = "_",  
    values_fill = list(volume = 0, volIM = 0)  
  )

# #------------------------------------------------------------------------------------------- KENYA TEST---
# #--------------------------------------------------------- KENYA TEST DATA ONLY INCLUDES ANC, FACDEL & PNC


##Step 4: Calculate Consistency Ratios -------------------------------------------------------------------

# print("Step 4: Calculating consistency ratios...")
# wide_data <- wide_data %>%
#   mutate(
#     # Example ratios for consistency analysis
#     ratio_anc_facdel = if_else(volume_facdel != 0, volume_anc / volume_facdel, NA_real_),
#     sratio_anc_facdel = ifelse(ratio_anc_facdel > 1, 1, 0),
# 
#     ratio_pnc_facdel = if_else(volume_facdel != 0, volume_pnc / volume_facdel, NA_real_),
#     sratio_pnc_facdel = ifelse(ratio_pnc_facdel >= 0.7 & ratio_pnc_facdel <= 1.3, 1, 0)
#   )
# 
# # Step 5: Reshape Data Back to Long Format ------------------------------------
# print("Step 5: Reshaping data back to long format...")
# long_data <- wide_data %>%
#   # Reshape data from wide to long format
#   pivot_longer(
#     cols = starts_with("ratio_"),   # Pivot columns that start with 'ratio_'
#     names_to = "ratio_type",        # New column to hold ratio names
#     values_to = "consistency_ratio" # New column to hold values
#   ) %>%
#   # Assign corresponding 'sconsistency' values based on ratio_type
#   mutate(
#     sconsistency = case_when(
#       ratio_type == "ratio_anc_facdel" ~ sratio_anc_facdel,
#       ratio_type == "ratio_pnc_facdel" ~ sratio_pnc_facdel,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   # Split 'geo_name' into admin_area_* columns dynamically
#   mutate(
#     geo_split = str_split(geo_name, "_"),  # Split geo_name by underscore
#     admin_area_list = map(geo_split, function(x) set_names(x, paste0("admin_area_", seq_along(x))))
#   ) %>%
#   unnest_wider(admin_area_list) %>%  # Unnest the split geo_name into separate columns
#   select(-geo_split, -geo_name) %>%  # Drop intermediate columns used for splitting
#   
#   # Reorder columns to place 'admin_area_*' columns first
#   select(
#     starts_with("admin_area_"), 
#     everything()
#   )
# #------------------------------------------------------------------------------------------- KENYA TEST---
# # --------------------------------------------------------------------------------------------------------



# -------------------------------------------------------------------------------------
# Step 4: Calculate Consistency Ratios ------------------------------------------------
print("Step 4: Calculating consistency ratios...")
wide_data <- wide_data %>%
  mutate(
    # Antenatal care ratio (ANC1 / ANC4) - Should be > 1
    ratio_anc1 = if_else(volume_anc4 > 0, volume_anc1 / volume_anc4, NA_real_),
    sratio_anc1 = if_else(!is.na(ratio_anc1) & ratio_anc1 > 1, 1, 0),

    # Penta vaccine ratios (PENTA1 / PENTA3) - Should be > 1
    ratio_penta1 = if_else(volume_penta3 > 0, volume_penta1 / volume_penta3, NA_real_),
    sratio_penta1 = if_else(!is.na(ratio_penta1) & ratio_penta1 > 1, 1, 0),

    ratio_penta3 = if_else(volume_penta3 > 0, volume_penta1 / volume_penta3, NA_real_),  # Duplicate logic
    sratio_penta3 = if_else(!is.na(ratio_penta3) & ratio_penta3 > 1, 1, 0),

    # Institutional delivery vs. vaccination ratios
    ratio_delivery = if_else(volume_delivery > 0, volume_bcg / volume_delivery, NA_real_),
    sratio_delivery = if_else(
      !is.na(ratio_delivery) & ratio_delivery >= 0.7 & ratio_delivery <= 1.3, 1, 0
    ),

    ratio_bcg = if_else(volume_delivery > 0, volume_bcg / volume_delivery, NA_real_),
    sratio_bcg = if_else(
      !is.na(ratio_bcg) & ratio_bcg >= 0.7 & ratio_bcg <= 1.3, 1, 0
    ),

    # Postnatal care ratio (PNC1 / Delivery) - Should be within 0.7 and 1.3
    ratio_pnc1 = if_else(volume_delivery > 0, volume_pnc1 / volume_delivery, NA_real_),
    sratio_pnc1 = if_else(
      !is.na(ratio_pnc1) & ratio_pnc1 >= 0.7 & ratio_pnc1 <= 1.3, 1, 0
    )
  )


# Step 5: Reshape Data Back to Long Format ------------------------------------
print("Step 5: Reshaping data back to long format...")
long_data <- wide_data %>%
  # Pivot longer for ratios
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "ratio_type",
    values_to = "consistency_ratio"
  ) %>%
  # Assign corresponding sconsistency values
  mutate(
    sconsistency = case_when(
      ratio_type == "ratio_anc1" ~ sratio_anc1,
      ratio_type == "ratio_penta1" ~ sratio_penta1,
      ratio_type == "ratio_penta3" ~ sratio_penta3,
      ratio_type == "ratio_delivery" ~ sratio_delivery,
      ratio_type == "ratio_bcg" ~ sratio_bcg,
      ratio_type == "ratio_pnc1" ~ sratio_pnc1,
      TRUE ~ NA_real_
    )
  ) %>%
  # Split `geo_name` into admin_area_* columns dynamically
  mutate(geo_split = str_split(geo_name, "_")) %>%
  mutate(admin_area_list = map(geo_split, function(x) set_names(x, paste0("admin_area_", seq_along(x))))) %>%
  unnest_wider(admin_area_list) %>%
  select(-geo_split, -geo_name) %>%  # Drop intermediate columns

  # Reorder columns to ensure admin_area_* are first
  select(
    starts_with("admin_area_"),
    everything()
  )

# Step 6: Final Clean-Up ------------------------------------------------------
print("Step 6: Cleaning up and selecting final variables...")
final_consistency_data <- long_data %>%
  select(any_of(c(geo_cols, "year", "ratio_type", "consistency_ratio", 
                  "sconsistency", "date_min", "date_max")))%>%
  drop_na(consistency_ratio, sconsistency)  # Drop rows with missing values

# PART 2: VISUALISATION --------------------------------------------------------
# Table - Districts with Incomplete Data ---------------------------------------
print("Creating table for districts with incomplete data...")
# Summarize average completeness by admin_area_2
incomplete_data_table <- final_consistency_data %>%
  group_by(admin_area_2) %>%  # Group by division
  summarise(
    avg_completeness = mean(consistency_ratio, na.rm = TRUE),# Calculate average completeness
    .groups = "drop"
  ) %>%
  filter(avg_completeness < 90) %>%  # Keep divisions with completeness < 90%
  arrange(avg_completeness)  # Sort by avg_completeness

# Print the result
print("Table of districts with incomplete data:")
print(incomplete_data_table)

# PART 2: SAVE ALL OUTPUTS ----------------------------------------------------
print("Saving all data outputs from Part 2...")
write.csv(final_consistency_data, "data_output_part2.csv", row.names = FALSE)
write.csv(incomplete_data_table, "incomplete_data_table.csv", row.names = FALSE)

print("Consistency analysis completed")

# PART 3: COMPLETENESS ANALYSIS -------------------------------------------------
# Step 1: Load Adjusted Data ----------------------------------------------------
print("Step 1: Loading adjusted data from the outlier analysis...")
data <- read.csv("data_output_part1.csv")  # Load the adjusted dataset from outlier analysis
print(head(data))
# Ensure 'date' is in Date format
data <- data %>%
  mutate(date = as.Date(date))

# Step 2: Create Panel Variable -------------------------------------------------
print("Step 2: Creating panel variable...")
data <- data %>%
  mutate(panelvar = paste(indicator_common_id, facility_id, sep = "_"))

# Step 3: Create Complete Time Series -------------------------------------------
print("Step 3: Filling gaps in the time series...")
data <- data %>%
  complete(
    panelvar, 
    date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "month")
  ) %>%
  fill(!!!syms(geo_cols), .direction = "downup") %>%  # Dynamically fill geographic columns
  fill(indicator_common_id, .direction = "downup") %>%  # Fill missing indicators
  mutate(year = year(date))  # Ensure year is derived correctly


# Step 4: Create Completeness Indicator
print("Step 4: Creating completeness indicator...")
data <- data %>%
  mutate(completeness = ifelse(!is.na(count) & count > 0, 1, 0))

# Smoothing Reporting Time Frames
print("Smoothing reporting time frames...")
data <- data %>%
  group_by(panelvar) %>%
  arrange(date) %>%
  mutate(first_occurrence = cumsum(!is.na(completeness))) %>%
  arrange(desc(date)) %>%
  mutate(last_occurrence = rev(cumsum(rev(!is.na(completeness))))) %>%
  mutate(
    num1 = row_number(),
    num2 = row_number(desc(date))
  ) %>%
  ungroup() %>%
  filter(
    !(first_occurrence == 0 & num1 > 12),
    !(last_occurrence == 0 & num2 > 12)
  )

# Step 6: Calculate Completeness Metrics
print("Step 6: Calculating completeness metrics...")
completeness_summary <- data %>%
  group_by(across(all_of(c(geo_cols, "indicator_common_id", "date")))) %>%
  summarise(
    completeness = mean(completeness, na.rm = TRUE),
    total_volume = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

# PART 3: VISUALISATION --------------------------------------------------------
# Generate Completeness Heatmap ----------------------------------------
print("Creating completeness heatmap...")
completeness_summary <- data %>%
  group_by(across(all_of(geo_cols)), indicator_common_id) %>%
  summarise(
    completeness = mean(completeness, na.rm = TRUE) * 100,  # Convert to percentage
    .groups = "drop"
  )

# Step 2: Create a heatmap for completeness
heatmap_plot <- ggplot(completeness_summary, aes(x = indicator_common_id, y = admin_area_2, fill = completeness)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = viz_colors,
    values = rescale(c(0, 50, 75, 90, 100)),  # Adjust thresholds for your data
    limits = c(0, 100),                       # Ensure the scale is [0, 100]
    name = "Completeness (%)"
  ) +
  labs(
    title = "Completeness by Indicator and Region",
    x = "Indicator",
    y = "Region",
    fill = "Completeness (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
print(heatmap_plot)

# PART 3: SAVE ALL OUTPUTS ----------------------------------------------------
print("Saving all data outputs from Part 3...")
write.csv(completeness_summary, "completeness_summary.csv", row.names = FALSE)
ggsave("completeness_heatmap.png", plot = heatmap_plot, width = 12, height = 8)