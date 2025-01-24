# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 24
# Module: SERVICE UTILIZATION



# Check if adjusted_data exists in the environment
if (!exists("adjusted_data_final")) {
  warning("Please run Module 2: Dynamic Adjustment. It is needed to generate the adjusted data.")
} else {
  data <- adjusted_data_final
}
