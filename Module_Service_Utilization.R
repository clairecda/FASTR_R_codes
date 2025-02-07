# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 24
# Module: SERVICE UTILIZATION

#Read input
data <- read.csv("M2_adjusted_data_admin_area.csv")

#Write Output
write.csv(data, "M3_service_utilization.csv", row.names = FALSE)