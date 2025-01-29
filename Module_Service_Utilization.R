# CB - R code FASTR PROJECT
# Last edit: 2025 Jan 24
# Module: SERVICE UTILIZATION

#Read input
data <- read_csv("M2.adjusted_data_final.csv")

#Write Output
write.csv(data, "M3_service_utilization.csv", row.names = FALSE)