
filtered_data <- final_results %>%
  filter(indicator_common_id == "bcg" & admin_area_2 == "IRS Kindia")


# Plot the smoothed counts and original counts over time using ggplot2
ggplot(filtered_data, aes(x = date)) +
  geom_line(aes(y = count_smooth, color = "Smoothed Count"), size = 1) +   # Line plot for count_smooth
  geom_line(aes(y = count_original, color = "Original Count"), size = 1, linetype = "dashed") +  # Line plot for count_original (dashed)
  

  geom_vline(data = filtered_data %>% filter(tagged == 1), aes(xintercept = as.numeric(date)), 
             color = "orange", linetype = "dotted", size = 1, alpha = 0.5) +  # Disruption lines
  
  
  labs(title = "bcg - IRS Kindia - actual vs smoothed trend line",
       x = "Date",
       y = "Count") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  # Hide legend title
  scale_color_manual(values = c("Smoothed Count" = "steelblue", "Original Count" = "darkred"))  # Customize the colors

