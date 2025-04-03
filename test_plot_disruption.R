DIFFPERCENT <- 10
# Filter to just Abia and prep the difference logic
plot_data <- summary_disruption_admin1 %>%
  group_by(period_id, indicator_common_id) %>%
  summarise(
    count_original = mean(count_original, na.rm = TRUE),
    count_expect_diff = mean(count_expect_diff, na.rm = TRUE),
    diff_percent = mean(diff_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period_date = as.Date(paste0(period_id, "01"), format = "%Y%m%d"),
    show_shading = abs(diff_percent) > DIFFPERCENT,
    above = ifelse(count_original > count_expect_diff & show_shading, count_original, count_expect_diff),
    below = ifelse(count_original < count_expect_diff & show_shading, count_original, count_expect_diff)
  )


ggplot(plot_data, aes(x = period_date)) +
  # Green (surplus): actual > expected
  geom_ribbon(aes(
    ymin = count_expect_diff,
    ymax = above,
    group = indicator_common_id,
    fill = "Surplus"
  ), alpha = 0.3, na.rm = TRUE) +
  
  # Red (disruption): actual < expected
  geom_ribbon(aes(
    ymin = below,
    ymax = count_expect_diff,
    group = indicator_common_id,
    fill = "Disruption"
  ), alpha = 0.3, na.rm = TRUE) +
  
  # Lines
  geom_line(aes(y = count_original, color = "Actual volume"), linewidth = 0.5) +
  geom_line(aes(y = count_expect_diff, color = "Expected volume"), linewidth = 0.5) +
  
  facet_wrap(~ indicator_common_id, scales = "free_y") +
  scale_fill_manual(
    name = "Shading",
    values = c("Surplus" = "darkcyan", "Disruption" = "lightcoral")
  ) +
  scale_color_manual(
    name = "Line",
    values = c("Actual volume" = "#00aa0020", "Expected volume" = "black")  # transparent green, adjusted is black
  ) +
  labs(
    title = "Disruption by indicator",
    x = "Date", y = "Service volume"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )





