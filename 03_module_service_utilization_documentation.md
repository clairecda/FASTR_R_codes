---
output:
  pdf_document: default
  html_document: default
---

# Module 3: Service Utilisation. Disruption analysis

This module analyzes disruptions in service utilization… Part 1 - Control Chart Part 2 - Disruption Analysis Before running regressions, the data is prepared: - Merging `M3_chartout_selected` to get `tagged` variable. - Identifying the **lowest geographic level** (`lowest_geo_level`) for clustering. The regression pipeline follows a structured, multi-level approach, starting from the broadest level (country-wide) and moving to more granular levels (province, then district).

### Country-Wide Regression (Indicator Level)

### Model Specification

For each `indicator_common_id`, we estimate:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \epsilon_{it}$$

Where: - $Y_{it}$ = volume (e.g., number of deliveries) - $\text{date}$ = time trend - $\text{month}_m$ = controls for seasonality (factor variable) - $\text{tagged}$ = dummy for disruption period - $\epsilon_{it}$ = error term, clustered at the district level (`admin_area_3`)

### Province-Level Regression. Runs one regression across all provinces with province fixed effects.

Model specification:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{province}} + \epsilon_{it}$$

Where: - $Y_{it}$ = volume (e.g., number of deliveries) - $\text{date}$ = time trend - $\text{month}_m$ = controls for seasonality (factor variable) - $\text{tagged}$ = dummy for disruption period - $\alpha_{\text{province}}$ = province fixed effects - $\epsilon_{it}$ = error term, clustered at the district level (`admin_area_3`)

### District-Level Regression. Runs one regression across all districts with district fixed effects.

Model specification:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \gamma \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{district}} + \epsilon_{it}$$

Where: - $Y_{it}$ = volume (e.g., number of deliveries) - $\text{date}$ = time trend - $\text{month}_m$ = controls for seasonality (factor variable) - $\text{tagged}$ = dummy for disruption period - $\alpha_{\text{district}}$ = district fixed effects - $\epsilon_{it}$ = error term

Each regression model (Country-wide, Province-level, and District-level) produces the following key outputs: - Expected values (`expect_admin_area_*`): The predicted service volume based on the regression model, adjusted for seasonality and time trends. - Disruption effect (`b_admin_area_*`): The estimated relative change in service utilization during the disruption period, computed as:

$$b_{\text{admin_area_*}} = -\frac{\text{diff mean}}{\text{predict mean}}$$

This measures the impact of the disruption relative to expected values.

-   Trend coefficient (`b_trend_admin_area_*`): The estimated underlying time trend in service volume, capturing long-term changes unrelated to disruptions.
-   Statistical significance (p-value) (`p_admin_area_*`): The probability that the disruption effect (`b_admin_area_*`) is due to random variation rather than an actual disruption. Lower values indicate stronger evidence of a true effect.
