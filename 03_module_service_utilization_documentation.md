---
editor_options:
  markdown:
    wrap: 72
output: pdf_document
---

# Module 3: Service Utilization

This module is used to

-   Detect disruptions or surpluses in service use.

-   Compare current service use volumes to historical trends and
    seasonality, adjusting for data quality.

## Background

Service utilization refers to the volume of health services delivered
and reported through routine health information systems (i.e. DHIS2). It
reflects how populations access and use essential healthcare services
over time and across different regions.

However, service utilization can fluctuate due to various factors,
including seasonal trends, policy changes, pandemics, or other external
shocks. Identifying whether these variations are part of normal patterns
or signal significant disruptions is important for health system
monitoring and decision-making.

(...)

The Control Chart analysis helps determine whether deviations in service
volumes are part of normal fluctuations or indicate significant
disruptions.

The Disruption analysis helps quantify the impact of these disruptions
by measuring how service volumes changed during flagged periods.

(...)

## Overview

The Service Utilization module is designed to evaluate trends in health
service usage by identifying disruptions and surpluses in service
delivery over time. The module consists of two key components:

1.  **Control chart analysis**

2.  **Disruption analysis**

### Control chart analysis

The control chart analysis aggregates service volume data at the
province level to monitor trends, filling in missing months to create a
continuous time series. A regression model estimates expected values
based on historical trends and seasonality, while smoothing techniques
such as rolling averages and interpolation reduce noise.

Anomalies (disruptions) are flagged when actual service volumes
significantly deviate from expected values based on the control variable
(`control`). The `control` variable is calculated as the standardized
residual, defined as the difference between actual service volume and
the smoothed predicted value (`count_smooth`), divided by the standard
deviation of residuals (`sd_residual`). This normalization ensures that
deviations are measured relative to typical variations, making it easier
to identify significant disruptions. If the absolute value of `control`
is greater than or equal to 2, the point is flagged as an anomaly
(`tag = 1`).\
Forward and backward tagging ensures systemic disruptions are captured,
rather than isolated events, by propagating flagged anomalies in both
directions.

Once anomalies are flagged, they are merged with a disruption database
that contains information on known events such as pandemics (e.g.,
COVID-19), conflicts, coups, and policy changes. If a flagged anomaly
falls within a known disruption period, the `tagged` variable is updated
to ensure these disruptions are explicitly marked in the dataset.

The results of the control chart analysis are saved in
`M3_chartout.csv`, which serves as an input for the disruption analysis.

### Disruption analysis

Once anomalies are identified and saved in `M3_chartout.csv`, the
disruption analysis quantifies their impact using regression models.
These models estimate how much service utilization changed during the
flagged disruption periods by adjusting for long-term trends and
seasonal variations.

For each indicator, we estimate:

$`Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month}_m + \beta_2 \cdot \text{tagged} + \epsilon_{it}`$

where:\
- $`Y_{it}`$ is the observed service volume,\
- $`\text{date}`$ captures time trends,\
- $`\text{month}_m`$ controls for seasonality,\
- $`\text{tagged}`$ is the disruption dummy (from the control chart
analysis),\
- $`\epsilon_{it}`$ is the error term.

The coefficient on $`\text{tagged}`$ ($`\beta_2`$) measures the relative
change in service utilization during flagged disruptions. Separate
regressions are run at the province and district levels to assess the
impact across different geographic scales.

#### Detailed analysis steps

##### **PART 1 - Control Chart Analysis**

**Step 1: Prepare the data (note here for Claire to review - should we
read form module 2? if yes: create new output = remove outliers do not
adjust the data and aggregate to province)**

-   Read service utilization data and remove flagged outliers.

-   Create a `date` column combining `year` and `month`.

-   Generates a panel variable (`panelvar`) that groups data by
    `indicator_common_id` and `admin_area_2` (province level).

-   Aggregates data at the province level while keeping country-level
    identifiers (`admin_area_1`).

-   Expands missing months within each province group to ensure
    continuity in the time series.

-   Fills missing values in `indicator_common_id`, `admin_area_1`, and
    `admin_area_2` using forward and backward filling.

**Step 2: Filter out low-volume data**

-   Compute the global mean of service volumes for each group
    (`panelvar`).

-   Drop months where service volumes are less than 50% of the global
    mean.

**Step 3: Apply regression and smoothing**

-   Run a linear regression for each province group to estimate expected
    service volumes

    $`Y_{it} = \beta_0 + \sum_{m=1}^{12} \gamma_m \cdot \text{month}_m + \beta_1 \cdot \text{date} + \epsilon_{it}`$

Where:

$Y_{it}$ is the observed service volume,

$\text{month}_m$ controls for seasonality,

$\text{date}$ captures time trends,

$\epsilon_{it}$ is the error term.

-   If sufficient data (≥12 months) is available, predicted values are
    generated; otherwise, the median is used.

**Step 4: Smoothing and residual calculation**

-   Applies lead-lag smoothing as described below.

*Lead-lag smoothing* is a technique used to remove noise from a dataset
by averaging observed values across past and future periods. The process
works as follows:

1.  For each data point, the algorithm considers a window of past
    (lagged) and future (leading) values within a specified number of
    months.

2.  Averages these values, creating a smoothed estimate that reduces
    short-term fluctuations and better represents the underlying trend.

3.  Interpolates missing values using linear interpolation, which
    estimates the missing values by assuming a straight-line
    relationship between the known data points before and after the gap.

4.  Replaces each observed value with the computed smoothed value.

Mathematically, lead-lag smoothing is represented as:

$`{count\_smooth}_{it} = \frac{1}{2k+1} \sum_{j=-k}^{k} Y_{i,t+j}`$

where:

-   $Y_{i,t}$ is the observed service volume at time ${t}$ for province
    ${i}$,

-   ${k}$ is the number of months considered for smoothing (e.g., 6
    months),

-   ${t+j}$ represents the lead (+) and lag (-) months,

-   The sum takes the average across the window of ${2k+1}$ months.

-   Computes residuals: the difference between actual values and
    smoothed predictions `count_smooth`.

$`{residual}_{it} = Y_{it} -   {count\_smooth}_{it}`$

where:

${Y_{it}}$ is the observed service volume

${count\_smooth}$ is the smoothed expected value.

-   Standardizes residuals using the standard deviation (`sd_residual`)
    to obtain the `control` variable.

The `control` variable measures how many standard deviations the
observed volume deviates from the expected trend.

$`\text{control}_{it} = \frac{\text{residual}_{it}}{sd\_residual}`$

If:

$\text{control}_{it}> 2$, the observed service volume is significantly
higher than expected.

$\text{control}_{it}< 2$, the observed service volume is significantly
lower than expected.

Values between -2 and 2 are considered within the normal range of
variability.

**Step 5: Tagging disruptions**

Once the `control` variable is computed, disruptions are identified and
tagged:

**Initial Tagging:**

-   If `abs(control) >= 2`, the time point is flagged as an anomaly
    (`tag = 1`).

-   If the deviation is small (`abs(control) <= 0.5`), it is considered
    within normal fluctuations and assigned `tag = 0`.

If the absolute difference between `Y_{it}` and `count_smooth` is less
than 5% of `count_smooth`, it is also assigned `tag = 0`.

$`\text{tag}_{it} =
\begin{cases}
1, & \text{if } |\text{control}_{it}| \geq 2 \\
0, & \text{if } |Y_{it} - \text{count\_smooth}_{it} | \text{count\_smooth}_{it} < 0.05 \\
0, & \text{if } |\text{control}_{it}| \leq 0.5 \\
\text{NA}, & \text{otherwise}
\end{cases}`$

**Step 6: Crossing with the disruption database**

-   The flagged anomalies from the control chart analysis are merged
    with the disruption database, which contains known events such as
    pandemics (e.g., COVID-19), conflicts, coups, and policy changes.
-   If a flagged anomaly falls within a known disruption period (i.e.,
    the date of service disruption is within the start and end dates of
    a documented event), the `tagged` variable is updated to explicitly
    mark it as a disruption.
-   The results are saved in `M3_chartout.csv`, which serves as the
    input for Part 2: Disruption Regression Analysis.

##### PART 2. Disruption Analysis

**Step 1: Data preparation**

-   The `M3_chartout` dataset is merged with the main dataset to
    integrate the `tagged` variable, which identifies flagged
    disruptions.
-   The lowest available geographic level (`lowest_geo_level`) is
    identified for clustering, based on the highest-resolution
    `admin_area_*` column available.

... then the regression pipeline follows a structured, multi-level
approach, starting from the broadest level (country-wide) and moving to
more granular levels (province, then district).

**Step 2: Country-wide regression**

The country-wide regression estimates how service utilization changes at
the national level when a disruption occurs. Instead of analyzing
individual provinces or districts separately, this model considers the
entire country's data in a single regression. Errors are clustered at
the lowest available geographic level (`lowest_geo_level`), which can be
districts or wards.

-   A panel regression model is applied at the country-wide level,
    estimating the expected service volume (`expect_admin_area_1`) for
    each indicator (`indicator_common_id`).
-   The model adjusts for historical trends and seasonal variations,
    ensuring that deviations are measured against expected patterns.
-   If a disruption (`tagged` = 1) is detected, the predicted service
    volume is adjusted by subtracting the estimated effect of the
    disruption to isolate its impact.

Model Specification

For each `indicator_common_id`, we estimate:

$`Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \epsilon_{it}`$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\epsilon_{it}$ = error term, clustered at the district level
(`admin_area_3`)

The country-wide regression is implemented using the `feols()` function
in R:
`model <- tryCatch( feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged")), data = indicator_data, cluster = as.formula(paste0("~", lowest_geo_level))), error = function(e) { print(paste("Regression failed for:", indicator, "Error:", e$message)) return(NULL) } )`

**Step 3: Province-level regression**

The province-level disruption regression estimates how service
utilization changes at the province level when a disruption occurs.
Unlike the country-wide model, which treats the entire country as a
single unit, this approach runs separate regressions for each province
to capture regional variations in service utilization during
disruptions. Errors are clustered at the lowest available geographic
level, districts or wards, *to account for local variation within each
province.*

-   A fixed effects panel regression model is applied at the province
    level, estimating expected service volume (`expect_admin_area_2`)
    while controlling for province-specific factors.

-   The model adjusts for historical trends and seasonal variations,
    ensuring deviations are compared against expected patterns.

-   If a disruption (`tagged` = 1) is detected, the predicted service
    volume is adjusted by subtracting the estimated effect of the
    disruption to isolate its impact.

Model specification:

$`Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{province}} + \epsilon_{it}`$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\alpha_{\text{province}}$ = province fixed effects

$\epsilon_{it}$ = error term, clustered at the district level
(`admin_area_3`)

The province-level regression is implemented using the `feols()`
function in R:
`model_province <- tryCatch( feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged | admin_area_2")), data = province_data, cluster = as.formula(paste0("~", lowest_geo_level))), error = function(e) { print(paste("Regression failed for:", indicator, "in", province, "Error:", e$message)) return(NULL) } )`

**Step 4: District-level regression**

The district-level disruption regression estimates how service
utilization changes at the district level when a disruption occurs. This
approach runs separate regressions for each district to capture
localized variations in service utilization during disruptions. Errors
are clustered at the **l**owest available geographic level, typically
wards or districts, to account for variations within each district.
**REVIEW** THIS \> might need to cluster at the district always..

-   A fixed effects panel regression model is applied at the district
    level, estimating expected service volume (`expect_admin_area_3`)
    while controlling for district-specific factors.

-   The model adjusts for historical trends and seasonal variations,
    ensuring deviations are compared against expected patterns.

-   If a disruption (`tagged` = 1) is detected, the predicted service
    volume is adjusted by subtracting the estimated effect of the
    disruption to isolate its impact.

Model specification:

$`Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \gamma \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{district}} + \epsilon_{it}`$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\alpha_{\text{district}}$ = district fixed effects

$\epsilon_{it}$ = error term

The district-level regression is implemented using the `feols()`
function in R:
`model_district <- tryCatch( feols(as.formula(paste(SELECTEDCOUNT, "~ date + factor(month) + tagged | admin_area_3")), data = district_data, cluster = as.formula(paste0("~", lowest_geo_level))), error = function(e) { print(paste("Regression failed for:", indicator, "in", district, "Error:", e$message)) return(NULL) } )`

Step 5: Prepare outputs for visualization

Each regression model (country-wide, province-level, and district-level)
produces the following key outputs:

-   Expected values (`expect_admin_area_*`): The predicted service
    volume based on the regression model, adjusted for seasonality and
    time trends.

-   Disruption effect (`b_admin_area_*`): The estimated relative change
    in service utilization during the disruption period, computed as:
    $`b_{\text{admin\_area_*}} = -\frac{\text{diff mean}}{\text{predict mean}}`$.
    This measures the impact of the disruption relative to expected
    values.

-   Trend coefficient (`b_trend_admin_area_*`): The estimated underlying
    time trend in service volume, capturing long-term changes unrelated
    to disruptions.

-   Statistical significance (p-value) (`p_admin_area_*`): The
    probability that the disruption effect (`b_admin_area_*`) is due to
    random variation rather than an actual disruption. Lower values
    indicate stronger evidence of a true effect.

(ADD BELOW : HOW IS THE DATA PREPARED FOR VISUALIZATION / Line graphs
and Maps as per Guinea Slide deck)

------------------------------------------------------------------------

last edit March 19
