---
editor_options: 
  markdown: 
    wrap: 72
---

# Module 3: Service Utilization

This module is used to

-   Detect disruptions or surpluses in service use.

-   Compare current service use volumes to historical trends and
    seasonality, adjusting for data quality.

## PART 1. Control Chart Analysis

The Control Chart analysis helps determine whether deviations in service
volumes are part of normal fluctuations or indicate significant
disruptions.

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

The results are saved in `M3_chartout.csv`, which is later merged with
external disruption databases for tagging known disruptions.

A note on *lead-lag smoothing*.

Lead-lag smoothing is a technique used to remove noise from a dataset by
averaging observed values across past and future periods. The process
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

$\text{count_smooth}_{it} = \frac{1}{2k+1} \sum_{j=-k}^{k} Y_{i,t+j}$

where:

-   $Y_{i,t}$ is the observed service volume at time ${t}$ for province
    ${i}$,

-   ${k}$ is the number of months considered for smoothing (e.g., 6
    months),

-   ${t+j}$ represents the lead (+) and lag (-) months,

-   The sum takes the average across the window of ${2k+1}$ months.

#### Detailed analysis steps

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

    $$
    Y_{it} = \beta_0 + \sum_{m=1}^{12} \gamma_m \cdot \text{month}_m + \beta_1 \cdot \text{date} + \epsilon_{it}
    $$

Where:

$Y_{it}$ is the observed service volume,

$\text{month}_m$ controls for seasonality,

$\text{date}$ captures time trends,

$\epsilon_{it}$ is the error term.

-   If sufficient data (≥12 months) is available, predicted values are
    generated; otherwise, the median is used.

**Step 4: Smoothing and residual calculation**

-   Applies lead-lag smoothing as described above.

-   Computes residuals: the difference between actual values and
    smoothed predictions `count_smooth`.

$$
    {residual}_{it} = Y_{it} -   {count\_smooth}_{it}
$$where:

${Y_{it}}$is the observed service volume

${count\_smooth}$ is the smoothed expected value.

-   Standardizes residuals using the standard deviation (`sd_residual`)
    to obtain the `control` variable.

The `control` variable measures how many standard deviations the
observed volume deviates from the expected trend.

$$
\text{control}_{it} = \frac{\text{residual}_{it}}{sd\_residual}
$$If:

-   $\text{control}_{it}> 2$, the observed service volume is
    significantly higher than expected.

-   $\text{control}_{it}< 2$, the observed service volume is
    significantly lower than expected.

-   Values between -2 and 2 are considered within the normal range of
    variability.

## PART 2. Disruption Analysis

intro... before running the regressions, the data is prepared

Merging `M3_chartout_selected` to get `tagged` variable Identifying the
lowest geographic level (`lowest_geo_level`) for clustering

then the regression pipeline follows a structured, multi-level approach,
starting from the broadest level (country-wide) and moving to more
granular levels (province, then district).

#### Country-Wide Regression (Indicator Level)

Model Specification

For each `indicator_common_id`, we estimate:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \epsilon_{it}$$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\epsilon_{it}$ = error term, clustered at the district level
(`admin_area_3`)

### Province-Level Regression. Runs one regression across all provinces with province fixed effects.

Model specification:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \sum_{m=1}^{12} \gamma_m \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{province}} + \epsilon_{it}$$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\alpha_{\text{province}}$ = province fixed effects

$\epsilon_{it}$ = error term, clustered at the district level
(`admin_area_3`)

### District-Level Regression. Runs one regression across all districts with district fixed effects.

Model specification:

$$Y_{it} = \beta_0 + \beta_1 \cdot \text{date} + \gamma \cdot \text{month} + \beta_2 \cdot \text{tagged} + \alpha_{\text{district}} + \epsilon_{it}$$

Where:

$Y_{it}$ = volume (e.g., number of deliveries)

$\text{date}$ = time trend

$\text{month}_m$ = controls for seasonality (factor variable)

$\text{tagged}$ = dummy for disruption period

$\alpha_{\text{district}}$ = district fixed effects

$\epsilon_{it}$ = error term

Each regression model (Country-wide, Province-level, and District-level)
produces the following key outputs: - Expected values
(`expect_admin_area_*`): The predicted service volume based on the
regression model, adjusted for seasonality and time trends. - Disruption
effect (`b_admin_area_*`): The estimated relative change in service
utilization during the disruption period, computed as:

$$b_{\text{admin_area_*}} = -\frac{\text{diff mean}}{\text{predict mean}}$$

This measures the impact of the disruption relative to expected values.

-   Trend coefficient (`b_trend_admin_area_*`): The estimated underlying
    time trend in service volume, capturing long-term changes unrelated
    to disruptions.
-   Statistical significance (p-value) (`p_admin_area_*`): The
    probability that the disruption effect (`b_admin_area_*`) is due to
    random variation rather than an actual disruption. Lower values
    indicate stronger evidence of a true effect.
