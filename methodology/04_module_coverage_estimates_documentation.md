# Module 4: Coverage Estimates

## Background

(...) Coverage estimates provide a way to assess the performance of
health service delivery by comparing reported service volumes to the
expected target population.

## Overview

This module estimates health service coverage by integrating adjusted
health service volume data, population projections, and survey data
(MICS/DHS). Coverage estimates are calculated for key health indicators
using multiple denominator sources, and the optimal denominator is
selected by minimizing the error relative to survey data. The final
estimates are used to analyze trends in service coverage at national and
sub-national levels.

### Overview of the additional data used in the module

Population projections

Sourced from the United Nations World Population Prospects (WPP), these
estimates provide age-specific and total population figures used to
calculate denominators for coverage estimates. These projections account
for demographic trends, including fertility, mortality, and migration.

Survey data - MICS

The Multiple Indicator Cluster Surveys (MICS), conducted by UNICEF,
provide household survey-based estimates for key health indicators,
including coverage of maternal and child health services.

Survey data - DHS

The Demographic and Health Surveys (DHS), conducted by USAID, provide
survey data on health service utilization, including immunization rates
and maternal care coverage.

#### Detailed analysis steps

**Step 1: Load and map adjusted service volumes**

Health service volume data is loaded (output from Module 2) The selected
count variable (e.g., count adjusted for both outlier and completeness)
is mapped to coverage indicators. The data is aggregated at the annual
level for each administrative area.

**Step 2: Merge and extend survey data**

The survey data from MICS, DHS, and UNWPP is loaded and filtered to
match available HMIS data. The missing years in survey data are
forward-filled to ensure continuous time series. The survey estimates
are merged, prioritizing DHS over MICS when both exist.

**Step 3: Calculate denominators**

For each indicator, denominators are estimated based on service volumes
and reported survey coverage. The general formula is:

$$
\text{Denominator} = \frac{\text{Service volume}}{\text{Survey-based coverage} / 100}
$$

Denominators are derived from two sources:

1.  HMIS-based denominators – computed from health facility service
    volumes.
2.  UNWPP-based denominators – derived from population projections.

Each denominator is adjusted using pregnancy loss rates, stillbirth
rates, twin birth rates, and mortality rates.

**Default Adjustment Rates**

The following default rates are applied when adjusting denominators:

-   **Pregnancy loss rate** = 3%
-   **Twin rate** = 1.5%
-   **Stillbirth rate** = 2%
-   **Neonatal mortality rate** = 3%
-   **Infant mortality rate** = 5%

These rates account for losses at different stages (pregnancy, birth,
early infancy) and are used to refine denominator estimates.

#### HMIS-based denominator calculations

**ANC1 denominator**

$$
d_{\text{anc1, pregnancy}} = \frac{\text{count}_{\text{anc1}}}{\text{coverage}_{\text{anc1}} / 100}
$$

$$
d_{\text{anc1, livebirth}} = d_{\text{anc1, pregnancy}} \times (1 - \text{pregnancy loss rate}) \times (1 - \frac{\text{twin rate}}{2}) \times (1 - \text{stillbirth rate})
$$

$$
d_{\text{anc1, dpt}} = d_{\text{anc1, pregnancy}} \times (1 - \text{pregnancy loss rate}) \times (1 - \frac{\text{twin rate}}{2}) \times (1 - \text{stillbirth rate}) \times (1 - \text{neonatal mortality rate})
$$

$$
d_{\text{anc1, mcv}} = d_{\text{anc1, pregnancy}} \times (1 - \text{pregnancy loss rate}) \times (1 - \frac{\text{twin rate}}{2}) \times (1 - \text{stillbirth rate}) \times (1 - \text{infant mortality rate})
$$

**Delivery denominator (live births)**

$$
d_{\text{delivery, pregnancy}} = \frac{\text{count}_{\text{delivery}}}{\text{coverage}_{\text{delivery}} / 100} \times (1 - \text{pregnancy loss rate})
$$

$$
d_{\text{delivery, dpt}} = d_{\text{delivery, pregnancy}} \times (1 + \text{twin rate}) \times (1 - \text{stillbirth rate}) \times (1 - \text{neonatal mortality rate})
$$

$$
d_{\text{delivery, mcv}} = d_{\text{delivery, pregnancy}} \times (1 + \text{twin rate}) \times (1 - \text{stillbirth rate}) \times (1 - \text{infant mortality rate})
$$

**BCG denominator**

$$
d_{\text{bcg, pregnancy}} = \frac{\text{count}_{\text{bcg}}}{\text{coverage}_{\text{bcg}} / 100} \times (1 - \text{pregnancy loss rate}) \times (1 + \text{twin rate}) \times (1 - \text{stillbirth rate})
$$

$$
d_{\text{bcg, dpt}} = d_{\text{bcg, pregnancy}} \times (1 - \text{neonatal mortality rate})
$$

$$
d_{\text{bcg, mcv}} = d_{\text{bcg, pregnancy}} \times (1 - \text{neonatal mortality rate}) \times (1 - \text{post-neonatal mortality rate})
$$

**Penta1 denominator**

$$
d_{\text{penta1, pregnancy}} = \frac{\text{count}_{\text{penta1}}}{\text{coverage}_{\text{penta1}} / 100} \times (1 - \text{pregnancy loss rate}) \times (1 + \text{twin rate}) \times (1 - \text{stillbirth rate}) \times (1 - \text{neonatal mortality rate})
$$

$$
d_{\text{penta1, livebirth}} = d_{\text{penta1, pregnancy}} \times (1 - \text{stillbirth rate}) \times (1 - \text{neonatal mortality rate})
$$

$$
d_{\text{penta1, mcv}} = d_{\text{penta1, pregnancy}} \times (1 - \text{post-neonatal mortality rate})
$$

#### UNWPP-based denominator calculations

Some denominators can also be derived from World Population Prospects
(WPP) projections instead of service volumes.

**Estimated pregnancies based on crude birth rate (CBR) and total
population**

$$
d_{\text{wpp, pregnancy}} = \left( \frac{\text{CBR}}{1000} \right) \times \text{Total population} \times \frac{12}{\text{months reported}}
$$

**Estimated live births**

$$
d_{\text{wpp, livebirth}} = \text{Total live births from WPP} \times \frac{12}{\text{months reported}}
$$

**Estimated population eligible for DPT1 (under 1 year)**

$$
d_{\text{wpp, dpt}} = \text{Total under-1 population from WPP} \times \frac{12}{\text{months reported}}
$$

**Estimated population eligible for MCV (under 5 years)**

$$
d_{\text{wpp, mcv}} = \text{Total under-5 population from WPP} \times \frac{12}{\text{months reported}}
$$

HMIS data can contains incomplete or partial reporting, where service
volumes are only available for a subset of months in a given year. If
fewer than 12 months of data are reported, directly using the raw count
would underestimate the total number of pregnancies. To adjust for this,
we scale the reported number up to a full year equivalent by applying
the factor $\frac{12}{\text{months reported}}$.

**Step 4: Select the best denominator**

Both HMIS-based and UNWPP-based denominators are computed, but only one
denominator is used per indicator-year based on squared error
minimization.

The survey-derived coverage estimates (`avgsurvey_*`) are used as
reference values for comparison. These values are extracted for each
year and admin area.

For each denominator option (HMIS-derived and WPP-derived), a squared
error is calculated:

$$
\text{Squared error} = (\text{HMIS-derived coverage} - \text{Survey coverage})^2
$$

where:

$$
\text{HMIS-derived coverage} = \frac{\text{HMIS service volume}}{\text{denominator}} \times 100
$$

For each year, indicator, and admin area, the denominator with the
smallest squared error is selected:

$$
\text{Best denominator} = \arg \min_{\text{denominator}} (\text{squared error})
$$

**Step 5: Compute coverage estimates**

Once the best denominator is selected, final coverage is calculated as:

$$
\text{Coverage} = \frac{\text{service volume}}{\text{selected denominator}} \times 100
$$

**Step 6: Extrapolate missing estimates**

For years where survey estimates are unavailable, coverage is projected
using the year-over-year coverage change (Δ coverage) derived from HMIS
data. The assumption is that the trend in coverage observed in HMIS data
can be used to estimate missing survey values.

This is done by applying the coverage delta, which represents the annual
change in coverage, to the most recent available survey estimate.

$$
\text{Projected coverage}_{t} = \text{Survey coverage}_{t-1} + \Delta \text{coverage}_{\text{HMIS}, t}
$$

where:

\- ${Survey\ coverage,{_t}_{-1}}$ = last available survey-based coverage
estimate\
- ${\text{Δ coverage HMIS},_t}$ = annual change in coverage derived from
HMIS data\
- $Projected\ coverage_t$ = estimated coverage for the missing year

**Step 7: Combine results and finalize output**

The coverage estimates from HMIS-derived calculations, survey estimates,
and projections are merged.

The final results are exported in a structured format for visualization
and reporting.
