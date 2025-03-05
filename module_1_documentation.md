# Module 1: Data Quality Assessment (DQA)
## Background
Routinely reported health facility data are an important source for health indicators at the facility and population levels. Health facilities report on events such as immunizations given or live births attended by a skilled provider. As with any data, quality is an issue. Data need to be checked to consider completeness of reporting by health facilities, identify extreme outliers, and evaluate internal consistency. A standard approach for assessing data quality allows for assessment of progress over time.  
The FASTR approach conducts an analysis of monthly data by facility and by indicator to assess data quality. Results are presented as annual estimates but may comprise a partial year of data given the availability of data at the time the analysis is conducted (e.g., an analysis conducted in June 2024 may contain data from January – May 2024, and this will be presented as the analysis for 2024).

## Overview

The Data Quality Assessment (DQA) module is designed to evaluate the reliability of HMIS (Health Management Information System) data by examining three key components:

- **Detecting Outliers** – Identifies extreme observations that may indicate reporting errors or anomalies.
- **Assessing Completeness** – Evaluates whether health facilities are consistently reporting data over time.
- **Measuring Consistency** – Checks whether related indicators align with expected patterns.

Finally, these assessments are integrated to generate a DQA score, which reflects the overall data quality. The following sections will provide a detailed explanation of each component, outlining the methodology and parameters applied in the analysis.

### Outlier Detection
For the FASTR analysis, we identify outliers which are suspiciously high values compared to the usual volume of services reported by the facility (e.g., low values are not identified as outliers in the FASTR analysis). Outliers are identified by assessing the within-facility variation in monthly reporting for each indicator. An outlier is defined as: 
- A value greater than 10 times the median absolute deviation (MAD) from the monthly median value for the indicator in each time period,  **OR** a value for which the proportional contribution in volume for a facility, indicator, and time period  is greater than 80%, **AND**
 For which:
- The volume is greater than or equal to the median **AND**
- The volume is not missing **AND**
- The volume is greater than 100.

#### Detailed analysis steps
This analysis is designed to detect and correct outliers in service volume data reported by health facilities. The code applies multiple methods to identify outliers, including the calculation of the Median Absolute Deviation (MAD) and the proportion of total service volume reported in a given time period. There are X main steps in this process:

(insert here steps -cb)

### Statistical notes
#### How is a median absolute deviation (MAD) calculated ?
- Compute the median: Find the median of the dataset.
- Calculate absolute deviations: Subtract the median from each data point to get the absolute deviations (i.e., take the absolute value of the difference between each data point and the median).
- Find the median of absolute deviations: Calculate the median of these absolute deviations.
- To determine the degree of outlier, calculate the median average absolute deviation residuals:
$$\frac{\text{(abs(volume - median volume))}}{\text{MAD}}$$

- If this value is greater than 10, the value is an outlier.

### Consistency between related indicators
Program indicators with a predictable relationship are examined to determine whether the expected relationship exists between them. In other words, this process examines whether the observed relationship between the indicators, as shown in the reported data, is that which is expected.

#### FASTR definition of internal consistency
FASTR assesses the following pairs of indicators to measure internal consistency:

| Indicator Pair            | Expected Relationship     |
| ------------------------- | ------------------------- |
| ANC1 / ANC4               | Ratio \> 1                |
| Penta1 / Penta3           | Ratio \> 1                |
| BCG / Facility Deliveries | Ratio between 0.7 and 1.3 |

These pairs of indicators have expected relationships. For example, we expect the number of pregnant women receiving a first ANC visit will always be higher than the number of pregnant women receiving a fourth ANC visit. BCG is a birth dose vaccine so we expect that these indicators will be equal. However, we recognize there may be more variability in this predicted relationship thus we set a range of within 30%. 

#### Detailed analysis steps
This analysis is designed to identify inconsistencies in service volume data reported by health facilities. Pairs of indicators with an expected relationship are compared as the district and national levels. Consistency benchmarks are applied to identify potential challenges with internal consistency of related indicators. There are xx main steps in this process:

(Draft below)
1. Remove outliers
2. Aggregate data
3. Calculate consistency ratios
4. Assess benchmarks
5. Save data (output object name here)

- Drop outlier values as identified in the previous step.
- For each indicator, calculate the service volume (i.e. unadjusted volume ) per admin area and year.
- Calculate the ratio of service volumes as per the above indicator pairs. 

If the following criteria are met, the ratio is considered consistent:

$$\text{ANC Consistency} =
\begin{cases} 
1, & \frac{\text{ANC1 Volume}}{\text{ANC4 Volume}} > 1 \\ 
0, & \text{otherwise}
\end{cases}$$

$$\text{Penta Consistency} =
\begin{cases} 
1, & \frac{\text{ANC1 Volume}}{\text{ANC4 Volume}} > 1 \\ 
0, & \text{otherwise}
\end{cases}$$

$$\text{BCG/Delivery Consistency} =
\begin{cases} 
1, & 0.7 \leq \frac{\text{BCG Volume}}{\text{Delivery Volume}} \leq 1.3 \\ 
0, & \text{otherwise}
\end{cases}$$

#### Analysis outputs, data visualization and interpretation
The FASTR analysis generates one main output related to internal consistency:
(Insert here image/illustration heat-map)
Percent of districts meeting consistency benchmarks
For a given indicator-pair in a given time period (i.e. year),

$$
\text{Percentage of districts that are consistent} = \frac{\text{Number of districts meeting consistency benchmark}}{\text{Total number of districts}} \times 100
$$



The % of districts meeting consistency benchmarks can be presented at national level as well as subnational level.  This is generally presented for each indicator of interest and not aggregated across indicators. 

The following colors have been used to create a heat map within the visualization: **insert here the color scale(s)**


### Indicator completeness
Indicator completeness measures the extent to which facilities that are supposed to report data on the selected core indicators are in fact doing so.  Higher completeness improves reliability of the data, especially when completeness is stable over time. This is different from overall reporting completeness in that it looks at completeness of specific data elements and not only at the receipt of the monthly reporting form.

#### FASTR definition of indicator completeness
For the FASTR analysis, completeness is defined as the percentage of reporting facilities each month out of the total number of facilities expected to report. For a given indicator in a given month,

\text{\% of districts that are consistent} = \frac{\text{Number of districts meeting consistency benchmark}}{\text{Total number of districts}} \times 100

A facility is deemed to be “reporting” if there is a non-missing, non-zero value recorded for the indicator and month. A facility is expected to report if it has reported any volume for each indicator anytime within a year  . 

Notes on completeness:
• A high level of completeness does not necessarily indicate that the HMIS is representative of all service delivery in the country as some services many not be delivered in facilities, or some facilities may not report.

• For countries where the DHIS2 system does not store zeroes, indicator completeness may be underestimated if there are many low-volume facilities for a given indicator.

Completeness is estimated for the following indicators in the core FASTR analysis:
- Outpatient visits
- Outpatient department visits (OPD)
- First antenatal care visit (ANC1)
- Fourth antenatal care visit (ANC4) 
- Institutional delivery
- Postnatal care visit 1 (PNC1)
- BCG vaccine (BCG)
- First pentavalent vaccine (Penta 1)
- Third pentavalent vaccine (Penta 3)

Country specific adaptations may include additional indicators of interest to the country.

#### Detailed analysis steps (DRAFT)
For each indicator, identify the earliest and latest month with reported data across all facilities. 

Create a complete facility-month grid ensuring each facility has records for every month within its valid reporting period. 

Combine the generated grid with actual reported data, preserving service volume values where available. 

Determine whether a facility has reported data for each month.
```r
complete_data[, has_reported := !is.na(count), by = facility_id]

```
Identify when a facility first and last reported for each indicator:
```r
complete_data[, first_report_idx := cumsum(has_reported) > 0, by = facility_id]
complete_data[, last_report_idx := rev(cumsum(rev(has_reported)) > 0), by = facility_id]

```

(…)
Create variable to record complete, incomplete or offline
1 (Complete): A facility has valid service volume data for the given month.
0 (Incomplete): The facility is within its reporting period but missing data.
2 (Offline): A facility is inactive **for more than 6 months** before or after any reporting period.
Filter out offline. Exclude offline months to avoid penalizing facilities that may have opened later or closed earlier.



### Data Quality Assessment (DQA)
A composite measure of data quality provides an overall view of how well a dataset meets quality standards. By integrating multiple dimensions of data quality into a single score, it simplifies the interpretation of detailed information from several measures. This allows health systems to quickly assess the reliability of data, making it easier to identify trends and issues at a glance.

FASTR definition of overall data quality score
For the FASTR analysis, we defined adequate data quality as:
1. No missing indicator data for OPD, Penta1, ANC1, and family planning indicators, where available AND
2. No outliers for OPD, Penta1, ANC1, and family planning indicators, where available AND
3. Consistent reporting between Penta1/Penta3 and ANC1/ANC4.

#### Detailed analysis steps 

This analysis is designed to generate a summary data quality score. There are **insert how many** main steps in this process:


---- 
Last edit 2025 March 5
