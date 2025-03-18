# FASTR Modules

R scripts and test data for FASTR data quality assessment

Description:
This repository contains R scripts and test data for conducting:

A/ Data Quality Assessment using the FASTR methodology. The steps include:
- Identifying and adjusting outliers in service volume data.
- Assessing data consistency across related indicators.
- Evaluating data completeness over time.

B/ Dynamic Adjustments (for outliers and completeness)

C/ Disruptions Analysis using the FASTR methodology.
This module analyzes disruptions in essential service delivery using a two-step approach:
- The control chart method is applied to historical service data to detect unusual deviations from expected trends. This step:

Flags periods of significant variation in service delivery.
Accounts for seasonal patterns and long-term trends.
Generates binary flags (tagged) indicating potential disruptions.

- Once disruptions are identified, regression models are used to:

Quantify how much service delivery deviated from expected levels.
Estimate the effect size of disruptions (e.g., pandemic impact).
Assess statistical significance of deviations.
Model trends at national, province, and district levels.
