# FASTR_Modules

R scripts and test data for FASTR data quality assessment

Description:
This repository contains R scripts and test data for conducting:
A/ Data Quality Assessment using the FASTR methodology. The steps include:
- Identifying and adjusting outliers in service volume data.
- Assessing data consistency across related indicators.
- Evaluating data completeness over time.

B/ Dynamic Adjustments (for outliers and completeness)

C/ Control Charts (tbc)

D/ Disruptions Analysis. The steps include:
- Identifying deviations in service delivery from expected trends (identified using a panel regression that models the relationship between volume, time, and seasonal effects).
- Quantifying these deviations for specific time periods (e.g., during the pandemic)
- Evaluating their significance using statistical methods
The outputs include disruption metrics by facility, indicator, and time.
