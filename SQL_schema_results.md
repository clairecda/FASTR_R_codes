## Module 01 - Data Quality Assessment

### Outlier- SQL table schema

``` js
CREATE TABLE ro_m1_output_outliers_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  indicator_common_id TEXT NOT NULL,
  outlier_flag INTEGER NOT NULL
);
```

### Consistency - SQL table schema

Re the `ro_m1_consistency_geo_csv` …. the `admin_area` field is dynamically selected. If the user sets `admin_area_4` but it is not available in the data, the code automatically defaults to the lowest available geographic level (e.g., `admin_area_3`, `admin_area_2`, or `admin_area_1`).

``` js
CREATE TABLE ro_m1_consistency_geo_csv (
  admin_area_* TEXT NOT NULL, -- admin_area_3 default
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  ratio_type TEXT NOT NULL,
  sconsistency INTEGER NOT NULL
);
```

``` js
CREATE TABLE ro_m1_consistency_facility_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  ratio_type TEXT NOT NULL,
  sconsistency INTEGER NOT NULL
);
```

### Completeness - SQL table schema

``` js
CREATE TABLE ro_m1_completeness_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  completeness_flag INTEGER NOT NULL
);
```

### DQA - SQL table schema

``` js
CREATE TABLE ro_m1_dqa_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  dqa_mean NUMERIC NOT NULL,
  dqa_score NUMERIC NOT NULL
);
```

------------------------------------------------------------------------

## Module 02 - Data Quality Adjustment

### Adjusted data (at the facility level) - SQL table schema

``` js
CREATE TABLE ro_m2_adjusted_data_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  indicator_common_id TEXT NOT NULL,
  count_final_none NUMERIC,
  count_final_outliers NUMERIC,
  count_final_completeness NUMERIC,
  count_final_both NUMERIC
);
```

### Adjusted data (aggregated at the smallest geo-level available for example District) - SQL table schema

Re the `ro_m2_adjusted_data_admin_area_csv` below –\> this table aggregates to the lowest available geographic level. In some cases, this includes `admin_area_1`, `admin_area_2`, `admin_area_3`, and `admin_area_4`, depending on what is available in the source data. note for Tim \>\> in some cases we might also have `admin_area_4` as a field !!

``` js
CREATE TABLE ro_m2_adjusted_data_admin_area_csv (
  admin_area_1 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  // admin_area_4 TEXT NOT NULL, -- unsure how to manage this, if admin area 4 not used then drop
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  indicator_common_id TEXT NOT NULL,
  count_final_none NUMERIC,
  count_final_outliers NUMERIC,
  count_final_completeness NUMERIC,
  count_final_both NUMERIC
);
```

### Adjusted data (aggregated at the national-level) - SQL table schema

``` js
CREATE TABLE ro_m2_adjusted_data_national_csv (
  admin_area_1 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  indicator_common_id TEXT NOT NULL,
  count_final_none NUMERIC,
  count_final_outliers NUMERIC,
  count_final_completeness NUMERIC,
  count_final_both NUMERIC
);
```

------------------------------------------------------------------------

## Module 03 - Service Utilization

### Service Utilization (Facility Level) - SQL table schema

``` js
CREATE TABLE ro_m3_service_utilization_csv (
  facility_id TEXT NOT NULL,
  admin_area_3 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  indicator_common_id TEXT NOT NULL,
  count_final_none NUMERIC,
  count_final_outliers NUMERIC,
  count_final_completeness NUMERIC,
  count_final_both NUMERIC
);
```

### Control Chart (tagging disruption) - SQL table schema

``` js
CREATE TABLE ro_m3_chartout_csv (
  admin_area_2 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  tagged INTEGER NOT NULL
);
```

### Disruptions Analysis (National level) - SQL table schema

``` js
CREATE TABLE ro_m3_disruptions_analysis_admin_area_1_csv (
  admin_area_1 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  count_sum NUMERIC,
  count_expect_sum NUMERIC
);
```

### Disruptions Analysis (Sub-National level) - SQL table schema

``` js
CREATE TABLE ro_m3_disruptions_analysis_admin_area_2_csv (
  admin_area_2 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  count_sum NUMERIC,
  count_expect_sum NUMERIC
);
```

### Disruptions Analysis (District level) - SQL table schema

*Note: This output is optional. It will only be generated if the user enables RUN_DISTRICT_MODEL in the script.*

``` js
CREATE TABLE ro_m3_disruptions_analysis_admin_area_3_csv (
  admin_area_3 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  period_id INTEGER NOT NULL,
  quarter_id INTEGER NOT NULL,
  year INTEGER NOT NULL,
  count_sum NUMERIC,
  count_expect_sum NUMERIC
);
```

------------------------------------------------------------------------

## Module 04 - Coverage Estimates - SQL table schema

-   `denominator`: Denominator used (e.g., `danc1_pregnancy`, `ddelivery_livebirth`)\
-   `coverage_original_estimate`: Survey-based estimate (when available)\
-   `coverage_avgsurveyprojection`: Projected survey estimate (when survey data is missing)\
-   `coverage_cov`: Coverage calculated from HMIS data\

``` js
CREATE TABLE ro_m4_coverage_combined_national_csv (
  admin_area_1 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  year INTEGER NOT NULL,
  denominator TEXT NOT NULL,
  coverage_original_estimate NUMERIC,
  coverage_avgsurveyprojection NUMERIC,
  coverage_cov NUMERIC
);
```

### Coverage results (sub-national level) - SQL table schema

For each **indicator** and **year**, this result object is presented as a **histogram**:

- **X-axis:** Names of sub-national areas (`admin_area_2`), ordered from **highest to lowest** coverage.
- **Y-axis:** Coverage estimate (`coverage_cov`), which is the proportion of the target population covered in %.
- **Stratifier:** Coverage can be calculated using several denominator definitions, so the plot could be **faceted by `denominator`** to compare results across different estimation methods.

``` js
CREATE TABLE ro_combined_coverage_province_csv (
  admin_area_1 TEXT NOT NULL,
  admin_area_2 TEXT NOT NULL,
  indicator_common_id TEXT NOT NULL,
  year INTEGER NOT NULL,
  denominator TEXT NOT NULL,
  coverage_cov NUMERIC
);
```
