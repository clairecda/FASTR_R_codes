## Module 01 - Data Quality Assessment

### Outlier- SQL table schema

``` js
CREATE TABLE ro_m1_output_outliers_csv (
  facility_id text NOT NULL,
  period_id integer NOT NULL,
  indicator_common_id text NOT NULL,
  outlier_flag integer NOT NULL
);
```

### Consistency - SQL table schema

Re the `ro_m1_consistency_geo_csv` …. the `admin_area` field is dynamically selected. If the user sets `admin_area_4` but it is not available in the data, the code automatically defaults to the lowest available geographic level (e.g., `admin_area_3`, `admin_area_2`, or `admin_area_1`).

``` js
CREATE TABLE ro_m1_consistency_geo_csv (
  admin_area text NOT NULL,
  period_id integer NOT NULL,
  ratio_type text NOT NULL,
  sconsistency integer NOT NULL
);
```

``` js
CREATE TABLE ro_m1_consistency_facility_csv (
  facility_id text NOT NULL,
  period_id integer NOT NULL,
  ratio_type text NOT NULL,
  sconsistency integer NOT NULL
);
```

### Completeness - SQL table schema

``` js
CREATE TABLE ro_m1_completeness_csv (
  facility_id text NOT NULL,
  indicator_common_id text NOT NULL,
  period_id integer NOT NULL,
  completeness_flag integer NOT NULL
);
```

### DQA - SQL table schema

``` js
CREATE TABLE ro_m1_dqa_csv (
  facility_id text NOT NULL,
  period_id integer NOT NULL,
  dqa_mean numeric NOT NULL,
  dqa_score numeric NOT NULL
);
```
