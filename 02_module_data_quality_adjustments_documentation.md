---
editor_options:
  markdown:
    wrap: 72
output: pdf_document
---

# Module 2: Data Quality Adjustment

## Background

Ensuring high-quality health service data is important for accurate
decision-making and analysis. However, routine health data often contain
*outliers* (extreme values due to reporting errors) and missing values
(due to incomplete reporting). These issues can distort trends and
impact downstream analyses. The Data Quality Adjustment module addresses
these challenges by applying systematic corrections to raw data.

## Overview

The Data Quality Adjustment module consists of two key components:

1.  Outlier adjustment
2.  Completeness adjustment

### Outlier adjustment

### Completeness adjustment

## **Detailed Analysis Steps**

**Step 1: setup** The `count_working` column is initialized as the
actual service volume (`count`).

**Step 2: Replace outliers**

Method 1: mean with facility-type grouping

-   If `facility_type` data exists, the script calculates the average of
    non-outlier values for the same indicator, month, and facility type.

-   This group-based mean is then used to replace flagged outlier values
    (if row is outlier `count_working` paste the mean in there, if not
    outlier, paste the count

Method 2: 12-month rolling average

If `facility_type` data is not available, the script uses a rolling
average to estimate expected values.

-   The rolling average is calculated over 12 months, using only
    non-outlier values from the same facility and indicator

-   if row is outlier.. `count_working` replaced with this rolling
    average if row is not oulier... paste value from count to
    `count_working`
