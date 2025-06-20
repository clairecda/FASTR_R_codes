# Module 2: Data Quality Adjustment

## Background
Ensuring high-quality health service data is critical for accurate decision-making and analysis. However, routine health management information system (HMIS) data often contain *outliers* (extreme values due to reporting errors, data entry mistakes, or system issues) and missing values (due to incomplete reporting or data collection gaps). These issues can distort trends, mask true patterns, and significantly impact downstream analyses. The Data Quality Adjustment module addresses these challenges by applying systematic, evidence-based corrections to raw data.

## Overview
The Data Quality Adjustment module processes facility-level health service data through two key adjustment components:

1. **Outlier adjustment**: Replaces flagged outlier values with statistically robust estimates
2. **Completeness adjustment**: Fills missing values using historical patterns and averages

The module applies these adjustments across four different scenarios to provide flexibility in analysis:
- **None**: No adjustments applied (raw data)
- **Outliers**: Only outlier adjustment applied
- **Completeness**: Only completeness adjustment applied  
- **Both**: Both outlier and completeness adjustments applied

### Key Features
- Dynamic adjustment logic that adapts to data availability
- Multiple fallback methods to ensure robust replacements
- Preservation of certain sensitive indicators (deaths) from adjustment
- Generation of facility, subnational, and national-level outputs
- Comprehensive tracking of adjustment methods used

## Detailed Analysis Steps

### Step 1: Data Preparation and Setup
- **Input files**: Raw HMIS data, outlier flags (from Module 1), and completeness data (from Module 1)
- **Exclusions**: Certain indicators (`u5_deaths`, `maternal_deaths`) are excluded from adjustment to preserve their original values
- **Working column**: The `count_working` column is initialized with actual service volumes (`count`) and serves as the target for all adjustments

### Step 2: Outlier Adjustment
The outlier adjustment uses an **enhanced rolling average approach** with intelligent fallback mechanisms:

#### Primary Method: 6-Month Rolling Average
The system applies different averaging strategies based on the temporal position of the outlier:

**For early months (first 6 months of data):**
- Uses **forward-looking 6-month average** from subsequent valid (non-outlier, \>0) values
- Method tag: `roll6_forward`

**For recent months (last 6 months of data):**
- Uses **backward-looking 6-month average** from preceding valid values
- Method tag: `roll6_backward`

**For middle months:**
- Uses **centered 6-month average**: 3 months before + 3 months after the outlier
- Method tag: `roll6_center`

#### Fallback Method: Same Month Previous Year
If insufficient valid values exist for rolling averages:
- Replaces outlier with the value from the same month in the previous year
- Only applied if the previous year value is valid (non-outlier, \>0)
- Method tag: `same_month_last_year`
- Includes adjustment note with the reference date

#### Final Fallback: No Adjustment  
If no valid replacement can be found:
- Original outlier value is retained
- Method tag: `unadjusted`
- Note: "no valid replacement"

### Step 3: Completeness Adjustment
The completeness adjustment targets missing values using a **hierarchical replacement strategy**:

#### Primary Method: 12-Month Rolling Average
- Calculates rolling average over 12 months using only valid (non-missing, \>0) values
- **Minimum threshold**: Requires at least 6 valid values within the 12-month window
- Uses centered alignment for balanced temporal representation

#### Fallback Method: Historical Mean
If rolling average cannot be calculated (insufficient valid values):
- Uses the overall mean of all valid values for the same facility-indicator combination
- Provides a stable baseline estimate based on historical performance

#### Data Quality Safeguards
- Only positive values are considered valid for averaging
- Missing values remain missing if no valid replacement can be determined
- Fast processing using `data.table` operations for efficiency

### Step 4: Scenario Processing
The module processes all four adjustment scenarios simultaneously:

1. **None scenario**: Original data with no adjustments
2. **Outliers scenario**: Outlier adjustment only
3. **Completeness scenario**: Completeness adjustment only  
4. **Both scenario**: Sequential application of outlier then completeness adjustments

Each scenario produces a separate `count_final_[scenario]` column in the output.

### Step 5: Geographic Aggregation
The module generates three levels of geographic aggregation:

#### Facility Level (`M2_adjusted_data.csv`)
- Individual facility data with all four adjustment scenarios
- Includes facility metadata and geographic administrative area codes
- Excludes national-level geographic identifier for schema consistency

#### Subnational Level (`M2_adjusted_data_admin_area.csv`)  
- Aggregated to subnational administrative areas (excluding national level)
- Sums facility-level adjusted values by geographic area, indicator, and time period
- Maintains all four adjustment scenarios

#### National Level (`M2_adjusted_data_national.csv`)
- Aggregated to national level (admin\_area\_1 only)
- Provides country-level totals for all indicators and time periods
- Includes all four adjustment scenarios for comparative analysis
---- 

Last edit 2025 May 19
