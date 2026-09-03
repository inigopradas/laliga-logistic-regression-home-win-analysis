# Combined-Season Binomial Formation Results

This directory contains the documents reporting the comparative results of the
binomial logistic regression analyses focused on tactical formations.

The analyses compare the results obtained using the single-season database with
those obtained using the multi-season database.

Only formation-related analyses are included in this directory. Results from
the offensive, defensive, physical, match-control, contextual and general block
models are stored in their corresponding result directories.

## Databases

The comparative documents use two databases.

### 2024/2025 season

The single-season database contains match-level observations from the
2024/2025 LaLiga season.

The database is generally imported into R using the object name:

`d`

The original Excel file is stored in:

`01_data/2024_2025_season/variables_Estudio (9).xlsx`

### 2022/2023 to 2024/2025 seasons

The multi-season database contains match-level observations from:

- 2022/2023
- 2023/2024
- 2024/2025

The database is generally imported into R using the object name:

`d1`

The original Excel file is:

`LaLiga_22-25_completo_v2 (2).xlsx`

## Important sample-overlap consideration

The 2024/2025 season is included in both databases.

Therefore, the single-season and multi-season databases are not independent
samples.

The comparisons evaluate how the formation results change when the 2024/2025
analysis is expanded by adding the 2022/2023 and 2023/2024 seasons.

The results should not be interpreted as comparisons between two completely
independent periods.

## Dependent variable

The dependent variable indicates whether the home team won the match:

- `1`: home-team victory.
- `0`: no home-team victory, including draws and away-team victories.

Depending on the data-preparation stage, the variable may be stored as:

- `win_local`
- `win_local_num`

## Statistical method

The reported results are obtained using binary logistic regression with a logit
link.

The models are estimated using `glm()` with:

```r
family = binomial(link = "logit")
