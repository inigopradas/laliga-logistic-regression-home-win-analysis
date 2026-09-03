# 2022/2023 to 2024/2025 Seasons Dataset

This directory contains the original dataset used for the multi-season analysis
of LaLiga matches from the 2022/2023, 2023/2024 and 2024/2025 seasons.

## Dataset

- `LaLiga_22-25_completo_v2 (2).xlsx`

## Description

The dataset contains match-level observations and variables used in the
binomial and multinomial logistic regression analyses of home-team match
outcomes across three LaLiga seasons.

The analyses include offensive, defensive, physical, match-control, contextual
and tactical variables. The dataset also contains information about the initial
formations used by the home and visiting teams.

Some variables contain missing values because the corresponding information
was not collected for every match or season. The original missing values are
preserved in this file.

## Seasons included

- 2022/2023
- 2023/2024
- 2024/2025

## Data policy

The original Excel file must not be modified or overwritten.

Any cleaned, recoded, imputed or transformed version of the dataset should be
stored in the corresponding processed-data directory.

The treatment of missing values and the construction of derived variables
should be performed through reproducible R scripts and documented in the
project metadata.

## Main R object

The dataset is generally imported into R using the object name:

`d1`

## Main derived variables

The analysis scripts may create derived variables such as:

- `win_local_num`
- `resultado_partido_local`
- `diff_descanso`
- `no_descanso_previo`
- `formacion_local_dep`
- `formacion_visit_dep`

These derived variables are not part of the immutable original dataset unless
they were already present in the Excel file.

## Important methodological note

The 2024/2025 season is included in this multi-season dataset and is also
analysed separately in the single-season dataset. Therefore, the single-season
and multi-season samples are not independent.
