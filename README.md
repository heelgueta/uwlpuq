# Urban Wetland Perceptions in Punta Arenas Questionnaire (UWLPUQ) Dataset

This repository contains the Urban Wetland Perceptions in Punta Arenas Questionnaire (UWLPUQ) dataset, which includes survey responses from a probability sample of 297 participants in Punta Arenas, Chile, regarding their perceptions, knowledge, and use of urban wetlands in the city. The data was collected through in-person interviews conducted by trained interviewers.

## Dataset Overview

The UWLPUQ dataset consists of 225 variables organized into the following groups:

1. Context variables
2. General and environmental views
3. Wetlands knowledge and opinions
4. Parque Maria Behety
5. Humedal Tres Puentes
6. Behavior intentions
7. Regional social identification
8. Place attachment
9. Background/demographic variables
10. Weighting

## Data Format

The dataset is provided as an R data frame object (`odf`) saved in an RDS file named "uwlpuq.rds". The data frame has 297 rows (observations) and 225 columns (variables).

## Variable Types and Coding

- Numeric variables: caseid, loclat, loclon, genenv, mddam1-6, natcn1-4, envdf1-7, enbhv1-6, ensac1-3, wlknw1-7, wlim01-15, wlan01-12, wlpl01-07, wlth01-14, wlpri1-4, mbfreq, mbleng, mbac01-09, mbaccs, mbstat, mbbe01-14, mbpr01-08, mblvis, tpfreq, tpleng, tpac01-09, tpaccs, tpstat, tpbe01-14, tppr01-10, tplvis, bint01-10, regid1-4, place1-4, nation, bplace, educat, agepar, weight
- Character variables: prchil, prchot, prpers, prpeot, wlknot, wlanot, wlplot, wlthot, wlrspn, mbacot, mbtran, mbtrot, mpprot, mbnvis, mbnvot, tpacot, tptran, tptrot, tpprot, tpnvis, tpnvot, tpbemi, natiot, bplaot, ethnic, ethnot, emplst, occupa, gender, sexpar, agegrp, educar, encnam
- Date-time variables: startd, finisd
- Factor variable: encnam

Detailed information on the coding and processing of variables can be found in the "Data Cleaning and Processing" section of the metadata.

## Data Cleaning and Processing

The dataset has undergone the following cleaning and processing steps:

- Missing values and responses such as "No sabe (no leer)" and "No responde (no leer)" have been recoded as NA.
- Likert scale items have been converted to numeric values ranging from 1 to 5.
- Frequency items have been converted to numeric values ranging from 0 to 4.
- Other ordinal variables have been converted to numeric values.
- Dichotomous mention variables have been recoded as 0 (not mentioned) and 1 (mentioned).
- The variable 'encnam' has been recoded to consolidate inconsistent interviewer names.

## Usage

To use the UWLPUQ dataset in R, follow these steps:

1. Download the "uwlpuq.rds" file from this repository.
2. Load the RDS file into your R environment using the `readRDS()` function:

```R
odf <- readRDS("uwlpuq.rds")
```

3. Explore the dataset using standard R functions such as `summary()`, `str()`, and `head()`.

## Research Applications

This dataset can be used to explore relationships between environmental attitudes, wetland knowledge and perceptions, wetland use, and socio-demographic characteristics of residents in Punta Arenas, Chile. The data may be particularly useful for understanding how urban populations interact with and value nearby wetland ecosystems.

## License

The UWLPUQ dataset is provided under the [CC BY-NC 4.0 License](https://creativecommons.org/licenses/by-nc/4.0/). You are free to share and adapt the material for non-commercial purposes, provided you give appropriate credit, provide a link to the license, and indicate if changes were made.

## Contact

For more information about the study design, data collection procedures, or specific variable definitions, please contact the original researchers.