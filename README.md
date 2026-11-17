# NDL_Fuel_Poverty
This repo prepares a set of contextual data and health outcomes feeding into the Health Foundation's [analysis of fuel poverty and health outcomes in Great Britain](https://www.health.org.uk/reports-and-analysis/analysis/rethinking-fuel-poverty-in-great-britain). It also features codelists which were used to identify inpatient admissions with respiratory, mental health or cardiovascular diagnoses in the Hospital Episodes Statistics - Admitted Patient Care (HES-APC) dataset. 

The repo contains the following scripts:

-	00_Setup_and_functions: Loads any packages required and defines functions which will be used throughout. 
-	01_contextual_variables: Downloads and processes a wide range of geographic, socio-economic and demographic data on LSOAs across GB.
-	02_QOF_variables: Downloads data on the prevalence of a range of health outcomes among GP practice populations, before profiling them to LSOA level. 
-	03_OpenPrescribing_variables: Downloads data on prescriptions by GP practices from the OpenPrescribing database, before profiling them to LSOA level.
-	04_prepare_data_for_import: Combines all data above for import into our secure data environment. 

All further analysis for this project, using secure or proprietary data on health events in secondary care and on fuel poverty, was performed in a secure data environment. 
