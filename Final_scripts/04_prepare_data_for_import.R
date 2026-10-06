# # Written in R-4.3.1

# Check that all necessary previous scripts have been run
if (exists('SETUP_RUN')) { print(SETUP_RUN)
} else {source('Final_scripts/00_Setup_and_functions.R')}

################################################################################
## 04. Combine all variables
#
# Here we combine the outputs of all previous scripts into a large single csv, ready for import into the SDE


# Check if outputs of previous scripts have been produced - if not, previous scripts will run

if (file.exists('Outputs/all_GB_contextual_variables.csv')){ print('Contextual variables already produced')
} else {source('Final_scripts/01_contextual_variables.R')}

if (file.exists('Outputs/qof_by_lsoa.csv')){ print('QOF variables already produced')
} else {source('Final_scripts/02_QOF_variables.R')}

if (file.exists('Outputs/all_drugs_lsoa.csv')){ print('OpenPrescribing variables already produced')
} else {source('Final_scripts/03_OpenPrescribing_variables.R')}


contextual_variables <- read_csv('Outputs/all_GB_contextual_variables.csv') # Expected size: 43,064 LSOAs, all LSOAs/DZs in GB
qof_variables <- read_csv('Outputs/qof_by_lsoa.csv') %>% filter(LSOA_code != 'EMPTY')     # Expected size: 33,755, all LSOAs in England 
prescribing_variables <- read_csv('Outputs/all_drugs_lsoa.csv') %>% filter(!(LSOA_code %in% c('EMPTY', 'CLOSED'))) # Expected size: 33,755, all LSOAs in England 


# Join all datasets

all_data_for_SDE <- left_join(contextual_variables, qof_variables, by = 'LSOA_code') %>%
  left_join(., prescribing_variables, by = 'LSOA_code') %>%
  select(-c(`...1.x`, `...1.y`, `...1`))

# Examine dataset to check if matches expectations
# Contextual variables aside from ethnicity should all have no NAs; ethnicity, QOF, and OpenPrescribing variables should 
# all have 9,309 NAs, as they are all England only. 

summary(all_data_for_SDE)

# Write CSV of all data for SDE

write.csv(all_data_for_SDE, 'Outputs/all_fp_data_for_SDE.csv')
