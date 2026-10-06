# # Written in R-4.3.1

# Check that all necessary previous scripts have been run
if (exists('SETUP_RUN')) { print(SETUP_RUN)
} else {source('Final_scripts/00_Setup_and_functions.R')}


################################################################################################################
## 3. OpenPrescribing variables
##
## NOTE: The intention of this script is to directly download data on drug prescriptions from OpenPrescribing.
## However, the code used to download the data has not been working lately. It starts to download a few months of data,
## then we're hit with an "R encountered a fatal error". It's possible that the OpenPrescribing API has become more 
## stringent about rejecting numerous, high-velocity requests, or maybe our own systems don't like it.
## Either way, it makes the original method of downloading the data unworkable, which isn't great for replicability, 
## and I've had to manually download the data and upload it to GitHub. If you want to try the original code and see if it 
## works for you - I've left it be for transparency, and in case it ever starts working again - 
## set run_original_code to 'YES'. Otherwise, leave as 'NO' and it will just pull the manually compiled data uploaded to GitHub. 

## This issue only affects the early data download stage. Following this, practice-level prescribing measures are profiled to LSOAs.
## This profiling is done in the same manner as the QOF variables, using the Patients Registered in a GP practice dataset


run_original_code <- 'NO'

################################################################################################################


if (run_original_code != 'YES') { 
  
op_url <- getURL('https://raw.githubusercontent.com/tom-prendergast-thf/NDL_Fuel_Poverty/refs/heads/main/Data/OpenPrescribing/all_drugs_grouped.csv')
  
all_drugs_grouped <- read.csv(text = op_url)    
  

} else {


####################
########### INHALERS
####################

# Create prescription data directory if not present

if(dir.exists('Data/OpenPrescribing')){print('Prescription data directory already exists')
  } else { dir.create('Data/OpenPrescribing')}

# Create vector of dates for the period of interest, FY 25-26
dates <- list('2025-04-01', '2025-05-01', '2025-06-01', '2025-07-01', '2025-08-01', '2025-09-01', 
              '2025-10-01', '2025-11-01', '2025-12-01', '2026-01-01', '2026-02-01', '2026-03-01')

# List of BNF sub-chapters of interest - 0301 (bronchodilators) and 0302 (respiratory corticosteroids)
inhaler_codelist <- c('0301', '0302')

# This function directly downloads practice-level prescriptions data for a given set of BNF codes across a given timeframe from the OpenPrescribing
# database. It does this month by month, before combining that month by month data into a single df.
read_presc_function <- function(codes, drug_type, dates){
  
  full_list <- list()
  
  for (i in 1:length(codes)){
    
    minilist <- list()
    
    for (x in 1:length(dates)){
      
      download.file(url = paste0("https://openprescribing.net/api/1.0/spending_by_org/?org_type=practice&code=", codes[[i]], "&date=", dates[[x]],"&format=csv"),
                    destfile = paste0('Data/OpenPrescribing/', drug_type, '_', dates[[x]], '.csv'))
      
      
      single_df <- read_csv(paste0('Data/OpenPrescribing/', drug_type, '_', dates[[x]], '.csv'),
                            col_types = list(ccg = col_character(),
                                             row_id = col_character(),
                                             row_name = col_character(),
                                             actual_cost = col_double(),
                                             items = col_double(),
                                             quantity = col_double(),
                                             setting = col_character(),
                                             date = col_date()
                            ))
      
      single_df$bnf_code <- codes[[i]]
      
      single_df$drug <- drug_type[[i]]
      
      minilist <- append(minilist, list(single_df))
      
    }
    
    combined_df <- bind_rows(minilist)
    
    full_list <- append(full_list, list(combined_df))
    
  }
  
  return(full_list)
  
}

full_inhaler_list <- read_presc_function(codes = inhaler_codelist, drug_type = 'inhaler', dates = dates)

# Create df

full_inhaler_df <- bind_rows(full_inhaler_list)

# Group df

inhalers_grouped <- full_inhaler_df %>%
  group_by(drug, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


########################
########### ANTI ANXIETY
########################


#Read in BNF code file

full_antianx_list <- read_presc_function(codes = list('040102'), drug_type = 'antianx', dates = dates)

# Create df
antianx_df <- full_antianx_list[[1]]

# Group df
antianx_grouped <- antianx_df %>%
  group_by(drug, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


#############################
########### ANTI DEPRESSANTS
#############################

#Read in BNF code file

full_antidepress_list <- read_presc_function(codes = list('0403'), drug_type = 'antidep', dates = dates)

# Create df
antidepress_df <- full_antidepress_list[[1]]

#Group df
antidepress_grouped <- antidepress_df %>%
  group_by(drug, row_id, row_name) %>%
  summarise(total_cost = sum(actual_cost), total_items = sum(items), total_quantity = sum(quantity))


all_drugs_grouped <- rbind(inhalers_grouped, antianx_grouped, antidepress_grouped)

}

######################################################################################################
#### PROFILE TO LSOAs
######################################################################################################

# The below code will only re-download the GP patients data if the data is not already present - as such,
# if you have already run the QOF script nothing new will be downloaded here. 

# Context of data:
# NHS England publishes data on the LSOAs of residence of the patient population for each GP practice in England.
# This allows us to estimate how practice-level measures, such as the prescribing metrics downloaded above, are spread between 
# LSOAs. Key assumption is that measures are spread evenly across the practice's patient population. 

# Introduce GP patients data

if(dir.exists('Data/GP_practice_data')) {print('GP practice data directory already exists.')
} else {dir.create('Data/GP_practice_data')}

if (file.exists('Data/GP_practice_data/GPApr26.zip')){print('GP practice data already downloaded')
} else {
  
  GP_zip_url <- 'https://files.digital.nhs.uk/D8/54ED89/gp-reg-pat-prac-lsoa-male-female-Apr-26.zip'
  
  download.file(url = GP_zip_url, destfile = 'Data/GP_practice_data/GPApr26.zip')
  
  unzip('Data/GP_practice_data/GPApr26.zip', exdir = 'Data/GP_practice_data/GP_Apr_26_unzipped')
  
}

# Import GP registered patients data and produce scaling factor for practice-level metrics
gp_patients_data <- read_csv('Data/GP_practice_data/GP_Apr_26_unzipped/gp-reg-pat-prac-lsoa-all.csv') %>%
  filter(SEX == 'ALL') %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS) 


## Now we distribute the various drugs between LSOAs based on the geographic distribution of their patient population

####### Joining function

join_practices_prescriptions <- function(practices_df = gp_patients_data, prescription_df){ 
  practices_joined <- left_join(practices_df, prescription_df, join_by(PRACTICE_CODE == row_id), relationship = "many-to-many") %>%
    mutate_at(c('total_items', 'total_quantity', 'total_cost'), ~replace_na(.,0)) %>%
    mutate(items_per_LSOA = total_items*PATIENT_PROPORTION) %>%
    mutate(quantity_per_LSOA = total_quantity*PATIENT_PROPORTION) %>%
    mutate(cost_per_LSOA = total_cost*PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE, drug) %>%
    summarise(items = sum(items_per_LSOA), quantity = sum(quantity_per_LSOA), cost = sum(cost_per_LSOA))
  
  return(LSOA_summed)
}

### Run function 

all_drugs_lsoa_long <- join_practices_prescriptions(prescription_df = all_drugs_grouped) %>%
  filter((grepl('^E', LSOA_CODE) | LSOA_CODE == 'CLOSED') & (!is.na(drug))) 

# This should result in a 101,271 row dataset - 3X 33,755 LSOAs, plus 3x EMPTY and CLOSED
# The EMPTY and CLOSED designations are kept to assess what % of prescriptions/spending are unaccounted for

all_drugs_lsoa <- all_drugs_lsoa_long %>%
  pivot_wider(names_from = drug, values_from = c(items, quantity, cost)) %>%
  rename(LSOA_code = LSOA_CODE)

write.csv(all_drugs_lsoa, 'Outputs/all_drugs_LSOA.csv')

## Check for what % of items, spending and quantity the EMPTY and CLOSED lines account for

all_drugvars_summed <- all_drugs_lsoa %>% ungroup() %>% summarise_if(is.numeric, sum)

all_drugvars_empty <- all_drugs_lsoa %>% ungroup() %>% filter(LSOA_code == 'EMPTY') %>% select(-LSOA_code)

all_drugvars_closed <- all_drugs_lsoa %>% ungroup() %>% filter(LSOA_code == 'CLOSED') %>% select(-LSOA_code)

perc_empty <- (all_drugvars_empty/all_drugvars_summed) %>% mutate(code = 'EMPTY')

perc_closed <- (all_drugvars_closed/all_drugvars_summed) %>% mutate (code = 'CLOSED')

empty_closed_check <- rbind(perc_empty, perc_closed)

write.csv(empty_closed_check, 'Outputs/OpenPrescribing_emptylsoa_check.csv')

rm(list=ls())
