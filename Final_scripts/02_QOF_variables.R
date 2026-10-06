# # Written in R-4.3.1

# Check that all necessary previous scripts have been run
if (exists('SETUP_RUN')) { print(SETUP_RUN)
} else {source('Final_scripts/00_Setup_and_functions.R')}


## 02. QOF variables

# Here we download a set of data form the Quality and Outcomes Framework dataset from NHS England, which records a wide set of metrics for 
# GP practices in England. We import six variables from QOF - prevalence of athma, COPD, depression, mental health and rheumatoid arthritis
# among the practice's patient population (which we're interested in as outcomes of FP), and registered smokers among the patient population
# (which we're interested in as a control variable)

############################
#### Introduce QOF data 
############################

# Here, we download the QOF data before profiling to LSOAs based on the geographic distribution of a practice's patients (see below).

# Record codes of QOF variables of interest
qof_codes <- c('AST', 'COPD', 'DEP', 'MH', 'RA', 'SMOK')

# Create QOF data directory if not already created
if(dir.exists('Data/QOF')) {print('QOF data directory already exists.')
} else {dir.create('Data/QOF')}

# Download zip file of QOF prevalence metrics, plus smoking data
if (file.exists('Data/QOF/QOF2526.zip') & file.exists('Data/QOF/smoking_QOF_data.xlsx')){print('QOF data already downloaded')
} else {
  
  raw_zip_url <- 'https://files.digital.nhs.uk/C6/77B386/QOF%202526.zip'
  
  download.file(url = raw_zip_url, destfile = 'Data/QOF/QOF2526.zip')
  
  unzip('Data/QOF/QOF2526.zip', exdir = 'Data/QOF/QOF_unzipped')
  
  smoking_QOF_url <- 'https://files.digital.nhs.uk/49/BCDC28/qof-2526-prac-prev-ach-pca-ls.xlsx'
  
  download.file(url = smoking_QOF_url, destfile = 'Data/QOF/smoking_QOF_data.xlsx', 
                mode = 'wb')
  
  }

# Load in QOF prevalence metrics and select relevant variables
all_qof <- read_csv('Data/QOF/QOF_unzipped/PREVALENCE_2526.csv') %>%
  filter(GROUP_CODE %in% qof_codes) %>%
  select(PRACTICE_CODE, GROUP_CODE, REGISTER_SIZE, PRACTICE_LIST_SIZE)

# Load in smoking information separately from the rest, as this comes from a different QOF table.
# This will generate a bunch of parsing warnings when it's loaded, but don't worry - it's just because it's an
# awkwardly formatted excel file, and the columns we're using aren't troublesome
smoking_QOF <- read_excel('Data/QOF/smoking_QOF_data.xlsx', sheet = 'SMOK', skip = 11) %>%
  select(PRACTICE_CODE = 4, PRACTICE_LIST_SIZE = 7, REGISTER_SIZE = 28) %>%
  mutate(PRACTICE_LIST_SIZE = as.numeric(PRACTICE_LIST_SIZE),
         REGISTER_SIZE = as.numeric(REGISTER_SIZE),
         GROUP_CODE = 'SMOK')

# Bind prevalence and smoking metrics together
all_qof <- rbind(all_qof, smoking_QOF)


#######################################
### PROFILING TO LSOAs
#######################################

# NHS England publishes data on the LSOAs of residence of the patient population for each GP practice in England.
# This allows us to estimate how practice-level measures, such as the QOF metrics downloaded above, are spread between 
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


# Create list of QOF metrics

qof_list <- lapply(1:length(qof_codes), function(i){df <- all_qof %>% filter(GROUP_CODE == qof_codes[[i]])})

#  Profile to LSOAs
# This function joins the GP patients by LSOA data to the QOF data by practice code, then scales each QOF metric by the 
# proportion of the LSOA's populatio nthat that given practice accounts for. These scaled metrics are then summed for each LSOA
# to produce an estimate of that metric's prevalence in each LSOA.  

join_practices_qof <- function(practices_df, QOF_df){ 
  
  practices_joined <- left_join(practices_df, QOF_df, by = join_by(PRACTICE_CODE)) %>%
    mutate_at(c('REGISTER_SIZE', 'PRACTICE_LIST_SIZE'), ~replace_na(.,0)) %>%
    mutate(register_per_LSOA = REGISTER_SIZE*PATIENT_PROPORTION,
           list_per_lsoa = PRACTICE_LIST_SIZE*PATIENT_PROPORTION)
  
  LSOA_summed <- practices_joined %>%
    group_by(LSOA_CODE) %>%
    summarise(register = sum(register_per_LSOA), list = sum(list_per_lsoa), NUMBER_OF_PATIENTS = sum(NUMBER_OF_PATIENTS))
  
  return(LSOA_summed)
}

# Here we apply the above function to the QOF data, running through each metric one by one
qof_by_lsoa <- lapply(1:length(qof_codes), function(i){
  df <- join_practices_qof(practices_df = gp_patients_data, QOF_df = as.data.frame(qof_list[[i]])) %>%
    mutate(metric = qof_codes[[i]]) %>%
    filter(grepl('^E', LSOA_CODE))
})

# We then combne the results of that into a single df
# This should produce a dataframe with register size, list size, and prevalence of each metric 
# for the 33,755 LSOAs in England. It will have one excess row - this si expected, as som epatients may be registered
# as living in defunct LSOAs or may live in Scotland or Wales

qof_by_lsoa_all <- do.call('rbind', qof_by_lsoa) %>%
  select(LSOA_CODE, metric, register, list) %>%
  mutate(prevalence = register/list) %>%
  pivot_wider(names_from = metric, values_from = c(register, list, prevalence)) %>%
  rename(LSOA_code = LSOA_CODE)

# Save results 

write.csv(qof_by_lsoa_all, 'Outputs/qof_by_lsoa.csv')

rm(list=ls())
