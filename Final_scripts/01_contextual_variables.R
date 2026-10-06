
# # Written in R-4.3.1

# 1. Contextual variables

# Here we download and process a set of contextual variables for LSOAs (in England and Wales) and Datazones (Scotland)
# We 
# - LSOAs/DZs and the larger geographies they fit into (Source: OpenGeography Portal for Eng/Wal, Scottish Government for Sct)
# - LSOA/DZ populations by age, mid-year 2024 (Source: ONS for Eng/Wal, National Records of Scotland for Sct)
# - IMD, WIMD, and SIMD scores, 2025 (Source: MHCLG for Eng, Welsh Government for Wales, Scottish Government and National Records of Scotland for Sct)
# - Urban/rural (Source: OpenGeography Portal for Eng/Wal, NHS Scotland for Sct)
# - Parliamentary constituencies (Source: National Data Library for Eng/Wal, Scottish government for Sct)
# - Population split by ethnicity as of 2021 Census (Source: ONS Census 2021, accessed through Census API)

# All data is directly downloaded by the script if not already present in the working directory 

# This function checks if the setup and packages script has been run, and runs it if needed
if (exists('SETUP_RUN')) { print(SETUP_RUN)
  } else {source('Final_scripts/00_Setup_and_functions.R')}




#####################################################################################
################# LSOAs, MSOAs and LAs
#####################################################################################

# Here we gather the full list of LSOAs (in England/Wales) and DataZones (equivalent geography in Scotland) and 
# the larger MSOAs and Local Authorities they are nested in. This list of LSOAs/Datazones form the canonical units
# for the rest of the analysis - all else will ultimately be joined to these.

if ((dir.exists('Data/LSOAs'))){
  print('LSOA data directory already exists.')
} else {                                          # Checks for existence of populations data sub-folder
  # and creates it if necessary   
  dir.create('Data/LSOAs')  
}

#####################
# England/Wales
####################

# This query will take a little while if not already downloaded because the Open Geography Portal makes it bizarrely difficult to directly link to their data.
# It's easiest to download as a GeoJSON including geometries, which is why the file is so large. 

# Download Eng/Wales population excel file if not present
if ((file.exists('Data/LSOAs/eng_wales_lsoa.geojson'))) {
  print('England/Wales LSOA data already downloaded.')
} else {   
  
  
  eng_wal_lsoas <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
  
  
  sf::write_sf(eng_wal_lsoas, 'Data/LSOAs/eng_wales_lsoa.geojson')
} 

# Lookup is at output area level, so we isolate every unique combination of LSOA, MSOA and LA to create our final LSOA list. This should result in 35,672 LSOAs,
# as each LSOA should only fit into one MSOA and one LA. 
eng_wal_lsoas <- read_sf('Data/LSOAs/eng_wales_lsoa.geojson') %>%
  select(LSOA_code = LSOA21CD, LSOA_name = LSOA21NM, MSOA_code = MSOA21CD, MSOA_name = MSOA21NM, LA_code = LAD22CD, LA_name = LAD22NM) %>%
  distinct() %>%
  st_drop_geometry() %>%
  as.data.frame()

############
# Scotland
############

# We rename data zones to LSOAs and Intermediate zones to MSOAs here for uniformity with England and Wales
sct_dzs <- read_csv('https://statistics.gov.scot/downloads/file?id=9a072ac6-8511-4b90-b00c-cd56698f359e%2FDataZone2022lookup_2026-05-07.csv') %>%
  select(LSOA_code = DZ22_Code, LSOA_name = DZ22_Name, MSOA_code = IZ22_Code, MSOA_name = IZ22_Name, LA_code = LA_Code, LA_name = LA_Name)


### Combine all GB
# All going well this should result in 43,064 LSOAs

all_GB_lsoas <- rbind(eng_wal_lsoas, sct_dzs)







######################################################################################
####### POPULATION DATA
######################################################################################

if ((dir.exists('Data/populations'))){
  print('Population data directory already exists.')
} else {                                          # Checks for existence of populations data sub-folder
                                                        # and creates it if necessary   
  dir.create('Data/populations')  
}

# Download Eng/Wales population excel file if not present
if ((file.exists('Data/populations/eng_wales_lsoa_pop.xlsx'))) {
  print('England/Wales population data already downloaded.')
} else {   

eng_wales_pop_url <-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx'


download.file(url = eng_wales_pop_url, destfile = 'Data/populations/eng_wales_lsoa_pop.xlsx',
              mode = 'wb')
} 

# Download Scotland population excel file if not present

if ((file.exists('Data/populations/sct_dz_pop.xlsx'))) { 
  print('Scotland population data already downloaded.')
} else {   
  
  sct_pop_url <-'https://www.nrscotland.gov.uk/media/efyoyacy/sape-2024-22dz-data.xlsx'
  
  download.file(url = sct_pop_url, destfile = 'Data/populations/sct_dz_pop.xlsx',
                mode = 'wb')
} 


#####################################
### Process England and Wales data
#####################################

# Create vectors for three age categories (0-17, 18-64 (working age), 65 and over)
# This is necessary for aggregating the raw ENG/WAL data (in individual year split by sex) into broader age groups
# Data is left split by sex in case age/sex standardisation is desirable at any point 

m_to_17 <- sapply(0:17, function(i){paste0('M', i)})
f_to_17 <- sapply(0:17, function(i){paste0('F', i)})
m_wa <- sapply(18:64, function(i){paste0('M', i)})
f_wa <- sapply(18:64, function(i){paste0('F', i)})
m_65plus <- sapply(65:90, function(i){paste0('M', i)})
f_65plus <- sapply(65:90, function(i){paste0('F', i)})

engwal_pop <- read_excel('Data/populations/eng_wales_lsoa_pop.xlsx', 
                            sheet = 'Mid-2024 LSOA 2021', skip = 3) %>%
  pivot_longer(5:187, names_to = 'cat', values_to = 'value') %>%
  mutate(agg_cat = case_when(cat %in% m_to_17 ~ 'm_to_17', 
                             cat %in% f_to_17 ~ 'f_to_17', 
                             cat %in% m_wa ~ 'm_wa', 
                             cat %in% f_wa ~ 'f_wa', 
                             cat %in% m_65plus ~ 'm_65plus',
                             cat %in% f_65plus ~ 'f_65plus',
                             cat == 'Total' ~ 'Total')) %>%
  group_by(`LSOA 2021 Code`, agg_cat) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = agg_cat, values_from = value) %>%
  mutate(to_17 = m_to_17 + f_to_17,
         wa_18_to_64 = m_wa + f_wa,
         over_65 = m_65plus + f_65plus, 
         perc_to_17 = to_17/Total,
         perc_wa = wa_18_to_64/Total,
         perc_65plus = over_65/Total) %>%
  rename(LSOA_code = 1)

################################
## Process Scotland data
################################

# Create vectors for variable names
sct_to_17 <- sapply(0:17, function(i){paste0('Age ', i)})
sct_wa <- sapply(18:64, function(i){paste0('Age ', i)})
sct_65plus <- sapply(65:100, function(i){paste0('Age ', i)})


sct_pop <- read_excel('Data/populations/sct_dz_pop.xlsx', 
                         sheet = '2024', skip = 2) %>%
  pivot_longer(6:97, names_to = 'cat', values_to = 'value')  %>%
  mutate(agg_cat = case_when(cat %in% sct_to_17 & Sex == 'Males' ~ 'm_to_17', 
                             cat %in% sct_to_17 & Sex == 'Females' ~ 'f_to_17', 
                             cat %in% sct_to_17 & Sex == 'Persons' ~ 'to_17',
                             
                             cat %in% sct_wa & Sex == 'Males'  ~ 'm_wa', 
                             cat %in% sct_wa & Sex == 'Females' ~ 'f_wa', 
                             cat %in% sct_wa & Sex == 'Persons' ~ 'wa_18_to_64',
                             
                             (cat %in% sct_65plus | cat == 'Age 90 and over') & Sex == 'Males'  ~ 'm_65plus',
                             (cat %in% sct_65plus | cat == 'Age 90 and over') & Sex == 'Females' ~ 'f_65plus',
                             (cat %in% sct_65plus | cat == 'Age 90 and over') & Sex == 'Persons' ~ 'over_65',
                             
                             cat == 'Total population' ~ 'Total')) %>%
  group_by(`Data zone code`, agg_cat) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = agg_cat, values_from = value) %>%
  mutate(perc_to_17 = to_17/Total,
         perc_wa = wa_18_to_64/Total,
         perc_65plus = over_65/Total) %>%
  rename(LSOA_code = 1)

######################
# Combine populations 
######################

all_GB_pop <- rbind(engwal_pop, sct_pop)







###########################################################################
##################### IMPORT AND PROCESS IMD 
###########################################################################

#############
## England 
#############

# For England, both regular IMD scores and exponential transformations of each constituent domain of IMD must be imported
# The transformed IMD scores are used to calculate alternative IMD scores with income excluded, health excluded, and both excluded
# These will eventually be used for the matching/regression processes and sensitivity analyses. Re-weighting domains to produce alternative scores
# is performed in line with the MHCLG methodology outlined in the IMD 2025 research report and technical report, available here:
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025

# Create IMD sub-folder if it does not exist
if ((dir.exists('Data/IMD'))){
  print('IMD data directory already exists.')
} else {                                 # Checks for existence of IMD data sub-folder
                                          # and creates it if necessary   
  dir.create('Data/IMD')  
  
} 

# Download Eng IMD excel file if not present
if ((file.exists('Data/IMD/ENG_IMD_transformed_scores.xlsx'))) {
   print('England transformed IMD data already downloaded.')
} else {   
  
  eng_IMD_transformed_url <- 'https://assets.publishing.service.gov.uk/media/691ded670dcbf6343e9a2a6c/File_9_IoD2025_Transformed_Domain_Scores.xlsx'
  
  
  download.file(url = eng_IMD_transformed_url, destfile = 'Data/IMD/ENG_IMD_transformed_scores.xlsx',
                mode = 'wb')
} 

# Import English IMD
eng_IMD <- read_csv('https://assets.publishing.service.gov.uk/media/691ded56d140bbbaa59a2a7d/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv') %>%
  select(LSOA_code = 1, IMD_Country_Decile = 1)

# Create df of domain weights (incl alternative weights for no income, no health IMD) in line with MHCLG methodology
IMD_domain_weights <- data.frame(domain = c('Income', 'Employment', 'Education', 'Health', 'Crime', 'Barriers', 'Environment'),
                                 weights_standard = c(0.225, 0.225, 0.135, 0.135, 0.093, 0.093, 0.093),
                                 weights_noincome = c(0, 0.225, 0.135, 0.135, 0.093, 0.093, 0.093),
                                 weights_nohealth = c(0.225, 0.225, 0.135, 0, 0.093, 0.093, 0.093),
                                 weights_nohi = c(0, 0.225, 0.135, 0, 0.093, 0.093, 0.093))

# Import transformed scores and produce alternative versions of IMD using the weights defined above
# A regular IMD score is calculated here too for validation. Spot checks indicate that 
eng_IMD_transformed <- read_excel('Data/IMD/ENG_IMD_transformed_scores.xlsx', sheet = 'IoD25 Transformed Domain Scores') %>%
  rename(LSOA_code = 1, LSOA_name = 2, Income = 5, Employment = 6, Education = 7, Health = 8, Crime = 9, Barriers = 10, Environment = 11) %>%
  pivot_longer(5:11, names_to = 'domain', values_to = 'score') %>%
  left_join(., IMD_domain_weights, by = 'domain') %>%
  mutate(score_standard = score*weights_standard,
         score_noincome = score*weights_noincome,
         score_nohealth = score*weights_nohealth,
         score_nohi= score*weights_standard) %>%
  group_by(LSOA_code) %>%
  summarise(score_standard = sum(score_standard), 
            score_noincome = sum(score_noincome), 
            score_nohealth = sum(score_nohealth), 
            score_nohi = sum(score_nohi)) %>%
  ungroup() %>%
  mutate(decile_standard = ntile(-score_standard, 10),
         decile_noincome = ntile(-score_noincome, 10),
         decile_nohealth = ntile(-score_nohealth, 10),
         decile_nohi = ntile(-score_nohi, 10))
  
###############
### Wales
###############

if ((file.exists('Data/IMD/wales_imd.ods'))){
  print('Wales IMD data already downloaded.')
} else {   
  
  wales_IMD_url <- 'https://www.gov.wales/sites/default/files/statistics-and-research/2025-11/wimd-2025-index-and-domain-ranks-by-small-area.ods' 
  
  
  download.file(url = wales_IMD_url, destfile = 'Data/IMD/wales_imd.ods',
                mode = 'wb')
} 

wales_IMD <- read_ods('Data/IMD/wales_imd.ods', sheet = 'Deciles_quintiles_quartiles', skip = 3) %>%
  select(LSOA_code = 1, IMD_Country_Decile = 5)

###############
### Scotland
###############

## Scottish IMD scores are only available for 2011 Data Zones, not for the more recent 2022 Data Zones which are used elsewhere in this project.
## As such, SIMD scores are here profiled to 2022 DZs by first assigning each output area (the smallest geographic unit) the IMD score of the 2011 DZ it is part of. 
## These are then re-aggregated back into 2022 DZs, and the DZ received the SIMD score of the plurality of OAs within it. 
# Awkwardly, several datasets must be downloaded to do this - the DZ2011 IMD scores, OA22 to DZ11 lookup, and OA22 to DZ22 lookup.
# The function below downloads all of these if any are not present in the working directory

# Profile populations and IMD to 2022 DZs

if ((file.exists('Data/IMD/sct_imd.xlsx') & file.exists('Data/IMD/sct_oa22_to_dz22.zip') & file.exists('Data/IMD/sct_census_index.zip'))){
  print('Scotland IMD data already downloaded.')
} else {   
  
  sct_IMD_url <- 'https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx'
  
  sct_oa22_to_dz22_url <- 'https://www.nrscotland.gov.uk/media/iz3evrqt/oa22_dz22_iz22.zip'
  
  sct_census_index_url <- 'https://www.nrscotland.gov.uk/media/utrbt5ze/census_2022_index.zip' 
  
  
  download.file(url = sct_IMD_url, destfile = 'Data/IMD/sct_imd.xlsx',
                mode = 'wb')
  
  download.file(url = sct_oa22_to_dz22_url, destfile = 'Data/IMD/sct_oa22_to_dz22.zip')
  
  unzip(zipfile = 'Data/IMD/sct_oa22_to_dz22.zip', exdir = 'Data/IMD/sct_oa22_to_dz22_unzip')
  
  download.file(url = sct_census_index_url, destfile = 'Data/IMD/sct_census_index.zip')
  
  unzip(zipfile = 'Data/IMD/sct_census_index.zip', exdir = 'Data/IMD/sct_census_index_unzip')
  
}

# Import SIMD scores rankings and split into deciles
simd <- read_excel('Data/IMD/sct_imd.xlsx', sheet = 'SIMD 2020v2 ranks') %>%
  select(DZ2011 = Data_Zone, SIMD_rank = SIMD2020v2_Rank) %>%
  mutate(IMD_Country_Decile = ntile(SIMD_rank, 10))

# Import OA22 to DZ11 lookup
sct_oa_to_dz11 <- read_csv('Data/IMD/sct_census_index_unzip/Census_2022_Index/OA_TO_HIGHER_AREAS.csv') %>%
  select(OA22 = OA2022, DZ2011)

# Import OA22 to DZ22 lookup
sct_oa_to_dz22 <- read_csv('Data/IMD/sct_oa22_to_dz22_unzip/OA22_DZ22_IZ22.csv')

# Join all above together and produce 2022 DZ IMD scores
sct_dz22_IMD <- left_join(sct_oa_to_dz22, sct_oa_to_dz11, by = 'OA22') %>%
  left_join(., simd, by = 'DZ2011') %>%
  select(OA22, DZ22, DZ2011, IMD_Country_Decile) %>%
  group_by(DZ22) %>%
  summarise(IMD_Country_Decile = get_mode(IMD_Country_Decile)) %>%
  rename(LSOA_code = DZ22) # Changing data zone variable name to LSOA for uniformity with ENG/WAL, allowing for binding


##############################
### Create single IMD dataset
##############################

all_GB_IMD <- rbind(eng_IMD, wales_IMD, sct_dz22_IMD)




############################################################################
######### URBAN/RURAL 
###########################################################################

### England/Wales

engwal_urbrur <- read_csv('https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/9dbf7613cbb147b8bb8627ddb3568cff/csv?layers=0') %>%
  select(LSOA_code = 1, urban_rural_class = RUC21NM)

### Scotland

sct_urbrur <- read_csv('https://www.opendata.nhs.scot/dataset/a7acd6f7-8f50-4433-b952-cee6807d0ff6/resource/86b314f5-652f-48f1-8902-b12963b02eb5/download/datazone2022_urban_rural_2022_04042025.csv') %>%
  select(LSOA_code = 1, urban_rural_class = UrbanRural6fold2022_Name)

all_GB_urbrur <- rbind(engwal_urbrur, sct_urbrur)





##########################################################################
######## PARLIAMENTARY CONSTITUENCIES
##########################################################################

#England and Wales
engwal_constituencies <- read_csv('https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/fcb6546344194e55bd60e0b583fb8564/csv?layers=0') %>%
  select(LSOA_code = LSOA21CD, UKPC_code = PCON24CD, UKPC_name = PCON24NM)
  

# Scotland - as above, DZs renamed as LSOAs for binding with Eng and Wales data
sct_constituencies <- read_csv('https://data.gov.scot/dataset/2022_data_zone_lookup/resource/81b010f5-0207-4ee5-bd2d-e7821aa6a3dc/download') %>%
  select(LSOA_code = DZ22_Code, UKPC_code = UKPC_Code, UKPC_name = UKPC_Name)

# Bind the above to prepare for joining to other variables

all_GB_cons <- rbind(engwal_constituencies, sct_constituencies)




###########################################################################
######### ETHNCIITY (ENGLAND ONLY)
###########################################################################

# ONS census API must be used for this 
# The API request was produced by Claude, which is why the syntax looks a bit different to elsewhere - 
# native pipe used rather than the dplyr pipe, and purrr functions used rather than lapply 
# (both of which I should also start doing really)

BASE  <- "https://api.beta.ons.gov.uk/v1"
POP   <- "UR"
DIM   <- "ethnic_group_tb_20b"
BATCH <- 50  # LSOA codes per request

get_json <- function(url, query = list()) {
  request(url) |>
    req_url_query(!!!query) |>
    req_retry(max_tries = 3) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_json()
}

# 1. All LSOA codes; England only (E01...), Welsh LSOAs start W01
codes <- character()
offset <- 0
repeat {
  page <- get_json(
    sprintf("%s/population-types/%s/area-types/lsoa/areas", BASE, POP),
    list(limit = 1000, offset = offset)
  )
  items <- page$items
  codes <- c(codes, keep(map_chr(items, "id"), \(x) startsWith(x, "E01")))
  offset <- offset + length(items)
  if (length(items) == 0 || offset >= page$total_count) break
}
message(length(codes), " English LSOAs")

# 2. Request observations in batches
batches <- split(codes, ceiling(seq_along(codes) / BATCH))

rows <- map_dfr(batches, function(chunk) {
  data <- get_json(
    sprintf("%s/population-types/%s/census-observations", BASE, POP),
    list(`area-type` = paste0("lsoa,", paste(chunk, collapse = ",")),
         dimensions = DIM)
  )
  
  map_dfr(data$observations, function(obs) {
    d <- set_names(obs$dimensions, map_chr(obs$dimensions, "dimension_id"))
    tibble(
      lsoa21cd     = d$lsoa$option_id,
      lsoa21nm     = d$lsoa$option,
      ethnic_group = d[[DIM]]$option,
      count        = obs$observation
    )
  })
})


# Ethnic groups are aggregated here into larger groups, as they will ultimately be used in the regression sensitivity analysis. 
# Population counts are hten converted into population proportions, and prepared for joining to other variables. 

eng_eth <- rows |>
  mutate(grouping_variable = case_when(
    grepl('^Asian', ethnic_group) ~ 'Asian or Asian British',
    grepl('^Black', ethnic_group) ~ 'Black or Black British',
    grepl('^Mixed', ethnic_group) ~ 'Mixed or multiple ethnic groups',
    grepl('^White', ethnic_group) ~ 'White British, Irish or other',
    grepl('^Other', ethnic_group) ~ 'Other ethnic group',
    TRUE ~ 'Unknown'
  )) %>%
  group_by(lsoa21cd, grouping_variable) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = grouping_variable, values_from = count) %>%
  mutate(total_pop = (`Asian or Asian British` + `Black or Black British` + `Mixed or multiple ethnic groups` + 
                        `White British, Irish or other` + `Other ethnic group` + `Unknown`),
         eth_percent_a = `Asian or Asian British`/total_pop,
         eth_percent_b = `Black or Black British`/total_pop,
         eth_percent_m = `Mixed or multiple ethnic groups`/total_pop,
         eth_percent_w = `White British, Irish or other`/total_pop,
         eth_percent_other = `Other ethnic group`/total_pop,
         eth_percent_unknown = `Unknown`/total_pop
         ) %>%
  select(LSOA_code = lsoa21cd, eth_percent_a, eth_percent_b, eth_percent_m, 
         eth_percent_w, eth_percent_other, eth_percent_unknown)




###########################################################################
######### COMBINE ALL AND PRODUCE FINAL OUTPUT
###########################################################################

all_GB_contextual_var <- left_join(all_GB_lsoas, all_GB_pop, by = 'LSOA_code') %>%
  left_join(., all_GB_IMD, by = 'LSOA_code') %>%
  left_join(., all_GB_urbrur, by = 'LSOA_code') %>%
  left_join(., all_GB_cons, by = 'LSOA_code') %>%
  left_join(., eng_eth, by = 'LSOA_code') %>%
  left_join(., eng_IMD_transformed, by = 'LSOA_code')


write.csv(all_GB_contextual_var, 'Outputs/all_GB_contextual_variables.csv')

rm(list=ls())
