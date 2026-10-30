# 05_additional viz aggregations

# These had to be run outside of the SDE due to either a) requests that came later or b) data that was necessary never made it into the SDE

# Regional comparisons
# regional data wasn't included in the port into the SDE

la_to_itl <- read_csv('Data/ITL/LA_to_ITL.csv')
#itl_boundaries <- read_sf('Data/ITL/ITL2_JAN_2025_UK_BGC.shp')


fp_data <- read_csv('Outputs/HH_data_updated_061026_with_cc_and_EPC.csv') %>%
  left_join(., la_to_itl, by = join_by(LA_CODE == LAD24CD)) %>%
  #  left_join(., itl_boundaries, by = 'ITL225CD') %>%
  mutate(Income_quintile = ntile(LSOA_incmedian, 5),
         Fp_quintile = ntile(FP_decile, 5))

# values for Wales and Scotland - % of LSOAs in the highest quintile of FP
fp_data %>%
  filter(!grepl('^E', LA_CODE)) %>%
  tabyl(Fp_quintile, ITL225NM) %>% adorn_percentages(denominator = 'col') %>%
  pivot_longer(2:10, names_to = 'ITL225NM', values_to = 'value') %>%
  filter(Fp_quintile == 5) 

# Same but for English regions
fp_data %>%
  filter(grepl('^E', LA_CODE)) %>%
  tabyl(Fp_quintile, ITL125NM) %>% adorn_percentages(denominator = 'col') %>%
  pivot_longer(2:10, names_to = 'ITL125NM', values_to = 'value') %>%
  filter(Fp_quintile == 5) 



# Aggregate incomes data 
# This was in response to Lisa's request to do a histogram of LSOAs by income bands, split by FP quintile

incomes_data <- fp_data %>%
  select(LSOA_CODE, LSOA_incmedian, Fp_quintile) %>%
  mutate(income_band = paste0('£',
    floor(LSOA_incmedian / 1000),
    "k–£",
    floor(LSOA_incmedian / 1000) + 1,
    "k"
  )) %>%
  group_by(Fp_quintile) %>%
  mutate(FP_group_median = median(LSOA_incmedian)) %>%
  ungroup()

grouped_incomes_data <- incomes_data %>%
  group_by(Fp_quintile, income_band) %>%
  summarise(LSOA_count = n_distinct(LSOA_CODE)) %>%
  mutate(bin_start = as.numeric(paste0(substr(income_band, 2,3), '000')),
         bin_start_k = as.numeric(substr(income_band, 2,3)),
         fuel_poverty_risk_quintile = case_when(Fp_quintile == 1 ~ '1st (lowest risk)',
                                                Fp_quintile == 2 ~ '2nd',
                                                Fp_quintile == 3 ~ '3rd',
                                                Fp_quintile == 4 ~ '4th',
                                                Fp_quintile == 5 ~ '5th (highest risk)'
         )
         
         )

write.csv(grouped_incomes_data, 'Outputs/grouped_incomes_data.csv')

# Comparison of ethnic groups 
# Unfortunately we have to run the long-winded ONS API request again to get accurate percentages,
# because the first time I only kept proportions


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


# Ethnic groups are aggregated here into larger groups

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
         eth_a = `Asian or Asian British`,
         eth_b = `Black or Black British`,
         eth_m = `Mixed or multiple ethnic groups`,
         eth_w = `White British, Irish or other`,
         eth_other = `Other ethnic group`,
         eth_unknown = `Unknown`
  ) %>%
  select(LSOA_CODE = lsoa21cd, eth_a, eth_b, eth_m, 
         eth_w, eth_other, eth_unknown)

fp_with_eth <- fp_data %>%
  filter(grepl('^E', LA_CODE)) %>%
  left_join(., eng_eth, by = 'LSOA_CODE') %>%
  group_by(Fp_quintile) %>%
  summarise(total_population = sum(Total),
    eth_a = sum(eth_a), 
         eth_b = sum(eth_b), 
         eth_m = sum(eth_m), 
         eth_w = sum(eth_w), 
         eth_other = sum(eth_other), 
         eth_unknown = sum(eth_unknown)) %>%
  mutate(Perc_eth_a = eth_a/total_population, 
         Perc_eth_b = eth_b/total_population, 
         Perc_eth_m = eth_m/total_population, 
         Perc_eth_w = eth_w/total_population, 
         Perc_eth_other = eth_other/total_population, 
         Perc_eth_unknown = eth_unknown/total_population)
