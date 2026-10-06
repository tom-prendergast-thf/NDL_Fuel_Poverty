# # Written in R-4.3.1

## Install and load packages
packages <- c('dplyr', 'tidyr', 'readr', 'readxl', 'lubridate', 'readODS', 'sf', 'httr2', 'RCurl', 'purrr')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)


### Functions

# This function is used to summarise variables on the basis of their modal value, 
# that is, the most common value observed within a group. This is useful for summarising categorical variables
# by which value appears most frequently for a given group. 

get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  if (length(x) == 0) return(NA)
  
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[which.max(tab)]
}

SETUP_RUN <- 'SETUP RUN'
