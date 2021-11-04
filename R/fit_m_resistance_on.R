library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

# get prevalences (mainly DHS) used to fit m (mosquito density)
# parameter
annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000)

temp_mean <- readRDS("data/processed/input_mean.rds")
temp_lower <- readRDS("data/processed/input_lower.rds")
temp_upper <- readRDS("data/processed/input_upper.rds")

# go through and do fitting using ML (albeit with a stochastic model)
m_results <- fit_all_m(resistance_off=FALSE,
                       temp_mean=temp_mean,
                       temp_lower=temp_lower,
                       temp_upper=temp_upper,
                       num_people=20000)
saveRDS(m_results, "data/processed/m_fits_on.rds")