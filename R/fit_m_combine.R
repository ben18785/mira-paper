library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

# script just combines m values from overall resistance on
# and olyset village fits

# overall fits
m_results_on <- readRDS("data/processed/m_fits_on.rds")

# cascade fits
olyset <- readRDS("data/processed/m_fits_cascades_olyset.rds")
m_2 <- olyset$m

# get sample sizes for olyset
annual <- readRDS("data/raw/prevalence_olyset_mira.rds") %>%
  mutate(size=round((1 / prevalence) * n_troph_any)) %>% 
  rename(positive=n_troph_any) %>% 
  mutate(year=year(date),
         month=month(date)) %>% 
  filter(source=="Olyset") %>% 
  mutate(lower_age_r=0,
         upper_age_r=5,
         age_range_r="0-5") %>% 
  mutate(adminLevel1="Cascades")

size_olyset <- sum(annual$size)

## get sample sizes for overall
size_other <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000) %>% 
  filter(adminLevel1=="Cascades") %>% 
  pull(size) %>% 
  sum()

## take weighted average of ms
m_results_on <- m_results_on %>% 
  mutate(m=as.numeric(as.character(m))) %>% 
  mutate(m = if_else(NAME_1=="Cascades", (size_olyset * m_2 + size_other * m) / (size_olyset + size_other),
                     as.numeric(as.character(m))))
saveRDS(m_results_on, "data/processed/m_fits_all.rds")