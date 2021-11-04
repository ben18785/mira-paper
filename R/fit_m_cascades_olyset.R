library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

# fit m using olyset trial village prevalences
annual <- readRDS("data/raw/prevalence_olyset_mira.rds") %>%
  mutate(size=round((1 / prevalence) * n_troph_any)) %>% 
  rename(positive=n_troph_any) %>% 
  mutate(year=year(date),
         month=month(date)) %>% 
  filter(source=="Olyset") %>% 
  mutate(lower_range_r=0,
         upper_range_r=5,
         age_range_r="0-5") %>% 
  mutate(adminLevel1="Cascades")

temp_input <- readRDS("data/processed/input_mean.rds")

cascades <- temp_input %>% 
  filter(NAME_1=="Cascades")
cascades <- remove_post_2018_interventions(cascades)

# assume from 2014 onwards that netes are olyset
int <- cascades$interventions[[1]] %>% 
  mutate(net_type=if_else(year >= 2014, "oly", net_type))

# load olyset itn data
itns <- readRDS("data/raw/itn.rds") %>% 
  ungroup() %>% 
  mutate(village=tolower(village)) %>% 
  group_by(year) %>%
  summarise(itn_coverage=mean(itn_coverage)) %>% 
  filter(year >= 2014)
int <- int %>%
  mutate(llin = if_else(year >= 2014,
                        itns$itn_coverage[match(year, itns$year)],
                        llin))
cascades$interventions <- list(int)

test <- f_find_m("Cascades", annual, cascades, num_people_ = 20000)

saveRDS(test, "data/processed/m_fits_cascades_olyset.rds")