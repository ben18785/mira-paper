library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000) %>% 
  filter(adminLevel1=="Cascades")

annual1 <- readRDS("data/raw/prevalence_olyset_mira.rds") %>%
  mutate(size=round((1 / prevalence) * n_troph_any)) %>% 
  rename(positive=n_troph_any) %>% 
  mutate(year=year(date),
         month=month(date)) %>% 
  filter(source=="Olyset") %>% 
  mutate(lower_age_r=0,
         upper_age_r=5,
         age_range_r="0-5") %>% 
  mutate(adminLevel1="Cascades")

annual <- annual %>% 
  bind_rows(annual1)

temp_input <- readRDS("data/processed/input_bf_only.rds")
m_fits <- readRDS("data/processed/m_fits_all.rds")
m <- m_fits %>% 
  filter(NAME_1=="Cascades") %>% 
  pull(m)
cascades <- temp_input %>%
  filter(NAME_1=="Cascades") %>%
  mutate(total_M=m)
temp <- purrr::pmap(cascades, f_run_model)
outputs <- temp[[1]]$output

g <- f_plot_specific_simple("Cascades", outputs, annual) +
  ggtitle("Cascades")

ggsave(paste0("outputs/fit_plots/fit_res_on_Cascades_inc_olyset.pdf"),
       g, width = 10, height = 6)