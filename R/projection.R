setwd("/Volumes/Samsung1.5TB/Github/mira-lightweight/src/pete")

rm(list=ls())
library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
source("fit_site.R")
source("run.R")
source("helper.R")

args <- commandArgs(trailingOnly=TRUE)
id <- as.numeric(args[1])
itn_scenario <- as.numeric(args[2])
smc_scenario <- as.numeric(args[3])
irs_scenario <- as.numeric(args[4])
resistance_scenario <- as.numeric(args[5])

annual <- readRDS("../../data/transmission_dynamics/monthly_prevalence.rds") %>%
  filter(year >= 2000)
temp_input <- readRDS("../../data/input_bf_only.rds")
m_fits <- readRDS("../../data/pete_m_all.rds")

# select base data
names1 <- unique(temp_input$NAME_1)
a_NAME_1 <- names1[id]
a_df <- temp_input %>%
  filter(NAME_1==names1[id]) %>%
  mutate(total_M=m_fits$m[match(a_NAME_1, temp_input$NAME_1)])

# go through scenarios and modify data
a <- a_df$interventions[[1]]
## as base case (before modified below) keep interventions the same
laster <- a %>%
  filter(year==2018)
last_1 <- laster %>% mutate(year=2019)
last_2 <- laster %>% mutate(year=2020)
last_3 <- laster %>% mutate(year=2021)
last_4 <- laster %>% mutate(year=2022)
last_5 <- laster %>% mutate(year=2023)
last_6 <- laster %>% mutate(year=2024)
last_7 <- laster %>% mutate(year=2025)
a <- a %>%
  bind_rows(last_1) %>%
  bind_rows(last_2) %>%
  bind_rows(last_3) %>%
  bind_rows(last_4) %>%
  bind_rows(last_5) %>%
  bind_rows(last_6) %>%
  bind_rows(last_7)

# itn scenarios
if(itn_scenario == 1) {
  ## no ITN coverage into the future
  a <- a %>%
    mutate(llin=if_else(year > 2018, 0, llin))
} else if(itn_scenario == 2) {
  ## standard nets at same coverage as in 2018 (no need to do anything to 'a')
} else if(itn_scenario == 3) {
  ## IG2 at same coverage as standard net coverage in 2018
  a <- a %>%
    mutate(net_type=if_else(year > 2018, "ig2", net_type))
} else if(itn_scenario == 4) {
  ## PBO at same coverage as standard net coverage in 2018
  a <- a %>%
    mutate(net_type=if_else(year > 2018, "pbo", net_type))
}

# smc scenarios
a <- a %>%
  mutate(lower_smc_age=0.5,
         upper_smc_age=5)
if (smc_scenario == 1) {
  ## turn smc off in future
  a <- a %>%
    mutate(smc = if_else(year > 2018, 0, smc))
} else if(smc_scenario == 2) {
  ## 0-5s: default
} else if(smc_scenario == 3) {
  ## smc for 0-10s
  a <- a %>%
    mutate(upper_smc_age = if_else(year > 2018, 10, upper_smc_age))
} else if(smc_scenario == 4) {
  ## smc for 0-15s
  a <- a  %>%
    mutate(upper_smc_age = if_else(year > 2018, 15, upper_smc_age))
}

# irs scenarios
if (irs_scenario == 1) {
  ## irs off (no need to change anything)
} else if(irs_scenario == 2) {
  ## irs on at 80%
  a <- a %>%
    mutate(irs = if_else(year > 2018, 0.8, irs))
}

a_df <- a_df %>%
  mutate(interventions=list(a))

# resistance scenario
res <- a_df$resistance[[1]]
if(resistance_scenario == 1) {
  ## turn resistance off in 2019 onwards
  res <- res %>%
    mutate(resistance=if_else(year > 2018, 0, resistance))
} else if(resistance_scenario == 2) {
  ## keep resistance fixed at 2018 levels
  a_val <- res %>%
    filter(year==2018) %>%
    pull(resistance)
  res <- res %>%
    mutate(resistance=if_else(year > 2018, a_val, resistance))
} else if(resistance_scenario == 3) {
  ## allow resistance to trend (so do nothing)
}
a_df <- a_df %>%
  mutate(resistance = list(res)) %>%
  mutate(num_people=20000)

temp <- purrr::pmap(a_df, f_run_model_longer)
outputs <- temp[[1]]$output %>%
  mutate(year = year + 2000)
inputs <- temp[[1]]$input

id <- as.numeric(args[1])
itn_scenario <- as.numeric(args[2])
smc_scenario <- as.numeric(args[3])
irs_scenario <- as.numeric(args[4])
resistance_scenario <- as.numeric(args[5])

saveRDS(list(outputs=outputs,
             inputs=inputs,
             big_input=a_df),
        paste0("../../outputs/pete_projections/project_",
               id, "_",
               itn_scenario, "_",
               smc_scenario, "_",
               irs_scenario, "_",
               resistance_scenario, ".rds"))

