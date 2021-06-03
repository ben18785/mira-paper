library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

args <- commandArgs(trailingOnly=TRUE)
id <- as.numeric(args[1])
params_df <- readRDS("data/processed/projection_params.rds")
params_df <- params_df[id, ]

itn_coverage_scenario <- params_df$itn_coverage
itn_type_scenario <- params_df$itn_type
smc_scenario <- params_df$smc
irs_scenario <- params_df$irs
resistance_scenario <- params_df$resistance

annual <- readRDS("data/raw/monthly_prevalence.rds") %>%
  filter(year >= 2000)
temp_input <- readRDS("data/processed/input_bf_only.rds")
m_fits <- readRDS("data/processed/m_fits_all.rds")

# select base data
a_NAME_1 <- params_df$NAME_1
a_df <- temp_input %>%
  filter(NAME_1==a_NAME_1) %>%
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

# itn coverage scenarios
if(itn_coverage_scenario == 0) {
  ## no ITN coverage into the future
  a <- a %>%
    mutate(llin=if_else(year >= 2020, 0.00000001, llin)) # trying a small number to avoid issues with seed
} else if(itn_coverage_scenario == 2) { # scenario 1 is default
  a <- a %>%
    mutate(llin=if_else(year >= 2020, 0.8, llin))
}

# itn type scenarios
if(itn_type_scenario == 1) {
  a <- a %>%
    mutate(net_type=if_else(year >= 2020, "pbo", net_type))
}

# smc scenarios
a <- a %>%
  mutate(lower_smc_age=0.5,
         upper_smc_age=5)
if (smc_scenario == 0) {
  ## turn smc off in future
  a <- a %>%
    mutate(smc = if_else(year >= 2020, 0, smc))
}

# irs scenarios
if (irs_scenario == 1) {
  a <- a %>%
    mutate(irs = if_else(year >= 2020, 0.8, irs))
}

a_df <- a_df %>%
  mutate(interventions=list(a))

# resistance scenario
res <- a_df$resistance[[1]]
if(resistance_scenario == 1) {
  ## turn resistance off in 2019 onwards
  res <- res %>%
    mutate(resistance=if_else(year >= 2020, 0, resistance))
}
a_df <- a_df %>%
  mutate(resistance = list(res)) %>%
  mutate(num_people=20000)

temp <- purrr::pmap(a_df, f_run_model_longer)
outputs <- temp[[1]]$output %>%
  mutate(year = year + 2000) %>% 
  cbind(params_df)
inputs <- temp[[1]]$input

filename <- paste0("outputs/simulations/projection_", id, ".rds")
final_df <- list(outputs=outputs,
                 inputs=inputs,
                 big_input=a_df)
saveRDS(final_df, filename)

