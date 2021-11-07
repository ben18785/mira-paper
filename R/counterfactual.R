library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

args <- commandArgs(trailingOnly=TRUE)
id <- as.numeric(args[1])
print(paste0("Running counterfactual scenario ", id))

counterfactual_params <- readRDS("data/processed/counterfactual_params.rds")
counterfactual_params <- counterfactual_params[id, ]

itn_scenario <- counterfactual_params$itn
smc_scenario <- counterfactual_params$smc
biting_scenario <- counterfactual_params$biting
resistance_scenario <- counterfactual_params$resistance
act_scenario <- counterfactual_params$act
a_NAME_1 <- counterfactual_params$NAME_1
itn_coverage <- counterfactual_params$itn_coverage

annual <- readRDS("data/raw/monthly_prevalence.rds") %>%
  filter(year >= 2000)
if(itn_coverage=="mean") {
  temp_input <- readRDS("data/processed/input_mean.rds")
}else if(itn_coverage=="lower") {
  temp_input <- readRDS("data/processed/input_lower.rds")
} else if(itn_coverage=="upper"){
  temp_input <- readRDS("data/processed/input_upper.rds")
}
m_fits <- readRDS("data/processed/m_fits_all.rds") %>% 
  filter(itn_scenario==itn_coverage)

# select base data
a_df <- temp_input %>%
  filter(NAME_1==a_NAME_1) %>%
  mutate(total_M=m_fits$m[match(a_NAME_1, temp_input$NAME_1)])

# go through scenarios and modify data
## remove post-2020 obs
a <- a_df$interventions[[1]]
a <- a %>% 
  filter(year <= 2020)
  
# itn scenarios
if(itn_scenario == 0) {
  ## no ITN coverage
  a <- a %>%
    mutate(llin=0)
}

# smc scenarios
a <- a %>%
  mutate(lower_smc_age=0.5,
         upper_smc_age=5)
if (smc_scenario == 0) {
  ## turn smc off in future
  a <- a %>%
    mutate(smc = 0)
} 

# act scenarios
if(act_scenario == 0) {
  a <- a %>%
    mutate(prop_act = 0)
}

a_df <- a_df %>%
  mutate(interventions=list(a))

# resistance scenario
res <- a_df$resistance[[1]]
if(resistance_scenario == 0) {
  ## turn resistance off
  res <- res %>%
    mutate(resistance=0)
  a_df <- a_df %>%
    mutate(resistance = list(res))
}

# vectors: make outdoor biting equal to overall Africa value from PNAS
if(biting_scenario == 0) {
  a_vectors <- a_df$vectors[[1]] %>%
    mutate(gamb_ss_Q_in=0.875,
           fun_Q_in=0.875,
           arab_Q_in=0.875) %>% 
    mutate(gamb_ss_Q_bed=0.875,
           fun_Q_bed=0.875,
           arab_Q_bed=0.875)
  a_df <- a_df %>%
    mutate(vectors=list(a_vectors))
}

a_df <- a_df %>%
  mutate(num_people=20000)

temp <- purrr::pmap(a_df, f_run_model_now)
outputs <- temp[[1]]$output %>%
  mutate(year = year + 2000)
inputs <- temp[[1]]$input

saveRDS(list(outputs=outputs,
             inputs=inputs,
             big_input=a_df),
        paste0("outputs/simulations/counterfactual_",
               id, ".rds"))

