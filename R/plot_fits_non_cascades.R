library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")
args <- commandArgs(trailingOnly = T)
i <- as.numeric(args[1])

annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000)
m_results <- readRDS("data/processed/m_fits_on.rds")

# Plot and save fits
a_NAME_1 <- m_results$NAME_1[i]
itn_scenario <- m_results$itn_scenario[i]
if(itn_scenario=="mean") {
  temp_input <- readRDS("data/processed/input_mean.rds")
}else if(itn_scenario=="lower") {
  temp_input <- readRDS("data/processed/input_lower.rds")
} else if(itn_scenario=="upper"){
  temp_input <- readRDS("data/processed/input_upper.rds")
}

temp_input <- temp_input %>% 
  filter(NAME_1==a_NAME_1)
temp_input <- remove_post_2018_interventions(temp_input)

filename <- gsub(" ", "_", a_NAME_1)
print(a_NAME_1)
a_df <- temp_input %>%
  filter(NAME_1==a_NAME_1) %>%
  mutate(total_M=m_results$m[i])

temp <- purrr::pmap(a_df, f_run_model)
outputs <- temp[[1]]$output
g <- f_plot_specific_simple(a_NAME_1, outputs, annual) +
  ggtitle(a_NAME_1)
ggsave(paste0("outputs/fit_plots/fit_res_on_", i, ".pdf"),
       g, width = 10, height = 6)