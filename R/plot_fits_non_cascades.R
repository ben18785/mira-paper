library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000)
temp_input <- readRDS("data/processed/input_bf_only.rds")
m_results <- readRDS("data/processed/m_fits_on.rds")

# Plot and save fits
for(i in seq_along(m_results$NAME_1)) {
  a_NAME_1 <- m_results$NAME_1[i]
  print(a_NAME_1)
  a_df <- temp_input %>%
    filter(NAME_1==a_NAME_1) %>%
    mutate(total_M=m_results$m[i])
  
  temp <- purrr::pmap(a_df, f_run_model)
  outputs <- temp[[1]]$output
  g <- f_plot_specific_simple(a_NAME_1, outputs, annual) +
    ggtitle(a_NAME_1)
  ggsave(paste0("outputs/fit_plots/fit_res_on_",
                m_results$NAME_1[i], ".pdf"),
         g, width = 10, height = 6)
}