library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

counterfactual_params <- readRDS("data/processed/projection_params.rds")
for(i in seq_along(counterfactual_params$NAME_1)) {
  filename <- paste0("outputs/simulations/projection_", i, ".rds")
  temp_df <- counterfactual_params[i, ]
  df <- readRDS(filename)$outputs
  if(i == 1)
    big_df <- df
  else
    big_df <- big_df %>% bind_rows(df)
}

saveRDS(big_df, "outputs/simulations/projection_all_simulations.rds")