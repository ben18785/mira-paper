library(tidyverse)
counterfactual_params <- tribble(
  ~scenario, ~itn, ~smc, ~resistance, ~biting,
  "baseline", 0, 0, 1, 1,
  "current", 1, 1, 1, 1,
  "no bednets", 0, 1, 1, 1,
  "no smc", 1, 0, 1, 1,
  "no resistance", 1, 1, 0, 1,
  "no outdoor biting", 1, 1, 1, 0,
  "no resistance or outdoor biting", 1, 1, 0, 0
)
temp_input <- readRDS("data/processed/input_bf_only.rds") %>% 
  select(NAME_0, NAME_1)
counterfactual_params <- expand_grid(temp_input,
                                     counterfactual_params) %>% 
  filter(NAME_1=="Cascades")

saveRDS(counterfactual_params, "data/processed/counterfactual_params.rds")