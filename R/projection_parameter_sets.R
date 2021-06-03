library(tidyverse)
parameters_df <- tribble(
  ~scenario, ~itn_coverage, ~itn_type, ~smc, ~irs, ~resistance,
  "baseline", 0, 0, 0, 0, 0,
  "current", 1, 0, 1, 0, 0,
  "stop bednets", 0, 0, 1, 0, 0,
  "stop SMC", 1, 0, 0, 0, 0,
  "universal LLINs", 2, 0, 1, 0, 0,
  "universal PBO LLINs", 2, 1, 1, 0, 0,
  "universal LLINs + IRS", 2, 0, 1, 1, 0,
  "universal PBO LLINs + IRS", 2, 1, 1, 1, 0,
  "universal perfect LLINs", 2, 0, 1, 0, 1,
  "universal perfect LLINs + IRS", 2, 0, 1, 1, 1)

temp_input <- readRDS("data/processed/input_bf_only.rds") %>% 
  select(NAME_0, NAME_1)
parameters_df <- expand_grid(temp_input,
                             parameters_df)

saveRDS(parameters_df, "data/processed/projection_params.rds")