library(tidyverse)

df <- readRDS("outputs/simulations/projection_all_simulations.rds")

load("data/raw/population_projections.RData")

population_projections <- population_projections %>% 
  filter(NAME_0=="Burkina Faso") %>% 
  group_by(NAME_1, year) %>% 
  summarise(pop=sum(pop),
            par=sum(par),
            .groups="drop") %>% 
  rename(year_round=year)

year_df <- df %>% 
  mutate(year_round=round(year)) %>% 
  left_join(population_projections, by=c("NAME_1", "year_round")) %>% 
  group_by(NAME_1, year_round, scenario, itn_coverage_past) %>% 
  summarise(
    cases=mean(clin_inc_all_smooth * par),
    clin_inc_all_smooth=weighted.mean(clin_inc_all_smooth, par),
    clin_inc_5_15_smooth=weighted.mean(clin_inc_5_15_smooth, par),
    .groups="drop")

# use incidence estimates from 10 MIRA villages to match incidence in
# 5-15s
df_mira <- read.csv("data/raw/Incidence by village for Tom_030921.csv")

# calculate sample-size weighted incidence
incidence_2017 <- weighted.mean(df_mira$incidence, df_mira$sample_size)

# inflate / deflate incidence (and cases)
modelled_incidence_2017 <- year_df %>%
  filter(scenario=="current") %>% 
  group_by(year_round, itn_coverage_past) %>% 
  summarise(clin_inc_5_15_smooth=mean(clin_inc_5_15_smooth)) %>% 
  filter(year_round==2017) %>% 
  mutate(ratio=incidence_2017 / clin_inc_5_15_smooth) %>% 
  select(-clin_inc_5_15_smooth) %>% 
  ungroup() %>% 
  select(-year_round)

year_df <- year_df %>% 
  left_join(modelled_incidence_2017) %>% 
  mutate(cases = cases * ratio) %>% 
  mutate(clin_inc_all_smooth=clin_inc_all_smooth * ratio) %>% 
  mutate(clin_inc_5_15_smooth=clin_inc_5_15_smooth * ratio)
saveRDS(year_df, "data/processed/projection_cases_adjusted.rds")