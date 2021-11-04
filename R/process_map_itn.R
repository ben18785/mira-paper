# adds in ITN data from MAP to data file

library(tidyverse)

df_itns <- readRDS("data/processed/compare_map_old_itns.rds")
df <- readRDS("data/processed/input_bf_only.rds")

# since MAP upper prevalences look strange pre-2005
# set these two mean prevalence
df_itns <- df_itns %>% 
  pivot_wider(id_cols = c("year", "NAME_1"), values_from="value",
              names_from="name") %>% 
  group_by(NAME_1) %>% 
  arrange(year) %>% 
  mutate(upper=if_else(year < 2005, mean, upper)) %>% 
  pivot_longer(c(lower, mean, upper)) %>% 
  ungroup()

# create three files: lower, mean, upper ITN coverage
# also make data for projections by default taking 2018
# values apart from ITNs

create_new_interventions <- function(chosen_itn, a_NAME_1, df, df_itns) {
  df_interventions <- df %>% 
    filter(NAME_1==a_NAME_1) %>% 
    pull(interventions)
  df_interventions <- df_interventions[[1]]
  
  # add data up to 2030
  extra_rows <- df_interventions[rep(nrow(df_interventions), 12), ] %>% 
    mutate(year=seq(2019, 2030, 1))
  df_interventions <- df_interventions %>% 
    bind_rows(extra_rows)
  
  df_chosen <- df_itns %>% 
    filter(NAME_1==a_NAME_1) %>% 
    filter(name==chosen_itn) %>% 
    select(year, value)
  
  df_interventions <- df_interventions %>% 
    left_join(df_chosen, by="year") %>% 
    fill(value) %>% 
    mutate(llin=value) %>% 
    select(-value)
  df_interventions
}

create_all_new_interventions <- function(chosen_itn, df, df_itns) {
  name_1s <- unique(df$NAME_1)
  for(i in seq_along(name_1s)) {
    a_NAME_1 <- name_1s[i]
    df_int <- create_new_interventions(chosen_itn, a_NAME_1,
                                       df, df_itns)
    df$interventions[[i]] <- df_int
  }
  df
}

df_mean <- create_all_new_interventions("mean", df, df_itns)
df_lower <- create_all_new_interventions("lower", df, df_itns)
df_upper <- create_all_new_interventions("upper", df, df_itns)

saveRDS(df_mean, "data/processed/input_mean.rds")
saveRDS(df_lower, "data/processed/input_lower.rds")
saveRDS(df_upper, "data/processed/input_upper.rds")