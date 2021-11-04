# adds in ITN data from MAP to data file

library(tidyverse)

df_map <- read.csv("data/raw/SSA_ITN_use_adm1.csv") %>% 
  filter(NAME_0=="Burkina Faso")
df_old <- readRDS("data/processed/input_bf_only.rds")

old_name1s <- unique(df_old$NAME_1)
new_name1s <- unique(df_map$NAME_1)

if(mean(new_name1s %in% old_name1s) != 1)
  stop("name_1s do not match")

# add in the new coverages
for(i in seq_along(old_name1s)) {
  temp <- df_old %>% 
    filter(NAME_1==old_name1s[i]) %>% 
    pull(interventions)
  temp <- temp[[1]]
  temp_new <- df_map %>% 
    filter(NAME_1==old_name1s[i])
  
  a_df <- tibble(year=temp_new$year,
                 value=temp_new$ITN_coverage_pop_weighted,
                 name=temp_new$type)
  b_df <- tibble(year=temp$year,
                 value=temp$llin,
                 name="old")
  both_df <- a_df %>% 
    bind_rows(b_df) %>% 
    mutate(NAME_1=old_name1s[i])
  if(i == 1)
    big_df <- both_df
  else
    big_df <- big_df %>% bind_rows(both_df)
}

saveRDS(big_df, "data/processed/compare_map_old_itns.rds")
