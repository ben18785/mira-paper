library(tidyverse)

df <- readRDS("outputs/simulations/counterfactual_all_simulations.rds") %>% 
  mutate(scenario=if_else(scenario=="no resistance or outdoor biting",
                          "no resistance\nor outdoor biting",
                          scenario))
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
  group_by(NAME_1, year_round, scenario) %>% 
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
  group_by(year_round) %>% 
  summarise(clin_inc_5_15_smooth=mean(clin_inc_5_15_smooth)) %>% 
  filter(year_round==2017) %>% 
  pull(clin_inc_5_15_smooth)
ratio <- incidence_2017 / modelled_incidence_2017

year_df <- year_df %>% 
  mutate(cases = cases * ratio) %>% 
  mutate(clin_inc_all_smooth=clin_inc_all_smooth * ratio) %>% 
  mutate(clin_inc_5_15_smooth=clin_inc_5_15_smooth * ratio)

# work out differences versus current and plot
percent_df <- year_df %>% 
  group_by(NAME_1, scenario) %>% 
  summarise(cases=sum(cases))

current_df <- percent_df %>% 
  filter(scenario=="current") %>% 
  select(NAME_1, cases) %>% 
  rename(current=cases)

percent_df <- percent_df %>% 
  left_join(current_df, by="NAME_1") %>% 
  mutate(percentage_diff = (cases - current) / current)

g <- percent_df %>% 
  filter(scenario != "current") %>% 
  ggplot(aes(x=fct_reorder(scenario, percentage_diff),
             y=percentage_diff)) +
  geom_col() +
  facet_wrap(~NAME_1) +
  xlab("Scenario") +
  ylab("Difference vs current") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  coord_flip()
ggsave("outputs/counterfactual_plots/cases_admin1_percentage_mira.pdf",
       g, width = 12, height = 8)

total_df <- percent_df %>% 
  group_by(scenario) %>% 
  summarise(cases=sum(cases))

current_total_df <- total_df %>% 
  filter(scenario=="current") %>% 
  rename(current=cases)

total_df <- total_df %>% 
  mutate(current=current_total_df$current) %>% 
  mutate(percentage_diff = (cases - current) / current)

g <- total_df %>% 
  filter(scenario != "current") %>% 
  ggplot(aes(x=fct_reorder(scenario, percentage_diff),
             y=percentage_diff)) +
  geom_col() +
  xlab("Scenario") +
  ylab("Difference vs current") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),
                     limits = c(-0.1, NA)) +
  coord_flip()
ggsave("outputs/counterfactual_plots/cases_percentage_mira.pdf",
       g)

temp_df <- total_df %>% 
  filter(scenario != "current") %>% 
  mutate(cases=(cases-current)/1e6)
g <- temp_df %>% 
  ggplot(aes(x=fct_reorder(scenario, percentage_diff),
             y=cases)) +
  geom_col() +
  xlab("Scenario") +
  ylab("Cases, millions") +
  scale_y_continuous() +
  coord_flip()
saveRDS(temp_df, "outputs/simulations/counterfactual_cases_absolute_mira.rds")

ggsave("outputs/counterfactual_plots/cases_absolute_mira.pdf",
       g)