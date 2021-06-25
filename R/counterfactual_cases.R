library(tidyverse)
library(purrr)

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
            .groups="drop")

# use cases from WMR (2019) to change all cases
# so that current matches these reported numbers
wmr <- read.csv("data/raw/wmr_cases_2010_2018.csv") %>% 
  filter(country=="Burkina Faso") %>% 
  rename(NAME_0=country,
         wmr_cases=cases,
         year_round=year)

overall_df <- year_df %>%
  filter(scenario=="current") %>% 
  group_by(year_round) %>% 
  summarise(cases=sum(cases)) %>% 
  left_join(wmr) %>% 
  filter(!is.na(wmr_cases)) %>% 
  ungroup() %>% 
  summarise(cases=sum(cases),
            wmr_cases=sum(wmr_cases)) %>% 
  mutate(ratio=wmr_cases / cases)

year_df <- year_df %>% 
  mutate(cases = cases * overall_df$ratio)

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
ggsave("outputs/counterfactual_plots/cases_admin1_percentage.pdf",
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
ggsave("outputs/counterfactual_plots/cases_percentage.pdf",
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
saveRDS(temp_df, "outputs/simulations/counterfactual_cases_absolute.rds")

ggsave("outputs/counterfactual_plots/cases_absolute.pdf",
       g)