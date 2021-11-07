library(tidyverse)

inputs <- readRDS("data/processed/input_mean.rds")
interventions <- inputs %>% 
  select(NAME_1, interventions) %>% 
  unnest(cols = c(interventions))

g <- interventions %>%
  select(NAME_1, year, llin, smc, irs, prop_act) %>% 
  pivot_longer(!c(NAME_1, year)) %>% 
  ggplot(aes(x=year, y=value, colour=name)) +
  geom_line() +
  facet_wrap(~NAME_1) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  xlab("Year") +
  ylab("Coverage") +
  scale_y_continuous(labels=scales::percent)
ggsave("outputs/counterfactual_plots/interventions.pdf", g,
       width = 12, height = 8)

load("data/raw/population_projections.RData")
population_projections <- population_projections %>% 
  filter(NAME_0=="Burkina Faso") %>% 
  group_by(NAME_1, year) %>% 
  summarise(pop=sum(pop),
            par=sum(par),
            .groups="drop") %>% 
  rename(year_round=year)

interventions_overall <- interventions %>% 
  mutate(year_round=round(year)) %>% 
  left_join(population_projections, by=c("NAME_1", "year_round")) %>% 
  select(year_round, par, llin, irs, smc) %>% 
  group_by(year_round) %>% 
  summarise(
    llin=weighted.mean(llin, par),
    irs=weighted.mean(irs, par),
    smc=weighted.mean(smc, par),
    .groups="drop")
saveRDS(interventions_overall, "data/processed/intervention_coverages_whole_burkina.rds")