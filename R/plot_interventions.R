library(tidyverse)
library(purrr)

inputs <- readRDS("data/processed/input_bf_only.rds")
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