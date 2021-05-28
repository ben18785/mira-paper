library(tidyverse)
library(purrr)

df <- readRDS("outputs/simulations/counterfactual_all_simulations.rds")
g <- df %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=as.factor(scenario))) +
  geom_line() +
  facet_wrap(~NAME_1) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  xlab("Year") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
ggsave("outputs/counterfactual_plots/scenarios.pdf", g,
       width = 12, height = 8)