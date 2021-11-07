library(tidyverse)

# mean
df <- readRDS("outputs/simulations/counterfactual_all_simulations.rds")
g <- df %>% 
  filter(itn_coverage=="mean") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=as.factor(scenario))) +
  geom_line() +
  facet_wrap(~NAME_1) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  xlab("Year") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5))
ggsave("outputs/counterfactual_plots/scenarios.pdf", g,
       width = 12, height = 8)

# lower
g <- df %>% 
  filter(itn_coverage=="lower") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=as.factor(scenario))) +
  geom_line() +
  facet_wrap(~NAME_1) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  xlab("Year") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5))
ggsave("outputs/counterfactual_plots/scenarios_lower.pdf", g,
       width = 12, height = 8)

# upper
g <- df %>% 
  filter(itn_coverage=="upper") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=as.factor(scenario))) +
  geom_line() +
  facet_wrap(~NAME_1) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  xlab("Year") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5))
ggsave("outputs/counterfactual_plots/scenarios_upper.pdf", g,
       width = 12, height = 8)