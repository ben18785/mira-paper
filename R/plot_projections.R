library(tidyverse)
library(purrr)

df <- readRDS("outputs/simulations/projection_all_simulations.rds")
g <- df %>% 
  filter(itn_coverage_past=="mean") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=scenario)) +
  geom_line() +
  facet_wrap(~NAME_1) +
  xlab("Year") +
  scale_x_continuous(limits=c(2010, 2030)) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
ggsave("outputs/projection_plots/scenarios.pdf", g,
       width = 12, height = 8)

g <- df %>% 
  filter(itn_coverage_past=="lower") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=scenario)) +
  geom_line() +
  facet_wrap(~NAME_1) +
  xlab("Year") +
  scale_x_continuous(limits=c(2010, 2030)) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
ggsave("outputs/projection_plots/scenarios_lower.pdf", g,
       width = 12, height = 8)

g <- df %>% 
  filter(itn_coverage_past=="upper") %>% 
  ggplot(aes(x=year, y=prev_all_smooth, colour=scenario)) +
  geom_line() +
  facet_wrap(~NAME_1) +
  xlab("Year") +
  scale_x_continuous(limits=c(2010, 2030)) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
ggsave("outputs/projection_plots/scenarios_upper.pdf", g,
       width = 12, height = 8)