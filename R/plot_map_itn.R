library(tidyverse)

big_df <- readRDS("data/processed/compare_map_old_itns.rds")

g <- big_df %>% 
  pivot_wider(id_cols = c(year, NAME_1), names_from=name,
              values_from=value) %>% 
  pivot_longer(c(mean, old)) %>% 
  ggplot(aes(x=year, y=value)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="blue", alpha=0.2) +
  geom_line(aes(colour=name)) +
  facet_wrap(~NAME_1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer("Variable", palette = "Dark2")

ggsave("outputs/counterfactual_plots/map_vs_old_itn.pdf", g, width = 12, height = 8)