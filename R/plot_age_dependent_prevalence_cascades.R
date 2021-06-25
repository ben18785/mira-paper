library(tidyverse)
library(purrr)

# data from cross-sectional MIRA survey in 2017
prevalence_cascades <- readRDS("data/raw/prevalence_age_cascades.rds") %>% 
  mutate(name=paste0("age_", lower_age, "_", upper_age)) %>% 
  mutate(lower=qbeta(0.025, 1 + infected, 1 + n - infected)) %>% 
  mutate(upper=qbeta(0.975, 1 + infected, 1 + n - infected)) %>% 
  select(name, prevalence, lower, upper) %>% 
  rename(value=prevalence) %>% 
  mutate(year=2017) %>% 
  mutate(type="actual")

# get corresponding simulation data
temp <- readRDS("outputs/simulations/counterfactual_all_simulations.rds") %>%
  filter(NAME_1=="Cascades") %>% 
  mutate(year=round(year)) %>% 
  group_by(year) %>% 
  summarise(age_0_5=mean(prev_0_5_smooth),
            age_5_15=mean(prev_5_15_smooth),
            age_15_25=mean(prev_15_25_smooth),
            age_25_35=mean(prev_25_35_smooth),
            age_35_45=mean(prev_35_45_smooth),
            age_45_100=mean(prev_45_100_smooth)) %>% 
  pivot_longer(-year) %>% 
  mutate(type="simulated")

# combine the two
combined <- prevalence_cascades %>% 
  bind_rows(temp) %>% 
  mutate(name=gsub("age_", "", name)) %>% 
  mutate(name=gsub("_", "-", name)) %>% 
  rename(age=name) %>% 
  mutate(age=as.factor(age)) %>% 
  mutate(age=fct_relevel(age, "0-5", "5-15", "15-25",
                         "25-35", "35-45", "45-100")) %>% 
  filter(year %in% c(2000, 2017))

g <- ggplot(combined %>% filter(type=="simulated"),
       aes(x=age, y=value, group=as.factor(year),
           colour=as.factor(year), shape=as.factor(type))) +
  geom_point() +
  geom_line() +
  geom_point(data=combined %>% filter(type=="actual")) +
  scale_y_continuous(labels=scales::percent) +
  scale_color_brewer("Year", palette = "Dark2") +
  scale_shape("Data") +
  xlab("Age") +
  ylab("Prevalence")
ggsave("outputs/counterfactual_plots/age_prevalence_cascades.pdf", g,
       width = 7, height = 7)
saveRDS(combined, "data/processed/cascades_actual_fitted_age_prevalence.rds")