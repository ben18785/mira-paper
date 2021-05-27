rm(list=ls())
# devtools::install_github("ben18785/mlgtsmira",
#                          ref="main",
#                          auth_token = "xyz") # put your token here
library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)

# Load model inputs used to run GTS
load("data/raw/input.RData")
temp_input <- input %>% filter(NAME_0=="Burkina Faso")

# ITNs
## Create more bespoke version of Burkina Faso using
## finescaled data (for Cascades only; otherwise use GTS coverage data)
clinical <- readRDS("data/raw/clinical.rds")
clinical$dorssmoust <- as.numeric(clinical$dorssmoust)
itn_2017 <- sum(clinical$dorssmoust, na.rm = T) / nrow(clinical)

# substitute interventions
int <- temp_input %>% 
  filter(NAME_1=="Cascades", ur=="urban") %>% 
  pull(interventions)
int <- int[[1]]

int <- int %>%
  mutate(llin=if_else(year >= 2017, itn_2017, llin))

temp_input <- temp_input %>% 
  mutate(interventions=if_else(NAME_1=="Cascades",
                               list(int), interventions))


# Resistance
## add in most recent estimates of resistance from Lambert et al (2021)
reser_df <- readRDS("data/raw/subnational_resistance.rds") %>% 
  filter(NAME_0=="Burkina Faso")
for(i in seq_along(reser_df$NAME_0)) {
  temp <- reser_df[i, ]
  a <- temp$resistance[[1]] %>% 
    rename(resistance=middle)
  
  temp1 <- temp_input %>%
    filter(NAME_1==temp$NAME_1, ur==temp$ur) %>% 
    mutate(resistance=list(a))
  if(i == 1) {
    big_df <- temp1
  }else {
    big_df <- big_df %>% bind_rows(temp1)
  }
}

# Merge urban / rural units as there's no distinction in our prevalence data
temp_input <- big_df

urban_df <- temp_input %>% 
  group_by(NAME_0, NAME_1) %>% 
  count()
unseparated <- urban_df %>% 
  filter(n==1)
separated <- urban_df %>% 
  filter(n==2)
separated_df <- temp_input %>% 
  filter(NAME_1%in%separated$NAME_1) %>% 
  arrange(NAME_1)

# go through and check for differences
ldemog <- vector(length = nrow(separated))
lvectors <- vector(length = nrow(separated))
linterventions <- vector(length = nrow(separated))
lseasonality <- vector(length = nrow(separated))

for(i in seq_along(separated$NAME_1)) {
  short_df <- temp_input %>% filter(NAME_1==separated$NAME_1[i])
  ldemog[i] <- sum(short_df$demog[[1]] - short_df$demog[[2]])
  lvectors[i] <- sum(short_df$vectors[[1]] - short_df$vectors[[2]])
  linterventions[i] <- sum(short_df$interventions[[1]] %>% select_if(is.numeric) - short_df$interventions[[2]]%>% select_if(is.numeric))
  lseasonality[i] <- sum(short_df$seasonality[[1]] - short_df$seasonality[[2]])
}

if(sum(ldemog) != 0)
  stop("Demog differences are non-zero.")
if(sum(lvectors) != 0)
  stop("Vector differences are non-zero.")
if(sum(linterventions) != 0)
  stop("Intervention differences are non-zero.")
if(sum(lseasonality) != 0)
  stop("Seasonality differences are non-zero.")

## take mean of urban and rural population sizes
for(i in seq_along(separated$NAME_1)) {
  short_df <- temp_input %>% filter(NAME_1==separated$NAME_1[i])
  pop_1 <- temp_input$pop[[1]] %>% mutate(ur=temp_input$ur[[1]])
  pop_2 <- temp_input$pop[[2]] %>% mutate(ur=temp_input$ur[[2]])
  pop_df <- pop_1 %>% 
    bind_rows(pop_2) %>% 
    group_by(year) %>% 
    summarise(p=mean(p))
  short_df1 <- short_df[1, ] %>% 
    mutate(pop=list(pop_df))
  if(i == 1) {
    temper <- short_df1 
  } else {
    temper <- temper %>% bind_rows(short_df1)
  }
}
temper <- temper %>% 
  bind_rows(temp_input %>% filter(NAME_1 %in% unseparated$NAME_1)) %>% 
  mutate(ur=NA)

# Change phi_i and phi_b for all units
for(i in seq_along(temper$NAME_0)) {
  temp_all <- temper[i, ]
  temp <- temp_all$vectors[[1]]
  ins <- colnames(temp)[str_detect(colnames(temp), "Q_in")]
  bs <- colnames(temp)[str_detect(colnames(temp), "Q_bed")]
  both <- c(ins, bs)
  temp[1, match(both, colnames(temp))] <- 0.787
  temp_all <- temp_all %>% mutate(vectors=list(temp))
  if(i == 1)
    big_df <- temp_all
  else
    big_df <- big_df %>% bind_rows(temp_all)
}

saveRDS(big_df, "data/processed/input_bf_only.rds")