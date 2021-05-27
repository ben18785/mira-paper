library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000)
temp_input <- readRDS("data/processed/input_bf_only.rds")
m_results <- matrix(nrow = nrow(temp_input),
                    ncol = 3)
for(i in seq_along(temp_input$NAME_0)) {
  temp <- temp_input[i, ]
  res <- temp$resistance[[1]]
  # turn resistance off
  res$resistance <- 0
  temp_input$resistance[[i]] <- res
  print(temp$NAME_1)
  test <- f_find_m(temp$NAME_1, annual, temp_input, num_people_ = 20000)
  m_results[i, ] <- c(temp$NAME_1, test$m, -test$log_likelihood_negative)
}
colnames(m_results) <- c("NAME_1", "m", "log_likelihood")
m_results <- as.data.frame(m_results)
saveRDS(m_results, "data/processed/m_fits_off.rds")