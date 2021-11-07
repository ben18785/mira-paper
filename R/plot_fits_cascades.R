library(mlgts)
library(tidyverse)
library(purrr)
library(reshape2)
library(lubridate)
source("R/fit_site.R")
source("R/run.R")
source("R/helper.R")

annual <- readRDS("data/raw/monthly_prevalence.rds") %>% 
  filter(year >= 2000) %>% 
  filter(adminLevel1=="Cascades")

annual1 <- readRDS("data/raw/prevalence_olyset_mira.rds") %>%
  mutate(size=round((1 / prevalence) * n_troph_any)) %>% 
  rename(positive=n_troph_any) %>% 
  mutate(year=year(date),
         month=month(date)) %>% 
  filter(source=="Olyset") %>% 
  mutate(lower_age_r=0,
         upper_age_r=5,
         age_range_r="0-5") %>% 
  mutate(adminLevel1="Cascades")

annual <- annual %>% 
  bind_rows(annual1)

temp_input <- readRDS("data/processed/input_mean.rds")
m_fits <- readRDS("data/processed/m_fits_all.rds")

# plot only mean scenario
m <- m_fits %>% 
  filter(NAME_1=="Cascades") %>% 
  filter(itn_scenario=="mean") %>% 
  pull(m)

cascades <- temp_input %>%
  filter(NAME_1=="Cascades") %>%
  mutate(total_M=m)
cascades <- remove_post_2018_interventions(cascades)
temp <- purrr::pmap(cascades, f_run_model)
outputs <- temp[[1]]$output

g <- f_plot_specific_simple("Cascades", outputs, annual) +
  ggtitle("Cascades")

ggsave(paste0("outputs/fit_plots/fit_res_on_Cascades_inc_olyset.pdf"),
       g, width = 10, height = 6)

# Generate data for figure
NAME_1 <- "Cascades"
temp <- outputs
temp$year <- temp$year + 2000
temp$date1 <- temp$year
temp$year1 <- floor(temp$year)
temp <- temp %>%
  group_by(year1) %>%
  mutate(month=seq_along(year1)) %>%
  mutate(year=floor(year)) %>%
  filter(year <= 2018)
aShortDF <- annual %>%
  filter(adminLevel1==NAME_1)
temp <- left_join(temp, aShortDF, by = c("year", "month"))

lAgeRange <- temp$age_range_r
lAgeRange1 <- gsub("-", "_", lAgeRange)
lNames <- map_chr(lAgeRange1, ~paste0("prev_", .))
temp$col <- match(lNames, colnames(temp))
# temp <- temp[, c(c(1, match("month", colnames(temp)), match("date1", colnames(temp)), 50:ncol(temp)), unique(temp$col[!is.na(temp$col)]))]
aLookup <- as.data.frame(table(annual$age_range_r)) %>%
  rename(age_range=Var1) %>%
  mutate(age_range_r=age_range) %>%
  left_join(annual %>% ungroup() %>%
              select(age_range_r, lower_age_r, upper_age_r) %>%
              unique(), by = "age_range_r") %>%
  mutate(middle=0.5 * (lower_age_r + upper_age_r)) %>%
  arrange(middle) %>%
  mutate(num=seq_along(age_range))
annual$age_range_r <- factor(annual$age_range_r)
annual$age_range_r <- fct_relevel(annual$age_range_r,
                                  as.character(aLookup$age_range_r))
n <- n_distinct(annual$age_range_r)
col <- setNames(hcl(seq(15,375,length=n+1)[1:n], 100, 65), levels(annual$age_range_r))

temp1 <- select(temp %>% ungroup(), date1, contains("prev_")) %>%
  select(-contains("smooth")) %>%
  select(-contains("prev_all")) %>%
  select(-contains("prev_2_10")) %>%
  melt(id.vars="date1") %>%
  rename(age_range=variable)
temp1$age_range <- gsub("prev_", "", temp1$age_range)
temp1$age_range <- gsub("_", "-", temp1$age_range)
temp1$num <- aLookup$age_range[match(temp1$age_range, aLookup$age_range)]
temp1 <- temp1 %>%
  filter(!is.na(num))
lAgeRange2 <- lAgeRange1[!is.na(lAgeRange1)] %>% unique()
lAgeRange2 <- gsub("_", "-", lAgeRange2)
temp1 <- temp1 %>%
  filter(age_range%in%lAgeRange2)
temp <- temp %>%
  ungroup() %>%
  mutate(lower=qbeta(0.025, 1 + positive, 1 + size - positive),
         upper=qbeta(0.975, 1 + positive, 1 + size - positive)) %>%
  mutate(age_range_r=as.factor(age_range_r))
temp$age_range_r=fct_relevel(temp$age_range_r,
                             as.character(aLookup$age_range_r[aLookup$age_range_r%in%unique(temp$age_range_r)]))
saveRDS(temp, "data/processed/cascades_fit_prevalence_1.rds")
saveRDS(temp1, "data/processed/cascades_fit_prevalence_2.rds")