# make analysis dataframe for covid paper
rm(list = ls())
# load libraries
library(tidyverse)
library(janitor)

select <- dplyr::select

# set wd
setwd("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity")

# load time-series data
cmob <- read_rds("all_data_covid_and_mobility.rds")

# filter counties that have fewer than 825 obs; pull fips and filter for later
ckeep <-
  cmob %>%
  filter(!is.na(fips) & !is.na(retail_and_recreation_percent_change_from_baseline)) %>%
  count(fips) %>%
  filter(n >= 825) %>%
  pull(fips)

# for each county, get cumulative count of cases
cumcounts <-
  cmob %>%
  filter(fips %in% ckeep, !is.na(retail_and_recreation_percent_change_from_baseline)) %>%
  arrange(fips, date) %>%
  rename(retail = retail_and_recreation_percent_change_from_baseline,
         groc = grocery_and_pharmacy_percent_change_from_baseline,
         work = workplaces_percent_change_from_baseline,
         res = residential_percent_change_from_baseline) %>%
  mutate(cases = ifelse(cases == -1, 0, cases)) %>%
  group_by(fips) %>%
  mutate(day_record_start = min(date),
         cases_cum = cumsum(cases), deaths_cum = cumsum(deaths),
         retail_l1 = lag(retail), retail_l2 = lag(retail_l1)) %>%
  ungroup() %>%
  mutate(day = as.integer(difftime(date, day_record_start, units = 'days'))) %>%
  group_by(fips) %>%
  mutate(t0 = min(day[cases_avg_per_100k >= 10], na.rm = TRUE),
         tE = 
           ifelse(is.infinite(min(day[retail >= 0 & retail_l1 >= 0 & 
                                        retail_l2 >=0 & day >= t0], na.rm = TRUE)),
                  max(day[!is.na(retail) & !is.na(retail_l1) & !is.na(retail_l2)], na.rm = TRUE),
                  min(day[retail >= 0 & retail_l1 >= 0 & 
                            retail_l2 >=0 & day >= t0], na.rm = TRUE)),
         surv_time = tE - t0)

# subset to only relevant vars
retail_df <-
  cumcounts %>%
  select(fips, sub_region_1, sub_region_2, day_record_start, date, day,
         t0, tE, surv_time, cases, cases_cum, cases_avg, cases_avg_per_100k,
         deaths, deaths_cum, deaths_avg, deaths_avg_per_100k,
         retail, retail_l1, retail_l2)
#saveRDS(retail_df, file = "master_county_covid_rates_and_retail_mobility.rds")


# dataset with aggregate survival, sociodemographic, and electoral data ------------

# load sociodem data
socdems <- read_rds("county_sociodems_and_elect_returns.rds")
# get agg survival patterns from retail df
surv <- 
  retail_df %>%
  select(fips, sub_region_1, sub_region_2, day_record_start,
         t0, tE, surv_time) %>%
  distinct()
# join
county_surv <- 
  socdems %>%
  left_join(surv, by = c('county_fips' = 'fips'))
# save
#saveRDS(county_surv, file = 'survival_data.rds')







