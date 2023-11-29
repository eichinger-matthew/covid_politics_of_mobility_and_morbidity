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
# load sociodem data for population, need to calculate rate of cases
sdems <- read_rds("county_sociodems_and_elect_returns.rds")
pops <- select(sdems, fips, total_population_2018) %>% arrange(fips)
# filter counties that have fewer than 825 obs; pull fips and filter for later
ckeep <-
  cmob %>%
  filter(!is.na(fips) & !is.na(retail_and_recreation_percent_change_from_baseline)) %>%
  count(fips) %>%
  filter(n >= 825) %>%
  pull(fips)

# for each county, get cumulative count of cases
# join with population data
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
         day_record_end = max(date),
         cases_cum = cumsum(cases), deaths_cum = cumsum(deaths),
         retail_l1 = lag(retail), retail_l2 = lag(retail_l1)) %>%
  ungroup() %>%
  left_join(pops, by = c('fips')) %>%
  mutate(cumcases_per100k = cases_cum*(100000/total_population_2018),
         cumdeaths_per100k = deaths_cum*(100000/total_population_2018))
  
# define start time baased on cumulative cases
# add 1 to surv time to make sure no one survives 0 days
cumcounts_small <-
  cumcounts %>%
  mutate(day = as.integer(difftime(date, day_record_start, units = 'days'))) %>%
  group_by(fips) %>%
  mutate(t0 = min(day[cumcases_per100k >= 100], na.rm = TRUE),
         tE = 
           ifelse(is.infinite(min(day[retail >= 0 & retail_l1 >= 0 & 
                                        retail_l2 >=0 & day >= t0], na.rm = TRUE)),
                  max(day[!is.na(retail) & !is.na(retail_l1) & !is.na(retail_l2)], na.rm = TRUE),
                  min(day[retail >= 0 & retail_l1 >= 0 & 
                            retail_l2 >=0 & day >= t0], na.rm = TRUE)),
         surv_time = tE - t0 + 1,
         right_censor = ifelse(max(day, na.rm = TRUE) == tE, 1, 0)) %>%
  ungroup()

# subset to only relevant vars
retail_df <-
  cumcounts_small %>%
  select(fips, sub_region_1, sub_region_2, day_record_start, date, day,
         t0, tE, surv_time, right_censor, cases, cases_cum, cumcases_per100k,
         cases_avg, cases_avg_per_100k,
         deaths, deaths_cum, cumdeaths_per100k, deaths_avg, deaths_avg_per_100k,
         retail, retail_l1, retail_l2)
#saveRDS(retail_df, file = "master_county_covid_rates_and_retail_mobility.rds")


# dataset with aggregate survival, sociodemographic, and electoral data ------------

# load sociodem data
socdems <- read_rds("county_sociodems_and_elect_returns.rds")
# get agg survival patterns from retail df
surv <- 
  retail_df %>%
  select(fips, sub_region_1, sub_region_2, day_record_start,
         t0, tE, surv_time, right_censor) %>%
  distinct() %>%
  mutate(entry_indic = ifelse(is.infinite(t0), "never entered", "entered"))
# join
county_surv <- 
  socdems %>%
  left_join(surv, by = c('fips'))
# save
#saveRDS(county_surv, file = 'survival_data.rds')







