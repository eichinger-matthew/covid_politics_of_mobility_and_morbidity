# preemption analysis
rm(list = ls())
# load libraries
library(tidyverse)
library(broom)
library(kableExtra)
library(tseries)
library(wesanderson)
library(lubridate)
library(vars)

select <- dplyr::select

# load data
dat <- read_rds("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity/master_county_covid_rates_and_retail_mobility.rds")
covid <- read_rds("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity/county_sociodems_and_elect_returns.rds")

# measure preemption variable under continuous and binary setting
# calculate mean mobility across days before April 1
# just to get a sense of the data, see the earliest day recorded for counties
start_days <-
  dat %>%
  group_by(fips) %>%
  filter(date == min(date, na.rm = TRUE)) %>%
  ungroup() %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line()

# binary
preempt_bin <-
  dat %>%
  select(fips, date, day, t0, tE, surv_time, retail, retail_l1, retail_l2) %>%
  group_by(fips) %>%
  filter(date <= "2020-04-01" & date >= "2020-03-13") %>%
  mutate(preempt = ifelse(retail < 0 & retail_l1 < 0 & retail_l2 < 0, 1, 0)) %>%
  summarise(preempt_bin = ifelse(1 %in% preempt, 1, 0))
# continuous
preempt_cont <-
  dat %>%
  select(fips, date, day, t0, tE, surv_time, retail, retail_l1, retail_l2) %>%
  group_by(fips) %>%
  filter(date <= "2020-04-01" & date >= "2020-03-13") %>%
  summarise(preempt_cont = -mean(retail[day <= t0], na.rm = TRUE))
# put into political df
# 'covid' can easily be filtered to exclude these obs., but no points at moment
covid <- 
  covid %>%
  left_join(preempt_bin, by = c('fips')) %>%
  left_join(preempt_cont, by = c('fips'))

# plots ---------------------------

# average preemption as function of vote share
preempt_vshare <-
  covid %>%
  mutate(elect_winner = ifelse(rep_frac > dem_frac, "Republican", "Democrat")) %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = preempt_cont, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  lims(y = c(-5, 80)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Fraction of votes for Trump, 2016",
       y = "Average preemption",
       title = "Preempting COVID-19: Average Mobility Scores \nBetween March 13 and April 1, 2020") +
  theme_minimal() +
  theme(axis.line = element_line())



# treatment effect model ----------------------

# load treatment df from match.R file
load("treatment_matching_results.RData")

# from preempt model, take county id and preemption dvs
preempt_vars <- 
  covid %>% 
  select(fips, preempt_bin, preempt_cont, rep_frac, dem_frac)

# from match data, take the fifth (out of 10) glm model
# join preemption dv to matching-adjusted data
df <- 
  match_res$glm[[5]]$adjusted_data %>%
  left_join(preempt_vars, "fips") %>%
  mutate(dem_treat = ifelse(elect_winner == "Democrat", 1, 0))

# hand-calculate treatment effect
mean(df$preempt_cont[df$dem_treat == 1]*df$weights[df$dem_treat == 1], 
     na.rm = TRUE)
mean(df$preempt_cont[df$dem_treat == 0]*df$weights[df$dem_treat == 0],
     na.rm = TRUE)
# same plot as above, but with weights
df %>%
  mutate(elect_winner = ifelse(rep_frac > dem_frac, "Republican", "Democrat")) %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = preempt_cont*weights, col = elect_winner)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  lims(y = c(-5, 80)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Fraction of votes for Trump, 2016",
       y = "Average preemption",
       title = "Preempting COVID-19: Average Mobility Scores \nBetween March 13 and April 1, 2020") +
  theme_minimal() +
  theme(axis.line = element_line())

# estimate ATE as linear function of treatment
# adjust regression with selection weights
mod <- lm(preempt_cont ~ dem_treat, data = df, weights = weights)
# get summary
summary(mod)

# sensitivity analysis -----------------------

# results may be sensitive to the caliper hyperparameter
# good practice to characterize how the ATE changes as a function of it
# loop over selection dfs from match.R file
# calculate ATE for each model and store parameter info in container

# container
preempt_res <- list()
# loop
for(i in seq_along(match_res$glm)){
  # get data
  d <- match_res$glm[[i]]$adjusted_data
  d <- left_join(d, preempt_vars, "fips")
  d <- mutate(d, dem_treat = ifelse(elect_winner == "Democrat", 1, 0))
  # estimate model
  fit <- lm(preempt_cont ~ dem_treat, data = d, weights = weights)
  # assign to tibble
  cfs <- tidy(fit, conf.int = TRUE)
  # get caliper
  calip <- match_res$glm[[i]]$matching_model$caliper[[1]]
  cfs <- mutate(cfs, caliper = calip)
  preempt_res[[i]] <- cfs
  # what to do at end
  if(i == 10){
    fin <- do.call(rbind, preempt_res)
  }
}

# get average preempt coef
mean(fin$estimate[fin$term == "dem_treat"])

# plot ATE estimate with CI as f'n of caliper hyperparameter
preempt_coefs <-
  fin %>%
  filter(term == "dem_treat") %>%
  ggplot(aes(x = caliper, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = 2) +
  lims(y = c(0, 10)) +
  labs(x = "Caliper value", y = "Coefficient estimate",
       title = "Average Preemption Effect of Democratic Status",
       subtitle = "By caliper value used in matching procedure") +
  theme_minimal() +
  theme(axis.line = element_line(),
        plot.margin = margin(t = 20, r = 50, b = 40, l = 20))




