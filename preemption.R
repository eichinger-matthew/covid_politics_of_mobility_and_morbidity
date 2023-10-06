# preemption analysis
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
dat <- read_rds("county_covid_elections_survival_with_first_wave_full.rds")
covid <- read_rds("county_covid_survival_basic.rds")

# measure preemption variable under continuous and binary setting
# calculate mean mobility across days less than day_fifty

# binary
preempt_bin <-
  dat %>%
  select(county_fips, date, day_num, retail_rec_pct_change_base, day_fifty) %>%
  rename(mobility = retail_rec_pct_change_base) %>%
  group_by(county_fips) %>%
  filter(date <= "2020-04-01") %>%
  mutate(mob_l1 = lag(mobility), mob_l2 = lag(lag(mobility)),
         preempt = ifelse(mobility < 0 & mob_l1 < 0 & mob_l2 < 0, 1, 0)) %>%
  summarise(preempt_bin = ifelse(1 %in% preempt, 1, 0))
# continuous
preempt_cont <-
  dat %>%
  select(county_fips, date, day_num, retail_rec_pct_change_base, day_fifty) %>%
  rename(mobility = retail_rec_pct_change_base) %>%
  group_by(county_fips) %>%
  filter(date <= "2020-04-01") %>%
  summarise(preempt_cont = mean(mobility[day_num <= day_fifty], na.rm = TRUE))
# put into political df
covid <- covid %>%
  left_join(preempt_bin, by = "county_fips") %>%
  left_join(preempt_cont, by = "county_fips")

# plots ---------------------------

# average preemption as function of vote share
preempt_vshare <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = preempt_cont, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Fraction of votes for Trump, 2016",
       y = "Average mobility score",
       title = "Preempting COVID-19: Average Mobility Scores \nBetween March 13 and April 1, 2020") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# modeling ----------------------

# load treatment df
load("treatment_matching_results.RData")

# get a couple vars from covid
preempt_vars <- 
  covid %>% 
  select(county_fips, preempt_bin, preempt_cont)

# get eighth model
df <- match_res$glm[[8]]$adjusted_data %>%
  left_join(preempt_vars, "county_fips") %>%
  mutate(dem_treat = ifelse(elect_winner == "Democrat", 1, 0))

# estimate linear model
mod <- lm(preempt_cont ~ dem_treat, data = df, weights = weights)
summary(mod)

# use example above in a loop
preempt_res <- list()
for(i in seq_along(match_res$glm)){
  # get data
  d <- match_res$glm[[i]]$adjusted_data
  d <- left_join(d, preempt_vars, "county_fips")
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


# plot results ------
fin %>%
  filter(term == "dem_treat") %>%
  ggplot(aes(x = caliper, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Caliper value", y = "Coefficient estimate",
       title = "Marginal Preemption Effect of Democratic Status",
       subtitle = "By caliper value used in matching procedure") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))



  
