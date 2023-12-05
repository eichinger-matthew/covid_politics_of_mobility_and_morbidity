# load libraries
rm(list = ls())
library(tidyverse)
library(broom)
library(kableExtra)
library(tseries)
library(wesanderson)
library(lubridate)
library(vars)

setwd("C:/Users/eichi/Desktop/covid_mobility_and_morbidity")

# fixed commands
select <- dplyr::select
q <- 
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1),
        plot.margin = margin(t = 20, r = 30, b = 10, l = 20))

# load data
dat <- read_rds("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity/master_county_covid_rates_and_retail_mobility.rds")


# fit a var model for hennepin ------
# get mn data
mn <- 
  dat %>%
  filter(sub_region_1 == "Washington", sub_region_2 == "King County") %>%
  select(fips, date, day, cases_avg_per_100k, retail) %>%
  rename(cases = cases_avg_per_100k, mobility = retail)

# plot mobility for king county (seattle)
mob <-
  mn %>%
  ggplot(aes(x = date, y = mobility)) +
  geom_line(col = 'firebrick') +
  geom_hline(yintercept = 0, linetype = 2) +
  annotate(geom = 'rect', xmin = as.Date("2020-05-01"), 
           xmax = as.Date("2020-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  annotate(geom = 'rect', xmin = as.Date("2021-05-01"), 
           xmax = as.Date("2021-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  annotate(geom = 'rect', xmin = as.Date("2022-05-01"), 
           xmax = as.Date("2022-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  labs(x = "Date", y = "Mobility",
       title = "Mobility Time-Series for King County, Washington",
       subtitle = "Summer periods shaded") +
  q

# plot mobility and cases
mn %>%
  pivot_longer(cols = c(mobility, cases), names_to = "series", values_to = "nums") %>%
  ggplot(aes(x = date, y = nums, col = series)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  annotate(geom = 'rect', xmin = as.Date("2020-05-01"), 
           xmax = as.Date("2020-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  annotate(geom = 'rect', xmin = as.Date("2021-05-01"), 
           xmax = as.Date("2021-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  annotate(geom = 'rect', xmin = as.Date("2022-05-01"), 
           xmax = as.Date("2022-09-01"), ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.4) +
  scale_color_manual(name = "Series", values = c("firebrick", "steelblue"),
                   labels = c("Cases", "Mobility")) +
  labs(x = "Date", y = "Mobility/Case rate",
       title = "Time-series of Mobility for King County, Washington",
       subtitle = "Summer periods shaded") +
  q

# estimate best lag from info criteria
lag_check <- VARselect(y = mn[,4:5], lag.max = 14, type = "both", season = 7)
# save best lag
lag_choice <- lag_check$selection[[1]]
# estimate var model with best lag
var_mod <- vars::VAR(y = mn[,4:5], p = lag_choice, type = "both", season = 7)

# get fitted values, add NA for number of lags
fitted_cases <- c(rep(NA, lag_choice), var_mod$varresult$cases$fitted.values)
fitted_mobility <- c(rep(NA, lag_choice), var_mod$varresult$mobility$fitted.values)
# gather into dataframe with original data
df_fit <- 
  tibble("cases" = mn$cases, "mobility" = mn$mobility, 
         fitted_cases, fitted_mobility,
         "day" = mn$day, "date" = mn$date)
# plot
df_fit %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cases), col = "steelblue") +
  geom_line(aes(y = mobility), col = "firebrick") +
  geom_line(aes(y = fitted_cases), col = "purple") +
  geom_line(aes(y = fitted_mobility), col = "yellow")


# regression example, using only past 7 days





# functions to estimate vars for every county
estim_var <- function(df){
  # choose best lag
  lag_check <- VARselect(y = df[,6:7], lag.max = 14, type = "both")
  # use best-performing lag... for some reason this literally does not fucking work
  # lag_choice <- lag_check$selection[[1]]
  # estimate var model with best lag
  var_mod <- vars::VAR(y = df[,6:7], p = lag_check$selection[[1]], type = "both")
  
  # get fitted values
  fitted_cases <- c(as.numeric(rep(NA, lag_check$selection[[1]])), var_mod$varresult$cases$fitted.values)
  fitted_mobility <- c(as.numeric(rep(NA, lag_check$selection[[1]])), var_mod$varresult$mobility$fitted.values)
  # put fitted vals into dataframe
  fits <- 
    tibble("cases" = df$cases, "mobility" = df$mobility, 
           fitted_cases, fitted_mobility,
           "day" = df$day, "date" = df$date) %>%
    mutate(resid_mob = mobility - fitted_mobility,
           resid_cases = cases - fitted_cases)
  
  # calculate RMSE as basic measure of error
  root_mse_mobility <- sqrt(mean(fits$resid_mob^2, na.rm = TRUE))
  root_mse_cases <- sqrt(mean(fits$resid_cases^2, na.rm = TRUE))
  # calculate impulse response
  imp_resp <- irf(var_mod, impulse = "cases", response = "mobility", n.ahead = 14)
  # calculate average mobility over horizon
  avg_response <- mean(imp_resp$irf$cases)
  # make list of everything
  res <- 
    list("original_data" = df, "model" = var_mod, "fitted_values" = fits,
         "metrics" = list("rmse_mobility" = root_mse_mobility, 
                          "rmse_cases" = root_mse_cases),
         "impulse_response_function" = list("imp_resp" = imp_resp,
                                            "average_mobility_response" = avg_response))
  return(res)
}

# make grouped list of dataframes like the mn one above
ckeep <-
  dat %>%
  count(fips) %>%
  filter(n >= 800) %>%
  pull(fips)
# make grouped list
county_list <-
  dat %>%
  filter(fips %in% ckeep) %>%
  select(fips, sub_region_1, sub_region_2,
         date, day, cases_avg_per_100k, retail) %>%
  rename(cases = cases_avg_per_100k, mobility = retail) %>%
  group_by(fips) %>%
  group_split()
# apply var estimation to grouped list
county_vars <-
  county_list %>%
  map(estim_var)

# save
#save(county_vars, file = "var_timeseries_results.RData")

# get irf data
irfs <- list()
for(i in seq_along(county_vars)){
  # get county info
  info <- 
    county_vars[[i]]$original_data[1,1:3] %>%
    mutate(avg_irf = county_vars[[i]]$impulse_response_function$average_mobility_response)
  # add to list
  irfs[[i]] <- info
}
# rbind to get irfs
all_irfs <- do.call(rbind, irfs)
# save to avoid loading var results every time
write_csv(all_irfs, file = "average_irfs_reactions.csv")


# Example VAR plots through King County -------------------

# load
#load("var_timeseries_results.RData")

# extract King County county - have to figure out which it is
seattle <- NA
for(i in seq_along(county_vars)){
  if(county_vars[[i]]$original_data$sub_region_1[1] == "Washington" &
     county_vars[[i]]$original_data$sub_region_2[1] == "King County"){
    print(i)
    seattle <- i
  }
}
# is 1377
# get seattle data from list
seattlemod <- county_vars[[seattle]]
# plot VAR series without fitted
seattlemod$fitted_values %>%
  pivot_longer(cols = c(cases, mobility, fitted_cases, fitted_mobility),
               names_to = "series", values_to = "nums") %>%
  ggplot(aes(x = date, y = nums, col = series)) +
  geom_line() +
  scale_color_viridis_d(name = "Series",
                        labels = c("Case rate", "Fitted case rate",
                                   "Fitted mobility", "Mobility"),
                        alpha = 0.5) +
  labs(x = "Date", y = "Case rate/Mobility level",
       title = "VAR(14) System of Mobility and COVID-19 Case Rate",
       subtitle = "King County, Washington",
       color = "Series", linetype = "Series") +
  q
# plot impulse response
seattleirfs <- 
  tibble("pred_mob" = seattlemod$impulse_response_function$imp_resp$irf$cases,
         "lowerci" = seattlemod$impulse_response_function$imp_resp$Lower$cases,
         "upperci" = seattlemod$impulse_response_function$imp_resp$Upper$cases,
         "horizon" = seq(1, 15, 1))
seattleirfs %>%
  ggplot() +
  geom_line(aes(x = horizon, y = pred_mob)) +
  geom_ribbon(aes(x = horizon, ymin = lowerci, ymax = upperci), alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Forecast, n-steps ahead", y = "Mobility score",
       title = "Mobility Response for Hennepin County, Minnesota",
       subtitle = "Following a 1sd impulse shock to the county case rate") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# IRFS -----

#
all_irfs <- read_csv("average_irfs_reactions.csv")

# load matching results to get adjustment weights
load("treatment_matching_results.RData")
# join fifth caliper matching results to irf data
fin_irfs <-
  all_irfs %>%
  left_join(match_res$glm[[5]]$adjusted_data, by = c('fips'))

# plot irf densities by election winner
irf_plot <-
  fin_irfs %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = avg_irf*weights, group = elect_winner, linetype = elect_winner)) +
  geom_density(position = 'identity') +
  scale_linetype_manual(name = "Election Winner", values = c(1, 2)) +
  labs(x = "Average Mobility Response", y = "Density",
       title = "Average Mobility Reactions to COVID-19 Shock",
       subtitle = "Calculated using IRFs over a 14-day forecast horizon") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1)) +
  q
# t-test of whether the distributions have different means
reps <- fin_irfs$avg_irf[fin_irfs$elect_winner == "Republican"]*
  fin_irfs$weights[fin_irfs$elect_winner == "Republican"]
dems <- fin_irfs$avg_irf[fin_irfs$elect_winner == "Democrat"]*
  fin_irfs$weights[fin_irfs$elect_winner == "Democrat"]
t.test(x = reps, y = dems, na.action = 'omit')







