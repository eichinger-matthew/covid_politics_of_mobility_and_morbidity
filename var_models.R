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

# fit a var model for hennepin ------
# get mn data
mn <- 
  dat %>%
  filter(sub_region_1 == "Minnesota", sub_region_2 == "Hennepin County") %>%
  select(county_fips, date, day_num, cases_avg_per_100k, retail_rec_pct_change_base) %>%
  rename(cases = cases_avg_per_100k, mobility = retail_rec_pct_change_base)

# plot mobility
mob <-
  mn %>%
  ggplot(aes(x = date, y = mobility)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Date", y = "Mobility",
       title = "Time-series of Mobility for Hennepin County, Minnesota") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# plot mobility and cases
mn %>%
  pivot_longer(cols = c(mobility, cases), names_to = "series", values_to = "nums") %>%
  ggplot(aes(x = date, y = nums, group = series, linetype = series)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Date", y = "Mobility/Case rate",
       title = "Time-series of Mobility for Hennepin County, Minnesota") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

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
         "day" = mn$day_num, "date" = mn$date)
# plot
df_fit %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cases), col = "steelblue") +
  geom_line(aes(y = mobility), col = "firebrick") +
  geom_line(aes(y = fitted_cases), col = "purple") +
  geom_line(aes(y = fitted_mobility), col = "yellow")


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
           "day" = df$day_num, "date" = df$date) %>%
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
  count(county_fips) %>%
  filter(n >= 600) %>%
  pull(county_fips)
# make grouped list
county_list <-
  dat %>%
  filter(county_fips %in% ckeep) %>%
  select(county_fips, sub_region_1, sub_region_2,
         date, day_num, cases_avg_per_100k, retail_rec_pct_change_base) %>%
  rename(cases = cases_avg_per_100k, mobility = retail_rec_pct_change_base) %>%
  group_by(county_fips) %>%
  group_split()
# apply var estimation to grouped list
county_vars <-
  county_list %>%
  map(estim_var)

# save
save(county_vars, file = "var_timeseries_results.RData")

# load
#load("var_timeseries_results.RData")

# IRFS -----
# get irf data
irfs <- list()
for(i in seq_along(county_vars)){
  # get county info
  info <- 
    county_vars[[i]]$original_data[1,1:3] %>%
    mutate(avg_irf = 
             county_vars[[i]]$impulse_response_function$average_mobility_response)
  # add to list
  irfs[[i]] <- info
}
# rbind to get irfs
irfs <- do.call(rbind, irfs)
# load dataset with county info
counties <- read_rds("county_covid_survival_basic.rds")
# match county info to irfs
irfs <-
  irfs %>%
  left_join(counties, by = c("county_fips"))

# plot irf densities by election winner
irfs %>%
  filter(!is.na(elect_winner)) %>%
  rename("Election Winner" = elect_winner) %>%
  ggplot(aes(x = avg_irf, group = `Election Winner`, linetype = `Election Winner`)) +
  geom_density() +
  labs(x = "Average Mobility Response, County", y = "Density",
       title = "Shock Response: Average Mobility Responses to Simulated Increase in COVID-19 Cases",
       subtitle = "County averages are calculated over a 14-day horizon") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))
# t-test of whether the distributions have different means
t.test(x = irfs$avg_irf[irfs$elect_winner == "Republican"],
       y = irfs$avg_irf[irfs$elect_winner == "Democrat"])

# plot scatterplot of average response and size of republican share
irfs %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = avg_irf, col = elect_winner)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Fraction of votes for Trump, county", y = "Average mobility response, county",
       title = "Shock Response: Average Mobility Response to an Increase in COVID-19 Cases",
       subtitle = "Mobility averages are calculated over a 14-day horizon") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# Example VARs -----

# extract hennepin county - have to figure out which it is
mn_hennep <- NA
for(i in seq_along(county_vars)){
  if(county_vars[[i]]$original_data$sub_region_1[1] == "Minnesota" &
     county_vars[[i]]$original_data$sub_region_2[1] == "Hennepin County"){
    print(i)
    mn_hennep <- i
  }
}
# get hennepin data from list
mn <- county_vars[[mn_hennep]]
# plot VAR series without fitted
mn$fitted_values %>%
  pivot_longer(cols = c(cases, mobility, fitted_cases, fitted_mobility),
               names_to = "series", values_to = "nums") %>%
  ggplot(aes(x = date, y = nums, group = series, col = series, linetype = series)) +
  geom_line() +
  labs(x = "Date", y = "Cases per 100,000/Mobility score",
       title = "VAR System for Case Rate and Mobility, Results",
       subtitle = "Hennepin County, Minnesota") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))
# plot impulse response
mnirfs <- 
  tibble("pred_mob" = mn$impulse_response_function$imp_resp$irf$cases,
         "lowerci" = mn$impulse_response_function$imp_resp$Lower$cases,
         "upperci" = mn$impulse_response_function$imp_resp$Upper$cases,
         "horizon" = seq(1, 15, 1))
mnirfs %>%
  ggplot() +
  geom_line(aes(x = horizon, y = pred_mob)) +
  geom_ribbon(aes(x = horizon, ymin = lowerci, ymax = upperci), alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Forecast, n-steps ahead", y = "Mobility score",
       title = "Mobility Response for Hennepin County, Minnesota",
       subtitle = "Following a 1sd impulse shock to the county case rate") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# plot error rates
mn$fitted_values %>%
  pivot_longer(cols = c(resid_cases, resid_mob),
               names_to = "series", values_to = "nums") %>%
  ggplot(aes(x = date, y = nums, group = series, col = series, linetype = series)) +
  geom_line() +
  labs(x = "Date", y = "Residual Error",
       title = "VAR System Errors, Results",
       subtitle = "Hennepin County, Minnesota") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))













