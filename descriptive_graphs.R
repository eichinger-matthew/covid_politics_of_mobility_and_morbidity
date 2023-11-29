# descriptive graphs for showing difference between dem and rep counties
rm(list = ls())

# load libraries
library(tidyverse)
library(broom)            # for tidy results
library(MatchThem)          # matching
library(optmatch)         # for optimal matching
library(cobalt)           # love plots
library(survival)         # survival models
library(wesanderson)
library(gghighlight)
library(kableExtra)
library(ggsurvfit)
library(ggpubr)
library(sf)
library(usmap)

# set graph shortcut
q <- 
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1),
        plot.margin = margin(r = 50, l = 20, b = 10, t = 10))

# load data
setwd("C:/Users/eichi/Desktop/covid_mobility_and_morbidity")
covid <- read_rds("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity/survival_data.rds")
anes <- read_csv("C:/Users/eichi/Downloads/anes_social_media/anes_specialstudy_2020_socialmedia_csv_20211118.csv")

# anes graphs --------------

# small version
anes_small <- 
  anes %>% 
  select(trustfox, w2trustfox, trustvote, 
         con_cdc, con_elect, lcself,
         covid_worry, covid_know) %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(.x < 0, NA, .x)),
         across(.cols = everything(), .fns = ~ordered(.x)))
# trust fox and covid worry
fox_covid_worry <-
  anes_small %>%
  filter(!is.na(trustfox), !is.na(covid_worry)) %>%
  ggplot(aes(y = reorder(trustfox, desc(trustfox)))) +
  geom_bar(aes(fill = covid_worry), position = position_fill(reverse = TRUE)) +
  labs(x = "Proportion", y = "Trust in Fox News",
       title = "Trust in Fox News and Worries About COVID-19",
       subtitle = "Higher values indicate more trust and stronger worries") +
  scale_fill_viridis_d(name = "Worried") +
  q

# worry about covid and conservatism
cons_worry <-
  anes_small %>%
  filter(!is.na(lcself), !is.na(covid_worry)) %>%
  ggplot(aes(y = reorder(lcself, desc(lcself)))) +
  geom_bar(aes(fill = covid_worry), position = position_fill(reverse = TRUE)) +
  labs(x = "Proportion", y = "Left-right scale",
       title = "Political Ideology and Worries About COVID-19",
       subtitle = "Higher values indicate higher conservatism and stronger worries") +
  q
# trust in fox news and conservatism
ideo_fox <-
  anes_small %>%
  filter(!is.na(trustfox), !is.na(lcself)) %>%
  ggplot(aes(y = lcself))+
  geom_bar(aes(fill = trustfox), position = position_fill(reverse = TRUE)) +
  scale_fill_viridis_d(name = "Trust in Fox") +
  labs(x = "Proportion", y = "Left-right scale",
       title = "Political Ideology and Worries About COVID-19",
       subtitle = "Higher values indicate higher conservatism and stronger worries") +
  q

# plot together
ggarrange(fox_covid_worry, ideo_fox, ncol = 1)

  

# show distribution of variables by group -----

# survival times, by election winner
covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = surv_time, group = elect_winner, fill = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "Survival Time (after day of infection)", 
       y = "Density", title = "County Survival Times, by Election Winner",
       subtitle = "Start time as day when cumulative cases > 100 per 100,000") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1),
        plot.margin = margin(r = 50, l = 20, b = 40, t = 20))


# log population
pop <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = log(total_population_2018), group = elect_winner,
             fill = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "(log) Population", y = "Density", title = "Population") +
  q

# income
inc <- 
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = median_hh_inc_2018/10000, fill = elect_winner, group = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "Median HH income", y = "Density",
       title = "Income") +
  q

# foreign born
fborn <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = foreignborn_pct_2018/100, fill = elect_winner, group = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "%Foreign-born", y = "Density",
       title = "Foreign-born") +
  q

# rural area
rural <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rural_pct_2018, group = elect_winner, fill = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "%Area rural", y = "Density",
       title = "Rural") +
  q

# college educated
college <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = lesscollege_pct_2018, fill = elect_winner, gruoup = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "%Less than college", y = "Density",
       title = "College") +
  q

# covid rates
# crates <-
#   covid %>%
#   filter(!is.na(elect_winner)) %>%
#   ggplot(aes(x = surv_time, fill = elect_winner, group = elect_winner)) +
#   geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
#                  aes(y = after_stat(density))) +
#   scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
#   labs(x = "Time-to-Baseline Mobility (days)", y = "Count",
#        title = "COVID-19") +
#   q

# old age
old <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = age65andolder_pct_2018, fill = elect_winner, group = elect_winner)) +
  geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "%Age > 65", y = "Density",
       title = "Older") +
  q

# young age
# young <-
#   covid %>%
#   filter(!is.na(elect_winner)) %>%
#   ggplot(aes(x = age29andunder_pct_2018, fill = elect_winner, group = elect_winner)) +
#   geom_histogram(position = 'identity', alpha = 0.5, col = 'black',
#                  aes(y = after_stat(density))) +
#   scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
#   labs(x = "%age 29 and under", y = "Density",
#        title = "Younger") +
#   q


# ggarrange plot
ggarrange(college, inc, pop, fborn, rural, old, common.legend = TRUE)


# show scatterplot relationships -----

# log population
s.pop <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = log(total_population_2018), col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "(log) Population", 
       title = "Population") +
  q

# income
s.inc <- 
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = median_hh_inc_2018/10000, col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "Median HH income", 
      title = "Income") +
  q

# foreign born
s.fborn <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = foreignborn_pct_2018/100, col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "%Foreign-born", 
       title = "Foreign-born") +
  q

# rural area
s.rural <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = rural_pct_2018, col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "%Area rural",
       title = "Rural") +
  q

# college educated
s.college <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = lesscollege_pct_2018, col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "%Less than college",
       title = "College") +
  q

# covid rates
# s.crates <-
#   covid %>%
#   filter(!is.na(elect_winner)) %>%
#   ggplot(aes(x = rep_frac, y = surv_time, col = elect_winner, group = 1)) +
#   geom_point() +
#   geom_smooth() +
#   scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
#   labs(x = "Republican vote share", y = "Survival time (days)",
#        title = "COVID-19") +
#   q

# old age
s.old <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = age65andolder_pct_2018, col = elect_winner, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share", y = "%Age > 65",
       title = "Older") +
  q

# young age
# s.young <-
#   covid %>%
#   filter(!is.na(elect_winner)) %>%
#   ggplot(aes(x = rep_frac, y = age29andunder_pct_2018, col = elect_winner, group = 1)) +
#   geom_point() +
#   geom_smooth() +
#   scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
#   labs(x = "Republican vote share", y = "%age 29 and under",
#        title = "Younger") +
#   q


# ggarrange plot
ggarrange(s.college, s.inc, s.pop, s.fborn, s.rural, s.old, common.legend = TRUE)


# matching diagnostics --------------------

# load matching results
load("treatment_matching_results.RData")

# get matching results for fifth caliper model
mres <- match_res$glm$caliper_5
# print and save balance plots
popwhite <-
  mres$balance_plots[[1]] +
  labs(x = "%Population as white", 
       title = "Distributional Balance")
popblack <-
  mres$balance_plots[[2]] +
  labs(x = "%Population as black", 
       title = "Distributional Balance")
popforeign <-
  mres$balance_plots[[3]] +
  labs(x = "%Population as foreign born", 
       title = "Distributional Balance")
popelderly <-
  mres$balance_plots[[4]] +
  labs(x = "%Population as Older than 65", 
       title = "Distributional Balance")
hhinc <-
  mres$balance_plots[[5]] +
  labs(x = "(log) Median household income", 
       title = "Distributional Balance")
college <-
  mres$balance_plots[[6]] +
  labs(x = "%Population less than college education", 
       title = "Distributional Balance")
rural <-
  mres$balance_plots[[7]] +
  labs(x = "%Area as rural", 
       title = "Distributional Balance")
# arrange
ggarrange(popwhite, popblack, popforeign, popelderly,
          hhinc, college, rural, common.legend = TRUE)






