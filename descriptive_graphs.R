# descriptive graphs for showing difference between dem and rep counties


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
library(tmap)
library(usmap)

# load data
setwd("C:/Users/eichi/Desktop/covid_mobility_and_morbidity")
covid <- read_rds("county_covid_survival_basic.rds")
# show distribution of variables by group -----

# log population
pop <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = log(total_population_2018), 
             fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "(log) Totalpopulation, 2018", y = "Density", title = "Population") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# income
inc <- covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = median_hh_inc_2018/10000, fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "Median household income (tens of thousands), 2018", y = "Density",
       title = "Income") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# foreign born
fborn <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = foreignborn_pct_2018/100, fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "%residents as foreign born, 2018", y = "Density",
       title = "Foreign-born") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# rural area
rural <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rural_pct_2018, group = elect_winner, fill = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 15, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", 
                    values = c("dodgerblue", "firebrick")) +
  labs(x = "%area as rural, 2018", y = "Density",
       title = "Rural") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# college educated
college <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = lesscollege_pct_2018, fill = elect_winner, gruoup = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "%less than college education, 2018", y = "Density",
       title = "College") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# covid rates
crates <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = survtime50_3consc, fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Time-to-Baseline Mobility (days)", y = "Density",
       title = "COVID-19") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# old age
old <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = age65andolder_pct_2018, fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "%age 65 and older", y = "Density",
       title = "Older") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# young age
young <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = age29andunder_pct_2018, fill = elect_winner, group = elect_winner)) +
  geom_histogram(alpha = 0.5, bins = 20, col = "black",
                 aes(y = after_stat(density))) +
  scale_fill_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "%age 29 and under", y = "Density",
       title = "Younger") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# ggarrange plot
ggarrange(college, inc, pop, fborn, rural, crates, old, young)


# show scatterplot relationships -----

# log population
s.pop <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = log(total_population_2018), col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "(log) Population, county", 
       title = "Population") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# income
s.inc <- 
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = median_hh_inc_2018/10000, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "Median HH income", 
      title = "Income") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# foreign born
s.fborn <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = foreignborn_pct_2018/100, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "%residents as foreign born, 2018", 
       title = "Foreign-born") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# rural area
s.rural <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = rural_pct_2018, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "%area as rural, 2018",
       title = "Rural") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# college educated
s.college <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = lesscollege_pct_2018, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "%less than college",
       title = "College") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# covid rates
s.crates <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = survtime50_3consc, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", 
       y = "Survival time (days)",
       title = "COVID-19") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# old age
s.old <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = age65andolder_pct_2018, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "%age 65 and older",
       title = "Older") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# young age
s.young <-
  covid %>%
  filter(!is.na(elect_winner)) %>%
  ggplot(aes(x = rep_frac, y = age29andunder_pct_2018, col = elect_winner, group = 1)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(name = "Election winner", values = c("dodgerblue", "firebrick")) +
  labs(x = "Republican vote share, county", y = "%age 29 and under",
       title = "Younger") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))


# ggarrange plot
ggarrange(s.college, s.inc, s.pop, s.fborn, s.rural, s.crates, s.old, s.young)


# make map of vote shares across counties -------

# filter covid data
mapdf <- 
  covid %>% 
  select(county_fips, rep_frac) %>%
  rename(fips = county_fips)

# load shapefile
shp <- 
  st_read("cb_2016_us_county_5m.shp") %>%
  st_as_sf(.) %>%
  left_join(mapdf, by = c("GEOID" = "fips"))

# plot
shp %>%
  ggplot() +
  geom_sf(aes(fill = rep_frac))




plot_usmap(regions = "counties", data = mapdf, values = "rep_frac", exclude = "AK",
           color = "darkgray") +
  scale_fill_continuous(name = "Republican vote share", low = "blue", high = "red",
                        na.value = "white")+
  labs(title = "Distribution of County Vote Shares Republican Candidate",
       subtitle = "US Presidential Election, 2016", x = "", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1),
        axis.text = element_blank())








