# clean google mobility data
# downloaded on feb 14, 2022

# libraries
library(tidyverse)
library(janitor)
library(tigris)


# loading data ------

setwd("C:/Users/eichi/Downloads")

# get mobility data from https://www.google.com/covid19/mobility/
file_wants <- 
  grep('_US_Region', unzip('Region_Mobility_Report_CSVs.zip', list=TRUE)$Name, 
       ignore.case=TRUE, value=TRUE)
# unzip only those files
unzip('Region_Mobility_Report_CSVs.zip', files = file_wants)
# load data files
for(i in seq_along(file_wants)){
  if(i == 1){
    mob2020 <- read_csv(file_wants[i])
  }else if(i == 2){
    mob2021 <- read_csv(file_wants[i])
  }else{
    mob2022 <- read_csv(file_wants[i])
  }
}

# get 2020 covid data
county_covid_2020 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2020.csv")
# Get 2021 county covid data
county_covid_2021 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2021.csv")
# Get 2022 county covid data
county_covid_2022 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2022.csv")

# county election returns from mit
creturns <- read_csv("countypres_2000-2020.csv") %>% filter(year == 2016)
# county sociodems from mit
cchars <- 
  read_csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv") %>%
  mutate(fips = ifelse(str_length(fips) == 4,
                       str_pad(fips, width = 5, side = "left", pad = "0"), fips)) %>%
  rename_with(.cols = total_population:ruralurban_cc,
              .fn = ~paste(.x, "2018", sep = "_"))


# merging and cleaning --------

# join county sociodems with election results
cfull <-
  creturns %>%
  filter(mode == "TOTAL") %>%
  select(county_fips, party, candidatevotes, totalvotes) %>%
  filter(party == "REPUBLICAN" | party == "DEMOCRAT") %>%
  drop_na() %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  mutate(rep_frac = REPUBLICAN/totalvotes,
         dem_frac = DEMOCRAT/totalvotes,
         rep_margin = rep_frac-dem_frac) %>%
  arrange(desc(rep_margin)) %>%
  left_join(cchars, by = c("county_fips" = "fips")) %>%
  mutate(county_fips = 
           ifelse(str_length(county_fips) == 4, 
                  str_pad(county_fips, 4, "left", "0"), county_fips)) %>%
  clean_names()
# save
# saveRDS(cfull, file = 'county_sociodems_and_elect_returns.rds')

# row-bind mobility data
mob <- 
  rbind(mob2020, mob2021, mob2022) %>%
  rename(fips = census_fips_code) %>%
  select(fips, sub_region_1, sub_region_2, date:residential_percent_change_from_baseline)
# get plot of num obs. for counties
mob %>%
  count(fips) %>%
  filter(!is.na(fips)) %>%
  ggplot(aes(x = n)) +
  geom_histogram(fill = 'steelblue') +
  theme_minimal()

# bind daily covid data
covid <- 
  rbind(county_covid_2020, county_covid_2021, county_covid_2022) %>%
  mutate(fips = str_remove(geoid, "USA-"))
# join to daily mobility
daily_measures <-
  covid %>%
  left_join(small_mob, by = c('date', 'fips'))
saveRDS(daily_measures, file = "county_covid_rates_and_mobility.rds")









