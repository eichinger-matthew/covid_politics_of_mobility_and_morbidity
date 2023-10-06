# matching model for covid paper
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
library(MatchIt)
library(dagitty)
library(ggdag)
library(DiagrammeR)
library(kableExtra)

# set directory
setwd("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity")

# load data
svl <- read_rds("survival_data.rds")




# causal graph for admissible set --------------------------------

# define graph using ggdag for easy use
d <- dagify(
  "part" ~ "rural" + "income" + "age" + "sociodems",
  "adapt" ~ "income",
  "media" ~ "part",
  "risks" ~ "media" + "age",
  "mobility" ~ "age" + "partisanship" + "risks" + "adapt",
  "covid" ~ "mobility",
  "mobility" ~ "covid",
  labels = c("part" = "Partisanship",
             "rural" = "Rural",
             "income" = "Income",
             "age" = "Age",
             "adapt" = "Able \n to \n adapt",
             "sociodems" = "Sociodemographics",
             "risks" = "Risk \n beliefs",
             "media" = "Media",
             "mobility" = "Mobility",
             "covid" = "COVID \n rates"),
  exposure = "part", outcome = "mobility")
# plot example
plot(graphLayout(d))
# do for diagrammer, actual graph in paper
d.diagrammer <- DiagrammeR::grViz("
digraph {
  graph []
  node [shape = plaintext]
    part [label = 'Partisanship', shape = oval]
    rural [label = 'Rural', shape = box]
    income [label = 'Income', shape = box]
    adapt [label = 'Able to adapt']
    age [label = 'Age', shape = box]
    sociodems [label = 'Sociodems', shape = box]
    risks [label = 'Risk beliefs']
    media [label = 'Media']
    mobility [label = 'Mobility', shape = oval]
    covid [label = 'Covid rates']
  edge []
    rural -> part
    income -> part
    age -> part
    sociodems -> part
    income -> adapt
    part -> media
    media -> risks
    age -> risks
    adapt -> mobility
    age -> mobility
    part -> mobility
    risks -> mobility
    covid -> mobility
    mobility -> covid
  {rank = same; rural; income; age; sociodems}
}
")
d.diagrammer



# matching for selection adjustment weights -------------------------------

# use causal graph to identify admissible set of adjusters
adjustmentSets(d, "part", "mobility", type = "all")
# one of the many admissible set includes:
# age, income, rural, industry, education

# get matching df
setup_df <-
  svl %>%
  select(county_fips, surv_time, elect_winner, 
         total_population_2018:ruralurban_cc_2018) %>%
  drop_na() %>%
  mutate(elect_winner_num = ifelse(elect_winner == "Republican", 1, 0))

# make storage object for loop
match_res <- list()
# make storage object for balance calculations
binfo <- list()
# define caliper hyperparameters to loop over
calips <- seq(0.001, 0.01, 0.001)
# start loop
for(i in seq_along(calips)){
  # estimate propensity treatment selection model
  # use 2-to-1 matching with nearest neighbor matching
  m.glm <- 
    matchit(elect_winner_num ~ 
              white_pct_2018 + black_pct_2018 +
              foreignborn_pct_2018 + age65andolder_pct_2018 + 
              median_hh_inc_2018 + lesscollege_pct_2018 + rural_pct_2018, 
            data = setup_df, method = "nearest", distance = "glm",
            ratio = 2, caliper = calips[i])
  # get adjustment weights from model
  df.glm <- 
    match.data(m.glm) %>%
    arrange(subclass)
  # produce love plot for visual diagnostics
  love_plot <- 
    love.plot(m.glm, stats = c("mean", "var"),
              thresholds = c(cor = .1), abs = TRUE, wrap = 20,
              limits = list(ks = c(0, .5)), 
              var.order = "unadjusted", line = TRUE)
  # produce balance table
  b <- bal.tab(m.glm, un = TRUE, stats = c("mean.diffs", "variance.ratios"), which.treat = .all)
  # make dataset of balance info for distance and each variable (mean, var), given caliper
  btab <- 
    b$Balance %>%
    mutate(vname = row.names(.),
           caliper = calips[i]) %>%
    rename(unadj_meandiff = Diff.Un,
           unadj_vratio = V.Ratio.Un,
           adj_meandiff = Diff.Adj,
           adj_vratio = V.Ratio.Adj)
  # add to easy list
  binfo[[i]] <- btab
    
  # balance plots
  var_names <- colnames(m.glm$X)
  balance_plots <- lapply(var_names, bal.plot, x = m.glm, which = "both")
  
  # storing
  # model
  match_res[["glm"]][[paste0("caliper", sep = "_", i)]][["matching_model"]] <- m.glm
  # store adjusted data
  match_res[["glm"]][[paste0("caliper", sep = "_", i)]][["adjusted_data"]] <- df.glm
  # store love plot
  match_res[["glm"]][[paste0("caliper", sep = "_", i)]][["love_plot"]] <- love_plot
  # store balance plot
  match_res[["glm"]][[paste0("caliper", sep = "_", i)]][["balance_plots"]] <- balance_plots
  # tables
  match_res[["glm"]][[paste0("caliper", sep = "_", i)]][["balance_tables"]] <- btab
}

# save object
# save(match_res, file = "treatment_matching_results.RData")


# matching model visualizations ---------------------------------- 

# get balance info
balinfo <- do.call(rbind, binfo)
# plot adj mean diff and adj var ratio for each variable
balinfo %>%
  pivot_longer(cols = c(adj_meandiff, adj_vratio), names_to = "statistic", values_to = "nums") %>%
  ggplot(aes(x = caliper, y = abs(nums), col = statistic)) +
  geom_line() +
  labs(x = "Caliper value", y = "Statistic value",
       title = "Matching Quality at Different Caliper Values",
       subtitle = "Judged by Mean Difference and Variance Ratio") +
  facet_wrap(~vname) +
  scale_color_manual(name = "Series", values = c("orange", "purple")) +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))



# make balance table
d <- match_res$glm$caliper_3$balance_tables
d <- 
  d %>%
  rename(Variable = vname, "Unadj.Diff" = unadj_meandiff, "Adj.Diff" = adj_meandiff,
         "Unadj.Ratio" = unadj_vratio, "Adj.Ratio" = adj_vratio) %>%
  select(Variable, Unadj.Diff, Adj.Diff, Unadj.Ratio, Adj.Ratio, -c(Type, caliper)) %>%
  remove_rownames() %>%
  mutate(Variable = 
           case_when(
             Variable == "distance" ~ "Distance",
             Variable == "white_pct_2018" ~ "Pct White",
             Variable == "black_pct_2018" ~ "Pct Black",
             Variable == "foreignborn_pct_2018" ~ "Pct Foreign-born",
             Variable == "age65andolder_pct_2018" ~ "Pct Age > 65",
             Variable == "median_hh_inc_2018" ~ "Median HH Income",
             Variable == "lesscollege_pct_2018" ~ "Pct Edu. < College",
             Variable == "rural_pct_2018" ~ "Pct Rural"
           ),
         across(where(is.numeric), ~round(.x, 2)))
# save
saveRDS(d, file = "mtab.R")
q <- 
  d %>%
  kable(format = "latex")


