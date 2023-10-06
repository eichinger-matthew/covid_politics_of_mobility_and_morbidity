# survival models
library(tidyverse)
library(ggsurvfit)
library(survival)
library(MatchIt)
library(survival)
library(broom)
library(marginaleffects)

# load
load("treatment_matching_results.RData")

# get dataframe from fifth matching model
df <- match_res$glm[[5]]$adjusted_data
# add indicator for whether a county is dead at end period
df <- 
  df %>%
  mutate(dem_treat = ifelse(elect_winner == "Democrat", 1, 0),
         dead = 1)

# estimate cox-ph model
fit <- coxph(Surv(surv_time, dead) ~ 
               dem_treat, data = df, weights = weights)
# get summary
summary(fit)
# plot kaplan-meier curve
plot(survfit(fit, newdata = data.frame("dem_treat" = c(0,1)), se.fit = TRUE))
# plot curve using ggsurvfit
survfit2(Surv(surv_time, dead) ~ 
           dem_treat, data = df, weights = weights) %>%
  tidy_survfit() %>%
  ggplot(aes(x = time, y = estimate, linetype = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x = "Time", y = "Survival probability",
       title = "Survival Curves for Treatment (Democratic) \nand Control (Republican) Groups") +
  scale_linetype_manual(name = "Democratic", values = c(1, 2)) +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# sensitivity analysis ------------------------------------

# ate results could depend on caliper hyperparameter in matching
# estimate cox-ph mod for each caliper dataframe and plot results 

# container
coefs <- list()
# loop over matching datasets
for(i in seq_along(match_res$glm)){
  d <- match_res$glm[[i]]$adjusted_data
  d <- mutate(d, dem_treat = ifelse(elect_winner == "Democrat", 1, 0),
              dead = 1)
  # estimate model
  fit <- coxph(Surv(surv_time, dead) ~ dem_treat, data = d, weights = weights)
  # assign to tibble
  cfs <- tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  # get caliper
  calip <- match_res$glm[[i]]$matching_model$caliper[[1]]
  cfs <- mutate(cfs, caliper = calip)
  coefs[[i]] <- cfs
  # what to do at end
  if(i == 10){
    fin <- do.call(rbind, coefs)
  }
}

# plot how dem ate varies as f'n of caliper hyperparameter
fin %>%
  ggplot(aes(x = calips, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Caliper constraint value", y = "HR estimate (Democratic)",
       title = "Hazard Ratios for the Discrete Effect of Democratic Status",
       subtitle = "By caliper value used in matching procedure") +
  theme_minimal() +
  theme(axis.line = element_line())




