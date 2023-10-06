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

# assign df
df <- match_res$glm[[8]]$adjusted_data
df <- mutate(df, dem_treat = ifelse(elect_winner == "Democrat", 1, 0))
# estimate model
fit <- coxph(Surv(survtime50_3consc, dead50_3consc) ~ 
               dem_treat, data = df, weights = weights)
summary(fit)
# plot kaplan-meier curve for 8th caliper
plot(survfit(fit, newdata = data.frame("dem_treat" = c(0,1)), se.fit = TRUE))
# plot using ggsurvfit
ggsurvplot(fit, data = df)
survfit2(Surv(survtime50_3consc, dead50_3consc) ~ 
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

# use example above in a loop
coefs <- list()
for(i in seq_along(match_res$glm)){
  d <- match_res$glm[[i]]$adjusted_data
  d <- mutate(d, dem_treat = ifelse(elect_winner == "Democrat", 1, 0))
  # estimate model
  fit <- coxph(Surv(survtime50_3consc) ~ dem_treat, data = d, weights = weights)
  # assign to tibble
  coefs[[i]] <- tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  # what to do at end
  if(i == 10){
    fin <- do.call(rbind, coefs)
  }
}
# get calipers
calips <- vector(mode = "numeric")
for(i in seq_along(match_res$glm)){
  # get caliper
  calips[i] <- match_res$glm[[i]]$matching_model$caliper[[1]]
}
# add to final coef info
fin$calips <- calips

# plot coef plot
fin %>%
  ggplot(aes(x = calips, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Caliper constraint value", y = "HR estimate (Democratic)",
       title = "Hazard Ratios for the Discrete Effect of Democratic Status",
       subtitle = "By caliper value used in matching procedure") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))









