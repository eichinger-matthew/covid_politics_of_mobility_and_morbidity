# survival models
rm(list = ls())

library(tidyverse)
library(ggsurvfit)
library(survival)
library(MatchIt)
library(survival)
library(survminer)
library(broom)
library(marginaleffects)

# set plotting rules
q <- theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1),
        plot.margin = margin(r = 50, l = 20, b = 10, t = 10))
  

# load match data
load("treatment_matching_results.RData")
# load surv data (forgot to include end day of survival, need for censoring)
sdf <- read_rds("C:/Users/eichi/Desktop/github_projects/covid_politics_of_mobility_and_morbidity/survival_data.rds")
sdf <- select(sdf, fips, t0, tE, surv_time, right_censor, entry_indic)
# assign df
df <- match_res$glm[[5]]$adjusted_data %>% left_join(sdf, c('fips', 'surv_time'))
# adjust indicator for death, currently baseline is "alive"
df <- mutate(df, dem_treat = ifelse(elect_winner == "Democrat", 1, 0),
             dead = ifelse(right_censor == 0, 1, 0))
# cox model ------

# estimate model
fit <- coxph(Surv(time = surv_time, event = dead) ~ 
               dem_treat, data = df, weights = weights,
             ties = 'breslow', robust = TRUE)
summary(fit)
# schoenfeld test for proportionality
prop.test <- cox.zph(fit)
# plot test
ggcoxzph(prop.test)
# plot kaplan-meier curve for 5th caliper
plot(survfit(fit, newdata = data.frame("dem_treat" = c(0,1)), se.fit = TRUE))
# plot using ggsurvfit
ggsurvplot(fit, data = df)
survfit2(Surv(surv_time, dead) ~ 
           dem_treat, data = df, weights = weights) %>%
  tidy_survfit() %>%
  ggplot(aes(x = time, y = estimate, linetype = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x = "Time", y = "Survival probability",
       title = "Survival Curves for Treatment (Democratic) \nand Control (Republican) Groups") +
  scale_linetype_manual(name = "Democratic treatment", values = c(1, 2)) +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# use example above in a loop
coefs <- list()
for(i in seq_along(match_res$glm)){
  # load adjusted data
  d <- match_res$glm[[i]]$adjusted_data %>% left_join(sdf, by = c('fips', 'surv_time'))
  # add dem treat and death indicator
  d <- mutate(d, dem_treat = ifelse(elect_winner == "Democrat", 1, 0),
              dead = ifelse(right_censor == 0, 1, 0))
  # estimate model
  fit <- coxph(Surv(time = surv_time, event = dead) ~ 
                 dem_treat, data = d, weights = weights,
               ties = 'breslow', robust = TRUE)
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
  labs(x = "Caliper value", y = "HR estimate",
       title = "Hazard Ratio Estimates for Democratic Treatment",
       subtitle = "By caliper value used in matching procedure") +
  q



