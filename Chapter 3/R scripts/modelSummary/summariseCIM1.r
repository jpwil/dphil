##################
## SUMMARISE CI ##
##################

rm(list = ls())
library(tidyverse)
library(metafor)
library(mice)
library(scales)
library(ggtext)

prog <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")

# fixed effect meta-analysis: equal weights
weight_equal <- prog$data %>%
  count(STUDYID) %>%
  pull(n)

# fixed effect meta-analysis: pair weights
dataset <- prog$data %>%
  select(STUDYID, OUT_DC_RELAPSE) %>%
  mutate(OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE))
dataset_split <- split(dataset, dataset$STUDYID)

weight_pairs <- numeric()
for (j in seq_along(dataset_split)) { # loop over clusters
  outcome <- dataset_split[[j]]$OUT_DC_RELAPSE
  weight_pairs[j] <- sum(outcome) * sum(!outcome)
}

# yi and vi are the same for all rma.uni outputs
yi <- prog$evalCI$result$evalCI_AP_RE$yi
vi <- prog$evalCI$result$evalCI_AP_RE$vi

# this step is required for ISC
est <- c(yi[1:5], NA, yi[6:length(yi)])
var <- c(vi[1:5], NA, vi[6:length(vi)])

summary <- prog$data %>%
  group_by(STUDYID) %>%
  summarise(num = n(), events = sum(OUT_DC_RELAPSE)) %>%
  ungroup() %>%
  mutate(
    STUDY_NAME = case_when(
      STUDYID == 1 ~ "Rijal 2010(A)",
      STUDYID == 2 ~ "Rijal 2003",
      STUDYID == 3 ~ "Sundar 2008(A)",
      STUDYID == 4 ~ "Sundar 2009",
      STUDYID == 5 ~ "Koirala 2003",
      STUDYID == 6 ~ "Chakraborty 2008",
      STUDYID == 7 ~ "Rijal 2010(B)",
      STUDYID == 8 ~ "Sundar 2014",
      STUDYID == 9 ~ "Bhattacharya 2007",
      STUDYID == 10 ~ "Sundar 2010",
      STUDYID == 11 ~ "Pandey 2017",
      STUDYID == 12 ~ "Das 2009",
      STUDYID == 13 ~ "Pandey 2016",
      STUDYID == 14 ~ "Sundar 2008(B)",
      STUDYID == 15 ~ "Sundar 2015",
      STUDYID == 16 ~ "Sundar 2019",
      STUDYID == 17 ~ "Sundar 2011",
      STUDYID == 18 ~ "Sundar 2007",
      STUDYID == 19 ~ "Sundar 2012"
    ),
    weight_pairs = weight_pairs,
    est = est,
    var = var,
    est_logit = qlogis(est),
    var_logit = ifelse(is.na(var), NA_real_, var / (est^2 * (1 - est)^2)),
    ci_ll = plogis(est_logit - 1.96 * sqrt(var_logit)),
    ci_ul = plogis(est_logit + 1.96 * sqrt(var_logit))
  )

##################################
## explore summary C-statistics ##
##################################

## FIRST: let's perform random-effects meta-analysis (on both CI and probability scales)

re_prob <- rma.uni(yi = summary$est, vi = summary$var, method = "REML", test = "knha")
re_logit <- rma.uni(yi = summary$est_logit, vi = summary$var_logit, method = "REML", test = "knha")

re_prob_pred <- predict(re_prob)
re_logit_pred <- predict(re_logit, transf = transf.ilogit)

# estimates are similar when using both scales
re_prob_pred
re_logit_pred

# let's limit to > 5 events
summary_limited <- summary %>% filter(events > 5)
summary_limited

re_prob <- rma.uni(yi = summary_limited$est, vi = summary_limited$var, method = "REML", test = "knha")
re_logit <- rma.uni(yi = summary_limited$est_logit, vi = summary_limited$var_logit, method = "REML", test = "knha")

re_prob_pred <- predict(re_prob)
re_logit_pred <- predict(re_logit, transf = transf.ilogit)

# estimates are similar when using both scales
re_prob_pred
re_logit_pred

# ## SECOND: let's perform fixed-effects meta-analysis with pair weightings

fe_prob <- rma.uni(yi = summary$est, vi = summary$var, method = "FE", weights = summary$weight_pairs)
fe_pred <- predict(fe_prob)
fe_pred

# limit to > 5 events
fe_prob <- rma.uni(yi = summary_limited$est, vi = summary_limited$var, method = "FE", weights = summary_limited$weight_pairs)
fe_pred <- predict(fe_prob)
fe_pred

## LAST: create output for building forest plots
saveRDS(file = "results/summaryCIM1_withPD_ma.rds", list(re = re_prob_pred, fe = fe_pred))
saveRDS(file = "results/summaryCIM1_withPD.rds", summary)
