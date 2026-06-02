# this function extracts the calibration slope optimism and the adjusted (study-average) intercept

rm(list = ls())
library(tidyverse)
library(lme4)
library(mice)

source("R/evalModel/predictAverageRandomIntercept.r")
source("R/modelSummary/optimismParam.r")

# for testing
origModel1 <- readRDS("results-share/20250701_valOrig_withPD/originalModel_1.rds")
sumModel1 <- readRDS("results-share/20250701_WITH_PD.rds")
op1A1 <- optimismParam(origModel = origModel1, sumModel = sumModel1, action = 1)
op1A2 <- optimismParam(origModel = origModel1, sumModel = sumModel1, action = 2)

origModel1$varSelectRR$result$pooledModel %>% summary()
# sf: 0.9281058
# int_adj1: -0.2012446
# int_adj2: -0.2804235

origModel2 <- readRDS("results-share/20250701_valOrig_withoutPD/originalModel_2.rds")
sumModel2 <- readRDS("results-share/20250701_WITHOUT_PD.rds")
op2A1 <- optimismParam(origModel = origModel2, sumModel = sumModel2, action = 1)
op2A2 <- optimismParam(origModel = origModel2, sumModel = sumModel2, action = 2)

origModel2$varSelectRR$result$pooledModel %>% summary()
# sf: 0.8712314
# int_adj1: -0.2804235
# int_adj2: -0.4888844

M1_estimate <- origModel1$varSelectRR$result$pooledModel %>%
  summary() %>%
  select(term, estimate) %>%
  rename(M1_estimate = estimate) %>%
  mutate(M1_estimate_adj_sa = M1_estimate * op1A1[[1]] + ifelse(term == "(Intercept)", op1A1[[2]], 0)) %>%
  mutate(M1_estimate_adj_su = M1_estimate * op1A1[[1]] + ifelse(term == "(Intercept)", op1A2[[2]], 0))

M2_estimate <- origModel2$varSelectRR$result$pooledModel %>%
  summary() %>%
  select(term, estimate) %>%
  rename(M2_estimate = estimate) %>%
  mutate(M2_estimate_adj_sa = M2_estimate * op2A1[[1]] + ifelse(term == "(Intercept)", op2A1[[2]], 0)) %>%
  mutate(M2_estimate_adj_su = M2_estimate * op2A1[[1]] + ifelse(term == "(Intercept)", op2A2[[2]], 0))

coeff <- M1_estimate %>% full_join(M2_estimate)
coeff <- coeff %>%
  mutate(across(-term, ~ sprintf("%.5f", .x))) %>%
  mutate(across(-term, ~ ifelse(.x == "NA", "", .x)))
coeff %>% glimpse()
coeff %>% xtable()

origModel1$varSelectRR$result$tau_squared %>% mean()
origModel1$varSelectRR$result$icc %>% mean()

origModel2$varSelectRR$result$tau_squared %>% mean()
origModel2$varSelectRR$result$icc %>% mean()
