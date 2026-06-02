###########################
# SAMPLE SIZE CALCULATION #
###########################

# This R script uses methodology developed by Riley et al (2018) for sample size calculation in
# multivariable prediction models with binary outcomes

# Riley RD, Snell KI, Ensor J, Burke DL, Harrell FE Jr, Moons KG, Collins GS. Minimum sample size for
# developing a multivariable prediction model: PART II - binary and time-to-event outcomes.
# Stat Med. 2019 Mar 30;38(7):1276-1296. doi: 10.1002/sim.7992. Epub 2018 Oct 24.
# Erratum in: Stat Med. 2019 Dec 30;38(30):5672. doi: 10.1002/sim.8409. PMID: 30357870; PMCID: PMC6519266.

# The package pmsampsze, developed by Joie Ensor, is an implementation of this methodology.
# https://CRAN.R-project.org/package=pmsampsize

library(tidyverse)
library(pmsampsize)

# There are no previous prediction models for VL relapse
# Therefore, our choice of Cox-Snell R squared value is based on assuming
# R2 Nagelkerke of 0.15 as suggested by Riley et al.

# "
# Further research is needed to help researchers when there are no existing studies or information to identify a sensible
# value of the expected Cox-Snell R2. Medical diagnosis and prediction of health-related outcomes are, generally speaking,
# low signal-to-noise ratio situations. It is not uncommon in these situations to see R2 Nagelkerke values in the 0.1 to 0.2 range.

# Therefore, in the absence of any other information, we suggest that sample sizes be derived assuming the value of R2
# CS_adjR2 corresponds to an R2 Nagelkerke of 0.15. An exception is when predictors include “direct” (mechanistic)
# measurements, such as including the baseline version of the binary or ordinal outcome (eg, including smoking status at
# baseline when predicting smoking status at 1 year), or direct measures of the processes involved (eg, including physiologic
# function of patients in intensive care when predicting risk of death within 48 hours). Then, in this special situation,
# an R2 = 0.5 may be a more appropriate default choice.
# "

#######################
# Indian subcontinent #
#######################

# 4,599 participants
# 228 relapses
# maximum number of predictor parameters = 25

pmsampsize(
  type = "b",
  nagrsquared = 0.15,
  parameters = 59,
  prevalence = 228 / 4599, # (4.9%)
  seed = 123456,
  shrinkage = 0.80
)

###############
# East Africa #
###############

# 2,051 participants
# 99 relapses
# maximum number of predictor parameters = 25

pmsampsize(
  type = "b",
  nagrsquared = 0.15,
  parameters = 11,
  prevalence = 99 / 2051, # (4.9%)
  seed = 123456,
  shrinkage = 0.90
)
