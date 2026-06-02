##########################################################
## forest plot for study-specific calibration intercept ##
##########################################################

rm(list = ls())
library(tidyverse)
library(metafor)
library(mice)

# prepare metadata
df <- readRDS("data/ads_impute.rds")
df <- df %>%
  dplyr::select(
    STUDYID, OUT_DC_RELAPSE
  ) %>%
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
    )
  ) %>%
  group_by(STUDYID) %>%
  summarise(
    total = n(),
    relapse = sum(OUT_DC_RELAPSE),
    study_name = first(STUDY_NAME)
  ) %>%
  ungroup() %>%
  relocate(STUDYID, study_name, total, relapse)

#######################################
# calculate intercepts for each study #
#######################################

source("R/evalModel/predictAverageRandomIntercept.r")
# source("R/modelSummary/calcOpt.r")
# prog$varSelectRR$result$pooledModel

origModel <- readRDS("results-share/originalModel_WITH_PD_JAN2026.rds")

model <- list()
dataset <- list()
lp <- list()

# load datasets and linear predictors (using study-average intercept)
for (i in 1:origModel$mice$result_grp$m) {
  dataset[[i]] <- mice::complete(origModel$mice$result_grp, i)
  lp[[i]] <- predictAverageRandomIntercept(data = dataset[[i]], model = origModel$varSelectRR$result$pooledModel)
  dataset[[i]][["lp"]] <- lp[[i]]
}

estimates <- array(numeric(0), dim = c(origModel$mice$result_grp$m, length(unique((dataset[[1]]$STUDYID)))))
ses <- array(numeric(0), dim = c(origModel$mice$result_grp$m, length(unique((dataset[[1]]$STUDYID)))))

for (i in 1:origModel$mice$result_grp$m) {
  model[[i]] <- list()
  for (j in seq_along(unique((dataset[[1]]$STUDYID)))) {
    cat("\ni (Imp) is ", i, "\n")
    cat("j (Study) is ", j, "\n")
    options(warn = 1)
    model[[i]][[j]] <- glm(OUT_DC_RELAPSE ~ offset(lp), data = dataset[[i]] %>% filter(STUDYID == j), family = binomial)
    estimates[i, j] <- summary(model[[i]][[j]])$coefficients[["(Intercept)", "Estimate"]]
    ses[i, j] <- summary(model[[i]][[j]])$coefficients[["(Intercept)", "Std. Error"]]
  }
}

# this function calculates the pooled estimates & standard errors using Rubin's rules
rr_extract <- function(est, se, sid) {
  estimates <- est[, sid]
  ses <- se[, sid]
  M <- length(estimates)
  U_bar <- mean(ses^2)
  Q_bar <- mean(estimates)
  B <- var(estimates)
  T_var <- U_bar + (1 + 1 / M) * B
  T_se <- sqrt(T_var)
  c(Q_bar, T_se) # return value
}

# construct data frame with the estimates and standard errors (excluding the 3 imputations for study 1 that failed to converge)
fp <- data.frame()
# fp[1, c(1, 2)] <- c(rr_extract(est = estimates[-c(11, 17, 20), ], se = ses[-c(11, 17, 20), ], sid = 1)) # study 1
for (i in 1:19) {
  fp[i, c(1, 2)] <- c(rr_extract(est = estimates, se = ses, sid = i)) # study 1
}
fp <- fp %>% mutate(STUDYID = row_number())

df <- df %>%
  left_join(fp, by = "STUDYID")
df <- df %>% rename(int_estimate = V1, int_se = V2)

# create forest plot
res_int <- rma.uni(
  yi = int_estimate,
  sei = int_se,
  data = df %>% filter(STUDYID != 6), # zero events, standard error very inflated
  method = "REML",
  test = "knha",
  slab = study_name
)

saveRDS(res_int, "data/forestCalIntM1.rds")
