# function for calculating the optimism adjusted calibration and new intercept based on whether
# recalibrating model to study-average or Sundar 2019 study

optimismParam <- function(origModel, sumModel, action = 1, int_sf = TRUE) {
  source("R/evalModel/predictAverageRandomIntercept.r")
  # action = 1: study-average random intercept recalibration (original model)
  # action = 2: Sundar 2019 intercept recalibration

  # Step 1 - extract average optimism from model summary object
  required_packages <- c("tidyverse", "mice", "lme4")
  lapply(required_packages, library, character.only = TRUE)

  # library(tidyverse)
  # sumModel <- readRDS("results-share/202605_WITH_PD_BOOT.rds")
  # origModel <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")

  df <- data.frame(
    terms = character(),
    ci_fe_ap = numeric(),
    ci_fe_bt = numeric(),
    cal_slope_brms_ap = numeric(),
    cal_slope_brms_bt = numeric(),
    cal_slope_fe_ap = numeric(),
    cal_slope_fe_bt = numeric(),
    cal_int_re_ap = numeric(),
    cal_int_re_bt = numeric(),
    seed = numeric()
  )

  m <- sumModel
  safe <- function(x) if (is.null(x) || length(x) == 0) NA else x

  for (i in seq_along(m)) {
    df[[i, 1]] <- safe(m[[i]]$finalVariables$term %>% paste(collapse = ", ")) # final terms
    # df[[i, 2]] <- safe(m[[i]]$evalCI_AP)
    # df[[i, 3]] <- safe(m[[i]]$evalCI_BT)
    df[[i, 2]] <- safe(m[[i]]$evalCI_AP_FE)
    df[[i, 3]] <- safe(m[[i]]$evalCI_BT_FE)
    df[[i, 4]] <- safe(m[[i]]$evalCal_SlopeAP_BRMS)
    df[[i, 5]] <- safe(m[[i]]$evalCal_SlopeBT_BRMS)
    df[[i, 6]] <- safe(m[[i]]$evalCal_SlopeAP)
    df[[i, 7]] <- safe(m[[i]]$evalCal_SlopeBT)
    df[[i, 8]] <- safe(m[[i]]$evalCal_IntAP)
    df[[i, 9]] <- safe(m[[i]]$evalCal_IntBT)
    df[[i, 10]] <- safe(as.numeric(m[[i]]$call$seed))
  }

  opt_ci <- df %>%
    mutate(mean = ci_fe_ap - ci_fe_bt) %>%
    summarise(mean = mean(mean, na.rm = TRUE)) %>%
    pull(mean)

  opt_cal <- df %>%
    mutate(mean = cal_slope_fe_ap - cal_slope_fe_bt) %>%
    summarise(mean = mean(mean, na.rm = TRUE)) %>%
    pull(mean)

  opt_int <- df %>%
    mutate(mean = cal_int_re_ap - cal_int_re_bt) %>%
    summarise(mean = mean(mean, na.rm = TRUE)) %>%
    pull(mean)

  # Step 2 - subtract average optimism from apparent calibration slope to obtain global shrinkage factor (sf)
  sf <- origModel$evalCal$result$poolCalSlopeAP_BRMS["estimate"] - opt_cal

  # Step 3 - fit the adjusted model to the data (for each imputed dataset)
  int_adj <- numeric()
  m <- list()
  data <- list()
  lp <- list()

  if (action == 1) {
    # fit the adjusted model
    for (i in 1:origModel$mice$result$m) {
      data[[i]] <- complete(origModel$mice$result_grp, action = i)
      lp[[i]] <- predictAverageRandomIntercept(data = data[[i]], model = origModel$varSelectRR$result$pooledModel, sf = ifelse(int_sf, sf, 1))
      data[[i]][["lp"]] <- lp[[i]]

      m[[i]] <- glmer(
        data = data[[i]],
        formula = OUT_DC_RELAPSE ~ (1 | STUDYID) + offset(lp),
        family = binomial,
        control = glmerControl(optimizer = "bobyqa")
      )
      int_adj[i] <- summary(m[[i]])$coefficients[1, 1]
    }
  } else if (action == 2) {
    # fit the adjusted model
    for (i in 1:origModel$mice$result$m) {
      data[[i]] <- complete(origModel$mice$result_grp, action = i) %>% filter(STUDYID == 16)
      lp[[i]] <- predictAverageRandomIntercept(data = data[[i]], model = origModel$varSelectRR$result$pooledModel, sf = ifelse(int_sf, sf, 1))
      data[[i]][["lp"]] <- lp[[i]]

      m[[i]] <- glm(
        data = data[[i]],
        formula = OUT_DC_RELAPSE ~ offset(lp),
        family = binomial
      )
      int_adj[i] <- summary(m[[i]])$coefficients[1, 1]
    }
  } else {
    stop("action must be 1 (study-average recalibration) or 2 (Sundar 2019 recalibration)")
  }
  # return value
  list(
    summaryMeasures = df,
    opt_ci = opt_ci,
    opt_cal = opt_cal,
    opt_int = opt_int,
    shrinkageFactor = sf,
    interceptAdjustment = mean(int_adj),
    call = list(origModel = origModel, sumModel = sumModel, action = action, int_sf = int_sf)
  ) # taking the mean (Rubin's Rules)
}
