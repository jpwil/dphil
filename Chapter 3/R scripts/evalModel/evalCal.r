###########################
## calculate calibration ##
###########################

# For details see Wynants 2018 (DOI: 10.1177/0962280216668555), equation (11) and explanatory text
# this version does not run the random slope model - ie ignoring clustering when calculating calibration slope

# Safely wrap the function to capture errors and warnings
safeEvalCal <- function(prog, boot, data_eval = NULL) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- evalCal(prog, boot, data_eval)
          time <- list(time_start = time_start, time_end = Sys.time())
          prog$evalCal <- list(result = result, warnings = warnings_list, error = NULL, time = time)
          prog # return value
        },
        warning = function(w) {
          # Capture warnings in the warnings_list vector
          cat("processing warning now")
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning") # Prevent the warning from being displayed
        }
      )
    },
    error = function(e) {
      # Handle the error
      message("An error occurred: ", e$message)
      result <- NULL # Set result to NULL in case of error
      time <- list(time_start = time_start, time_end = Sys.time())
      prog$evalCal <- list(result = result, warnings = warnings_list, error = e$message, time = time)
      prog # return value
    },
    finally = {}
  )
}

# calculate calibration measures according to whether performing original model or bootstrapping
evalCal <- function(prog, boot, data_eval) {
  if (boot) {
    calcCalAP <- calcCal(prog = prog, boot = FALSE, data_eval = NULL)
    calcCalBT <- calcCal(prog = prog, boot = TRUE, data_eval = data_eval)

    result <- list(
      calIntAP = calcCalAP$calITL,
      calSlopeAP = calcCalAP$calSlope,
      calSlopeAP_BRMS = calcCalAP$calSlopeBRMS,
      calIntBT = calcCalBT$calITL,
      calSlopeBT = calcCalBT$calSlope,
      calSlopeBT_BRMS = calcCalBT$calSlopeBRMS,
      poolCalIntAP = poolCal(calcCalAP$calITL),
      poolCalSlopeAP = poolCal(calcCalAP$calSlope),
      poolCalSlopeAP_BRMS = poolCal(calcCalAP$calSlopeBRMS),
      poolCalIntBT = poolCal(calcCalBT$calITL),
      poolCalSlopeBT = poolCal(calcCalBT$calSlope),
      poolCalSlopeBT_BRMS = poolCal(calcCalBT$calSlopeBRMS)
    )
  } else {
    calcCalAP <- calcCal(prog = prog, boot = FALSE, data_eval = NULL)
    calcCalBT <- NULL

    result <- list(
      calIntAP = calcCalAP$calITL,
      calSlopeAP = calcCalAP$calSlope,
      calSlopeAP_BRMS = calcCalAP$calSlopeBRMS,
      poolCalIntAP = poolCal(calcCalAP$calITL),
      poolCalSlopeAP = poolCal(calcCalAP$calSlope),
      poolCalSlopeAP_BRMS = poolCal(calcCalAP$calSlopeBRMS)
    )
  }

  result # return value
}

##################################
# CALCULATE CALIBRATION MEASURES #
##################################

# this function fits models and extract the calibration intercept (calibration-in-the-large) and calibration slopes for each imputed dataset
calcCal <- function(prog, boot, data_eval) {
  # # for debugging:
  # boot <- FALSE
  # prog <- readRDS("results/202512141236_WITH_PD/originalModel_4.rds")
  # data_eval <- NULL
  # source("R/evalModel/predictAverageRandomIntercept.r") ## EVALUATE LINEAR PREDICTOR FOR AVERAGE RANDOM INTERCEPT (CALLED IN BOTH evalCIndex and MI_CAL)

  if (boot && is.null(data_eval)) {
    stop("When boot is TRUE, data_eval must be specified")
  }
  if (!boot && !is.null(data_eval)) {
    stop("When boot is FALSE, data_eval should not be specified")
  }
  if (boot && class(data_eval) != "mids") {
    stop("data_eval must be 'mids' class")
  }

  if (boot) {
    data <- data_eval
  } else {
    data <- prog$mice$result_grp
  }

  imp <- data$m
  # imp <- 3

  clust_no <- length(unique(mice::complete(data, 1)$STUDYID))

  ## CLUSTER LEVEL / CONDITIONAL CALIBRATION MEASURES
  ## USING AVERAGE RANDOM-INTERCEPT PREDICTIONS

  model_citl <- list()
  model_cs <- list()
  # model_cs_brms <- list()

  intercept_fixed <- array(numeric(), dim = c(imp, 2))
  # intercept <- array(numeric(), dim = c(imp, 2, clust_no))

  # slope_glmer <- array(numeric(), dim = c(imp, 2, clust_no))
  slope_fixed <- array(numeric(), dim = c(imp, 2))

  # slope_brms <- array(numeric(), dim = c(imp, 2, clust_no))
  # slope_fixed_brms <- array(numeric(), dim = c(imp, 2))

  # dimnames(intercept) <- list(paste0("imp", 1:imp), c("estimate", "se"), paste0("clust", 1:clust_no))
  # dimnames(slope_brms) <- dimnames(intercept)

  dimnames(intercept_fixed) <- list(paste0("imp", 1:imp), c("estimate", "se"))
  dimnames(slope_fixed) <- dimnames(intercept_fixed)

  for (i in 1:imp) {
    # for (i in 1:2) {
    cat("\nwithin-cluster calibration modelling, imputation: ", i, "\n")
    dataset <- mice::complete(data, i)
    lp <- predictAverageRandomIntercept(data = dataset, model = prog$varSelectRR$result$pooledModel) # average random intercept (set b_j = 0)
    dataset <- cbind(dataset, lp)

    cat("\nRunning the random intercept model with linear predictor as offset term (CITL terms)\n")
    model_citl[[i]] <- lme4::glmer(
      formula = OUT_DC_RELAPSE ~ offset(lp) + (1 | STUDYID),
      data = dataset,
      family = binomial(),
      lme4::glmerControl(optimizer = "bobyqa")
    )

    cat("\nRunning the random intercept and NON-random slope (glmer) model (CS terms)\n")
    model_cs[[i]] <- lme4::glmer(
      formula = OUT_DC_RELAPSE ~ (1 | STUDYID) + lp, # random intercept and NON-random slope model
      data = dataset,
      family = binomial(),
      lme4::glmerControl(optimizer = "bobyqa")
    )

    # cat("\nRunning the random intercept and random slope (brms) model (CS terms)\n")
    # model_cs_brms[[i]] <- brms::brm(
    #   formula = OUT_DC_RELAPSE ~ 1 + lp + (1 + lp | STUDYID), # random intercept and random slope model
    #   data = dataset,
    #   family = bernoulli(),
    #   iter = 2500,
    #   control = list(adapt_delta = 0.95)
    # )

    # intercept[i, 1, ] <- lme4::ranef(model_citl[[i]])$STUDYID[[1]] + lme4::fixef(model_citl[[i]])[[1]] # BLUP estimates (including fixed effect)
    # intercept[i, 2, ] <- sqrt(attr(lme4::ranef(model_citl[[i]])$STUDYID, "postVar")[1, 1, ]) # SE of the BLUPs (conditional on fixed effect)
    intercept_fixed[i, 1] <- summary(model_citl[[i]])$coefficients[1]
    intercept_fixed[i, 2] <- summary(model_citl[[i]])$coefficients[2]

    # slope_glmer[i, 1, ] <- lme4::fixef(model_cs[[i]])[2] + lme4::ranef(model_cs[[i]])$STUDYID[, 2] # Bayes' estimates for cluster-specific slope
    # slope_glmer[i, 2, ] <- sqrt(attr(lme4::ranef(model_cs[[i]])$STUDYID, "postVar")[2, 2, ]) # Standard error for cluster-specific slope
    # slope_fixed_glmer[i, 1] <- summary(model_cs[[i]])$coefficients[2, 1]
    # slope_fixed_glmer[i, 2] <- summary(model_cs[[i]])$coefficients[2, 2]

    # # adapt this for BRMS
    # slope_brms[i, 1, ] <- coef(model_cs_brms[[5]])$STUDYID[, 1, 2]
    # slope_brms[i, 2, ] <- coef(model_cs_brms[[5]])$STUDYID[, 2, 2]

    # slope_fixed_brms[i, 1] <- summary(model_cs_brms[[i]])$fixed[2, 1]
    # slope_fixed_brms[i, 2] <- summary(model_cs_brms[[i]])$fixed[2, 2]

    slope_fixed[i, 1] <- summary(model_cs[[i]])$coefficients[2, 1]
    slope_fixed[i, 2] <- summary(model_cs[[i]])$coefficients[2, 2]
  }

  ## POPULATION LEVEL / STANDARD CALIBRATION MEASURES
  ## USING AVERAGE RANDOM-INTERCEPT PREDICTIONS
  ## ALWAYS DO THIS

  # model_citl_pop <- list()
  # model_cs_pop <- list()

  # intercept_pop <- array(numeric(), dim = c(imp, 2))
  # slope_pop <- array(numeric(), dim = c(imp, 2))

  # dimnames(intercept_pop) <- list(paste0("imp", 1:imp), c("estimate", "se"))
  # dimnames(slope_pop) <- dimnames(intercept_fixed)

  # for (i in 1:imp) {
  #   cat("\nStandard (population) calibration modelling, imputation: ", i, "\n")
  #   dataset <- mice::complete(data, i)
  #   lp <- predictAverageRandomIntercept(data = dataset, model = prog$varSelectRR$result$pooledModel) # average random intercept (set b_j = 0)
  #   dataset <- cbind(dataset, lp)

  #   model_citl_pop[[i]] <- glm(
  #     formula = OUT_DC_RELAPSE ~ offset(lp),
  #     data = dataset,
  #     family = binomial()
  #   )

  #   model_cs_pop[[i]] <- glm(
  #     formula = OUT_DC_RELAPSE ~ lp,
  #     data = dataset,
  #     family = binomial(),
  #   )

  #   intercept_pop[i, 1] <- coef(model_citl_pop[[i]])
  #   intercept_pop[i, 2] <- sqrt(vcov(model_citl_pop[[i]]))
  #   slope_pop[i, 1] <- coef(model_cs_pop[[i]])[2]
  #   slope_pop[i, 2] <- sqrt(vcov(model_cs_pop[[i]])[2, 2])
  # }

  cat("\n\nResults from calcCal(): ")
  cat("\nintercept_fixed:\n:")
  print(intercept_fixed)
  cat("\nslope_fixed:\n:")
  print(slope_fixed)
  # cat("\nslope_fixed_brms:\n:")
  # print(slope_fixed_brms)

  result <- list(
    calITL = intercept_fixed,
    calSlope = slope_fixed
    # calSlopeBRMS = slope_fixed_brms
  )
  result # return value
}

#############################
# POOL CALIBRATION MEASURES #
#############################

# this function uses Rubin's rules to pool the calibration slope and intervals created from calcCal()
poolCal <- function(cal) {
  if (is.null(cal)) {
    return(NULL)
  }

  # cal <- calIntAP
  alpha <- 0.05 # for confidence intervals

  pool <- array(numeric(), dim = c(5))
  dimnames(pool) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))

  pool_temp <- mice::pool.scalar(
    Q = cal[, 1],
    U = cal[, 2]^2,
    n = Inf
  )

  pool[1] <- pool_temp$qbar
  pool[2] <- pool_temp$t
  pool[3] <- pool_temp$df
  pool[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  pool[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

  pool # return value
}
