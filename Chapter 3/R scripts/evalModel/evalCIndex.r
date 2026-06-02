# calculate cluster specific Harrell's c-index and variance estimates (bootstrap, deLong)

# van Klaveren, D., Steyerberg, E.W., Perel, P. et al.
# Assessing discriminative ability of risk models in clustered data. BMC Med Res Methodol 14, 5 (2014).
# https://doi.org/10.1186/1471-2288-14-5

library(tidyverse)
library(pROC)
library(mice)
library(lme4)
library(broom.mixed)
library(metafor)

###################################################
# Calculate cluster specific C index and variance #
###################################################

# define helper function for calculating the cluster-specific C-indices and variances
clusterCI <- function(prog, boot, data_eval, varMethod = "b") {
  # variables for number of clusters and number of imputations

  # for debugging
  # prog <- readRDS("results-share/originalModel_207.rds")
  # boot <- FALSE
  # varMethod <- "b"

  ## INITIALISE VALUES ##

  if (boot) {
    data <- data_eval
  } else {
    data <- prog$mice$result_grp
  }

  cluster_num <- length(unique(complete(data, 1)[, "STUDYID"]))
  imp_num <- data$m

  # array of CI estimates (each imputation is a row, each cluster is a column)
  ci_est <- array(NA, dim = c(imp_num, cluster_num))

  # array of CI variances (each imputation is a row, each cluster is a column)
  ci_var <- array(NA, dim = c(imp_num, cluster_num))

  # for population (standard) C-indices
  ci_est_pop <- rep(NA_real_, imp_num)
  ci_var_pop <- rep(NA_real_, imp_num)

  # for weighting of fixed-effects C-indices, calculate the number of subject pairs (event & non-event)
  weight_pairs <- rep(NA_real_, cluster_num)

  ## CALCULATE NUMBER OF PAIRS OF RELAPSES AND NON RELAPSES FOR EACH CLUSTER ##

  # split dataset into a list of smaller single-cluster datasets
  # let's just look at the first imputed dataset, as outcome & study ID is the same for all imputations
  dataset1 <- complete(data, 1)
  dataset1 <- dataset1 %>% arrange(STUDYID)
  dataset_split1 <- split(dataset1, dataset1$STUDYID)

  # calculate the number of weighted pairs for each cluster (same for all imputations!)
  for (j in seq_len(cluster_num)) { # loop over clusters
    outcome <- dataset_split1[[j]]$OUT_DC_RELAPSE
    weight_pairs[j] <- sum(outcome) * sum(!outcome)
  }

  ## CALCULATE C-STATISTICS (AUCs) FOR EACH IMPUTATION (i) AND CLUSTER (k) ##

  for (i in seq_len(data$m)) { # loop over imputations
    dataset <- complete(data, i)
    lp <- predictAverageRandomIntercept(data = dataset, model = prog$varSelectRR$result$pooledModel)
    dataset$lp <- lp
    dataset <- dataset %>% arrange(STUDYID)

    # for comparison, let's calculate the standard C-index (ignoring clustering)
    roc_obj <- roc(response = dataset$OUT_DC_RELAPSE, predictor = dataset$lp, direction = "<", quiet = TRUE)
    ci_est_pop[i] <- roc_obj$auc
    ci_var_pop[i] <- var(roc_obj, method = varMethod)

    # calculate c-statistic for individual clusters
    dataset_split <- split(dataset, dataset$STUDYID)
    for (k in seq_len(cluster_num)) { # loop over clusters
      cat("\nCalculating for imputation: ", i, " and cluster: ", k, "\n")
      if (length(unique(dataset_split[[k]]$OUT_DC_RELAPSE)) != 2) { # need at least one event and one non-event
        ci_est[i, k] <- NA
        ci_var[i, k] <- NA
      } else {
        roc_obj <- roc(response = dataset_split[[k]]$OUT_DC_RELAPSE, predictor = dataset_split[[k]]$lp, direction = "<", quiet = TRUE)
        ci_est[i, k] <- roc_obj$auc

        if (ci_est[i, k] == 0 || ci_est[i, k] == 1) {
          ci_var[i, k] <- NA # CI variance not defined for CI of 0 or 1
        } else {
          ci_var[i, k] <- var(roc_obj, method = varMethod)
        }
      }
    }
  }

  list(weightPairs = weight_pairs, estimateArray = ci_est, varianceArray = ci_var, estPop = ci_est_pop, varPop = ci_var_pop) # return this
}

#########################################################
# Combine cluster-specific C-indices across imputations #
#########################################################

# Burgess S, White IR, Resche-Rigon M, Wood AM.
# Combining multiple imputation and meta-analysis with individual participant data. Stat Med. 2013 Nov 20;32(26):4499-514.
# doi: 10.1002/sim.5844. Epub 2013 May 24. PMID: 23703895; PMCID: PMC3963448.

# Burgess et al [assuming congeniality between analysis and imputation models]:
# In an inverse-variance weighted meta-analysis, we should impute missing data and
# apply Rubin’s rules at the study level prior to meta-analysis, rather than meta-analyzing
# each of the multiple imputations and then combining the meta-analysis estimates using Rubin’s rules

# this is the helper function that pools the clusterCIs
poolClusterCI <- function(clusterCIs) {
  cluster_num <- dim(clusterCIs$estimateArray)[2]
  ci_est <- clusterCIs$estimateArray
  ci_var <- clusterCIs$varianceArray

  Qbar <- numeric(cluster_num)
  Tvar <- numeric(cluster_num)

  for (i in seq_len(cluster_num)) {
    df_temp <- data.frame(est = ci_est[, i], var = ci_var[, i])
    df_temp <- df_temp %>% filter(!is.na(est) & !is.na(var))

    rr <- pool.scalar(df_temp$est, df_temp$var)
    Qbar[i] <- rr$qbar
    Tvar[i] <- rr$t
  }

  list(pooledEstimate = Qbar, totalVariances = Tvar) # return this
}

####################################################################################
# Perform random effects meta-analysis to calculate overall within-cluster C index #
####################################################################################

# wrapper
evalCIout <- function(prog, boot, data_eval) {
  if (boot) {
    evalCI_AP <- evalCI(prog = prog, boot = FALSE, data_eval = NULL)
    evalCI_BT <- evalCI(prog = prog, boot = TRUE, data_eval = data_eval)

    result <- list(
      evalCI_AP_RE = evalCI_AP$RE,
      evalCI_AP_FE_cond = evalCI_AP$FE_cond,
      evalCI_AP_FE_pop = evalCI_AP$FE_pop,
      evalCI_BT_RE = evalCI_BT$RE,
      evalCI_BT_FE_cond = evalCI_BT$FE_cond,
      evalCI_BT_FE_pop = evalCI_BT$FE_pop
    )
  } else {
    evalCI_AP <- evalCI(prog = prog, boot = FALSE, data_eval = NULL)
    evalCI_BT <- NULL

    result <- list(
      evalCI_AP_RE = evalCI_AP$RE,
      evalCI_AP_FE_cond = evalCI_AP$FE_cond,
      evalCI_AP_FE_pop = evalCI_AP$FE_pop,
      evalCI_BT_RE = NULL,
      evalCI_BT_FE_cond = NULL,
      evalCI_BT_FE_pop = NULL
    )
  }
  result # return value
}

# perform random-effects meta-analysis using REML for between-cluster variance and Knapp and Hartung method
# [Hartung–Knapp–Sidik–Jonkman (HKSJ) adjustment] for adjusting CI of summary effect

## TEST CODE

# the main function which calls the above helper functions
evalCI <- function(prog, boot, data_eval) {
  clusterCIs <- clusterCI(prog, boot, data_eval, "b")
  pooledClusterCIs <- poolClusterCI(clusterCIs)

  # random effects (within-cluster)
  res1 <- rma.uni(
    yi = pooledClusterCIs$pooledEstimate,
    vi = pooledClusterCIs$totalVariances,
    method = "REML",
    test = "knha"
  )

  # fixed effects (within-cluster, cluster-specific weights are pairs of events & non-events within the cluster)
  res2 <- rma.uni(
    yi = pooledClusterCIs$pooledEstimate,
    vi = pooledClusterCIs$totalVariances,
    weights = clusterCIs$weightPairs
  )

  res3 <- pool.scalar(Q = clusterCIs$estPop, U = clusterCIs$varPop)

  # fixed effects (overall / population / standard) - for comparison only, don't use this
  list(RE = res1, FE_cond = res2, FE_pop = res3) # return value
}

safeEvalCIout <- function(prog, boot, data_eval) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- evalCIout(prog, boot, data_eval)
          time <- list(time_start = time_start, time_end = Sys.time())
          prog$evalCI <- list(result = result, warnings = warnings_list, error = NULL, time = time)
          prog # Return value
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
      prog$evalCI <- list(result = result, warnings = warnings_list, error = e$message, time = time)
      prog # Return value
    },
    finally = {}
  )
}
