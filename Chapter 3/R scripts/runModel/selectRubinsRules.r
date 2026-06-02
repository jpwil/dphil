# this function performs backwards stepwise for the fixed terms of a glmm (glmer object),
# by applying Rubin's Rules at each stage of variable selection

selectRubinsRules <- function(prog, keep, p_select, outcome = "OUT_DC_RELAPSE", cluster = "STUDYID", cat = c("TREAT_GRP4", "ZZ_MAL")) {
  # If ZZ_MAL or TREAT_GRP4 are in the list of predictors, then ensure they only appear as one term for the formula
  predictors_init <- prog$varSelectWald$result$selectedVariables
  if (any(grepl("^ZZ_MAL", predictors_init))) {
    predictors_init <- c(predictors_init[!grepl("^ZZ_MAL", predictors_init)], "ZZ_MAL")
  }

  if (any(grepl("^TREAT_GRP4", predictors_init))) {
    predictors_init <- c(predictors_init[!grepl("^TREAT_GRP4", predictors_init)], "TREAT_GRP4")
  }

  # initiate values
  optimizer <- "nloptwrap"
  optCtrl <- list(nloptwrap = list(
    maxeval   = 200000,
    ftol_abs  = 1e-8,
    xtol_abs  = 1e-8
  ))

  var_track <- data.frame(matrix(ncol = 3, nrow = 0)) # to allow tracking of variable selection
  colnames(var_track) <- c("Step number", "Variable", "removal p.value")

  formula_str <- paste0(outcome, "~", paste0(predictors_init, collapse = " + "), "+ (1 |", cluster, ")")
  formula_new <- as.formula(formula_str)

  i <- 1
  stop <- FALSE

  # one while loop for each selection stage
  while (!stop) {
    cat("\n\n** Starting while loop", i, "**\n\n\n")
    models_fit <- list()
    tau_squared <- numeric()
    icc <- numeric()

    # fit the model across all imputations and store in models_fit[j = 1 to m]
    # the final pooled model (run in the last while loop) is returned by the selectRubinsRules function (plus tau and ICC terms for each component model)
    for (j in 1:prog$mice$result$m) {
      cat("Full imputation:", j, "\n")
      data <- mice::complete(prog$mice$result_grp, j)
      models_fit[[j]] <- lme4::glmer(
        data = data,
        formula = formula_new,
        family = binomial,
        control = lme4::glmerControl(optimizer = optimizer, optCtrl = optCtrl)
      )
      tau_squared[j] <- lme4::VarCorr(models_fit[[j]])$STUDYID[1, 1]
      icc[j] <- tau_squared[j] / (tau_squared[j] + pi^2 / 3)
    }

    # pool models using Rubin's rules
    models_pool <- mice::pool(models_fit) # An object of class mipo
    model_terms <- as.character(summary(models_pool)$term) # univariate Wald tests combined with Rubin's Rules
    select_table <- summary(models_pool) %>% dplyr::select(term, p.value)

    # reduced model for D1 multivariate Wald test - TREAT_GRP4
    if (any(grepl("^TREAT_GRP4", model_terms))) {
      models_fit_comp <- list()
      formula_temp <- update(formula_new, . ~ . - TREAT_GRP4) # need to fit model without categorical variable

      for (j in 1:prog$mice$result$m) {
        cat("TREAT_GRP multivariate Wald model:", j, "\n")
        models_fit_comp[[j]] <-
          glmer(
            data = data,
            formula = formula_temp,
            family = binomial,
            control = lme4::glmerControl(optimizer = optimizer, optCtrl = optCtrl)
          )
      }

      comparison <- mice::D1(models_fit, models_fit_comp)
      add_row <- data.frame(term = "TREAT_GRP4", p.value = summary(comparison)$comparisons$p.value)
      select_table <- rbind(select_table, add_row)
      select_table <- select_table %>% filter(!term %in% c("TREAT_GRP4SDA", "TREAT_GRP4OTHER"))
    }

    # reduced model for D1 multivariate Wald test - ZZ_MAL
    if (any(grepl("^ZZ_MAL", model_terms))) {
      models_fit_comp <- list()
      formula_temp <- update(formula_new, . ~ . - ZZ_MAL) # fit model without categorical variable
      for (j in 1:prog$mice$result$m) {
        cat("ZZ_MAL multivariate Wald model:", j, "\n")
        models_fit_comp[[j]] <-
          lme4::glmer(
            data = data,
            formula = formula_temp,
            family = binomial,
            control = lme4::glmerControl(optimizer = optimizer, optCtrl = optCtrl)
          )
      }
      comparison <- mice::D1(models_fit, models_fit_comp)
      add_row <- data.frame(term = "ZZ_MAL", p.value = summary(comparison)$comparisons$p.value)
      select_table <- rbind(select_table, add_row)
      select_table <- select_table %>% filter(!term %in% c("ZZ_MALModerate", "ZZ_MALSevere"))
    }

    # clean up remaining terms so they can be used to update formula
    select_table <- select_table %>%
      mutate(
        term = as.character(term)
      ) %>%
      filter(term != "(Intercept)")
    # force only highest polynomial terms to be considered for removal

    select_table_available <- select_table
    if ("ZZ_AGEs3" %in% select_table[, 1]) {
      select_table_available <- select_table_available %>% filter(!term %in% c("DM_AGEs", "ZZ_AGEs2"))
    } else if ("ZZ_AGEs2" %in% select_table[, 1]) {
      select_table_available <- select_table_available %>% filter(!term %in% c("DM_AGEs"))
    }

    # force certain variables to stay in the model (no terms are removed if keep is NULL)
    select_table_available <- select_table_available %>%
      filter(!(term %in% keep))

    # identify the term with highest p-value
    highest_p <- select_table_available[which.max(select_table_available$p.value), ]
    remove_term <- paste("~ . -", highest_p[[1]])

    if (highest_p[[2]] > p_select) {
      var_track[i, 1] <- i
      var_track[i, 2] <- highest_p[[1]]
      var_track[i, 3] <- highest_p[[2]]
      cat("\nRemoving predictor: ", highest_p[[1]], ", with p-value of: ", highest_p[[2]], "\n", sep = "")
      formula_new <- update(formula_new, as.formula(remove_term))
    } else {
      cat("\nThere are no further terms with p > ", p_select, "\n")
      stop <- TRUE
    }
    i <- i + 1
  }
  result <- list(pooledModel = models_pool, finalVariables = select_table, trackSelection = var_track, finalModels = models_fit, tau_squared = tau_squared, icc = icc)
  result # output
}

safeSelectRubinsRules <- function(prog, keep, p_select, outcome, cluster, cat) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          # Call the select_rr function
          result <- selectRubinsRules(prog, keep, p_select, outcome, cluster, cat)
          time <- list(time_start = time_start, time_end = Sys.time())
          prog$varSelectRR <- list(result = result, warnings = warnings_list, error = NULL, time = time)
          prog # Return the result if successful
        },
        warning = function(w) {
          # Capture warnings in the warnings_list vector
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning") # Prevent the warning from being displayed
        }
      )
    },
    error = function(e) {
      # Handle the error
      message("An error occurred: ", e$message)
      result <<- NULL # Set result to NULL in case of error
      time <- list(time_start = time_start, time_end = Sys.time())
      prog$varSelectRR <- list(result = NULL, warnings = warnings_list, error = e$message, time = time)
      prog # Return error message
    },
    finally = {
      # Code that will always run, even if there is an error or warning
      cat("\nCompleted execution of selectRubinsRules\n")
    }
  )
}
