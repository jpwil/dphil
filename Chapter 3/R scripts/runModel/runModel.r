# this script defines a function that develops and evaluates a VL prognostic model with relapse as the outcome

runModel <- function(data, data_scale, iter, imp, seed, keep, p_wald, wald_prop_thres, p_rr, predictors_init) {
  ## for debugging purposes only (uncomment if needed)
  # data <- readRDS(file = "data/ads_impute.rds")
  # data_scale <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()
  # keep <- NULL # set to TRUE to force age and sex into the model
  # cal_cond <- TRUE # set to TRUE to evaluate conditional ....
  # imp <- 4 # number of imputations
  # iter <- 4 # number of iterations per imputation
  # wald_prop_thres <- 50 # of imp models selected using backwards selection and Wald test, this is the proportion of models that must contain the variable for the variable to continue to second stage of variable selection
  # p_wald <- 0.10 # p-value for wald selection
  # p_rr <- 0.05 # p-value for rr selection
  # seed <- 1
  # predictors_init <- c(
  #   "DM_SEX", "DM_AGEs", "ZZ_AGEs2", "ZZ_AGEs3", "TREAT_GRP4", "ZZ_MAL",
  #   "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "LB_BL_WBCs", "LB_BL_PLATs",
  #   "LB_BL_ALTs", "LB_BL_CREATs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs"
  # )

  ## load helper functions
  source("R/evalModel/predictAverageRandomIntercept.r")
  source("R/evalModel/evalCIndex.r")
  source("R/runModel/createGroups.r") ## Grouping anaemia and malnutrition
  # source("R/runModel/selectWald.r") ## Variable selection - imputation specific (S2 method)
  source("R/runModel/selectRubinsRules.r") ## Variable selection - Rubin's rules
  source("R/runModel/scaleFunctions.r") # Functions for converting between scaled variables and unscaled variables

  ####################################
  ## INITIALISE MULTIPLE IMPUTATION ##
  ####################################

  source("R/runModel/miceWrapper.r") # load custom mice wrapper (error, warning, timing tracking)
  source("R/runModel/passiveImputation.r") # load passive imputation functions, called by mice via method argument (method string defined below)

  # These 3 lines of code inject code to the mice::sampler() function for debugging purposes. This works with mice version 3.16.0. Caution- will cause problems if using mice version 3.17.0.
  source("R/runModel/samplerJW.r") # source edited version of sampler function
  environment(sampler) <- asNamespace("mice") # set environment of my sampler function to namespace environment of mice package
  assignInNamespace("sampler", sampler, ns = "mice") # overwrite existing sampler function in mice namespace with my version

  # 'Method' defines the imputation method for each variable undergoing multiple imputation.
  # This includes passive imputation methods for age quadratic and cubic terms, BMI and BMI-for-age-z-scores.
  # For documentation on specific methods see ?mice.impute.pois, ?mice.impute.2l.lmer, etc.
  method <- c(
    STUDYID = "",
    DM_SEX = "",
    DM_AGEs = "2l.lmer",
    VS_BL_WEIGHTs = "2l.lmer",
    VS_BL_HEIGHTs = "2l.lmer",
    # VL_HISTORY = "2l.bin",
    VL_DURATIONs = "2l.lmer",
    TREAT_GRP4 = "",
    LB_BL_HGBs = "2l.lmer",
    LB_BL_ALTs = "2l.lmer",
    LB_BL_WBCs = "2l.lmer",
    LB_BL_PLATs = "2l.lmer",
    LB_BL_CREATs = "2l.lmer",
    MP_BL_SPLEEN_LENGTHs2 = "2l.lmer",
    MB_COMBINEDs = "2l.poisson",
    OUT_DC_RELAPSE = "",
    ZZ_BMIs = "~I(passivelyImputeBMIs(data_scale, VS_BL_WEIGHTs, VS_BL_HEIGHTs, DM_AGEs))", # sourced from "R/runModel/passiveImputation.r"
    ZZ_BMI_Z = "~I(passivelyImputeBMIzScore(data_scale, DM_SEX, DM_AGEs, VS_BL_WEIGHTs, VS_BL_HEIGHTs))", # sourced from "R/runModel/passiveImputation.r"
    ZZ_AGEs2 = "~I(DM_AGEs^2)",
    ZZ_AGEs3 = "~I(DM_AGEs^3)"
  )

  # The predictor matrix (a named numeric 2x2 matrix) defines the relationship between predictors and the outcome in the imputation model.
  # Each row corresponds to the variable to be imputed.
  # Each column describes the variables to be included for the corresponding variable to be imputed.
  # 1: include as fixed effect; 0: do not include; -2: include as random effect (this is the random intercept term to maintain congeniality)
  predictor.matrix <- readRDS(file = "data/mi_pm.rds")
  predictor.matrix["MB_COMBINEDs", "STUDYID"] <- -2

  # Post-imputation processes. Ensures non-sensical, but rare, imputed values are coerced to sensible values.
  # E.g. avoiding negative age, and imputation of reasonable weights and heights
  # Without this, the code would throw errors when calculating BMI-for-age and WFH z-scores
  # These changes only affect a handful of values. Otherwise, all it takes is one out-of-range value to break the code (antho/anthroplus code)
  post <- c(
    STUDYID = "",
    DM_SEX = "",
    DM_AGEs = "imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(1, 80) - data_scale[data_scale$var == \"DM_AGE\", \"means\"])/data_scale[data_scale$var == \"DM_AGE\", \"sd\"])",
    VS_BL_WEIGHTs = "imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(1, 100) - data_scale[data_scale$var == \"VS_BL_WEIGHT\", \"means\"])/data_scale[data_scale$var == \"VS_BL_WEIGHT\", \"sd\"])",
    VS_BL_HEIGHTs = "imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(40, 200) - data_scale[data_scale$var == \"VS_BL_HEIGHT\", \"means\"])/data_scale[data_scale$var == \"VS_BL_HEIGHT\", \"sd\"])",
    # VL_HISTORY = "",
    VL_DURATIONs = "",
    TREAT_GRP4 = "",
    LB_BL_HGBs = "",
    LB_BL_ALTs = "",
    LB_BL_WBCs = "",
    LB_BL_PLATs = "",
    LB_BL_CREATs = "",
    MP_BL_SPLEEN_LENGTHs2 = "",
    MB_COMBINEDs = "",
    OUT_DC_RELAPSE = "",
    ZZ_BMIs = "",
    ZZ_BMI_Z = "",
    ZZ_AGEs2 = "",
    ZZ_AGEs3 = ""
  )

  # Define the 'prog' list which contains model data
  prog <- list()
  prog$data <- data
  prog$data_scale <- data_scale

  #################################
  ## PERFORM MULTIPLE IMPUTATION ##
  #################################

  # returns a multiply imputed data set (mids) class
  prog$mice <- miceWrapper(
    data = data,
    predictorMatrix = predictor.matrix,
    method = method,
    m = imp, # number of imputations
    maxit = iter, # number of iterations
    seed = seed, # specify seed for reproducibility
    post = post,
    printFlag = TRUE
  )

  #############################
  ## GROUP IMPUTED VARIABLES ##
  #############################

  # perform grouping of BMI and anaemia in the imputed datasets
  # Save as a mids class object in prog$mice$result_grp
  prog <- createGroups(prog)

  ##########################################
  ## VARIABLE SELECTION AND MODEL FITTING ##
  ##########################################

  ## FIRST STAGE
  # Variable selection is performed separately in each of the m imputed datasets, and variables are selected that appear in at least
  # (wald_prop_thres)% of the m models. If p_wald = 1, do not perform this stage (current setting).
  # In Austin et al 2019 and Wood et al 2008, this is described as 'Separate imputations S2' when wald_prop_thres is 50%.
  if (p_wald != 1) {
    prog <- safeSelectWald(
      prog = prog,
      keep = keep,
      p_select = p_wald,
      predictors_init = predictors_init, # predictors to be considered in the model
      outcome = "OUT_DC_RELAPSE",
      cluster = "STUDYID",
      cat = c("TREAT_GRP4", "ZZ_MAL"),
      prop_thres = wald_prop_thres
    )
  } else {
    prog$varSelectWald$result$selectedVariables <- predictors_init # pass all candidate variables to next function
  }

  ## SECOND STAGE
  # This step involves using Rubin's Rules at each stage of variable selection to determine the statistical significance of each candidate predictor.
  # The coefficient with the largest p-value is then removed (if above the p_rr threshold).
  # predictors_init is not supplied here as an argument, because it can be extracted from prog$varSelectWald$result$selectedVariables
  # if p_rr = 1, do not perform this step
  if (p_rr != 1) {
    prog <- safeSelectRubinsRules( # this function lives in selectRubinsRules.r
      prog = prog,
      keep = keep,
      p_select = p_rr,
      outcome = "OUT_DC_RELAPSE",
      cluster = "STUDYID",
      cat = c("TREAT_GRP4", "ZZ_MAL") # currently not used in function
    )
  }

  ############
  ## OUTPUT ##
  ############

  prog$call <- list(iter = iter, imp = imp, seed = seed, keep = keep, p_wald = p_wald, wald_prop_thres = wald_prop_thres, p_rr = p_rr, predictors_init = predictors_init, time = Sys.time())
  prog # return value
}
