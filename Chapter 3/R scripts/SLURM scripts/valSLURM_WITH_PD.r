#!/usr/bin/env Rscript

# This R script is intended to be run on the computing cluster using SLURM. It performs internal validation of an index model (with parasite grade) using bootstrapping and saves the results as an RDS file. The original model is specified as an argument when running the script, and a random seed is set based on the SLURM array batch number to ensure reproducibility across different runs of the script.

#########################
## INTERNAL VALIDATION ##
#########################

args <- commandArgs(trailingOnly = TRUE)
cat("\nSeed: ", args[1], "\n") # SLUM batch number
cat("\nModel: ", args[2], "\n") # Model object (rds file)

array_seed <- args[1]
set.seed(array_seed)
# set.seed(1)
# array_seed <- 1
originalModel <- readRDS(args[2])

# LOAD PACKAGES
packages <- c(
  "tidyverse",
  "lme4", # mixed modelling with glmer()
  "mice", # core package for multiple imputation: multiple imputatation with chained equations
  "micemd", # provides a range of multilevel MICE algorithms
  "countimp", # provides for multilevel poisson MICE algorithm (mice.impute.pois)
  "anthro", # WHO child growth standards (z-scores). Used for calculating BMI-for-age-z-scores in under 5s
  "anthroplus", # WHO 2007 References for School-age Children and Adolescents (5 to 19 years) (z-scores). Used for calculating BMI-for-age-z-scores in over 5s
  "aod", # provides the function wald.test() which performs a Wald chi square test for > 1 coefficient given their variance-covariance matrix. This is used during Wald test variable selection (selectWald function)
  "broom.mixed",
  "pROC", # for calculating the AUC (C-index) using in evalCIndex.r
  "meta",
  "brms", # Bayesian model fitting (for calibration)
  "boot", # for creating bootstrapped samples to calculate confidence intervals of C-index (evalCIndex.r)
  "arm"
)

p.error <- sapply(packages, require, character.only = TRUE)

# load function scripts that are called directly in this R script
source("R/runModel/runModel.r") # defines the function runModel() - for model development
source("R/evalModel/evalModel.r") # defines the function evalModel() - for model evaluation (both apparent validation & internal validation)
source("R/runModel/bootstrapData.r") # defines a function mi_bootstrap() that performs bootstrapping either by individual patient or by study ID

# the model requires these R datasets (scaled data) - created in R script 'mi_prepare'
data <- readRDS(file = "data/ads_impute.rds")
data_scale <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()

# define constants
# FOLDER <- format(Sys.time(), "%Y%m%d_%H") # the directory within the results/ directory to store model results
FOLDER <- "202605_WITH_PD_BOOT"
KEEP <- NULL # set to TRUE to force age and sex into the model
CLUSTER <- FALSE # set to TRUE to perform bootstrapping by cluster
IMP <- 30 # number of imputations
ITER <- 20 # number of iterations per imputation
P_WALD <- 1 # p-value for wald selection
P_RR <- 0.1 # p-value for rr selection

PREDICTORS_INIT <- c(
  "TREAT_GRP4", "ZZ_MAL", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs", "DM_SEX", "DM_AGEs", "ZZ_AGEs2", "ZZ_AGEs3",
  "LB_BL_HGB_GRP3", "VL_DURATIONs", "LB_BL_WBCs", "LB_BL_PLATs",
  "LB_BL_ALTs", "LB_BL_CREATs"
)

# create output folder if it doesn't already exist
if (!dir.exists(paste0("results/", FOLDER))) {
  dir.create(paste0("results/", FOLDER), recursive = TRUE)
}

#########################
## INTERNAL VALIDATION ##
#########################

# load original model into memory
# originalModel <- readRDS(args[2])

# create bootstraped dataset
data_boot <- bootstrapData(df = originalModel$data, bs_num = 1, cluster = CLUSTER)

# develop model in bootstrapped dataset
bootModel <- runModel(
  data = data_boot[[1]],
  data_scale = data_scale,
  iter = ITER,
  imp = IMP,
  seed = array_seed,
  keep = KEEP,
  p_wald = P_WALD,
  wald_prop_thres = 50, # percentage of models that include the predictor for it to be considered in Rubin's Rules variable selection stage
  p_rr = P_RR,
  predictors_init = PREDICTORS_INIT
)

saveRDS(bootModel, paste0("results/mice_only/bootModel_", array_seed, ".rds"))

# saveRDS(bootModel, paste0("results/", FOLDER, "/bootModel_init", array_seed, ".rds"))
# bootModel <- readRDS("results/20260103_WITH_PD_BOOT_1/bootModel_1.rds")

# evaluate model from bootstrapped dataset in both bootstrapped dataset and the original dataset
bootModel <- evalModel(
  prog = bootModel,
  boot = TRUE,
  data_eval = originalModel$mice$result_grp
)

saveRDS(bootModel, paste0("results/", FOLDER, "/bootModel_", array_seed, ".rds"))
