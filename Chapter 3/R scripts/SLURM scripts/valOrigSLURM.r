#!/usr/bin/env Rscript

# This R script is intended to be run on the computing cluster using SLURM. It performs apparent validation of an index model and saves the results as an RDS file. The original model is specified as an argument when running the script.

# WITH PD: models 6, 8
# WITHOUT PD: models

#########################
## INTERNAL VALIDATION ##
#########################

args <- commandArgs(trailingOnly = TRUE)
cat("\nModel: ", args[1], "\n") # Model object (rds file)

# array_seed <- 1
# originalModel <- args[1]

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
  # "boot", # for creating bootstrapped samples to calculate confidence intervals of C-index (evalCIndex.r)
  "arm"
)

p.error <- sapply(packages, require, character.only = TRUE)

# load function scripts that are called directly in this R script
# source("R/runModel/runModel.r") # defines the function runModel() - for model development
source("R/evalModel/evalModel.r") # defines the function evalModel() - for model evaluation (both apparent validation & internal validation)
# source("R/runModel/bootstrapData.r") # defines a function mi_bootstrap() that performs bootstrapping either by individual patient or by study ID

# the model requires these R datasets (scaled data) - created in R script 'mi_prepare'
# data <- readRDS(file = "data/ads_impute.rds")
# data_scale <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()

# define constants
# FOLDER <- format(Sys.time(), "%Y%m%d_%H") # the directory within the results/ directory to store model results
FOLDER <- "20250722_ORIG_PD_eval1"
# KEEP <- NULL # set to TRUE to force age and sex into the model
# CLUSTER <- FALSE # set to TRUE to perform bootstrapping by cluster
# IMP <- 20 # number of imputations
# ITER <- 20 # number of iterations per imputation
# P_WALD <- 1 # p-value for wald selection
# P_RR <- 0.05 # p-value for rr selection

# create output folder if it doesn't already exist
if (!dir.exists(paste0("results/", FOLDER))) {
  dir.create(paste0("results/", FOLDER), recursive = TRUE)
}

#########################
## APPARENT VALIDATION ##
#########################

# load original model into memory
originalModel <- readRDS(args[1])

# evaluate model in original dataset only
originalModel <- evalModel(
  prog = originalModel,
  boot = FALSE
  # data_eval = originalModel$mice$result_grp
)

fn <- sub(".*/", "", args[1])
saveRDS(originalModel, paste0("results/", FOLDER, "/", fn))
