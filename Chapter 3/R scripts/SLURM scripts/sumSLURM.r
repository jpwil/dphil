#!/usr/bin/env Rscript

# this R script is intended to be run on the computing cluster using SLURM. It summarises the results of all models in a folder and saves the summary as an RDS file.

library(tidyverse)

##################################
## SUMMARISE MODELS IN A FOLDER ##
##################################

args <- commandArgs(trailingOnly = TRUE)
cat("\nFolder: ", args[1], "\n\n")
FOLDER <- args[1]
FFOLDER <- paste0("results/", FOLDER, "/")

modelSummary <- function(FFOLDER, wald_thres) {
  rdsFiles <- sort(dir(FFOLDER, pattern = ".rds$"))
  df <- list()
  l <- length(rdsFiles)
  callDetails <- readRDS(paste0(FFOLDER, rdsFiles[1]))
  callDetails <- callDetails$call
  for (i in seq_along(rdsFiles)) {
    cat("Processing ", i, " of ", l, "models in Folder: ", FFOLDER, "\n")
    model <- readRDS(paste0(FFOLDER, rdsFiles[i]))
    wt <- model$varSelectWald$result$termCount
    df[[i]] <- list(
      terms_rr = paste0(sort(model$varSelectRR$result$finalVariables$term), collapse = ","),
      terms_wald = paste0(sort(wt[wt$prop >= wald_thres & wt$term != "(Intercept)", ]$term), collapse = ","),
      CI_AP = ifelse(is.null(model$evalCI$result$conditionalAP[4]), NA, model$evalCI$result$conditionalAP[4]),
      CI_BT = ifelse(is.null(model$evalCI$result$conditionalBT[4]), NA, model$evalCI$result$conditionalBT[4]),
      Cal_Int_AP = ifelse(is.null(model$evalCal$result$poolCalIntAP[1]), NA, model$evalCal$result$poolCalIntAP[1]),
      Cal_Int_BT = ifelse(is.null(model$evalCal$result$poolCalIntBT[1]), NA, model$evalCal$result$poolCalIntBT[1]),
      Cal_Slope_AP = ifelse(is.null(model$evalCal$result$poolCalSlopeAP[1]), NA, model$evalCal$result$poolCalSlopeAP[1]),
      Cal_Slope_BT = ifelse(is.null(model$evalCal$result$poolCalSlopeBT[1]), NA, model$evalCal$result$poolCalSlopeBT[1])
    )
    rm(model)
    rm(wt)
    gc()
  }

  out <- list(df = do.call(rbind, lapply(df, as.data.frame)), call = callDetails)
  out # return value
}

out <- modelSummary(FFOLDER = FFOLDER, wald_thres = 50)
cat("\nsaving file to ", paste0("results-share/", args[1]))
out <- saveRDS(out, paste0("results-share/", FOLDER, ".rds"))
cat("\nfile saved")
