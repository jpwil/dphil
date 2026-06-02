# here we can test the multiple imputation

rm(list = ls())
library(mice)
library(tidyverse)

filenames <- c("originalModel_WITH_PD_JAN2026_updated", "originalModel_WITHOUT_PD_JAN2026_updated")
mod_name <- c("with_parasite_grade", "without_parasite_grade")
df <- readRDS("data/ads_impute.rds")

# df <- readRDS("data/zip20251002.rds")
# df$mice %>% names()
# mice_obj <- df$mice$result
# plot(mice_obj)

# df_summary <- df %>%
#   group_by(CLUSTER_FCT) %>%
#   summarise(
#     n = n(),
#     across(
#       .cols = everything(),
#       .fns = list(
#         nMissing = ~ sum(is.na(.x)),
#         perMissing = ~ 100 * sum(is.na(.x)) / n()
#       ),
#       .names = "{.col}_{.fn}"
#     )
#   )

width <- 12 * 1.2
height <- 7 * 1.2

strip <-
  for (i in 1:2) {
    cat(i, "\n")
    df <- readRDS(paste0("results-share/", filenames[i], ".rds"))
    mice_obj <- df$mice$result

    # trace plots
    pdf(paste0("data/missing/tracePlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(plot(mice_obj))
    dev.off()

    # box and whisker plots
    pdf(paste0("data/missing/bwPlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(bwplot(mice_obj))
    dev.off()

    # density plots
    pdf(paste0("data/missing/densityPlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(densityplot(mice_obj))
    dev.off()

    # xyplots
    pdf(paste0("data/missing/weightHeightPlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(xyplot(mice_obj, VS_BL_WEIGHTs ~ VS_BL_HEIGHTs | .imp))
    dev.off()

    pdf(paste0("data/missing/weightAgePlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(xyplot(mice_obj, VS_BL_WEIGHTs ~ DM_AGEs | .imp))
    dev.off()

    pdf(paste0("data/missing/heightAgePlotModel_", mod_name[i], ".pdf"), width = width, height = height)
    print(xyplot(mice_obj, VS_BL_HEIGHTs ~ DM_AGEs | .imp))
    dev.off()
  }
