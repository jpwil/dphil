###########################################################
# CREATE TRACE PLOTS AND COMPARE MISSING VS ORIGINAL DATA #
###########################################################

# missing data diagnostics
# run this script to populate the results/pdf/ folder with diagnostic plots

library(mice)
library(tidyverse)
rm(list = ls())

# Model 1
# prog <- readRDS("results-share/20250701_valOrig_withPD/originalModel_1.rds")
prog <- readRDS("data/test_poisson.rds")
mice1 <- prog$mice$result # rename mids object as mice

# Model 2
# prog <- readRDS("results-share/20250701_valOrig_withoutPD/originalModel_2.rds")
prog <- readRDS("data/test_zip.rds")
mice2 <- prog$mice$result # rename mids object as mice

# trace plots
pdf("results/pdf/tracePlotModel1.pdf")
plot(mice1)
dev.off()

pdf("results/pdf/tracePlotModel2.pdf")
plot(mice2)
dev.off()

# box and whisker plots
pdf("results/pdf/bwPlotModel1.pdf")
bwplot(mice1)
dev.off()

pdf("results/pdf/bwPlotModel2.pdf")
bwplot(mice2)
dev.off()

# density plots
pdf("results/pdf/densityPlotModel1.pdf")
densityplot(mice1)
dev.off()

pdf("results/pdf/densityPlotModel2.pdf")
densityplot(mice2)
dev.off()

# xyplots
pdf("results/pdf/weightHeightPlotModel1.pdf")
xyplot(mice1, VS_BL_WEIGHTs ~ VS_BL_HEIGHTs | .imp)
dev.off()

pdf("results/pdf/weightAgePlotModel1.pdf")
xyplot(mice1, VS_BL_WEIGHTs ~ DM_AGEs | .imp)
dev.off()

pdf("results/pdf/heightAgePlotModel1.pdf")
xyplot(mice1, VS_BL_HEIGHTs ~ DM_AGEs | .imp)
dev.off()

pdf("results/pdf/weightHeightPlotModel2.pdf")
xyplot(mice2, VS_BL_WEIGHTs ~ VS_BL_HEIGHTs | .imp)
dev.off()

pdf("results/pdf/weightAgePlotModel2.pdf")
xyplot(mice2, VS_BL_WEIGHTs ~ DM_AGEs | .imp)
dev.off()

pdf("results/pdf/heightAgePlotModel2.pdf")
xyplot(mice2, VS_BL_HEIGHTs ~ DM_AGEs | .imp)
dev.off()
