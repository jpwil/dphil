library(tidyverse)
library(cowplot)
rm(list = ls())

forest1 <- readRDS("graphs/plot_list_forest1.rds")
forest2 <- readRDS("graphs/plot_list_forest2.rds")
text1 <- readRDS("graphs/plot_list_text1.rds")
text2 <- readRDS("graphs/plot_list_text2.rds")

plot_forest <- plot_grid(
  forest1[[1]],
  forest1[[2]],
  forest2[[3]],
  nrow = 1,
  align = "h"
)

plot_text <- plot_grid(
  plot_list_text[[1]],
  plot_list_text[[2]],
  plot_list_text[[3]],
  plot_list_text[[4]],
  plot_list_text[[5]],
  plot_list_text[[6]],
  nrow = 1,
  align = "h"
)
