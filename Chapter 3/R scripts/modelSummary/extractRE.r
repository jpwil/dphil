## combine forest plots for manuscript figure

rm(list = ls())
library(patchwork)
library(tidyverse)
library(metafor)

# load data
re_int1 <- readRDS("data/forestCalIntM1.rds")
re_slope1 <- readRDS("data/forestCalSlopetM1.rds")
re_ci1 <- readRDS("data/forestCIM1.rds")

re_int2 <- readRDS("data/forestCalIntM2.rds")
re_slope2 <- readRDS("data/forestCalSlopetM2.rds")
re_ci2 <- readRDS("data/forestCIM2.rds")

# arrange data for output
extract_for_table <- function(model) {
  sum <- summary(model)
  pred <- predict(model)
  c(
    estimate = sum$b,
    estimate_lci = sum$ci.lb,
    estimate_uci = sum$ci.ub,
    predict_lci = pred$pi.lb,
    predict_uci = pred$pi.ub,
    tau2 = sum$tau2,
    I2 = sum$I2,
    H2 = sum$H2,
    Q = sum$QE,
    df = sum$df,
    p = sum$QEp
  )
}

int_m1 <- extract_for_table(re_int1)
slope_m1 <- extract_for_table(re_slope1)
ci_m1 <- extract_for_table(re_ci1)
int_m2 <- extract_for_table(re_int2)
slope_m2 <- extract_for_table(re_slope2)
ci_m2 <- extract_for_table(re_ci2)

df <- rbind(int_m1, slope_m1, ci_m1, int_m2, slope_m2, ci_m2) %>% as.data.frame()
df[["term"]] <- rownames(df)

df <- df %>% mutate(across(c(estimate, estimate_lci, estimate_uci, predict_lci.x.flip, predict_uci.y.flip), ~ ifelse(term %in% c("ci_m1", "ci_m2"), plogis(.x), .x)))
df %>% names()

df_char <- df %>%
  mutate(
    across(
      c(estimate, estimate_lci, estimate_uci, predict_lci.x.flip, predict_uci.y.flip, H2, tau2),
      ~ sprintf("%.2f", .x)
    ),
    Q = sprintf("%.1f", Q),
    I2 = sprintf("%.1f", I2),
    p = sprintf("%.2e", p)
  )

df_char_final <- df_char %>%
  mutate(
    est = paste0(estimate, " (", estimate_lci, " - ", estimate_uci, ")"),
    pi = paste0(predict_lci.x.flip, " - ", predict_uci.y.flip)
  )

df_char_final %>%
  select(est, pi, tau2, I2, H2, Q, df, p) %>%
  xtable()
