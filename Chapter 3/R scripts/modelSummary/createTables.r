#########################
# CREATE SUMMARY TABLES #
#########################

library(glue)
library(metafor)
library(lme4)
source("R/modelSummary/optimismParam.r")
df_scale <- readRDS("data/ads_impute_scale.rds")

# the aim of this script is to create summary tables for LaTeX output for

# 1) model coefficients (both original and optimism-corrected)
# 2) performance measures (original and optimism-corrected)

## PERFORMANCE MEASURES

prog1 <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")
cstat1 <- readRDS("results/summaryCIM1_withPD_ma.rds")
cal1 <- readRDS("data/forestCalSlopeM1.rds")
sum1 <- readRDS("results-share/202605_WITH_PD_BOOT.rds")
op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 2, int_sf = TRUE)


cal1_est <- prog1$evalCal$result$poolCalSlopeAP_BRMS[1] %>% formatC(format = "f", digits = 2)
cal1_ll <- prog1$evalCal$result$poolCalSlopeAP_BRMS[4] %>% formatC(format = "f", digits = 2)
cal1_ul <- prog1$evalCal$result$poolCalSlopeAP_BRMS[5] %>% formatC(format = "f", digits = 2)
cal1_opt <- op1$opt_cal %>% formatC(format = "f", digits = 3)
cal1_est_adj <- formatC(prog1$evalCal$result$poolCalSlopeAP_BRMS[1] - op1$opt_cal, format = "f", digits = 2)

cstat1_est <- cstat1$re$pred %>% formatC(format = "f", digits = 2)
cstat1_ll <- cstat1$re$ci.lb %>% formatC(format = "f", digits = 2)
cstat1_ul <- cstat1$re$ci.ub %>% formatC(format = "f", digits = 2)
cstat1_opt <- op1$opt_ci %>% formatC(format = "f", digits = 3)
cstat1_est_adj <- formatC(cstat1$re$pred - op1$opt_ci, format = "f", digits = 2)

prog2 <- readRDS("results-share/originalModel_WITHOUT_PD_JAN2026_updated.rds")
cstat2 <- readRDS("results/summaryCIM2_withPD_ma.rds")
cal2 <- readRDS("data/forestCalSlopeM2.rds")
sum2 <- readRDS("results-share/202605_WITHOUT_PD_BOOT.rds")


cal2_est <- prog2$evalCal$result$poolCalSlopeAP_BRMS[1] %>% formatC(format = "f", digits = 2)
cal2_ll <- prog2$evalCal$result$poolCalSlopeAP_BRMS[4] %>% formatC(format = "f", digits = 2)
cal2_ul <- prog2$evalCal$result$poolCalSlopeAP_BRMS[5] %>% formatC(format = "f", digits = 2)
cal2_opt <- op2$opt_cal %>% formatC(format = "f", digits = 3)
cal2_est_adj <- formatC(prog2$evalCal$result$poolCalSlopeAP_BRMS[1] - op2$opt_cal, format = "f", digits = 2)

cstat2_est <- cstat2$re$pred %>% formatC(format = "f", digits = 2)
cstat2_ll <- cstat2$re$ci.lb %>% formatC(format = "f", digits = 2)
cstat2_ul <- cstat2$re$ci.ub %>% formatC(format = "f", digits = 2)
cstat2_opt <- op2$opt_ci %>% formatC(format = "f", digits = 3)
cstat2_est_adj <- formatC(cstat2$re$pred - op2$opt_ci, format = "f", digits = 2)

latex_text <- glue::glue("
\\begin{table}[htbp]
    \\begin{tabular}{@{}llll@{}}
        \\toprule
                                       & Estimate (95\\% CI)  & Average  & Optimism--adjusted \\\\\\
                                       &                      & optimism & performance       \\\\\\
        \\midrule
        \\textbf{Model: with PG}        &                      &          &                   \\\\\\
        \\hspace{1em} C-statistic       & <<cstat1_est>> (<<cstat1_ll>>--<<cstat1_ul>>)    & <<cstat1_opt>>    & <<cstat1_est_adj>>              \\\\\\
        \\hspace{1em} Calibration slope & <<cal1_est>> (<<cal1_ll>>--<<cal1_ul>>)          & <<cal1_opt>>    & <<cal1_est_adj>>              \\\\\\
        \\midrule
        \\textbf{Model: without PG}     &                      &          &                   \\\\\\
        \\hspace{1em} C-statistic       & <<cstat2_est>> (<<cstat2_ll>>--<<cstat2_ul>>)    & <<cstat2_opt>>    & <<cstat2_est_adj>>              \\\\\\
        \\hspace{1em} Calibration slope & <<cal2_est>> (<<cal2_ll>>--<<cal2_ul>>)          & <<cal2_opt>>    & <<cal2_est_adj>>              \\\\\\
        \\bottomrule
    \\end{tabular}
    \\caption{Apparent and optimism--adjusted performance measures. Abbreviations: CI: confidence interval; PG: parasite grade}
    \\label{tab:performance}
\\end{table}
", .open = "<<", .close = ">>")

latex_text

## MODEL COEFFICIENTS - MODEL 1
library(xtable)

prog1 %>% names()
prog1$varSelectRR$result %>% names()
df <- prog1$varSelectRR$result$pooledModel %>% summary()

colnames(df) <- c("term", "estimate", "std.error", "t", "df (effective)", "p value")
df <- df %>% mutate(
    term = case_when(
        term == "MB_COMBINEDs" ~ "Parasite grade",
        term == "DM_AGEs" ~ "Age",
        term == "ZZ_AGEs2" ~ "Age$^2$",
        term == "LB_BL_HGB_GRP3" ~ "Anaemia: Severe",
        term == "VL_DURATIONs" ~ "Fever duration",
        term == "TREAT_GRP4SDA" ~ "Treatment: SDA",
        term == "TREAT_GRP4OTHER" ~ "Treatment: Other",
        .default = term
    )
)

INT <- df %>%
    filter(term == "(Intercept)") %>%
    select(estimate) %>%
    pull()

print(
    xtable(
        df,
        caption = "Model coefficients \\textemdash\\ including parasite grade",
        label = "tab:model_coeff_with_pg",
        align = c("r", "l", "r", "r", "r", "r", "r"),
        digits = c(0, 0, 4, 4, 2, 2, 2),
        display = c("s", "s", "f", "f", "f", "f", "E")
    ),
    include.rownames = FALSE,
    booktabs = TRUE
)

# Unadjusted intercept (Sundar 2019)
op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 2, int_sf = FALSE)
INT + op1$interceptAdjustment
# Uniform shrinkage factor
op1$shrinkageFactor
# Adjusted intercept (average)
op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 1, int_sf = TRUE)
INT + op1$interceptAdjustment
# Adjusted intercept (Sundar 2019)
op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 2, int_sf = TRUE)
INT + op1$interceptAdjustment
# Pooled tau squared
prog1$varSelectRR$result$tau_squared %>% mean()
# Pooled ICC
prog1$varSelectRR$result$icc %>% mean()

## MODEL COEFFICIENTS - MODEL 2
library(xtable)

prog2 %>% names()
df <- prog2$varSelectRR$result$pooledModel %>% summary()

colnames(df) <- c("term", "estimate", "std.error", "t", "df (effective)", "p value")
df <- df %>% mutate(
    term = case_when(
        term == "DM_AGEs" ~ "Age",
        term == "ZZ_AGEs2" ~ "Age$^2$",
        term == "LB_BL_HGB_GRP3" ~ "Anaemia: Severe",
        term == "VL_DURATIONs" ~ "Fever duration",
        term == "TREAT_GRP4SDA" ~ "Treatment: SDA",
        term == "TREAT_GRP4OTHER" ~ "Treatment: Other",
        .default = term
    )
)

INT <- df %>%
    filter(term == "(Intercept)") %>%
    select(estimate) %>%
    pull()

print(
    xtable(
        df,
        caption = "Model coefficients \\textemdash\\ including parasite grade",
        label = "tab:model_coeff_with_pg",
        align = c("r", "l", "r", "r", "r", "r", "r"),
        digits = c(0, 0, 4, 4, 2, 2, 2),
        display = c("s", "s", "f", "f", "f", "f", "E")
    ),
    include.rownames = FALSE,
    booktabs = TRUE
)

# Unadjusted intercept (Sundar 2019)
op2 <- optimismParam(origModel = prog2, sumModel = sum2, action = 2, int_sf = FALSE)
INT + op2$interceptAdjustment
# Uniform shrinkage factor
op2$shrinkageFactor
# Adjusted intercept (average)
op2 <- optimismParam(origModel = prog2, sumModel = sum2, action = 1, int_sf = TRUE)
INT + op2$interceptAdjustment
# Adjusted intercept (Sundar 2019)
op2 <- optimismParam(origModel = prog2, sumModel = sum2, action = 2, int_sf = TRUE)
INT + op2$interceptAdjustment
# Pooled tau squared
prog2$varSelectRR$result$tau_squared %>% mean()
# Pooled ICC
prog2$varSelectRR$result$icc %>% mean()

# MODEL STABILITY
ms1 <- op1$summaryMeasures$terms %>%
    paste(collapse = ", ") %>%
    strsplit(split = ", ") %>%
    table() %>%
    sort(decreasing = TRUE)

ms1_df <- data.frame(terms = names(ms1), freq = as.numeric(ms1))

ms2 <- op2$summaryMeasures$terms %>%
    paste(collapse = ", ") %>%
    strsplit(split = ", ") %>%
    table() %>%
    sort(decreasing = TRUE)

ms2_df <- data.frame(terms = names(ms2), freq = as.numeric(ms2))

df <- left_join(ms1_df, ms2_df, by = "terms")

print(
    xtable(df, digits = c(0, 0, 0, 0)),
    include.rownames = FALSE,
    booktabs = TRUE
)
