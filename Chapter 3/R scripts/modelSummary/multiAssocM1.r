# create a figure showing the multivariable associations between the final predictors in the final models

rm(list = ls())
library(tidyverse)
library(mice)
library(patchwork)
source("R/evalModel/predictAverageRandomIntercept.r")
source("R/modelSummary/optimismParam.r")
scale_df <- readRDS("data/ads_impute_scale.rds")

# to calculate the predictions with 95% confidence intervals we need
# (i) row vector of predictor values x, length k = [1, age, age^2, fever_dur, ...]
# (ii) column vector of coefficients beta, length k = [intercept, beta_age, beta_age^2, beta_fever_dur, ...]
# (iii) sigma = kxk covariance matrix of the coefficients
# (iv) Var(lp) = x.sigma.x^T

# linear predictor for each data point is then lp = x^T.beta
# variance is x^T.sigma.x

# load model development object
prog <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")
sum <- readRDS("results-share/202605_WITH_PD_BOOT.rds")

opt <- optimismParam(origModel = prog, sumModel = sum, action = 1)
sf <- opt$shrinkageFactor

# prog <- readRDS("results-share/originalModel_486.rds")

#############################################
### (i) row vectors of predictor values x ###
#############################################

# median parasite density = 2 [MB_COMBINEDs = 1]
# median age = 18 years [DM_AGEs = (18 - 22.298955) / 15.4153550 = -0.2788749]
# median fever duration = 30 days [VL_DURATIONs = (log(30) - 3.550509) / 0.8693318 = -0.1717545]

# range of plotted values
## parasite density: 1 - 6 (0 to 5)
## age: 1 - 80 (-1.381671 to 3.743089)
## fever duration: 1 - 300 (-4.084182 to 2.476929)

df_summary <- readRDS("data/ads_summary.rds")
df_orig <- readRDS("data/ads_impute.rds")
df_scale <- readRDS("data/ads_impute_scale.rds")

length.out <- 1000

AGE_MEAN <- df_scale[df_scale$var == "DM_AGE", "means"]
AGE_SD <- df_scale[df_scale$var == "DM_AGE", "sd"]

FD_MEAN <- df_scale[df_scale$var == "VL_DURATIONs", "means"]
FD_SD <- df_scale[df_scale$var == "VL_DURATIONs", "sd"]

# varying parasite count
df1 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - AGE_MEAN) / AGE_SD,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - FD_MEAN) / FD_SD,
  MB_COMBINEDs = seq(1, 6, 1) - 1,
  TREAT_GRP4SDA = 1,
  TREAT_GRP4OTHER = 0,
  AGE = 18,
  FEVER_DUR = 30,
  PARA = seq(1, 6, 1),
  VAR = "MB_COMBINEDs"
)

# varying fever duration
df2 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - AGE_MEAN) / AGE_SD,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 1,
  TREAT_GRP4OTHER = 0,
  AGE = 18,
  FEVER_DUR = seq(1, 300, length.out = length.out),
  PARA = 2,
  VAR = "VL_DURATIONs"
) %>%
  mutate(
    VL_DURATIONs = (log(FEVER_DUR) - FD_MEAN) / FD_SD,
  )

# varying age
df3 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = seq((1 - AGE_MEAN) / AGE_SD, (80 - AGE_MEAN) / AGE_SD, length.out = length.out),
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - FD_MEAN) / FD_SD,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 1,
  TREAT_GRP4OTHER = 0,
  AGE = seq(1, 80, length.out = length.out),
  FEVER_DUR = 30,
  PARA = 2,
  VAR = "DM_AGEs and ZZ_AGEs2"
)

# varying anaemia severity group
df4 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - AGE_MEAN) / AGE_SD,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = c(0, 1),
  VL_DURATIONs = (log(30) - FD_MEAN) / FD_SD,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 1,
  TREAT_GRP4OTHER = 0,
  AGE = 18,
  FEVER_DUR = 30,
  PARA = 2,
  VAR = "LB_BL_HGB_GRP3"
)

# varying treatment severity group (SDA)
df5 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - AGE_MEAN) / AGE_SD,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - FD_MEAN) / FD_SD,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = c(0, 1),
  TREAT_GRP4OTHER = 0,
  AGE = 18,
  FEVER_DUR = 30,
  PARA = 2,
  VAR = "TREAT_SDA"
)

df6 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - AGE_MEAN) / AGE_SD,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - FD_MEAN) / FD_SD,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 0,
  TREAT_GRP4OTHER = c(0, 1),
  AGE = 18,
  FEVER_DUR = 30,
  PARA = 2,
  VAR = "TREAT_OTHER"
)

df <- bind_rows(df1, df2, df3, df4, df5, df6)
df <- df %>%
  select(INTERCEPT, DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, MB_COMBINEDs, TREAT_GRP4SDA, TREAT_GRP4OTHER, AGE, FEVER_DUR, PARA, VAR)

##########################################
### (ii) column vector of coefficients ###
##########################################

# extract unadjusted model coefficients
beta <- prog$varSelectRR$result$pooledModel$pool$estimate
# INTERCEPT, MB_COMBINEDs, DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, TREAT_GRP4SDA, TREAT_GRP4OTHER

terms <- prog$varSelectRR$result$pooledModel$pool$term
prog$varSelectRR$result$pooledModel$pool

# logistic recalibration to "VSGPDL" (Sundar 2019)
df_model <- complete(prog$mice$result_grp, action = "stack")
df_model <- df_model %>% filter(STUDYID == 16)

lp <- predictAverageRandomIntercept(df_model, prog$varSelectRR$result$pooledModel)
df_model[["lp"]] <- lp
df_model[["lp_shrink"]] <- lp * sf

m <- glm(
  data = df_model,
  formula = OUT_DC_RELAPSE ~ offset(lp_shrink),
  family = binomial
)

int_adj <- summary(m)$coefficients[1, "Estimate"]

# adjust coefficients from original model
beta <- beta * sf
beta[1] <- beta[1] + int_adj

######################################
### (iii) pooled covariance matrix ###
######################################

models <- prog$varSelectRR$result$finalModels
m <- length(models) # number of imputations (20)

# vw is the within imputation covariance matrix
vw <- Reduce("+", lapply(models, vcov)) / (m)

# qbar are the pooled parameter estimates
qbar <- getqbar(prog$varSelectRR$result$pooledModel) # = beta

# qhat are the imputation specific estimates
prog$varSelectRR$result %>% names()
qhats <- sapply(models, function(m) summary(m)$coefficients[, 1])

# can then calculate the between imputation covariance matrix
vb <- (1 / (m - 1)) * (qhats - qbar) %*% t(qhats - qbar)

# total between imputation covariance matrix
vt <- vw + (1 + 1 / (m)) * vb

# scaled vt
vt <- sf^2 * vt

###################################################################
### (iv) prepare linear predictors and 95% confidence intervals ###
###################################################################

# linear predictor
terms <- as.character(terms)
terms[1] <- "INTERCEPT"
df_matrix <- df %>%
  select(as.character(terms)) %>%
  as.matrix()

# df_matrix
beta %>% length()

lp <- df_matrix %*% beta

se <- numeric()
for (i in seq_len(nrow(df_matrix))) {
  se[i] <- sqrt(t(df_matrix[i, ]) %*% vt %*% df_matrix[i, ])
}

se %>% length()
df %>% nrow()
lp %>% length()

df <- df %>%
  mutate(
    LP = lp,
    SE = se,
    UCI = lp + 1.96 * SE,
    LCI = lp - 1.96 * SE,
    prob = plogis(LP),
    prob_UCI = plogis(UCI),
    prob_LCI = plogis(LCI)
  )

##################################
## PRODUCE PLOTS FOR MANUSCRIPT ##
##################################

# age
age <- df %>%
  filter(VAR == "DM_AGEs and ZZ_AGEs2") %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = AGE,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    alpha = 0.3,
    fill = "#b44949"
  ) +
  geom_line(
    aes(
      x = AGE,
      y = prob
    ),
    colour = "#000000"
  ) +
  coord_cartesian(
    xlim = c(0, 70),
    ylim = c(0, 0.12)
  ) +
  scale_x_continuous(
    name = ""
  ) +
  scale_y_continuous(
    name = "Relapse probability (%)",
    breaks = c(0, 0.05, 0.10, 0.15),
    minor_breaks = seq(0, 0.15, by = 0.01),
    labels = c("0", "5", "10", "15")
  ) +
  ggtitle("Age (years)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# fever duration
fever_dur <- df %>%
  filter(VAR == "VL_DURATIONs") %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = FEVER_DUR,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    alpha = 0.3,
    fill = "#ca4d4d"
  ) +
  geom_line(
    aes(
      x = FEVER_DUR,
      y = prob
    )
  ) +
  coord_cartesian(
    xlim = c(2, 300),
    ylim = c(0, 0.12)
  ) +
  scale_x_log10(
    name = "",
    breaks = c(seq(1, 9, 1), seq(10, 90, 10), seq(100, 300, 100)),
    labels = c("", "", "3", "", "", "", "", "", "", "10", "", "30", "", "", "", "", "", "", "100", "", "300")
  ) +
  scale_y_continuous(
    name = "Relapse probability (%)",
    breaks = c(0, 0.05, 0.10, 0.15),
    minor_breaks = seq(0, 0.15, by = 0.01),
    labels = c("0", "5", "10", "15")
  ) +
  ggtitle("Fever duration (days, log scale)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# parasite count
para <- df %>%
  filter(VAR == "MB_COMBINEDs") %>%
  ggplot() +
  geom_errorbar(
    aes(
      x = PARA,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    width = 0.05
  ) +
  geom_ribbon(
    aes(
      x = PARA,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    alpha = 0.3,
    fill = "#ca4d4d"
  ) +
  geom_line(
    aes(
      x = PARA,
      y = prob
    ),
    linetype = "dashed"
  ) +
  geom_point(
    aes(
      x = PARA,
      y = prob
    ),
    shape = 21,
    fill = "#0a0266"
  ) +
  coord_cartesian(
    xlim = c(1, 6),
    ylim = c(0, 0.12)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = c("1+", "2+", "3+", "4+", "5+", "6+")
  ) +
  scale_y_continuous(
    name = "Relapse probability (%)",
    breaks = c(0, 0.05, 0.10, 0.15),
    minor_breaks = seq(0, 0.15, by = 0.01),
    labels = c("0", "5", "10", "15")
  ) +
  ggtitle("Parasite grade") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# anaemia severity
anaemia <- df %>%
  filter(VAR == "LB_BL_HGB_GRP3") %>%
  ggplot() +
  geom_errorbar(
    aes(
      x = LB_BL_HGB_GRP3,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    width = 0.03
  ) +
  geom_point(
    aes(
      x = LB_BL_HGB_GRP3,
      y = prob
    ),
    shape = 21,
    fill = "#0a0266"
  ) +
  coord_cartesian(
    xlim = c(-0.5, 1.5),
    ylim = c(0, 0.12)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(0, 1),
    labels = c("Non-severe", "Severe")
  ) +
  scale_y_continuous(
    name = "Relapse probability (%)",
    breaks = c(0, 0.05, 0.10, 0.15),
    minor_breaks = seq(0, 0.15, by = 0.01),
    labels = c("0", "5", "10", "15")
  ) +
  ggtitle("Anaemia severity") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# treatment
df <- df %>%
  mutate(
    TREAT = case_when(
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 0 ~ 0,
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 1 ~ 1,
      VAR == "TREAT_OTHER" & TREAT_GRP4OTHER == 1 ~ 2,
      .default = -1
    )
  )

treat <- df %>%
  filter(VAR %in% c("TREAT_SDA", "TREAT_OTHER")) %>%
  ggplot() +
  geom_errorbar(
    aes(
      x = TREAT,
      ymin = prob_LCI,
      ymax = prob_UCI
    ),
    width = 0.03
  ) +
  geom_point(
    aes(
      x = TREAT,
      y = prob
    ),
    shape = 21,
    fill = "#0a0266"
  ) +
  coord_cartesian(
    xlim = c(-0.25, 2.25),
    ylim = c(0, 0.12)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(0, 1, 2),
    labels = c("28-day MF", "Single-dose LAmB", "Other")
  ) +
  scale_y_continuous(
    name = "Relapse probability (%)",
    breaks = c(0, 0.05, 0.10, 0.15),
    minor_breaks = seq(0, 0.15, by = 0.01),
    labels = c("0", "5", "10", "15")
  ) +
  ggtitle("Treatment") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# align plots
p_list <- list(fever_dur, para, treat, anaemia)

# remove y-axis text and ticks from other plots
p_list <- lapply(p_list, function(p) {
  p + theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 7, face = "bold"),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major.y = element_line(color = "grey85", size = 0.3),
    panel.grid.minor.y = element_line(color = "grey92", size = 0.2)
  )
})

age <- age + theme(
  plot.title = element_text(size = 7, face = "bold"),
  axis.title = element_text(size = 6),
  axis.text = element_text(size = 6),
  panel.grid.major.y = element_line(color = "grey85", size = 0.3),
  panel.grid.minor.y = element_line(color = "grey92", size = 0.2)
)

# Combine all
library(cowplot)

final_plot <- plot_grid(
  age,
  p_list[[1]],
  p_list[[3]],
  p_list[[4]],
  p_list[[2]],
  nrow = 1,
  rel_widths = c(1.2, 1, 1, 1, 1)
)

x <- 1.3
ggsave(filename = "figures/multiAssocM1.pdf", plot = final_plot, width = 20 * x, height = 5 * x, dpi = 1200, units = "cm")
