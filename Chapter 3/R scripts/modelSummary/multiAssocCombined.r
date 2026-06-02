# create a figure showing the multivariable associations between the final predictors in the final models

rm(list = ls())
library(tidyverse)
library(cowplot)
library(mice)
library(patchwork)

source("R/evalModel/predictAverageRandomIntercept.r")
source("R/modelSummary/optimismParam.r")

# to calculate the predictions with 95% confidence intervals we need
# (i) row vector of predictor values x, length k = [1 intercept), age, age^2, fever_dur, ...]
# (ii) column vector of coefficients beta, length k = [intercept, beta_age, beta_age^2, beta_fever_dur, ...]
# (iii) k x k covariance matrix of the coefficients (sigma)
# (iv) Var(lp) = x.sigma.x^T

# linear predictor for each data point: lp = x^T.beta
# variance is x^T.sigma.x

# load model objects
prog1 <- readRDS("results-share/20250701_valOrig_withPD/originalModel_1.rds")
sum1 <- readRDS("results-share/20250701_WITH_PD.rds")

prog2 <- readRDS("results-share/20250701_valOrig_withoutPD/originalModel_2.rds")
sum2 <- readRDS("results-share/20250701_WITHOUT_PD.rds")

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

df_orig <- readRDS("data/ads_clean.rds")
df_orig <- readRDS("data/ads_impute.rds")
df_scale <- readRDS("data/ads_impute_scale.rds")

length.out <- 1000
# varying parasite count
df1 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = (18 - 22.298955) / 15.4153550,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - 3.550509) / 0.8693318,
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
  DM_AGEs = (18 - 22.298955) / 15.4153550,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(seq(1, 300, length.out = length.out)) - 3.550509) / 0.8693318,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 1,
  TREAT_GRP4OTHER = 0,
  AGE = 18,
  FEVER_DUR = seq(1, 300, length.out = length.out),
  PARA = 2,
  VAR = "VL_DURATIONs"
)

# varying age
df3 <- tibble(
  INTERCEPT = 1,
  DM_AGEs = seq((1 - 22.298955) / 15.4153550, (80 - 22.298955) / 15.4153550, length.out = length.out),
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - 3.550509) / 0.8693318,
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
  DM_AGEs = (18 - 22.298955) / 15.4153550,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = c(0, 1),
  VL_DURATIONs = (log(30) - 3.550509) / 0.8693318,
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
  DM_AGEs = (18 - 22.298955) / 15.4153550,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - 3.550509) / 0.8693318,
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
  DM_AGEs = (18 - 22.298955) / 15.4153550,
  ZZ_AGEs2 = DM_AGEs^2,
  LB_BL_HGB_GRP3 = 1,
  VL_DURATIONs = (log(30) - 3.550509) / 0.8693318,
  MB_COMBINEDs = 2 - 1,
  TREAT_GRP4SDA = 0,
  TREAT_GRP4OTHER = c(0, 1),
  AGE = 18,
  FEVER_DUR = 30,
  PARA = 2,
  VAR = "TREAT_OTHER"
)

df <- bind_rows(df1, df2, df3, df4, df5, df6)
df1 <- df %>%
  select(INTERCEPT, DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, MB_COMBINEDs, TREAT_GRP4SDA, TREAT_GRP4OTHER, AGE, FEVER_DUR, PARA, VAR)

df2 <- df %>%
  select(INTERCEPT, DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, TREAT_GRP4SDA, TREAT_GRP4OTHER, AGE, FEVER_DUR, PARA, VAR)

##########################################
### (ii) column vector of coefficients ###
##########################################

# extract unadjusted model coefficients
beta1 <- prog1$varSelectRR$result$pooledModel$pool$estimate
beta2 <- prog2$varSelectRR$result$pooledModel$pool$estimate


# extract calibration slope optimism (shrinkage factor)
op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 2)
op2 <- optimismParam(origModel = prog2, sumModel = sum2, action = 2)

# adjust coefficients from original model
beta1 <- beta1 * op1[[1]]
beta1[1] <- beta1[1] + op1[[2]]

beta2 <- beta2 * op2[[1]]
beta2[1] <- beta2[1] + op2[[2]]

######################################
### (iii) pooled covariance matrix ###
######################################

### model 1
models1 <- prog1$varSelectRR$result$finalModels
m1 <- length(models1) # number of imputations (20)

# vw is the within imputation covariance matrix
vw1 <- Reduce("+", lapply(models1, vcov)) / (m1)

# qbar are the pooled parameter estimates
qbar1 <- getqbar(prog1$varSelectRR$result$pooledModel) # = beta

# qhat are the imputation specific estimates
prog1$varSelectRR$result %>% names()
qhats1 <- sapply(models1, function(m) summary(m)$coefficients[, 1])

# can then calculate the between imputation covariance matrix
vb1 <- (1 / (m1 - 1)) * (qhats1 - qbar1) %*% t(qhats1 - qbar1)

# total between imputation covariance matrix
vt1 <- vw1 + (1 + 1 / (m1)) * vb1

# scaled vt
vt1 <- op1[[1]]^2 * vt1

### model 2
models2 <- prog2$varSelectRR$result$finalModels
m2 <- length(models2) # number of imputations (20)

# vw is the within imputation covariance matrix
vw2 <- Reduce("+", lapply(models2, vcov)) / (m2)

# qbar are the pooled parameter estimates
qbar2 <- getqbar(prog2$varSelectRR$result$pooledModel) # = beta

# qhat are the imputation specific estimates
prog2$varSelectRR$result %>% names()
qhats2 <- sapply(models2, function(m2) summary(m2)$coefficients[, 1])

# can then calculate the between imputation covariance matrix
vb2 <- (1 / (m2 - 1)) * (qhats2 - qbar2) %*% t(qhats2 - qbar2)

# total between imputation covariance matrix
vt2 <- vw2 + (1 + 1 / (m2)) * vb2

# scaled vt
vt2 <- op2[[1]]^2 * vt2

###################################################################
### (iv) prepare linear predictors and 95% confidence intervals ###
###################################################################

### model 1
df_matrix1 <- df1 %>%
  select(-c(VAR, AGE, FEVER_DUR, PARA)) %>%
  as.matrix()

beta1 %>% length()

lp1 <- df_matrix1 %*% beta1

se1 <- numeric()
for (i in seq_len(nrow(df_matrix1))) {
  se1[i] <- sqrt(t(df_matrix1[i, ]) %*% vt1 %*% df_matrix1[i, ])
}

df1 <- df1 %>%
  mutate(
    LP = lp1,
    SE = se1,
    UCI = lp1 + 1.96 * SE,
    LCI = lp1 - 1.96 * SE,
    prob = plogis(LP),
    prob_UCI = plogis(UCI),
    prob_LCI = plogis(LCI)
  )

### model 2
df_matrix2 <- df2 %>%
  select(-c(VAR, AGE, FEVER_DUR, PARA)) %>%
  as.matrix()

lp2 <- df_matrix2 %*% beta2

se2 <- numeric()
for (i in seq_len(nrow(df_matrix2))) {
  se2[i] <- sqrt(t(df_matrix2[i, ]) %*% vt2 %*% df_matrix2[i, ])
}

df2 <- df2 %>%
  mutate(
    LP = lp2,
    SE = se2,
    UCI = lp2 + 1.96 * SE,
    LCI = lp2 - 1.96 * SE,
    prob = plogis(LP),
    prob_UCI = plogis(UCI),
    prob_LCI = plogis(LCI)
  )

##################################
## PRODUCE PLOTS FOR MANUSCRIPT ##
##################################

# age
age1 <- df1 %>%
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
    ylim = c(0, 0.15)
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

age2 <- df2 %>%
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
    ylim = c(0, 0.15)
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
fever_dur1 <- df1 %>%
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
    ylim = c(0, 0.15)
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

fever_dur2 <- df2 %>%
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
    ylim = c(0, 0.15)
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
para1 <- df1 %>%
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
    ylim = c(0, 0.15)
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
  ggtitle("Parasite count") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # removes all grid lines
    panel.border = element_blank(), # optional: removes panel border
    axis.line = element_line(), # keeps axis lines (x and y axes)
    axis.ticks = element_line() # Keep tick marks
  )

# anaemia severity
anaemia1 <- df1 %>%
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
    ylim = c(0, 0.15)
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

anaemia2 <- df2 %>%
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
    ylim = c(0, 0.15)
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
df1 <- df1 %>%
  mutate(
    TREAT = case_when(
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 0 ~ 0,
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 1 ~ 1,
      VAR == "TREAT_OTHER" & TREAT_GRP4OTHER == 1 ~ 2,
      .default = -1
    )
  )

df2 <- df2 %>%
  mutate(
    TREAT = case_when(
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 0 ~ 0,
      VAR == "TREAT_SDA" & TREAT_GRP4SDA == 1 ~ 1,
      VAR == "TREAT_OTHER" & TREAT_GRP4OTHER == 1 ~ 2,
      .default = -1
    )
  )

treat1 <- df1 %>%
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
    ylim = c(0, 0.15)
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


treat2 <- df2 %>%
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
    ylim = c(0, 0.15)
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
p_list1 <- list(fever_dur1, anaemia1, treat1, para1)
p_list2 <- list(fever_dur2, anaemia2, treat2)


# remove y-axis text and ticks from other plots
p_list1 <- lapply(p_list1, function(p) {
  p + theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 7, face = "bold"),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  )
})

p_list2 <- lapply(p_list2, function(p) {
  p + theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  )
})

age1 <- age1 + theme(
  plot.title = element_text(size = 7, face = "bold"),
  axis.title = element_text(size = 6),
  axis.text = element_text(size = 6)
  # axis.text.x = element_blank()
)

age2 <- age2 + theme(
  plot.title = element_blank(),
  axis.title = element_text(size = 6),
  axis.text = element_text(size = 6)
)

# Combine all
multivar <- plot_grid(age1, p_list1[[1]], p_list1[[2]], p_list1[[3]], p_list1[[4]], age2, p_list2[[1]], p_list2[[2]], p_list2[[3]], nrow = 2)
ggsave(filename = "graphs/multiAssocCombined.png", plot = multivar, width = 20 * 1.2, height = 8 * 1.2, dpi = 300, units = "cm")
