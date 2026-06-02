###################################################
# Create calibration plots for groups of interest #
###################################################

rm(list = ls())
library(tidyverse)
library(mice)
library(lme4)
library(mgcv)
library(cowplot)
library(binom)
source("R/evalModel/predictAverageRandomIntercept.r")
source("R/modelSummary/optimismParam.r")

# Methodology for producing calibration plots with both clustering and multiple imputations

# load model objects
# prog1 <- readRDS("results-share/originalModel_207.rds")
prog1 <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")
sum1 <- readRDS("results-share/202605_WITH_PD_BOOT.rds")

# prog2 <- readRDS("results-share/originalModel_486.rds")
prog2 <- readRDS("results-share/originalModel_WITHOUT_PD_JAN2026_updated.rds")
sum2 <- readRDS("results-share/202605_WITHOUT_PD_BOOT.rds")

# STEP 1: Combine all 30 imputed datasets into one long dataset

data1 <- prog1$mice$result_grp
df1 <- complete(data1, action = "long")

data2 <- prog2$mice$result_grp
df2 <- complete(data2, action = "long")

# STEP 2: Calculate linear predictor across imputed datasets (ignoring clustering term for now) and attach to stacked dataset

op1 <- optimismParam(origModel = prog1, sumModel = sum1, action = 2)
op2 <- optimismParam(origModel = prog2, sumModel = sum2, action = 2)

modelSummary1 <- prog1$varSelectRR$result$pooledModel
lp1 <- predictAverageRandomIntercept(
  df1, modelSummary1,
  sf = 1, # op1$shrinkageFactor,
  int_adj = op1$interceptAdjustment
)
df1[["lp"]] <- lp1
mean(lp1)
sd(lp1)

modelSummary2 <- prog2$varSelectRR$result$pooledModel
lp2 <- predictAverageRandomIntercept(
  df2, modelSummary2,
  sf = 1, # op2$shrinkageFactor,
  int_adj = op2$interceptAdjustment
)
df2[["lp"]] <- lp2
mean(lp2)
sd(lp2)

# STEP 3: Fit random intercept, optimism-adjusted model across stacked imputed datasets with linear predictor as an offset to allow recalibration of intercepts to each cluster

# fit optimism adjusted model (intercept adjusted) [fitted intercept now zero as anticipated]
m_adj1 <- glmer(
  data = df1,
  formula = OUT_DC_RELAPSE ~ (1 | STUDYID) + offset(lp),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

m_adj2 <- glmer(
  data = df2,
  formula = OUT_DC_RELAPSE ~ (1 | STUDYID) + offset(lp),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# STEP 4: Make predictions from intercept-adjusted random intercept model
df_joined1 <- df1 %>%
  mutate(
    test_lp = predict(m_adj1, newdata = df1, type = "link", re.form = NULL),
    test_prob = plogis(test_lp)
  )

df_joined2 <- df2 %>%
  mutate(
    test_lp = predict(m_adj2, newdata = df2, type = "link", re.form = NULL),
    test_prob = plogis(test_lp)
  )

saveRDS(df_joined1, file = "data/calM1Predictions.rds")
saveRDS(df_joined2, file = "data/calM2Predictions.rds")

######################
# PLOT DECILES OF LP #
######################

# calculate expected and mean probabilities by probability (linear predictor) deciles
df_joined_sum1 <- df_joined1 %>%
  mutate(
    lp_decile = ntile(test_lp, 10),
    prob = plogis(test_lp)
  ) %>%
  group_by(lp_decile) %>%
  summarise(
    mean_expected_prob = mean(prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / prog1$mice$result_grp$m
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  mutate(
    lp_decile = ntile(test_lp, 10),
    prob = plogis(test_lp)
  ) %>%
  group_by(lp_decile) %>%
  summarise(
    mean_expected_prob = mean(prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / prog2$mice$result_grp$m
  ) %>%
  ungroup()

# add Wilson intervals where effective sample size is adjusted for stacking the multiple imputations
bin1 <- binom.confint(
  df_joined_sum1$observed_prob * df_joined_sum1$effective_n,
  df_joined_sum1$effective_n,
  conf.level = 0.95,
  method = "wilson"
)

bin2 <- binom.confint(
  df_joined_sum2$observed_prob * df_joined_sum2$effective_n,
  df_joined_sum2$effective_n,
  conf.level = 0.95,
  method = "wilson"
)

cplot1 <- cbind(df_joined_sum1, bin1) %>% select(
  lp_decile, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  lp_decile, mean_expected_prob, observed_prob, lower, upper
)

# Fit a GAM model to the data in preparation for calibration plot
# LOESS is computationally expensive with so many data points
gam_model1 <- gam(
  formula = OUT_DC_RELAPSE ~ s(test_prob, bs = "cs"),
  method = "REML",
  data = df_joined1,
  family = "binomial"
)

gam_model2 <- gam(
  formula = OUT_DC_RELAPSE ~ s(test_prob, bs = "cs"),
  method = "REML",
  data = df_joined2,
  family = "binomial"
)

newdata1 <- data.frame(test_prob = seq(0, 1, length.out = 1000))
pred_gam1 <- predict(gam_model1, newdata = newdata1, se.fit = TRUE)
newdata1 <- newdata1 %>%
  mutate(
    gam_predict = pred_gam1$fit,
    gam_se = pred_gam1$se.fit,
    gam_lower = gam_predict - sqrt(30) * 1.96 * gam_se,
    gam_upper = gam_predict + sqrt(30) * 1.96 * gam_se,
    gam_predict_logit = plogis(gam_predict),
    gam_predict_logit_lower = plogis(gam_lower),
    gam_predict_logit_upper = plogis(gam_upper)
  )

newdata2 <- data.frame(test_prob = seq(0, 1, length.out = 1000))
pred_gam2 <- predict(gam_model2, newdata = newdata2, se.fit = TRUE)
newdata2 <- newdata2 %>%
  mutate(
    gam_predict = pred_gam2$fit,
    gam_se = pred_gam2$se.fit,
    gam_lower = gam_predict - sqrt(30) * 1.96 * gam_se,
    gam_upper = gam_predict + sqrt(30) * 1.96 * gam_se,
    gam_predict_logit = plogis(gam_predict),
    gam_predict_logit_lower = plogis(gam_lower),
    gam_predict_logit_upper = plogis(gam_upper)
  )

# create calibration plot

breaks <- seq(0, 1, length.out = 100)

df_counts1 <- df_joined1 %>%
  mutate(
    bin = cut(
      test_prob,
      breaks = breaks,
      include.lowest = TRUE
    ),
    x = as.numeric(bin) / (2 * length(breaks))
  ) %>%
  group_by(OUT_DC_RELAPSE, bin, x) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(OUT_DC_RELAPSE) %>%
  mutate(
    n_adj = n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    n = ifelse(!OUT_DC_RELAPSE, -n, n),
    n_adj = ifelse(!OUT_DC_RELAPSE, -n_adj, n_adj)
  )

df_counts2 <- df_joined2 %>%
  mutate(
    bin = cut(
      test_prob,
      breaks = breaks,
      include.lowest = TRUE
    ),
    x = as.numeric(bin) / (2 * length(breaks))
  ) %>%
  group_by(OUT_DC_RELAPSE, bin, x) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(OUT_DC_RELAPSE) %>%
  mutate(
    n_adj = n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    n = ifelse(!OUT_DC_RELAPSE, -n, n),
    n_adj = ifelse(!OUT_DC_RELAPSE, -n_adj, n_adj)
  )

y_int <- 0.2

cal1 <- ggplot() +
  geom_linerange(
    data = df_counts1,
    aes(x = x, ymin = y_int, ymax = n_adj / 5 + y_int),
    alpha = 0.4,
    position = "identity"
  ) +
  geom_hline(aes(yintercept = y_int), colour = "#686868", alpha = 0.3) +
  geom_ribbon(
    data = newdata1,
    aes(x = test_prob, ymin = gam_predict_logit_lower, ymax = gam_predict_logit_upper),
    fill = "lightblue",
    colour = "#90cee2",
    alpha = 1
  ) +
  geom_line(
    data = newdata1,
    aes(x = test_prob, y = gam_predict_logit),
    colour = "#4794ae",
    linetype = 3
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_point(
    data = cplot1,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.25),
    ylim = c(0, 0.25)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_line()
  )


cal2 <- ggplot() +
  geom_linerange(
    data = df_counts2,
    aes(x = x, ymin = y_int, ymax = n_adj / 5 + y_int),
    alpha = 0.4,
    position = "identity"
  ) +
  geom_hline(aes(yintercept = y_int), colour = "#686868", alpha = 0.3) +
  geom_ribbon(
    data = newdata2,
    aes(x = test_prob, ymin = gam_predict_logit_lower, ymax = gam_predict_logit_upper),
    fill = "lightblue",
    colour = "#90cee2",
    alpha = 1
  ) +
  geom_line(
    data = newdata2,
    aes(x = test_prob, y = gam_predict_logit),
    colour = "#4794ae",
    linetype = 3
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_point(
    data = cplot2,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  geom_errorbar(
    data = cplot2,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.25),
    ylim = c(0, 0.25)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_line()
  )

cal <- plot_grid(cal1, cal2 + theme(axis.title.y = element_blank()), nrow = 1)
x <- 0.8
ggsave(
  filename = "graphs/calPlot.pdf", plot = cal, width = 12.15 * x, height = 12.15 * x / 2, dpi = 600
)
