# this file presents the odds ratios of the different variables in a 'forest plot' format
library(tidyverse)
library(ggtext)
library(mice)

#############
## WITH PD ##
#############

sc <- readRDS("data/ads_impute_scale.rds")
FD_SD <- sc[sc$var == "VL_DURATIONs", "sd"]

# import the model
m1 <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")
# m2 <- readRDS("results-share/originalModel_486.rds")

# extract pooled model
model <- m1$varSelectRR$result$pooledModel$pool
summary(m1$varSelectRR$result$pooledModel)

# calculate confidence intervals on estimates
model <- model %>%
  select(term, est = estimate, t) %>%
  mutate(
    se = sqrt(t),
    est_upper = est + 1.96 * se,
    est_lower = est - 1.96 * se,
  ) %>%
  mutate( # convert fever duration coefficient such that it represents the change in log odds per doubling of fever duration (e.g. 15 to 30 days, 30 to 60 days)
    est = ifelse(term == "VL_DURATIONs", est * log(2) / FD_SD, est),
    est_upper = ifelse(term == "VL_DURATIONs", est_upper * log(2) / FD_SD, est_upper),
    est_lower = ifelse(term == "VL_DURATIONs", est_lower * log(2) / FD_SD, est_lower)
  ) %>%
  mutate(
    estr = exp(est),
    estr_lower = exp(est_lower),
    estr_upper = exp(est_upper),
    p = 2 * pnorm(-abs(est / se))
  ) %>%
  select(-t) %>%
  mutate(
    term = factor(term, levels = rev(c("DM_AGEs", "ZZ_AGEs2", "VL_DURATIONs", "MB_COMBINEDs", "TREAT_GRP4SDA", "TREAT_GRP4OTHER", "LB_BL_HGB_GRP3")))
  )

##########
## PLOT ##
##########

p1 <- model %>%
  filter(term != "(Intercept)") %>%
  ggplot() +
  labs(
    title = "With parasite grade"
  ) +
  geom_pointrange(
    aes(
      y = term,
      x = estr,
      xmin = estr_lower,
      xmax = estr_upper
    )
  ) +
  scale_x_log10(
    minor_breaks = seq(0.1, 2, 0.1),
    breaks = c(0.5, 1, 1.5, 2),
    name = "Odds ratio"
  ) +
  scale_y_discrete(
    name = "Final predictors",
    labels = c(
      "<strong>Severe anaemia</strong><br> <span style='color:#808080;'>Reference: Non-severe </span>",
      "<strong>Treatment: Other</strong><br> <span style='color:#808080;'>Reference: Miltefosine </span>",
      "<strong>Treatment: SDA</strong><br> <span style='color:#808080;'>Reference: Miltefosine </span>",
      "<strong>Parasite grade</strong><br> <span style='color:#808080;'>Per 1+ increase</span>",
      "<strong>Fever duration</strong><br> <span style='color:#808080;'>Per 2-fold increase</span>",
      "<strong>Age<sup>2</sup></strong><br> <span style='color:#808080;'>Interpretation: see text</span>",
      "<strong>Age</strong><br> <span style='color:#808080;'>Interpretation: see text</span>"
    )
  ) +
  geom_vline(xintercept = 1.0, color = "red") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.y = element_markdown(hjust = 1, lineheight = 1.3),
    axis.title = element_markdown(),
    plot.title = element_text(size = 12),
    axis.ticks.y = element_blank()
  ) +
  coord_cartesian(xlim = c(0.2, 1.6))

x <- 0.8
ggsave(filename = "results/var_forest_with_pd.pdf", plot = p1, width = 7 * x, height = 9 * x, dpi = 600)


################
## WITHOUT PD ##
################

# rm(list = ls())
sc <- readRDS("data/ads_impute_scale.rds")
FD_SD <- sc[sc$var == "VL_DURATIONs", "sd"]

# import the model
# m1 <- readRDS("results-share/originalModel_WITH_PD_JAN2026_updated.rds")
m2 <- readRDS("results-share/originalModel_WITHOUT_PD_JAN2026_updated.rds")

# extract pooled model
model <- m2$varSelectRR$result$pooledModel$pool
summary(m2$varSelectRR$result$pooledModel)

# calculate confidence intervals on estimates
model <- model %>%
  select(term, est = estimate, t) %>%
  mutate(
    se = sqrt(t),
    est_upper = est + 1.96 * se,
    est_lower = est - 1.96 * se,
  ) %>%
  mutate( # convert fever duration coefficient such that it represents the change in log odds per doubling of fever duration (e.g. 15 to 30 days, 30 to 60 days)
    est = ifelse(term == "VL_DURATIONs", est * log(2) / FD_SD, est),
    est_upper = ifelse(term == "VL_DURATIONs", est_upper * log(2) / FD_SD, est_upper),
    est_lower = ifelse(term == "VL_DURATIONs", est_lower * log(2) / FD_SD, est_lower)
  ) %>%
  mutate(
    estr = exp(est),
    estr_lower = exp(est_lower),
    estr_upper = exp(est_upper),
    p = 2 * pnorm(-abs(est / se))
  ) %>%
  select(-t) %>%
  mutate(
    term = factor(term, levels = rev(c("DM_AGEs", "ZZ_AGEs2", "VL_DURATIONs", "MB_COMBINEDs", "TREAT_GRP4SDA", "TREAT_GRP4OTHER", "LB_BL_HGB_GRP3")))
  )

##########
## PLOT ##
##########

p2 <- model %>%
  filter(term != "(Intercept)") %>%
  ggplot() +
  labs(
    title = "Without parasite grade"
  ) +
  geom_pointrange(
    aes(
      y = term,
      x = estr,
      xmin = estr_lower,
      xmax = estr_upper
    )
  ) +
  scale_x_log10(
    minor_breaks = seq(0.1, 2, 0.1),
    breaks = c(0.5, 1, 1.5, 2),
    name = "Odds ratio"
  ) +
  scale_y_discrete(
    drop = FALSE,
    name = "Final predictors",
    labels = c(
      "<strong>Severe anaemia</strong><br> <span style='color:#808080;'>Reference: Non-severe </span><br><span style='color:#00008B;'>p = 0.069 </span>",
      "<strong>Treatment: Other</strong><br> <span style='color:#808080;'>Reference: Miltefosine </span><br><span style='color:#00008B;'>p = 0.017 </span>",
      "<strong>Treatment: SDA</strong><br> <span style='color:#808080;'>Reference: Miltefosine </span><br><span style='color:#00008B;'>p = 0.028 </span>",
      "<strong>Parasite grade</strong><br> <span style='color:#C21807;'>Omitted from model</span>",
      "<strong>Fever duration</strong><br> <span style='color:#808080;'>Per 2-fold increase</span><br><span style='color:#00008B;'>p < 0.0001 </span>",
      "<strong>Age<sup>2</sup></strong><br> <span style='color:#808080;'>Interpretation: see text</span><br><span style='color:#00008B;'>p = 0.027 </span>",
      "<strong>Age</strong><br> <span style='color:#808080;'>Interpretation: see text</span><br><span style='color:#00008B;'>p = 0.026 </span>"
    )
  ) +
  geom_vline(xintercept = 1.0, color = "red") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.title = element_markdown(),
    plot.title = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_cartesian(xlim = c(0.2, 1.6))

x <- 0.8
ggsave(filename = "results/var_forest_without_pd.pdf", plot = p2, width = 7 * x, height = 9 * x, dpi = 600)

##################
# COMBINED PLOTS #
##################

library(patchwork)
out <- p1 + p2 + plot_layout(widths = c(2, 2))

x <- 0.7
ggsave(filename = "results/var_forest_combined.pdf", plot = out, width = 13 * x, height = 6 * x, dpi = 600)
