## plot continuous distributions overall, for outcome, and association with outcome

rm(list = ls())

library(tidyverse)
library(ggridges)
library(patchwork)
library(ggtext)
library(mgcv)

df <- readRDS("data/ads_summary.rds")

# start with age

width <- 5 * 0.9
height <- 1 * 0.9

alpha <- 0.3
out_col <- c("green", "red")
ov_col <- "grey"

# overall
age_overall <- df %>% ggplot() +
  geom_density(
    aes(x = AGE),
    fill = ov_col
  ) +
  scale_x_continuous(
    name = "Age (years)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/age_overall.pdf", age_overall, width = width, height = height)

# by outcome

age_out <- df %>% ggplot() +
  geom_density(
    aes(x = AGE, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_continuous(
    name = "Age (years)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/age_out.pdf", age_out, width = width, height = height)

# GAM
age_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = AGE, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Age (years)",
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(AGE), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(AGE = seq(min(df2$AGE, na.rm = TRUE),
  max(df2$AGE, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
age_logodds <- ggplot(newdata, aes(x = AGE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "Age",
    y = "log(odds) of relapse"
    # title = "Estimated log-odds of outcome across age (95% CI)"
  ) +
  theme_minimal()

# combine the plots

age_overall <- age_overall + theme(axis.title.x = element_blank())
age_out <- age_out + theme()
age_gam <- age_gam #+ theme(axis.text.y = element_blank(), axis.title.y = element_blank())


age_comb <- (age_overall / age_out) | age_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/age_comb.pdf", age_comb, width = 2 * width, height = 2 * height)


## HEIGHT ##

# overall
height_overall <- df %>% ggplot() +
  geom_density(
    aes(x = HEIGHT),
    fill = ov_col
  ) +
  scale_x_continuous(
    name = "Height (cm)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/height_overall.pdf", age_overall, width = width, height = height)

# by outcome

height_out <- df %>% ggplot() +
  geom_density(
    aes(x = HEIGHT, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_continuous(
    name = "Height (cm)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.position.inside = c(0.2, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/height_out.pdf", height_out, width = width, height = height)

# GAM
height_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = HEIGHT, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Height (cm)",
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/height_gam.pdf", height_gam, width = width, height = height + 1)

# combine the plots

height_overall <- height_overall + theme(axis.title.x = element_blank())
height_out <- height_out + theme()
height_gam <- height_gam #+ theme(axis.text.y = element_blank(), axis.title.y = element_blank())


height_comb <- (height_overall / height_out) | height_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/height_comb.pdf", height_comb, width = 2 * width, height = 2 * height)

## WEIGHT ##

# overall
weight_overall <- df %>% ggplot() +
  geom_density(
    aes(x = WEIGHT),
    fill = ov_col
  ) +
  scale_x_continuous(
    name = "Weight (kg)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/weight_overall.pdf", age_overall, width = width, height = height)

# by outcome

weight_out <- df %>% ggplot() +
  geom_density(
    aes(x = WEIGHT, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_continuous(
    name = "Weight (kg)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/weight_out.pdf", weight_out, width = width, height = height)

# GAM
weight_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = WEIGHT, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Weight (kg)",
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/weight_gam.pdf", weight_gam, width = width, height = height + 1)

# combine the plots

weight_overall <- weight_overall + theme(axis.title.x = element_blank())
weight_out <- weight_out + theme()
weight_gam <- weight_gam #+ theme(axis.text.y = element_blank(), axis.title.y = element_blank())


weight_comb <- (weight_overall / weight_out) | weight_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/weight_comb.pdf", weight_comb, width = 2 * width, height = 2 * height)

## SPLEEN SIZE ##

# overall
ss_overall <- df %>% ggplot() +
  geom_density(
    aes(x = SPLEEN_LENGTH, fill = "grey")
  ) +
  scale_x_continuous(
    name = "Spleen size (cm)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    name = "",
    values = ov_col,
    labels = c("Overall")
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.75), # x, y within plot panel
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/ss_overall.pdf", age_overall, width = width, height = height)

# by outcome

ss_out <- df %>% ggplot() +
  geom_density(
    aes(x = SPLEEN_LENGTH, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_continuous(
    name = "Spleen size (cm)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.77), # x, y within plot panel
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/ss_out.pdf", ss_out, width = width, height = height)

# GAM
ss_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = SPLEEN_LENGTH, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Spleen size (cm)",
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/ss_gam.pdf", ss_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(SPLEEN_LENGTH), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(SPLEEN_LENGTH = seq(min(df2$SPLEEN_LENGTH, na.rm = TRUE),
  max(df2$SPLEEN_LENGTH, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
ss_logodds <- ggplot(newdata, aes(x = SPLEEN_LENGTH)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "Spleen size (cm)",
    y = "log(odds) of relapse"
    # title = "Estimated log-odds of outcome across age (95% CI)"
  ) +
  coord_cartesian(ylim = c(-4.5, -2)) +
  theme_minimal()

ss_logodds2 <- ggplot(newdata, aes(x = log(SPLEEN_LENGTH + 0.5))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "log(Spleen size + 1)",
    y = "log(odds) of relapse"
    # title = "Estimated log-odds of outcome across age (95% CI)"
  ) +
  coord_cartesian(ylim = c(-4.5, -2)) +
  theme_minimal()

# combine the plots

ss_overall <- ss_overall + theme(axis.title.x = element_blank())
ss_out <- ss_out #+ theme(legend.position = "")
ss_gam <- ss_gam #+ theme(axis.text.y = element_blank(), axis.title.y = element_blank())


ss_comb <- (ss_overall / ss_out) | ss_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/ss_comb.pdf", ss_comb, width = 2 * width, height = 2 * height)

## FEVER DURATION ##

# overall
fd_overall <- df %>% ggplot() +
  geom_density(
    aes(x = FEVER_DURATION),
    fill = ov_col
  ) +
  scale_x_log10(
    name = "Duration of fever (days, log scale)",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/fd_overall.pdf", age_overall, width = width, height = height)

# by outcome

fd_out <- df %>% ggplot() +
  geom_density(
    aes(x = FEVER_DURATION, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "Duration of fever (days, log scale)",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/fd_out.pdf", fd_out, width = width, height = height)

# GAM
fd_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = FEVER_DURATION, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "Duration of fever (days, log scale)",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/fd_gam.pdf", fd_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(FEVER_DURATION), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(FEVER_DURATION = seq(min(df2$FEVER_DURATION, na.rm = TRUE),
  max(df2$FEVER_DURATION, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
fd_logodds <- ggplot(newdata, aes(x = FEVER_DURATION)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "Fever duration (days)",
    y = "log(odds) of relapse"
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  coord_cartesian(ylim = c(-5.5, -2)) +
  theme_minimal()

# combine the plots

fd_overall <- fd_overall + theme(axis.title.x = element_blank())
fd_out <- fd_out + theme(legend.position = "none")
fd_gam <- fd_gam #+ theme(axis.text.y = element_blank(), axis.title.y = element_blank())


fd_comb <- (fd_overall / fd_out) | fd_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/fd_comb.pdf", fd_comb, width = 2 * width, height = 2 * height)

## WBC ##

# overall
wbc_overall <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_WBC),
    fill = ov_col
  ) +
  scale_x_log10(
    name = "WBC (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/wbc_overall.pdf", age_overall, width = width, height = height)

# by outcome

wbc_out <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_WBC, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "WBC (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/wbc_out.pdf", wbc_out, width = width, height = height)

# GAM
wbc_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = LAB_WBC, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "WBC (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10)),
    expand = c(0, 0),
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/wbc_gam.pdf", wbc_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(LAB_WBC), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(LAB_WBC = seq(min(df2$LAB_WBC, na.rm = TRUE),
  max(df2$LAB_WBC, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
wbc_logodds <- ggplot(newdata, aes(x = LAB_WBC)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "White blood cell count x10^9/L, log scale)",
    y = "log(odds) of relapse"
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  coord_cartesian(ylim = c(-4, -2)) +
  theme_minimal()

# combine the plots

wbc_overall <- wbc_overall + theme(axis.title.x = element_blank())
wbc_out <- wbc_out + theme(legend.position = "none", axis.title.x = element_markdown())
wbc_gam <- wbc_gam + theme(axis.title.x = element_markdown())

wbc_comb <- (wbc_overall / wbc_out) | wbc_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/wbc_comb.pdf", wbc_comb, width = 2 * width, height = 2 * height)

## PLATELETS ##

# overall
plt_overall <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_PLT),
    fill = ov_col
  ) +
  scale_x_log10(
    name = "Platelets (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/plt_overall.pdf", age_overall, width = width, height = height)

# by outcome

plt_out <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_PLT, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "Platelets (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/plt_out.pdf", plt_out, width = width, height = height)

# GAM
plt_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = LAB_PLT, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "Platelets (x10<sup>9</sup>/L, log scale)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100, 200, 500, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/plt_gam.pdf", plt_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(LAB_PLT), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(LAB_PLT = seq(min(df2$LAB_PLT, na.rm = TRUE),
  max(df2$LAB_PLT, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
plt_logodds <- ggplot(newdata, aes(x = LAB_PLT)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "Platelet count x10^9/L, log scale)",
    y = "log(odds) of relapse"
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  coord_cartesian(ylim = c(-4, -2)) +
  theme_minimal()

# combine the plots

plt_overall <- plt_overall + theme(axis.title.x = element_blank())
plt_out <- plt_out + theme(legend.position = "none", axis.title.x = element_markdown())
plt_gam <- plt_gam + theme(axis.title.x = element_markdown())

plt_comb <- (plt_overall / plt_out) | plt_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/plt_comb.pdf", plt_comb, width = 2 * width, height = 2 * height)


## HAEMOGLOBIN ##

# overall
hb_overall <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_HGB),
    fill = ov_col
  ) +
  scale_x_log10(
    name = "Haemoglobin (g/L, log scale)",
    breaks = c(20, 30, 40, 50, 60, 80, 100, 120, 160, 200),
    minor_breaks = c(seq(1, 10, 1), seq(10, 200, 10)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/hb_overall.pdf", age_overall, width = width, height = height)

# by outcome

hb_out <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_HGB, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "Haemoglobin (g/L, log scale)",
    breaks = c(20, 30, 40, 50, 60, 80, 100, 120, 160, 200),
    minor_breaks = c(seq(1, 10, 1), seq(10, 200, 10)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/hb_out.pdf", hb_out, width = width, height = height)

# GAM
hb_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = LAB_HGB, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "Haemoglobin (g/L, log scale)",
    breaks = c(20, 30, 40, 50, 60, 80, 100, 120, 160, 200),
    minor_breaks = c(seq(1, 10, 1), seq(10, 200, 10)),
    expand = c(0, 0),
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/hb_gam.pdf", hb_gam, width = width, height = height + 1)

# combine the plots

hb_overall <- hb_overall + theme(axis.title.x = element_blank())
hb_out <- hb_out + theme(legend.position = "none", axis.title.x = element_markdown())
hb_gam <- hb_gam + theme(axis.title.x = element_markdown())

hb_comb <- (hb_overall / hb_out) | hb_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/hb_comb.pdf", hb_comb, width = 2 * width, height = 2 * height)

## ALT ##

# overall
alt_overall <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_ALT),
    fill = ov_col
  ) +
  scale_x_log10(
    name = "ALT (IU/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/alt_overall.pdf", age_overall, width = width, height = height)

# by outcome

alt_out <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_ALT, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "ALT (IU/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8), # x, y within plot panel
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/alt_out.pdf", alt_out, width = width, height = height)

# GAM
alt_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = LAB_ALT, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "ALT (IU/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/alt_gam.pdf", alt_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(LAB_ALT), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(LAB_ALT = seq(min(df2$LAB_ALT, na.rm = TRUE),
  max(df2$LAB_ALT, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
alt_logodds <- ggplot(newdata, aes(x = LAB_ALT)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "ALT (IU, log scale)",
    y = "log(odds) of relapse"
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  coord_cartesian(ylim = c(-4, -2)) +
  theme_minimal()

# combine the plots

alt_overall <- alt_overall + theme(axis.title.x = element_blank())
alt_out <- alt_out + theme(legend.position = "none", axis.title.x = element_markdown())
alt_gam <- alt_gam + theme(axis.title.x = element_markdown())

alt_comb <- (alt_overall / alt_out) | alt_gam + plot_annotation(tag_levels = "a")
ggsave("figures/dist/contOut/alt_comb.pdf", alt_comb, width = 2 * width, height = 2 * height)

## CREATININE ##

# overall
cr_overall <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_CREAT, fill = ov_col),
  ) +
  scale_fill_manual(
    values = ov_col,
    label = "Overall",
    name = ""
  ) +
  scale_x_log10(
    name = "Creatinine (&micro;mol/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0),
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.882, 0.7), # x, y within plot panel
    # legend.background = element_rect(fill = NA),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/cr_overall.pdf", age_overall, width = width, height = height)

# by outcome

cr_out <- df %>% ggplot() +
  geom_density(
    aes(x = LAB_CREAT, fill = factor(OUTCOME)),
    alpha = alpha
  ) +
  scale_x_log10(
    name = "Creatinine (&micro;mol/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    labels = c("Final cure", "Relapse"),
    values = out_col,
    name = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.8), # x, y within plot panel
    # legend.background = element_rect(fill = NA),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("figures/dist/contOut/cr_out.pdf", cr_out, width = width, height = height)

# GAM
cr_gam <- df %>% ggplot() +
  geom_smooth(
    aes(x = LAB_CREAT, y = as.numeric(OUTCOME)),
    method = "gam",
    method.args = list(family = binomial, method = "GCV.Cp"),
    formula = y ~ s(x),
    se = TRUE
  ) + # loess smoother and 95% CI
  scale_y_continuous(
    name = "% relapse",
    minor_breaks = seq(0, 0.15, 0.01),
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    expand = c(0, 0)
  ) +
  scale_x_log10(
    name = "Creatinine (&micro;mol/L, log scale)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100)),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 0.15)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
  )

ggsave("figures/dist/contOut/cr_gam.pdf", cr_gam, width = width, height = height + 1)

# make sure OUTCOME is 0/1 numeric
df2 <- df %>%
  mutate(OUTCOME_bin = as.numeric(OUTCOME))

# fit GAM with logit link
mod <- gam(OUTCOME_bin ~ s(LAB_CREAT), family = binomial(link = "logit"), data = df2)

# newdata grid over age
newdata <- data.frame(LAB_CREAT = seq(min(df2$LAB_CREAT, na.rm = TRUE),
  max(df2$LAB_CREAT, na.rm = TRUE),
  length.out = 200
))

# predict on link (log-odds) scale, get se
pr <- predict(mod, newdata, type = "link", se.fit = TRUE)

crit <- qnorm(0.975) # 1.96
newdata <- newdata %>%
  mutate(
    fit = pr$fit,
    se = pr$se.fit,
    upper = fit + crit * se,
    lower = fit - crit * se
  )

# plot log-odds with 95% CI ribbon
creat_logodds <- ggplot(newdata, aes(x = LAB_CREAT)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(
    x = "Creatinine (micromol/L, log scale)",
    y = "log(odds) of relapse"
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  coord_cartesian(ylim = c(-4, -2)) +
  theme_minimal()

# combine the plots

cr_overall <- cr_overall + theme(axis.title.x = element_blank())
cr_out <- cr_out + theme(legend.position = "inside", axis.title.x = element_markdown())
cr_gam <- cr_gam + theme(axis.title.x = element_markdown())

cr_comb <- (cr_overall / cr_out) | cr_gam
ggsave("figures/dist/contOut/cr_comb.pdf", cr_comb, width = 2 * width, height = 2 * height)
