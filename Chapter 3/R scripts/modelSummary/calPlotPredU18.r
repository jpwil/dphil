###################################################
# Create calibration plots for groups of interest #
###################################################

rm(list = ls())
library(tidyverse)
library(mgcv)
library(binom)
library(cowplot)

df_joined1 <- readRDS("data/calM1PredictionsU18.rds")
df_joined2 <- readRDS("data/calM2PredictionsU18.rds")

df_scale <- readRDS("data/ads_impute_scale.rds")

AGE_MEAN <- df_scale[df_scale$var == "DM_AGE", "means"]
AGE_SD <- df_scale[df_scale$var == "DM_AGE", "sd"]

FD_MEAN <- df_scale[df_scale$var == "VL_DURATIONs", "means"]
FD_SD <- df_scale[df_scale$var == "VL_DURATIONs", "sd"]

####################
# PARASITE DENSITY #
####################

# calculate expected and mean probabilities by probability (linear predictor) deciles
df_joined_sum1 <- df_joined1 %>%
  group_by(MB_COMBINEDs) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  group_by(MB_COMBINEDs) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
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
  MB_COMBINEDs, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  MB_COMBINEDs, mean_expected_prob, observed_prob, lower, upper
)

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

cal_width <- 0.005
alt_width <- 0.1
line_width <- 1

cplot1 <- cplot1 %>% filter(MB_COMBINEDs < 6)
cplot2 <- cplot2 %>% filter(MB_COMBINEDs < 6)

cal1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = factor(MB_COMBINEDs)),
    width = cal_width,
    linewidth = line_width
  ) +
  geom_point(
    data = cplot1, # exclude imputed parasite grade 7+
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Parasite grade",
    palette = "Dark2",
    breaks = seq(0, 5),
    labels = c("1+", "2+", "3+", "4+", "5+", "6+")
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.32),
    ylim = c(0, 0.32)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.3),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )


cal3 <- cplot1 %>% ggplot() +
  geom_point(
    aes(x = MB_COMBINEDs + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = MB_COMBINEDs, y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = MB_COMBINEDs, ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width,
    linewidth = line_width
  ) +
  scale_x_continuous(
    name = "Parasite grade",
    breaks = seq(0, 5),
    labels = c("1+", "2+", "3+", "4+", "5+", "6+")
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.37, 0.01),
    breaks = seq(0, 0.37, 0.05)
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  coord_cartesian(
    ylim = c(0, 0.32)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), legend.position = "inside",
    legend.position.inside = c(0.4, 0.6),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

# ggsave(
#   filename = "graphs/U18/calPlotPDM1.pdf", plot = cal1, height = 5, width = 5, dpi = 600
# )

# # ggsave(
# #   filename = "graphs/U18/calPlotPDM2.pdf", plot = cal2, height = 7, width = 7, dpi = 300
# # )

# ggsave(
#   filename = "graphs/U18/calPlotAltPDM1.pdf", plot = cal3, height = 5, width = 5, dpi = 600
# )

comb_pg <- plot_grid(cal1, cal3, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotPD.pdf", plot = comb_pg <- plot_grid(cal1, cal3, ncol = 2, align = "h"),
  height = 5, width = 10, dpi = 600
)

##################
# FEVER DURATION #
##################

# convert the fever durations to days from log scale
df_joined1 <- df_joined1 %>%
  mutate(VL_DURATION = exp(VL_DURATIONs * FD_SD + FD_MEAN))

df_joined2 <- df_joined2 %>%
  mutate(VL_DURATION = exp(VL_DURATIONs * FD_SD + FD_MEAN))

# scale <- readRDS("data/ads_impute_scale.rds")
# summary(df_joined$VL_DURATION)

df_joined1$VL_DURATION_GRP <- cut(
  df_joined1$VL_DURATION,
  breaks = quantile(df_joined1$VL_DURATION, probs = seq(0, 1, 0.2)),
  include.lowest = TRUE
)

df_joined2$VL_DURATION_GRP <- cut(
  df_joined2$VL_DURATION,
  breaks = quantile(df_joined2$VL_DURATION, probs = seq(0, 1, 0.2)),
  include.lowest = TRUE
)

df_joined1 %>% count(VL_DURATION_GRP)
# based on quantil distributions, let's choose these groups
## < 15 days
## 15 - 30 days
## 30 - 45 days
## 45 - 80 days
## ≥ 80 days

df_joined1$VL_DURATION_GRP2 <- cut(
  df_joined1$VL_DURATION,
  breaks = c(0, 10, 20, 30, 45, 80, Inf),
  include.lowest = TRUE
)

df_joined2$VL_DURATION_GRP2 <- cut(
  df_joined2$VL_DURATION,
  breaks = c(0, 10, 20, 30, 45, 80, Inf),
  include.lowest = TRUE
)

# calculate expected and mean probabilities in groups
df_joined_sum1 <- df_joined1 %>%
  group_by(VL_DURATION_GRP2) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  group_by(VL_DURATION_GRP2) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
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
  VL_DURATION_GRP2, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  VL_DURATION_GRP2, mean_expected_prob, observed_prob, lower, upper
)

# calibration plots
fever_dur_label <- c("< 10", "10 - 20", "20 - 30", "30 - 45", "45 - 80", "80+")

calA1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = VL_DURATION_GRP2),
    linewidth = line_width,
    width = cal_width
  ) +
  geom_point(
    data = cplot1,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Fever duration (days)",
    palette = "Dark2",
    labels = fever_dur_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.15),
    ylim = c(0, 0.15)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.8),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calA2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot2,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = VL_DURATION_GRP2),
    linewidth = line_width,
    width = cal_width
  ) +
  geom_point(
    data = cplot2,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Fever duration (days)",
    palette = "Dark2",
    labels = fever_dur_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.15),
    ylim = c(0, 0.15)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.8),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB1 <- cplot1 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(VL_DURATION_GRP2) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(VL_DURATION_GRP2), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = as.numeric(VL_DURATION_GRP2), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Fever duration (days)",
    breaks = seq(1, 6),
    labels = fever_dur_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.15)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.8),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB2 <- cplot2 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(VL_DURATION_GRP2) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(VL_DURATION_GRP2), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot2,
    aes(x = as.numeric(VL_DURATION_GRP2), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Fever duration (days)",
    breaks = seq(1, 6),
    labels = fever_dur_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.15)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.title = element_blank(),
    legend.position.inside = c(0.7, 0.8),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

comb_fd1 <- plot_grid(calA1, calB1, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotFD1.pdf", plot = comb_fd1, height = 5, width = 10, dpi = 600
)

comb_fd2 <- plot_grid(calA2, calB2, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotFD2.pdf", plot = comb_fd2, height = 5, width = 10, dpi = 600
)

#######
# AGE #
#######

scale <- readRDS("data/ads_impute_scale.rds")
# df_orig <- readRDS("data/ads_clean.rds")
df_joined1 <- df_joined1 %>%
  mutate(AGE = DM_AGEs * AGE_SD + AGE_MEAN)

df_joined2 <- df_joined2 %>%
  mutate(AGE = DM_AGEs * AGE_SD + AGE_MEAN)

df_joined1$AGE_GRP2 <- cut(
  df_joined1$AGE,
  breaks = c(0, 5, 8, 12, Inf),
  include.lowest = TRUE
)

df_joined2$AGE_GRP2 <- cut(
  df_joined2$AGE,
  breaks = c(0, 5, 8, 12, Inf),
  include.lowest = TRUE
)

# calculate expected and mean probabilities in groups
df_joined_sum1 <- df_joined1 %>%
  group_by(AGE_GRP2) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  group_by(AGE_GRP2) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
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
  AGE_GRP2, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  AGE_GRP2, mean_expected_prob, observed_prob, lower, upper
)

# calibration plots
age_label <- c("< 5", "5 - 8", "8 - 12", "12 - 18")
# age_label_color <- c("< 5 years", "5 - 10 years", "10 - 15 years", "15 - 25 years", "25 - 35 years", "35 - 50 years", "50+ years")

calA1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = AGE_GRP2),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot1,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Age (years)",
    palette = "Dark2",
    labels = age_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.12),
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.3),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calA2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot2,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = AGE_GRP2),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot2,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Age (years)",
    palette = "Dark2",
    labels = age_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.12),
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.3),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )


calB1 <- cplot1 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(AGE_GRP2) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(AGE_GRP2), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = as.numeric(AGE_GRP2), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Age (years)",
    breaks = seq(1, 4),
    labels = age_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.2),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB2 <- cplot2 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(AGE_GRP2) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(AGE_GRP2), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot2,
    aes(x = as.numeric(AGE_GRP2), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Age (years)",
    breaks = seq(1, 4),
    labels = age_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.title = element_blank(),
    legend.position.inside = c(0.5, 0.2),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

comb_a1 <- plot_grid(calA1, calB1, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotA1.pdf", plot = comb_a1, height = 5, width = 10, dpi = 600
)

comb_a2 <- plot_grid(calA2, calB2, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotA2.pdf", plot = comb_a2, height = 5, width = 10, dpi = 600
)

####################
# ANAEMIA SEVERITY #
####################

# calculate expected and mean probabilities in groups
df_joined_sum1 <- df_joined1 %>%
  group_by(LB_BL_HGB_GRP3) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  group_by(LB_BL_HGB_GRP3) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
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
  LB_BL_HGB_GRP3, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  LB_BL_HGB_GRP3, mean_expected_prob, observed_prob, lower, upper
)

# calibration plots
anaemia_label <- c("Non-severe", "Severe")

calA1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = factor(LB_BL_HGB_GRP3)),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot1,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_manual(
    name = "Anaemia severity",
    values = c("1" = "#ec0000", "0" = "#00b100"),
    labels = anaemia_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.1),
    ylim = c(0, 0.1)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.25),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calA2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot2,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = factor(LB_BL_HGB_GRP3)),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot2,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_manual(
    name = "Anaemia severity",
    values = c("1" = "#ec0000", "0" = "#00b100"),
    labels = anaemia_label
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.1),
    ylim = c(0, 0.1)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.25),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB1 <- cplot1 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(LB_BL_HGB_GRP3) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(LB_BL_HGB_GRP3), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = as.numeric(LB_BL_HGB_GRP3), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width / 2,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Anaemia severity",
    breaks = seq(0, 1),
    labels = anaemia_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.1),
    xlim = c(-0.5, 1.5)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.2),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB2 <- cplot2 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(LB_BL_HGB_GRP3) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(LB_BL_HGB_GRP3), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot2,
    aes(x = as.numeric(LB_BL_HGB_GRP3), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width / 2,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Anaemia severity",
    breaks = seq(0, 1),
    labels = anaemia_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.1),
    xlim = c(-0.5, 1.5)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.title = element_blank(),
    legend.position.inside = c(0.7, 0.2),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

comb_an1 <- plot_grid(calA1, calB1, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotAnaemia1.pdf", plot = comb_an1, height = 5, width = 10, dpi = 600
)

comb_an2 <- plot_grid(calA2, calB2, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotAnaemia2.pdf", plot = comb_an2, height = 5, width = 10, dpi = 600
)

###################
# TREATMENT GROUP #
####################

# calculate expected and mean probabilities in groups
df_joined_sum1 <- df_joined1 %>%
  group_by(TREAT_GRP4) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
  ) %>%
  ungroup()

df_joined_sum2 <- df_joined2 %>%
  group_by(TREAT_GRP4) %>%
  summarise(
    mean_expected_prob = mean(test_prob),
    observed_prob = mean(OUT_DC_RELAPSE),
    n = n(),
    effective_n = n() / 30
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
  TREAT_GRP4, mean_expected_prob, observed_prob, lower, upper
)

cplot2 <- cbind(df_joined_sum2, bin2) %>% select(
  TREAT_GRP4, mean_expected_prob, observed_prob, lower, upper
)

treat_label <- c("MF", "Single-dose LAMB", "Other")
# calibration plots

calA1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot1,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = factor(TREAT_GRP4)),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot1,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Treatment",
    labels = treat_label,
    palette = "Dark2"
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.12),
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.2),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calA2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(
    data = cplot2,
    aes(x = mean_expected_prob, ymin = lower, ymax = upper, colour = factor(TREAT_GRP4)),
    linewidth = line_width,
    width = cal_width / 3
  ) +
  geom_point(
    data = cplot2,
    aes(x = mean_expected_prob, y = observed_prob)
  ) +
  labs(x = "Predicted probability", y = "Observed probability") +
  scale_colour_brewer(
    name = "Treatment",
    labels = treat_label,
    palette = "Dark2"
  ) +
  coord_fixed(
    ratio = 1,
    xlim = c(0, 0.12),
    ylim = c(0, 0.12)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.2),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB1 <- cplot1 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(TREAT_GRP4) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(TREAT_GRP4), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot1,
    aes(x = as.numeric(TREAT_GRP4), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width / 2,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Treatment",
    breaks = c(1, 2, 3),
    labels = treat_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.12),
    xlim = c(0.5, 3.5)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.8),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

calB2 <- cplot2 %>% ggplot() +
  geom_point(
    aes(x = as.numeric(TREAT_GRP4) + 0.2, y = mean_expected_prob, colour = "P_PROB"),
    shape = 5
  ) +
  geom_point(
    aes(x = as.numeric(TREAT_GRP4), y = observed_prob, colour = "O_PROB")
  ) +
  geom_errorbar(
    data = cplot2,
    aes(x = as.numeric(TREAT_GRP4), ymin = lower, ymax = upper, colour = "O_PROB"),
    width = alt_width / 2,
    linewidth = line_width
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("P_PROB" = "black", "O_PROB" = "darkblue"),
    labels = c("Observed probability (95% CI)", "Predicted probability")
  ) +
  scale_x_continuous(
    name = "Treatment",
    breaks = c(1, 2, 3),
    labels = treat_label,
  ) +
  scale_y_continuous(
    name = "Probability",
    minor_breaks = seq(0, 0.2, 0.01),
    breaks = seq(0, 0.2, 0.05)
  ) +
  coord_cartesian(
    ylim = c(0, 0.12),
    xlim = c(0.5, 3.5)
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "inside",
    legend.title = element_blank(),
    legend.position.inside = c(0.7, 0.8),
    legend.background = element_rect(
      fill = "#ffffff",
      color = "black",
      linewidth = 0.5,
      linetype = "solid"
    )
  )

comb_rx1 <- plot_grid(calA1, calB1, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotRx1.pdf", plot = comb_rx1, height = 5, width = 10, dpi = 600
)

comb_rx2 <- plot_grid(calA2, calB2, ncol = 2, align = "h")
ggsave(
  filename = "graphs/U18/calPlotRx2.pdf", plot = comb_rx2, height = 5, width = 10, dpi = 600
)

# mod1 <- plot_grid(comb_pg, comb_fd1, comb_a1, comb_an1, comb_rx1, ncol = 1)
# ggsave(
#   filename = "graphs/U18/calComb1.pdf", plot = mod1, height = 25, width = 10, dpi = 600
# )
