## plot continuous distributions overall, for outcome, and association with outcome

rm(list = ls())

library(tidyverse)
library(ggridges)
library(patchwork)
library(ggtext)
library(mgcv)

# Functionalise the creation of GAM plots
plot_gam_logodds <- function(df,
                             predictor, # string name of predictor column, e.g. "AGE"
                             x_axis_title = predictor,
                             n_grid = 200) {
  # safety checks
  if (!is.data.frame(df)) stop("df must be a data.frame")
  if (!is.character(predictor) || length(predictor) != 1) stop("predictor must be a single string")
  if (!("OUTCOME" %in% names(df))) stop("df must contain column named 'OUTCOME'")
  if (!predictor %in% names(df)) stop("predictor not found in df")

  # create working variable 'var' using base R
  df$var <- df[[predictor]]

  # fit GAM with logit link
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("package 'mgcv' is required but not installed")
  }

  mod <- mgcv::gam(OUTCOME ~ s(var), family = binomial(link = "logit"), data = df)

  # newdata grid
  vr_min <- min(df$var, na.rm = TRUE)
  vr_max <- max(df$var, na.rm = TRUE)
  new_var <- seq(vr_min, vr_max, length.out = n_grid)
  newdata <- data.frame(var = new_var)

  # predict on link scale and get se
  pr <- predict(mod, newdata = newdata, type = "link", se.fit = TRUE)
  fit <- pr$fit
  se <- pr$se.fit
  crit <- qnorm(0.975)
  upper <- fit + crit * se
  lower <- fit - crit * se

  # Return a list with model and newdata (useful for further work)
  result <- list(
    model = mod,
    newdata = data.frame(var = new_var, fit = fit, se = se, upper = upper, lower = lower)
  )

  # plotting

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required for plot_type = 'ggplot'")
  }

  p <- ggplot(result$newdata, ggplot2::aes(x = var)) +
    geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(ggplot2::aes(y = fit), size = 1) +
    labs(x = x_axis_title, y = "log(odds) of relapse") +
    theme_minimal()

  result$plot <- p
  result
}

## USE FUNCTION ##

df <- readRDS("data/ads_summary.rds")
df <- df %>%
  mutate(
    FD = log(FEVER_DURATION),
    SS = log(SPLEEN_LENGTH + 1),
    WBC = log(LAB_WBC),
    PLT = log(LAB_PLT),
    ALT = log(LAB_ALT),
    CRT = log(LAB_CREAT)
  )

predictors <- c("AGE", "FD", "SS", "WBC", "PLT", "ALT", "CRT")
x_titles <- c(
  "Age (years)",
  "log(fever duration (days))",
  "log(spleen size (cm) + 1)",
  expression(log(white ~ blood ~ cell ~ count(x10^9 / L))),
  expression(log(platelets, (x10^9 / L))),
  "log(ALT (IU/L))",
  "log(creatinine (micromol/L))"
)

plot_list <- list()

for (i in seq_along(predictors)) {
  out <- plot_gam_logodds(
    df = df,
    predictor = predictors[i],
    x_axis_title = x_titles[i],
    n_grid = 200
  )
  plot_list[[i]] <- out$plot
}

# add parasite count

# relapse prob
relsum <- df %>%
  filter(!is.na(PARASITE)) %>%
  group_by(PARASITE) %>%
  summarise(
    n = n(),
    r = sum(OUTCOME, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    p = ifelse(n > 0, r / n, NA_real_)
  )

# compute Wilson CIs for each row (binom.confint accepts vector input)
ci_df <- binom.confint(relsum$r, relsum$n, methods = "wilson")
relsum <- relsum %>%
  mutate(
    ci_lower = ci_df$lower,
    ci_upper = ci_df$upper
  )

# Plot: log odds with CI
para_logodds <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(PARASITE), y = qlogis(p)),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(PARASITE), ymin = qlogis(ci_lower), ymax = qlogis(ci_upper)),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "log(odds) of relapse",
    # breaks = seq(0, 0.11, 0.01),
    # limits = c(0, 0.11),
    # labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Parasite grade",
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1+", "2+", "3+", "4+", "5+"),
    position = "bottom"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

plot_list[[length(plot_list) + 1]] <- para_logodds

library(cowplot)

p <- plot_grid(
  plot_list[[1]],
  plot_list[[2]],
  plot_list[[3]],
  plot_list[[4]],
  plot_list[[5]],
  plot_list[[6]],
  plot_list[[7]],
  plot_list[[8]],
  ncol = 2
)

ggsave("figures/dist/isc_logodds.pdf", p, width = 10, height = 10, dpi = 600)
