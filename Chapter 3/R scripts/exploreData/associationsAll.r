# CONTINUOUS ASSOCIATIONS

rm(list = ls())

df <- readRDS("data/ads_summary.rds") # required packages
library(dplyr)
library(ggplot2, lib.loc = "~/.Rlib") # load the latest version of ggplot  to allow GGally to work.
library(binom) # for Wilson CIs
library(patchwork) # combine panels
library(GGally)
library(corrplot)

relapse_colour <- "#b20000"
cure_colour <- "#00a000"

df %>% names()
df <- df %>% mutate(
  MAL = case_when(
    MAL_MILD_NORM ~ "Mild/norm",
    MAL_MODERATE ~ "Moderate",
    MAL_SEVERE ~ "Severe",
    .default = NA
  ),
  TREAT = case_when(
    TREAT_SDA ~ "SDA",
    TREAT_MF ~ "MF",
    TREAT_OTHER ~ "Other",
    .default = "ERROR"
  )
)

# select categorical variables
df %>% names()
df_cat <- df %>%
  select(
    SEX_MALE,
    MAL,
    SEVERE_ANAEMIA,
    TREAT,
    PARASITE
  ) %>%
  mutate(
    PARASITE = factor(PARASITE)
  )

log_vars <- c(
  "FEVER_DURATION",
  "LAB_HGB",
  "LAB_PLT",
  "LAB_WBC",
  "LAB_ALT",
  "LAB_CREAT"
)

df_cont <- df %>%
  select(
    AGE,
    HEIGHT,
    WEIGHT,
    FEVER_DURATION,
    SPLEEN_LENGTH,
    LAB_HGB,
    LAB_PLT,
    LAB_WBC,
    LAB_ALT,
    LAB_CREAT
  ) %>%
  mutate(
    across(
      all_of(log_vars), ~ log10(.)
    )
  )

# Custom function for GAM smoother with 95% CI
my_gam_smooth <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_point(alpha = 0.7, size = 0.3, colour = "#7c7c7c") +
    geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "tp"),
      se = TRUE, # show 95% CI
      color = "blue",
      fill = "orange",
      linewidth = 0.2,
      ...
    ) +
    theme_minimal()
}

cont_corr <- ggpairs(
  df_cont,
  # columns = c("var1", "var2", "var3"),
  lower = list(continuous = my_gam_smooth),
  upper = list(continuous = wrap("cor", size = 4)),
  columnLabels = c(
    "Age",
    "Height",
    "Weight",
    "FevDur",
    "SpnSize",
    "Hb",
    "Plt",
    "WBC",
    "ALT",
    "Crt"
  )
)

ggsave("figures/dist/ass/cont_cont.pdf", cont_corr, width = 20, height = 12)

### CATEGORICAL - CONTINUOUS PLOTS ####

# --- specify your variables ---
df <- df %>% mutate(PARASITE = as.character(PARASITE))

cont_vars <- c(
  "AGE",
  "WEIGHT",
  "HEIGHT",
  "FEVER_DURATION",
  "SPLEEN_LENGTH",
  "LAB_HGB",
  "LAB_PLT",
  "LAB_WBC",
  "LAB_ALT",
  "LAB_CREAT"
)


cat_vars <- c(
  "SEX_MALE",
  "MAL",
  "SEVERE_ANAEMIA",
  "TREAT",
  "PARASITE"
)

#
plot_list <- matrix(list(), nrow = length(cat_vars), ncol = length(cont_vars))

for (j in seq_along(cat_vars)) {
  for (i in seq_along(cont_vars)) {
    xvar <- cont_vars[i]
    yvar <- cat_vars[j]
    df_temp <- df %>% filter(!is.na(.data[[yvar]]), !is.na(.data[[xvar]]))
    p_temp <- ggplot(df_temp, aes(y = .data[[yvar]], x = .data[[xvar]])) +
      # geom_violin(trim = FALSE, alpha = 0.35) +
      geom_boxplot(width = 0.6, alpha = 0.6, ) +
      # geom_jitter(width = 0.12, size = 0.4, alpha = 0.35) +
      # scale_y_discrete(
      #   name = "Parasite grade",
      #   labels = c("1+", "2+", "3+", "4+", "5+")
      #   # breaks = c(1, 2, 3, 4, 5)
      # ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
        # axis.text.y = ifelse(i == 1, element_text(), element_blank()),
        # axis.title.y = ifelse(i == 1, element_text(), element_blank())
      )
    plot_list[[j, i]] <- p_temp
  }
}

for (j in seq_along(cat_vars)) {
  for (i in seq_along(cont_vars)) {
    if (cont_vars[[i]] != "AGE") plot_list[[j, i]] <- plot_list[[j, i]] + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    if (cont_vars[[i]] == "AGE") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(name = "Age") + coord_cartesian(xlim = range(df$AGE, na.rm = TRUE))
    if (cont_vars[[i]] == "WEIGHT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(name = "Weight") + coord_cartesian(xlim = range(df$WEIGHT, na.rm = TRUE))
    if (cont_vars[[i]] == "HEIGHT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(name = "Height") + coord_cartesian(xlim = range(df$HEIGHT, na.rm = TRUE))
    if (cont_vars[[i]] == "FEVER_DURATION") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "FevDur", breaks = c(1, 10, 100, 1000), minor_breaks = c(seq(0, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))) + coord_cartesian(xlim = range(df$FEVER_DURATION, na.rm = TRUE))
    if (cont_vars[[i]] == "SPLEEN_LENGTH") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(name = "SpnSize") + coord_cartesian(xlim = range(df$SPLEEN_LENGTH, na.rm = TRUE))
    if (cont_vars[[i]] == "LAB_HGB") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "Hb", breaks = c(20, 50, 100, 200), minor_breaks = c(seq(20, 100, 10), seq(100, 200, 10))) + coord_cartesian(xlim = range(df$LAB_HGB, na.rm = TRUE))
    if (cont_vars[[i]] == "LAB_PLT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "Plt", breaks = c(10, 100, 1000), minor_breaks = c(seq(10, 100, 10), seq(100, 1000, 100))) + coord_cartesian(xlim = range(df$LAB_PLT, na.rm = TRUE))
    if (cont_vars[[i]] == "LAB_WBC") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "WBC", breaks = c(1, 3, 10, 30), minor_breaks = seq(1, 20, 1)) + coord_cartesian(xlim = range(df$LAB_WBC, na.rm = TRUE))
    if (cont_vars[[i]] == "LAB_ALT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "ALT", breaks = c(10, 30, 100, 300), minor_breaks = c(seq(10, 100, 10), seq(100, 300, 100))) + coord_cartesian(xlim = range(df$LAB_ALT, na.rm = TRUE))
    if (cont_vars[[i]] == "LAB_CREAT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_log10(name = "Cr", breaks = c(10, 30, 100, 300), minor_breaks = c(seq(10, 100, 10), seq(100, 300, 100))) + coord_cartesian(xlim = range(df$LAB_CREAT, na.rm = TRUE))
    if (j != length(cat_vars)) plot_list[[j, i]] <- plot_list[[j, i]] + theme(axis.title.x = element_blank())
    # if (j == 1 && i %in% c(1, 2, 3, 5)) plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(position = "top")
    # if (j == 1 && !i %in% c(1, 2, 3, 5)) plot_list[[j, i]] <- plot_list[[j, i]] + scale_x_continuous(position = "top")
    # if (cat_vars[[j]] != "PARASITE") plot_list[[j, i]] <- plot_list[[j, i]] + theme(axis.title.x = element_blank())
    # if (cat_vars[[j]] == "PARASITE") plot_list[[j, i]] <- plot_list[[j, i]] + theme(axis.title.x = element_text())
    if (cat_vars[[j]] == "SEX_MALE") plot_list[[j, i]] <- plot_list[[j, i]] + scale_y_discrete(name = "Sex", labels = c("Female", "Male")) + scale_x_continuous(position = "top")
    if (cat_vars[[j]] == "MAL") plot_list[[j, i]] <- plot_list[[j, i]] + scale_y_discrete(name = "Malnutrition")
    if (cat_vars[[j]] == "SEVERE_ANAEMIA") plot_list[[j, i]] <- plot_list[[j, i]] + scale_y_discrete(name = "Anaemia", labels = c("Non-severe", "Severe"))
    if (cat_vars[[j]] == "TREAT") plot_list[[j, i]] <- plot_list[[j, i]] + scale_y_discrete(name = "Treatment")
    if (cat_vars[[j]] == "PARASITE") plot_list[[j, i]] <- plot_list[[j, i]] + scale_y_discrete(name = "Parasite grade", labels = c("1+", "2+", "3+", "4+", "5+"))
    if (cat_vars[[j]] %in% c("TREAT", "SEVERE_ANAEMIA", "MAL")) plot_list[[j, i]] <- plot_list[[j, i]] + theme(axis.text.x = element_blank())
  }
}

combined <- wrap_plots(
  as.vector(t(plot_list)),
  nrow = length(cat_vars), guides = "collect",
  heights = c(2, 3, 2, 3, 5.3)
)

# combined

ggsave("figures/dist/ass/cont_cat.pdf", combined, width = 20, height = 12)
