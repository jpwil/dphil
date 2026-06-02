## plot distributions for thesis results

rm(list = ls())

library(tidyverse)
library(ggridges)
library(patchwork)
library(ggtext)

# https://github.com/tidyverse/ggplot2/issues/6752
# install.packages("remotes")
# remotes::install_version("ggplot2", version = "3.5.2")
# packageVersion("ggplot2")

################
## CONTINUOUS ##
################

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    AGE_MISSING_N = sum(!is.na(AGE)),
    AGE_MISSING_P = 100 * AGE_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", AGE_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - AGE_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

## CONTINUOUS VARIABLES

# AGE #
binwidth <- (max(df$AGE, na.rm = TRUE) - min(df$AGE, na.rm = TRUE)) / 38
binwidth <- 2

age1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = AGE,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.5,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Age (years)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
  )

ggsave("figures/dist/age_ridgeplot1.pdf", plot = age1, width = 7.5, height = 10)

age2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = AGE,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.5,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Age (years, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/age_ridgeplot2.pdf", plot = age2, width = 7.5, height = 10)

age2 <- age2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
age1 <- age1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

library(cowplot)

age_comb <- cowplot::ggdraw() +
  draw_plot(age1, x = 0, width = 0.5, height = 0.962) +
  draw_plot(age2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/age_comb.pdf", plot = age_comb, device = cairo_pdf, width = 10, height = 7)

# HEIGHT #
# binwidth <- (max(df$AGE, na.rm = TRUE) - min(df$AGE, na.rm = TRUE)) / 38

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    HEIGHT_MISSING_N = sum(!is.na(HEIGHT)),
    HEIGHT_MISSING_P = 100 * HEIGHT_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", HEIGHT_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - HEIGHT_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )


binwidth <- 2

height1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = HEIGHT,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Height (cm, normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/height_ridgeplot1.pdf", width = 7.5, height = 10)


height2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = HEIGHT,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Height (cm, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/height_ridgeplot2.pdf", width = 7.5, height = 10)

height2 <- height2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
height1 <- height1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

height_comb <- cowplot::ggdraw() +
  draw_plot(height1, x = 0, width = 0.5, height = 0.935) +
  draw_plot(height2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/height_comb.pdf", height_comb, width = 10, height = 7)


# WEIGHT #
# binwidth <- (max(df$AGE, na.rm = TRUE) - min(df$AGE, na.rm = TRUE)) / 38

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    WEIGHT_MISSING_N = sum(!is.na(WEIGHT)),
    WEIGHT_MISSING_P = 100 * WEIGHT_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", WEIGHT_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - WEIGHT_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )


binwidth <- 2
weight1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = WEIGHT,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Weight (kg, normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/weight_ridgeplot1.pdf", width = 7.5, height = 10)

weight2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = WEIGHT,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Weight (kg, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/weight_ridgeplot2.pdf", width = 7.5, height = 10)

weight2 <- weight2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
weight1 <- weight1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

weight_comb <- cowplot::ggdraw() +
  draw_plot(weight1, x = 0, width = 0.5, height = 0.97) +
  draw_plot(weight2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/weight_comb.pdf", weight_comb, width = 10, height = 7)


# SPLEEN SIZE #
# binwidth <- (max(df$AGE, na.rm = TRUE) - min(df$AGE, na.rm = TRUE)) / 38
binwidth <- 1

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    SPLEEN_LENGTH_MISSING_N = sum(!is.na(SPLEEN_LENGTH)),
    SPLEEN_LENGTH_MISSING_P = 100 * SPLEEN_LENGTH_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", SPLEEN_LENGTH_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - SPLEEN_LENGTH_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

ss1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = SPLEEN_LENGTH,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Spleen size (cm, normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/spleen_ridgeplot1.pdf", width = 7.5, height = 10)


ss2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = SPLEEN_LENGTH,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Spleen size (cm, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/spleen_ridgeplot2.pdf", width = 7.5, height = 10)

ss2 <- ss2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
ss1 <- ss1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

ss_comb <- cowplot::ggdraw() +
  draw_plot(ss1, x = 0, width = 0.5, height = 0.98) +
  draw_plot(ss2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/ss_comb.pdf", ss_comb, width = 10, height = 7)


# BMI (19 and over, only)
binwidth <- 0.5

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  filter(AGE >= 19 & !is.na(AGE)) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    BMI_MISSING_N = sum(!is.na(BMI)),
    BMI_MISSING_P = 100 * BMI_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", BMI_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - BMI_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

bmi1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = BMI,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "BMI (kg/m<sup>2</sup>, normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/bmi_ridgeplot1.pdf", width = 7.5, height = 10)

bmi2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = BMI,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "BMI (kg/m<sup>2</sup>, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/bmi_ridgeplot2.pdf", width = 7.5, height = 10)

bmi2 <- bmi2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
bmi1 <- bmi1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

bmi_comb <- cowplot::ggdraw() +
  draw_plot(bmi1, x = 0, width = 0.5, height = 0.95) +
  draw_plot(bmi2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/bmi_comb.pdf", bmi_comb, width = 10, height = 7)

# BMI-FOR_AGE Z score (5-18 inclusive)
binwidth <- 0.5

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  filter(AGE >= 5 & AGE < 19 & !is.na(AGE)) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    BMIZ_MISSING_N = sum(!is.na(BMIZ)),
    BMIZ_MISSING_P = 100 * BMIZ_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", BMIZ_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - BMIZ_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

bmiz1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = BMIZ,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "BMI-for-age z-score (normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/bmiz_ridgeplot1.pdf", width = 7.5, height = 10)

bmiz2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = BMIZ,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "BMI-for-age z-score (normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/bmiz_ridgeplot2.pdf", width = 7.5, height = 10)

bmiz2 <- bmiz2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
bmiz1 <- bmiz1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

bmiz_comb <- cowplot::ggdraw() +
  draw_plot(bmiz1, x = 0, width = 0.5, height = 0.948) +
  draw_plot(bmiz2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/bmiz_comb.pdf", bmiz_comb, width = 10, height = 7)

# WEIGHT-FOR-HEIGHT Z score (under 5)
binwidth <- 0.5

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  filter(AGE < 5 & !is.na(AGE)) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    WFHZ_MISSING_N = sum(!is.na(WFHZ)),
    WFHZ_MISSING_P = 100 * WFHZ_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", WFHZ_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - WFHZ_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )


df <- df %>%
  mutate(
    STUDY_LABEL = case_when(
      study_n == 6 & WFHZ_MISSING_N == 1 ~ paste0(
        "<b>", STUDY_NAME, "</b>",
        "<br/> n = 1/ 6",
        "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - WFHZ_MISSING_P), "</span><br/><br/><br/>"
      ),
      study_n == 5 & WFHZ_MISSING_N == 3 ~ paste0(
        "<b>", STUDY_NAME, "</b>",
        "<br/> n = 3/ 5",
        "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - WFHZ_MISSING_P), "</span><br/><br/><br/>"
      ),
      .default = STUDY_LABEL
    )
  )

wfh1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = WFHZ,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Weight-for-height z-score (normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/wfhz_ridgeplot1.pdf", width = 7.5, height = 7)

wfh2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = WFHZ,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Weight-for-height z-score (normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/wfhz_ridgeplot2.pdf", width = 7.5, height = 7)

wfh2 <- wfh2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
wfh1 <- wfh1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

wfh_comb <- cowplot::ggdraw() +
  draw_plot(wfh1, x = 0, width = 0.5, height = 0.865) +
  draw_plot(wfh2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/wfh_comb.pdf", wfh_comb, width = 10, height = 7)

# FEVER DURATION
binwidth <- 15

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    FEVER_DURATION_MISSING_N = sum(!is.na(FEVER_DURATION)),
    FEVER_DURATION_MISSING_P = 100 * FEVER_DURATION_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", FEVER_DURATION_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - FEVER_DURATION_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

fd1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = FEVER_DURATION,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Duration of fever (days, normalised within study)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("figures/dist/fd_ridgeplot1.pdf", width = 7.5, height = 10)




fd2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = FEVER_DURATION,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_continuous(
    name = "Duration of fever (days, normalised across all studies)"
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )


ggsave("figures/dist/fd_ridgeplot2.pdf", width = 7.5, height = 10)

fd2 <- fd2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
fd1 <- fd1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

fd_comb <- cowplot::ggdraw() +
  draw_plot(fd1, x = 0, width = 0.5, height = 0.97) +
  draw_plot(fd2, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/fd_comb.pdf", fd_comb, width = 10, height = 7)

# FEVER DURATION LOG SCALE
binwidth <- 0.1

fd1_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = FEVER_DURATION,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Duration of fever (days, log scale, normalised within study)",
    breaks = c(1, 10, 100, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

fd2_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = FEVER_DURATION,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Duration of fever (days, log scale, normalised across studies)",
    breaks = c(1, 10, 100, 1000),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

fd2_log <- fd2_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
fd1_log <- fd1_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

fd_log_comb <- cowplot::ggdraw() +
  draw_plot(fd1_log, x = 0, width = 0.5, height = 0.941) +
  draw_plot(fd2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/fd_log_comb.pdf", fd_log_comb, width = 10, height = 7.2)

# # PARASITE GRADE
# binwidth <- 1

# df <- readRDS("data/ads_summary.rds")
# df %>% names()
# df <- df %>%
#   group_by(STUDYID) %>%
#   mutate(
#     study_n = n(),
#     PARASITE_MISSING_N = sum(!is.na(PARASITE)),
#     PARASITE_MISSING_P = 100 * PARASITE_MISSING_N / study_n
#   ) %>%
#   ungroup() %>%
#   mutate(
#     STUDY_LABEL = paste0(
#       "<b>", STUDY_NAME, "</b>",
#       "<br/> n = ", PARASITE_MISSING_N, "/", study_n,
#       "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - PARASITE_MISSING_P), "</span><br/><br/><br/>"
#     ),
#     STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
#   )

# df %>% ggplot() +
#   geom_density_ridges(
#     stat = "binline",
#     binwidth = binwidth,
#     aes(
#       x = PARASITE,
#       y = STUDY_LABEL,
#       height = after_stat(density)
#     ),
#     alpha = 0.6,
#     scale = 0.9,
#     draw_baseline = FALSE,
#     panel_scaling = TRUE
#   ) +
#   scale_x_continuous(
#     name = "Parasite grade",
#     breaks = seq(1, 5, 1),
#     minor_break = seq(1, 5, 1),
#     labels = c("1+", "2+", "3+", "4+", "5+")
#   ) +
#   scale_y_discrete(
#     name = "Study"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_markdown(lineheight = 1.15),
#     legend.position = "none",
#     plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
#   )

# ggsave("figures/dist/pg_ridgeplot1.pdf", width = 7.5, height = 10)

# df %>% ggplot() +
#   geom_density_ridges(
#     stat = "binline",
#     binwidth = binwidth,
#     aes(
#       x = PARASITE,
#       y = STUDY_LABEL,
#       height = after_stat(count)
#     ),
#     alpha = 0.6,
#     scale = 0.9,
#     draw_baseline = FALSE,
#     panel_scaling = TRUE
#   ) +
#   scale_x_continuous(
#     name = "Parasite grade",
#     breaks = seq(1, 5, 1),
#     minor_break = seq(1, 5, 1),
#     labels = c("1+", "2+", "3+", "4+", "5+")
#   ) +
#   scale_y_discrete(
#     name = "Study"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_markdown(lineheight = 1.15),
#     legend.position = "none",
#     plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
#   )

# ggsave("figures/dist/pg_ridgeplot2.pdf", width = 7.5, height = 10)

# WBC
binwidth <- 0.04

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    LAB_WBC_MISSING_N = sum(!is.na(LAB_WBC)),
    LAB_WBC_MISSING_P = 100 * LAB_WBC_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", LAB_WBC_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - LAB_WBC_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

wbc_log1 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_WBC,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "WBC (x10<sup>9</sup>/L, log scale, normalised within study)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 30),
    minor_breaks = c(seq(0.4, 1, 0.1), seq(1, 10, 1), seq(10, 30, 10))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

wbc_log2 <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_WBC,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "WBC (x10<sup>9</sup>/L, log scale, normalised across studies)",
    breaks = c(1, 2, 3, 4, 5, 10, 20, 30),
    minor_breaks = c(seq(0.4, 1, 0.1), seq(1, 10, 1), seq(10, 30, 10))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

wbc2_log <- wbc_log2 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
wbc1_log <- wbc_log1 + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

wbc_log_comb <- cowplot::ggdraw() +
  draw_plot(wbc1_log, x = 0, width = 0.5, height = 0.973) +
  draw_plot(wbc2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/wbc_log_comb.pdf", wbc_log_comb, width = 10, height = 7.2)

# Platelets (LOG)
binwidth <- 0.04

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    LAB_PLT_MISSING_N = sum(!is.na(LAB_PLT)),
    LAB_PLT_MISSING_P = 100 * LAB_PLT_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", LAB_PLT_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - LAB_PLT_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

plt1_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_PLT,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Platelets (x10<sup>9</sup>/L, log scale, normalised within study)",
    breaks = c(10, 20, 30, 100, 200, 300, 1000),
    minor_breaks = c(6, 7, 8, 9, 10, seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

plt2_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_PLT,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Platelets (x10<sup>9</sup>/L, log scale, normalised across studies)",
    breaks = c(10, 20, 30, 100, 200, 300, 1000),
    minor_breaks = c(6, 7, 8, 9, 10, seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

plt2_log <- plt2_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
plt1_log <- plt1_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

plt_log_comb <- cowplot::ggdraw() +
  draw_plot(plt1_log, x = 0, width = 0.5, height = 0.973) +
  draw_plot(plt2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/plt_log_comb.pdf", plt_log_comb, width = 10, height = 7)

# Haemoglobin
binwidth <- 0.03

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    LAB_HGB_MISSING_N = sum(!is.na(LAB_HGB)),
    LAB_HGB_MISSING_P = 100 * LAB_HGB_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", LAB_HGB_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - LAB_HGB_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

hb1_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_HGB,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Haemoglobin (g/L, log scale, normalised within study)",
    breaks = c(20, 30, 40, 50, 100, 200),
    minor_breaks = seq(20, 200, 10)
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

hb2_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_HGB,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Haemoglobin (g/L, log scale, normalised across studies)",
    breaks = c(20, 30, 40, 50, 100, 200),
    minor_breaks = seq(20, 200, 10)
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

hb2_log <- hb2_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
hb1_log <- hb1_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

hb_log_comb <- cowplot::ggdraw() +
  draw_plot(hb1_log, x = 0, width = 0.5, height = 0.974) +
  draw_plot(hb2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/hb_log_comb.pdf", hb_log_comb, width = 10, height = 7)

# ALT
binwidth <- 0.05

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    LAB_ALT_MISSING_N = sum(!is.na(LAB_ALT)),
    LAB_ALT_MISSING_P = 100 * LAB_ALT_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", LAB_ALT_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - LAB_ALT_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

alt1_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_ALT,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "ALT (U/L, log scale, normalised within study)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

alt2_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_ALT,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "ALT (U/L, log scale, normalised across studies)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

alt2_log <- alt2_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
alt1_log <- alt1_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

alt_log_comb <- cowplot::ggdraw() +
  draw_plot(alt1_log, x = 0, width = 0.5, height = 0.974) +
  draw_plot(alt2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/alt_log_comb.pdf", alt_log_comb, width = 10, height = 7)

# CREATININE

binwidth <- 0.05

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    LAB_CREAT_MISSING_N = sum(!is.na(LAB_CREAT)),
    LAB_CREAT_MISSING_P = 100 * LAB_CREAT_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", LAB_CREAT_MISSING_N, "/", study_n,
      "<br/><span style='color:red'>Missing: ", sprintf("%.1f%%", 100 - LAB_CREAT_MISSING_P), "</span><br/><br/><br/>"
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

cr1_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_CREAT,
      y = STUDY_LABEL,
      height = after_stat(density)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Creatinine (&micro;mol/L, log scale, normalised within study)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

cr2_log <- df %>% ggplot() +
  geom_density_ridges(
    stat = "binline",
    binwidth = binwidth,
    aes(
      x = LAB_CREAT,
      y = STUDY_LABEL,
      height = after_stat(count)
    ),
    alpha = 0.6,
    scale = 1.2,
    draw_baseline = FALSE,
    panel_scaling = TRUE
  ) +
  scale_x_log10(
    name = "Creatinine (&micro;mol/L, log scale, normalised across studies)",
    breaks = c(5, 10, 20, 30, 50, 100, 200, 300, 500),
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100))
  ) +
  scale_y_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

cr2_log <- cr2_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())
cr1_log <- cr1_log + theme(axis.text.y = element_markdown(size = 6), axis.title.y = element_blank())

cr_log_comb <- cowplot::ggdraw() +
  draw_plot(cr1_log, x = 0, width = 0.5, height = 0.974) +
  draw_plot(cr2_log, x = 0.5, width = 0.5, height = 1)

ggsave("figures/dist/cr_log_comb.pdf", cr_log_comb, width = 10, height = 7)


##############
## DISCRETE ##
##############

### FRONT PANEL ###
# OUTCOME
df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    OUTCOME_MISSING_N = sum(!is.na(OUTCOME)),
    OUTCOME_MISSING_P = 100 * OUTCOME_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

out <- df %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = OUTCOME
    ),
    position = "fill",
    width = 0.4,
    colour = "black"
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Relapse: % <span style='color:#b80202'><b>yes</b></span>",
    breaks = seq(0, 0.15, 0.05),
    labels = c("0", "5", "10", "15"),
    minor_breaks = seq(0, 0.15, 0.01)
  ) +
  scale_fill_manual(
    name = "",
    labels = c("Final cure", "Relapse"),
    values = c("white", "#b80202")
  ) +
  coord_flip(
    ylim = c(0, 0.15)
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/out_bar1.pdf", width = 5, height = 10)

# SEX
df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    SEX_MALE_MISSING_N = sum(!is.na(SEX_MALE)),
    SEX_MALE_MISSING_P = 100 * SEX_MALE_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

sex <- df %>% ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = SEX_MALE
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Sex: % <span style='color:#b80202'><b>male</b></span>",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    # position = "right"
  ) +
  scale_fill_manual(
    name = "",
    labels = c("Female", "Male"),
    values = c("white", "#b80202")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

# study number
num <- df %>% ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL
    ),
    position = "stack",
    width = 0.4,
    fill = "darkblue"
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Sample size",
    breaks = seq(0, 1000, 200),
    minor_breaks = seq(0, 1000, 100)
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
  )

ggsave("figures/dist/study_number.pdf", plot = num, width = 3, height = 10)

out <- out + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
sex <- sex + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
age1 <- age1 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())

comb <- (num | out | sex | age1) + plot_layout(widths = c(0.6, 0.6, 0.6, 2))

ggsave("figures/dist/main_dist.pdf", plot = comb, width = 10, height = 7)

# ANAEMIA

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n(),
    SEVERE_ANAEMIA_MISSING_N = sum(!is.na(SEVERE_ANAEMIA)),
    SEVERE_ANAEMIA_MISSING_P = 100 * SEVERE_ANAEMIA_MISSING_N / study_n
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

anaemia1 <- df %>%
  filter(!is.na(SEVERE_ANAEMIA)) %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = SEVERE_ANAEMIA
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Anaemia: % <span style='color:#b80202'><b>severe</b></span>",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    # position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("TRUE" = "#b80202", "FALSE" = "white")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/anaemia_bar1.pdf", plot = anaemia1, width = 5, height = 10)

anaemia2 <- df %>% ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = is.na(SEVERE_ANAEMIA)
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Anaemia: % missing",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("white", "black")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none"
  )

ggsave("figures/dist/anaemia_bar2.pdf", plot = anaemia2, width = 5, height = 10)


# MALNUTRITION

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  mutate(MAL_GROUP = case_when(
    MAL_MILD_NORM ~ "Mild",
    MAL_MODERATE ~ "Moderate",
    MAL_SEVERE ~ "Severe",
    .default = NA
  )) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n()
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

mal1 <- df %>%
  group_by(STUDYID) %>%
  mutate(
    MAL_GROUP = if (all(is.na(MAL_GROUP))) "ALL_MISSING" else MAL_GROUP
  ) %>%
  ungroup() %>%
  filter(!is.na(MAL_GROUP)) %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = factor(MAL_GROUP, levels = c("Severe", "Moderate", "Mild", "ALL_MISSING"))
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Malnutrition: % <span style='color:#62a8bf'><b>mild</b></span> | <span style='color:#5656fd'><b>mod</b></span> | <span style='color:#00008b'><b>sev</b></span> | <span style='color: #b2b2b2'><b>NA</b></span>",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("#00008b", "#5656fd", "#62a8bf", "#b2b2b2")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/mal_bar1.pdf", plot = mal1, width = 5, height = 10)

mal2 <- df %>% ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = is.na(MAL_GROUP)
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Malnutrition: % missing",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  scale_fill_manual(
    name = "",
    values = c("white", "black")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none"
  )

ggsave("figures/dist/mal_bar2.pdf", plot = mal2, width = 5, height = 10)

# TREATMENT

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  mutate(TREAT_GROUP = case_when(
    TREAT_MF ~ "Miltefosine",
    TREAT_OTHER ~ "Other",
    TREAT_SDA ~ "SDA",
    .default = NA
  )) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n()
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

trt1 <- df %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = factor(TREAT_GROUP, levels = c("SDA", "Miltefosine", "Other"))
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = "Treatment: % <span style='color:#d48a00'><b>SDA</b></span> | <span style='color:#00008b'><b>MF</b></span> | <span style='color:#006400'><b>other</b></span>",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("#d48a00", "#00008b", "#006400")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/trt_bar1.pdf", plot = trt1, width = 5, height = 10)

# PARASITE SOURCE

df <- readRDS("data/ads_summary.rds")
df %>% names()
df <- df %>%
  mutate(PARA_SOURCE = case_when(
    PARASITE_SOURCE_SPLEEN ~ "Spleen",
    PARASITE_SOURCE_BONE ~ "Bone",
    is.na(PARASITE) ~ "No aspirate",
    .default = "Missing"
  )) %>%
  group_by(STUDYID) %>%
  mutate(
    study_n = n()
  ) %>%
  ungroup() %>%
  mutate(
    STUDY_LABEL = paste0(
      "<b>", STUDY_NAME, "</b>",
      "<br/> n = ", study_n
    ),
    STUDY_LABEL = fct_reorder(as.factor(STUDY_LABEL), STUDY_LABEL, .fun = ~ length(.x)),
  )

label_para_source <- "Parasite source: % <span style='color: #010187'><b>spleen</b></span> | <span style='color: #006300'><b>bone</b></span> | <span style='color: #b2b2b2'><b>NA</b></span>"

para1 <- df %>%
  group_by(STUDYID) %>%
  mutate(
    PARA_SOURCE = if (all(PARA_SOURCE == "No aspirate")) "NO_ASP" else PARA_SOURCE
  ) %>%
  ungroup() %>%
  filter(PARA_SOURCE %in% c("Spleen", "Bone", "Missing", "NO_ASP")) %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = factor(PARA_SOURCE, levels = rev(c("Spleen", "Bone", "Missing", "NO_ASP")))
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = label_para_source,
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = rev(c("#010187", "#006300", "#b2b2b2"))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/para_bar1.pdf", plot = para1, width = 5, height = 10)

para2 <- df %>% ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = (PARA_SOURCE == "No aspirate")
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study",
    position = "top"
  ) +
  scale_y_continuous(
    name = "Parasite source: % no aspirate",
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1),
    # position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("white", "black")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    legend.position = "none"
  )

ggsave("figures/dist/para_bar2.pdf", plot = para2, width = 5, height = 10)

# PARASITE GRADE

label_pg <- "Parasite grade: % <span style='color: #0071bd'><b>1+</b></span> | <span style='color: #00bd8b'><b>2+</b></span> | <span style='color: #1cbd00'><b>3+</b></span> | <span style='color: #94bd00'><b>4+</b></span> | <span style='color: #e49801'><b>5+</b></span> | <span style='color: #b2b2b2'><b>NA</b></span>"

# "#e49801", "#94bd00", "#1cbd00", "#00bd8b", "#0071bd", "#b2b2b2"

pg <- df %>%
  group_by(STUDYID) %>%
  mutate(
    PARA_SOURCE = if (all(PARA_SOURCE == "No aspirate")) "NO_ASP" else PARA_SOURCE,
    PARASITE = if (all(is.na(PARASITE))) -1 else PARASITE
  ) %>%
  ungroup() %>%
  filter(PARA_SOURCE != "No aspirate") %>%
  ggplot() +
  geom_bar(
    aes(
      x = STUDY_LABEL,
      fill = factor(PARASITE, levels = rev(sort(unique(PARASITE))))
    ),
    colour = "black",
    position = "fill",
    width = 0.4
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  scale_y_continuous(
    name = label_pg,
    breaks = seq(0, 1, 0.2),
    labels = c("0", "20", "40", "60", "80", "100"),
    minor_breaks = seq(0, 1, 0.1)
    # position = "right"
  ) +
  scale_fill_manual(
    name = "",
    values = c("#e49801", "#94bd00", "#1cbd00", "#00bd8b", "#0071bd", "#b2b2b2")
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(lineheight = 1.15),
    axis.title.x = element_markdown(),
    legend.position = "none"
  )

ggsave("figures/dist/para_bar3.pdf", plot = pg, width = 5, height = 10)

# COMBINE STUDY SPECIFIC CATEGORICAL PREDICTOR SUMMARY

num <- num + theme(axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
sex <- sex + theme(axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
mal1 <- mal1 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_markdown(size = 10))
mal2 <- mal2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_markdown(size = 10))
trt1 <- trt1 + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
anaemia1 <- anaemia1 + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
anaemia2 <- anaemia2 + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
para1 <- para1 + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
para2 <- para2 + theme(axis.title.y = element_blank(), axis.title.x = element_markdown(size = 10))
pg <- pg + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_markdown(size = 10))

comb_discrete <- (num | sex | mal1 | mal2 | trt1 | anaemia1 | anaemia2 | pg | para1 | para2) + plot_layout(widths = c(1, 1, 1.3, 1, 1, 1, 1, 1.3, 1.3, 1))
ggsave("figures/dist/comb_discrete.pdf", plot = comb_discrete, width = 12 * 1.4, height = 7 * 1.25)
