rm(list = ls())

df <- readRDS("data/ads_summary.rds")

# required packages
library(dplyr)
library(ggplot2)
library(binom) # for Wilson CIs
library(patchwork) # combine panels

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

width <- 0.5

# SEX

# overall
sex_overall <- df %>% ggplot() +
  geom_bar(
    aes(x = SEX_MALE, y = after_stat(count / sum(count))),
    colour = "grey",
    width = width
  ) +
  scale_y_continuous(
    name = "Overall %",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    limit = c(0, 0.6)
  ) +
  scale_x_discrete(
    name = "Sex",
    labels = c("Female", "Male")
  ) +
  coord_cartesian(
    ylim = c(0, 0.6)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df2 <- df %>%
  count(OUTCOME, SEX_MALE) %>% # counts per outcome x sex
  group_by(OUTCOME) %>%
  mutate(pct = n / sum(n)) %>% # pct within each OUTCOME
  ungroup()

# by relapse
sex_out <- df2 %>% ggplot() +
  geom_col(
    aes(x = SEX_MALE, y = pct, fill = OUTCOME),
    width = width,
    position = position_dodge(width = width)
  ) +
  scale_y_continuous(
    name = "% by relapse",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  coord_cartesian(
    ylim = c(0, 0.63)
  ) +
  scale_x_discrete(
    name = "Sex",
    labels = c("Female", "Male")
  ) +
  scale_fill_manual(
    name = "",
    values = c(cure_colour, relapse_colour)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.line.x = element_line()
  )

# relapse prob
relsum <- df %>%
  filter(!is.na(SEX_MALE)) %>%
  group_by(SEX_MALE) %>%
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

# Plot: probability with CI
sex_prob <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(SEX_MALE), y = p),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(SEX_MALE), ymin = ci_lower, ymax = ci_upper),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "% relapse",
    breaks = seq(0, 0.11, 0.01),
    limits = c(0, 0.11),
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    labels = c("Female", "Male"),
    position = "top",
    name = "Sex"
  ) + # adjust if your coding differs
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

# Plot: log odds with CI
sex_logodds <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(SEX_MALE), y = qlogis(p)),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(SEX_MALE), ymin = qlogis(ci_lower), ymax = qlogis(ci_upper)),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "log(odds) of relapse",
    # breaks = seq(0, 0.11, 0.01),
    # limits = c(0, 0.11),
    # labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    labels = c("Female", "Male"),
    position = "bottom",
    name = "Sex"
  ) + # adjust if your coding differs
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

sex_comb <- sex_prob / sex_out / sex_overall
ggsave("figures/dist/catOut/sex_comb.pdf", sex_comb, width = 3.5, height = 10)

# MALNUTRITION

# overall
mal_overall <- df %>%
  filter(!is.na(MAL)) %>%
  ggplot() +
  geom_bar(
    aes(x = MAL, y = after_stat(count / sum(count))),
    colour = "grey",
    width = width
  ) +
  scale_y_continuous(
    name = "Overall %",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    limit = c(0, 0.6)
  ) +
  coord_cartesian(
    ylim = c(0, 0.6)
  ) +
  scale_x_discrete(
    name = "Malnutrition",
    labels = c("Norm/mild", "Moderate", "Severe")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df2 <- df %>%
  filter(!is.na(MAL)) %>%
  count(OUTCOME, MAL) %>% # counts per outcome x sex
  group_by(OUTCOME) %>%
  mutate(pct = n / sum(n)) %>% # pct within each OUTCOME
  ungroup()

# by relapse
mal_out <- df2 %>% ggplot() +
  geom_col(
    aes(x = MAL, y = pct, fill = OUTCOME),
    width = width,
    position = position_dodge(width = width)
  ) +
  scale_y_continuous(
    name = "% by relapse",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  coord_cartesian(
    ylim = c(0, 0.63)
  ) +
  scale_x_discrete(
    name = "Malnutrition",
    labels = c("Norm/mild", "Moderate", "Severe")
  ) +
  scale_fill_manual(
    name = "Denominator",
    values = c(cure_colour, relapse_colour),
    labels = c("Final cure", "Relapse")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.position.inside = c(0.8, 0.8),
    legend.background = element_rect(fill = alpha("white")),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.line.x = element_line()
  )

# relapse prob
relsum <- df %>%
  filter(!is.na(MAL)) %>%
  group_by(MAL) %>%
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

# Plot: probability with CI
mal_prob <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(MAL), y = p),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(MAL), ymin = ci_lower, ymax = ci_upper),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "% relapse",
    breaks = seq(0, 0.11, 0.01),
    limits = c(0, 0.11),
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Malnutrition",
    labels = c("Norm/mild", "Moderate", "Severe"),
    position = "top"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

# Plot: logodds with CI
mal_logodds <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(MAL), y = qlogis(p)),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(MAL), ymin = qlogis(ci_lower), ymax = qlogis(ci_upper)),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "log(odds) of relapse",
    # breaks = seq(0, 0.11, 0.01),
    # limits = c(0, 0.11),
    # labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Malnutrition",
    labels = c("Norm/mild", "Moderate", "Severe"),
    position = "bottom"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

mal_prob <- mal_prob + theme(axis.title.y = element_blank())
mal_out <- mal_out + theme(axis.title.y = element_blank())
mal_overall <- mal_overall + theme(axis.title.y = element_blank())

mal_comb <- mal_prob / mal_out / mal_overall
ggsave("figures/dist/catOut/mal_comb.pdf", mal_comb, width = 3.5, height = 10)


# ANAEMIA

# overall
anaemia_overall <- df %>%
  filter(!is.na(SEVERE_ANAEMIA)) %>%
  ggplot() +
  geom_bar(
    aes(x = SEVERE_ANAEMIA, y = after_stat(count / sum(count))),
    colour = "grey",
    width = width
  ) +
  scale_y_continuous(
    name = "Overall %",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    limit = c(0, 0.6)
  ) +
  coord_cartesian(
    ylim = c(0, 0.6)
  ) +
  scale_x_discrete(
    name = "Anaemia",
    labels = c("Non-severe", "Severe")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df2 <- df %>%
  filter(!is.na(SEVERE_ANAEMIA)) %>%
  count(OUTCOME, SEVERE_ANAEMIA) %>% # counts per outcome x sex
  group_by(OUTCOME) %>%
  mutate(pct = n / sum(n)) %>% # pct within each OUTCOME
  ungroup()

# by relapse
anaemia_out <- df2 %>% ggplot() +
  geom_col(
    aes(x = SEVERE_ANAEMIA, y = pct, fill = OUTCOME),
    width = width,
    position = position_dodge(width = width)
  ) +
  scale_y_continuous(
    name = "% by relapse",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  coord_cartesian(
    ylim = c(0, 0.63)
  ) +
  scale_x_discrete(
    name = "Anaemia",
    labels = c("Non-severe", "Severe")
  ) +
  scale_fill_manual(
    name = "",
    values = c(cure_colour, relapse_colour)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.line.x = element_line()
  )

# relapse prob
relsum <- df %>%
  filter(!is.na(SEVERE_ANAEMIA)) %>%
  group_by(SEVERE_ANAEMIA) %>%
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

# Plot: probability with CI
anaemia_prob <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(SEVERE_ANAEMIA), y = p),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(SEVERE_ANAEMIA), ymin = ci_lower, ymax = ci_upper),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "% relapse",
    breaks = seq(0, 0.11, 0.01),
    limits = c(0, 0.11),
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Anaemia",
    labels = c("Non-severe", "Severe"),
    position = "top"
  ) + # adjust if your coding differs
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

# Plot: probability with CI
anaemia_logodds <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(SEVERE_ANAEMIA), y = qlogis(p)),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(SEVERE_ANAEMIA), ymin = qlogis(ci_lower), ymax = qlogis(ci_upper)),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "log(odds) of relapse",
    # breaks = seq(0, 0.11, 0.01),
    # limits = c(0, 0.11),
    # labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Anaemia",
    labels = c("Non-severe", "Severe"),
    position = "bottom"
  ) + # adjust if your coding differs
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

anaemia_prob <- anaemia_prob + theme(axis.title.y = element_blank())
anaemia_out <- anaemia_out + theme(axis.title.y = element_blank())
anaemia_overall <- anaemia_overall + theme(axis.title.y = element_blank())

anaemia_comb <- anaemia_prob / anaemia_out / anaemia_overall
ggsave("figures/dist/catOut/anaemia_comb.pdf", anaemia_comb, width = 3.5, height = 10)

# TREATMENT

# overall
trt_overall <- df %>% ggplot() +
  geom_bar(
    aes(x = TREAT, y = after_stat(count / sum(count))),
    colour = "grey",
    width = width
  ) +
  scale_y_continuous(
    name = "Overall %",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    limit = c(0, 0.6)
  ) +
  coord_cartesian(
    ylim = c(0, 0.6)
  ) +
  scale_x_discrete(
    name = "Treatment",
    labels = c("MF", "Other", "SDA")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df2 <- df %>%
  count(OUTCOME, TREAT) %>% # counts per outcome x sex
  group_by(OUTCOME) %>%
  mutate(pct = n / sum(n)) %>% # pct within each OUTCOME
  ungroup()

# by relapse
trt_out <- df2 %>% ggplot() +
  geom_col(
    aes(x = TREAT, y = pct, fill = OUTCOME),
    width = width,
    position = position_dodge(width = width)
  ) +
  scale_y_continuous(
    name = "% by relapse",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  coord_cartesian(
    ylim = c(0, 0.63)
  ) +
  scale_x_discrete(
    name = "Treatment",
    labels = c("MF", "Other", "SDA")
  ) +
  scale_fill_manual(
    name = "",
    values = c(cure_colour, relapse_colour)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.line.x = element_line()
  )

# relapse prob
relsum <- df %>%
  filter(!is.na(TREAT)) %>%
  group_by(TREAT) %>%
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

# Plot: probability with CI
trt_prob <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(TREAT), y = p),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(TREAT), ymin = ci_lower, ymax = ci_upper),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "% relapse",
    breaks = seq(0, 0.11, 0.01),
    limits = c(0, 0.11),
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Treatment",
    labels = c("MF", "Other", "SDA"),
    position = "top"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

trt_logodds <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(TREAT), y = qlogis(p)),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(TREAT), ymin = qlogis(ci_lower), ymax = qlogis(ci_upper)),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "log(odds) of relapse",
    # breaks = seq(0, 0.11, 0.01),
    # limits = c(0, 0.11),
    # labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Treatment",
    labels = c("MF", "Other", "SDA"),
    position = "bottom"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

trt_prob <- trt_prob + theme(axis.title.y = element_blank())
trt_out <- trt_out + theme(axis.title.y = element_blank())
trt_overall <- trt_overall + theme(axis.title.y = element_blank())

trt_comb <- trt_prob / trt_out / trt_overall
ggsave("figures/dist/catOut/trt_comb.pdf", trt_comb, width = 4, height = 10)

# PARASITE COUNT

# overall
para_overall <- df %>%
  filter(!is.na(PARASITE)) %>%
  mutate(PARASITE = as.character(PARASITE)) %>%
  ggplot() +
  geom_bar(
    aes(x = PARASITE, y = after_stat(count / sum(count))),
    colour = "grey",
    width = width
  ) +
  scale_y_continuous(
    name = "Overall %",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    limit = c(0, 0.6)
  ) +
  coord_cartesian(
    ylim = c(0, 0.6)
  ) +
  scale_x_discrete(
    name = "Parasite grade",
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1+", "2+", "3+", "4+", "5+")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df2 <- df %>%
  mutate(PARASITE = as.character(PARASITE)) %>%
  filter(!is.na(PARASITE)) %>%
  count(OUTCOME, PARASITE) %>% # counts per outcome x sex
  group_by(OUTCOME) %>%
  mutate(pct = n / sum(n)) %>% # pct within each OUTCOME
  ungroup()

# by relapse
para_out <- df2 %>% ggplot() +
  geom_col(
    aes(x = PARASITE, y = pct, fill = OUTCOME),
    width = width,
    position = position_dodge(width = width)
  ) +
  scale_y_continuous(
    name = "% by relapse",
    labels = c("0", "20", "40", "60", "80", "100"),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  coord_cartesian(
    ylim = c(0, 0.63)
  ) +
  scale_x_discrete(
    name = "Parasite grade",
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1+", "2+", "3+", "4+", "5+")
  ) +
  scale_fill_manual(
    name = "Denominator",
    values = c(cure_colour, relapse_colour),
    labels = c("Final cure", "Relapse")
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.background = element_rect(fill = alpha("white")),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.line.x = element_line()
  )

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

# Plot: probability with CI
para_prob <- relsum %>%
  ggplot() +
  geom_point(
    aes(x = factor(PARASITE), y = p),
    size = 3
  ) +
  geom_errorbar(
    aes(x = factor(PARASITE), ymin = ci_lower, ymax = ci_upper),
    width = 0.05
  ) +
  scale_y_continuous(
    name = "% relapse",
    breaks = seq(0, 0.11, 0.01),
    limits = c(0, 0.11),
    labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  ) +
  scale_x_discrete(
    name = "Parasite grade",
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1+", "2+", "3+", "4+", "5+"),
    position = "top"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    # axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
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

para_prob <- para_prob + theme(axis.title.y = element_blank())
para_out <- para_out + theme(axis.title.y = element_blank())
para_overall <- para_overall + theme(axis.title.y = element_blank())

para_comb <- para_prob / para_out / para_overall
ggsave("figures/dist/catOut/para_comb.pdf", para_comb, width = 4.5, height = 10)

# combine all
library(cowplot)

cat_comb <- align_plots(sex_comb, mal_comb, anaemia_comb, trt_comb, para_comb)
out <- plot_grid(
  cat_comb[[1]], cat_comb[[2]], cat_comb[[3]], cat_comb[[4]], cat_comb[[5]],
  nrow = 1,
  align = "v",
  rel_widths = c(1, 1.3, 1, 1.3, 1.8)
)

ggsave("figures/dist/catOut/comb_cat.pdf", out, width = 13, height = 7)


# combine log odds
library(patchwork)
plots <- list(sex_logodds, mal_logodds, anaemia_logodds, trt_logodds, para_logodds)
plots <- lapply(
  plots,
  function(p) p + coord_cartesian(ylim = c(-3.8, -2.3)) + theme(axis.title.y = element_blank())
)
plots[[5]] <- plots[[5]] + coord_cartesian(ylim = c(-6, -2))
plots[[1]] <- plots[[1]] + theme(axis.title.y = element_text(angle = 90))

logodds_cat <- wrap_plots(plots) + plot_layout(nrow = 1)
saveRDS(logodds_cat, "data/logodds_cat.rds")
