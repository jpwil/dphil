# explore associations in the raw data
# complete case only (not using imputed data)

##################
## WITH OUTCOME ##
##################

# I think the best way to visualise these associations is with a logistic GAM

library(tidyverse)
library(fastDummies)
library(mgcv)

rm(list = ls())

# load unscaled dataset
df <- readRDS("data/ads_ns_impute.rds")
# df <- readRDS("/Users/jameswilson/proj/vl_relapse_model_orig/results/mergedEA.rds")

# df <- df %>% filter(is.na(MB_BL_HIV) | MB_BL_HIV == FALSE)

## AGE

# 2D density plots
df %>% names()
p1 <- df %>%
  filter(!is.na(DM_AGE)) %>%
  ggplot() +
  geom_density(
    aes(x = DM_AGE, fill = OUT_DC_RELAPSE),
    alpha = 0.3
  ) +
  scale_x_continuous(
    name = "Age (years)"
  ) +
  scale_y_continuous(
    name = "Density"
  ) +
  scale_fill_manual(
    name = "Relapse",
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("TRUE" = "Yes", "FALSE" = "No")
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8), # x, y from 0 to 1
    legend.background = element_rect(fill = "white", color = "black")
  )

# Fit the model
df_age <- df %>%
  filter(!is.na(DM_AGE)) %>%
  mutate(STUDYID = factor(STUDYID))
gam_model <- gam(
  OUT_DC_RELAPSE ~ s(DM_AGE) + s(STUDYID, bs = "re"),
  data = df_age,
  family = binomial
)

# Create prediction data
newdata <- data.frame(DM_AGE = seq(min(df_age$DM_AGE), max(df_age$DM_AGE), length.out = 200))
newdata$STUDYID <- 0

predict(gam_model, newdata, type = "link", se.fit = TRUE)

newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
newdata %>% ggplot() +
  # geom_jitter(aes(x = DM_AGE, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(aes(x = DM_AGE, y = prob), color = "orange") +
  geom_ribbon(aes(x = DM_AGE, ymin = lower, ymax = upper), alpha = 0.2, fill = "orange") +
  scale_x_continuous(
    name = "Age (years)"
  ) +
  scale_y_continuous(
    name = "Probability of relapse (95% CI)"
  ) +
  theme_minimal()

ggplot(data = df_age) +
  geom_line(data = newdata, aes(x = DM_AGE, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = DM_AGE, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Age", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Age)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.15))

## ANAEMIA

# Fit the model
df1 <- df %>% filter(!is.na(LB_BL_HGB))
gam_model <- gam(OUT_DC_RELAPSE ~ s(LB_BL_HGB), data = df1, family = binomial)

# Create prediction data
newdata <- data.frame(LB_BL_HGB = seq(min(df1$LB_BL_HGB), max(df1$LB_BL_HGB), length.out = 200))
newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
ggplot(data = df1) +
  geom_jitter(aes(x = LB_BL_HGB, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(data = newdata, aes(x = LB_BL_HGB, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = LB_BL_HGB, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Haemoglobin", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Haemoglobin)"
  ) +
  theme_minimal()

ggplot(data = df1) +
  geom_line(data = newdata, aes(x = LB_BL_HGB, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = LB_BL_HGB, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Haemoglobin", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Haemoglobin)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.15))

## SYMPTOM DURATION

# Fit the model
df1 <- df %>% filter(!is.na(VL_DURATION) & VL_DURATION >= 10)
gam_model <- gam(OUT_DC_RELAPSE ~ s(VL_DURATION), data = df1, family = binomial)

# Create prediction data
newdata <- data.frame(VL_DURATION = seq(min(df1$VL_DURATION), max(df1$VL_DURATION), length.out = 200))
newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
ggplot(data = df1) +
  geom_jitter(aes(x = VL_DURATION, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(data = newdata, aes(x = VL_DURATION, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = VL_DURATION, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Symptom Duration", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Symptom Duration)"
  ) +
  theme_minimal()

ggplot(data = df1) +
  geom_line(data = newdata, aes(x = VL_DURATION, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = VL_DURATION, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Symptom Duration", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Symptom Duration)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.35))

# LOG SCALE
# Fit the model
df1 <- df %>%
  filter(!is.na(VL_DURATION) & VL_DURATION >= 10) %>%
  mutate(VL_DURATION = log(VL_DURATION + 1))
gam_model <- gam(OUT_DC_RELAPSE ~ s(VL_DURATION), data = df1, family = binomial)

# Create prediction data
newdata <- data.frame(VL_DURATION = seq(min(df1$VL_DURATION), max(df1$VL_DURATION), length.out = 200))
newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
ggplot(data = df1) +
  geom_jitter(aes(x = VL_DURATION, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(data = newdata, aes(x = VL_DURATION, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = VL_DURATION, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Symptom Duration", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Symptom Duration)"
  ) +
  theme_minimal()

ggplot(data = df1) +
  geom_line(data = newdata, aes(x = VL_DURATION, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = VL_DURATION, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Symptom Duration (log scale)", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(log(Symptom Duration))"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.4))

## BASELINE PARASITAEMIA

# GAM not possible with only 5 data points on the x axis
# let's just do this the old fashioned way

# overall
summary_df <- df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED > 0) %>%
  mutate(OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE)) %>%
  group_by(PD = as.factor(MB_COMBINED)) %>%
  summarise(
    n = n(),
    pos = sum(OUT_DC_RELAPSE),
    prop = mean(OUT_DC_RELAPSE),
    se = sqrt(prop * (1 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  )

ggplot(summary_df, aes(x = PD, y = prop)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Baseline parasitaemia",
    y = "Proportion with Relapse (95% CI)",
    title = "Proportion of Relapse by Baseline Parasitaemia"
  ) +
  theme_minimal()

# by HIV status
summary_df <- df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED > 0) %>%
  mutate(OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE)) %>%
  group_by(PD = as.factor(MB_COMBINED), HIV = MB_BL_HIV) %>%
  summarise(
    n = n(),
    pos = sum(OUT_DC_RELAPSE),
    prop = mean(OUT_DC_RELAPSE),
    se = sqrt(prop * (1 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  )

ggplot(summary_df, aes(x = PD, y = prop)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Baseline parasitaemia",
    y = "Proportion with Relapse (95% CI)",
    title = "Proportion of Relapse by Baseline Parasitaemia"
  ) +
  theme_minimal() +
  facet_wrap(~HIV)

# by age group
summary_df <- df %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED > 0, !is.na(DM_AGE)) %>%
  mutate(OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE)) %>%
  group_by(PD = as.factor(MB_COMBINED), age_group) %>%
  summarise(
    n = n(),
    pos = sum(OUT_DC_RELAPSE),
    prop = mean(OUT_DC_RELAPSE),
    se = sqrt(prop * (1 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  )

ggplot(summary_df, aes(x = PD, y = prop)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Baseline parasitaemia",
    y = "Proportion with Relapse (95% CI)",
    title = "Proportion of Relapse by Baseline Parasitaemia"
  ) +
  theme_minimal() +
  facet_wrap(~age_group)

# by study ID
summary_df <- df %>%
  filter(!(STUDYID %in% c("VFETIZ", "VLEALTT", "VLZUKHR", "VSGPDL", "VYDSGR"))) %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED > 0, !is.na(DM_AGE)) %>%
  mutate(OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE)) %>%
  group_by(PD = as.factor(MB_COMBINED), STUDYID) %>%
  summarise(
    n = n(),
    pos = sum(OUT_DC_RELAPSE),
    prop = mean(OUT_DC_RELAPSE),
    se = sqrt(prop * (1 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  )

ggplot(summary_df, aes(x = PD, y = prop)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Baseline parasitaemia",
    y = "Proportion with Relapse (95% CI)",
    title = "Proportion of Relapse by Baseline Parasitaemia"
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)

## SPLEEN LENGTH

# Fit the model
df1 <- df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH)) %>%
  mutate(MP_BL_SPLEEN_LENGTH = MP_BL_SPLEEN_LENGTH)
gam_model <- gam(OUT_DC_RELAPSE ~ s(MP_BL_SPLEEN_LENGTH), data = df1, family = binomial)

# Create prediction data
newdata <- data.frame(MP_BL_SPLEEN_LENGTH = seq(min(df1$MP_BL_SPLEEN_LENGTH), max(df1$MP_BL_SPLEEN_LENGTH), length.out = 200))
newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
ggplot(data = df1) +
  geom_jitter(aes(x = MP_BL_SPLEEN_LENGTH, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Spleen Length", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Spleen length)"
  ) +
  theme_minimal()

ggplot(data = df1) +
  geom_line(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Spleen Length", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Spleen length)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.2))

# LOG SCALE
# Fit the model
df1 <- df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH)) %>%
  mutate(MP_BL_SPLEEN_LENGTH = log(MP_BL_SPLEEN_LENGTH + 1))
gam_model <- gam(OUT_DC_RELAPSE ~ s(MP_BL_SPLEEN_LENGTH), data = df1, family = binomial)

# Create prediction data
newdata <- data.frame(MP_BL_SPLEEN_LENGTH = seq(min(df1$MP_BL_SPLEEN_LENGTH), max(df1$MP_BL_SPLEEN_LENGTH), length.out = 200))
newdata$fit <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$fit
newdata$se <- predict(gam_model, newdata, type = "link", se.fit = TRUE)$se.fit
newdata$prob <- plogis(newdata$fit)
newdata$lower <- plogis(newdata$fit - 1.96 * newdata$se)
newdata$upper <- plogis(newdata$fit + 1.96 * newdata$se)

# Plot
ggplot(data = df1) +
  geom_jitter(aes(x = MP_BL_SPLEEN_LENGTH, y = as.numeric(OUT_DC_RELAPSE)), height = 0.05, alpha = 0.3) +
  geom_line(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Spleen Length", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(Spleen length)"
  ) +
  theme_minimal()

ggplot(data = df1) +
  geom_line(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, y = prob), color = "blue") +
  geom_ribbon(data = newdata, aes(x = MP_BL_SPLEEN_LENGTH, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    x = "Spleen Length (log scale)", y = "Probability of Relapse",
    title = "GAM Fit with 95% CI: Relapse ~ s(log(Spleen length))"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.2))
