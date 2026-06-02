# explore assocations in the raw data
# complete case only (not using imputed data)

#############
# STRUCTURE #
#############

## AGE ##
## HAEMOGLOBIN LEVEL ##
## PARASITE DENSITY ##
## SPLEEN LENGTH ##

## AGE AND SPLEEN LENGTH ##
## AGE AND PARASITE DENSITY ##
## AGE AND SYMPTOM DURATION ##
## AGE AND HAEMOGLOBIN LEVEL ##

## SPLEEN LENGTH AND HAEMOGLOBIN LEVEL ##
## SPLEEN LENGTH AND PARASITE DENSITY ##
## SPLEEN LENGTH AND SYMPTOM DURATION ##

## PARASITE DENSITY AND SYMPTOM DURATION ##
## PARASITE DENSITY AND HAEMOGLOBIN LEVEL ##

## HAEMOGLOBIN LEVEL AND SYMPTOM DURATION ##

library(tidyverse)
library(corrplot)
library(fastDummies)

rm(list = ls())

# load unscaled dataset
df <- readRDS("data/ads_ns_impute.rds")
df <- readRDS("/Users/jameswilson/proj/vl_relapse_model_orig/results/mergedEA.rds")
# df <- df %>% filter(is.na(MB_BL_HIV) | MB_BL_HIV == FALSE)

df %>%
  names() %>%
  sort()

df %>% count(STUDYID)

# # filter variables
# df <- df %>%
#   select(
#     STUDYID,
#     OUT_DC_RELAPSE,
#     MB_COMBINED,
#     MP_BL_SPLEEN_LENGTH,
#     VL_DURATION,
#     DM_AGE,
#     LB_BL_HGB
#   )

#############################
## CONTINUOUS - CONTINUOUS ##
#############################

## AGE ##

# overall histogram
df %>%
  filter(!is.na(DM_AGE)) %>%
  ggplot() +
  geom_histogram(
    aes(x = DM_AGE),
    binwidth = 1
  ) +
  theme_minimal() +
  labs(x = "Age (years)", y = "Number of patients")

# overall histogram
df %>%
  filter(!is.na(DM_AGE)) %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  ggplot() +
  geom_histogram(
    aes(x = age_group),
    stat = "count"
  ) +
  theme_minimal() +
  labs(x = "Age group (years)", y = "Number of patients")

# overall histogram by HIV
df %>%
  filter(!is.na(DM_AGE)) %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  ggplot() +
  geom_bar(
    aes(x = age_group, fill = MB_BL_HIV)
  ) +
  theme_minimal() +
  labs(x = "Age group (years)", y = "Number of patients", fill = "HIV status")

# overall histogram by HIV
df %>%
  filter(!is.na(DM_AGE)) %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  ggplot() +
  geom_bar(
    aes(x = age_group, fill = MB_BL_HIV),
    position = "fill"
  ) +
  theme_minimal() +
  labs(x = "Age group (years)", y = "% of patients per group", fill = "HIV status")

# histogram across studies
df %>%
  filter(!is.na(DM_AGE)) %>%
  ggplot() +
  geom_histogram(
    aes(x = DM_AGE),
    binwidth = 1
  ) +
  theme_minimal() +
  labs(x = "Age (years)", y = "Number of patients") +
  facet_wrap(~STUDYID)

# histogram across studies less than 20 years old across outcome
df %>%
  filter(!is.na(DM_AGE) & DM_AGE < 20) %>%
  ggplot() +
  geom_histogram(
    aes(x = DM_AGE, fill = OUT_DC_RELAPSE),
    binwidth = 1,
    position = "stack"
  ) +
  theme_minimal() +
  labs(x = "Age (years)", y = "Number of patients") #+
# facet_wrap(~STUDYID)

## SPLEEN LENGTH ##

# overall histogram
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_SPLEEN_LENGTH)
  ) +
  theme_minimal()

# overall histogram log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot() +
  geom_histogram(
    aes(x = log(MP_BL_SPLEEN_LENGTH + 1))
  ) +
  theme_minimal()

# histogram across studies
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_SPLEEN_LENGTH)
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)

## SYMPTOM DURATION ##

# overall histogram
df %>%
  filter(!is.na(VL_DURATION)) %>%
  ggplot() +
  geom_histogram(
    aes(x = VL_DURATION)
  ) +
  theme_minimal()

df %>%
  filter(!is.na(VL_DURATION)) %>%
  mutate(VL_DURATIONs = log(VL_DURATION)) %>%
  ggplot() +
  geom_histogram(
    aes(x = VL_DURATIONs)
  ) +
  theme_minimal()

# by STUDYID
df %>%
  filter(!is.na(VL_DURATION)) %>%
  mutate(VL_DURATIONs = log(VL_DURATION)) %>%
  ggplot() +
  geom_histogram(
    aes(x = VL_DURATIONs)
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)

# by STUDYID
df %>%
  filter(!is.na(VL_DURATION)) %>%
  mutate(VL_DURATIONs = VL_DURATION) %>%
  ggplot() +
  geom_histogram(
    aes(x = VL_DURATIONs)
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)

## HAEMOGLOBIN LEVEL ##

# by STUDYID
df %>%
  filter(!is.na(LB_BL_HGB)) %>%
  mutate(VL_DURATIONs = log(LB_BL_HGB)) %>%
  ggplot() +
  geom_histogram(
    aes(x = LB_BL_HGB)
  ) +
  theme_minimal()

# by STUDYID
df %>%
  filter(!is.na(LB_BL_HGB)) %>%
  mutate(VL_DURATIONs = log(LB_BL_HGB)) %>%
  ggplot() +
  geom_histogram(
    aes(x = LB_BL_HGB)
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)

## PARASITE DENSITY ##

# missing data by STUDYID
df %>%
  mutate(
    missing = is.na(MB_COMBINED)
  ) %>%
  count(missing)

df %>%
  mutate(
    missing = is.na(MB_COMBINED)
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = STUDYID, fill = missing),
    position = "stack"
  ) +
  theme_minimal() +
  labs(x = "IDDO Study ID", y = "Number of IPD per study") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>%
  mutate(
    missing = is.na(MB_COMBINED)
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = STUDYID, fill = missing),
    position = "fill"
  ) +
  theme_minimal() +
  labs(x = "IDDO Study ID", y = "% of IPD per study") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## overall histograms
df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED != 0) %>%
  ggplot() +
  geom_histogram(
    aes(x = MB_COMBINED)
  ) +
  theme_minimal() +
  labs(x = "Baseline parasitaemia", y = "Number of patients")

# by STUDYID
df %>%
  filter(!is.na(MB_COMBINED)) %>%
  filter(!(STUDYID %in% c("VFETIZ", "VLEALTT", "VLZUKHR", "VSGPDL", "VYDSGR"))) %>%
  ggplot() +
  geom_histogram(
    aes(x = MB_COMBINED)
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID) +
  labs(x = "Baseline parasitaemia", y = "Number of patients")

# by HIV status
df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED != 0) %>%
  ggplot() +
  geom_bar(
    aes(x = MB_BL_HIV, fill = as.factor(MB_COMBINED))
  ) +
  theme_minimal() +
  labs(x = "HIV status", fill = "Baseline parasite density", y = "Number of patients")

# by HIV status
df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED != 0) %>%
  ggplot() +
  geom_bar(
    aes(x = MB_BL_HIV, fill = as.factor(MB_COMBINED)),
    position = "fill"
  ) +
  theme_minimal() +
  labs(x = "HIV status", fill = "Baseline parasite density", y = "% of patients")

# by HIV status
df %>%
  filter(!is.na(MB_COMBINED), MB_COMBINED != 0) %>%
  ggplot() +
  geom_bar(
    aes(x = MB_COMBINED),
    position = "stack"
  ) +
  theme_minimal() +
  labs(x = "Baseline parasite density", y = "Number of patients") +
  facet_wrap(~MB_BL_HIV)


# by age group
df %>%
  filter(!is.na(MB_COMBINED)) %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  count(age_group)

df %>%
  filter(!is.na(MB_COMBINED), !is.na(DM_AGE)) %>%
  mutate(age_group = cut(
    DM_AGE,
    breaks = c(-Inf, 10, 15, 25, 40, Inf),
    labels = c("[0, 5)", "[5, 15)", "[15, 25)", "[25, 40)", "[40, Inf)"),
    right = FALSE # means intervals are left-closed, right-open: [a, b)
  )) %>%
  ggplot() +
  geom_histogram(
    aes(x = MB_COMBINED)
  ) +
  theme_minimal() +
  facet_wrap(~age_group) +
  labs(x = "Age group", y = "Number of patients")


## COMBINATIONS - 10 IN TOTAL

## AGE AND SPLEEN LENGTH ##

## ISC
# As expected we see smaller spleen lengths in infants and children,
# and also a tailing off of spleen sizes over the age of 20 (seen across studies)

## EA
# Again, we see smaller spleen lengths in infants and children

df %>%
  filter(!is.na(DM_AGE) & !is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = MP_BL_SPLEEN_LENGTH),
    width = 0.5, height = 0.4
  )

# with loess
df %>%
  filter(!is.na(DM_AGE) & !is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot(aes(x = DM_AGE, y = MP_BL_SPLEEN_LENGTH)) +
  geom_jitter(
    width = 0.5, height = 0.4
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# with loess, by STUDYID
df %>%
  filter(!is.na(DM_AGE) & !is.na(MP_BL_SPLEEN_LENGTH) & STUDYID != "VFFFOP") %>%
  ggplot(aes(x = DM_AGE, y = MP_BL_SPLEEN_LENGTH)) +
  geom_jitter(
    width = 0.5, height = 0.4
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  facet_wrap(~STUDYID)

# log scale spleen
df %>%
  filter(!is.na(DM_AGE) & !is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = log(MP_BL_SPLEEN_LENGTH + 1)),
    width = 0.5, height = 0.4
  )

# with loess and log scale
df %>%
  filter(!is.na(DM_AGE) & !is.na(MP_BL_SPLEEN_LENGTH)) %>%
  ggplot(aes(x = DM_AGE, y = log(MP_BL_SPLEEN_LENGTH + 1))) +
  geom_jitter(
    width = 0.5, height = 0.4
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# relationship is non-linear, so interpret the correlation coefficient with caution
cor.test(df$DM_AGE, df$MP_BL_SPLEEN_LENGTH, use = "complete.obs", method = "pearson")

## AGE AND PARASITE DENSITY ##

## ISC
# no clear association between age and parasite density
# slightly higher parasite counts seen in older patients, but no significant correlation seen

## EA
# trend for < 10 years to have less parasites

df %>%
  filter(!is.na(DM_AGE) & !is.na(MB_COMBINED)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = MB_COMBINED),
    width = 0.5, height = 0.1
  )

df %>%
  filter(!is.na(DM_AGE) & !is.na(MB_COMBINED)) %>%
  ggplot() +
  geom_violin(
    aes(x = as.factor(MB_COMBINED), y = DM_AGE),
    fill = "skyblue",
    color = "black"
  )

df %>%
  filter(!is.na(DM_AGE) & !is.na(MB_COMBINED)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = as.factor(MB_COMBINED), y = DM_AGE),
    fill = "skyblue",
    color = "black"
  )

df %>%
  filter(!is.na(DM_AGE) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = log(MB_COMBINED), x = DM_AGE)) +
  geom_jitter(
    height = 0.1,
    width = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

df %>%
  filter(!is.na(DM_AGE) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = MB_COMBINED, x = DM_AGE)) +
  geom_jitter(
    height = 0.1,
    width = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

cor.test(df$DM_AGE, df$MB_COMBINED, use = "complete.obs", method = "pearson")

## AGE AND SYMPTOM DURATION ##

## ISC
# there is a slight trend for older patients to have longer durations of symptoms
# there is a weak, but significant correlation

## EA
# definite trend for older patients having longer duration of symptoms

df %>%
  filter(!is.na(DM_AGE) & !is.na(VL_DURATION)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = VL_DURATION),
    width = 0.5, height = 0.1
  )

# log scale
df %>%
  filter(!is.na(DM_AGE) & !is.na(VL_DURATION)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = log(VL_DURATION)),
    width = 0.5, height = 0.1
  )

# log scale and loess
df %>%
  filter(!is.na(DM_AGE) & !is.na(VL_DURATION)) %>%
  ggplot(aes(x = DM_AGE, y = log(VL_DURATION))) +
  geom_jitter(
    width = 0.5, height = 0.1
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

# log scale and loess and by STUDYID
df %>%
  filter(!is.na(DM_AGE) & !is.na(VL_DURATION)) %>%
  ggplot(aes(x = DM_AGE, y = log(VL_DURATION))) +
  geom_jitter(
    width = 0.5, height = 0.1
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  facet_wrap(~STUDYID)

cor.test(df$DM_AGE, df$VL_DURATION, use = "complete.obs", method = "pearson")
cor.test(df$DM_AGE, log(df$VL_DURATION), use = "complete.obs", method = "pearson")

## AGE AND HAEMOGLOBIN LEVEL ##

## ISC
# Not surprisingly, there is a marked positive correlation between age and haemoglobin level
# Rapidly increasing up to the age of ~ 20-25, but the continuing to increase slowly.
# There is a fairly strong and significant correlation; rho = 0.25 (0.22-0.28)

## EA
# Again, strong correlation with Hb increasing with age

df %>%
  filter(!is.na(DM_AGE) & !is.na(LB_BL_HGB)) %>%
  ggplot() +
  geom_jitter(
    aes(x = DM_AGE, y = LB_BL_HGB),
    width = 0.5, height = 0.5
  )

df %>%
  filter(!is.na(DM_AGE) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(x = DM_AGE, y = LB_BL_HGB)) +
  geom_jitter(
    width = 0.5, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

df %>%
  filter(!is.na(DM_AGE) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(x = DM_AGE, y = LB_BL_HGB)) +
  geom_jitter(
    width = 0.5, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  facet_wrap(~STUDYID)

cor.test(df$DM_AGE, log(df$LB_BL_HGB), use = "complete.obs", method = "pearson")

## SPLEEN LENGTH AND HAEMOGLOBIN LEVEL ##

## ISC
# Significant negative correlation between Hb and spleen length
# Patients with more severe anaemia have larger spleens
# rho = -0.26

## EA
# Significant negative correlation between Hb and spleen length seen again

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  )

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  facet_wrap(~STUDYID)

# spleen length on log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  )

# spleen length on log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

# spleen length on log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = LB_BL_HGB)) +
  geom_jitter(
    width = 0.1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  facet_wrap(~STUDYID)

cor.test(df$MP_BL_SPLEEN_LENGTH, df$LB_BL_HGB, use = "complete.obs", method = "pearson")
cor.test(log(1 + df$MP_BL_SPLEEN_LENGTH), df$LB_BL_HGB, use = "complete.obs", method = "pearson")

# SPLEEN LENGTH AND PARASITE DENSITY

## ISC
# Significant positive association - patients with higher parasite densities have larger spleens (on average)
# Correlation coefficient of approx 0.25.

## EA
# Again, significant association, higher parasite densities have larger spleens (on average)

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot() +
  geom_jitter(
    aes(y = MP_BL_SPLEEN_LENGTH, x = MB_COMBINED),
    width = 0.1, height = 0.2
  )

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = as.factor(MB_COMBINED))) +
  geom_boxplot(fill = "skyblue", colour = "black") +
  theme_minimal()

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = as.factor(MB_COMBINED))) +
  geom_boxplot(fill = "skyblue", colour = "black") +
  theme_minimal() +
  facet_wrap(~STUDYID)

# log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot() +
  geom_jitter(
    aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = MB_COMBINED),
    width = 0.1, height = 0.2
  )

# log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = MB_COMBINED)) +
  geom_jitter(
    width = 0.1, height = 0.2
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95)

# log scale, violins
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = as.factor(MB_COMBINED))) +
  geom_violin(fill = "skyblue", colour = "black") +
  theme_minimal()

# log scale, boxplot
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(MB_COMBINED)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = as.factor(MB_COMBINED))) +
  geom_boxplot(fill = "skyblue", colour = "black") +
  theme_minimal()

cor.test(df$MP_BL_SPLEEN_LENGTH, df$MB_COMBINED, use = "complete.obs", method = "pearson")
cor.test(log(df$MP_BL_SPLEEN_LENGTH + 1), df$MB_COMBINED, use = "complete.obs", method = "pearson")

## SPLEEN LENGTH AND SYMPTOM DURATION ##

## ISC
# patients with longer symptom durations have larger spleens
# this is a significant trend; rho = approx 0.25

## EA
# strong correlation - patients with longer symptom durations have larger spleens

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(VL_DURATION)) %>%
  ggplot() +
  geom_jitter(
    aes(y = MP_BL_SPLEEN_LENGTH, x = VL_DURATION),
    width = 0.1, height = 0.2
  ) +
  theme_minimal()

df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = MP_BL_SPLEEN_LENGTH, x = VL_DURATION)) +
  geom_jitter(
    width = 0.1, height = 0.2
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = VL_DURATION)) +
  geom_jitter(width = 0.1, height = 0.2) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = VL_DURATION)) +
  geom_jitter(width = 0.1, height = 0.2) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal() +
  facet_wrap(~STUDYID)

# log-log scale
df %>%
  filter(!is.na(MP_BL_SPLEEN_LENGTH) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = log(MP_BL_SPLEEN_LENGTH + 1), x = log(VL_DURATION))) +
  geom_jitter(width = 0.1, height = 0.2) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

cor.test(df$MP_BL_SPLEEN_LENGTH, df$VL_DURATION, use = "complete.obs", method = "pearson")
cor.test(log(df$MP_BL_SPLEEN_LENGTH + 1), df$VL_DURATION, use = "complete.obs", method = "pearson")
cor.test(log(df$MP_BL_SPLEEN_LENGTH + 1), log(df$VL_DURATION), use = "complete.obs", method = "pearson")

## PARASITE DENSITY AND SYMPTOM DURATION ##

## ISC
# This is interesting;..
# Patients with a longer duration of symptoms have higher parasite densities (on average)
# This is significant with a correlation coefficient of 0.15 (0.10 - 0.21) when symptom duration is on the log scale

## EA
# Strong correlation between longer duration of symptoms and higher parasite density

df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = MB_COMBINED, x = VL_DURATION)) +
  geom_jitter(
    width = 2, height = 0.2
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# log duration
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = MB_COMBINED, x = log(VL_DURATION))) +
  geom_jitter(
    width = 2, height = 0.2
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# log duration
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = MB_COMBINED, x = log(VL_DURATION))) +
  geom_jitter(
    width = 2, height = 0.2
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal() +
  facet_wrap(~STUDYID)

# parasite density on x axis and discrete...

# boxplot
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(x = as.factor(MB_COMBINED), y = VL_DURATION)) +
  geom_boxplot() +
  theme_minimal()

# boxplot & log scale
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(x = as.factor(MB_COMBINED), y = log(VL_DURATION))) +
  geom_boxplot() +
  theme_minimal()

# violin plot & log scale
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(VL_DURATION)) %>%
  ggplot(aes(x = as.factor(MB_COMBINED), y = log(VL_DURATION))) +
  geom_violin() +
  theme_minimal()

cor.test(df$MB_COMBINED, df$VL_DURATION, use = "complete.obs", method = "pearson")
cor.test(df$MB_COMBINED, log(df$VL_DURATION), use = "complete.obs", method = "pearson")

## PARASITE DENSITY AND HAEMOGLOBIN LEVEL ##

## ISC
# We see that patients with higher parasite densities overall are slightly more anaemia (lower Hb)
# This is significant with a correlation coefficient (rho) of -0.14 (-0.18 to -0.10)

## EA
# Again we see that patients with higher parasite densities are more anaemic

df %>%
  filter(!is.na(MB_COMBINED) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(x = MB_COMBINED, y = LB_BL_HGB)) +
  geom_jitter() +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# boxplot
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(x = as.factor(MB_COMBINED), y = LB_BL_HGB)) +
  geom_boxplot() +
  theme_minimal()

# violin plot
df %>%
  filter(!is.na(MB_COMBINED) & !is.na(LB_BL_HGB)) %>%
  ggplot(aes(x = as.factor(MB_COMBINED), y = LB_BL_HGB)) +
  geom_violin() +
  theme_minimal()

cor.test(df$MB_COMBINED, df$LB_BL_HGB, use = "complete.obs", method = "pearson")

## HAEMOGLOBIN LEVEL AND SYMPTOM DURATION ##

## ISC
# As expected, longer symptom duration associated with more severe anaemia
# Correlation coefficient of -0.13 (-0.16 to -0.09) (with duration on log scale)

## EA
# Longer symptom durations -> more severe anaemia (up to ~2 months then plateaus)

df %>%
  filter(!is.na(LB_BL_HGB) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = LB_BL_HGB, x = VL_DURATION)) +
  geom_jitter(
    width = 1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# by study
df %>%
  filter(!is.na(LB_BL_HGB) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = LB_BL_HGB, x = VL_DURATION)) +
  geom_jitter(
    width = 1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal() +
  facet_wrap(~STUDYID)

# log duration
df %>%
  filter(!is.na(LB_BL_HGB) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = LB_BL_HGB, x = log(VL_DURATION))) +
  geom_jitter(
    width = 1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal()

# by study
df %>%
  filter(!is.na(LB_BL_HGB) & !is.na(VL_DURATION)) %>%
  ggplot(aes(y = LB_BL_HGB, x = log(VL_DURATION))) +
  geom_jitter(
    width = 1, height = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  theme_minimal() +
  facet_wrap(~STUDYID)

cor.test(df$LB_BL_HGB, df$VL_DURATION, use = "complete.obs", method = "pearson")
cor.test(df$LB_BL_HGB, log(df$VL_DURATION), use = "complete.obs", method = "pearson")

## SOME EXTRA CORRELATIONS

# BILIRUBIN
df %>%
  filter(!is.na(LB_BL_BILI)) %>%
  filter(LB_BL_BILI < 100) %>%
  ggplot() +
  geom_histogram(
    aes(x = LB_BL_BILI)
  ) +
  theme_minimal()

df %>%
  filter(
    !is.na(LB_BL_BILI),
    LB_BL_BILI < 100,
    !is.na(LB_BL_HGB)
  ) %>%
  ggplot() +
  geom_jitter(
    aes(x = LB_BL_HGB, y = LB_BL_BILI)
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    aes(x = LB_BL_HGB, y = LB_BL_BILI)
  ) +
  theme_minimal()

# ALBUMIN
df %>%
  filter(!is.na(LB_BL_ALB)) %>%
  ggplot() +
  geom_histogram(
    aes(x = LB_BL_ALB)
  ) +
  theme_minimal()

df %>%
  filter(
    !is.na(LB_BL_ALB),
    !is.na(LB_BL_HGB)
  ) %>%
  ggplot() +
  geom_jitter(
    aes(x = LB_BL_HGB, y = LB_BL_ALB)
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    aes(x = LB_BL_HGB, y = LB_BL_ALB)
  ) +
  theme_minimal()

## look at difference in symptom duration between patients with and without HIV
df %>% ggplot() +
  geom_boxplot(
    aes(x = as.factor(MB_BL_HIV), y = log(VL_DURATION))
  ) +
  theme_minimal() +
  facet_wrap(~STUDYID)
