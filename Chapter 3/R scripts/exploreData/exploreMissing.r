# Explore missing data in final dataset
# See box 3 in Debray et al 2023
# This R script creates the missingness tables by dataset and by study
library(tidyverse)

rm(list = ls())
df <- readRDS("data/ads_summary.rds")

# the number of missing values per study and predictor
df %>% names()
df %>% count()

# number of missing
df_missing <- df %>%
  mutate(
    DATASET = case_when(
      STUDYID == 1 ~ "VAQMOU",
      STUDYID == 2 ~ "VDXALE",
      STUDYID == 3 ~ "VEZMZD",
      STUDYID == 4 ~ "VFEFCS",
      STUDYID == 5 ~ "VFETIZ",
      STUDYID == 6 ~ "VFFFOP",
      STUDYID == 7 ~ "VGKSTG",
      STUDYID == 8 ~ "VIVXJN",
      STUDYID == 9 ~ "VIZGFA",
      STUDYID == 10 ~ "VLAULV",
      STUDYID == 11 ~ "VLEALTT",
      STUDYID == 12 ~ "VLNXMEA",
      STUDYID == 13 ~ "VLZUKHR",
      STUDYID == 14 ~ "VQKRHN",
      STUDYID == 15 ~ "VRBQIF",
      STUDYID == 16 ~ "VSGPDL",
      STUDYID == 17 ~ "VLNAZSK/VVNGOE",
      STUDYID == 18 ~ "VWPJRM",
      STUDYID == 19 ~ "VYDSGR"
    ),
    STUDY_NAME = case_when(
      STUDYID == 1 ~ "Rijal 2010(A)",
      STUDYID == 2 ~ "Rijal 2003",
      STUDYID == 3 ~ "Sundar 2008(A)",
      STUDYID == 4 ~ "Sundar 2009",
      STUDYID == 5 ~ "Koirala 2003",
      STUDYID == 6 ~ "Chakraborty 2008",
      STUDYID == 7 ~ "Rijal 2010(B)",
      STUDYID == 8 ~ "Sundar 2014",
      STUDYID == 9 ~ "Bhattacharya 2007",
      STUDYID == 10 ~ "Sundar 2010",
      STUDYID == 11 ~ "Pandey 2017",
      STUDYID == 12 ~ "Das 2009",
      STUDYID == 13 ~ "Pandey 2016",
      STUDYID == 14 ~ "Sundar 2008(B)",
      STUDYID == 15 ~ "Sundar 2015",
      STUDYID == 16 ~ "Sundar 2019",
      STUDYID == 17 ~ "Sundar 2011",
      STUDYID == 18 ~ "Sundar 2007",
      STUDYID == 19 ~ "Sundar 2012"
    ),
    MAL_overall = ifelse(!is.na(BMI) | !is.na(BMIZ) | !is.na(WFHZ), "PRESENT", NA)
  ) %>%
  group_by(STUDYID) %>%
  mutate(n = n()) %>%
  select(
    STUDY_NAME, STUDYID, DATASET,
    Total = n,
    Sex = SEX_MALE,
    Age = AGE,
    `Fever duration` = FEVER_DURATION,
    Height = HEIGHT,
    Weight = WEIGHT,
    Malnutrition = MAL_overall,
    `Spleen size` = SPLEEN_LENGTH,
    WBC = LAB_WBC,
    Platelets = LAB_PLT,
    Creatinine = LAB_CREAT,
    ALT = LAB_ALT,
    Haemoglobin = LAB_HGB,
    Anaemia = SEVERE_ANAEMIA,
    `Parasite grade` = PARASITE,
    Treatment = TREAT,
    Outcome = OUTCOME
  )

df_missing %>% count(DATASET, STUDY_NAME)
df_missing %>%
  ungroup() %>%
  count()

# overall fraction of missing data


# number missing by study and predictor/outcome
df_missing_num <- df_missing %>%
  group_by(STUDYID, STUDY_NAME, DATASET, Total) %>%
  summarise(across(everything(), ~ sum(!is.na(.x)))) %>%
  ungroup()

write_csv(
  df_missing_num,
  file = "data/missing_num.csv"
)

# number missing by predictor/outcome across all studies
df_missing_pred <- df_missing_num %>%
  summarise(across(-c(STUDYID, DATASET, STUDY_NAME), ~ sum(.x))) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  rename(num_present = value) %>%
  mutate(pct_present = 100 * num_present / nrow(df))

write_csv(
  df_missing_pred,
  file = "data/missing_pred.csv"
)

df_missing_pred %>% summarise(mean = mean(pct_present))

# percentage missing by study and predictor/outcome
df_missing_pct <- df_missing %>%
  group_by(STUDYID, STUDY_NAME, DATASET, Total) %>%
  summarise(across(everything(), ~ 100 * sum(!is.na(.x)) / n())) %>%
  ungroup()

write_csv(
  df_missing_pct,
  file = "data/missing_pct.csv"
)

# number of missing variables per participant
df_missing %>% names()
df_missing_counts <- df_missing %>% select(-c(Weight, Height))
na_counts <- apply(df_missing_counts, 1, function(x) sum(is.na(x)))
missingness <- as_tibble(na_counts) %>%
  count(value) %>%
  mutate(pct = 100 * n / nrow(df))

write_csv(missingness, "data/missing_participant.csv")

# figures for missingness

##################
# CREATE FIGURES #
##################

# SIMPLE BAR CHART OF MISSINGNESS

var_order <- df_missing_pred %>%
  arrange(pct_present, desc(name)) %>%
  pull(name)
var_order <- var_order[var_order != "Total"]

df_missing_pred %>%
  arrange(pct_present, desc(name)) %>%
  filter(name != "Total") %>%
  mutate(
    name = factor(name, levels = unique(name))
  ) %>%
  ggplot() +
  geom_col(
    aes(y = name, x = pct_present)
  ) +
  scale_y_discrete(
    name = "Variable"
  ) +
  scale_x_continuous(
    name = "% not missing",
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  theme_minimal()

# 2D DENSITY PLOT BY STUDY

df_missing_pred
df_plot1_overall <- df_missing_pred %>%
  filter(name != "Total") %>%
  select(
    Variable = name,
    nm = pct_present
  ) %>%
  mutate(
    STUDY_NAME = "ZZ"
  )

df_plot1 <- df_missing_pct %>%
  select(-c(STUDYID, DATASET, Total)) %>%
  pivot_longer(
    cols = -STUDY_NAME,
    names_to = "Variable",
    values_to = "nm"
  )

df_plot1 <- bind_rows(df_plot1, df_plot1_overall)

df_plot2 <- df_plot1 %>%
  arrange(desc(STUDY_NAME), Variable) %>%
  mutate(
    STUDY_NAME = factor(STUDY_NAME, levels = unique(STUDY_NAME)),
    Variable = factor(Variable, levels = var_order),
    STUDY_NAME_NUM = as.numeric(STUDY_NAME),
    Variable_NUM = as.numeric(Variable)
  ) %>%
  mutate(
    STUDY_NAME_NUM = ifelse(STUDY_NAME_NUM == 1, 0.5, STUDY_NAME_NUM),
    STUDY_NAME = fct_recode(STUDY_NAME, "Overall" = "ZZ")
  )

df_plot2 <- df_plot2 %>%
  mutate(
    shape = case_when(
      nm == 100 ~ "A",
      nm == 0 ~ "B",
      .default = NA
    )
  )

# create marginal totals

missing <- df_plot2 %>% ggplot() +
  geom_point(
    aes(y = STUDY_NAME_NUM, x = Variable_NUM, fill = nm),
    size = 8,
    shape = 22
  ) +
  geom_point(
    aes(y = STUDY_NAME_NUM, x = Variable_NUM, shape = shape),
    size = 0.8,
    stroke = 0.8
  ) +
  scale_y_continuous(
    name = "Study",
    breaks = unique(df_plot2$STUDY_NAME_NUM),
    labels = levels(df_plot2$STUDY_NAME)
  ) +
  scale_x_continuous(
    name = "Variable",
    breaks = seq_along(unique(df_plot2$Variable_NUM)),
    labels = levels(df_plot2$Variable)
  ) +
  scale_fill_gradientn(
    name = "% not missing",
    colours = c("red", "yellow", "green"),
    breaks = seq(0, 100, 20),
    guide = guide_colorbar(
      frame.colour = "black", # legend border
      frame.linewidth = 0.2,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  ) +
  scale_shape_manual(
    values = c("A" = 1, "B" = 3, NA),
    labels = c("All present", "All missing", ""),
    name = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.08),
    panel.grid.minor = element_blank()
  ) +
  coord_fixed(ratio = 1)

ggsave("figures/missing/summary.pdf", missing, width = 6.3, height = 6.3)
