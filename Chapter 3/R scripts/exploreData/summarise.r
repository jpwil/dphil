# summarise predictors and outcome for presentation in publication

library(tidyverse)
rm(list = ls())

df <- readRDS("data/ads_ns_impute.rds")

df <- df %>%
  mutate(
    AGE_GROUP = case_when(
      DM_AGE < 5 ~ "Under 5",
      DM_AGE >= 5 & DM_AGE < 19 ~ "5-18 inclusive",
      DM_AGE >= 19 ~ "19 and over",
      .default = NA
    )
  )

# the number of missing values per study and predictor
df %>% names()
df %>% count()

## WRANGLE DATA
df <- df %>%
  arrange(STUDYID) %>%
  mutate(STUDYID = as.numeric(factor(STUDYID))) %>%
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
    BMI_overall = ifelse(!is.na(VS_BMI) & !is.na(ZZ_BMI_Z), TRUE, NA)
  ) %>%
  group_by(STUDYID) %>%
  mutate(n = n())



df <- df %>%
  ungroup() %>%
  mutate(
    ZZ_BMI_Z_GRP = case_when(
      DM_AGE >= 5 & DM_AGE < 19 & ZZ_BMI_Z > -2 & !is.na(ZZ_BMI_Z) ~ "Mild_norm",
      DM_AGE >= 5 & DM_AGE < 19 & ZZ_BMI_Z <= -2 & ZZ_BMI_Z > -3 & !is.na(ZZ_BMI_Z) ~ "Moderate",
      DM_AGE >= 5 & DM_AGE < 19 & ZZ_BMI_Z <= -3 & !is.na(ZZ_BMI_Z) ~ "Severe",
      .default = NA
    ),
    ZZ_WFH_Z_GRP = case_when(
      DM_AGE < 5 & VS_WFH_Z_u5 > -2 ~ "Mild_norm",
      DM_AGE < 5 & VS_WFH_Z_u5 <= -2 & VS_WFH_Z_u5 > -3 ~ "Moderate",
      DM_AGE < 5 & VS_WFH_Z_u5 <= -3 ~ "Severe",
      .default = NA
    ),
    ZZ_BMI_Z_GRP = factor(ZZ_BMI_Z_GRP, levels = c("Mild_norm", "Moderate", "Severe")),
    ZZ_BMIs_GRP = case_when(
      DM_AGE >= 5 & DM_AGE >= 19 & VS_BMI >= 18.5 & !is.na(VS_BMI) ~ "Mild_norm",
      DM_AGE >= 5 & DM_AGE >= 19 & VS_BMI < 18.5 & VS_BMI >= 16 & !is.na(VS_BMI) ~ "Moderate",
      DM_AGE >= 5 & DM_AGE >= 19 & VS_BMI < 16 & !is.na(VS_BMI) ~ "Severe",
      .default = NA
    ),
    ZZ_BMIs_GRP = factor(ZZ_BMIs_GRP, levels = c("Mild_norm", "Moderate", "Severe")),
    ZZ_MAL = case_when(
      DM_AGE >= 19 ~ ZZ_BMIs_GRP,
      DM_AGE >= 5 & DM_AGE < 19 ~ ZZ_BMI_Z_GRP,
      DM_AGE < 5 ~ ZZ_WFH_Z_GRP,
      .default = NA
    ),
    LB_BL_HGB_GRP1 = case_when(
      DM_AGE < 2 & LB_BL_HGB < 70 ~ "Severe",
      DM_AGE < 2 & LB_BL_HGB >= 70 & LB_BL_HGB < 95 ~ "Moderate",
      DM_AGE < 2 & LB_BL_HGB >= 95 & LB_BL_HGB < 105 ~ "Mild",
      DM_AGE < 2 & LB_BL_HGB >= 105 ~ "No",
      DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB < 70 ~ "Severe",
      DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 70 & LB_BL_HGB < 100 ~ "Moderate",
      DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 100 & LB_BL_HGB < 110 ~ "Mild",
      DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 110 ~ "No",
      DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB < 80 ~ "Severe",
      DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 80 & LB_BL_HGB < 110 ~ "Moderate",
      DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 110 & LB_BL_HGB < 115 ~ "Mild",
      DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 115 ~ "No",
      DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB < 80 ~ "Severe",
      DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110 ~ "Moderate",
      DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 120 ~ "Mild",
      DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 120 ~ "No",
      DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB < 80 ~ "Severe",
      DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110 ~ "Moderate",
      DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 130 ~ "Mild",
      DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 130 ~ "No",
      DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB < 80 ~ "Severe",
      DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110 ~ "Moderate",
      DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 120 ~ "Mild",
      DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 120 ~ "No",
      .default = NA
    ),
    LB_BL_HGB_GRP3 = case_when(
      LB_BL_HGB_GRP1 %in% c("Severe") ~ 1,
      LB_BL_HGB_GRP1 %in% c("Moderate", "Mild", "No") ~ 0,
      .default = NA
    )
  )

df <- df %>%
  mutate(
    VS_WFH_Z_u5 = ifelse(DM_AGE < 5 & !is.na(DM_AGE), VS_WFH_Z_u5, NA),
    ZZ_BMI_Z = ifelse(DM_AGE >= 5 & DM_AGE < 19 & !is.na(DM_AGE), ZZ_BMI_Z, NA),
    VS_BMI = ifelse(DM_AGE >= 19 & !is.na(DM_AGE), VS_BMI, NA)
  )

df <- df %>%
  select(
    STUDYID, DATASET, STUDY_NAME, OUT_DC_RELAPSE, TREAT_GRP4, VS_BL_HEIGHT,
    VS_BL_WEIGHT, VS_BMI, ZZ_BMI_Z, ZZ_BMI_Z_GRP, ZZ_BMIs_GRP, VS_WFH_Z_u5, ZZ_WFH_Z_GRP,
    ZZ_MAL, DM_AGE, AGE_GROUP, DM_SEX, MP_BL_SPLEEN_LENGTH, VL_DURATION, LB_BL_HGB_GRP3,
    LB_BL_WBC, LB_BL_PLAT, LB_BL_HGB, LB_BL_ALT, LB_BL_CREAT,
    MB_COMBINED, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN
  ) %>%
  relocate(
    STUDYID, DATASET, STUDY_NAME, OUT_DC_RELAPSE, TREAT_GRP4, VS_BL_HEIGHT,
    VS_BL_WEIGHT, VS_BMI, ZZ_BMI_Z, ZZ_BMI_Z_GRP, ZZ_BMIs_GRP, VS_WFH_Z_u5, ZZ_WFH_Z_GRP,
    ZZ_MAL, MP_BL_SPLEEN_LENGTH, VL_DURATION, LB_BL_HGB_GRP3,
    LB_BL_WBC, LB_BL_PLAT, LB_BL_HGB, LB_BL_ALT, LB_BL_CREAT,
    MB_COMBINED, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN
  )

# 19 and over: overall 2154, 841/2154 (39.0%) have missing BMI
# 19 and over relapse: 97, 37/97 (38.1%) have missing BMI
# 19 and over not relapse:  2057, 804/2057 (39.1%) have missing BMI

# 5-18 inclusive: overall: 2301 963/2301 (41.9%) have missing BMI-for-age z-score
# 5-18 inclusive: relapse: 125 60/125 (48.0%) have missing BMI-for-age z-score
# 5-18 inclusive: not relapse 2176, 903/2176 (41.5%) have missing BMI-for-age z-score

# Under 5: overall 138, 111/138 (80.4%) have missing WFH-z-score
# Under 5: relapse: overall 6, 3/6 (50%) have missing WFH-z-score
# Under 5: not relapse: overall 132, 108/132 (81.8%) have missing WFH-z-score



# df %>% View()
df %>%
  count(STUDY_NAME) %>%
  arrange(STUDY_NAME) # %>% View()

## SUMMARISE DATA
df_sum <- df %>%
  mutate(
    TREAT_SDA = TREAT_GRP4 == "SDA",
    TREAT_MF = TREAT_GRP4 == "MF",
    TREAT_OTHER = TREAT_GRP4 == "Other",
    MAL_SEVERE = ZZ_MAL == "Severe",
    MAL_MODERATE = ZZ_MAL == "Moderate",
    MAL_MILD_NORM = ZZ_MAL == "Mild_norm", ,
    PARASITE_SOURCE_BONE = ifelse(MB_BL_LSHMANIA_BONE >= 1, TRUE, NA),
    PARASITE_SOURCE_SPLEEN = ifelse(MB_BL_LSHMANIA_SPLEEN >= 1, TRUE, NA),
    PARASITE_1 = MB_COMBINED == 1,
    PARASITE_2 = MB_COMBINED == 2,
    PARASITE_3 = MB_COMBINED == 3,
    PARASITE_4 = MB_COMBINED == 4,
    PARASITE_5 = MB_COMBINED == 5
  ) %>%
  select(-c(ZZ_BMI_Z_GRP, ZZ_BMIs_GRP))

# tidy up parasite density
df_sum <- df_sum %>%
  mutate(MB_BL_LSHMANIA_BONE = ifelse(MB_BL_LSHMANIA_BONE == 0, NA, MB_BL_LSHMANIA_BONE))
df_sum %>% count(MB_COMBINED, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN)
df_sum %>% glimpse()

df_sum <- df_sum %>%
  rename(
    MAL = ZZ_MAL,
    TREAT = TREAT_GRP4,
    OUTCOME = OUT_DC_RELAPSE,
    HEIGHT = VS_BL_HEIGHT,
    WEIGHT = VS_BL_WEIGHT,
    BMI = VS_BMI,
    BMIZ = ZZ_BMI_Z,
    WFHZ = VS_WFH_Z_u5,
    SPLEEN_LENGTH = MP_BL_SPLEEN_LENGTH,
    FEVER_DURATION = VL_DURATION,
    SEVERE_ANAEMIA = LB_BL_HGB_GRP3,
    LAB_WBC = LB_BL_WBC,
    LAB_HGB = LB_BL_HGB,
    LAB_ALT = LB_BL_ALT,
    LAB_CREAT = LB_BL_CREAT,
    LAB_PLT = LB_BL_PLAT,
    PARASITE = MB_COMBINED,
    AGE = DM_AGE,
    SEX_MALE = DM_SEX
  ) %>%
  mutate(
    across(c(SEX_MALE, SEVERE_ANAEMIA), ~ as.logical(.x))
  )

df_sum %>% glimpse()
saveRDS(df_sum, "data/ads_summary.rds")

#############################
# CONVERT TO SUMMARY TABLES #
#############################

df_sum1 <- df_sum %>%
  group_by(STUDYID, DATASET, STUDY_NAME) %>%
  summarise(
    n = n(),
    across(
      where(is.logical), # SEVERE_ANAEMIA, OUTCOME, PARASITE_BONE, PARASITE_SPLEEN, SEX, TREAT_*, MAL_*
      list(
        TOTAL = ~ sum(.x, na.rm = TRUE),
        TOTAL_PCT = ~ 100 * sum(.x, na.rm = TRUE) / n,
        MISSING = ~ sum(is.na(.x)),
        MISSING_PCT = ~ 100 * sum(is.na(.x)) / n
      ),
      .names = "{.col}_{.fn}"
    ),
    across(
      c(AGE, HEIGHT, WEIGHT, BMI, BMIZ, WFHZ, SPLEEN_LENGTH, FEVER_DURATION, PARASITE, starts_with("LAB_")),
      list(
        MISSING = ~ sum(is.na(.x)),
        MISSING_PCT = ~ 100 * sum(is.na(.x)) / n,
        MEAN = ~ mean(.x, na.rm = TRUE),
        MIN = ~ quantile(.x, probs = 0, na.rm = TRUE),
        LQ = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        MEDIAN = ~ quantile(.x, probs = 0.5, na.rm = TRUE),
        UQ = ~ quantile(.x, probs = 0.75, na.rm = TRUE),
        MAX = ~ quantile(.x, probs = 1, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

stat_suffixes <- c(
  "MISSING", "MISSING_PCT", "MEAN", "MIN", "LQ",
  "MEDIAN", "UQ", "MAX", "TOTAL", "TOTAL_PCT"
)
suffix_pattern <- paste(stat_suffixes, collapse = "|")

df_sum_long1 <- df_sum1 %>%
  pivot_longer(
    cols = matches(paste0("_(?:", suffix_pattern, ")$")),
    names_to = c("VARIABLE", "STATISTIC"),
    names_pattern = paste0("^(.*)_(", suffix_pattern, ")$"),
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = value
  )

df_sum_long1

saveRDS(df_sum_long1, "data/summaryStudy.rds")
write_csv(df_sum_long1, "data/summaryStudy.csv")

### categoery table overall
df_sum2_cat_ov <- df_sum %>%
  select(-c(HEIGHT:FEVER_DURATION, LAB_WBC:PARASITE)) %>%
  ungroup() %>%
  mutate(
    SEX_FEMALE = !SEX_MALE,
    SEVERE_ANAEMIA_FALSE = !SEVERE_ANAEMIA,
  ) %>%
  rename(SEVERE_ANAEMIA_TRUE = SEVERE_ANAEMIA) %>%
  summarise(
    n = n(),
    across(
      where(is.logical), # SEVERE_ANAEMIA, OUTCOME, PARASITE_BONE, PARASITE_SPLEEN, SEX, TREAT_*, MAL_*
      list(
        TOTAL = ~ sum(.x, na.rm = TRUE),
        TOTAL_PCT = ~ 100 * sum(.x, na.rm = TRUE) / n,
        MISSING_TOTAL = ~ sum(is.na(.x)),
        MISSING_TOTAL_PCT = ~ 100 * sum(is.na(.x)) / n
      ),
      .names = "{.col}_{.fn}"
    )
  )

stat_suffixes <- c("TOTAL", "TOTAL_PCT")
suffix_pattern <- paste(stat_suffixes, collapse = "|")

df_sum2_cat_ov_long <- df_sum2_cat_ov %>%
  pivot_longer(
    cols = matches(paste0("_(?:", suffix_pattern, ")$")),
    names_to = c("VARIABLE", "STATISTIC"),
    names_pattern = paste0("^(.*)_(", suffix_pattern, ")$"),
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = value
  )

df_sum2_cat_ov_long <- df_sum2_cat_ov_long %>% arrange(VARIABLE)
# df_sum2_cat_ov_long %>% View()

### categoery table by outcome
df_sum2_cat_out <- df_sum %>%
  select(-c(HEIGHT:FEVER_DURATION, LAB_WBC:PARASITE)) %>%
  ungroup() %>%
  mutate(
    SEX_FEMALE = !SEX_MALE,
    SEVERE_ANAEMIA_FALSE = !SEVERE_ANAEMIA,
  ) %>%
  rename(SEVERE_ANAEMIA_TRUE = SEVERE_ANAEMIA) %>%
  group_by(OUTCOME) %>%
  summarise(
    n = n(),
    across(
      where(is.logical), # SEVERE_ANAEMIA, OUTCOME, PARASITE_BONE, PARASITE_SPLEEN, SEX, TREAT_*, MAL_*
      list(
        TOTAL = ~ sum(.x, na.rm = TRUE),
        TOTAL_PCT = ~ 100 * sum(.x, na.rm = TRUE) / n,
        MISSING_TOTAL = ~ sum(is.na(.x)),
        MISSING_TOTAL_PCT = ~ 100 * sum(is.na(.x)) / n
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    OUTCOME = ifelse(OUTCOME, "R", "NR")
  )

stat_suffixes <- c("TOTAL", "TOTAL_PCT")
suffix_pattern <- paste(stat_suffixes, collapse = "|")

df_sum2_cat_out_long <- df_sum2_cat_out %>%
  pivot_longer(
    cols = matches(paste0("_(?:", suffix_pattern, ")$")),
    names_to = c("VARIABLE", "STATISTIC"),
    names_pattern = paste0("^(.*)_(", suffix_pattern, ")$"),
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = value
  ) %>%
  pivot_wider(
    id_cols = VARIABLE,
    names_from = OUTCOME,
    values_from = c(TOTAL, TOTAL_PCT),
    names_vary = "slowest"
  )

df_sum2_cat_out_long <- df_sum2_cat_out_long %>% arrange(VARIABLE)
df_sum2_cat_out_long

df_cat_master <- df_sum2_cat_ov_long %>% full_join(df_sum2_cat_out_long)
df_cat_master <- df_cat_master %>%
  filter(TOTAL != 0) %>%
  filter(!VARIABLE %in% c("SEVERE_ANAEMIA_FALSE_MISSING", "PARASITE_SOURCE_SPLEEN_MISSING", "PARASITE_SOURCE_BONE_MISSING", "MAL_MILD_NORM_MISSING", "MAL_MODERATE_MISSING"))

df_cat_master <- df_cat_master %>%
  mutate(
    across(
      c(TOTAL_PCT, TOTAL_PCT_NR, TOTAL_PCT_R),
      ~ sprintf("%.1f", .x)
    ),
    across(
      c(TOTAL, TOTAL_NR, TOTAL_R),
      ~ format(.x, big.mark = ",", scientific = FALSE)
    )
  ) %>%
  mutate(
    overall = paste0(TOTAL, " (", TOTAL_PCT, ")"),
    relapse = paste0(TOTAL_R, " (", TOTAL_PCT_R, ")"),
    no_relapse = paste0(TOTAL_NR, " (", TOTAL_PCT_NR, ")")
  )

df_cat_master %>%
  select(VARIABLE, overall, no_relapse, relapse) %>%
  write_csv(file = "data/SummaryCat.csv")

library(xtable)
xtable(df_cat_master %>%
  select(VARIABLE, overall, no_relapse, relapse))

#### continuous table overall
df_cont_ov <- df_sum %>%
  select(-(SEX_MALE:PARASITE_5)) %>%
  ungroup() %>%
  summarise(
    n = n(),
    across(
      c(AGE, HEIGHT, WEIGHT, BMI, BMIZ, WFHZ, SPLEEN_LENGTH, FEVER_DURATION, PARASITE, starts_with("LAB_")),
      list(
        MISSING = ~ sum(is.na(.x)),
        MISSING_PCT = ~ 100 * sum(is.na(.x)) / n,
        MEAN = ~ mean(.x, na.rm = TRUE),
        MIN = ~ quantile(.x, probs = 0, na.rm = TRUE),
        LQ = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        MEDIAN = ~ quantile(.x, probs = 0.5, na.rm = TRUE),
        UQ = ~ quantile(.x, probs = 0.75, na.rm = TRUE),
        MAX = ~ quantile(.x, probs = 1, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

stat_suffixes <- c(
  "MISSING", "MISSING_PCT", "MEAN", "MIN", "LQ",
  "MEDIAN", "UQ", "MAX", "TOTAL", "TOTAL_PCT"
)
suffix_pattern <- paste(stat_suffixes, collapse = "|")

df_cont_ov_long <- df_cont_ov %>%
  pivot_longer(
    cols = matches(paste0("_(?:", suffix_pattern, ")$")),
    names_to = c("VARIABLE", "STATISTIC"),
    names_pattern = paste0("^(.*)_(", suffix_pattern, ")$"),
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = value
  )

#### continuous table by outcome
df_cont_out <- df_sum %>%
  select(-(SEX_MALE:PARASITE_5)) %>%
  ungroup() %>%
  group_by(OUTCOME) %>%
  summarise(
    n = n(),
    across(
      c(AGE, HEIGHT, WEIGHT, BMI, BMIZ, WFHZ, SPLEEN_LENGTH, FEVER_DURATION, PARASITE, starts_with("LAB_")),
      list(
        MISSING = ~ sum(is.na(.x)),
        MISSING_PCT = ~ 100 * sum(is.na(.x)) / n,
        MEAN = ~ mean(.x, na.rm = TRUE),
        MIN = ~ quantile(.x, probs = 0, na.rm = TRUE),
        LQ = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        MEDIAN = ~ quantile(.x, probs = 0.5, na.rm = TRUE),
        UQ = ~ quantile(.x, probs = 0.75, na.rm = TRUE),
        MAX = ~ quantile(.x, probs = 1, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    OUTCOME = ifelse(OUTCOME, "R", "NR")
  )


stat_suffixes <- c(
  "MISSING", "MISSING_PCT", "MEAN", "MIN", "LQ",
  "MEDIAN", "UQ", "MAX", "TOTAL", "TOTAL_PCT"
)
suffix_pattern <- paste(stat_suffixes, collapse = "|")

df_cont_out_long <- df_cont_out %>%
  pivot_longer(
    cols = matches(paste0("_(?:", suffix_pattern, ")$")),
    names_to = c("VARIABLE", "STATISTIC"),
    names_pattern = paste0("^(.*)_(", suffix_pattern, ")$"),
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = STATISTIC,
    values_from = value
  ) %>%
  pivot_wider(
    id_cols = VARIABLE,
    names_from = OUTCOME,
    values_from = MISSING:MAX,
    names_vary = "slowest"
  )

df_cont_master <- df_cont_ov_long %>% full_join(df_cont_out_long)
df_cont_master <- df_cont_master %>%
  mutate(
    across(
      c(MISSING_PCT, MISSING_PCT_NR, MISSING_PCT_R),
      ~ sprintf("%.1f", .x)
    ),
    across(
      c(MISSING, MISSING_NR, MISSING_R),
      ~ format(.x, big.mark = ",", scientific = FALSE)
    )
  ) %>%
  mutate(
    missing_overall = paste0(MISSING, " (", MISSING_PCT, ")"),
    range_overall = paste0(MIN, " -- ", MAX),
    median_overall = paste0(MEDIAN, " (", LQ, " -- ", UQ, ")"),
    missing_nr = paste0(MISSING_NR, " (", MISSING_PCT_NR, ")"),
    range_nr = paste0(MIN_NR, " -- ", MAX_NR),
    median_nr = paste0(MEDIAN_NR, " (", LQ_NR, " -- ", UQ_NR, ")"),
    missing_r = paste0(MISSING_R, " (", MISSING_PCT_R, ")"),
    range_r = paste0(MIN_R, " -- ", MAX_R),
    median_r = paste0(MEDIAN_R, " (", LQ_R, " -- ", UQ_R, ")")
  ) %>%
  select(
    VARIABLE, median_overall, range_overall, missing_overall, median_nr, missing_nr, median_r, missing_r
  )

xtable(df_cont_master)

df %>% names()
df %>% count(AGE_GROUP, is.na(VS_BMI))
