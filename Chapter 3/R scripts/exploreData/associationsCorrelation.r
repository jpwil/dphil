# explore assocations in the raw data
# complete case only (not using imputed data)

library(tidyverse)
library(corrplot)
library(fastDummies)

rm(list = ls())

# load unscaled dataset
df <- readRDS("data/ads_ns_impute.rds")
df %>%
  names() %>%
  sort()

##########################
## CORRELATION MATRICES ##
##########################

df1 <- df %>%
  select(-c(STUDYID, OUT_DC_RELAPSE))

#
df2 <- df1 %>%
  mutate(
    MP_BL_SPLEEN_LENGTH = log(MP_BL_SPLEEN_LENGTH + 1),
    VL_DURATION = log(VL_DURATION)
  ) %>%
  rename(
    "Parasite density" = MB_COMBINED,
    "Spleen length (log)" = MP_BL_SPLEEN_LENGTH,
    "Symptom duration (log)" = VL_DURATION,
    "Age" = DM_AGE,
    "Haemoglobin" = LB_BL_HGB
  )

# note that the complete case subset is 1140 records out of 4387(!)
df3 <- df2[complete.cases(df1), ]
corrplot(cor(df2), method = "circle")

# using pairwise complete cases instead
corrplot(cor(df2, use = "pairwise.complete.obs"))

# load unscaled dataset
df <- readRDS("data/ads_ns_impute.rds")
df %>%
  names() %>%
  sort()
df_cor <- df %>%
  select(
    VS_BL_WEIGHT, VS_BL_HEIGHT, VL_DURATION, MP_BL_SPLEEN_LENGTH, LB_BL_HGB, MB_COMBINED, LB_BL_ALT, LB_BL_CREAT, LB_BL_AST, LB_BL_ALP,
    DM_SEX, VL_HISTORY, TREAT_GRP4, OUT_DC_RELAPSE
  ) %>%
  mutate(across(where(is.logical), as.numeric))

df_cor %>% View()

dummy_df_cor <- dummy_cols(df_cor, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
cor(dummy_df_cor, use = "pairwise.complete.obs") %>% corrplot()
