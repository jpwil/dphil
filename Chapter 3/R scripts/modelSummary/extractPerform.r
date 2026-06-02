## extract model summaries
## JANUARY 2026
rm(list = ls())
library(tidyverse)

# m <- readRDS("results-share/20260104_WITH_PD.rds")
# m <- readRDS("results-share/20260104_WITHOUT_PD.rds")
# m <- readRDS("results-share/202601031334_WITH_PD_BOOT_1.rds")
m <- readRDS("results-share/202605_WITH_PD_BOOT.rds")

m %>% length()
m[[1]] %>% names()
m[[1]]$call$seed
m[[1]]$call

df <- data.frame(
  terms = character(),
  ci_re_ap = numeric(),
  ci_re_bt = numeric(),
  ci_fe_ap = numeric(),
  ci_fe_bt = numeric(),
  cal_slope_brms_ap = numeric(),
  cal_slope_brms_bt = numeric(),
  cal_slope_fe_ap = numeric(),
  cal_slope_fe_bt = numeric(),
  cal_int_re_ap = numeric(),
  cal_int_re_bt = numeric(),
  seed = numeric()
)

safe <- function(x) if (is.null(x) || length(x) == 0) NA else x
for (i in seq_along(m)) {
  df[[i, 1]] <- safe(m[[i]]$finalVariables$term %>% paste(collapse = ", ")) # final terms
  df[[i, 2]] <- safe(m[[i]]$evalCI_AP)
  df[[i, 3]] <- safe(m[[i]]$evalCI_BT)
  df[[i, 4]] <- safe(m[[i]]$evalCI_AP_FE)
  df[[i, 5]] <- safe(m[[i]]$evalCI_BT_FE)
  df[[i, 6]] <- safe(m[[i]]$evalCal_SlopeAP_BRMS)
  df[[i, 7]] <- safe(m[[i]]$evalCal_SlopeBT_BRMS)
  df[[i, 8]] <- safe(m[[i]]$evalCal_SlopeAP)
  df[[i, 9]] <- safe(m[[i]]$evalCal_SlopeBT)
  df[[i, 10]] <- safe(m[[i]]$evalCal_IntAP)
  df[[i, 11]] <- safe(m[[i]]$evalCal_IntBT)
  df[[i, 12]] <- safe(as.numeric(m[[i]]$call$seed))
}

# df %>%
#   relocate(seed, ci_fe_ap, terms) %>%
#   arrange(ci_fe_ap) %>%
#   View()

# CI

df$ci_re_ap %>% hist()
df$ci_fe_ap %>% hist()

mean(df$ci_fe_ap)

df %>%
  mutate(temp = ci_fe_ap - ci_re_ap) %>%
  pull(temp) %>%
  hist()

mean(df$ci_fe_ap - df$ci_fe_bt)

# # SLOPE

# df$cal_slope_brms_ap %>% hist()
# df$cal_slope_brms_bt %>% hist()

# df %>%
#   mutate(temp = cal_slope_brms_ap - cal_slope_brms_bt) %>%
#   pull(temp) %>%
#   hist()

# df %>%
#   mutate(temp = cal_slope_brms_ap - cal_slope_brms_bt) %>%
#   summarise(mean = mean(temp))


df$cal_slope_fe_ap %>% hist()
df$cal_slope_fe_bt %>% hist()

df %>%
  mutate(temp = cal_slope_fe_ap - cal_slope_fe_bt) %>%
  pull(temp) %>%
  hist()

df %>%
  mutate(temp = cal_slope_fe_ap - cal_slope_fe_bt) %>%
  summarise(mean = mean(temp))

# INTERCEPT

df$cal_int_re_ap %>% hist()
df$cal_int_re_bt %>% hist()

df %>%
  mutate(temp = cal_int_re_ap - cal_int_re_bt) %>%
  pull(temp) %>%
  hist()

df %>%
  mutate(temp = cal_int_re_ap - cal_int_re_bt) %>%
  summarise(mean = mean(temp))

# df$cal_int_re_ap %>% hist()
