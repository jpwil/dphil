# explore model summary files

library(tidyverse)
library(mice)
library(lme4)

files <- dir("results-share")
files

# df <- readRDS("data/ads_impute_small.rds")
# df %>% count()
# df %>% count(OUT_DC_RELAPSE)

# LOOK AT SPECIFIC MODEL

prog <- readRDS("results-share/20251005_WITH_PD_SMALL_originalModel_199.rds")
prog %>% names()
prog$call$seed
prog$evalCI_new %>% names()
prog$evalCI_new$error

# LOOK AT SUMMARY
files
sum <- readRDS(paste0("results-share/", files[48]))
length(sum)

# temp <- numeric()
# for (i in 1:500) {
#   cat("\nSeed", sum[[i]]$call$seed, ":", sum[[i]]$evalCI_new$randomEst, "\n")
# }

sum[[1]] %>% names()

df <- do.call(
  rbind,
  lapply(sum, function(x) {
    data.frame(
      terms = paste0(x$finalVariables$term, collapse = ", "),
      seed = x$call$seed,
      cindex_pop = ifelse(is.null(x$evalCI_new$popEst), NA, x$evalCI_new$popEst),
      cindex_fe = ifelse(is.null(x$evalCI_new$fixedEst), NA, x$evalCI_new$fixedEst),
      cindex_re = ifelse(is.null(x$evalCI_new$randomEst), NA, x$evalCI_new$randomEst)
    )
  })
)

# 207
df %>% names()
df %>%
  arrange(cindex_re) %>%
  View()

df %>%
  count(terms)
df %>%
  pull(cindex_re) %>%
  mean(na.rm = TRUE)

# 486
df %>%
  filter(terms == "DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, TREAT_GRP4") %>%
  arrange(cindex_re) %>%
  select(seed, cindex_re) %>%
  print()

df %>% count(is.na(cindex_pop))

df %>% ggplot() +
  geom_histogram(
    aes(x = cindex_re, fill = terms)
  ) +
  theme_minimal() +
  scale_x_continuous(name = "C-index (RE meta-analysis)") +
  scale_y_continuous(name = "Number of models")

median(df$cindex_re)

# Oct 6 2025 selection
############
# EA MODEL #
############

# 20251005_WITH_PD_LARGE.rds

# SEED 424 (CI 0.7165671)

# 20251005_WITHOUT_PD_LARGE.rds

# "LB_BL_HGB_GRP3, VL_DURATIONs, ZZ_MAL" (CI 0.6535118, SEED 290)

# 20251005_WITH_PD_SMALL.rds

# "LB_BL_HGB_GRP3, MP_BL_SPLEEN_LENGTHs2, MB_COMBINEDs" (CI 0.7022273)

# SEED 336

# "LB_BL_HGB_GRP3, MP_BL_SPLEEN_LENGTHs2, MB_COMBINEDs" (CI: 0.7117774)

# SEED 305

# 20251005_WITHOUT_PD_SMALL.rds (CI: 0.5389529, SEED 149)

#############
# ISC MODEL #
#############

# 20251005_WITH_PD.rds

# "DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, MB_COMBINEDs, TREAT_GRP4" (CI 0.6935447, seed 11)

# "DM_AGEs, ZZ_AGEs2, LB_BL_HGB_GRP3, VL_DURATIONs, MP_BL_SPLEEN_LENGTHs2, MB_COMBINEDs, TREAT_GRP4" (CI 0.6970935, seed 428)

# 20251005_WITHOUT_PD.rds (CI 0.6857521, seed 313)


# VISUALISE INDIVIDUAL STUDIES

files <- dir("results-share")
files

# LOOK AT SPECIFIC MODEL

prog <- readRDS("results-share/20251005_WITHOUT_PD_originalModel_313.rds")
prog %>% names()

prog$evalCI_new$result$RE
prog$call$seed
prog$varSelectRR$result %>% names()

modSum <- prog$varSelectRR$result$pooledModel %>% summary()
prog$varSelectRR$result$finalVariables

conf.level <- 0.95
zval <- qnorm((1 + conf.level) / 2)

modPlot <- modSum[-1, ] %>%
  mutate(
    OR = exp(estimate),
    lower = exp(estimate - zval * std.error),
    upper = exp(estimate + zval * std.error)
  )
modPlot %>% count(term)

modPlot <- modPlot %>%
  mutate(term = case_when(
    term == "TREAT_GRP4SDA" ~ "Rx: Single dose LAMB (vs miltefosine)",
    term == "TREAT_GRP4OTHER" ~ "Rx: Other (vs miltefosine)",
    term == "DM_AGEs" ~ "Age (linear, per sd increase)",
    term == "ZZ_AGEs2" ~ "Age (squared, per sd increase squared)",
    term == "ZZ_MALSevere" ~ "Malnutrition, severe (vs mild/normal)",
    term == "ZZ_MALModerate" ~ "Malnutrition, moderate (vs mild/normal)",
    term == "MB_COMBINEDs" ~ "Parasite count (baseline, per + increase)",
    term == "MP_BL_SPLEEN_LENGTHs2" ~ "Spleen length (baseline, per x 2.7 increase)",
    term == "VL_DURATIONs" ~ "Fever duration (per sd increase on log scale)",
    term == "LB_BL_HGB_GRP3" ~ "Anaemia, severe (vs non-severe)",
    .default = "ERROR"
  ))

ggplot(modPlot, aes(x = term, y = OR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.8) +
  coord_flip() + # flip so terms are on y axis and errorbars are horizontal
  geom_hline(yintercept = 1, linetype = "dashed") + # OR = 1 reference
  scale_y_log10(
    # breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x))
  ) + # log scale (recommended for OR)
  labs(
    x = NULL, y = "Odds Ratio (log scale)",
    title = "Odds ratios with 95% CI"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
