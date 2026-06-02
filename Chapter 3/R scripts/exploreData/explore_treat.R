###############
## TREATMENT ##
###############

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
ads_model <- readRDS("data/ads_impute_full.rds")
# load("Analysis/ads_dirty.RData")


STUDYID_FCT <- ads_model %>%
  group_by(STUDY_NAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(STUDY_NAME)
STUDYID_FCT

TREATMENT_FCT <- ads_model %>%
  group_by(TREAT_DRUG_NAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(TREAT_DRUG_NAME)
TREATMENT_FCT

ads_model$TREAT_DRUG_NAME <- factor(ads_model$TREAT_DRUG_NAME, levels = c(
  "Ampho.",
  "L-AmB",
  "L-AmB (BS)",
  "L-AmB (LI)",
  "MF",
  "L-AmB / MF",
  "Pent.",
  "PM",
  "SSG",
  "HPE"
))

treat_dist1a <- ads_model %>%
  group_by(STUDY_NAME, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = factor(STUDY_NAME, levels = STUDYID_FCT),
      y = total,
      fill = TREAT_DRUG_NAME,
      group = factor(TREAT_TEXT)
    )
  ) +
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) +
  scale_fill_manual(
    values = c(
      "Ampho." = "#ff860d",
      "L-AmB" = "yellow",
      "L-AmB (BS)" = "yellow4",
      "L-AmB (LI)" = "#a7d631",
      "MF" = "#f45252",
      "L-AmB / MF" = "#c1c0c0",
      "Pent." = "#ff59e3",
      "PM" = "lightgreen",
      "SSG" = "deepskyblue",
      "HPE" = "white"
    ),
    labels = c(
      "Amphotericin B deoxycholate",
      "Liposomal amphotericin B (Gilead)",
      "Liposomal amphotericin B (Bharat Serum and Vaccines)",
      "Liposomal amphotericin B (Life Care Innovations)",
      "Miltefosine",
      "Liposomal amphotericin B (Gilead) & miltefosine",
      "Pentamidine",
      "Paromomycin",
      "Sodium stibogluconate",
      "Human placenta extract"
    )
  ) +
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    x = "IDDO Study ID",
    y = "Number of patients",
    caption = "Each colour corresponds to different treatment (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 10
    ),
    legend.position = c(0.85, 0.85),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA), # plot panel
    plot.background = element_rect(fill = "white", colour = NA) # entire plot
  )

treat_dist1a

ggsave(filename = "Analysis/treatment/treat_dist1a.pdf", plot = treat_dist1a, width = 20, height = 12)

ads_model_adj <- ads_model %>%
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  summarise(total_sg = n()) %>%
  ungroup() %>%
  group_by(STUDYID) %>%
  mutate(total_study = sum(total_sg)) %>%
  ungroup() %>%
  mutate(prop = total_sg / total_study)

treat_dist1b <- ads_model_adj %>%
  ggplot(
    aes(
      x = factor(STUDYID, levels = STUDYID_FCT),
      y = prop,
      fill = TREAT_DRUG_NAME,
      group = factor(TREAT_TEXT)
    )
  ) +
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) +
  scale_fill_manual(
    values = c(
      "Ampho." = "#ff860d",
      "L-AmB" = "yellow",
      "L-AmB (BS)" = "yellow4",
      "L-AmB (LI)" = "#a7d631",
      "MF" = "#f45252",
      "L-AmB / MF" = "#c1c0c0",
      "PM" = "lightgreen",
      "SSG" = "deepskyblue",
      "HPE" = "white"
    )
  ) +
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Treatment regimen by IDDO VL Dataset",
    x = "IDDO VL Dataset",
    y = "Proportion of patients",
    caption = "Each colour corresponds to different treatment (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.caption = element_text(
      hjust = 0,
      vjust = 1,
      size = 10
    )
  )

ggsave(filename = "Analysis/treatment/treat_dist1b.pdf", plot = treat_dist1b, width = 16, height = 12)

treat_dist1c <- ads_model %>%
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(
    aes(
      x = factor(TREAT_DRUG_NAME, levels = TREATMENT_FCT),
      y = total,
      fill = STUDYID,
      group = factor(TREAT_TEXT)
    )
  ) +
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) +
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "VL Datasets by Treatment",
    x = "Treatment",
    y = "Number of patients",
    caption = "Each colour corresponds to different dataset (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "VL Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.caption = element_text(
      hjust = 0,
      vjust = 1,
      size = 10
    )
  )

ggsave(filename = "Analysis/treatment/treat_dist1c.pdf", plot = treat_dist1c, width = 20, height = 12)


ads_model_adj2 <- ads_model %>%
  arrange(TREAT_DRUG_NAME, TREAT_TEXT) %>%
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  summarise(total_sg = n()) %>%
  ungroup() %>%
  group_by(TREAT_DRUG_NAME) %>%
  mutate(total_treat = sum(total_sg)) %>%
  ungroup() %>%
  mutate(prop = total_sg / total_treat)

treat_dist1d <- ads_model_adj2 %>%
  ggplot(
    aes(
      x = factor(TREAT_DRUG_NAME, levels = TREATMENT_FCT),
      y = prop,
      fill = STUDYID,
      group = factor(TREAT_TEXT)
    )
  ) +
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) +
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "VL Datasets by Treatment",
    x = "Treatment",
    y = "Proportion of patients",
    caption = "Each colour corresponds to different dataset (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.caption = element_text(
      hjust = 0,
      vjust = 1,
      size = 10
    )
  )

ggsave(filename = "Analysis/treatment/treat_dist1d.pdf", plot = treat_dist1d, width = 16, height = 12)

# let's do some modelling
ads_model %>%
  group_by(TREAT_GRP3) %>%
  summarise(
    prop_relapse = sum(OUT_DC_RELAPSE) / n(),
    total = n()
  ) %>%
  arrange(prop_relapse)

group1_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP1 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(group1_uni)

# as expected, this does not converge (unsolvable due to collinearity)
group2_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP2 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(group2_uni)

group2_multi <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP2 + DM_AGEs + I(DM_AGEs^2) + DM_SEX + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(group2_multi)

# group3 - parisominious group
group3_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP3 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(group3_uni)

group3_multi <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP3 + DM_AGEs + I(DM_AGEs^2) + DM_SEX + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(group3_multi)
