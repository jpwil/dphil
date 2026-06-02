###############
## TREATMENT ##
###############

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
library(ggrepel)

df <- readRDS("data/ads_ns_impute.rds")
df %>% names()

df <- df %>% mutate(
  TREAT_DRUG_NAME = case_when(
    TREAT_DRUG_NAME == "Ampho." ~ "ABD",
    TREAT_DRUG_NAME == "L-AmB" ~ "LAMB",
    TREAT_DRUG_NAME == "L-AmB (BS)" ~ "LAMB (BS)",
    TREAT_DRUG_NAME == "L-AmB (LI)" ~ "LAMB (LI)",
    TREAT_DRUG_NAME == "L-AmB / MF" ~ "LAMB / MF",
    .default = TREAT_DRUG_NAME
  )
)

df %>%
  count(TREAT_DRUG_NAME, TREAT_TEXT) %>%
  print(n = Inf)

df <- df %>% mutate(
  TREAT_TEXT = case_match(
    TREAT_TEXT,
    "10m 1D" ~ "10mg 1D",
    "15m 1D" ~ "15mg 1D",
    "5m 1D" ~ "5mg 1D",
    "5m 3D" ~ "5mg 3D",
    "15m 1D" ~ "15mg 1D",
    "3m 3D" ~ "3mg 3D",
    "4m 3D" ~ "4mg 3D",
    "5m 3D" ~ "5mg 3D",
    "3.75m 1D / M14D" ~ "3.75mg 1D / 14D",
    "5m 1D /  M10D" ~ "5mg 1D / 10D",
    "5m 1D / M14D" ~ "5mg 1D / 14D",
    "5m 1D / M7D" ~ "5m 1D / 7D",
    "M28D" ~ "28D",
    .default = TREAT_TEXT
  )
)

df %>%
  count(STUDY_NAME, STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  print(n = Inf)

TREAT_TEXT <- df %>%
  group_by(STUDY_NAME, TREAT_DRUG_NAME, TREAT_TEXT) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(TREAT_TEXT)
TREAT_TEXT

STUDYID_FCT <- df %>%
  group_by(STUDY_NAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(STUDY_NAME)
STUDYID_FCT

df <- df %>%
  mutate(
    point_label = case_when(
      TREAT_TEXT == "3mg 3D" ~ TRUE,
      TREAT_TEXT == "4mg 3D" ~ TRUE,
      TREAT_TEXT == "5mg 3D" ~ TRUE,
      TREAT_DRUG_NAME == "PM" ~ TRUE,
      TREAT_DRUG_NAME == "HPE" ~ TRUE,
      TREAT_DRUG_NAME == "ABD" & STUDY_NAME == "Chakraborty 2008" ~ TRUE,
      .default = FALSE
    )
  )

treat_dist1a <- df %>%
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
    position = "stack",
    width = 0.9
  ) +
  scale_fill_manual(
    name = "Drug(s)",
    values = c(
      "ABD" = "#ff860d",
      "LAMB" = "yellow",
      "LAMB (BS)" = "#89fd7c",
      "LAMB (LI)" = "#10ffd7",
      "MF" = "#f57373",
      "LAMB / MF" = "#d78fd7",
      "PM" = "#ffffff",
      "SSG" = "deepskyblue",
      "Pent." = "grey",
      "HPE" = "#fff7c4"
    ),
    breaks = c(
      "ABD",
      "LAMB",
      "LAMB (BS)",
      "LAMB (LI)",
      "LAMB / MF",
      "MF",
      "PM",
      "SSG",
      "Pent.",
      "HPE"
    ),
    labels = c(
      "Amphotericin B deoxycholate",
      "Liposomal amphotericin B (Gilead)",
      "Liposomal amphotericin B (Bharat Serums and Vaccines)",
      "Liposomal amphotericin B (Lifecare Innovations)",
      "Liposomal amphotericin B (Gilead) + miltefosine",
      "Miltefosine",
      "Paromomycin",
      "Sodium stibogluconate",
      "Pentamidine",
      "Human placenta extract"
    )
  ) +
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 2.3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  scale_y_continuous(
    name = "Patients included in the model",
    breaks = seq(0, 1000, 200),
    minor_breaks = seq(0, 1000, 50)
  ) +
  scale_x_discrete(
    name = "Study"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      margin = margin(t = -17)
    ),
    panel.grid.major.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.7), # x, y within plot panel
    legend.key = element_rect(colour = "#555555"),
    legend.background = element_rect(fill = alpha("white")),
  )

ggsave(filename = "figures/treatment/treat_dist1a.pdf", plot = treat_dist1a, width = 20 * 0.7, height = 11 * 0.7)

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
