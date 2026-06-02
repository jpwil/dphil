#####################################
## create forest plots for c-index ##
#####################################

# confidence intervals calculate on the logit scale
# use pair weights given variances unreliable with small number of events
# references: van Oirbeek 2012; Wynants 2018 ("Does ignoring clustering in multicenter data influence the performance of prediction models?")

rm(list = ls())
library(tidyverse)
library(metafor)
library(mice)
library(scales)
library(ggtext)

summary <- readRDS("results/summaryCIM2_withPD.rds")
summary_pool <- readRDS("results/summaryCIM2_withPD_ma.rds")

# --- 1. Prepare summary dataframe for plotting ---
plot_df <- summary %>%
  # ensure proper names exist
  rename(
    c_index = est,
    ci_lo_logit_back = ci_ul,
    ci_hi_logit_back = ci_ll
  ) %>%
  # keep original order info, then arrange by descending c_index (NA last)
  mutate(
    # create a sorting key: -c_index sorts descending, NA -> very small so ends up last
    sort_key = ifelse(is.na(c_index), -Inf, c_index)
  ) %>%
  mutate(
    small = events <= 5
  ) %>%
  arrange(desc(sort_key)) %>%
  mutate(
    # plotting y positions: highest c-index at top (large y)
    rank = row_number(),
    plot_y = rev(rank)
  )

# keep a vector of y positions for the study rows
study_plot_ys <- plot_df$plot_y

# # --- 3. Define y-positions for spacer + summaries (place them below the study rows) ---
gap <- 1.4
lowest_study_y <- min(plot_df$plot_y)
spacer_y <- lowest_study_y - gap / 2
# We'll place PW, EW, RE below the spacer
pw_y <- lowest_study_y - gap - 0.0
re_y <- lowest_study_y - gap - 1

pw_pred <- summary_pool$fe
re_pred <- summary_pool$re

summary_rows <- tibble(
  label = c("Fixed effects (pair weights)", "Random effects (REML)"),
  plot_y = c(pw_y, re_y),
  est = c(pw_pred$pred, re_pred$pred),
  ci_lo = c(pw_pred$ci.lb, re_pred$ci.lb),
  ci_hi = c(pw_pred$ci.ub, re_pred$ci.ub),
  pi_lo = c(NA, re_pred$pi.lb),
  pi_hi = c(NA, re_pred$pi.ub)
)

# --- 5. Diamond polygons for summary rows ---
diamond_h <- 0.18 # half-height for diamonds; adjust for aesthetics
diamond_df <- summary_rows %>%
  filter(!is.na(est)) %>%
  rowwise() %>%
  mutate(poly = list(tibble(
    x = c(ci_lo, est, ci_hi, est),
    y = c(plot_y, plot_y + diamond_h, plot_y, plot_y - diamond_h),
    group = paste0("d_", label)
  ))) %>%
  pull(poly) %>%
  bind_rows()

# --- 6. Plot with 3-column y-axis drawn by geom_text() ---

# y positions for axis ticks: studies followed by spacer and summary_rows
axis_y_positions <- c(plot_df$plot_y, spacer_y, summary_rows$plot_y)

# x positions for the three columns (tweak if you want more/less space)
x_name <- -0.43
x_events <- -0.1
x_n <- -0.02

# Build label data for study rows (plot_df already ordered for plotting)
labels_df <- plot_df %>%
  mutate(
    name = STUDY_NAME,
    n = as.character(num),
    events = as.character(events),
    # Keep plot_y as y position
  ) %>%
  select(plot_y, name, n, events)

# Add a header row just above the top-most study
header_y <- max(labels_df$plot_y) + 0.9
header_df <- tibble(
  plot_y = header_y,
  name = "Study name",
  n = "N",
  events = "Events"
)

# Build label data for summary rows (they already have plot_y)
summary_label_df <- summary_rows %>%
  mutate(
    name = label,
    n = NA_character_,
    events = NA_character_
  ) %>%
  select(plot_y, name, n, events)

# Combined label dataset (header, studies, spacer if needed, summaries)
# We'll keep a blank spacer line by inserting a small empty row at spacer_y
spacer_df <- tibble(plot_y = spacer_y, name = NA_character_, n = NA_character_, events = NA_character_)
all_labels <- bind_rows(header_df, labels_df, spacer_df, summary_label_df)

# now the plot
p <- ggplot() +
  # study-level errorbars: use the logit-based back-transformed intervals (ci_lo_trans / ci_hi_trans)
  geom_errorbarh(
    data = plot_df %>% filter(!is.na(c_index)),
    aes(y = plot_y, xmin = ci_lo_logit_back, xmax = ci_hi_logit_back, colour = small),
    height = 0.18, linewidth = 0.6
  ) +
  geom_point(
    data = plot_df %>% filter(!is.na(c_index)),
    aes(x = c_index, y = plot_y, colour = small),
    size = 2.5
  ) +
  # RE prediction interval (dashed) if available
  geom_errorbarh(
    data = summary_rows %>% filter(!is.na(pi_lo) & !is.na(pi_hi)),
    aes(y = plot_y, xmin = pi_lo, xmax = pi_hi, colour = "anything"),
    height = 0.3, linewidth = 0.8
  ) +
  scale_colour_manual(
    values = c("TRUE" = "grey", "FALSE" = "black", "anything" = "darkred"),
    breaks = c("TRUE", "anything"),
    labels = c("Excluded from meta-analysis", "95% prediction interval"),
    name = ""
  ) +
  # vertical reference line
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey60") +
  # diamond summaries
  geom_polygon(
    data = diamond_df,
    aes(x = x, y = y, group = group),
    fill = "darkblue",
    color = "darkblue",
    alpha = 1
  ) +

  # ---- TEXT COLUMNS ----
  # STUDY NAME column (HTML allowed via element_markdown)
  geom_text( # from ggtext; uses element_markdown-like rendering for text
    data = all_labels[-1, ],
    aes(x = x_name, y = plot_y, label = name),
    hjust = 0, vjust = 0.5,
    fill = NA, label.color = NA, # makes text background/box invisible
    size = 3.5,
    inherit.aes = FALSE
  ) +
  geom_text( # from ggtext; uses element_markdown-like rendering for text
    data = data.frame(x = c(x_name, x_n, x_events), y = rep(header_y, 3), label = c("Study name", "N", "Events")),
    aes(x = x, y = y, label = label, hjust = c(0, 1, 1)),
    vjust = 0.5,
    fontface = "bold",
    fill = NA, label.color = NA, # makes text background/box invisible
    size = 3.5,
    inherit.aes = FALSE
  ) +
  # N column (monospace, right-aligned)
  geom_text(
    data = all_labels[-1, ] %>% filter(!is.na(n)),
    aes(x = x_n, y = plot_y, label = n),
    hjust = 1, vjust = 0.5,
    size = 3.5
  ) +
  # Events column (monospace, right-aligned)
  geom_text(
    data = all_labels[-1, ] %>% filter(!is.na(events)),
    aes(x = x_events, y = plot_y, label = events),
    hjust = 1, vjust = 0.5,
    size = 3.5
  ) +

  # ---- AXES / SCALES ----
  # We hide y-axis labels because we draw them manually.
  scale_y_continuous(
    breaks = axis_y_positions,
    labels = rep("", length(axis_y_positions)),
    expand = expansion(add = c(1, 0.6))
  ) +
  # extend x limits to make room on the left for columns
  scale_x_continuous(
    name = "C-statistic",
    limits = c(-0.6, 1.00),
    breaks = seq(0.1, 1.0, by = 0.1),
    labels = label_number(accuracy = 0.1)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(hjust = 0.69),
    # Use element_markdown for any ggtext-rendered axis text (we used geom_richtext above)
    axis.text.y.left = element_blank(),
    # Keep the x-axis nice
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.55, 0.92),
    legend.background = element_rect(fill = alpha("white")),
    legend.title = element_blank()
    # plot.margin = margin(0, 10, 10, 10) # increase left/right margin if necessary
  )

# Print the plot
x <- 1.3
ggsave(filename = "graphs/forestCIM2.pdf", p, width = 7 * x, height = 5 * x)
