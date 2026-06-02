# libs
library(tidyverse)
library(metafor)
library(cowplot)

# -------------------------
# 1. Load your rma.uni objects
# -------------------------

# Replace these paths with your actual .rds paths
intercept_rds_path <- "data/forestCalIntM1.rds"
slope_rds_path <- "data/forestCalSlopeM1.rds"

intercept_mod <- readRDS(intercept_rds_path)
slope_mod <- readRDS(slope_rds_path)

# -------------------------
# 2. Helper: extract study-level info from rma.uni object
#    - tries common places (object$yi, object$vi, object$slab)
#    - if missing yi/vi, gives an informative stop()
# -------------------------
extract_study_info <- function(mod) {
  # yi / vi
  yi <- mod$yi
  vi <- mod$vi

  if (is.null(yi) || is.null(vi)) {
    stop(
      "Could not find study-level estimates (yi/vi) in the rma.uni object.\n",
      "Common rma.uni objects contain yi/vi; if your object does not, please supply\n",
      "a data.frame with study-level estimates and variances to merge with the plot code."
    )
  }

  slab <- mod$slab

  tibble(
    study_label = as.character(slab),
    yi = yi,
    vi = vi
  )
}

# add the missing study (with zero events)
intercept_df <- extract_study_info(intercept_mod)
slope_df <- extract_study_info(slope_mod)

intercept_df <- intercept_df %>% add_row(
  study_label = "Chakraborty 2008",
  yi = NA_real_,
  vi = NA_real_
)

slope_df <- slope_df %>% add_row(
  study_label = "Chakraborty 2008",
  yi = NA_real_,
  vi = NA_real_
)

study_info <- readRDS("data/ads_summary.rds")
study_info <- study_info %>%
  select(STUDY_NAME, OUTCOME) %>%
  group_by(STUDY_NAME) %>%
  summarise(N = n(), events = sum(OUTCOME)) %>%
  rename(study_label = STUDY_NAME)

# -------------------------
# 4. Build a unified plot_df anchored by the intercept ordering
#    We'll use intercept estimates to order studies (lowest -> highest).
# -------------------------
# merge intercept & slope study-level data to ensure same studies (inner join on study_label)
all_studies <- full_join(
  intercept_df %>% rename(yi_intercept = yi, vi_intercept = vi),
  slope_df %>% rename(yi_slope = yi, vi_slope = vi),
  by = "study_label"
)

# Merge study_info (N/events)
all_studies <- all_studies %>%
  left_join(study_info, by = "study_label")

# Order by intercept estimate (lowest -> highest); break ties by study_label
# Put NA intercepts last
all_studies <- all_studies %>%
  mutate(order_key = ifelse(is.na(yi_intercept), -Inf, yi_intercept)) %>%
  arrange(order_key, study_label) %>%
  mutate(
    rank = row_number(),
    plot_y = (rank) # highest (last in arrange) at top when plotting with increasing y
  )

# change Rijal 2010(A) slope to NA, due to small sample size
all_studies <- all_studies %>% mutate(
  yi_slope = ifelse(study_label == "Rijal 2010(A)", NA_real_, yi_slope),
  vi_slope = ifelse(study_label == "Rijal 2010(A)", NA_real_, vi_slope),
)

# Make a plotting dataframe for intercept and slope separately
plot_intercept_df <- all_studies %>%
  transmute(
    study_label,
    plot_y,
    est = yi_intercept,
    var = vi_intercept,
    N = N,
    events = events
  )

plot_slope_df <- all_studies %>%
  transmute(
    study_label,
    plot_y,
    est = yi_slope,
    var = vi_slope,
    N = N,
    events = events
  )

# Function to compute CI endpoints
add_ci <- function(df) {
  df %>% mutate(
    se = sqrt(var),
    ci_lo = est - 1.96 * se,
    ci_hi = est + 1.96 * se
  )
}

plot_intercept_df <- add_ci(plot_intercept_df)
plot_slope_df <- add_ci(plot_slope_df)

# -------------------------
# 5. Summary predictions (diamond) for each model
# -------------------------
get_summary_pred <- function(mod) {
  pr <- predict(mod)

  tibble(
    pred = pr$pred,
    ci_lb = pr$ci.lb,
    ci_ub = pr$ci.ub,
    pi_lb = pr$pi.lb,
    pi_ub = pr$pi.ub
  )
}

intercept_summary <- get_summary_pred(intercept_mod)
slope_summary <- get_summary_pred(slope_mod)

# We'll place summary diamonds below the studies (like your previous code).
# Determine spacer & summary y positions
gap <- 1.4
lowest_study_y <- min(all_studies$plot_y)
spacer_y <- lowest_study_y - gap / 2
summary_y <- lowest_study_y - gap

# Build diamond polygon for a summary prediction (if available)
make_diamond_df <- function(pred_df, y_val, half_h = 0.18) {
  if (is.null(pred_df)) {
    return(tibble())
  }
  tibble(
    x = c(pred_df$ci_lb, pred_df$pred, pred_df$ci_ub, pred_df$pred),
    y = c(y_val, y_val + half_h, y_val, y_val - half_h),
    group = "summary"
  )
}

diamond_intercept <- make_diamond_df(intercept_summary, summary_y)
diamond_slope <- make_diamond_df(slope_summary, summary_y)


# -------------------------
# 6. Make a plotting function that draws a forest plot given df
#    Left plot will include the name/N/events columns (via geom_text).
#    Right plot will be the same style but WITHOUT the left columns.
# -------------------------
make_forest_plot <- function(df, diamond_df = NULL, re_pi, xlab = "Estimate", xlim = NULL,
                             show_left_labels = TRUE, x_int = 1, x_breaks,
                             arrow_lower = NULL, arrow_upper = NULL,
                             arrow_dx_rel = 0.01, arrow_length_mm = 1.5,
                             cap_height = 0.18, hjust = hjust, show.legend = "inside") {
  # text column x positions (negative space on x)
  x_name <- ifelse(is.null(x_breaks), -0.35, min(x_breaks) - 3.2)
  x_n <- ifelse(is.null(x_breaks), -0.12, min(x_breaks) - 0.8)
  x_events <- ifelse(is.null(x_breaks), -0.02, min(x_breaks) - 0.2)

  # header y (above top study)
  header_y <- max(df$plot_y) + 0.9

  header_df <- tibble(
    plot_y = header_y,
    name = "Study name",
    N = "N",
    events = "Events"
  )

  labels_df <- df %>%
    transmute(plot_y,
      name = study_label, N = ifelse(is.na(N), NA_character_, as.character(N)),
      events = ifelse(is.na(events), NA_character_, as.character(events))
    )

  # NOTE: spacer_y must exist in the environment (kept as in your original)
  all_labels <- bind_rows(
    header_df,
    labels_df,
    tibble(plot_y = spacer_y, name = NA_character_, N = NA_character_, events = NA_character_)
  )

  labels_header <- data.frame(x = c(x_name, x_n, x_events), y = rep(header_y, 3), label = c("Study name", "Events", "N"))
  # prepare clipped endpoints + arrow flags only for rows with non-missing est
  df_plot <- df %>% filter(!is.na(est))

  # compute x-range and arrow_dx
  xr <- if (!is.null(xlim)) xlim else range(c(df$ci_lo, df$ci_hi, df$est), na.rm = TRUE)
  xspan <- diff(xr)
  if (xspan == 0 || is.na(xspan)) xspan <- 1
  arrow_dx <- arrow_dx_rel * xspan

  # set clip and arrow flags
  df_plot <- df_plot %>%
    mutate(
      xmin_clip = if (!is.null(arrow_lower)) pmax(ci_lo, arrow_lower) else ci_lo,
      xmax_clip = if (!is.null(arrow_upper)) pmin(ci_hi, arrow_upper) else ci_hi,
      arrow_left = if (!is.null(arrow_lower)) (ci_lo < arrow_lower) else FALSE,
      arrow_right = if (!is.null(arrow_upper)) (ci_hi > arrow_upper) else FALSE,
      arrow_left_xstart = ifelse(arrow_left, arrow_lower + arrow_dx, NA_real_),
      arrow_left_xend = ifelse(arrow_left, arrow_lower, NA_real_),
      arrow_right_xstart = ifelse(arrow_right, arrow_upper - arrow_dx, NA_real_),
      arrow_right_xend = ifelse(arrow_right, arrow_upper, NA_real_)
    )

  # rows that are fully inside limits (no arrows) -> use geom_errorbarh (has caps)
  df_no_arrows <- df_plot %>% filter(!arrow_left & !arrow_right)

  # rows that have any arrow on either side -> draw horizontal segment (no caps) + manual caps
  df_with_arrows <- df_plot %>% filter(arrow_left | arrow_right)

  # For manual caps: cap half-height in y units
  cap_half <- cap_height / 2

  # caps on left side (for rows with arrows but LEFT side NOT clipped)
  left_caps_df <- df_with_arrows %>%
    filter(!arrow_left) %>%
    mutate(
      xcap = xmin_clip,
      y1 = plot_y - cap_half,
      y2 = plot_y + cap_half
    )

  # caps on right side (for rows with arrows but RIGHT side NOT clipped)
  right_caps_df <- df_with_arrows %>%
    filter(!arrow_right) %>%
    mutate(
      xcap = xmax_clip,
      y1 = plot_y - cap_half,
      y2 = plot_y + cap_half
    )

  p <- ggplot() +
    # errorbars for rows without arrows (will have end caps)
    {
      if (nrow(df_no_arrows) > 0) {
        geom_errorbarh(
          data = df_no_arrows,
          aes(y = plot_y, xmin = xmin_clip, xmax = xmax_clip),
          height = cap_height, linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # simple horizontal segments (no caps) for rows that need arrows
    {
      if (nrow(df_with_arrows) > 0) {
        geom_segment(
          data = df_with_arrows,
          aes(x = xmin_clip, xend = xmax_clip, y = plot_y, yend = plot_y),
          linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # manual left caps for arrow rows where left side is NOT clipped
    {
      if (nrow(left_caps_df) > 0) {
        geom_segment(
          data = left_caps_df,
          aes(x = xcap, xend = xcap, y = y1, yend = y2),
          linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # manual right caps for arrow rows where right side is NOT clipped
    {
      if (nrow(right_caps_df) > 0) {
        geom_segment(
          data = right_caps_df,
          aes(x = xcap, xend = xcap, y = y1, yend = y2),
          linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # points
    geom_point(
      data = df_plot,
      aes(x = est, y = plot_y),
      size = 2.5
    ) +

    # arrows for left (values below arrow_lower)
    {
      if (!is.null(arrow_lower)) {
        geom_segment(
          data = df_plot %>% filter(arrow_left),
          aes(
            x = arrow_left_xstart, xend = arrow_left_xend,
            y = plot_y, yend = plot_y
          ),
          arrow = arrow(length = unit(arrow_length_mm, "mm")),
          inherit.aes = FALSE,
          linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # arrows for right (values above arrow_upper)
    {
      if (!is.null(arrow_upper)) {
        geom_segment(
          data = df_plot %>% filter(arrow_right),
          aes(
            x = arrow_right_xstart, xend = arrow_right_xend,
            y = plot_y, yend = plot_y
          ),
          arrow = arrow(length = unit(arrow_length_mm, "mm")),
          inherit.aes = FALSE,
          linewidth = 0.6
        )
      } else {
        NULL
      }
    } +

    # vertical reference line at x_int
    geom_vline(xintercept = x_int, linetype = "dashed", color = "grey60") +
    geom_errorbarh(
      aes(y = summary_y, xmin = re_pi$pi_lb, xmax = re_pi$pi_ub, colour = "95% prediction interval"),
      height = 0.3, linewidth = 0.8
    ) +
    scale_colour_manual(
      values = c("95% prediction interval" = "darkred")
    ) +
    # diamond if present
    {
      if (!is.null(diamond_df) && nrow(diamond_df) > 0) geom_polygon(data = diamond_df, aes(x = x, y = y, group = group), fill = "darkblue", color = "darkblue") else NULL
    } +

    # draw text columns only on plot that requests them
    {
      if (show_left_labels) {
        list(
          geom_text(
            data = labels_header,
            aes(y = y, x = x, label = label, hjust = c(0, 1, 1)),
            vjust = 0.5,
            size = 3.5,
            fontface = "bold",
            inherit.aes = FALSE
          ),
          geom_text(
            data = all_labels[-1, ],
            aes(y = plot_y, label = name),
            x = x_name,
            hjust = 0, vjust = 0.5,
            size = 3.5,
            inherit.aes = FALSE
          ),
          geom_text(
            data = all_labels[-1, ] %>% filter(!is.na(N)),
            aes(y = plot_y, label = N),
            x = x_events,
            hjust = 1, vjust = 0.5,
            size = 3.5,
            inherit.aes = FALSE
          ),
          geom_text(
            data = all_labels[-1, ] %>% filter(!is.na(events)),
            aes(y = plot_y, label = events),
            hjust = 1, vjust = 0.5,
            size = 3.5,
            inherit.aes = FALSE,
            x = x_n
          ),
          geom_text(
            aes(x = x_name, y = summary_y, label = "Random effects (REML)"),
            hjust = 0, vjust = 0.5,
            size = 3.5,
            inherit.aes = FALSE
          )
        )
      } else {
        geom_text(
          data = all_labels %>% filter(!is.na(events)),
          aes(y = plot_y, label = rep("", length(events))),
          hjust = 1, vjust = 0.5,
          size = 3.5,
          inherit.aes = FALSE,
          x = x_events
        )
      }
    } +

    # scales - make y breaks align with plot_y positions; labels blank because we draw them
    scale_y_continuous(
      breaks = df$plot_y,
      labels = rep("", length(df$plot_y)),
      expand = expansion(add = c(3, 0.6))
    ) +
    scale_x_continuous(
      name = xlab,
      limits = xlim,
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(hjust = hjust),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 5, b = 10, l = 5),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = show.legend,
      legend.title = element_blank(),
      legend.background = element_rect(fill = alpha("white")),
      legend.position.inside = c(0.13, 0.05)
    )
  p
}

# -------------------------
# 7. Choose x-limits for each plot sensibly
#    Here we compute a symmetric-ish margin around observed CIs.
# -------------------------
calc_xlim <- function(df, safe_pad = 0.05) {
  x_min <- min(df$ci_lo, na.rm = TRUE)
  x_max <- max(df$ci_hi, na.rm = TRUE)
  if (!is.finite(x_min) || !is.finite(x_max)) {
    return(NULL)
  }
  rng <- x_max - x_min
  c(x_min - rng * safe_pad - 0.01, x_max + rng * safe_pad + 0.01)
}

xlim_intercept <- calc_xlim(plot_intercept_df)
xlim_slope <- calc_xlim(plot_slope_df)

# If either is NULL, provide a default
if (is.null(xlim_intercept)) xlim_intercept <- c(-1, 1)
if (is.null(xlim_slope)) xlim_slope <- c(-1, 1)

# Make left (intercept) and right (slope) plots
p_intercept <- make_forest_plot(plot_intercept_df, diamond_intercept, re_pi = intercept_summary, hjust = 0.85, arrow_lower = -2, arrow_upper = 2, xlim = c(-7, 2), x_int = 0, x_breaks = seq(-2, 2, 1), xlab = "Calibration intercept (CITL)", show_left_labels = TRUE, show.legend = "none")
p_slope <- make_forest_plot(plot_slope_df, diamond_slope, re_pi = slope_summary, arrow_lower = -4.5, hjust = 0.5, arrow_upper = 5.5, x_int = 1, x_breaks = seq(-4, 5, 1), xlab = "Calibration slope (CS)", xlim = c(-4.5, 5.5), show_left_labels = FALSE, show.legend = "inside")

# -------------------------
# 8. Align & combine horizontally with cowplot::plot_grid
#    - ensure both plots have the same vertical scale by sharing height visually.
# -------------------------
combined <- plot_grid(p_intercept, p_slope, ncol = 2, align = "v", axis = "l", rel_widths = c(1, 0.55))

# Save combined figure
x <- 1.3
ggsave("graphs/forestCalM1.pdf", combined, width = x * 7, height = x * 5)
