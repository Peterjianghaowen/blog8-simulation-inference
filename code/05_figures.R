# Figures: four static figures and the interactive widget.
#
# The static figures are the ones the blog post embeds; the interactive widget
# is saved as a self-contained HTML file and rebuilt inside the post so that it
# works on the published page.
#
# Inputs : results/*.csv (from 03_bootstrap.R and 04_simulate.R)
# Outputs: results/fig1_bootstrap.png        the bootstrap distributions
#          results/fig2_coverage.png         does the interval really cover?
#          results/fig3_null.png             the null distribution and p-value
#          results/fig4_power.png            how much a survey could have found
#          results/fig5_interactive.html     the interactive widget

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
})

RESULTS <- "results"
FIG     <- "results"

BLUE  <- "#1f77b4"
RED   <- "#d62728"
GREY  <- "#8c8c8c"
DARK  <- "#222222"

theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 13),
                plot.subtitle = element_text(size = 10, colour = "#555555"),
                plot.caption = element_text(size = 9, colour = "#777777"),
                plot.title.position = "plot",
                legend.position = "bottom"))

# Saving a self-contained widget needs pandoc. RStudio ships a copy, so point
# RSTUDIO_PANDOC at it when pandoc is not on the PATH.
if (Sys.which("pandoc") == "") {
  arch <- if (Sys.info()[["machine"]] == "arm64") "aarch64" else "x86_64"
  bundled <- file.path("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools", arch)
  if (dir.exists(bundled)) Sys.setenv(RSTUDIO_PANDOC = bundled)
}

key <- read.csv(file.path(RESULTS, "key_numbers.csv"))
get <- function(s) key$value[key$stat == s]

rate_2019 <- get("rate_2019")
rate_2024 <- get("rate_2024")
diff_obs  <- get("diff")
ci_diff   <- c(get("ci_low_diff"), get("ci_high_diff"))
wald_diff <- c(get("wald_low_diff"), get("wald_high_diff"))
p_sim     <- get("p_sim")

pct <- function(x, d = 2) sprintf(paste0("%.", d, "f"), 100 * x)

# ---------------------------------------------------------------------------
# Figure 1: the bootstrap distributions.
# ---------------------------------------------------------------------------
boot <- read.csv(file.path(RESULTS, "bootstrap_replicates.csv"))
boot_diff <- read.csv(file.path(RESULTS, "bootstrap_diff_replicates.csv"))$diff_star

f1a <- boot |>
  filter(month_id == "2024-01") |>
  ggplot(aes(x = rate_star)) +
  geom_histogram(binwidth = 0.0004, fill = BLUE, colour = NA, alpha = 0.85) +
  geom_vline(xintercept = rate_2024, colour = DARK, linewidth = 0.5) +
  annotate("rect", xmin = get("ci_low_2024"), xmax = get("ci_high_2024"),
           ymin = -Inf, ymax = Inf, fill = RED, alpha = 0.12) +
  scale_x_continuous(labels = function(x) sprintf("%.1f%%", 100 * x)) +
  labs(
    title = "A: Bootstrap distribution of the January 2024 rate",
    subtitle = "2,000 resamples of the 80,923 people interviewed",
    x = "Estimated unemployment rate",
    y = "Bootstrap replicates"
  )

f1b <- tibble(x = boot_diff) |>
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.0006, fill = BLUE, colour = NA, alpha = 0.85) +
  geom_vline(xintercept = diff_obs, colour = DARK, linewidth = 0.5) +
  annotate("rect", xmin = ci_diff[1], xmax = ci_diff[2],
           ymin = -Inf, ymax = Inf, fill = RED, alpha = 0.12) +
  annotate("rect", xmin = wald_diff[1], xmax = wald_diff[2],
           ymin = -Inf, ymax = Inf, fill = "#f0a202", alpha = 0.22) +
  scale_x_continuous(labels = function(x) sprintf("%+.2f", 100 * x)) +
  labs(
    title = "B: Bootstrap distribution of the change, 2019 to 2024",
    subtitle = "red = bootstrap 95% interval, orange = textbook interval",
    x = "Change in the unemployment rate (percentage points)",
    y = "Bootstrap replicates"
  )

fig1 <- cowplot::plot_grid(f1a, f1b, nrow = 1, align = "h",
                           caption = paste0(
                             "Resampling people with replacement, ",
                             format(get("boot_replicates"), big.mark = ","),
                             " times, and recomputing the statistic each time. ",
                             "The bootstrap interval is 49% wider than the textbook one ",
                             "because the CPS weights are very unequal."))
ggsave(file.path(FIG, "fig1_bootstrap.png"), fig1, width = 11, height = 4.8, dpi = 160)

# ---------------------------------------------------------------------------
# Figure 2: coverage - does a 95% interval really contain the truth 95% of
# the time?
# ---------------------------------------------------------------------------
coverage <- read.csv(file.path(RESULTS, "coverage.csv"))
small <- read.csv(file.path(RESULTS, "small_sample.csv"))

f2a <- coverage |>
  filter(n %in% c(1000, 5000, 80923)) |>
  mutate(n_lab = format(n, big.mark = ",")) |>
  ggplot(aes(x = level * 100, y = coverage * 100, colour = n_lab, group = n)) +
  geom_abline(slope = 1, intercept = 0, colour = GREY, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title = "A: Simulated coverage of the interval",
    subtitle = "2,000 repetitions per point; dashed line = perfect coverage",
    x = "Nominal confidence level (%)",
    y = "Share of intervals that contain the truth (%)",
    colour = "Sample size"
  )

f2b <- small |>
  ggplot(aes(x = n_per_month, group = 1)) +
  geom_hline(yintercept = 95, colour = GREY, linetype = "dashed") +
  geom_line(aes(y = coverage_boot * 100, colour = "Bootstrap interval"), linewidth = 0.7) +
  geom_line(aes(y = coverage_wald * 100, colour = "Textbook interval"), linewidth = 0.7) +
  geom_point(aes(y = coverage_boot * 100, colour = "Bootstrap interval"), size = 1.6) +
  geom_point(aes(y = coverage_wald * 100, colour = "Textbook interval"), size = 1.6) +
  scale_x_log10() +
  scale_colour_manual(values = c("Bootstrap interval" = BLUE, "Textbook interval" = "#f0a202")) +
  labs(
    title = "B: Coverage on the real CPS data",
    subtitle = "1,000 simulated surveys of n people per month",
    x = "People interviewed per month (log scale)",
    y = "Intervals covering the best estimate (%)"
  )

fig2 <- cowplot::plot_grid(f2a, f2b, nrow = 1,
                           caption = paste0(
                             "Left: a population with a known rate, sampled over and over. ",
                             "Right: the real CPS data, where the textbook interval ",
                             "covers the truth only about 80% of the time."))
ggsave(file.path(FIG, "fig2_coverage.png"), fig2, width = 11, height = 4.6, dpi = 160)

# ---------------------------------------------------------------------------
# Figure 3: the null distribution, the rejection region and the p-value.
# ---------------------------------------------------------------------------
null_dens <- read.csv(file.path(RESULTS, "null_distribution.csv"))
null_sum  <- read.csv(file.path(RESULTS, "null_summary.csv"))
nget <- function(s) null_sum$value[null_sum$stat == s]

crit <- c(nget("crit_low"), nget("crit_high"))

f3 <- null_dens |>
  ggplot(aes(x = delta * 100, y = density / 100)) +
  annotate("rect", xmin = -Inf, xmax = crit[1] * 100, ymin = -Inf, ymax = Inf,
           fill = RED, alpha = 0.15) +
  annotate("rect", xmin = crit[2] * 100, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = RED, alpha = 0.15) +
  geom_area(fill = BLUE, alpha = 0.8) +
  geom_vline(xintercept = diff_obs * 100, colour = DARK, linewidth = 0.6) +
  annotate("text", x = diff_obs * 100, y = max(null_dens$density) / 100 * 0.92,
           label = paste0("observed change\n", pct(diff_obs), " pp"),
           hjust = 1.08, size = 3.4, colour = DARK) +
  annotate("text", x = crit[2] * 100, y = max(null_dens$density) / 100 * 0.55,
           label = paste0("rejection region\n(5% of the null distribution)"),
           hjust = 0, size = 3.4, colour = RED) +
  scale_x_continuous(labels = function(x) sprintf("%+.2f", x)) +
  labs(
    title = "The null distribution of the estimated change, simulated",
    subtitle = paste0("20,000 simulated surveys under H0: nothing changed; ",
                      "the p-value is the area beyond what we observed"),
    caption = paste0("Simulated p-value = ", sprintf("%.3f", p_sim),
                     "; the textbook formula would give ",
                     sprintf("%.3f", nget("p_norm")), "."),
    x = "Estimated change in the unemployment rate (percentage points)",
    y = "Density"
  )
ggsave(file.path(FIG, "fig3_null.png"), f3, width = 9, height = 5.2, dpi = 160)

# ---------------------------------------------------------------------------
# Figure 4: power - how big a change could a survey of a given size detect?
# ---------------------------------------------------------------------------
power <- read.csv(file.path(RESULTS, "power.csv"))

f4 <- power |>
  filter(n %in% c(1000, 10000, 25000, 80923)) |>
  mutate(n_lab = format(n, big.mark = ",")) |>
  ggplot(aes(x = delta * 100, y = power, colour = n_lab, group = n)) +
  geom_hline(yintercept = 0.8, colour = GREY, linetype = "dashed") +
  geom_vline(xintercept = diff_obs * 100, colour = DARK, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1), labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(
    title = "Power: the chance of detecting a real change",
    subtitle = paste0("the vertical line is the change we actually measured (",
                      pct(diff_obs), " pp); the dashed line is 80% power"),
    x = "True change in the unemployment rate (percentage points)",
    y = "Probability of rejecting the null",
    colour = "People per month"
  )
ggsave(file.path(FIG, "fig4_power.png"), f4, width = 9, height = 5.2, dpi = 160)

# ---------------------------------------------------------------------------
# Figure 5: the interactive widget.
#
# Top panel: the null distribution of the test statistic with the rejection
# region shaded; the observed statistic moves as the reader changes the sample
# size. Bottom panel: the power curve for the current sample size.
# ---------------------------------------------------------------------------
ws <- read.csv(file.path(RESULTS, "widget_summary.csv"))
wp <- read.csv(file.path(RESULTS, "widget_power.csv"))

z_grid <- seq(-4, 4, length.out = 401)
crit_z <- 1.96

# Rejection regions (constant): the area under the null density beyond
# +/- 1.96 standard errors.
xl <- seq(-4, -crit_z, length.out = 120)
xr <- seq(4, crit_z, length.out = 120)
left_x  <- c(xl, -crit_z, -4)
left_y  <- c(dnorm(xl), 0, 0)
right_x <- c(xr, crit_z, 4)
right_y <- c(dnorm(xr), 0, 0)

power_grid <- sort(unique(wp$delta)) * 100     # percentage points
power_y <- lapply(ws$n, function(n) {
  p <- wp$power[wp$n == n]
  approx(sort(unique(wp$delta)) * 100, p, xout = power_grid)$y
})

label_for <- function(i) {
  sprintf("n = %s per month<br>z = %+.2f &nbsp;|&nbsp; p-value = %s<br>%s",
          format(ws$n[i], big.mark = ","), ws$z_obs[i],
          ifelse(ws$p_value[i] < 0.001, "< 0.001", sprintf("%.3f", ws$p_value[i])),
          ifelse(ws$p_value[i] < 0.05,
                 "reject H0: the rate really changed",
                 "cannot reject H0: not enough evidence"))
}

p <- plot_ly(height = 720, width = 800)

# --- top panel: the null distribution of the test statistic ---
p <- p |> add_trace(
  x = z_grid, y = dnorm(z_grid), type = "scatter", mode = "lines",
  line = list(color = BLUE, width = 2), fill = "tozeroy",
  fillcolor = "rgba(31,119,180,0.18)", name = "Null distribution", hoverinfo = "skip",
  showlegend = FALSE, xaxis = "x", yaxis = "y")

p <- p |> add_trace(
  x = left_x, y = left_y, type = "scatter", mode = "lines", fill = "toself",
  fillcolor = "rgba(214,39,40,0.18)", line = list(width = 0), name = "Rejection region",
  hoverinfo = "skip", xaxis = "x", yaxis = "y", showlegend = FALSE)

p <- p |> add_trace(
  x = right_x, y = right_y, type = "scatter", mode = "lines", fill = "toself",
  fillcolor = "rgba(214,39,40,0.18)", line = list(width = 0), name = "Rejection region",
  hoverinfo = "skip", xaxis = "x", yaxis = "y", showlegend = FALSE)

# the observed statistic: the trace the slider moves
p <- p |> add_trace(
  x = c(ws$z_obs[1], ws$z_obs[1]), y = c(0, 0.42), type = "scatter", mode = "lines",
  line = list(color = DARK, width = 2, dash = "dash"), name = "Observed statistic",
  hoverinfo = "skip", showlegend = FALSE, xaxis = "x", yaxis = "y")

# --- bottom panel: the power curve ---
p <- p |> add_trace(
  x = power_grid, y = power_y[[1]], type = "scatter", mode = "lines",
  line = list(color = "#f0a202", width = 2.5), name = "Power",
  hoverinfo = "skip", showlegend = FALSE, xaxis = "x2", yaxis = "y2")

p <- p |> add_trace(
  x = c(diff_obs * 100, diff_obs * 100), y = c(0, 1), type = "scatter", mode = "lines",
  line = list(color = DARK, width = 1.2, dash = "dash"), name = "Observed change",
  hoverinfo = "skip", xaxis = "x2", yaxis = "y2", showlegend = FALSE)

p <- p |> add_trace(
  x = power_grid, y = rep(0.8, length(power_grid)), type = "scatter", mode = "lines",
  line = list(color = GREY, width = 1, dash = "dot"), name = "80% power",
  hoverinfo = "skip", xaxis = "x2", yaxis = "y2", showlegend = FALSE)

# One Plotly.update call per slider position: the observed statistic moves
# (trace 3), the power curve is redrawn (trace 4) and the label is rewritten
# (layout annotation 0).
steps <- lapply(seq_len(nrow(ws)), function(i) {
  list(
    label = paste0(format(ws$n[i], big.mark = ","), " per month"),
    method = "update",
    args = list(
      list(
        x = list(c(ws$z_obs[i], ws$z_obs[i]), power_grid),
        y = list(c(0, 0.42), power_y[[i]])
      ),
      list("annotations[0].text" = label_for(i)),
      c(3, 4)
    )
  )
})

p <- p |> layout(
  xaxis  = list(domain = c(0, 1), anchor = "y", range = c(-4, 4),
                title = "Test statistic z",
                zeroline = FALSE, title_font = list(size = 11)),
  yaxis  = list(domain = c(0.60, 1), anchor = "x", range = c(0, 0.52),
                title = "Null density", title_font = list(size = 11)),
  xaxis2 = list(domain = c(0, 1), anchor = "y2",
                title = "True change in the unemployment rate (percentage points)",
                title_font = list(size = 11)),
  yaxis2 = list(domain = c(0.19, 0.45), anchor = "x2", range = c(0, 1),
                title = "Power", title_font = list(size = 11)),
  title = list(text = paste0("How much evidence does the sample size buy?<br>",
                             "<sup>Drag the slider: the statistic moves, the ",
                             "p-value falls, and the verdict can flip</sup>"),
               font = list(size = 14), x = 0.02, xanchor = "left"),
  sliders = list(list(
    active = 0,
    currentvalue = list(prefix = "Sample size: "),
    pad = list(t = 30, b = 10),
    x = 0.13, y = 0.08, len = 0.80,
    steps = steps
  )),
  margin = list(l = 65, r = 30, t = 105, b = 55),
  hovermode = "closest",
  annotations = list(
    # annotations[0] is the label the slider rewrites
    list(x = 0.1, y = 0.47, xref = "x", yref = "y", showarrow = FALSE,
         align = "center", text = label_for(1),
         font = list(size = 12, color = DARK)),
    list(x = -3.55, y = 0.30, xref = "x", yref = "y", showarrow = FALSE,
         text = "null distribution under H0", font = list(size = 10, color = BLUE),
         xanchor = "left"),
    list(x = -2.9, y = 0.20, xref = "x", yref = "y", showarrow = FALSE,
         text = "rejection region (5%)", font = list(size = 10, color = RED)),
    list(x = diff_obs * 100 - 0.02, y = 0.62, xref = "x2", yref = "y2",
         showarrow = FALSE, text = "observed change", font = list(size = 10, color = DARK),
         xanchor = "right"),
    list(x = -0.78, y = 0.845, xref = "x2", yref = "y2", showarrow = FALSE,
         text = "80% power", font = list(size = 10, color = GREY), xanchor = "left")
  )
) |>
  config(displaylogo = FALSE, responsive = TRUE)

saveWidget(p, file.path(FIG, "fig5_interactive.html"), selfcontained = TRUE)

message("figures done")
message("  ", paste(list.files(FIG, pattern = "png|html"), collapse = ", "))
