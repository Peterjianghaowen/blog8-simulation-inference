# Simulation: does the interval really work, and what can a hypothesis test say?
#
# Three simulations run here.
#
# 1. COVERAGE. A population with a known unemployment rate is sampled over and
#    over; each time we build a 95% interval and check whether it contains the
#    truth. A 95% interval should contain the truth about 95% of the time.
#
# 2. THE NULL DISTRIBUTION. Under the null hypothesis that nothing changed,
#    the test statistic is simulated thousands of times, which shows what the
#    rejection region and the p-value actually are.
#
# 3. POWER. For each sample size we ask how often the test would reject when
#    the true change is X - the question "how big a change could this survey
#    have detected?".
#
# Outputs : results/coverage.csv          coverage by sample size and level
#           results/coverage_check.csv    check of the binomial shortcut
#           results/null_distribution.csv simulated null distribution
#           results/null_summary.csv      rejection region, p-value, KS check
#           results/power.csv             power curves
#           results/widget_*.csv          data behind the interactive figure

suppressMessages(library(dplyr))

SET_SEED <- 20261031
set.seed(SET_SEED)

key  <- read.csv("results/key_numbers.csv")
get  <- function(s) key$value[key$stat == s]

p_true   <- get("rate_2019")     # the population rate used in the simulations
diff_obs <- get("diff")          # the change we actually measured
se_boot  <- get("se_diff_boot")  # bootstrap standard error of the change
de       <- get("design_effect") # bootstrap SE / textbook SE
p_pooled <- mean(c(get("rate_2019"), get("rate_2024")))

N_GRID  <- c(250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 80923)
LEVELS  <- c(0.80, 0.90, 0.95, 0.99)
COV_REPS <- 2000

# ---------------------------------------------------------------------------
# 1. Coverage.
#
# For a proportion the bootstrap distribution is exactly Binomial(n, p_hat)/n:
# a bootstrap resample of n people is n independent draws from the empirical
# distribution, which is Bernoulli(p_hat). We therefore compute the interval
# exactly with qbinom instead of drawing resamples, and check the identity
# against an actual resampling bootstrap below.
# ---------------------------------------------------------------------------
coverage <- lapply(N_GRID, function(n) {
  p_hat <- rbinom(COV_REPS, n, p_true) / n
  out <- lapply(LEVELS, function(level) {
    a <- (1 - level) / 2
    lo <- qbinom(a, n, p_hat) / n
    hi <- qbinom(1 - a, n, p_hat) / n
    mean(lo <= p_true & p_true <= hi)
  })
  tibble(n = n, level = LEVELS, coverage = unlist(out))
}) |> bind_rows()

write.csv(coverage, "results/coverage.csv", row.names = FALSE)

# Check the binomial shortcut against a real resampling bootstrap.
n_check <- 1000
sample_draw <- rbinom(n_check, 1, p_true)   # n_check people, each unemployed or not
p_hat <- mean(sample_draw)
boot_draws <- replicate(5000, {
  idx <- sample.int(n_check, n_check, replace = TRUE)
  mean(sample_draw[idx])          # resample of a 0/1 vector = mean of the draws
})
check <- data.frame(
  n = n_check,
  quantile_low_exact    = qbinom(0.025, n_check, p_hat) / n_check,
  quantile_low_resample = unname(quantile(boot_draws, 0.025)),
  quantile_high_exact   = qbinom(0.975, n_check, p_hat) / n_check,
  quantile_high_resample = unname(quantile(boot_draws, 0.975))
)
check$max_abs_difference <- max(
  abs(check$quantile_low_exact - check$quantile_low_resample),
  abs(check$quantile_high_exact - check$quantile_high_resample))
write.csv(check, "results/coverage_check.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. The null distribution of the test statistic.
#
# The recentred bootstrap draws describe the sampling distribution of the
# estimated change around its observed value; shifting them to zero turns them
# into the null distribution of the estimated change under H0: no change.
# The rejection region is the part of that distribution we would call
# "significant", and the p-value is the share of it beyond what we observed.
# ---------------------------------------------------------------------------
boot_diff <- read.csv("results/bootstrap_diff_replicates.csv")$diff_star
null_delta <- boot_diff - diff_obs

# Smooth the simulated null distribution for the figure (kernel density on a
# fixed grid), and simulate 200,000 null draws by resampling the simulated
# distribution so that the p-value is not limited by the number of draws.
grid_delta <- seq(-0.012, 0.012, length.out = 401)   # decimal units: +/- 1.2 pp
null_density <- density(null_delta, from = grid_delta[1], to = grid_delta[length(grid_delta)],
                        n = length(grid_delta))
null_draws <- sample(null_delta, 200000, replace = TRUE)

z_null <- null_draws / se_boot
p_sim  <- mean(abs(null_draws) >= abs(diff_obs))
z_obs  <- diff_obs / se_boot

# The rejection region for a two-sided 5% test.
crit <- as.numeric(quantile(null_draws, c(0.025, 0.975)))

write.csv(
  data.frame(delta = grid_delta, density = null_density$y),
  "results/null_distribution.csv", row.names = FALSE)

# Independent check: simulate the experiment from scratch under H0 and see
# whether the test statistic really is standard normal. The statistic is a
# count, so its distribution is slightly lumpy; the quantiles are compared
# instead of running a Kolmogorov-Smirnov test on a discrete variable.
n_null <- 2000
reps_null <- 20000
x_null <- rbinom(reps_null, n_null, p_pooled)
y_null <- rbinom(reps_null, n_null, p_pooled)
se_null <- sqrt(p_pooled * (1 - p_pooled) * 2 / n_null)
z_null_raw <- (y_null - x_null) / n_null / se_null
q_null <- unname(quantile(z_null_raw, c(0.1, 0.5, 0.9)))
q_norm <- qnorm(c(0.1, 0.5, 0.9))

# ... and the same check on the real data: the recentred bootstrap draws
# should look like the standard normal too. (The test is run on the original
# draws, not on the 200,000 resampled ones, which would have heavy ties.)
ks <- suppressWarnings(ks.test(null_delta / se_boot, "pnorm"))

write.csv(
  data.frame(
    stat = c("p_true", "diff_obs", "se_boot", "design_effect",
             "z_obs", "p_sim", "crit_low", "crit_high",
             "p_norm", "ks_D", "ks_p", "n_null", "reps_null",
             "sim_q10", "sim_q50", "sim_q90",
             "normal_q10", "normal_q50", "normal_q90"),
    value = c(p_true, diff_obs, se_boot, de, z_obs, p_sim,
              crit[1], crit[2], 2 * (1 - pnorm(abs(z_obs))),
              unname(ks$statistic), ks$p.value, n_null, reps_null,
              q_null, q_norm)
  ),
  "results/null_summary.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Power.
#
# Power is the probability of rejecting the null when the true change is not
# zero. The standard error of the change in a survey of n people per month is
# the textbook one scaled by the design effect measured from the real data.
# ---------------------------------------------------------------------------
se_for_n <- function(n) de * sqrt(2 * p_pooled * (1 - p_pooled) / n)

POWER_N <- c(1000, 5000, 10000, 25000, 50000, 80923)
delta_grid <- seq(-0.008, 0.004, length.out = 61)    # decimal units: -0.8 to +0.4 pp

power <- lapply(POWER_N, function(n) {
  se <- se_for_n(n)
  tibble(
    n = n,
    delta = delta_grid,
    # P(change < -1.96 se) + P(change > +1.96 se), where the estimated change
    # is normal with mean delta and standard error se.
    power = pnorm(-1.96 - delta / se) +
            pnorm(1.96 - delta / se, lower.tail = FALSE)
  )
}) |> bind_rows()

write.csv(power, "results/power.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Data behind the interactive figure.
# ---------------------------------------------------------------------------
WIDGET_N <- c(500, 1000, 2500, 5000, 10000, 25000, 50000, 80923)

widget_power <- lapply(WIDGET_N, function(n) {
  se <- se_for_n(n)
  tibble(n = n, delta = delta_grid,
         power = pnorm(-1.96 - delta / se) +
                 pnorm(1.96 - delta / se, lower.tail = FALSE))
}) |> bind_rows()

# Everything the interactive figure needs for one position of the slider:
# the standard error of a survey of n people per month, the interval it would
# produce for the change we measured, the test statistic, and the p-value.
widget_summary <- tibble(
  n = WIDGET_N,
  se = sapply(WIDGET_N, se_for_n),
  ci_low  = diff_obs - 1.96 * sapply(WIDGET_N, se_for_n),
  ci_high = diff_obs + 1.96 * sapply(WIDGET_N, se_for_n),
  z_obs = diff_obs / sapply(WIDGET_N, se_for_n),
  p_value = 2 * (1 - pnorm(abs(diff_obs / sapply(WIDGET_N, se_for_n))))
)

write.csv(widget_power,   "results/widget_power.csv",   row.names = FALSE)
write.csv(widget_summary, "results/widget_summary.csv", row.names = FALSE)

message("simulations done")
message(sprintf("  coverage at n = 250, 1000, 80000 (95%% level): %.3f, %.3f, %.3f",
                coverage$coverage[coverage$n == 250 & coverage$level == 0.95],
                coverage$coverage[coverage$n == 1000 & coverage$level == 0.95],
                coverage$coverage[coverage$n == 80923 & coverage$level == 0.95]))
message(sprintf("  simulated p-value %.4f | rejection region [%+.4f, %+.4f] pp",
                p_sim, 100 * crit[1], 100 * crit[2]))
message(sprintf("  KS check of the real-data null distribution: D = %.4f, p = %.3f",
                unname(ks$statistic), ks$p.value))
message(sprintf("  from-scratch simulation quantiles (10/50/90%%): %.3f %.3f %.3f (normal: %.3f %.3f %.3f)",
                q_null[1], q_null[2], q_null[3], q_norm[1], q_norm[2], q_norm[3]))
message(sprintf("  binomial shortcut check: max quantile difference %.5f",
                check$max_abs_difference))
