# Bootstrap confidence intervals for the CPS unemployment rate, and for the
# change in the rate between January 2019 and January 2024.
#
# The bootstrap answers a question the formula cannot: "if we had interviewed a
# different set of people, how much would our answer move?" We answer it by
# resampling the people we did interview, with replacement, and recomputing the
# statistic every time.
#
# Inputs  : data/processed/focal_people.csv (from 02_clean.R)
# Outputs : results/bootstrap_replicates.csv       bootstrap draws of each rate
#           results/bootstrap_diff_replicates.csv  bootstrap draws of the change
#           results/small_sample.csv               what a smaller survey would
#                                                  have concluded
#           results/key_numbers.csv                the numbers quoted in the post

suppressMessages(library(dplyr))

B         <- 2000    # bootstrap replicates for the headline estimates
SIM_REPS  <- 1000    # repetitions of the "smaller survey" experiment
SIM_BOOT  <- 100     # bootstrap replicates inside each repetition
SET_SEED  <- 20261031

set.seed(SET_SEED)

focal <- read.csv("data/processed/focal_people.csv")
A    <- focal[focal$month_id == "2019-01", ]
Bdat <- focal[focal$month_id == "2024-01", ]

# ---------------------------------------------------------------------------
# The statistic: the weighted unemployment rate (unemployed / labor force).
# ---------------------------------------------------------------------------
rate <- function(d, idx = seq_len(nrow(d))) {
  w <- d$weight[idx]
  sum(w * d$in_lf[idx] * d$unemp[idx]) / sum(w * d$in_lf[idx])
}

rate_A   <- rate(A)
rate_B   <- rate(Bdat)
diff_obs <- rate_B - rate_A

# ---------------------------------------------------------------------------
# The bootstrap: resample people with replacement, recompute, repeat.
# ---------------------------------------------------------------------------
bootstrap_rate <- function(d, B) {
  n <- nrow(d)
  replicate(B, {
    idx <- sample.int(n, n, replace = TRUE)
    rate(d, idx)
  })
}

boot_A    <- bootstrap_rate(A, B)
boot_B    <- bootstrap_rate(Bdat, B)
boot_diff <- boot_B - boot_A

percentile_ci <- function(x, level = 0.95) {
  alpha <- (1 - level) / 2
  unname(quantile(x, c(alpha, 1 - alpha)))
}

ci_A    <- percentile_ci(boot_A)
ci_B    <- percentile_ci(boot_B)
ci_diff <- percentile_ci(boot_diff)

# The textbook (Wald) interval, for comparison with the bootstrap.
wald_se <- function(p, n) sqrt(p * (1 - p) / n)
se_A     <- wald_se(rate_A, nrow(A))
se_B     <- wald_se(rate_B, nrow(Bdat))
se_diff  <- sqrt(se_A^2 + se_B^2)

# The bootstrap standard error of the change, and the design effect: how much
# wider the bootstrap interval is than the textbook one. The CPS is a complex
# sample with very unequal weights, so the textbook formula - which assumes a
# simple random sample of equal-weight people - understates the real sampling
# error.
se_diff_boot <- sd(boot_diff)
design_effect <- se_diff_boot / se_diff

# ---------------------------------------------------------------------------
# Hypothesis testing by simulation.
#
# Under H0 ("the two months have the same unemployment rate") the estimated
# change has a sampling distribution centred on zero. The bootstrap draws we
# just computed describe the sampling distribution of the estimate around its
# observed value, so recentring them on zero gives a simulated null
# distribution of the estimated change - and the share of it that lies beyond
# the observed change is a simulation-based p-value.
# ---------------------------------------------------------------------------
null_delta <- boot_diff - diff_obs
p_sim  <- mean(abs(null_delta) >= abs(diff_obs))
p_null <- 2 * (1 - pnorm(abs(diff_obs) / se_diff_boot))

z_obs    <- diff_obs / se_diff_boot
z_wald   <- diff_obs / se_diff
p_wald   <- 2 * (1 - pnorm(abs(z_wald)))

# ---------------------------------------------------------------------------
# "What if we had interviewed fewer people?"
#
# A survey of n people is n draws from the population; the population is
# approximated here by the empirical distribution of the sample we have, so
# the draws are made with replacement. For each survey we estimate the change,
# build the interval exactly as the main analysis does, and record whether the
# interval (a) excludes zero - the survey found a "significant" change - and
# (b) covers the full-sample estimate. The textbook interval is put through
# the same test, which shows what the unequal CPS weights cost you.
# ---------------------------------------------------------------------------
bootstrap_rate_matrix <- function(d, B, idx) {
  # Indexing a vector with a matrix drops the shape in R, so it is restored
  # explicitly before the row sums.
  num <- rowSums(matrix((d$weight * d$in_lf * d$unemp)[idx], nrow = nrow(idx)))
  den <- rowSums(matrix((d$weight * d$in_lf)[idx], nrow = nrow(idx)))
  num / den
}

small_sample <- lapply(c(500, 1000, 2500, 5000, 10000, 25000), function(n) {
  reps <- replicate(SIM_REPS, {
    subA <- A[sample.int(nrow(A), n, replace = TRUE), ]
    subB <- Bdat[sample.int(nrow(Bdat), n, replace = TRUE), ]
    pA <- rate(subA)
    pB <- rate(subB)
    d  <- pB - pA

    # bootstrap interval for the change: resample inside each month separately
    idxA <- matrix(sample.int(n, n * SIM_BOOT, replace = TRUE),
                   nrow = SIM_BOOT, ncol = n)
    idxB <- matrix(sample.int(n, n * SIM_BOOT, replace = TRUE),
                   nrow = SIM_BOOT, ncol = n)
    d_star <- bootstrap_rate_matrix(subB, SIM_BOOT, idxB) -
              bootstrap_rate_matrix(subA, SIM_BOOT, idxA)
    ci_b <- percentile_ci(d_star)

    # textbook interval for the change
    se   <- sqrt(wald_se(pA, n)^2 + wald_se(pB, n)^2)
    ci_w <- d + c(-1, 1) * 1.96 * se

    c(power_boot = as.numeric(ci_b[1] * ci_b[2] > 0),
      power_wald = as.numeric(ci_w[1] * ci_w[2] > 0),
      cover_boot = as.numeric(ci_b[1] <= diff_obs && diff_obs <= ci_b[2]),
      cover_wald = as.numeric(ci_w[1] <= diff_obs && diff_obs <= ci_w[2]),
      width_boot = ci_b[2] - ci_b[1],
      width_wald = ci_w[2] - ci_w[1])
  })
  reps <- t(reps)
  data.frame(
    n_per_month   = n,
    power_boot    = mean(reps[, "power_boot"]),
    power_wald    = mean(reps[, "power_wald"]),
    coverage_boot = mean(reps[, "cover_boot"]),
    coverage_wald = mean(reps[, "cover_wald"]),
    median_width_boot = median(reps[, "width_boot"]),
    median_width_wald = median(reps[, "width_wald"])
  )
}) |> bind_rows()

# ---------------------------------------------------------------------------
# Save everything the figures and the blog post read.
# ---------------------------------------------------------------------------
write.csv(
  data.frame(month_id = rep(c("2019-01", "2024-01"), each = B),
             replicate = rep(seq_len(B), 2),
             rate_star = c(boot_A, boot_B)),
  "results/bootstrap_replicates.csv", row.names = FALSE)

write.csv(
  data.frame(replicate = seq_len(B), diff_star = boot_diff),
  "results/bootstrap_diff_replicates.csv", row.names = FALSE)

write.csv(small_sample, "results/small_sample.csv", row.names = FALSE)

key <- data.frame(
  stat = c(
    "rate_2019", "rate_2024", "diff",
    "ci_low_2019", "ci_high_2019", "ci_low_2024", "ci_high_2024",
    "ci_low_diff", "ci_high_diff",
    "wald_low_diff", "wald_high_diff",
    "se_diff_boot", "se_diff_wald", "design_effect",
    "z_boot", "p_sim", "p_wald",
    "n_2019", "n_2024", "boot_replicates"
  ),
  value = c(
    rate_A, rate_B, diff_obs,
    ci_A[1], ci_A[2], ci_B[1], ci_B[2], ci_diff[1], ci_diff[2],
    diff_obs - 1.96 * se_diff, diff_obs + 1.96 * se_diff,
    se_diff_boot, se_diff, design_effect,
    z_obs, p_sim, p_wald,
    nrow(A), nrow(Bdat), B
  )
)
write.csv(key, "results/key_numbers.csv", row.names = FALSE)

message("bootstrap done")
message(sprintf("  January 2019 : %.4f  95%% CI [%.4f, %.4f]  (n = %d)",
                rate_A, ci_A[1], ci_A[2], nrow(A)))
message(sprintf("  January 2024 : %.4f  95%% CI [%.4f, %.4f]  (n = %d)",
                rate_B, ci_B[1], ci_B[2], nrow(Bdat)))
message(sprintf("  change       : %+.4f pp  bootstrap 95%% CI [%+.4f, %+.4f]",
                100 * diff_obs, 100 * ci_diff[1], 100 * ci_diff[2]))
message(sprintf("               textbook 95%% CI [%+.4f, %+.4f]  (design effect %.2f)",
                100 * (diff_obs - 1.96 * se_diff), 100 * (diff_obs + 1.96 * se_diff),
                design_effect))
message(sprintf("  simulation p-value %.4f | textbook p-value %.4f",
                p_sim, p_wald))
print(small_sample)
