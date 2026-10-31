# Build the analysis files from the IPUMS CPS extract.
#
# Inputs  : data/raw/cps_00002.xml + data/raw/cps_00002.dat.gz (written by 01)
# Outputs : data/processed/focal_people.csv   person-level records for the two
#                                            months compared in the post
#            results/monthly_rates.csv         the national series (reference)
#
# The post compares the same month in two different years (January 2019 and
# January 2024) so that the seasonal pattern of the CPS cancels out.

suppressMessages({
  library(ipumsr)
  library(dplyr)
})

RAW_DDI <- "data/raw/cps_00002.xml"
PROC    <- "data/processed"
RESULTS <- "results"
dir.create(PROC, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS, showWarnings = FALSE, recursive = TRUE)

# The two months the post compares.
YEAR_A <- 2019; MONTH_A <- 1   # pre-pandemic reference month
YEAR_B <- 2024; MONTH_B <- 1   # the month under study

# ---------------------------------------------------------------------------
# Read the microdata. Only the variables the analysis needs are pulled, which
# keeps the read fast.
# ---------------------------------------------------------------------------
ddi <- read_ipums_ddi(RAW_DDI)
cps <- read_ipums_micro(ddi, vars = c("YEAR", "MONTH", "AGE", "EMPSTAT",
                                      "LABFORCE", "WTFINL"))
cps <- cps |> haven::zap_labels()

# ---------------------------------------------------------------------------
# Employment status, following the CPS definitions used in the class material:
#   in the labor force : LABFORCE == 2
#   unemployed          : EMPSTAT in {20, 21, 22}
#   employed            : EMPSTAT in {10, 12}
# The universe is the civilian noninstitutional population aged 16+.
# ---------------------------------------------------------------------------
cps <- cps |>
  filter(AGE >= 16, EMPSTAT != 1, LABFORCE %in% c(1, 2)) |>
  mutate(
    in_lf = LABFORCE == 2,
    unemp = EMPSTAT %in% c(20, 21, 22),
    employed = EMPSTAT %in% c(10, 12)
  )

# ---------------------------------------------------------------------------
# The national monthly series, kept in results/ as a reference table.
# ---------------------------------------------------------------------------
# A population rate estimated with the CPS person weight:
#   sum of the weights of the people in the group / sum of all the weights.
weighted_rate <- function(w, indicator) sum(w * indicator) / sum(w)

monthly_rates <- cps |>
  group_by(YEAR, MONTH) |>
  summarise(
    n            = n(),
    # The unemployment rate is unemployed / labor force (the BLS definition),
    # so the denominator is the weighted labor force, not the population.
    unemployment = weighted_rate(WTFINL * in_lf, unemp),
    employment   = weighted_rate(WTFINL * in_lf, employed),
    lfpr         = weighted_rate(WTFINL, in_lf),
    .groups = "drop"
  ) |>
  arrange(YEAR, MONTH)

write.csv(monthly_rates, file.path(RESULTS, "monthly_rates.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Person-level records for the two months that are compared in the post.
# ---------------------------------------------------------------------------
focal <- cps |>
  filter((YEAR == YEAR_A & MONTH == MONTH_A) | (YEAR == YEAR_B & MONTH == MONTH_B)) |>
  mutate(month_id = if_else(YEAR == YEAR_A, "2019-01", "2024-01")) |>
  select(month_id, in_lf, unemp, employed, weight = WTFINL)

write.csv(focal, file.path(PROC, "focal_people.csv"), row.names = FALSE)

message("cleaning done")
message("  results/monthly_rates.csv      ", nrow(monthly_rates), " months")
message("  data/processed/focal_people.csv", nrow(focal), " person-months")
for (m in unique(focal$month_id)) {
  d <- focal[focal$month_id == m, ]
  message("  ", m, ": n = ", nrow(d),
          ", unemployment rate = ",
          sprintf("%.4f", weighted_rate(d$weight * d$in_lf, d$unemp)))
}
