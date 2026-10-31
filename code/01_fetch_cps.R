# Fetch the IPUMS CPS microdata used in the post.
#
# The raw microdata are not redistributed here (see README.md). This script
# recreates them through the IPUMS API, which needs a free API key:
#
#   1. Register at https://cps.ipums.org and create a key at
#      https://account.ipums.org/api_keys.
#   2. In R: ipumsr::set_ipums_api_key("YOUR_KEY", save = TRUE)
#
# The extract requests the monthly CPS samples from January 2019 onward and
# only the variables the analysis uses.

suppressMessages({
  library(ipumsr)
  library(dplyr)
})

RAW_DIR <- "data/raw"
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

DDI <- file.path(RAW_DIR, "cps_00002.xml")
DAT <- file.path(RAW_DIR, "cps_00002.dat.gz")

if (file.exists(DDI) && file.exists(DAT)) {
  message("raw extract already present in data/raw/ - nothing to download")
  quit(save = "no", status = 0)
}

key <- tryCatch(get_ipums_api_key(), error = function(e) NULL)
if (is.null(key) || is.na(key) || !nzchar(key)) {
  stop(
    "No IPUMS API key found and data/raw/ is empty.\n",
    "  Register at https://cps.ipums.org, create a key at https://account.ipums.org/api_keys,\n",
    "  then run: ipumsr::set_ipums_api_key(\"YOUR_KEY\", save = TRUE)\n",
    "  and re-run this script. See README.md for the exact extract specification.",
    call. = FALSE
  )
}

# Every monthly CPS sample from January 2019 onwards (the ASEC annual sample is
# excluded by the regular-expression match below).
month_pattern <- paste(month.name, collapse = "|")
samples <- get_sample_info("cps") |>
  filter(grepl(paste0("^IPUMS-CPS, (", month_pattern, ") (2019|202\\d)$"), description)) |>
  pull(name)

extract <- define_extract_micro(
  collection  = "cps",
  description = "Monthly CPS 2019+ for blog post 8",
  samples     = samples,
  variables   = c("YEAR", "MONTH", "AGE", "EMPSTAT", "LABFORCE", "WTFINL")
)

extract |>
  submit_extract() |>
  wait_for_extract() |>
  download_extract(download_dir = RAW_DIR)

message("downloaded to ", RAW_DIR)
