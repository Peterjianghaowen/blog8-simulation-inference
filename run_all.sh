#!/usr/bin/env bash
# Run from the repository root:
#   ./run_all.sh
#
# The scripts use paths relative to the repository root and must be run in
# numeric order, because each one reads the tables the previous one wrote.
# 01 downloads the IPUMS extract, so it needs a network connection and an
# IPUMS API key; the remaining scripts only read what is already on disk.
set -euo pipefail

Rscript code/01_fetch_cps.R
Rscript code/02_clean.R
Rscript code/03_bootstrap.R
Rscript code/04_simulate.R
Rscript code/05_figures.R
