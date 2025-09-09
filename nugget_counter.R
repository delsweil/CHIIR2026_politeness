#!/usr/bin/env Rscript

# ============================================================
# Nugget Analysis & Visualisation (robust)
# - Analyses an existing nuggets file (fast)
# - Optionally joins step-level energy from dialogs_flat_*.csv
# - Adds normalised counts: per 100 words, per Wh (if energy available)
# - Creates summary tables + ggplot2 charts
# CLI example:
# Rscript nugget_counter.R \
#   nuggets="agent_nuggets_by_step.csv" \
#   dialogs_dir="/home/david/sim_runs/test_20250908_130819" \
#   dialogs_glob="dialogs_flat_*.csv" \
#   outdir="analysis_out"
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(ggplot2); library(fs); library(cli)
})

# ---------- Helpers ----------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    k <- tolower(trimws(kv[1])); v <- trimws(paste(kv[-1], collapse="="))
    out[[k]] <- v
  }
  out
}

# ---------- CLI (MUST be before any progress steps) ----------
args <- commandArgs(trailingOnly = TRUE)
cli_args <- parse_kv(args)

# If this is the *analyze* script:
path_nuggets <- cli_args$nuggets %||% "agent_nuggets_by_step.csv"
dialogs_dir  <- cli_args$dialogs_dir %||% NA_character_
dialogs_glob <- cli_args$dialogs_glob %||% "dialogs_flat_*.csv"
outdir       <- cli_args$outdir %||% "analysis_out"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# If this is the *counter* script, use its arg names instead:
# ddir       <- cli_args$dir       %||% NA_character_
# pattern    <- cli_args$pattern   %||% "dialogs_flat_*.csv"
# step_lookup<- cli_args$step_lookup %||% NA_character_
# out        <- cli_args$out       %||% "agent_nuggets_by_step.csv"
# model      <- cli_args$model     %||% "deepseek-r1:8b"
# max_rows   <- as.integer(cli_args$max_rows %||% 100000)

# ---------- Progress steps ----------
steps <- c(
  "Load nuggets",
  "Join step-level energy",
  "Compute normalisations",
  "Compute summaries",
  "Save summaries",
  "Create plots"
)

cli_progress_step(steps[1])
nuggets <- readr::read_csv(path_nuggets, show_col_types = FALSE)

cli_progress_step(steps[2])
nuggets <- attach_step_energy(nuggets, dialogs_dir, dialogs_glob)

cli_alert_success("All summaries and plots written to: {normalizePath(outdir)}")
