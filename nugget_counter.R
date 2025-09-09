#!/usr/bin/env Rscript

# ============================================================
# Nugget Analysis & Visualisation (robust)
# - Analyses an existing nuggets file (fast)
# - Optionally joins step-level energy from dialogs_flat_*.csv
# - Adds normalised counts: per 100 words, per Wh (if energy available)
# - Creates summary tables + ggplot2 charts
# CLI example:
# Rscript nugget_analyze.R \
#   nuggets="agent_nuggets_by_step.csv" \
#   dialogs_dir="/home/david/sim_runs/test_20250902_152940" \
#   dialogs_glob="dialogs_flat_*.csv" \
#   outdir="analysis_out"
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(ggplot2); library(fs)
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

attach_step_energy <- function(nuggets_df, ddir, pattern = "dialogs_flat_*.csv") {
  if (is.na(ddir) || !fs::dir_exists(ddir)) {
    message("No dialogs_dir provided (or not found); skipping energy join.")
    return(nuggets_df)
  }
  files <- fs::dir_ls(ddir, glob = file.path(ddir, pattern))
  if (!length(files)) {
    message("No dialogs_flat files matched; skipping energy join.")
    return(nuggets_df)
  }
  message("Reading dialogs from: ", ddir, " (", length(files), " files)")
  
  dialogs <- purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE))
  
  step_energy <- dialogs %>%
    dplyr::filter(role == "agent") %>%
    dplyr::group_by(conversation_id, recipe_title, step_id) %>%
    dplyr::summarise(
      energy_Wh = if (all(is.na(energy_Wh))) NA_real_ else sum(energy_Wh, na.rm = TRUE),
      .groups = "drop"
    )
  
  nuggets_df %>%
    dplyr::left_join(step_energy,
                     by = c("conversation_id","recipe_title","step_id"))
}

# ---------- CLI ----------
args <- commandArgs(trailingOnly = TRUE)
cli  <- parse_kv(args)

path_nuggets <- cli$nuggets    %||% "agent_nuggets_by_step.csv"
dialogs_dir  <- cli$dialogs_dir %||% NA_character_
dialogs_glob <- cli$dialogs_glob %||% "dialogs_flat_*.csv"
outdir       <- cli$outdir      %||% "analysis_out"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(path_nuggets)) {
  stop("Nuggets file not found: ", path_nuggets)
}

# ---------- Progress (base R) ----------
total_steps <- 6L  # update if you add/remove checkpoints
.i <- 0L
pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
on.exit({ utils::setTxtProgressBar(pb, total_steps); close(pb) }, add = TRUE)

step <- function(msg) {
  .i <<- .i + 1L
  cat(sprintf("\n[%02d/%02d] %s\n", .i, total_steps, msg))
  utils::setTxtProgressBar(pb, .i)
}

# ---------- Load nuggets ----------
step("Load nuggets")
nuggets <- readr::read_csv(path_nuggets, show_col_types = FALSE)

# Optional energy join (safe if not present)
step("Join step-level energy")
nuggets <- attach_step_energy(nuggets, dialogs_dir, dialogs_glob)

# ---------- Normalisations (robust to missing energy) ----------
step("Compute normalisations")
has_energy <- "energy_Wh" %in% names(nuggets)

nuggets <- nuggets %>%
  mutate(
    nuggets_per_100w = dplyr::if_else(agent_text_words > 0,
                                      nugget_count / agent_text_words * 100,
                                      NA_real_),
    nuggets_per_Wh = if (has_energy) {
      dplyr::if_else(!is.na(energy_Wh) & energy_Wh > 0,
                     nugget_count / energy_Wh, NA_real_)
    } else {
      NA_real_
    }
  )

# ---------- Summaries ----------
step("Compute summaries")
summary_cluster_recipe <- nuggets %>%
  group_by(cluster, recipe_title) %>%
  summarise(
    steps         = n(),
    total_nuggets = sum(nugget_count, na.rm = TRUE),
    mean_nuggets  = mean(nugget_count, na.rm = TRUE),
    mean_per_100w = mean(nuggets_per_100w, na.rm = TRUE),
    mean_per_Wh   = if (has_energy) mean(nuggets_per_Wh, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

summary_cluster <- nuggets %>%
  group_by(cluster) %>%
  summarise(
    steps         = n(),
    mean_nuggets  = mean(nugget_count, na.rm = TRUE),
    mean_per_100w = mean(nuggets_per_100w, na.rm = TRUE),
    mean_per_Wh   = if (has_energy) mean(nuggets_per_Wh, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# Save summaries
step("Save summaries")
readr::write_csv(summary_cluster_recipe, file.path(outdir, "summary_cluster_recipe.csv"))
readr::write_csv(summary_cluster,        file.path(outdir, "summary_cluster.csv"))

cat("\n=== Summary by Cluster & Recipe ===\n")
print(summary_cluster_recipe, n = Inf)

cat("\n=== Summary by Cluster ===\n")
print(summary_cluster, n = Inf)

# ---------- Visualisations ----------
step("Create plots")
# Heatmap: mean nuggets per step
p1 <- ggplot(summary_cluster_recipe, aes(x = recipe_title, y = cluster, fill = mean_nuggets)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", mean_nuggets)), color = "white") +
  scale_fill_gradient(low = "skyblue", high = "darkblue", na.value = "grey80") +
  labs(title = "Mean Nuggets per Step (Cluster × Recipe)", x = "Recipe", y = "Cluster") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(outdir, "heatmap_mean_nuggets.png"), p1, width = 11, height = 6, dpi = 150)

# Boxplot: nugget counts per step
p2 <- ggplot(nuggets, aes(x = cluster, y = nugget_count, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Nugget Counts per Step", x = "Cluster", y = "Nuggets") +
  theme(legend.position = "none")
ggsave(file.path(outdir, "boxplot_nuggets.png"), p2, width = 7, height = 5, dpi = 150)

# Scatter: nuggets vs words
p3 <- ggplot(nuggets, aes(x = agent_text_words, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Nuggets vs. Agent Text Length", x = "Agent words", y = "Nuggets")
ggsave(file.path(outdir, "scatter_nuggets_vs_words.png"), p3, width = 8, height = 5, dpi = 150)

# Scatter: nuggets vs Wh (only if energy present)
if (has_energy && any(!is.na(nuggets$energy_Wh))) {
  p4 <- ggplot(nuggets, aes(x = energy_Wh, y = nugget_count, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Nuggets vs. Energy (Wh)", x = "Energy (Wh)", y = "Nuggets")
  ggsave(file.path(outdir, "scatter_nuggets_vs_energy.png"), p4, width = 8, height = 5, dpi = 150)
} else {
  message("Energy not available; skipping nuggets vs Wh plot.")
}

cat("\n[Done] Summaries and plots written to: ", normalizePath(outdir), "\n", sep = "")
