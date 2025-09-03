#!/usr/bin/env Rscript

# ============================================================
# Nugget Analysis & Visualisation
# - Either run nugget counting (slow) or analyse existing nuggets (fast)
# - Adds normalised counts: per 100 words, per Wh
# - Creates summary tables + ggplot2 charts
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(ggplot2); library(fs)
})

# --- CLI args ---
args <- commandArgs(trailingOnly = TRUE)
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
cli <- parse_kv(args)
dialogs_dir   <- cli$dialogs_dir   %||% NA_character_  # e.g., "/home/david/sim_runs/test_20250902_152940"
dialogs_glob  <- cli$dialogs_glob  %||% "dialogs_flat_*.csv"


mode            <- cli$mode    %||% "analyze"   # "count" or "analyze"
path_nuggets    <- cli$nuggets %||% "agent_nuggets_by_step.csv"
outdir          <- cli$outdir  %||% "analysis_out"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- Load nuggets ---
if (!file.exists(path_nuggets)) {
  stop("Nuggets file not found: ", path_nuggets, "\nRun the counter first or set mode=count.")
}
nuggets <- readr::read_csv(path_nuggets, show_col_types = FALSE)

# --- Normalisations ---
nuggets <- nuggets %>%
  mutate(
    nuggets_per_100w = ifelse(agent_text_words > 0, nugget_count / agent_text_words * 100, NA_real_),
    nuggets_per_Wh   = ifelse(!is.na(energy_Wh) & energy_Wh > 0, nugget_count / energy_Wh, NA_real_)
  )



attach_step_energy <- function(nuggets_df, ddir, pattern) {
  if (is.na(ddir) || !dir_exists(ddir)) {
    message("No dialogs_dir provided (or not found); skipping energy join.")
    return(nuggets_df)
  }
  files <- dir_ls(ddir, glob = file.path(ddir, pattern))
  if (length(files) == 0) {
    message("No dialogs_flat files matched; skipping energy join.")
    return(nuggets_df)
  }
  message("Reading dialogs from: ", ddir, " (", length(files), " files)")
  dialogs <- purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE))
  
  # step-level energy (sum agent energy within step)
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

# Attach energy if available
nuggets <- attach_step_energy(nuggets, dialogs_dir, dialogs_glob)


# --- Summaries ---
summary_cluster_recipe <- nuggets %>%
  group_by(cluster, recipe_title) %>%
  summarise(
    steps = n(),
    total_nuggets = sum(nugget_count, na.rm = TRUE),
    mean_nuggets = mean(nugget_count, na.rm = TRUE),
    mean_per_100w = mean(nuggets_per_100w, na.rm = TRUE),
    mean_per_Wh   = mean(nuggets_per_Wh, na.rm = TRUE),
    .groups = "drop"
  )

summary_cluster <- nuggets %>%
  group_by(cluster) %>%
  summarise(
    steps = n(),
    mean_nuggets = mean(nugget_count, na.rm = TRUE),
    mean_per_100w = mean(nuggets_per_100w, na.rm = TRUE),
    mean_per_Wh   = mean(nuggets_per_Wh, na.rm = TRUE),
    .groups = "drop"
  )

# Save summaries
readr::write_csv(summary_cluster_recipe, file.path(outdir, "summary_cluster_recipe.csv"))
readr::write_csv(summary_cluster, file.path(outdir, "summary_cluster.csv"))

cat("\n=== Summary by Cluster & Recipe ===\n")
print(summary_cluster_recipe, n = Inf)

cat("\n=== Summary by Cluster ===\n")
print(summary_cluster, n = Inf)

# --- Visualisations ---
## Heatmap
p1 <- ggplot(summary_cluster_recipe, aes(x = recipe_title, y = cluster, fill = mean_nuggets)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", mean_nuggets)), color = "white") +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Mean Nuggets per Step (Cluster × Recipe)", x = "Recipe", y = "Cluster")

ggsave(file.path(outdir, "heatmap_mean_nuggets.png"), p1, width = 10, height = 6)

## Boxplot nuggets per step
p2 <- ggplot(nuggets, aes(x = cluster, y = nugget_count, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Nugget Counts per Step", x = "Cluster", y = "Nuggets") +
  theme(legend.position = "none")

ggsave(file.path(outdir, "boxplot_nuggets.png"), p2, width = 6, height = 5)

## Scatter nuggets vs. words
p3 <- ggplot(nuggets, aes(x = agent_text_words, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Nuggets vs. Agent Text Length", x = "Agent words", y = "Nuggets")

ggsave(file.path(outdir, "scatter_nuggets_vs_words.png"), p3, width = 7, height = 5)

## Scatter nuggets vs. Wh
p4 <- ggplot(nuggets, aes(x = energy_Wh, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Nuggets vs. Energy (Wh)", x = "Energy (Wh)", y = "Nuggets")

ggsave(file.path(outdir, "scatter_nuggets_vs_energy.png"), p4, width = 7, height = 5)

cat("\n[Done] Summaries and plots written to: ", normalizePath(outdir), "\n")
