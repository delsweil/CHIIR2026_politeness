suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(ggplot2); library(fs); library(cli)
})

# --- Progress setup ---
steps <- c(
  "Load nuggets",
  "Join step-level energy",
  "Compute normalisations",
  "Compute summaries",
  "Save summaries",
  "Create plots"
)
pb <- cli_progress_bar("Analysis progress", total = length(steps))

# ---------- Load nuggets ----------
cli_progress_update(id = pb, msg = steps[1])
nuggets <- readr::read_csv(path_nuggets, show_col_types = FALSE)

# ---------- Optional energy join ----------
cli_progress_update(id = pb, msg = steps[2])
nuggets <- attach_step_energy(nuggets, dialogs_dir, dialogs_glob)

# ---------- Normalisations ----------
cli_progress_update(id = pb, msg = steps[3])
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
cli_progress_update(id = pb, msg = steps[4])
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

# ---------- Save summaries ----------
cli_progress_update(id = pb, msg = steps[5])
readr::write_csv(summary_cluster_recipe, file.path(outdir, "summary_cluster_recipe.csv"))
readr::write_csv(summary_cluster,        file.path(outdir, "summary_cluster.csv"))

# ---------- Visualisations ----------
cli_progress_update(id = pb, msg = steps[6])
# … (plots unchanged)

cli_progress_done(id = pb)
cat("\n[Done] Summaries and plots written to: ", normalizePath(outdir), "\n", sep = "")
