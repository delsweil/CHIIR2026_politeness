#!/usr/bin/env Rscript

# This script samples conversations for the linguists to annotate. 
# The aim being to validate the cluster simulation approach

# run using the following

#Rscript build_conversations.R \
#DIALOGS_DIR="/home/david/sim_runs/test_20250902_152940" \
#DIALOGS_GLOB="dialogs_flat_*.csv" \
#OUT_CSV="conversations.csv" \
#N_PER_CLUSTER=10 \
#MAX_TURNS=14



suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr); library(jsonlite); library(fs)
})

# ---------- CONFIG ----------
dialogs_dir  <- Sys.getenv("DIALOGS_DIR", unset = "~/sim_runs/test_20250902_152940")
glob_pattern <- Sys.getenv("DIALOGS_GLOB", unset = "dialogs_flat_*.csv")
out_csv      <- Sys.getenv("OUT_CSV",     unset = "conversations.csv")
# sampling controls
N_CONV_PER_CLUSTER <- as.integer(Sys.getenv("N_PER_CLUSTER", unset = "12"))
MAX_TURNS_PER_CONV <- as.integer(Sys.getenv("MAX_TURNS",     unset = "12"))
INCLUDE_AGENT      <- tolower(Sys.getenv("INCLUDE_AGENT",   unset = "true")) %in% c("1","true","t","yes","y")

# ---------- LOAD ----------
# ---- find dialog files (robust) ----
files <- tryCatch(
  fs::dir_ls(path = dialogs_dir, glob = dialogs_glob),
  error = function(e) character(0)
)

# fallback to base R if fs glob fails
if (!length(files)) {
  files <- list.files(dialogs_dir, pattern = glob2rx(dialogs_glob),
                      full.names = TRUE, recursive = FALSE)
}

if (!length(files)) {
  stop("No dialogs_flat files found.\nSearched in: ", dialogs_dir,
       "\nUsing glob: ", dialogs_glob,
       "\nTry: ls -l ", file.path(dialogs_dir, dialogs_glob))
}

# stopifnot(length(files) > 0)
dialogs <- purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE))

# ---------- BLIND + SHAPE ----------
# Select turns for each conversation; sample N per cluster
set.seed(2025)
sampled_ids <- dialogs %>%
  distinct(cluster, conversation_id) %>%
  group_by(cluster) %>%
  slice_sample(n = min(n(), N_CONV_PER_CLUSTER)) %>%
  ungroup()

subset <- dialogs %>%
  semi_join(sampled_ids, by = c("cluster","conversation_id")) %>%
  arrange(cluster, conversation_id, event_idx)

# Build a compact JSON turns column for Shiny
make_json_turns <- function(df) {
  df %>%
    mutate(role = ifelse(role %in% c("user","agent"), role, "user")) %>%
    select(role, text) %>%
    mutate(text = ifelse(!INCLUDE_AGENT & role == "agent", "", text)) %>%
    head(MAX_TURNS_PER_CONV) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
}

convs <- subset %>%
  group_by(cluster, conversation_id, recipe_title) %>%
  summarise(
    json_turns = make_json_turns(cur_data()),
    .groups = "drop"
  ) %>%
  # Hide true cluster label from annotators (keep for later evaluation)
  mutate(cluster_hidden = cluster, cluster = NULL)

readr::write_csv(convs, out_csv)
cat("Wrote:", normalizePath(out_csv), "\n")
