#!/usr/bin/env Rscript

# Sample conversations for linguist annotation (cluster validation)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(jsonlite); library(fs)
})

# ---------- utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x

# ---------- CLI ----------
args <- commandArgs(trailingOnly = TRUE)
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    k <- tolower(trimws(kv[1]))   # force lowercase keys
    v <- trimws(paste(kv[-1], collapse="="))
    out[[k]] <- v
  }
  out
}
as_bool <- function(x, default = TRUE) {
  if (is.null(x)) return(default)
  y <- tolower(as.character(x))
  y %in% c("1","true","t","yes","y","on")
}

cli <- parse_kv(args)
dialogs_dir    <- cli$dialogs_dir     %||% NA_character_
dialogs_glob   <- cli$dialogs_glob    %||% "dialogs_flat_*.csv"
out_csv        <- cli$out_csv         %||% "conversations.csv"
n_per_cluster  <- as.integer(cli$n_per_cluster %||% "10")
max_turns      <- as.integer(cli$max_turns     %||% "14")
include_agent  <- as_bool(cli$include_agent    %||% "1")  # NEW: include agent text in JSON

# ---------- load ----------
files <- tryCatch(fs::dir_ls(path = dialogs_dir, glob = file.path(dialogs_dir, dialogs_glob)),
                  error = function(e) character(0))

if (!length(files)) {
  # fallback with base R glob
  files <- list.files(dialogs_dir, pattern = glob2rx(dialogs_glob),
                      full.names = TRUE, recursive = FALSE)
}

if (!length(files)) {
  stop("No dialogs_flat files found.\nSearched in: ", dialogs_dir,
       "\nUsing glob: ", dialogs_glob,
       "\nTry: ls -l ", file.path(dialogs_dir, dialogs_glob))
}

dialogs <- purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE))

needed <- c("cluster","conversation_id","role","text","recipe_title")
missing <- setdiff(needed, names(dialogs))
if (length(missing)) stop("Dialogs missing columns: ", paste(missing, collapse=", "))

# ---------- sample ----------
set.seed(2025)

sampled_ids <- dialogs %>%
  distinct(cluster, conversation_id) %>%
  group_by(cluster) %>%
  slice_sample(n = min(n(), n_per_cluster)) %>%
  ungroup()

subset <- dialogs %>%
  semi_join(sampled_ids, by = c("cluster","conversation_id"))

# robust order within conversation
if ("event_idx" %in% names(subset)) {
  subset <- subset %>% arrange(cluster, conversation_id, event_idx)
} else {
  subset <- subset %>% group_by(cluster, conversation_id) %>%
    mutate(.row = row_number()) %>%
    arrange(cluster, conversation_id, .row) %>%
    ungroup() %>% select(-.row)
}

# ---------- pack to JSON turns ----------
make_json_turns <- function(df, max_turns, include_agent = TRUE) {
  df %>%
    mutate(role = ifelse(role %in% c("user","agent"), role, "user")) %>%
    { if (!include_agent) filter(., role == "user") else . } %>%
    select(role, text) %>%
    head(max_turns) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
}

convs <- subset %>%
  group_by(cluster, conversation_id, recipe_title) %>%
  summarise(
    json_turns = make_json_turns(cur_data(), max_turns = max_turns, include_agent = include_agent),
    .groups = "drop"
  ) %>%
  # Blind the true label (keep a copy for scoring later)
  mutate(cluster_hidden = cluster, cluster = NULL)

readr::write_csv(convs, out_csv)
cat("Wrote:", normalizePath(out_csv), "\n")
