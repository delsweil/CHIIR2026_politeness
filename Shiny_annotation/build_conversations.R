#!/usr/bin/env Rscript

# Sample conversations for linguist validation (cluster-blind JSON turns)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(jsonlite); library(fs)
})

`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x

# ---------- CLI ----------
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    k  <- tolower(trimws(kv[1]))
    v  <- trimws(paste(kv[-1], collapse="="))
    out[[k]] <- v
  }
  out
}
cli <- parse_kv(commandArgs(trailingOnly = TRUE))

dialogs_dir    <- cli$dialogs_dir    %||% NA_character_
dialogs_glob   <- cli$dialogs_glob   %||% "dialogs_flat_*.csv"
out_csv        <- cli$out_csv        %||% "conversations.csv"
n_per_cluster  <- as.integer(cli$n_per_cluster %||% "10")
max_turns      <- as.integer(cli$max_turns     %||% "14")
include_agent  <- as.logical(as.integer(cli$include_agent %||% "1"))

if (!dir_exists(dialogs_dir)) {
  stop("dialogs_dir not found: ", dialogs_dir)
}

# ---------- LOAD ----------
# Get file list (fs glob first, fallback to base)
files <- tryCatch(
  fs::dir_ls(path = dialogs_dir, glob = file.path(dialogs_dir, dialogs_glob)),
  error = function(e) character(0)
)
if (!length(files)) {
  files <- list.files(dialogs_dir, pattern = glob2rx(dialogs_glob),
                      full.names = TRUE, recursive = FALSE)
}
if (!length(files)) {
  stop("No dialog files found. Looked for ", dialogs_glob, " in ", dialogs_dir)
}

message("Reading ", length(files), " dialog file(s)…")

# Ensure event_idx loads as integer (prevents lexicographic sort)
dialogs <- purrr::map_dfr(files, ~ readr::read_csv(
  .x, show_col_types = FALSE,
  col_types = readr::cols(
    cluster         = readr::col_character(),
    conversation_id = readr::col_character(),
    recipe_title    = readr::col_character(),
    role            = readr::col_character(),
    text            = readr::col_character(),
    event_idx       = readr::col_integer()
  )
))

# If any event_idx failed to parse, coerce safely
if (any(is.na(dialogs$event_idx))) {
  dialogs <- dialogs %>% mutate(event_idx = suppressWarnings(as.integer(event_idx)))
}

req <- c("cluster","conversation_id","recipe_title","role","text","event_idx")
missing <- setdiff(req, names(dialogs))
if (length(missing)) stop("dialogs missing columns: ", paste(missing, collapse=", "))

# ---------- SAMPLE N PER CLUSTER (robust) ----------
set.seed(2025)

conv_index <- dialogs %>%
  distinct(cluster, conversation_id, recipe_title)

sampled_ids <- conv_index %>%
  group_by(cluster) %>%
  group_modify(function(.x, .g) {
    m <- min(nrow(.x), n_per_cluster)
    if (m <= 0) return(.x[0,])
    .x %>% slice_sample(n = m)
  }) %>%
  ungroup() %>%
  select(cluster, conversation_id)

# ---------- BUILD JSON TURNS ----------
# Sort by numeric event_idx to keep true turn order
subset <- dialogs %>%
  semi_join(sampled_ids, by = c("cluster","conversation_id")) %>%
  arrange(cluster, conversation_id, event_idx, .by_group = FALSE)

make_json_turns <- function(df) {
  # Ensure within-conversation sort, in case upstream order was disturbed
  df <- df %>% arrange(event_idx)
  
  df2 <- df %>%
    mutate(role = ifelse(role %in% c("user","agent"), role, "user")) %>%
    { if (!include_agent) filter(., role != "agent") else . } %>%   # drop agents if requested
    select(role, text) %>%
    head(max_turns)
  
  # Be explicit that JSON array is ordered by rows
  jsonlite::toJSON(df2, dataframe = "rows", auto_unbox = TRUE)
}

convs <- subset %>%
  group_by(cluster, conversation_id, recipe_title) %>%
  summarise(json_turns = make_json_turns(cur_data()), .groups = "drop") %>%
  mutate(cluster_hidden = cluster, cluster = NULL) %>%
  arrange(conversation_id)

readr::write_csv(convs, out_csv)
cat("Wrote:", normalizePath(out_csv), "\n")
