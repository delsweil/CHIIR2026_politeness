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


# ---------- BLIND + SHAPE ----------
set.seed(2025)

# ensure this exists (0/1 or true/false from CLI)
INCLUDE_AGENT <- as.logical(as.integer(cli$include_agent %||% "1"))

# distinct conversations
conv_index <- dialogs %>%
  dplyr::distinct(cluster, conversation_id)

# compute sample size per cluster as a constant, then sample
sampled_ids <- conv_index %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(n_in_cluster = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cluster) %>%
  dplyr::slice_sample(n = min(first(n_in_cluster), n_per_cluster)) %>%
  dplyr::ungroup() %>%
  dplyr::select(cluster, conversation_id)

subset <- dialogs %>%
  dplyr::semi_join(sampled_ids, by = c("cluster","conversation_id")) %>%
  dplyr::arrange(cluster, conversation_id, event_idx)

make_json_turns <- function(df) {
  df %>%
    dplyr::mutate(role = ifelse(role %in% c("user","agent"), role, "user")) %>%
    dplyr::select(role, text) %>%
    dplyr::mutate(text = ifelse(!INCLUDE_AGENT & role == "agent", "", text)) %>%
    head(max_turns) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
}

convs <- subset %>%
  dplyr::group_by(cluster, conversation_id, recipe_title) %>%
  dplyr::summarise(json_turns = make_json_turns(dplyr::cur_data()), .groups = "drop") %>%
  dplyr::mutate(cluster_hidden = cluster, cluster = NULL)

readr::write_csv(convs, out_csv)
cat("Wrote:", normalizePath(out_csv), "\n")

