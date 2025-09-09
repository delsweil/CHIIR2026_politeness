#!/usr/bin/env Rscript
# ============================================================
# Information Nugget Counter (combined, stand-alone)
# - Reads multiple dialogs_flat CSVs (or a directory/glob)
# - Merges them + joins step_lookup
# - Collapses agent text per (cluster, conversation, recipe, step)
# - Uses an LLM (Ollama) to extract "information nuggets"
# - Writes a single combined CSV for downstream analysis
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(httr);  library(jsonlite); library(progress)
})

# ---------- CLI helpers ----------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    out[[tolower(trimws(kv[1]))]] <- trimws(paste(kv[-1], collapse="="))
  }
  out
}

# ---------- Defaults (override via CLI) ----------
args <- parse_kv(commandArgs(trailingOnly = TRUE))
# Option A: list of files (comma-separated)
DIALOGS_FILES   <- args$files   %||% ""      # e.g., "runA/dialogs_flat_*.csv,runB/dialogs_flat_*.csv"
# Option B: a directory + pattern (glob)
DIALOGS_DIR     <- args$dir     %||% ""      # e.g., "/home/david/sim_runs/test_20250902_152940"
DIALOGS_PATTERN <- args$pattern %||% "dialogs_flat_*.csv"

PATH_STEP_LOOKUP  <- args$step_lookup  %||% "step_lookup.csv"
PATH_NUGGETS_OUT  <- args$out          %||% "agent_nuggets_by_step.csv"
PATH_AGENT_BLOCKS <- args$cache_blocks %||% ""    # optional cache; if given and exists, we’ll load it

# LLM/Ollama settings
OLLAMA_URL        <- args$ollama_url   %||% "http://intern.schlaubox.de:11434/api/generate"
ANNOTATOR_MODEL   <- args$model        %||% "deepseek-r1:8b"
TEMPERATURE       <- as.numeric(args$temperature %||% "0.0")
NUM_CTX           <- as.integer(args$num_ctx     %||% "8192")
NUM_PREDICT       <- as.integer(args$num_predict %||% "512")
ON_FAIL           <- tolower(args$on_fail %||% "zero")  # "zero" or "apologize"
MAX_ROWS          <- as.integer(args$max_rows %||% "2147483647") # ~Inf

# ---------- I/O helpers ----------
as_chr1  <- function(x, default = "") if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])

# Collect dialogs files
collect_dialog_files <- function() {
  files <- character(0)
  if (nzchar(DIALOGS_FILES)) {
    parts <- unlist(strsplit(DIALOGS_FILES, ","))
    for (p in trimws(parts)) {
      # expand globs
      hits <- Sys.glob(p)
      files <- c(files, hits)
    }
  } else if (nzchar(DIALOGS_DIR)) {
    files <- Sys.glob(file.path(DIALOGS_DIR, DIALOGS_PATTERN))
  }
  files <- unique(files[file.exists(files)])
  if (!length(files)) stop("No dialogs files found. Provide files= or dir= + pattern=.")
  files
}

# ---------- JSON wranglers ----------
clean_model_output <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  x <- gsub("```[jJ][sS][oO][nN]\\s*|```", "", x)
  x <- gsub("<think>[\\s\\S]*?</think>", "", x, perl = TRUE)
  trimws(x)
}
parse_first_json <- function(x) {
  x <- clean_model_output(x)
  i <- regexpr("\\{", x)[1]; if (i == -1) return(NULL)
  open <- 0; end <- nchar(x)
  for (k in seq(i, nchar(x))) {
    ch <- substr(x, k, k)
    if (ch == "{") open <- open + 1
    if (ch == "}") { open <- open - 1; if (open == 0) { end <- k; break } }
  }
  tryCatch(jsonlite::fromJSON(substr(x, i, end)), error = function(e) NULL)
}
query_ollama_json <- function(prompt_text, model = ANNOTATOR_MODEL, temperature = TEMPERATURE) {
  body <- list(
    model  = model,
    prompt = prompt_text,
    stream = FALSE,
    format = "json",
    options = list(
      temperature = temperature,
      num_ctx     = NUM_CTX,
      num_predict = NUM_PREDICT
    )
  )
  res <- httr::POST(OLLAMA_URL, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json")
  if (res$status_code != 200) stop("Ollama HTTP error: ", res$status_code)
  httr::content(res)$response
}

# ---------- Nugget filtering ----------
is_bad_nugget <- function(s) {
  s <- tolower(trimws(s))
  if (!nzchar(s)) return(TRUE)
  meta_patterns <- c(
    "\\buser\\b", "\\bassistant\\b", "\\bagent\\b",
    "\\bconfus(ed|ion)\\b", "\\bunsure\\b",
    "\\bapolog(y|ise|ize|izing|ising)\\b", "\\bsorry\\b",
    "\\brequest(ed|ing)?\\b", "\\bask(ed|ing)?\\b",
    "\\bsummar(y|ise|ize)\\b", "\\bsummary\\b",
    "\\bthis step\\b", "\\bthe step\\b", "\\bthe recipe\\b"
  )
  if (any(grepl(paste(meta_patterns, collapse="|"), s))) return(TRUE)
  if (nchar(s) < 8) return(TRUE)
  if (grepl("^(mix|slice|do|go|next|continue)$", s)) return(TRUE)
  FALSE
}
filter_nuggets <- function(nuggets) {
  nuggets <- unique(trimws(nuggets))
  nuggets <- nuggets[nzchar(nuggets)]
  nuggets <- nuggets[!vapply(nuggets, is_bad_nugget, logical(1))]
  nuggets
}

# ---------- Prompt ----------
STRICT_HEADER <- "You MUST return exactly one JSON object on a single line.
No code fences. No explanations. No <think>…</think>. Output the JSON object only."
STRICT_FOOTER <- "Output exactly one line with the JSON object and nothing else."

build_nugget_prompt <- function(step_description, agent_text) {
  good_bad <- paste(
    "GOOD NUGGETS (examples):",
    "- \"Bake until the center reaches 195°F.\"",
    "- \"Score only the skin and fat; avoid cutting into the meat.\"",
    "- \"Cold-start rendering improves fat release.\"",
    "",
    "BAD NUGGETS (reject these):",
    "- \"The user is confused about…\"",
    "- \"The assistant apologizes…\"",
    "- \"Mix spices and slice apples\" (vague restatement)",
    "- \"History exists\" (vague/non-specific)",
    sep = "\n"
  )
  paste0(
    STRICT_HEADER, "\n\n",
    "You are an expert annotator of INFORMATION NUGGETS.\n",
    "Definition: An information nugget is a minimal, self-contained, factual unit\n",
    "drawn ONLY from the AGENT TEXT for THIS STEP. No meta, affect, or apologies.\n\n",
    good_bad, "\n\n",
    "Return JSON with schema:\n",
    "{ \"ok\": true, \"nugget_count\": <int>, \"nuggets\": [\"<n1>\", \"<n2>\", ...], \"notes\": \"<optional>\" }\n",
    "- nugget_count MUST equal length(nuggets)\n",
    "- Nuggets MUST be unique within this response\n\n",
    "STEP DESCRIPTION:\n", step_description, "\n\n",
    "AGENT TEXT (extract facts from this):\n", agent_text, "\n\n",
    STRICT_FOOTER
  )
}

# ---------- Build or load agent blocks ----------
load_or_build_agent_blocks <- function() {
  if (nzchar(PATH_AGENT_BLOCKS) && file.exists(PATH_AGENT_BLOCKS)) {
    message("Loading cached agent blocks: ", PATH_AGENT_BLOCKS)
    return(if (grepl("\\.rds$", PATH_AGENT_BLOCKS, TRUE)) readRDS(PATH_AGENT_BLOCKS) else read_csv(PATH_AGENT_BLOCKS, show_col_types = FALSE))
  }
  files <- collect_dialog_files()
  message("Reading ", length(files), " dialogs files…")
  dialogs <- purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE), .id = "src")
  
  # sanity columns
  must <- c("cluster","conversation_id","recipe_title","step_id","role","text","model_agent")
  miss <- setdiff(must, names(dialogs))
  if (length(miss)) stop("dialogs missing cols: ", paste(miss, collapse=", "))
  
  # de-dup (if the same row appears in multiple files)
  dialogs <- dialogs %>% distinct(cluster, conversation_id, recipe_title, step_id, role, text, model_agent, .keep_all = TRUE)
  
  step_lookup <- if (grepl("\\.rds$", PATH_STEP_LOOKUP, TRUE)) readRDS(PATH_STEP_LOOKUP) else readr::read_csv(PATH_STEP_LOOKUP, show_col_types = FALSE)
  if (!all(c("recipe_title","step_id","step_description") %in% names(step_lookup))) {
    stop("step_lookup missing required columns (recipe_title, step_id, step_description).")
  }
  
  agent_blocks <- dialogs %>%
    filter(role == "agent") %>%
    group_by(cluster, conversation_id, recipe_title, step_id) %>%
    summarise(
      agent_text  = paste(text, collapse = " "),
      agent_turns = n(),
      model_agent = first(model_agent),
      .groups = "drop"
    ) %>%
    left_join(step_lookup, by = c("recipe_title","step_id"))
  
  # optional cache write (only if user provided a path)
  if (nzchar(PATH_AGENT_BLOCKS)) {
    if (grepl("\\.rds$", PATH_AGENT_BLOCKS, TRUE)) saveRDS(agent_blocks, PATH_AGENT_BLOCKS) else readr::write_csv(agent_blocks, PATH_AGENT_BLOCKS)
    message("Cached agent blocks -> ", PATH_AGENT_BLOCKS)
  }
  agent_blocks
}

# ---------- Row annotator ----------
extract_nuggets_row <- function(row) {
  step_desc  <- as_chr1(row$step_description, "")
  agent_text <- as_chr1(row$agent_text, "")
  if (!nzchar(trimws(agent_text))) {
    return(tibble(nugget_count = 0, nuggets = "", ok = TRUE, annot_notes = "empty_agent_text"))
  }
  
  run_once <- function(extra_hint = NULL) {
    prompt <- build_nugget_prompt(step_desc, if (is.null(extra_hint)) agent_text else paste(agent_text, "\n\nSTRICT REMINDER:", extra_hint))
    raw <- tryCatch(query_ollama_json(prompt), error = function(e) NA_character_)
    if (is.na(raw)) return(list(ok=FALSE, nuggets=character(0), notes="ollama_error"))
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) return(list(ok=FALSE, nuggets=character(0), notes="invalid_json"))
    nlist <- parsed$nuggets %||% character(0)
    nlist <- unique(nlist[nzchar(trimws(nlist))])
    list(ok = isTRUE(parsed$ok), nuggets = nlist, notes = as_chr1(parsed$notes, ""))
  }
  
  r1 <- run_once()
  n1 <- filter_nuggets(r1$nuggets)
  need_retry <- length(n1) == 0 || (length(n1) < length(r1$nuggets) / 2)
  if (need_retry) {
    r2 <- run_once("Extract ONLY factual, content-bearing statements present in AGENT TEXT. Reject meta/affect/apologies/summaries. Return 1–8 nuggets.")
    n2 <- filter_nuggets(r2$nuggets)
    if (length(n2) >= length(n1)) { n_final <- n2; ok_flag <- r2$ok; notes <- if (nzchar(r2$notes)) r2$notes else "retry_used" }
    else                          { n_final <- n1; ok_flag <- r1$ok; notes <- if (nzchar(r1$notes)) r1$notes else "retry_kept_first" }
  } else { n_final <- n1; ok_flag <- r1$ok; notes <- r1$notes }
  
  if (length(n_final) == 0) {
    if (identical(ON_FAIL, "zero")) return(tibble(nugget_count=0, nuggets="", ok=ok_flag, annot_notes=paste0("filtered_all;", notes)))
    return(tibble(nugget_count=NA_integer_, nuggets="", ok=FALSE, annot_notes=paste0("filtered_all;", notes)))
  }
  
  n_final <- unique(n_final)
  tibble(nugget_count = length(n_final),
         nuggets      = paste(n_final, collapse = " | "),
         ok           = ok_flag,
         annot_notes  = notes)
}

# ---------- Main ----------
message("Building/Loading agent blocks …")
agent_blocks <- load_or_build_agent_blocks()

if (!is.finite(MAX_ROWS)) MAX_ROWS <- nrow(agent_blocks)
agent_blocks <- agent_blocks %>% slice(1:min(n(), MAX_ROWS))

message("Counting nuggets for ", nrow(agent_blocks), " blocks …")
pb <- progress_bar$new(total = nrow(agent_blocks), format = "[:bar] :current/:total :percent eta=:eta")

results <- agent_blocks %>%
  mutate(.row_id = row_number()) %>%
  group_split(.row_id, .keep = TRUE) %>%
  map_dfr(function(df1) {
    pb$tick()
    r <- df1[1, ]
    out <- extract_nuggets_row(r)
    bind_cols(select(r, -.row_id), out)
  })

results <- results %>%
  mutate(
    annotator_model = ANNOTATOR_MODEL,
    agent_text_chars = nchar(agent_text),
    agent_text_words = str_count(agent_text, boundary("word"))
  ) %>%
  select(
    cluster, conversation_id, recipe_title, step_id,
    step_description, agent_turns, model_agent,
    agent_text_chars, agent_text_words,
    nugget_count, nuggets, ok, annot_notes, annotator_model
  ) %>%
  arrange(cluster, conversation_id, recipe_title, step_id)

readr::write_csv(results, PATH_NUGGETS_OUT)
message("Wrote: ", PATH_NUGGETS_OUT)

# Small console summary
results %>%
  group_by(cluster, recipe_title) %>%
  summarise(
    steps = n(),
    total_nuggets = sum(nugget_count, na.rm = TRUE),
    mean_nuggets  = mean(nugget_count, na.rm = TRUE),
    .groups = "drop"
  ) %>% print(n = Inf)

