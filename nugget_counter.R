#!/usr/bin/env Rscript

# ============================================================
# Information Nugget Counter (stand-alone)
# - Reads dialogs + step_lookup (CSV or RDS)
# - Collapses agent text per (conversation, recipe, step)
# - Uses an LLM (Ollama) to extract "information nuggets"
# - Writes a tidy CSV with counts
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(httr);  library(jsonlite)
})

# -------------------------------
# Config
# -------------------------------
# Inputs (produced by your simulation script)
PATH_DIALOGS      <- "batch_dialogs_flat.csv"   # or .rds
PATH_STEP_LOOKUP  <- "step_lookup.csv"          # or .rds
# Optional prebuilt agent blocks (if present, it will be used)
PATH_AGENT_BLOCKS <- "agent_blocks_for_nuggets.csv"  # or .rds

# Output
PATH_NUGGETS_OUT  <- "agent_nuggets_by_step.csv"

# LLM/Ollama settings
OLLAMA_URL        <- "http://intern.schlaubox.de:11434/api/generate"
ANNOTATOR_MODEL   <- "deepseek-r1:8b"      # light, fast annotator (adjust to taste)
TEMPERATURE       <- 0.0              # deterministic extraction
NUM_CTX           <- 8192
NUM_PREDICT       <- 512

# If the model fails to produce valid JSON, fallback behavior:
ON_FAIL <- c("apologize", "zero")[[2]]  # "apologize" or "zero"

# Limit rows for a dry run (set to Inf to process all)
MAX_ROWS <- Inf

# -------------------------------
# Utils
# -------------------------------
`%||%`   <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
as_chr1  <- function(x, default = "") if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])
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
  out <- tryCatch(jsonlite::fromJSON(substr(x, i, end)), error = function(e) NULL)
  out
}

# POST to Ollama with strict JSON request
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

is_bad_nugget <- function(s) {
  s <- tolower(trimws(s))
  if (!nzchar(s)) return(TRUE)
  # meta / user- or assistant-state
  meta_patterns <- c(
    "\\buser\\b", "\\bassistant\\b", "\\bagent\\b",
    "\\bconfus(ed|ion)\\b", "\\bunsure\\b", "\\bunsure\\b",
    "\\bapolog(y|ise|ize|izing|ising)\\b", "\\bsorry\\b",
    "\\brequest(ed|ing)?\\b", "\\bask(ed|ing)?\\b",
    "\\bsummar(y|ise|ize)\\b", "\\bsummary\\b",
    "\\bthis step\\b", "\\bthe step\\b", "\\bthe recipe\\b"
  )
  if (any(grepl(paste(meta_patterns, collapse="|"), s))) return(TRUE)
  
  # too-vague / template-y
  if (nchar(s) < 8) return(TRUE)
  if (grepl("^mix|^slice|^do|^go|^next|^continue$", s)) return(TRUE)
  
  FALSE
}

filter_nuggets <- function(nuggets) {
  nuggets <- unique(trimws(nuggets))
  nuggets <- nuggets[ nzchar(nuggets) ]
  nuggets <- nuggets[ !vapply(nuggets, is_bad_nugget, logical(1)) ]
  nuggets
}


# -------------------------------
# Prompt builder
# -------------------------------
STRICT_HEADER <- "You MUST return exactly one JSON object on a single line.
No code fences. No explanations. No <think>…</think>. Output the JSON object only."
STRICT_FOOTER <- "Output exactly one line with the JSON object and nothing else."

nugget_definition <- paste(
  "You are an expert annotator of 'information nuggets'.",
  "An information nugget is an atomic piece of useful or interesting information",
  "that is novel within the SAME step (no duplicates), specific (not vague), and content-bearing.",
  "Do NOT include meta-chatter, hedges, apologies, or restatements unless they add new factual content.",
  "Split multi-part facts into separate nuggets only if they stand alone.",
  sep=" "
)

build_nugget_prompt <- function(step_description, agent_text) {
  good_bad <- paste(
    "GOOD NUGGETS (examples):",
    "- \"Bake until the center reaches 195°F.\"",
    "- \"Score only the skin and fat; avoid cutting into the meat.\"",
    "- \"Cold-start rendering improves fat release.\"",
    "These are atomic, actionable, and specific facts found in the AGENT TEXT.",
    "",
    "BAD NUGGETS (reject these):",
    "- \"The user is confused about…\" (meta/affect/summary of user)",
    "- \"The assistant apologizes…\" (apology/meta, not content)",
    "- \"Mix spices and slice apples\" (vague restatement with no added detail)",
    "- \"History exists\" (vague/non-specific)",
    "- \"This is irrelevant\" (opinion/judgment)",
    sep = "\n"
  )
  
  paste0(
    STRICT_HEADER, "\n\n",
    "You are an expert annotator of INFORMATION NUGGETS.\n",
    "Definition: An information nugget is a minimal, self-contained, content-bearing fact\n",
    "that can be directly recovered from the AGENT TEXT for THIS STEP.\n",
    "Do NOT produce meta-commentary, user state, apologies, or summaries.\n",
    "Do NOT describe what the user or assistant is doing or feeling.\n",
    "Do NOT paraphrase the whole step; extract only discrete facts.\n\n",
    good_bad, "\n\n",
    "Return JSON with schema:\n",
    "{ \"ok\": true, \"nugget_count\": <integer>, \"nuggets\": [\"<n1>\", \"<n2>\", ...], \"notes\": \"<optional>\" }\n",
    "- nugget_count MUST equal length(nuggets)\n",
    "- Nuggets MUST be unique (case-insensitive) within this response\n",
    "- Each nugget should be one sentence if possible, specific, and factual\n\n",
    "STEP DESCRIPTION:\n", step_description, "\n\n",
    "AGENT TEXT (only extract facts from this):\n", agent_text, "\n\n",
    STRICT_FOOTER
  )
}


# -------------------------------
# Load / build agent blocks
# -------------------------------
load_or_build_agent_blocks <- function() {
  # If prebuilt agent_blocks file exists, prefer that
  if (file.exists(PATH_AGENT_BLOCKS)) {
    message("Loading prebuilt agent blocks from: ", PATH_AGENT_BLOCKS)
    if (grepl("\\.rds$", PATH_AGENT_BLOCKS, ignore.case = TRUE)) {
      ab <- readRDS(PATH_AGENT_BLOCKS)
    } else {
      ab <- readr::read_csv(PATH_AGENT_BLOCKS, show_col_types = FALSE)
    }
    return(ab)
  }
  
  # Else, build from dialogs + step_lookup
  message("Building agent blocks from dialogs + step_lookup …")
  dialogs <- if (grepl("\\.rds$", PATH_DIALOGS, ignore.case = TRUE)) {
    readRDS(PATH_DIALOGS)
  } else {
    readr::read_csv(PATH_DIALOGS, show_col_types = FALSE)
  }
  step_lookup <- if (grepl("\\.rds$", PATH_STEP_LOOKUP, ignore.case = TRUE)) {
    readRDS(PATH_STEP_LOOKUP)
  } else {
    readr::read_csv(PATH_STEP_LOOKUP, show_col_types = FALSE)
  }
  
  req_cols <- c("cluster","conversation_id","recipe_title","step_id","role","text","model_agent")
  if (!all(req_cols %in% names(dialogs))) {
    stop("dialogs is missing columns: ", paste(setdiff(req_cols, names(dialogs)), collapse=", "))
  }
  if (!all(c("recipe_title","step_id","step_description") %in% names(step_lookup))) {
    stop("step_lookup is missing required columns.")
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
  
  # Save a copy for reuse next time
  readr::write_csv(agent_blocks, PATH_AGENT_BLOCKS)
  agent_blocks
}

# -------------------------------
# Nugget extraction per row
# -------------------------------
extract_nuggets_row <- function(row) {
  step_desc  <- as_chr1(row$step_description, "")
  agent_text <- as_chr1(row$agent_text, "")
  
  if (!nzchar(trimws(agent_text))) {
    return(tibble(nugget_count = 0, nuggets = "", ok = TRUE, annot_notes = "empty_agent_text"))
  }
  
  # helper to query + normalize once
  run_once <- function(extra_hint = NULL) {
    prompt <- build_nugget_prompt(
      step_description = step_desc,
      agent_text = if (is.null(extra_hint)) agent_text else paste(agent_text, "\n\nSTRICT REMINDER:", extra_hint)
    )
    raw <- tryCatch(query_ollama_json(prompt), error = function(e) NA_character_)
    if (is.na(raw)) return(list(ok=FALSE, nuggets=character(0), notes="ollama_error"))
    
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) return(list(ok=FALSE, nuggets=character(0), notes="invalid_json"))
    
    nlist <- parsed$nuggets %||% character(0)
    nlist <- nlist[nzchar(trimws(nlist))]
    # de-dup (case-insensitive)
    nlist <- unique(nlist[tolower(nlist) != ""])
    list(ok = isTRUE(parsed$ok), nuggets = nlist, notes = as_chr1(parsed$notes, ""))
  }
  
  # first pass
  r1 <- run_once()
  n1 <- filter_nuggets(r1$nuggets)
  
  # retry if all filtered or >50% filtered
  need_retry <- length(n1) == 0 || (length(n1) < length(r1$nuggets) / 2)
  if (need_retry) {
    r2 <- run_once(
      extra_hint = paste(
        "Extract ONLY factual, content-bearing statements present in AGENT TEXT.",
        "Reject meta (e.g., 'the user is confused'), affect, apologies, or summaries.",
        "Return 1–8 nuggets. Each must stand on its own as a fact.",
        sep=" "
      )
    )
    n2 <- filter_nuggets(r2$nuggets)
    # choose the better of the two
    if (length(n2) >= length(n1)) {
      n_final <- n2; ok_flag <- r2$ok; notes <- if (nzchar(r2$notes)) r2$notes else "retry_used"
    } else {
      n_final <- n1; ok_flag <- r1$ok; notes <- if (nzchar(r1$notes)) r1$notes else "retry_kept_first"
    }
  } else {
    n_final <- n1; ok_flag <- r1$ok; notes <- r1$notes
  }
  
  # if still nothing, apply fallback policy
  if (length(n_final) == 0) {
    if (identical(ON_FAIL, "zero")) {
      return(tibble(nugget_count = 0, nuggets = "", ok = ok_flag, annot_notes = paste0("filtered_all;", notes)))
    } else {
      return(tibble(nugget_count = NA_integer_, nuggets = "", ok = FALSE, annot_notes = paste0("filtered_all;", notes)))
    }
  }
  
  n_final <- unique(n_final)
  tibble(
    nugget_count = length(n_final),
    nuggets      = paste(n_final, collapse = " | "),
    ok           = ok_flag,
    annot_notes  = notes
  )
}


# -------------------------------
# Main
# -------------------------------
agent_blocks <- load_or_build_agent_blocks()

if (!is.finite(MAX_ROWS)) MAX_ROWS <- nrow(agent_blocks)
agent_blocks <- agent_blocks %>% slice(1:min(n(), MAX_ROWS))

message("Counting nuggets for ", nrow(agent_blocks), " (conversation × recipe × step) blocks…")

results <- agent_blocks %>%
  mutate(row_id = row_number()) %>%
  group_split(row_id, .keep = TRUE) %>%
  purrr::map_dfr(function(df1) {
    r <- df1[1, ]
    out <- extract_nuggets_row(r)
    bind_cols(select(r, -row_id), out)
  })

# Add some basic metadata
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
    nugget_count, nuggets, ok, annot_notes,
    annotator_model
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
