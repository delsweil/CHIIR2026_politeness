#!/usr/bin/env Rscript
# ============================================================
# Information Nugget Counter (stand-alone)
# - Reads dialogs_flat CSVs (files= or dir= + pattern=)
# - Joins step_lookup
# - Collapses agent text per (cluster, conversation, recipe, step)
# - Uses Ollama to extract "information nuggets"
# - Writes combined CSV for analysis
# - Shows a nohup-friendly progress bar + newline ticks
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
as_chr1  <- function(x, default = "") if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])

# ---------- Defaults (override via CLI) ----------
args <- parse_kv(commandArgs(trailingOnly = TRUE))
# Option A: explicit file globs (comma-separated)
DIALOGS_FILES   <- args$files   %||% ""      # e.g., "runA/dialogs_flat_*.csv,runB/dialogs_flat_*.csv"
# Option B: directory + glob pattern
DIALOGS_DIR     <- args$dir     %||% ""      # e.g., "/home/david/sim_runs/test_20250908_130819"
DIALOGS_PATTERN <- args$pattern %||% "dialogs_flat_*.csv"

PATH_STEP_LOOKUP  <- args$step_lookup  %||% "step_lookup.csv"
PATH_NUGGETS_OUT  <- args$out          %||% "agent_nuggets_by_step.csv"
PATH_AGENT_BLOCKS <- args$cache_blocks %||% ""    # optional cache; if present & exists, load

# LLM/Ollama settings
OLLAMA_URL        <- args$ollama_url   %||% "http://localhost:11434/api/generate"
ANNOTATOR_MODEL   <- args$model        %||% "deepseek-r1:8b"
TEMPERATURE       <- as.numeric(args$temperature %||% "0.0")
NUM_CTX           <- as.integer(args$num_ctx     %||% "8192")
NUM_PREDICT       <- as.integer(args$num_predict %||% "512")
ON_FAIL           <- tolower(args$on_fail %||% "zero")  # "zero" or "apologize"
MAX_ROWS          <- as.integer(args$max_rows %||% "2147483647") # ~Inf

# ---------- Collect dialogs files ----------
collect_dialog_files <- function() {
  files <- character(0)
  if (nzchar(DIALOGS_FILES)) {
    parts <- unlist(strsplit(DIALOGS_FILES, ","))
    for (p in trimws(parts)) files <- c(files, Sys.glob(p))
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
  res <- httr::POST(OLLAMA_URL, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json", httr::timeout(120))
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

# -------- Few-shot examples (same set used in evaluation) --------
shots <- list(
  # 0-nugget (apology/meta)
  list(
    step_description = "General help",
    agent_text = "Sorry, I can’t provide details for that request. As an assistant, I have limitations.",
    answer_json = '{"ok":true,"nugget_count":0,"nuggets":[],"notes":""}'
  ),
  # 1 nugget (simple, discourage splitting)
  list(
    step_description = "Bread baking",
    agent_text = "Bake the loaf at 220°C until the crust turns golden brown.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Bake at 220°C until crust is golden"],"notes":""}'
  ),
  # 2 nuggets
  list(
    step_description = "Pasta boiling",
    agent_text = "Add salt to the water to season the pasta. Stir occasionally to prevent noodles from sticking together.",
    answer_json = '{"ok":true,"nugget_count":2,"nuggets":["Salt in water seasons pasta","Stirring prevents noodles from sticking"],"notes":""}'
  ),
  # 1 nugget (Soufflé)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = "Folding beaten egg whites into the base serves a crucial role in creating a light and airy texture.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Folding egg whites adds air for lift"],"notes":""}'
  ),
  # 2 nuggets (Soufflé, with meta to ignore)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = paste(
      "I can’t show pictures.",
      "Fold egg whites gently with a spatula in a zigzag motion until just incorporated; this keeps the mixture light."
    ),
    answer_json = '{"ok":true,"nugget_count":2,"nuggets":["Fold whites gently with a spatula","Keeps mixture light and airy"],"notes":""}'
  ),
  # 3 nuggets (Duck)
  list(
    step_description = "Duck à l’Orange",
    agent_text = paste(
      "Pricking the duck skin helps render fat.",
      "Brief boiling mobilizes fat outward.",
      "Dry thoroughly to keep skin crisp."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Prick skin to render fat","Brief boil mobilizes fat outward","Dry thoroughly to keep skin crisp"],"notes":""}'
  ),
  # 3 nuggets (Soufflé oven science)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = paste(
      "Preheating ensures proper rise.",
      "Correct temperature gives even bake.",
      "Beaten egg bubbles expand with heat."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Preheat oven for proper rise","Correct temperature ensures even bake","Beaten egg bubbles expand with heat"],"notes":""}'
  ),
  # 3 nuggets (Double fry, de-duplicated)
  list(
    step_description = "Buttermilk-Brined Southern Fried Chicken",
    agent_text = paste(
      "Double-frying is a Southern method.",
      "Rest between fries reduces moisture and sets crust.",
      "Chilling period improves crispiness."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Double-fry method from Southern cooking","Rest between fries reduces moisture and sets crust","Chilling period improves crispiness"],"notes":""}'
  ),
  # 4 nuggets (Spatchcock)
  list(
    step_description = "Duck à l’Orange",
    agent_text = paste(
      "Spatchcock to butterfly and flatten duck.",
      "Trim excess fat and skin.",
      "Remove backbone; detach wings/legs to flatten.",
      "Reserve trimmings to make stock."
    ),
    answer_json = '{"ok":true,"nugget_count":4,"nuggets":["Spatchcock to butterfly and flatten duck","Trim excess fat and skin","Remove backbone; detach wings/legs to flatten","Reserve trimmings to make stock"],"notes":""}'
  ),
  # 5 nuggets (Pesto)
  list(
    step_description = "Pesto alla Genovese",
    agent_text = paste(
      "Pound basil with salt for pesto base.",
      "Ligurian origin; ancient roots.",
      "Grinding releases oils for green paste.",
      "Salt helps emulsify oil.",
      "Salt preserves by drawing moisture and inhibiting bacteria."
    ),
    answer_json = '{"ok":true,"nugget_count":5,"nuggets":["Pound basil with salt for pesto base","Originates from Liguria with ancient roots","Grinding basil releases oils for green paste","Salt helps emulsify the oil","Salt preserves by drawing moisture and inhibiting bacteria"],"notes":""}'
  ),
  # 1 nugget (combined/serial instruction = one nugget)
  list(
    step_description = "Old-Fashioned Apple Pie",
    agent_text = "Add tapioca, assemble and seal top crust, then chill before slicing.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Add tapioca, assemble and seal top crust, chill before slicing"],"notes":"combined instruction treated as one operation"}'
  )
)



# ---------- Prompt ----------
STRICT_HEADER <- "You MUST return exactly one JSON object on a single line.
No code fences. No explanations. No <think>…</think>. Output the JSON object only."
STRICT_FOOTER <- "Output exactly one line with the JSON object and nothing else."

# ---------- Prompt (policy-aligned, with shots) ----------
build_nugget_prompt <- function(step_description, agent_text, shots = NULL) {
  step_block <- if (nzchar(step_description)) paste0("STEP DESCRIPTION:\n", step_description, "\n\n") else ""
  shots_block <- ""
  if (!is.null(shots) && length(shots)) {
    fmt <- function(s) {
      sd <- s$step_description %||% ""
      sd_block <- if (nzchar(sd)) paste0("STEP DESCRIPTION (EXAMPLE):\n", sd, "\n") else ""
      paste0("EXAMPLE:\n", sd_block,
             "AGENT TEXT (EXAMPLE):\n", s$agent_text, "\n",
             "EXPECTED OUTPUT (single-line JSON):\n", s$answer_json, "\n\n")
    }
    shots_block <- paste0("FEW-SHOT EXAMPLES (match this style):\n",
                          paste(vapply(shots, fmt, character(1)), collapse = ""),
                          "END EXAMPLES.\n\n")
  }
  
  policy <- paste(
    "You are an expert annotator of INFORMATION NUGGETS.",
    "A nugget is a minimal, self-contained, factual unit drawn ONLY from the AGENT TEXT for THIS STEP.",
    "",
    "RETURN ALL DISTINCT FACTS:",
    "- Some steps have 0 nuggets; others may have 6–8 or more. List EVERY distinct fact you find.",
    "- Do NOT cap the number; include them all if present.",
    "",
    "SEPARATE vs MERGE:",
    "- Separate distinct facts even if they are short or appear in the same sentence.",
    "- MERGE paraphrases of the SAME fact into ONE nugget (avoid duplicate counting).",
    "- Treat tightly-coupled serial instructions that form ONE atomic operation (e.g., “chop the onion, add to pan, simmer 3–4 min”) as ONE nugget.",
    "",
    "EXCLUSIONS:",
    "- Drop meta/affect (apologies, capability notes, requests, uncertainty).",
    "- Drop vague statements with no concrete information.",
    "",
    "OUTPUT FORMAT (STRICT):",
    "- Output exactly ONE JSON object on ONE line (no code fences, no <think>, no commentary).",
    '- Schema: { \"ok\": true, \"nugget_count\": <int>, \"nuggets\": [\"<n1>\", ...], \"notes\": \"<optional>\" }',
    "- nugget_count MUST equal length(nuggets). nuggets MAY be empty [].",
    sep = "\n"
  )
  
  paste0(
    policy, "\n\n",
    "GOOD EXAMPLES:\n",
    "- \"Bake until the center reaches 195°F.\"\n",
    "- \"Add oil gradually to stabilize the emulsion.\"\n",
    "- \"Eggs act as a binder in this dough.\"\n\n",
    "BAD EXAMPLES:\n",
    "- Apologies/meta about capabilities.\n",
    "- Vague restatements without concrete new info.\n",
    "- Duplicate paraphrases of the same fact.\n\n",
    shots_block,
    step_block,
    "AGENT TEXT:\n", agent_text, "\n\n",
    "OUTPUT: (JSON on one line)"
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
    prompt <- build_nugget_prompt(
      step_description = step_desc,
      agent_text       = if (is.null(extra_hint)) agent_text else paste(agent_text, "\n\nSTRICT REMINDER:", extra_hint),
      shots            = shots
    )

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

total_blocks <- nrow(agent_blocks)
message("Counting nuggets for ", total_blocks, " (conversation × recipe × step) blocks…")

# Progress bar (forced for nohup) + visible newline ticks
pb <- progress::progress_bar$new(
  format     = "  counting nuggets [:bar] :current/:total (:percent) eta: :eta",
  total      = total_blocks,
  width      = 60,
  clear      = FALSE,
  show_after = 0,
  force      = TRUE
)

n_i <- 0L
results <- agent_blocks %>%
  mutate(row_id = row_number()) %>%
  group_split(row_id, .keep = TRUE) %>%
  purrr::map_dfr(function(df1) {
    n_i <<- n_i + 1L
    pb$tick()
    if (n_i %% 50 == 0L || n_i == 1L || n_i == total_blocks) {
      cat(sprintf("\n[progress] %d/%d blocks\n", n_i, total_blocks))
      flush.console()
    }
    r <- df1[1, ]
    out <- extract_nuggets_row(r)
    dplyr::bind_cols(dplyr::select(r, -row_id), out)
  })

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
