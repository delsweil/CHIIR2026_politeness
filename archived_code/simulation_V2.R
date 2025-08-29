# This archived version was the demo I gave to Christine and Anna on 28.8
# Early draft prompts for clusters 1 and 3 are implemented
# The code worked, but there was no scope for apologies

# ============================================================
# Politeness Simulation (LLM-led) — Clean Start
# Requires: httr, jsonlite, tibble, dplyr, tidyr, purrr, stringr
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(tibble); library(dplyr)
  library(tidyr); library(purrr); library(stringr)
})

set.seed(42)

# -------------------------------
# 0) Small utilities
# -------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
as_chr1 <- function(x, default = "")        if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])
as_int1 <- function(x, default = NA_integer_) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.integer(x[[1]])

STRICT_HEADER <- "You MUST return exactly one JSON object on a single line.
No code fences. No explanations. No <think>…</think>.
Output exactly one JSON object and nothing else."
STRICT_FOOTER <- "Output exactly one line containing only the JSON object."

# -------------------------------
# 1) Toy recipe + neutral needs
# -------------------------------
recipe_title <- "Crispy Duck Breast with Béchamel"

recipe_df <- tibble(
  recipe_title = recipe_title,
  step_id = 1:5,
  step_description = c(
    "Pat duck breasts dry and bring to room temperature.",
    "Score skin in a shallow cross-hatch; season with salt.",
    "Place skin-side down in a cold pan; turn heat to medium.",
    "Render fat until skin is golden; spoon off excess as needed.",
    "Rest duck; prepare a simple béchamel in a separate pan."
  ),
  need_task = list(
    "What is the correct duration for tempering before cooking?",
    "How deep should the scoring be to avoid cutting into the meat?",
    "At what point should the heat be adjusted from medium if needed?",
    "How frequently should excess fat be removed during rendering?",
    "What is the basic ratio of fat, flour, and milk for béchamel?"
  ) %>% map(~c(.x)),
  need_science = list(
    "How does starting from room temperature affect protein denaturation?",
    "How does scoring influence heat transfer and fat rendering?",
    "Why does starting in a cold pan change fat rendering dynamics?",
    "How does prolonged rendering affect skin crispness and moisture loss?",
    "What is the role of starch gelatinization in béchamel thickening?"
  ) %>% map(~c(.x)),
  need_history = list(
    "Is tempering meat a historically documented practice?",
    "Is skin scoring a traditional method in European preparations?",
    "Is the cold-pan method associated with specific culinary schools?",
    "Are there historical references to systematic fat rendering in duck cookery?",
    "When and where did béchamel sauce originate?"
  ) %>% map(~c(.x))
)

# -------------------------------
# 2) Profiles + persona text
# -------------------------------
profile_hyperpolite <- list(
  name = "C1_Hyperpolite",
  code_rates = list(
    THANK=0.80, ACK=0.70, COMPLIMENT=0.20, INDIRECT_REQ=0.60, INFO_Q=0.30,
    HINT=0.50, APOLOGY=0.10, DIRECT_REQ=0.05, PLEASE=0.05, PROCEED=0.15
  ),
  words_target = list(min=18, max=28)
)

profile_direct <- list(
  name = "C3_DirectLowPoliteness",
  code_rates = list(
    THANK=0.10, ACK=0.15, COMPLIMENT=0.02, INDIRECT_REQ=0.10, INFO_Q=0.15,
    HINT=0.05, APOLOGY=0.03, DIRECT_REQ=0.70, PLEASE=0.02, PROCEED=0.60
  ),
  words_target = list(min=6, max=12)
)

persona_text <- function(profile_name) {
  switch(profile_name,
         "C1_Hyperpolite" = paste(
           "- Face-enhancing, human-like: frequent THANK/ACK; occasional, understated COMPLIMENT.",
           "- Prefer INDIRECT requests and statement HINTS (e.g., “I’m ready for the next step.”).",
           "- Rare APOLOGY for repair; avoid PLEASE most of the time.",
           "- Combine THANK + ACK (+/- brief COMPLIMENT) when appropriate.",
           sep="\n"
         ),
         "C3_DirectLowPoliteness" = paste(
           "- Tool-like, concise, low engagement; minimal THANK/ACK/COMPLIMENT/APOLOGY.",
           "- Prefer DIRECT or non-sentential requests (e.g., “next”); rare PLEASE/HINT.",
           "- Ask relatively few information-eliciting questions; keep it brief and to the point.",
           sep="\n"
         ),
         ""
  )
}

# -------------------------------
# 3) Prompts (strict JSON)
# -------------------------------
user_system_prompt <- function(profile, recipe_title) {
  paste0(
    STRICT_HEADER, "\n\n",
    "ROLE: You simulate a human home cook interacting with an AI assistant while following a recipe step.\n\n",
    "PERSONA (", profile$name, "):\n", persona_text(profile$name), "\n",
    "Maintain approximately ", profile$words_target$min, "–", profile$words_target$max, " words per turn (±30%).\n\n",
    "Probabilistic tendencies (not per-turn rules):\n",
    toJSON(profile$code_rates, auto_unbox=TRUE), "\n\n",
    "CONTEXT:\n",
    "- You receive the recipe title, current step description, and recent context.\n",
    "- You also receive a NEUTRAL INFORMATION NEED (task/science/history) for this step; surface that need in your style.\n\n",
    "OUTPUT SCHEMA (JSON only):\n",
    "{ \"ok\": boolean, \"utterance\": string, \"codes\": string[], \"intent\": string, \"proceed\": boolean }\n\n",
    "NOTES:\n",
    "- Set proceed=true only when satisfied AND not asking another question.\n\n",
    STRICT_FOOTER
  )
}

agent_system_prompt <- function(recipe_title) {
  paste0(
    STRICT_HEADER, "\n\n",
    "ROLE: You are a cooking assistant. Answer the user's message for the current step.\n",
    "Keep it relevant (task/science/history as requested). No meta-talk.\n\n",
    "OUTPUT SCHEMA (JSON only):\n",
    "{ \"ok\": boolean, \"reply\": string }\n\n",
    STRICT_FOOTER
  )
}

# -------------------------------
# 4) Ollama call + parser
# -------------------------------
query_ollama <- function(utterance,
                         model = "llama3.1:8b",
                         base_prompt = "",
                         temperature = 0.3,
                         format_json = TRUE) {
  body <- list(
    model = model,
    prompt = paste0(base_prompt, "\n\nUtterance: ", utterance),
    stream = FALSE,
    options = list(temperature = temperature)
  )
  if (format_json) body$format <- "json"
  res <- httr::POST(
    url = "http://intern.schlaubox.de:11434/api/generate",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  if (res$status_code != 200) stop("Ollama HTTP ", res$status_code)
  httr::content(res)$response
}

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
  tryCatch(fromJSON(substr(x, i, end)), error=function(e) NULL)
}

# -------------------------------
# 5) Turn functions (LLM-led)
# -------------------------------
user_turn <- function(history_user, history_agent, profile, recipe_row, neutral_need,
                      model_user = "llama3.1:8b") {
  sys <- user_system_prompt(profile, recipe_row$recipe_title[1])
  ctx <- paste0(
    "RECIPE: ", recipe_row$recipe_title[1], "\n",
    "STEP: ", recipe_row$step_description[1], "\n",
    "NEUTRAL NEED: ", neutral_need, "\n",
    "RECENT USER TURNS: ", paste(tail(history_user, 3), collapse=" | "), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse=" | "), "\n",
    "Generate your next JSON response."
  )
  tries <- 0; parsed <- NULL; raw <- ""
  while (is.null(parsed) && tries < 3) {
    tries <- tries + 1
    raw <- query_ollama(ctx, model = model_user, base_prompt = sys, format_json = TRUE)
    parsed <- parse_first_json(raw)
  }
  if (is.null(parsed)) {
    cat("\n--- RAW USER OUTPUT ---\n", raw, "\n------------------------\n")
    stop("User LLM did not return valid JSON.")
  }
  # compatibility + guards
  parsed$utterance <- parsed$utterance %||% parsed$message %||% ""
  parsed$intent    <- parsed$intent    %||% NA_character_
  parsed$codes     <- parsed$codes     %||% character(0)
  # If asking, do NOT proceed yet
  is_ask <- (!is.na(parsed$intent) && grepl("^ASK_", parsed$intent)) || grepl("\\?", parsed$utterance)
  if (isTRUE(is_ask)) parsed$proceed <- FALSE
  parsed$proceed <- isTRUE(parsed$proceed)
  parsed
}

agent_turn <- function(history_user, history_agent, recipe_row,
                       model_agent = "llama3.1:8b") {
  sys <- agent_system_prompt(recipe_row$recipe_title[1])
  ctx <- paste0(
    "RECIPE: ", recipe_row$recipe_title[1], "\n",
    "STEP: ", recipe_row$step_description[1], "\n",
    "LATEST USER MESSAGE: ", tail(history_user, 1), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse=" | "), "\n",
    "Generate your next JSON response."
  )
  tries <- 0; parsed <- NULL; raw <- ""
  while (is.null(parsed) && tries < 3) {
    tries <- tries + 1
    raw <- query_ollama(ctx, model = model_agent, base_prompt = sys, format_json = TRUE)
    parsed <- parse_first_json(raw)
  }
  if (is.null(parsed)) {
    cat("\n--- RAW AGENT OUTPUT ---\n", raw, "\n-------------------------\n")
    stop("Agent LLM did not return valid JSON.")
  }
  parsed$reply <- parsed$reply %||% parsed$utterance %||% parsed$message %||% ""
  parsed
}

# -------------------------------
# 6) Conversation controller (one step)
# -------------------------------
run_step <- function(profile, recipe_row,
                     model_user = "llama3.1:8b",
                     model_agent = "llama3.1:8b",
                     max_questions_per_step = 2,
                     need_choice = c("task","science","history")) {
  
  need_choice <- match.arg(need_choice)
  neutral_need <- switch(
    need_choice,
    task    = sample(recipe_row$need_task[[1]], 1),
    science = sample(recipe_row$need_science[[1]], 1),
    history = sample(recipe_row$need_history[[1]], 1)
  )
  
  history_user <- character(); history_agent <- character()
  questions <- 0L; turn <- 1L; log <- list()
  
  repeat {
    # USER
    u <- user_turn(history_user, history_agent, profile, recipe_row, neutral_need, model_user)
    u$utterance <- trimws(u$utterance)
    history_user <- c(history_user, u$utterance)
    
    if (u$intent %in% c("ASK_TASK","ASK_SCIENCE","ASK_HISTORY","REPEAT","CLARIFY")) questions <- questions + 1L
    
    log[[length(log)+1]] <- list(
      step_id   = recipe_row$step_id[1],
      need_type = need_choice,
      turn      = turn,
      role      = "user",
      payload   = u
    )
    
    if (isTRUE(u$proceed) || questions >= max_questions_per_step) break
    
    # AGENT
    a <- agent_turn(history_user, history_agent, recipe_row, model_agent)
    a$reply <- trimws(a$reply)
    history_agent <- c(history_agent, a$reply)
    
    log[[length(log)+1]] <- list(
      step_id   = recipe_row$step_id[1],
      need_type = need_choice,
      turn      = turn,
      role      = "agent",
      payload   = a
    )
    
    turn <- turn + 1L
  }
  
  tibble(
    step_id   = map_int(log, ~ as_int1(.x$step_id %||% recipe_row$step_id[1])),
    need_type = map_chr(log, ~ as_chr1(.x$need_type %||% "")),
    turn      = map_int(log, ~ as_int1(.x$turn)),
    role      = map_chr(log, ~ as_chr1(.x$role)),
    text      = map_chr(log, ~ if (.x$role == "user") as_chr1(.x$payload$utterance, "") else as_chr1(.x$payload$reply, "")),
    intent    = map_chr(log, ~ if (.x$role == "user") as_chr1(.x$payload$intent, NA_character_) else NA_character_),
    codes     = map(log, ~ if (.x$role == "user") { cvec <- .x$payload$codes; if (is.null(cvec)) character(0) else as.character(cvec) } else NULL)
  )
}

# -------------------------------
# 7) Pretty printer + flatten
# -------------------------------
print_conversation <- function(df, conversation_id, step_id = NULL) {
  x <- df %>% filter(conversation_id == !!conversation_id) %>% arrange(step_id, turn, role)
  if (!is.null(step_id)) x <- x %>% filter(step_id == !!step_id)
  if (nrow(x) == 0) { cat("No rows for that selection.\n"); return(invisible(NULL)) }
  cat("\n==== Conversation:", conversation_id, "====\n")
  for (sid in unique(x$step_id)) {
    cat(sprintf("\n-- Step %d --\n", sid))
    xs <- x %>% filter(step_id == sid)
    apply(xs, 1, function(r) {
      prefix <- if (r[["role"]] == "user") "👤" else "🤖"
      cat(sprintf("%s [%s %s] %s\n", prefix, r[["role"]], r[["turn"]], r[["text"]]))
    })
  }
  invisible(x)
}

flatten_codes <- function(df) {
  df %>% mutate(codes = map_chr(codes, ~ if (is.null(.x) || length(.x)==0) "" else paste(.x, collapse=",")))
}

# -------------------------------
# 8) Minimal driver: one conversation per profile (all steps)
# -------------------------------
MODEL_USER  <- "deepseek-r1:32b"   # user-bot (compliant JSON)
MODEL_AGENT <- "deepseek-r1:8b" # agent-bot (or same as user)

run_one_conversation <- function(profile, model_user = MODEL_USER, model_agent = MODEL_AGENT) {
  out <- lapply(recipe_df$step_id, function(sid) {
    row <- recipe_df %>% filter(step_id == sid)
    need_type <- c("task","science","history")[ ((sid-1) %% 3) + 1 ]
    dlg <- run_step(profile, row, model_user, model_agent, max_questions_per_step = 2, need_choice = need_type)
    dlg$cluster <- profile$name
    dlg$conversation_id <- paste0(profile$name, "__debug")
    dlg
  })
  bind_rows(out)
}

dialogs_c1 <- run_one_conversation(profile_hyperpolite)
dialogs_c3 <- run_one_conversation(profile_direct)
dialogs_all <- bind_rows(dialogs_c1, dialogs_c3) %>% arrange(cluster, conversation_id, step_id, turn, role)
dialogs_flat <- flatten_codes(dialogs_all)

# Inspect
print(dialogs_flat, n = nrow(dialogs_flat))
# View(dialogs_flat)  # uncomment in RStudio

# Example pretty print
print_conversation(dialogs_flat, conversation_id = paste0(profile_hyperpolite$name, "__debug"))
