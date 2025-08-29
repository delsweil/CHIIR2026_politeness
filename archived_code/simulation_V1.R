# I archived this one: at this point the code generated conversations

# ============================================================
# Politeness Simulation (LLM-led) — Minimal Proof of Concept
# Requires: httr, jsonlite, tibble, dplyr, tidyr, purrr, stringr
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

set.seed(42)

# -------------------------------
# 0) Ollama wrapper (your function)
# -------------------------------
# Wrapper function to send a single request to Ollama
query_ollama <- function(utterance,
                         model = "deepseek-r1:32b",
                         base_prompt = "",
                         temperature = 0.7,
                         format_json = TRUE) {
  
  prompt_text <- paste0(base_prompt, "\n\nUtterance: ", utterance)
  
  body <- list(
    model = model,
    prompt = prompt_text,
    stream = FALSE,
    options = list(temperature = temperature)
  )
  if (format_json) body$format <- "json"  # <-- ask for JSON mode
  
  res <- httr::POST(
    url = "http://intern.schlaubox.de:11434/api/generate",
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  if (res$status_code != 200) return(NA)
  result <- httr::content(res)
  result$response
}


# -------------------------------
# 1) Tiny toy recipe with neutral needs
# -------------------------------
recipe_title <- "Crispy Duck Breast with Béchamel"

recipe_df <- tibble::tibble(
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
    c("What is the correct duration for tempering before cooking?"),
    c("How deep should the scoring be to avoid cutting into the meat?"),
    c("At what point should the heat be adjusted from medium if needed?"),
    c("How frequently should excess fat be removed during rendering?"),
    c("What is the basic ratio of fat, flour, and milk for béchamel?")
  ),
  need_science = list(
    c("How does starting from room temperature affect protein denaturation?"),
    c("How does scoring influence heat transfer and fat rendering?"),
    c("Why does starting in a cold pan change fat rendering dynamics?"),
    c("How does prolonged rendering affect skin crispness and moisture loss?"),
    c("What is the role of starch gelatinization in béchamel thickening?")
  ),
  need_history = list(
    c("Is tempering meat a historically documented practice?"),
    c("Is skin scoring a traditional method in European preparations?"),
    c("Is the cold-pan method associated with specific culinary schools?"),
    c("Are there historical references to systematic fat rendering in duck cookery?"),
    c("When and where did béchamel sauce originate?")
  )
)

# Long format helper (optional)
needs_long <- recipe_df |>
  tidyr::pivot_longer(cols = starts_with("need_"),
                      names_to = "need_type", values_to = "needs") |>
  tidyr::unnest(needs)

# -------------------------------
# 2) Cluster 1 – Hyperpolite profile (probabilities)
# Based on Anna's analysis
# -------------------------------

profile_hyperpolite <- list(
  name = "C1_Hyperpolite",
  code_rates = list(
    THANK       = 0.80,  # very frequent
    ACK         = 0.70,  # frequent
    COMPLIMENT  = 0.40,  # regular
    INDIRECT_REQ= 0.60,  # prefers indirect
    INFO_Q      = 0.30,  # occasional (we keep as a descriptor)
    HINT        = 0.50,  # statement hints (“I’m ready for the next step”)
    APOLOGY     = 0.10,  # rare, for repair
    DIRECT_REQ  = 0.05,  # avoided
    PLEASE      = 0.05,  # avoided
    PROCEED     = 0.30   # will move on with hint-style wording
  ),
  words_target = list(min=18, max=28)
)

# -------------------------------
# 3) System prompts (LLM-led generation, JSON-only)
# -------------------------------
user_system_prompt <- function(profile, recipe_title) {
  paste0(
    "ROLE: You simulate a human home cook interacting with an AI assistant while following a recipe step.

PERSONA (Cluster 1 – Hyperpolite):
- Highly face-enhancing: frequent THANK and ACK; sometimes COMPLIMENT.
- Prefer INDIRECT requests and statement HINTS (e.g., “I’m ready for the next step.”) instead of direct commands.
- Rare APOLOGY used only as repair for confusion.
- Avoid PLEASE and direct/non-sentential requests in most turns.
- Combine THANK + ACK + COMPLIMENT in multi-part turns when appropriate.
- Maintain approximately ", profile$words_target$min, "–", profile$words_target$max, " words per turn (±30%).

Probabilistic tendencies (not per-turn rules):
", jsonlite::toJSON(profile$code_rates, auto_unbox = TRUE), "

CONTEXT:
- You receive the recipe title, current step description, and RECENT CONTEXT from the conversation log.
- You also receive a NEUTRAL INFORMATION NEED (task/science/history) for this step; you should surface that need in your own hyperpolite style.

OUTPUT REQUIREMENTS:
You MUST return a single JSON object. No code fences. No explanations. No prefixes/suffixes. No <think>…</think>.
Return ONLY valid JSON with this schema on a single line:
{
  \"ok\": boolean,
  \"utterance\": string,      // your next message to the agent
  \"codes\": string[],        // any of {THANK,PLEASE,DIRECT_REQ,INDIRECT_REQ,ACK,APOLOGY,COMPLIMENT,HINT,INFO_Q,PROCEED} that you ACTUALLY used
  \"intent\": string,         // one of {ASK_TASK,ASK_SCIENCE,ASK_HISTORY,ACK,THANK,COMPLIMENT,REPEAT,CLARIFY,PROCEED}
  \"proceed\": boolean        // true if you want to move to next step now
}

NOTES:
- Surface the neutral need in your style (indirect/hint-heavy). Use PLEASE rarely. Prefer indirect requests.
- If you want to move on, do it as a hint (e.g., “I’m ready for the next step now, thank you.”) and set proceed=true."
  )
}

agent_system_prompt <- function(recipe_title) {
  paste0(
    "ROLE: You are a cooking assistant responding to a user.
- Stay on the current recipe step and the user’s explicit need.
- You may answer task, science, or history questions—brief, clear paragraphs.

CONTEXT:
- You receive the recipe title, current step description, and RECENT CONTEXT from the dialogue.

OUTPUT REQUIREMENTS:
You MUST return a single JSON object. No code fences. No explanations. No prefixes/suffixes. No <think>…</think>.
Return ONLY valid JSON with this schema on a single line:
{
  \"ok\": boolean,
  \"reply\": string
}

NOTES:
- Reply to the user’s latest message only.
- No meta-talk about being an AI. Keep it concise and relevant to the step."
  )
}

# -------------------------------
# 4) JSON parsing helper
# -------------------------------
parse_first_json <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  i <- regexpr("\\{", x)[1]
  if (i == -1) return(NULL)
  # naive brace matching
  open <- 0; end <- nchar(x)
  for (k in seq(i, nchar(x))) {
    ch <- substr(x, k, k)
    if (ch == "{") open <- open + 1
    if (ch == "}") { open <- open - 1; if (open == 0) { end <- k; break } }
  }
  json_txt <- substr(x, i, end)
  tryCatch(jsonlite::fromJSON(json_txt), error = function(e) NULL)
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
    "RECENT USER TURNS: ", paste(tail(history_user, 3), collapse = " | "), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 3), collapse = " | "), "\n",
    "Generate your next JSON response."
  )
  
  raw <- query_ollama(utterance = ctx, model = model_user, base_prompt = sys)
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) stop("User LLM did not return valid JSON.")
  parsed
}

agent_turn <- function(history_user, history_agent, recipe_row,
                       model_agent = "deepseek-r1:8b") {
  
  sys <- agent_system_prompt(recipe_row$recipe_title[1])
  ctx <- paste0(
    "RECIPE: ", recipe_row$recipe_title[1], "\n",
    "STEP: ", recipe_row$step_description[1], "\n",
    "LATEST USER MESSAGE: ", tail(history_user, 1), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse = " | "), "\n",
    "Generate your next JSON response."
  )
  
  raw <- query_ollama(utterance = ctx, model = model_agent, base_prompt = sys)
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) stop("Agent LLM did not return valid JSON.")
  parsed
}

# -------------------------------
# 6) Conversation controller (one step)
# -------------------------------
run_step <- function(profile, recipe_row,
                     model_user = "deepseek-r1:8b",
                     model_agent = "deepseek-r1:8b",
                     max_questions_per_step = 5,
                     need_choice = c("task","science","history")) {
  
  # Pick a neutral need type for this step (or set explicitly)
  need_choice <- match.arg(need_choice)
  neutral_need <- switch(
    need_choice,
    task = sample(recipe_row$need_task[[1]], 1),
    science = sample(recipe_row$need_science[[1]], 1),
    history = sample(recipe_row$need_history[[1]], 1)
  )
  
  history_user <- character()
  history_agent <- character()
  questions <- 0L
  turn <- 1L
  log <- list()
  
  repeat {
    # USER (LLM-led)
    u <- user_turn(history_user, history_agent, profile, recipe_row, neutral_need, model_user)
    u$utterance <- trimws(u$utterance)
    history_user <- c(history_user, u$utterance)
    
    if (u$intent %in% c("ASK_TASK","ASK_SCIENCE","ASK_HISTORY","REPEAT","CLARIFY")) {
      questions <- questions + 1L
    }
    
    # record the user turn
    log[[length(log)+1]] <- list(
      step_id = recipe_row$step_id[1],
      need_type = need_choice,
      turn = turn,
      role = "user",
      text = u$utterance,
      intent = u$intent,
      codes = u$codes
    )
    
    # stop if user wants to proceed or max questions reached
    if (isTRUE(u$proceed) || questions >= max_questions_per_step) break
    
    # AGENT (LLM-led)
    a <- agent_turn(history_user, history_agent, recipe_row, model_agent)
    a$reply <- trimws(a$reply)
    history_agent <- c(history_agent, a$reply)
    
    log[[length(log)+1]] <- list(
      step_id = recipe_row$step_id[1],
      need_type = need_choice,
      turn = turn,
      role = "agent",
      text = a$reply,
      intent = NA_character_,
      codes = NA
    )
    
    turn <- turn + 1L
  }
  
  tibble(
    step_id = map_int(log, "step_id"),
    need_type = map_chr(log, "need_type"),
    turn = map_int(log, "turn"),
    role = map_chr(log, "role"),
    text = map_chr(log, "text"),
    intent = map_chr(log, "intent"),
    codes = I(map(log, "codes"))
  )
}

# -------------------------------
# 7) Run a tiny demo across steps
# -------------------------------
# Choose your Ollama models here (user/agent can be the same or different)
MODEL_USER  <- "deepseek-r1:32b"     # e.g., "llama3.1:8b" or "deepseek-r1:7b"
MODEL_AGENT <- "deepseek-r1:8b"

# For a quick proof-of-concept, run 1–3 steps:
demo_runs <- lapply(1:3, function(sid) {
  row <- recipe_df |> filter(step_id == sid)
  # Alternate need types for variety
  need_type <- c("task","science","history")[(sid-1) %% 3 + 1]
  run_step(
    profile = profile_hyperpolite,
    recipe_row = row,
    model_user = MODEL_USER,
    model_agent = MODEL_AGENT,
    max_questions_per_step = 2,
    need_choice = need_type
  )
})

dialogs <- bind_rows(demo_runs)   # instead of demo <- ...
View(dialogs)                     # spreadsheet view (RStudio)
glimpse(dialogs)                  # compact structure
