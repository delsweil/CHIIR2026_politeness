# This version has functions to bulk create conversations using one toy recipe.
# Two politeness profiles are implemented but should be tweaked by Christine and Anna

# ============================================================
# Politeness Simulation (LLM-led) — Agent Codes + Apologies
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(tibble); library(dplyr)
  library(tidyr); library(purrr); library(stringr)
})

set.seed(42)

# -------------------------------
# Utilities
# -------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
as_chr1 <- function(x, default = "")        if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])
as_int1 <- function(x, default = NA_integer_) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.integer(x[[1]])

STRICT_HEADER <- "You MUST return exactly one JSON object on a single line.
No code fences. No explanations. No <think>…</think>.
Output exactly one JSON object and nothing else."
STRICT_FOOTER <- "Output exactly one line containing only the JSON object."

# -------------------------------
# Toy recipe + neutral needs
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
# Profiles
# -------------------------------
profile_hyperpolite <- list(
  name = "C1_Hyperpolite",
  code_rates = list(
    THANK=0.80, ACK=0.70, COMPLIMENT=0.20, INDIRECT_REQ=0.60, INFO_Q=0.30,
    HINT=0.50, APOLOGY=0.10, DIRECT_REQ=0.05, PLEASE=0.05, PROCEED=0.10
  ),
  words_target = list(min=18, max=28)
)

profile_direct <- list(
  name = "C3_DirectLowPoliteness",
  code_rates = list(
    THANK=0.10, ACK=0.15, COMPLIMENT=0.02, INDIRECT_REQ=0.10, INFO_Q=0.15,
    HINT=0.05, APOLOGY=0.03, DIRECT_REQ=0.70, PLEASE=0.02, PROCEED=0.45
  ),
  words_target = list(min=8, max=18)
)

# -------------------------------
# Persona text for prompts
# -------------------------------
persona_text <- function(profile_name) {
  switch(profile_name,
         "C1_Hyperpolite" = paste(
           "- Face-enhancing, human-like: frequent THANK/ACK; occasional, understated COMPLIMENT.",
           "- Prefer INDIRECT requests and statement HINTS (e.g., “I’m ready for the next step.”).",
           "- Rare APOLOGY for repair; avoid PLEASE most of the time.",
           "- If the agent apologises, accept politely (e.g., 'That’s OK') and mark 'Absolving – acceptance'.",
           "- Only proceed immediately if you are confident; otherwise consider asking a brief, relevant question.",  # NEW
           sep="\n"
         ),
         "C3_DirectLowPoliteness" = paste(
           "- Tool-like, concise, low engagement; minimal THANK/ACK/COMPLIMENT/APOLOGY.",
           "- Prefer DIRECT or non-sentential requests (e.g., 'next'); rare PLEASE/HINT.",
           "- Ask relatively few information-eliciting questions; keep it brief.",
           "- If the agent apologises, you usually just move on without absolving.",
           "- Only proceed immediately if you are confident; otherwise consider asking a brief, relevant question.",  # NEW
           sep="\n"
         ),
         ""
  )
}

# -------------------------------
# Prompts (strict JSON)
# -------------------------------
user_system_prompt <- function(profile, recipe_title) {
  paste0(
    STRICT_HEADER, "\n\n",
    "ROLE: You simulate a human home cook following a recipe.\n\n",
    "PERSONA (", profile$name, "):\n", persona_text(profile$name), "\n",
    "Maintain ~", profile$words_target$min, "–", profile$words_target$max, " words per turn.\n\n",
    "Probabilistic tendencies (not per-turn rules):\n",
    toJSON(profile$code_rates, auto_unbox=TRUE), "\n\n",
    "OUTPUT SCHEMA:\n",
    "{ \"ok\": boolean, \"utterance\": string, \"codes\": string[], \"intent\": string, \"proceed\": boolean }\n\n",
    STRICT_FOOTER
  )
}

agent_system_prompt <- function(recipe_title) {
  paste0(
    STRICT_HEADER, "\n\n",
    "ROLE: You are a cooking assistant.\n",
    "- Normally, answer clearly and concisely.\n",
    "- In ~10% of cases (esp. science/history needs), FAIL gracefully by apologising: 'I'm sorry, I don’t know.'\n",
    "- When you apologise, include code 'Apology (IFID)'.\n\n",
    "OUTPUT SCHEMA:\n",
    "{ \"ok\": boolean, \"reply\": string, \"codes\": string[] }\n\n",
    "NOTES:\n",
    "- `codes` must be zero or more of these: Apology (IFID), Thanking, Acknowledge response, Hint - statement, Information-eliciting question, Request – indirect, Compliment/praise/face-enhancing feedback, Absolving – acceptance, Absolving – denial, Absolving - ignore, Politeness marker please, Declining, Request – direct, Request – non-sentential.\n",
    STRICT_FOOTER
  )
}

# -------------------------------
# Ollama wrapper + parser
# -------------------------------
query_ollama <- function(utterance, model, base_prompt, temperature = 0.3, format_json = TRUE) {
  body <- list(
    model = model,
    prompt = paste0(base_prompt, "\n\nUtterance: ", utterance),
    stream = FALSE,
    options = list(temperature = temperature)
  )
  if (format_json) body$format <- "json"
  res <- httr::POST("http://intern.schlaubox.de:11434/api/generate",
                    body = toJSON(body, auto_unbox = TRUE),
                    encode = "json")
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
# User + Agent turn functions
# -------------------------------
user_turn <- function(history_user, history_agent, profile, recipe_row, neutral_need,
                      model_user = "llama3.1:8b") {
  sys <- user_system_prompt(profile, recipe_row$recipe_title[1])
  ctx <- paste0(
    "RECIPE: ", recipe_row$recipe_title[1], "\n",
    "STEP: ", recipe_row$step_description[1], "\n",
    "NEUTRAL NEED: ", neutral_need, "\n",
    "RECENT USER TURNS: ", paste(tail(history_user, 3), collapse=" | "), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse=" | "), "\n"
  )
  raw <- query_ollama(ctx, model_user, sys, format_json=TRUE, temperature = 0.5)
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) stop("User LLM did not return valid JSON:\n", raw)
  parsed$utterance <- parsed$utterance %||% parsed$message %||% ""
  parsed$intent    <- parsed$intent    %||% NA_character_
  parsed$codes     <- parsed$codes     %||% character(0)
  # guard: don’t proceed if asking
  is_ask <- (!is.na(parsed$intent) && grepl("^ASK_", parsed$intent)) || grepl("\\?", parsed$utterance)
  if (is_ask) parsed$proceed <- FALSE
  parsed
}

agent_turn <- function(history_user, history_agent, recipe_row,
                       neutral_need,
                       model_agent = "llama3.1:8b") {
  sys <- agent_system_prompt(recipe_row$recipe_title[1])
  ctx <- paste0(
    "RECIPE: ", recipe_row$recipe_title[1], "\n",
    "STEP: ", recipe_row$step_description[1], "\n",
    "LATEST USER MESSAGE: ", tail(history_user, 1), "\n",
    "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse=" | "), "\n"
  )
  raw <- query_ollama(ctx, model_agent, sys, format_json=TRUE)
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) stop("Agent LLM did not return valid JSON:\n", raw)
  parsed$reply <- parsed$reply %||% parsed$utterance %||% parsed$message %||% ""
  parsed$codes <- parsed$codes %||% character(0)
  parsed
}

# -------------------------------
# Conversation controller
# -------------------------------
run_step <- function(profile, recipe_row,
                     model_user = "llama3.1:8b",
                     model_agent = "llama3.1:8b",
                     max_questions_per_step = 2,
                     need_choice = c("task","science","history")) {
  
  #need_choice <- match.arg(need_choice)
  
  # make the choice of question stochastic with weighted sampling
  need_choice <- sample(c("task","science","history"),
                        size = 1,
                        prob = c(0.35, 0.4, 0.25))  # small tilt to science
  neutral_need <- switch(need_choice,
                         task    = sample(recipe_row$need_task[[1]], 1),
                         science = sample(recipe_row$need_science[[1]], 1),
                         history = sample(recipe_row$need_history[[1]], 1)
  )
  
  history_user <- character(); history_agent <- character()
  questions <- 0L; turn <- 1L; log <- list()
  
  repeat {
    u <- user_turn(history_user, history_agent, profile, recipe_row, neutral_need, model_user)
    history_user <- c(history_user, u$utterance)
    if (u$intent %in% c("ASK_TASK","ASK_SCIENCE","ASK_HISTORY","REPEAT","CLARIFY")) questions <- questions + 1L
    log[[length(log)+1]] <- list(step_id=recipe_row$step_id[1], need_type=need_choice, turn=turn, role="user", payload=u)
    if (isTRUE(u$proceed) || questions >= max_questions_per_step) break
    a <- agent_turn(history_user, history_agent, recipe_row, neutral_need, model_agent)
    history_agent <- c(history_agent, a$reply)
    log[[length(log)+1]] <- list(step_id=recipe_row$step_id[1], need_type=need_choice, turn=turn, role="agent", payload=a)
    turn <- turn + 1L
  }
  
  tibble(
    step_id   = map_int(log, ~ as_int1(.x$step_id)),
    need_type = map_chr(log, ~ as_chr1(.x$need_type)),
    turn      = map_int(log, ~ as_int1(.x$turn)),
    role      = map_chr(log, ~ as_chr1(.x$role)),
    text      = map_chr(log, ~ if (.x$role=="user") as_chr1(.x$payload$utterance,"") else as_chr1(.x$payload$reply,"")),
    intent    = map_chr(log, ~ if (.x$role=="user") as_chr1(.x$payload$intent,NA_character_) else NA_character_),
    codes     = map(log, ~ as.character(.x$payload$codes %||% character(0))),
    event_idx = seq_along(log) # chronological within this step
  )
}

# -------------------------------
# Driver: one conversation per profile
# -------------------------------
MODEL_USER  <- "deepseek-r1:32b"
MODEL_AGENT <- "deepseek-r1:8b"

# ================================
# Engagement summaries + seeding
# ================================

# 1) Reproducible RNG per conversation -----------------------
set_convo_seed <- function(seed) {
  # keep default RNG but let you reproduce runs
  if (!is.null(seed)) set.seed(as.integer(seed))
}

# 2) One conversation per profile with a seed ----------------
run_one_conversation <- function(profile,
                                 model_user = MODEL_USER,
                                 model_agent = MODEL_AGENT,
                                 seed = NULL) {
  set_convo_seed(seed)
  convo_tag <- paste0(profile$name, "__debug",
                      if (!is.null(seed)) paste0("__seed", seed) else "")
  out <- lapply(recipe_df$step_id, function(sid) {
    row <- dplyr::filter(recipe_df, step_id == sid)
    need_type <- c("task","science","history")[ ((sid-1) %% 3) + 1 ]
    dlg <- run_step(profile, row, model_user, model_agent,
                    max_questions_per_step = 2, need_choice = need_type)
    dlg$cluster <- profile$name
    dlg$conversation_id <- convo_tag
    dlg$seed <- seed
    dlg
  })
  dplyr::bind_rows(out)
}

# 3) Small batch runner (N conversations per profile) --------
run_batch <- function(profiles, n_conversations = 5,
                      model_user = MODEL_USER, model_agent = MODEL_AGENT,
                      base_seed = 2025) {
  runs <- list()
  for (p in profiles) {
    for (i in seq_len(n_conversations)) {
      seed_i <- base_seed + i
      runs[[length(runs)+1]] <- run_one_conversation(
        profile = p, model_user = model_user, model_agent = model_agent, seed = seed_i
      )
    }
  }
  dplyr::bind_rows(runs) %>%
    dplyr::arrange(cluster, conversation_id, step_id, event_idx)
}

# 4) Engagement summaries ------------------------------------
summarise_engagement <- function(dialogs_flat) {
  # step-level summary
  step_summary <- dialogs_flat %>%
    dplyr::group_by(cluster, conversation_id, step_id) %>%
    dplyr::summarise(
      user_asks        = sum(role == "user" & !is.na(intent) & grepl("^ASK_", intent)),
      agent_replied    = any(role == "agent"),
      proceeded        = any(role == "user" & intent == "PROCEED"),
      .groups = "drop"
    )
  
  # conversation-level summary
  conv_summary <- dialogs_flat %>%
    dplyr::group_by(cluster, conversation_id, seed) %>%
    dplyr::summarise(
      steps            = dplyr::n_distinct(step_id),
      total_user_turns = sum(role == "user"),
      total_agent_turns= sum(role == "agent"),
      total_asks       = sum(role == "user" & !is.na(intent) & grepl("^ASK_", intent)),
      prop_steps_with_q= mean(role == "user" & !is.na(intent) & grepl("^ASK_", intent)),
      prop_steps_proceed_no_q = mean(step_id %in% {
        # steps where user proceeded but no ask happened
        tmp <- dplyr::group_by(dialogs_flat, conversation_id, step_id)
        dplyr::filter(tmp,
                      conversation_id == cur_group()$conversation_id[1],
                      step_id == cur_group()$step_id[1]
        )
      }),
      .groups = "drop"
    )
  
  list(step_summary = step_summary, conv_summary = conv_summary)
}

# 5) Example: run a batch and print dashboard ----------------
profiles_list <- list(profile_hyperpolite, profile_direct)

dialogs_all <- run_batch(
  profiles = profiles_list,
  n_conversations = 5,         # <- change for bigger/smaller batches
  model_user = "deepseek-r1:32b",
  model_agent = "deepseek-r1:8b",
  base_seed = 20250
)

dialogs_flat <- dialogs_all %>%
  dplyr::mutate(
    codes = purrr::map_chr(codes, ~ if (is.null(.x) || length(.x)==0) "" else paste(.x, collapse=",")),
    intent = ifelse(is.na(intent), NA_character_, intent) # keep NA for agent rows
  )

dash <- summarise_engagement(dialogs_flat)

# Compact view by cluster
cat("\n=== Engagement (per cluster) ===\n")
dash$step_summary %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    steps_with_questions   = mean(user_asks > 0),
    steps_with_agent_reply = mean(agent_replied),
    steps_proceeded_no_q   = mean(proceeded & user_asks == 0),
    avg_questions_per_step = mean(user_asks),
    .groups = "drop"
  ) %>% print(n = Inf)

cat("\n=== Conversation stats (per cluster) ===\n")
dash$conv_summary %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    mean_user_turns  = mean(total_user_turns),
    mean_agent_turns = mean(total_agent_turns),
    mean_asks        = mean(total_asks),
    n_conversations  = dplyr::n(),
    .groups = "drop"
  ) %>% print(n = Inf)

# 6) Optional: save to files --------------------------------
# readr::write_csv(dialogs_flat, "batch_dialogs_flat.csv")
# readr::write_csv(dash$step_summary, "batch_step_summary.csv")
# readr::write_csv(dash$conv_summary, "batch_conv_summary.csv")
