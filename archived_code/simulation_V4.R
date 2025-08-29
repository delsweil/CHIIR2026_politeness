# This version batch generates conversations for different politeness profiles.
# We have defined prompts for clusters 1, 3 and 5.
# The conversations work well although we may need to tweak the prompts a little.
# I am not happy with the code annotation for the utterances, but I think we will move this to a separate script anyway
# since different models could introduce bias.

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

# ============================================================
# Recipe tibbles with templated info-needs (Frummet et al.)
# ============================================================

make_needs <- function(step) {
  list(
    task    = paste("What is the correct way to perform this step:", step),
    science = paste("What is the underlying science behind this step:", step),
    history = paste("Is there historical background or origin related to this step:", step)
  )
}

# Parisian Gnocchi -------------------------------------------
recipe_parisian_gnocchi <- tibble::tibble(
  recipe_title = "Parisian Gnocchi",
  step_id = 1:5,
  step_description = c(
    "Boil water with butter and salt; add flour and stir vigorously until smooth dough forms and pulls away from pot.",
    "Mix in mustard, cheese, and eggs one at a time (stand mixer or by hand); add herbs; transfer to piping bag.",
    "Let mixture rest 15–25 min; bring salted water to a simmer; pipe dough and cut into 1-inch pieces.",
    "Cook until gnocchi float; simmer 3 more minutes; lift out and transfer to baking sheet with oil.",
    "Repeat with remaining dough; cooled gnocchi can be refrigerated until later use."
  )
) %>% rowwise() %>% mutate(
  needs = list(make_needs(step_description))
) %>% unnest_wider(needs, names_sep="_") %>%
  rename(need_task = needs_task, need_science = needs_science, need_history = needs_history)

# Southern Fried Chicken -------------------------------------
recipe_fried_chicken <- tibble::tibble(
  recipe_title = "Buttermilk-Brined Southern Fried Chicken",
  step_id = 1:6,
  step_description = c(
    "Mix paprika, black pepper, garlic powder, oregano, cayenne in a bowl.",
    "Whisk buttermilk, egg, salt, spice mix; add chicken; marinate 4h–overnight in fridge.",
    "Mix flour, cornstarch, baking powder, salt, remaining spices; dredge chicken, coat thickly.",
    "Preheat oil to 425°F; fry chicken skin side down; maintain oil temp at 300°F.",
    "Transfer chicken to rack; bake at 350°F until 150°F breast / 165°F thigh.",
    "Optional double fry: chill 1h+, then refry at 400°F until extra crisp."
  )
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

# Duck à l’Orange --------------------------------------------
recipe_duck_orange <- tibble::tibble(
  recipe_title = "Duck à l’Orange",
  step_id = 1:10,
  step_description = c(
    "Optional: spatchcock and trim duck; reserve trimmings for stock.",
    "Prick skin, dip briefly in boiling water, dry thoroughly.",
    "Season duck all over with salt; rest uncovered 1–24h in fridge.",
    "Roast trimmings and vegetables until browned; start stock.",
    "Simmer and reduce stock 1–2h; strain and reduce further.",
    "Roast duck at high then low temp until well done (~175°F).",
    "Make caramel gastrique with sugar and vinegar.",
    "Blanch orange zest until softened.",
    "Finish sauce: combine stock, juices, gastrique, butter, zest; reduce.",
    "Carve duck and serve with orange sauce."
  )
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

# Savory Cheese Soufflé --------------------------------------
recipe_souffle <- tibble::tibble(
  recipe_title = "Savory Cheese Soufflé",
  step_id = 1:8,
  step_description = c(
    "Preheat oven; prepare ramekin with butter and cheese; chill.",
    "Make roux and béchamel sauce.",
    "Season béchamel; add mustard/cayenne; whisk in yolks.",
    "Whip egg whites with cream of tartar to glossy peaks.",
    "Fold whites into base; add Gruyère cheese.",
    "Transfer mixture to ramekin; smooth top.",
    "Bake until risen and browned (30–40 min).",
    "Serve immediately before deflating."
  )
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

# Pesto alla Genovese ----------------------------------------
recipe_pesto <- tibble::tibble(
  recipe_title = "Pesto alla Genovese",
  step_id = 1:5,
  step_description = c(
    "Pound garlic to a paste in mortar.",
    "Add pine nuts; crush to sticky paste.",
    "Add basil with salt, pound until fine.",
    "Add cheeses, drizzle oil, emulsify sauce.",
    "Serve immediately or store with oil layer."
  )
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

# Old-Fashioned Apple Pie ------------------------------------
recipe_apple_pie <- tibble::tibble(
  recipe_title = "Old-Fashioned Apple Pie",
  step_id = 1:6,
  step_description = c(
    "Mix spices in bag; slice apples into wedges.",
    "Macerate apples in bag 3–8h until volume reduced.",
    "Add tapioca; assemble pie with top crust; chill.",
    "Brush pie with egg wash; cut vents.",
    "Bake at 400°F until center 195°F (~75 min).",
    "Cool 1h before serving; reheat leftovers if desired."
  )
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

# ============================================================
# Combine into one list if convenient
# ============================================================
all_recipes <- list(
  recipe_parisian_gnocchi,
  recipe_fried_chicken,
  recipe_duck_orange,
  recipe_souffle,
  recipe_pesto,
  recipe_apple_pie
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

profile_impolite <- list(
  name = "C5_Impolite",
  code_rates = list(
    THANK        = 0.00,  # almost never thanks
    ACK          = 0.05,  # rarely acknowledges
    COMPLIMENT   = 0.00,  # never compliments
    INDIRECT_REQ = 0.05,  # hardly ever mitigates requests
    INFO_Q       = 0.10,  # occasionally asks, but abruptly
    HINT         = 0.00,  # no hints, just blunt
    APOLOGY      = 0.00,  # never apologises
    DIRECT_REQ   = 0.80,  # mainly imperatives
    PLEASE       = 0.00,  # never says please
    PROCEED      = 0.70,  # often just moves on brusquely
    # if you’ve added: Absolving strategies, Declining, etc.
    # you can keep them low but >0 to allow denial/complaints
    "Absolving – denial"   = 0.10,
    "Absolving – acceptance" = 0.00,
    "Absolving – ignore"   = 0.05,
    Declining              = 0.10,
  ),
  words_target = list(min = 4, max = 10) # short, brusque
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
         "C5_Impolite" = paste(
           "- Abrupt, brusque, sometimes rude: issues imperatives or terse commands.",
           "- Does not use THANK, PLEASE, or COMPLIMENT.",
           "- Rarely acknowledges information; may dismiss or deny it instead.",
           "- May include criticisms or complaints about the assistant’s answers.",
           "- Moves on quickly (‘next’, ‘continue’) without repair.",
           "- Keeps turns short and transactional.",
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
    "ROLE: You simulate a human home cook interacting with an AI assistant while following a recipe step.\n\n",
    "PERSONA (", profile$name, "):\n", persona_text(profile$name), "\n",
    "Maintain ~", profile$words_target$min, "–", profile$words_target$max, " words per turn.\n\n",
    "IMPORTANT ROLE BOUNDARIES:\n",
    "- You are the user, not the assistant.\n",
    "- DO ask questions, acknowledge, thank, compliment, or say 'next' to proceed.\n",
    "- DO refer to uncertainties, preferences, or requests for clarification.\n",
    "- DON'T give instructions about how to perform the step (e.g., 'Add X', 'Brush Y', 'Cool Z').\n",
    "- DON'T summarize or restate the procedure unless you are confirming understanding in one brief sentence (e.g., 'Got it, rest for 1 hour.').\n\n",
    "Probabilistic tendencies (not per-turn rules):\n",
    jsonlite::toJSON(profile$code_rates, auto_unbox=TRUE), "\n\n",
    "OUTPUT SCHEMA (JSON only):\n",
    "{ \"ok\": boolean, \"utterance\": string, \"codes\": string[], \"intent\": string, \"proceed\": boolean }\n",
    "intent MUST be one of: ASK_TASK, ASK_SCIENCE, ASK_HISTORY, ACK, THANK, COMPLIMENT, REPEAT, CLARIFY, PROCEED.\n\n",
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
allowed_intents <- c(
  "ASK_TASK","ASK_SCIENCE","ASK_HISTORY","ACK","THANK",
  "COMPLIMENT","REPEAT","CLARIFY","PROCEED"
)

normalize_intent <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  y <- trimws(tolower(x))
  if (grepl("^ask[_ ]?task", y))       return("ASK_TASK")
  if (grepl("^ask[_ ]?science", y))    return("ASK_SCIENCE")
  if (grepl("^ask[_ ]?history", y))    return("ASK_HISTORY")
  if (grepl("^(ack|acknowledge)", y))  return("ACK")
  if (grepl("^thank", y))              return("THANK")
  if (grepl("^compliment", y))         return("COMPLIMENT")
  if (grepl("^repeat", y))             return("REPEAT")
  if (grepl("^clarif", y))             return("CLARIFY")
  if (grepl("^(proceed|next)", y))     return("PROCEED")
  NA_character_
}

looks_imperative <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(FALSE)
  # simple heuristic: starts with a common verb (expand as needed)
  grepl(
    "^\\s*(add|brush|cool|mix|slice|whisk|place|transfer|bake|preheat|chill|score|prick|pound|fold|serve|cut|pipe|cook|heat)\\b",
    tolower(txt)
  )
}

user_turn <- function(history_user, history_agent, profile, recipe_row, neutral_need,
                      model_user = "llama3.1:8b") {
  sys <- user_system_prompt(profile, recipe_row$recipe_title[1])
  
  build_ctx <- function(extra_hint = NULL) {
    paste0(
      "RECIPE: ", recipe_row$recipe_title[1], "\n",
      "STEP: ", recipe_row$step_description[1], "\n",
      "NEUTRAL NEED: ", neutral_need, "\n",
      "RECENT USER TURNS: ", paste(tail(history_user, 3), collapse = " | "), "\n",
      "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse = " | "), "\n",
      if (!is.null(extra_hint)) paste0("\nREMINDER: ", extra_hint, "\n") else ""
    )
  }
  
  # 1) First attempt
  raw <- query_ollama(
    utterance   = build_ctx(),
    model       = model_user,
    base_prompt = sys,
    temperature = 0.5,         # user slightly more varied
    format_json = TRUE
  )
  parsed <- parse_first_json(raw)
  
  # 2) Validate; retry once if off-role or bad intent/imperative
  needs_retry <- is.null(parsed) ||
    is.na(normalize_intent(parsed$intent)) ||
    !(normalize_intent(parsed$intent) %in% allowed_intents) ||
    looks_imperative(parsed$utterance %||% "")
  
  if (needs_retry) {
    hint <- "You are the USER. Do NOT issue step-by-step instructions. Ask, acknowledge/thank, or say 'next' to proceed."
    raw <- query_ollama(
      utterance   = build_ctx(hint),
      model       = model_user,
      base_prompt = sys,
      temperature = 0.5,
      format_json = TRUE
    )
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) stop("User LLM did not return valid JSON:\n", raw)
  }
  
  # 3) Clean fields + guards
  parsed$utterance <- parsed$utterance %||% parsed$message %||% ""
  parsed$intent    <- normalize_intent(parsed$intent)
  if (is.na(parsed$intent)) parsed$intent <- "ACK"  # safe fallback label
  parsed$codes     <- parsed$codes %||% character(0)
  
  # If they ask a question, keep the step open
  is_ask <- (!is.na(parsed$intent) && grepl("^ASK_", parsed$intent)) || grepl("\\?", parsed$utterance)
  if (is_ask) parsed$proceed <- FALSE
  parsed$proceed <- isTRUE(parsed$proceed)
  
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
  
  tibble::tibble(
    step_id   = purrr::map_int(log, ~ as_int1(.x$step_id)),
    need_type = purrr::map_chr(log, ~ as_chr1(.x$need_type)),
    turn      = purrr::map_int(log, ~ as_int1(.x$turn)),
    role      = purrr::map_chr(log, ~ as_chr1(.x$role)),
    text      = purrr::map_chr(log, ~ if (.x$role == "user")
      as_chr1(.x$payload$utterance, "")
      else
        as_chr1(.x$payload$reply, "")),
    intent    = purrr::map_chr(log, ~ if (.x$role == "user")
      as_chr1(.x$payload$intent, NA_character_)
      else
        NA_character_),
    codes     = purrr::map(log, ~ {
      cvec <- .x$payload$codes
      if (is.null(cvec)) character(0) else as.character(cvec)
    }),
    event_idx = seq_along(log)
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
  
  # --- NEW print statement here ---
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "] Starting conversation:", convo_tag, "\n")
  
  out <- lapply(recipe_apple_pie$step_id, function(sid) {
    row <- dplyr::filter(recipe_apple_pie, step_id == sid)
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
profiles_list <- list(profile_hyperpolite, profile_direct,profile_impolite)

dialogs_all <- run_batch(
  profiles = profiles_list,
  n_conversations = 1,         # <- change for bigger/smaller batches
  model_user = "llama3:70b",#"deepseek-r1:32b",
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
 readr::write_csv(dialogs_flat, "batch_dialogs_flat.csv")
 readr::write_csv(dash$step_summary, "batch_step_summary.csv")
 readr::write_csv(dash$conv_summary, "batch_conv_summary.csv")
