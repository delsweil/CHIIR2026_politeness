# ============================================================
# Politeness Simulation (LLM-led) — Agent Codes + Apologies
# ============================================================

# pick a CRAN mirror so non-interactive installs don’t prompt
options(repos = c(CRAN = "https://cloud.r-project.org"))

required <- c("httr","jsonlite","tibble","dplyr","tidyr","purrr","stringr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  lapply(required, require, character.only = TRUE)
})


suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(tibble); library(dplyr)
  library(tidyr); library(purrr); library(stringr)
})

set.seed(42)

# ==== Global energy-monitoring switch ====
MONITOR_ENERGY  <- TRUE       # set TRUE to enable metering (run on GPU host)
GPU_INDEX       <- 0           # GPU index on server
SMI_INTERVAL_MS <- 200         # sampling interval

cat(sprintf("[Init] Energy monitoring: %s\n",
            if (MONITOR_ENERGY) "ON" else "OFF"))

# -------------------------------
# Utilities
# -------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
as_chr1 <- function(x, default = "")         if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])
as_int1 <- function(x, default = NA_integer_)if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.integer(x[[1]])

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
# Frummet et al. recipes (SeriousEats) — templated needs
# ============================================================
make_needs <- function(step) {
  list(
    task    = paste("What is the correct way to perform this step:", step),
    science = paste("What is the underlying science behind this step:", step),
    history = paste("Is there historical background or origin related to this step:", step)
  )
}

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
) %>% rowwise() %>% mutate(needs=list(make_needs(step_description))) %>% 
  unnest_wider(needs, names_sep="_") %>% 
  rename(need_task=needs_task, need_science=needs_science, need_history=needs_history)

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
    HINT=0.50, APOLOGY=0.10, DIRECT_REQ=0.05, PLEASE=0.05, PROCEED=0.35
  ),
  words_target = list(min=18, max=28)
)
profile_direct <- list(
  name = "C3_DirectLowPoliteness",
  code_rates = list(
    THANK=0.10, ACK=0.15, COMPLIMENT=0.02, INDIRECT_REQ=0.10, INFO_Q=0.15,
    HINT=0.05, APOLOGY=0.03, DIRECT_REQ=0.70, PLEASE=0.02, PROCEED=0.85
  ),
  words_target = list(min=8, max=18)
)
profile_impolite <- list(
  name = "C5_Impolite",
  code_rates = list(
    THANK=0.00, ACK=0.05, COMPLIMENT=0.00, INDIRECT_REQ=0.05, INFO_Q=0.10,
    HINT=0.00, APOLOGY=0.00, DIRECT_REQ=0.80, PLEASE=0.00, PROCEED=0.85,
    "Absolving – denial"=0.10, "Absolving – acceptance"=0.00,
    "Absolving – ignore"=0.05, Declining=0.10
  ),
  words_target = list(min=4, max=10)
)

# -------------------------------
# Persona text for prompts
# -------------------------------
persona_text <- function(profile_name) {
  switch(profile_name,
         "C1_Hyperpolite" = paste(
           "- Face-enhancing, human-like: frequent THANK/ACK (e.g., “Thanks. That was great”, “Thank you”, “Thank you. This is very useful”, “Interesting!”, “Useful tip, thanks!”); occasional, understated COMPLIMENT.",
           "- Prefer INDIRECT requests (e.g., “Can you tell me about ... ?”, “Can I do [some action]?”)", 
           "- Prefer statement HINTS (e.g., “I’m ready for the next step.”).",
           "- Rare APOLOGY for repair; avoid PLEASE most of the time.",
           "- If the agent apologises, accept politely (e.g., “That’s OK”) and mark 'Absolving – acceptance'.",
           "- Remember to pay attention to PROCEED probability. Most utterances will be to move on to the next step!",
           sep="\n"
         ),
         "C3_DirectLowPoliteness" = paste(
           "- Tool-like, concise, low engagement; minimal THANK/ACK/COMPLIMENT/APOLOGY.",
           "- Prefer DIRECT or non-sentential requests (e.g., 'next'); rare PLEASE/HINT.",
           "- Ask the occasional information-eliciting question; keep it brief.",
           "- If the agent apologises, you usually just move on without absolving.",
           "- Remember to pay attention to PROCEED probability. Most utterances will be to move on to the next step!",
           sep="\n"
         ),
         "C5_Impolite" = paste(
           "- Abrupt, brusque, sometimes rude: issues imperatives or terse commands.",
           "- Does not use THANK, PLEASE, or COMPLIMENT.",
           "- Rarely acknowledges information; sometimes dismiss or deny it instead.",
           "- Include criticisms or complaints about the agent’s answers. (e.g., ‘This is irrelevant!’; ‘That is a rubbish reply!’‘I don't see the point!’)",
           "- Question the agent’s competence  (e.g., ‘You don’t know what you are talking about!’, ‘My [Grandmother|Father|Mother|etc's] way is better!’)",
           "- Move on quickly (‘next’, ‘continue’) without repair.",
           "- Keeps turns short and transactional.",
           "- Remember to pay attention to PROCEED probability. Most utterances will be to move on to the next step!",
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
    jsonlite::toJSON(profile$code_rates, auto_unbox=TRUE), "\n",
    "- Treat these as tendencies. In particular, if the 'PROCEED' tendency is high and you feel confident, set `proceed=true`, set `intent='PROCEED'`, and make your utterance signal moving on (e.g., 'next step', or a persona-appropriate phrasing).\n\n",
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
    "- Answer clearly.\n",
    "- In ~10% of cases (esp. science/history needs), FAIL gracefully by apologising: \"I'm sorry, I don’t know.\" Include code \"Apology (IFID)\".\n\n",
    "- If the user asks for a picture FAIL gracefully by apologising. You cannot show images.",
    "OUTPUT SCHEMA:\n{ \"ok\": boolean, \"reply\": string, \"codes\": string[] }\n
REQUIREMENTS:\n- 'reply' must be a non-empty natural-language answer (at least one sentence).",
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
    options = list(
      temperature = temperature,
      num_ctx = 4096,
      num_predict = 256
    )
  )
  if (format_json) body$format <- "json"
  res <- httr::POST("http://intern.schlaubox.de:11434/api/generate",
                    body = jsonlite::toJSON(body, auto_unbox = TRUE),
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
allowed_intents <- c("ASK_TASK","ASK_SCIENCE","ASK_HISTORY","ACK","THANK","COMPLIMENT","REPEAT","CLARIFY","PROCEED")

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
  grepl("^\\s*(add|brush|cool|mix|slice|whisk|place|transfer|bake|preheat|chill|score|prick|pound|fold|serve|cut|pipe|cook|heat)\\b",
        tolower(txt))
}

user_turn <- function(history_user, history_agent, profile, recipe_row, neutral_need,
                      model_user = "llama3.1:8b",
                      extra_hint = NULL) {
  sys <- user_system_prompt(profile, recipe_row$recipe_title[1])
  
  build_ctx <- function(extra = NULL) {
    paste0(
      "RECIPE: ", recipe_row$recipe_title[1], "\n",
      "STEP: ", recipe_row$step_description[1], "\n",
      "NEUTRAL NEED: ", neutral_need, "\n",
      "RECENT USER TURNS: ", paste(tail(history_user, 3), collapse = " | "), "\n",
      "RECENT AGENT TURNS: ", paste(tail(history_agent, 2), collapse = " | "), "\n",
      if (!is.null(extra)) paste0("\nREMINDER: ", extra, "\n") else ""
    )
  }
  
  raw <- query_ollama(build_ctx(extra_hint), model_user, sys, temperature = 0.5, format_json = TRUE)
  parsed <- parse_first_json(raw)
  
  
  parsed$utterance <- parsed$utterance %||% parsed$message %||% ""
  utt <- parsed$utterance %||% ""
  if (grepl("\\b(next( step)?|continue|move on|ready for the next step)\\b", tolower(utt))) {
    parsed$intent   <- "PROCEED"
    parsed$proceed  <- TRUE
  }
  parsed$intent    <- normalize_intent(parsed$intent)
  if (is.na(parsed$intent)) parsed$intent <- "ACK"
  parsed$codes     <- parsed$codes %||% character(0)
  
  # infer question
  is_ask <- (!is.na(parsed$intent) && grepl("^ASK_", parsed$intent)) || grepl("\\?", parsed$utterance)
  
  # normalize proceed: questions never proceed, explicit proceed does
  if (identical(parsed$intent, "PROCEED")) parsed$proceed <- TRUE
  if (is_ask) parsed$proceed <- FALSE else parsed$proceed <- isTRUE(parsed$proceed)
  
  
  
  needs_retry <- is.null(parsed) ||
    is.na(normalize_intent(parsed$intent)) ||
    !(normalize_intent(parsed$intent) %in% allowed_intents) ||
    looks_imperative(parsed$utterance %||% "")
  
  if (needs_retry) {
    hint <- "You are the USER. Do NOT issue step-by-step instructions. Ask, acknowledge/thank, or say 'next' to proceed."
    raw <- query_ollama(build_ctx(hint), model_user, sys, temperature = 0.5, format_json = TRUE)
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) stop("User LLM did not return valid JSON:\n", raw)
  }
  
  parsed$utterance <- parsed$utterance %||% parsed$message %||% ""
  parsed$intent    <- normalize_intent(parsed$intent)
  if (is.na(parsed$intent)) parsed$intent <- "ACK"
  parsed$codes     <- parsed$codes %||% character(0)
  is_ask <- (!is.na(parsed$intent) && grepl("^ASK_", parsed$intent)) || grepl("\\?", parsed$utterance)
  if (is_ask) parsed$proceed <- FALSE
  parsed$proceed <- isTRUE(parsed$proceed)
  parsed
}

agent_turn <- function(history_user, history_agent, recipe_row, neutral_need,
                       model_agent = "llama3.1:8b") {
  sys <- agent_system_prompt(recipe_row$recipe_title[1])
  build_ctx <- function(extra_hint = NULL) {
    paste0(
      "RECIPE: ", recipe_row$recipe_title[1], "\n",
      "STEP: ", recipe_row$step_description[1], "\n",
      "USER SAID: ", tail(history_user, 1) %||% "", "\n",
      "NEED: ", neutral_need, "\n",
      "HISTORY (last turns): USER[", paste(tail(history_user, 2), collapse = " | "),
      "] | AGENT[", paste(tail(history_agent, 2), collapse = " | "), "]",
      if (!is.null(extra_hint)) paste0("\n\nREMINDER: ", extra_hint) else ""
    )
  }
  
  normalize_reply_obj <- function(obj) {
    # accept reply in multiple keys; ensure codes vector
    r <- obj$reply %||% obj$utterance %||% obj$message %||% ""
    c <- obj$codes  %||% character(0)
    list(ok = isTRUE(obj$ok), reply = as_chr1(r, ""), codes = as.character(c))
  }
  
  # 1) First attempt
  raw <- query_ollama(build_ctx(), model_agent, sys, temperature = 0.3, format_json = TRUE)
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) {
    # If completely unparsable, retry once with strict hint
    raw <- query_ollama(
      build_ctx("Return exactly ONE JSON object with a non-empty 'reply' string and optional 'codes'."),
      model_agent, sys, temperature = 0.2, format_json = TRUE
    )
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) stop("Agent LLM did not return valid JSON:\n", raw)
  }
  
  norm <- normalize_reply_obj(parsed)
  
  # 2) Validate reply content; retry once if empty or whitespace
  needs_retry <- !nzchar(trimws(norm$reply))
  if (needs_retry) {
    raw <- query_ollama(
      build_ctx("Your reply must answer the user in 1–3 sentences. The 'reply' field MUST NOT be empty."),
      model_agent, sys, temperature = 0.2, format_json = TRUE
    )
    parsed <- parse_first_json(raw)
    if (is.null(parsed)) {
      # fall back to apology JSON if still invalid
      return(list(ok = TRUE,
                  reply = "I’m sorry—I couldn’t compose a proper response. Could you rephrase or ask again?",
                  codes = c("Apology (IFID)")))
    }
    norm <- normalize_reply_obj(parsed)
    if (!nzchar(trimws(norm$reply))) {
      return(list(ok = TRUE,
                  reply = "I’m sorry—I don’t have a good answer to that right now.",
                  codes = c("Apology (IFID)")))
    }
  }
  
  # 3) Return normalized
  list(ok = TRUE, reply = norm$reply, codes = norm$codes)
}



# =========================
# GPU power measurement (NVIDIA)
# =========================
summarise_power_trace <- function(trace) {
  if (!is.data.frame(trace) || nrow(trace) < 1) {
    return(list(energy_Wh = 0, mean_W = NA_real_, peak_W = NA_real_, trace = data.frame(time=as.POSIXct(character()), W=numeric())))
  }
  tsec <- as.numeric(trace$time - trace$time[1], units = "secs")
  W    <- trace$W
  area_J <- if (length(W) >= 2) sum((W[-1] + W[-length(W)]) / 2 * diff(tsec)) else 0
  list(energy_Wh = area_J / 3600, mean_W = mean(W, na.rm = TRUE), peak_W = max(W, na.rm = TRUE), trace = trace)
}

.start_power_logger_proc <- function(gpu = 0, interval_ms = 200) {
  if (!requireNamespace("processx", quietly = TRUE)) return(NULL)
  logfile <- tempfile("gpu_power_", fileext = ".log")
  px <- processx::process$new(
    "nvidia-smi",
    args = c(paste0("-i=", gpu), "--query-gpu=power.draw", "--format=csv,noheader,nounits", paste0("--loop-ms=", interval_ms)),
    stdout = logfile,
    stderr = tempfile("gpu_power_err_", fileext = ".log"),
    supervise = TRUE
  )
  list(kind = "proc", proc = px, file = logfile, t0 = Sys.time(), gpu = gpu)
}

.stop_power_logger_proc <- function(h) {
  if (is.null(h) || h$kind != "proc") return(NULL)
  if (h$proc$is_alive()) h$proc$kill()
  lines <- readLines(h$file, warn = FALSE)
  W <- suppressWarnings(as.numeric(trimws(lines))); W <- W[is.finite(W)]
  if (!length(W)) return(summarise_power_trace(data.frame(time=h$t0, W=numeric(0))))
  t1 <- Sys.time(); ts <- seq(from = h$t0, to = t1, length.out = length(W))
  trace <- data.frame(time = ts, W = W)
  summarise_power_trace(trace)
}

.start_power_logger_R <- function(gpu = 0, interval_s = 0.2) {
  env <- new.env(parent = emptyenv()); env$running <- TRUE; env$time <- c(); env$W <- c()
  sampler <- function() {
    while (isTRUE(get("running", envir = env))) {
      t <- Sys.time()
      out <- tryCatch(
        system2("nvidia-smi",
                args = c(paste0("-i=", gpu), "--query-gpu=power.draw", "--format=csv,noheader,nounits"),
                stdout = TRUE, stderr = FALSE),
        error = function(e) NA_character_
      )
      w <- suppressWarnings(as.numeric(out[1]))
      if (is.finite(w)) { env$time <- c(env$time, t); env$W <- c(env$W, w) }
      Sys.sleep(interval_s)
    }
  }
  bg <- NULL
  if (requireNamespace("parallel", quietly = TRUE) && .Platform$OS.type != "windows") {
    bg <- parallel::mcparallel(sampler(), detached = TRUE)
  } else {
    # minimal synchronous tick: do nothing; main workload runs and we'll stop afterwards
  }
  list(kind = "R", bg = bg, env = env, t0 = Sys.time(), gpu = gpu, interval_s = interval_s)
}

.stop_power_logger_R <- function(h) {
  if (is.null(h) || h$kind != "R") return(NULL)
  assign("running", FALSE, envir = h$env)
  if (!is.null(h$bg) && requireNamespace("parallel", quietly = TRUE)) {
    try(parallel::mccollect(h$bg, wait = FALSE), silent = TRUE)
  }
  trace <- data.frame(time = h$env$time, W = h$env$W)
  summarise_power_trace(trace)
}

with_gpu_energy <- function(expr, gpu = 0, interval_ms = 200, quiet = TRUE) {
  if (!MONITOR_ENERGY) {
    result <- withVisible(eval.parent(substitute(expr)))
    return(list(
      result    = if (result$visible) result$value else invisible(result$value),
      energy_Wh = NA_real_, mean_W = NA_real_, peak_W = NA_real_, trace = NULL
    ))
  }
  
  # Try high-precision process logger first; fall back to portable sampler
  h <- .start_power_logger_proc(gpu = gpu, interval_ms = interval_ms)
  using_fallback <- FALSE
  if (is.null(h)) {
    if (!quiet) message("processx not available; using portable R sampler.")
    h <- .start_power_logger_R(gpu = gpu, interval_s = interval_ms / 1000)
    using_fallback <- TRUE
  }
  
  # Make sure we always stop the logger
  on.exit({
    if (using_fallback) {
      try(.stop_power_logger_R(h), silent = TRUE)
    } else {
      try(.stop_power_logger_proc(h), silent = TRUE)
    }
  }, add = TRUE)
  
  result <- withVisible(eval.parent(substitute(expr)))
  
  # Stop and collect metrics *now*, not via on.exit side-channel
  m <- if (using_fallback) .stop_power_logger_R(h) else .stop_power_logger_proc(h)
  
  list(
    result    = if (result$visible) result$value else invisible(result$value),
    energy_Wh = m$energy_Wh,
    mean_W    = m$mean_W,
    peak_W    = m$peak_W,
    trace     = m$trace
  )
}

agent_turn_with_energy <- function(history_user, history_agent, recipe_row,
                                   neutral_need,
                                   model_agent = "llama3.1:8b",
                                   gpu = 0,
                                   interval_ms = 200,
                                   measure_energy = TRUE) {
  if (!isTRUE(measure_energy)) {
    payload <- agent_turn(history_user, history_agent, recipe_row, neutral_need, model_agent)
    return(list(payload = payload,
                energy_Wh = NA_real_, mean_W = NA_real_, peak_W = NA_real_, trace = NULL))
  }
  
  met <- with_gpu_energy({
    agent_turn(history_user, history_agent, recipe_row, neutral_need, model_agent)
  }, gpu = gpu, interval_ms = interval_ms, quiet = TRUE)
  
  list(payload = met$result,
       energy_Wh = met$energy_Wh,
       mean_W    = met$mean_W,
       peak_W    = met$peak_W,
       trace     = met$trace)
}


# ================================
# Conversation controller
# ================================
set_convo_seed <- function(seed) { if (!is.null(seed)) set.seed(as.integer(seed)) }

# Helper: pick a recipe tibble by name
get_recipe_by_name <- function(name) {
  key <- tolower(gsub("[^a-z0-9]+", "_", name))
  switch(
    key,
    "parisian_gnocchi"          = recipe_parisian_gnocchi,
    "buttermilk_brined_southern_fried_chicken" = recipe_fried_chicken,
    "fried_chicken"             = recipe_fried_chicken,
    "duck_a_lorange"            = recipe_duck_orange,
    "savory_cheese_souffle"     = recipe_souffle,
    "pesto_alla_genovese"       = recipe_pesto,
    "old_fashioned_apple_pie"   = recipe_apple_pie,
    "apple_pie"                 = recipe_apple_pie,
    # default fallback:
    recipe_apple_pie
  )
}

run_step <- function(profile, recipe_row,
                     model_user = "llama3.1:8b",
                     model_agent = "llama3.1:8b",
                     max_questions_per_step = 2,
                     need_choice = c("task","science","history"),
                     measure_energy = MONITOR_ENERGY,
                     gpu_index = GPU_INDEX,
                     smi_interval_ms = SMI_INTERVAL_MS) {
  
  # stochastic need selection with slight bias to science
  need_choice <- sample(c("task","science","history"), size = 1, prob = c(0.35, 0.40, 0.25))
  neutral_need <- switch(need_choice,
                         task    = sample(recipe_row$need_task[[1]], 1),
                         science = sample(recipe_row$need_science[[1]], 1),
                         history = sample(recipe_row$need_history[[1]], 1))
  
  history_user <- character(); history_agent <- character()
  questions <- 0L; turn <- 1L
  turns <- list()  # <-- renamed from `log`
  
  repeat {
    p_proc <- as.numeric(profile$code_rates$PROCEED %||% 0.3)
    turn_wants_proceed <- runif(1) < p_proc
    hint_txt <- if (turn_wants_proceed)
      "You are inclined to move on now. If you feel confident, set intent='PROCEED', proceed=true, and use a short utterance (e.g., 'next step')."
    else
      NULL
    
    # USER TURN
    u <- user_turn(history_user, history_agent, profile, recipe_row, neutral_need,
                   model_user, extra_hint = hint_txt)
    history_user <- c(history_user, u$utterance)
    
    # Count question by intent OR by "?"
    is_ask <- (!is.na(u$intent) && grepl("^ASK_", u$intent)) || grepl("\\?", u$utterance)
    if (is_ask) questions <- questions + 1L
    
    turns[[length(turns)+1L]] <- list(
      step_id     = recipe_row$step_id[1],
      need_type   = need_choice,
      turn        = turn,
      role        = "user",
      payload     = u,
      model_user  = model_user,
      model_agent = model_agent
    )
    
    # If user explicitly proceeds, end the step before an agent reply
    if (isTRUE(u$proceed)) break
    
    # If the user asked/requests/clarifies, we need an agent reply.
    is_ask <- (!is.na(u$intent) && grepl("^ASK_", u$intent)) || grepl("\\?", u$utterance)
    codes_upper <- toupper(u$codes %||% character(0))
    is_request <- any(c("DIRECT_REQ","INDIRECT_REQ","REQUEST – DIRECT","REQUEST – INDIRECT","REQUEST – NON-SENTENTIAL") %in% codes_upper)
    needs_agent <- is_ask || is_request || (!is.na(u$intent) && u$intent %in% c("REPEAT","CLARIFY"))
    
    # If user explicitly proceeds, end now.
    if (isTRUE(u$proceed)) break
    
    # If no reply is needed, skip agent turn and go to next user turn.
    if (!needs_agent) {
      turn <- turn + 1L
      next
    }
    
    # AGENT TURN (metered if enabled)
    am <- agent_turn_with_energy(
      history_user, history_agent, recipe_row, neutral_need,
      model_agent   = model_agent,
      gpu           = gpu_index,
      interval_ms   = smi_interval_ms,
      measure_energy= measure_energy
    )
    a <- am$payload
    history_agent <- c(history_agent, a$reply)
    
    turns[[length(turns)+1L]] <- list(
      step_id     = recipe_row$step_id[1],
      need_type   = need_choice,
      turn        = turn,
      role        = "agent",
      payload     = a,
      energy_Wh   = am$energy_Wh,
      mean_W      = am$mean_W,
      peak_W      = am$peak_W,
      model_user  = model_user,
      model_agent = model_agent
    )
    
    # After answering, if we've hit the question cap, end the step
    if (questions >= max_questions_per_step) break
    
    turn <- turn + 1L
  }
  
  # Build tibble from `turns`
  tibble::tibble(
    step_id   = purrr::map_int(turns, ~ as_int1(.x$step_id)),
    need_type = purrr::map_chr(turns, ~ as_chr1(.x$need_type)),
    turn      = purrr::map_int(turns, ~ as_int1(.x$turn)),
    role      = purrr::map_chr(turns, ~ as_chr1(.x$role)),
    text      = purrr::map_chr(turns, ~ if (.x$role == "user") {
      as_chr1(.x$payload$utterance, "")
    } else {
      t <- as_chr1(.x$payload$reply, "")
      if (!nzchar(trimws(t))) "[empty agent reply]" else t
    }),
    intent    = purrr::map_chr(turns, ~ if (.x$role == "user")
      as_chr1(.x$payload$intent, NA_character_)
      else
        NA_character_),
    codes     = purrr::map(turns, ~ { cvec <- .x$payload$codes
    if (is.null(cvec)) character(0) else as.character(cvec) }),
    energy_Wh = purrr::map_dbl(turns, ~ if (as_chr1(.x$role) == "agent")
      as.numeric(.x$energy_Wh %||% NA_real_) else NA_real_),
    mean_W    = purrr::map_dbl(turns, ~ if (as_chr1(.x$role) == "agent")
      as.numeric(.x$mean_W %||% NA_real_) else NA_real_),
    peak_W    = purrr::map_dbl(turns, ~ if (as_chr1(.x$role) == "agent")
      as.numeric(.x$peak_W %||% NA_real_) else NA_real_),
    model_user  = purrr::map_chr(turns, ~ as_chr1(.x$model_user, "")),
    model_agent = purrr::map_chr(turns, ~ as_chr1(.x$model_agent, "")),
    recipe_title = purrr::map_chr(turns, ~ as_chr1(recipe_row$recipe_title[1], "")),
    event_idx = seq_along(turns)
  )
}


# ================================
# Engagement summaries + drivers
# ================================
run_one_conversation <- function(profile,
                                 recipe_tbl,                     # <— NEW
                                 model_user  = "llama3:8b",
                                 model_agent = "llama3:8b",
                                 seed        = NULL,
                                 measure_energy = MONITOR_ENERGY,
                                 gpu_index      = GPU_INDEX,
                                 smi_interval_ms= SMI_INTERVAL_MS) {
  set_convo_seed(seed)
  convo_tag <- paste0(profile$name, "__", gsub("[^A-Za-z0-9]+","", recipe_tbl$recipe_title[1]),
                      "__seed", ifelse(is.null(seed), "NA", seed))
  
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "] Starting conversation:", convo_tag,
      "| recipe:", recipe_tbl$recipe_title[1],
      "| user:", model_user, "| agent:", model_agent,
      "| energy:", if (measure_energy) "ON" else "OFF", "\n")
  
  out <- lapply(recipe_tbl$step_id, function(sid) {
    row <- dplyr::filter(recipe_tbl, step_id == sid)
    # (Need type can still be randomized inside run_step; this stride is a harmless hint)
    need_type <- c("task","science","history")[ ((sid-1) %% 3) + 1 ]
    dlg <- run_step(profile, row,
                    model_user  = model_user,
                    model_agent = model_agent,
                    max_questions_per_step = 2,
                    need_choice     = need_type,
                    measure_energy  = measure_energy,
                    gpu_index       = gpu_index,
                    smi_interval_ms = smi_interval_ms)
    dlg$cluster         <- profile$name
    dlg$conversation_id <- convo_tag
    dlg$seed            <- seed
    dlg$recipe_title    <- recipe_tbl$recipe_title[1]      # <— keep recipe in output
    dlg
  })
  dplyr::bind_rows(out)
}

run_batch <- function(profiles, n_conversations = 5,
                      recipe_tbl,                           # <— NEW
                      model_user  = "llama3:8b",
                      model_agent = "llama3:8b",
                      base_seed   = 2025,
                      measure_energy = MONITOR_ENERGY,
                      gpu_index      = GPU_INDEX,
                      smi_interval_ms= SMI_INTERVAL_MS) {
  runs <- list()
  for (p in profiles) {
    for (i in seq_len(n_conversations)) {
      seed_i <- base_seed + i
      runs[[length(runs)+1L]] <- run_one_conversation(
        profile         = p,
        recipe_tbl      = recipe_tbl,          # <— pass the chosen recipe
        model_user      = model_user,
        model_agent     = model_agent,
        seed            = seed_i,
        measure_energy  = measure_energy,
        gpu_index       = gpu_index,
        smi_interval_ms = smi_interval_ms
      )
    }
  }
  dplyr::bind_rows(runs) %>%
    dplyr::mutate(role = factor(role, levels = c("user","agent"))) %>%
    dplyr::arrange(cluster, conversation_id, step_id, event_idx)
}

summarise_engagement <- function(dialogs_flat) {
  step_summary <- dialogs_flat %>%
    dplyr::group_by(cluster, conversation_id, step_id) %>%
    dplyr::summarise(
      user_asks     = sum(role == "user" & !is.na(intent) & grepl("^ASK_", intent)),
      agent_replied = any(role == "agent"),
      proceeded     = any(role == "user" & intent == "PROCEED"),
      .groups = "drop"
    )
  
  conv_summary <- step_summary %>%
    dplyr::group_by(cluster, conversation_id) %>%
    dplyr::summarise(
      steps                 = dplyr::n(),
      steps_with_questions  = mean(user_asks > 0),
      steps_proceed_no_q    = mean(proceeded & user_asks == 0),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      dialogs_flat %>%
        dplyr::group_by(cluster, conversation_id) %>%
        dplyr::summarise(
          total_user_turns  = sum(role == "user"),
          total_agent_turns = sum(role == "agent"),
          total_asks        = sum(role == "user" & !is.na(intent) & grepl("^ASK_", intent)),
          .groups = "drop"
        ),
      by = c("cluster","conversation_id")
    )
  
  list(step_summary = step_summary, conv_summary = conv_summary)
}

# ================================
# Driver: CLI + env-configurable
# ================================

# --- helpers to parse args like key=value ---
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    k  <- tolower(trimws(kv[1])); v <- trimws(paste(kv[-1], collapse="="))
    out[[k]] <- v
  }
  out
}

as_bool <- function(x, default = FALSE) {
  if (is.null(x) || identical(x, "")) return(default)
  y <- tolower(as.character(x))
  y %in% c("1","true","t","yes","y","on")
}

# --- Profiles map for CLI selection ---
profiles_map <- list(
  C1 = profile_hyperpolite,
  C3 = profile_direct,
  C5 = profile_impolite
)

# --- Read command line and env defaults ---
cli <- parse_kv(commandArgs(trailingOnly = TRUE))

n_conversations <- as.integer(cli$n %||% Sys.getenv("SIM_N", unset = "1"))
recipe_name     <- cli$recipe %||% Sys.getenv("SIM_RECIPE", unset = "apple_pie")
model_user_cli  <- cli$user   %||% Sys.getenv("SIM_USER_MODEL", unset = "llama3:70b")
model_agent_cli <- cli$agent  %||% Sys.getenv("SIM_AGENT_MODEL", unset = "deepseek-r1:8b")
profiles_csv    <- cli$profiles %||% Sys.getenv("SIM_PROFILES", unset = "C1,C3,C5")
measure_energy  <- as_bool(cli$energy %||% Sys.getenv("SIM_ENERGY", unset = "0"))
base_seed       <- as.integer(cli$seed %||% Sys.getenv("SIM_SEED", unset = "20250"))
outdir          <- cli$outdir %||% Sys.getenv("SIM_OUTDIR", unset = "outputs")

# --- Build profiles set from CSV ---
profile_keys <- strsplit(profiles_csv, ",")[[1]] |> trimws()
profiles_list <- lapply(profile_keys, function(k) {
  if (!k %in% names(profiles_map)) stop("Unknown profile key: ", k,
                                        " (valid: ", paste(names(profiles_map), collapse=", "), ")")
  profiles_map[[k]]
})

# --- Resolve recipe tibble ---
recipe_tbl <- get_recipe_by_name(recipe_name)

# --- Make output dir ---
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Banner (reproducibility) ---
cat("\n========== RUN CONFIG ==========\n",
    "UTC time:        ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "Profiles:        ", paste(profile_keys, collapse=", "), "\n",
    "Recipe:          ", recipe_tbl$recipe_title[1], "\n",
    "n per profile:   ", n_conversations, "\n",
    "User model:      ", model_user_cli, "\n",
    "Agent model:     ", model_agent_cli, "\n",
    "Energy monitor:  ", if (measure_energy) "ON" else "OFF", "\n",
    "Base seed:       ", base_seed, "\n",
    "Output dir:      ", outdir, "\n",
    "================================\n\n", sep="")

# --- Run batch ---
dialogs_all <- run_batch(
  profiles        = profiles_list,
  n_conversations = n_conversations,
  recipe_tbl      = recipe_tbl,
  model_user      = model_user_cli,
  model_agent     = model_agent_cli,
  base_seed       = base_seed,
  measure_energy  = measure_energy,
  gpu_index       = GPU_INDEX,
  smi_interval_ms = SMI_INTERVAL_MS
)

# --- Flatten + sort ---
dialogs_flat <- dialogs_all %>%
  dplyr::mutate(
    codes = purrr::map_chr(codes, ~ if (length(.x)==0) "" else paste(.x, collapse=",")),
    role  = factor(role, levels = c("user","agent"))
  ) %>%
  dplyr::arrange(cluster, conversation_id, step_id, event_idx)

# --- Energy summaries (agent rows only) ---
energy_convo <- dialogs_flat %>%
  dplyr::filter(role == "agent") %>%
  dplyr::group_by(cluster, conversation_id, model_agent) %>%
  dplyr::summarise(
    energy_Wh_total = sum(energy_Wh, na.rm = TRUE),
    agent_turns     = dplyr::n(),
    mean_W_mean     = mean(mean_W, na.rm = TRUE),
    peak_W_max      = max(peak_W, na.rm = TRUE),
    .groups = "drop"
  )

energy_cluster <- energy_convo %>%
  dplyr::group_by(cluster, model_agent) %>%
  dplyr::summarise(
    conv_n            = dplyr::n(),
    mean_energy_Wh    = mean(energy_Wh_total, na.rm = TRUE),
    sd_energy_Wh      = sd(energy_Wh_total, na.rm = TRUE),
    mean_agent_turns  = mean(agent_turns, na.rm = TRUE),
    .groups = "drop"
  )

# --- Engagement dashboard ---
dash <- summarise_engagement(dialogs_flat)

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

# --- Save outputs with stamped filenames ---
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
readr::write_csv(dialogs_flat,      file.path(outdir, paste0("dialogs_flat_", stamp, ".csv")))
readr::write_csv(dash$step_summary, file.path(outdir, paste0("step_summary_", stamp, ".csv")))
readr::write_csv(energy_convo,      file.path(outdir, paste0("energy_convo_", stamp, ".csv")))
readr::write_csv(energy_cluster,    file.path(outdir, paste0("energy_cluster_", stamp, ".csv")))

cat("\n[Done] Files written to: ", normalizePath(outdir), "\n", sep="")



