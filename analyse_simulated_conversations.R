#!/usr/bin/env Rscript

# ============================
# Conversational Analysis Script
# ============================
options(repos = c(CRAN = "https://cloud.r-project.org"))

need <- c("readr","dplyr","tidyr","purrr","stringr","tibble","jsonlite")
inst <- setdiff(need, rownames(installed.packages()))
if (length(inst)) install.packages(inst, quiet = TRUE)
suppressPackageStartupMessages({ lapply(need, require, character.only = TRUE) })

# ---------- CLI ----------
parse_kv <- function(args) {
  out <- list()
  for (a in args) {
    if (!grepl("=", a, fixed = TRUE)) next
    kv <- strsplit(a, "=", fixed = TRUE)[[1]]
    out[[kv[1]]] <- paste(kv[-1], collapse="=")
  }
  out
}
cli <- parse_kv(commandArgs(trailingOnly = TRUE))
INDIR <- cli$indir %||% "outputs"
OUTDIR <- cli$out %||% file.path(INDIR, "analysis")
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%F %T")), ..., "\n", sep = "")

# ---------- Helpers ----------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x

tokenize <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("[^a-z0-9'\\s]+"," ", x)
  w <- unlist(strsplit(x, "\\s+"))
  w[nzchar(w)]
}
distinct_n <- function(tokens, n=1) {
  if (!length(tokens)) return(0)
  if (n == 1) return(length(unique(tokens))/length(tokens))
  if (length(tokens) < n) return(0)
  grams <- sapply(seq_len(length(tokens)-n+1), function(i) paste(tokens[i:(i+n-1)], collapse=" "))
  length(unique(grams))/length(grams)
}
# MTLD (very standard implementation)
mtld <- function(tokens, ttr_threshold = 0.72) {
  if (length(tokens) < 10) return(NA_real_)
  seq_mtld <- function(tok) {
    factor_count <- 0; types <- character(); token_count <- 0
    for (w in tok) {
      token_count <- token_count + 1
      if (!(w %in% types)) types <- c(types, w)
      ttr <- length(types)/token_count
      if (ttr <= ttr_threshold) {
        factor_count <- factor_count + 1
        types <- character(); token_count <- 0
      }
    }
    # partial factor
    if (token_count > 0) factor_count <- factor_count + (1 - (length(types)/token_count)) / (1 - ttr_threshold)
    length(tok) / factor_count
  }
  (seq_mtld(tokens) + seq_mtld(rev(tokens))) / 2
}

# JSD between two discrete distributions supplied as named numeric vectors
js_div <- function(p, q, eps=1e-12) {
  keys <- union(names(p), names(q))
  p <- p[keys] %||% 0; q <- q[keys] %||% 0
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  p <- p/sum(p); q <- q/sum(q)
  if (any(!is.finite(p)) || any(!is.finite(q))) return(NA_real_)
  m <- (p + q)/2
  KL <- function(a,b) {
    a <- pmax(a, eps); b <- pmax(b, eps)
    sum(a * log2(a/b))
  }
  0.5 * KL(p,m) + 0.5 * KL(q,m)
}

safe_read <- function(paths) purrr::map_dfr(paths, function(p) {
  tryCatch(readr::read_csv(p, show_col_types = FALSE) %>% mutate(.source = basename(p)),
           error = function(e) { msg("WARN: failed to read ", p, " — ", e$message); tibble() })
})

# ---------- Load data ----------
dlg_paths <- list.files(INDIR, pattern = "^dialogs_flat_.*\\.csv$", full.names = TRUE)
if (!length(dlg_paths)) {
  stop("No dialogs_flat_*.csv found under: ", INDIR)
}
msg("Reading ", length(dlg_paths), " dialog files…")
dialogs <- safe_read(dlg_paths)

energy_paths <- list.files(INDIR, pattern = "^batch_energy_convo_.*\\.csv$", full.names = TRUE)
energy <- if (length(energy_paths)) {
  msg("Reading ", length(energy_paths), " energy files…")
  safe_read(energy_paths)
} else tibble()

# ---------- Basic hygiene ----------
req_cols <- c("conversation_id","cluster","recipe_title","step_id","role","text","intent","codes","model_user","model_agent")
missing <- setdiff(req_cols, names(dialogs))
if (length(missing)) stop("dialogs_flat missing columns: ", paste(missing, collapse = ", "))

dialogs <- dialogs %>%
  mutate(
    role = tolower(as.character(role)),
    step_id = as.integer(step_id),
    codes = codes %||% "",
    code_vec = strsplit(ifelse(is.na(codes), "", codes), ",") %>% purrr::map(~trimws(.x[.x!=""]))
  )

# ---------- Turn-level features ----------
turn_feats <- dialogs %>%
  mutate(
    word_count = stringr::str_count(text %||% "", "\\S+"),
    char_count = nchar(text %||% ""),
    has_qmark  = grepl("\\?", text %||% ""),
    apology_flag = purrr::map_lgl(code_vec, ~ any(grepl("^apology", tolower(.x))))
  )

# ---------- Per-step JSD (user vs agent codes) ----------
per_step_jsd <- turn_feats %>%
  select(conversation_id, cluster, recipe_title, model_user, model_agent, step_id, role, code_vec) %>%
  tidyr::unnest(code_vec, keep_empty = TRUE) %>%
  mutate(code_norm = tolower(trimws(code_vec))) %>%
  filter(!is.na(role)) %>%
  group_by(conversation_id, step_id, role, code_norm, cluster, recipe_title, model_user, model_agent) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = role, values_from = p, values_fill = 0) %>%
  group_by(conversation_id, step_id, cluster, recipe_title, model_user, model_agent) %>%
  summarise(
    jsd_codes = js_div(tibble::deframe(cur_data() %>% select(code_norm, user) %>% tidyr::replace_na(list(user=0))),
                       tibble::deframe(cur_data() %>% select(code_norm, agent) %>% tidyr::replace_na(list(agent=0)))),
    .groups = "drop"
  )

# ---------- Lexical diversity per role (per conversation) ----------
lex_div <- turn_feats %>%
  group_by(conversation_id, cluster, recipe_title, model_user, model_agent, role) %>%
  summarise(
    tokens = list(unlist(lapply(text, tokenize))),
    .groups = "drop"
  ) %>%
  mutate(
    ttr = purrr::map_dbl(tokens, ~ if (!length(.x)) NA_real_ else length(unique(.x))/length(.x)),
    d1  = purrr::map_dbl(tokens, ~ distinct_n(.x, 1)),
    d2  = purrr::map_dbl(tokens, ~ distinct_n(.x, 2)),
    mtld_val = purrr::map_dbl(tokens, mtld),
    token_count = purrr::map_int(tokens, length)
  ) %>%
  select(-tokens)

# ---------- Conversation-level summaries ----------
conv_summary <- turn_feats %>%
  group_by(conversation_id, cluster, recipe_title, model_user, model_agent) %>%
  summarise(
    user_turns  = sum(role == "user"),
    agent_turns = sum(role == "agent"),
    mean_user_len_words  = mean(word_count[role=="user"],  na.rm = TRUE),
    mean_agent_len_words = mean(word_count[role=="agent"], na.rm = TRUE),
    ask_rate = mean(grepl("^ASK_", intent[role=="user"]) | has_qmark[role=="user"], na.rm = TRUE),
    proceed_rate = mean(intent[role=="user"] == "PROCEED", na.rm = TRUE),
    apology_rate_agent = mean(apology_flag[role=="agent"], na.rm = TRUE),
    .groups = "drop"
  )

# Attach energy if present
if (nrow(energy)) {
  energy_small <- energy %>%
    select(conversation_id, energy_Wh_total, mean_W_mean, peak_W_max) %>%
    distinct()
  conv_summary <- conv_summary %>% left_join(energy_small, by = "conversation_id")
}

# ---------- Step-level summaries (questions etc.) ----------
step_summary <- turn_feats %>%
  group_by(conversation_id, cluster, recipe_title, model_user, model_agent, step_id) %>%
  summarise(
    user_asks  = sum(role=="user" & (grepl("^ASK_", intent) | has_qmark), na.rm = TRUE),
    proceeded  = any(role=="user" & intent=="PROCEED"),
    agent_replied = any(role=="agent"),
    agent_apologised = any(role=="agent" & apology_flag),
    mean_agent_len = mean(word_count[role=="agent"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(per_step_jsd, by = c("conversation_id","cluster","recipe_title","model_user","model_agent","step_id"))

# ---------- Aggregates by cluster × recipe × model ----------
by_cluster_recipe <- conv_summary %>%
  group_by(cluster, recipe_title, model_user, model_agent) %>%
  summarise(
    convs = dplyr::n(),
    mean_user_len  = mean(mean_user_len_words, na.rm = TRUE),
    mean_agent_len = mean(mean_agent_len_words, na.rm = TRUE),
    mean_ask_rate  = mean(ask_rate, na.rm = TRUE),
    mean_proceed   = mean(proceed_rate, na.rm = TRUE),
    mean_apology   = mean(apology_rate_agent, na.rm = TRUE),
    mean_energy_Wh = mean(energy_Wh_total, na.rm = TRUE),
    .groups = "drop"
  )

jsd_by_cluster <- step_summary %>%
  group_by(cluster, recipe_title, model_user, model_agent) %>%
  summarise(
    steps = dplyr::n(),
    jsd_mean = mean(jsd_codes, na.rm = TRUE),
    jsd_median = median(jsd_codes, na.rm = TRUE),
    jsd_na_prop = mean(is.na(jsd_codes)),
    .groups = "drop"
  )

lex_by_cluster <- lex_div %>%
  group_by(cluster, recipe_title, model_user, model_agent, role) %>%
  summarise(
    n_convs = dplyr::n(),
    mean_TTR = mean(ttr, na.rm = TRUE),
    mean_D1  = mean(d1,  na.rm = TRUE),
    mean_D2  = mean(d2,  na.rm = TRUE),
    mean_MTLD = mean(mtld_val, na.rm = TRUE),
    .groups = "drop"
  )

# ---------- Save ----------
ts <- stamp()
readr::write_csv(turn_feats,         file.path(OUTDIR, paste0("turn_features_", ts, ".csv")))
readr::write_csv(step_summary,       file.path(OUTDIR, paste0("step_summary_", ts, ".csv")))
readr::write_csv(per_step_jsd,       file.path(OUTDIR, paste0("per_step_jsd_", ts, ".csv")))
readr::write_csv(conv_summary,       file.path(OUTDIR, paste0("conversation_summary_", ts, ".csv")))
readr::write_csv(by_cluster_recipe,  file.path(OUTDIR, paste0("cluster_recipe_summary_", ts, ".csv")))
readr::write_csv(jsd_by_cluster,     file.path(OUTDIR, paste0("jsd_by_cluster_", ts, ".csv")))
readr::write_csv(lex_div,            file.path(OUTDIR, paste0("lexical_diversity_conversation_", ts, ".csv")))
readr::write_csv(lex_by_cluster,     file.path(OUTDIR, paste0("lexical_diversity_cluster_", ts, ".csv")))

msg("Done. Outputs in: ", normalizePath(OUTDIR))
