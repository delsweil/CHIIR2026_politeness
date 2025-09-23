############################################################
# H1–H3 Analysis Pipeline (Slim, tidyverse-core only)
#
# Keep ggplot2/dplyr etc., but avoid the heavy tidyverse meta-package.
# Packages used: dplyr, readr, tidyr, stringr, purrr, ggplot2, forcats,
#                lme4, lmerTest, MASS, emmeans, broom, broom.mixed
# (No tidyverse meta; avoids pulling ragg/systemfonts/googledrive/...)
#
# Data layers
#   A) step-level nuggets: Final_study_*_agent_nuggets_by_step.csv
#   B) turn-level logs  : dialogs_flat_*.csv under Final_study_*/line_*/
# Merge keys: cluster + conversation_id + recipe_title + step_id
#
# Outputs
#   ./outputs/tables/*.csv  (descriptives, ANOVAs, EMMs, model summaries)
#   ./outputs/plots/*.png   (violins/boxplots, scatter+loess)
#
# Author: (you)    Date: 2025-09-23
############################################################

# ----------------------------
# 0) Setup & packages (core set only)
# ----------------------------
# Runtime toggles
VERBOSE <- TRUE         # print progress messages
FAST_MODE <- TRUE       # quick models (Poisson or faster NB); set FALSE for final
PLOT_SAMPLE_N <- 50000  # max points per scatter plot (downsample for speed)
CACHE_PATH <- "final_outputs/cache/an_step.rds"


required_pkgs <- c(
  # core tidyverse pieces individually (NOT 'tidyverse')
  "dplyr", "readr", "tidyr", "stringr", "purrr", "ggplot2", "forcats",
  # modelling & helpers
  "lme4", "lmerTest", "MASS", "emmeans", "broom", "broom.mixed"
)

install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, dependencies = c("Depends","Imports"))
}
install_if_missing(required_pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(forcats)
  library(lme4)
  library(lmerTest)
  library(MASS)
  library(emmeans)
  library(broom)
  library(broom.mixed)
})

set.seed(1234)
options(dplyr.summarise.inform = FALSE)

# ----------------------------
# 1) Paths (edit as needed)
# ----------------------------
BASE_DIR   <- "/home/david/sim_runs/final_20250909_212121"   # contains Final_study_1 … Final_study_20
NUGGET_DIR <- "./nugget_outputs"                             # set to NULL to auto-search under BASE_DIR

dir.create("final_outputs/plots", recursive = TRUE, showWarnings = FALSE)
.dir_tables <- "final_outputs/tables"; dir.create(.dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create("final_outputs/cache", recursive = TRUE, showWarnings = FALSE)


# ----------------------------
# 2) Load nugget step files (A)
# ----------------------------
find_nugget_files <- function() {
  f <- character(0)
  if (!is.null(NUGGET_DIR) && dir.exists(NUGGET_DIR)) {
    f <- list.files(NUGGET_DIR, pattern = "^Final_study_\\d+_agent_nuggets_by_step\\.csv$", full.names = TRUE)
  }
  if (!length(f)) {
    f <- list.files(BASE_DIR, pattern = "Final_study_\\d+_agent_nuggets_by_step\\.csv$", full.names = TRUE, recursive = TRUE)
  }
  f
}

if (VERBOSE) message("[1/7] Scanning for nugget step files…")
nug_files <- find_nugget_files()
if (!length(nug_files)) stop("Could not find any *agent_nuggets_by_step.csv files. Check NUGGET_DIR/BASE_DIR.")

read_nug <- function(path) readr::read_csv(path, show_col_types = FALSE) %>% mutate(source_file = basename(path))

if (VERBOSE) message("[2/7] Reading ", length(nug_files), " nugget files…")
t0 <- Sys.time()
nug_A <- purrr::map_dfr(nug_files, read_nug)
if (VERBOSE) message("…done in ", round(difftime(Sys.time(), t0, units="secs"),1), "s (", scales::comma(nrow(nug_A)), " rows)")

# Harmonize columns across possible names
# NOTE: coalesce on CHARACTER first (avoids type-mixing errors), then parse
pick_char <- function(d, ...) {
  cands <- c(...)
  out <- rep(NA_character_, nrow(d))
  for (nm in cands) if (nm %in% names(d)) out <- dplyr::coalesce(out, as.character(d[[nm]]))
  out
}
pick_num <- function(d, ...) suppressWarnings(as.numeric(pick_char(d, ...)))
pick_int <- function(d, ...) suppressWarnings(as.integer(pick_char(d, ...)))

nug_A <- nug_A %>% mutate(
  cluster         = pick_char(., "cluster","politeness_cluster","user_cluster","profile"),
  conversation_id = pick_char(., "conversation_id","conv_id","dialogue_id","conversation"),
  recipe_title    = pick_char(., "recipe_title","recipe","recipe_name"),
  step_id         = pick_int(.,  "step_id","step","step_index","recipe_step"),
  agent_text      = pick_char(., "agent_text","assistant_text","agent_response","assistant_message","text"),
  agent_turns     = pick_int(.,  "agent_turns","n_agent_turns"),
  model_agent     = pick_char(., "model_agent","agent_model","llm_agent"),
  nugget_count    = pick_num(.,  "nugget_count","nuggets","num_nuggets","nuggets_sum")
) %>% mutate(
  length_words_from_agent_text = ifelse(!is.na(agent_text) & nzchar(agent_text),
                                        stringr::str_count(stringr::str_squish(agent_text), "\\S+"), NA)
)

step_A <- nug_A %>% transmute(
  cluster = factor(cluster),
  conversation_id = factor(conversation_id),
  recipe_title = factor(recipe_title),
  step_id = as.integer(step_id),
  agent_model_A = factor(model_agent),
  agent_turns_A = as.integer(agent_turns),
  agent_text,
  length_words_A = as.numeric(length_words_from_agent_text),
  nugget_count = as.numeric(nugget_count)
)

# ----------------------------
# 3) Load dialogs_flat (B) and aggregate to step level
# ----------------------------
if (VERBOSE) message("[3/7] Scanning for dialogs_flat files…")
flat_files <- list.files(BASE_DIR, pattern = "dialogs_flat_.*\\.csv$", full.names = TRUE, recursive = TRUE)
if (VERBOSE) message("Found ", length(flat_files), " dialogs_flat files")
if (!length(flat_files)) stop("No dialogs_flat_*.csv files found under BASE_DIR.")

read_flat <- function(path) readr::read_csv(path, show_col_types = FALSE) %>% mutate(source_file_flat = basename(path))
# Parallel read for speed on Linux
if (VERBOSE) message("[4/7] Reading dialogs_flat files (this can take a while)…")
t0 <- Sys.time()
if (.Platform$OS.type == "unix" && length(flat_files) > 4) {
  flat_list <- parallel::mclapply(flat_files, read_flat, mc.cores = max(1, parallel::detectCores() - 1))
  flat_B <- dplyr::bind_rows(flat_list)
} else {
  flat_B <- purrr::map_dfr(flat_files, read_flat)
}
if (VERBOSE) message("…done in ", round(difftime(Sys.time(), t0, units="secs"),1), "s (", scales::comma(nrow(flat_B)), " rows)")

# Choose energy column if present
energy_col <- intersect(names(flat_B), c("energy_Wh","energy_wh","energy.wh"))
energy_col <- if (length(energy_col)) energy_col[1] else NA_character_

flat_B <- flat_B %>% mutate(
  role = tolower(as.character(role)),
  is_agent_turn = !is.na(role) & role != "user",
  text = as.character(text),
  turn_words = ifelse(!is.na(text) & nzchar(text), stringr::str_count(stringr::str_squish(text), "\\S+"), 0),
  energy_num = if (!is.na(energy_col)) suppressWarnings(as.numeric(.data[[energy_col]])) else NA_real_
)

step_B <- flat_B %>%
  filter(is_agent_turn) %>%
  group_by(cluster, conversation_id, recipe_title, step_id) %>%
  summarise(
    agent_model_B = dplyr::first(na.omit(model_agent)),
    agent_turns_B = dplyr::n(),
    words_B = sum(turn_words, na.rm = TRUE),
    energy_wh_B = sum(energy_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cluster = factor(cluster),
    conversation_id = factor(conversation_id),
    recipe_title = factor(recipe_title),
    step_id = as.integer(step_id)
  )

# ----------------------------
# 4) Merge A + B and derive analysis variables
# ----------------------------
by_keys <- c("cluster","conversation_id","recipe_title","step_id")
if (VERBOSE) message("[5/7] Merging step-level nuggets with aggregated agent turns…")
merged <- step_A %>% full_join(step_B, by = by_keys)
if (VERBOSE) message("Merged rows: ", scales::comma(nrow(merged)))

an <- merged %>%
  mutate(
    # coalesce to character, then make a factor (robust across forcats versions)
    agent_model = factor(dplyr::coalesce(
      as.character(agent_model_A),
      as.character(agent_model_B)
    )),
    
    length_words = dplyr::coalesce(length_words_A, as.numeric(words_B)),
    agent_turns  = dplyr::coalesce(agent_turns_A, as.integer(agent_turns_B)),
    energy_wh    = suppressWarnings(as.numeric(energy_wh_B)),
    recipe       = recipe_title
  ) %>%
  mutate(
    has_words   = is.finite(length_words) & length_words >= 0,
    has_nuggets = is.finite(nugget_count) & nugget_count >= 0,
    has_energy  = is.finite(energy_wh)   & energy_wh > 0,
    nuggets_per_100w = ifelse(has_words & has_nuggets & length_words > 0,
                              100 * nugget_count / length_words, NA_real_),
    nuggets_per_wh   = ifelse(has_energy & has_nuggets,
                              nugget_count / energy_wh, NA_real_)
  ) %>%
  dplyr::select(
    cluster, agent_model, recipe, conversation_id, step_id,
    length_words, nugget_count, energy_wh,
    nuggets_per_100w, nuggets_per_wh,
    has_words, has_nuggets, has_energy
  )


# Merge diagnostics
merge_diag <- merged %>% summarise(
  n_total = dplyr::n(),
  n_from_A_only = sum(is.na(agent_turns_B)),
  n_from_B_only = sum(is.na(agent_text)),
  n_with_words = sum(is.finite(dplyr::coalesce(length_words_A, as.numeric(words_B))) & dplyr::coalesce(length_words_A, as.numeric(words_B)) >= 0, na.rm=TRUE),
  n_with_nuggets = sum(is.finite(nugget_count) & nugget_count >= 0, na.rm=TRUE),
  n_with_energy = sum(is.finite(energy_wh_B) & energy_wh_B > 0, na.rm=TRUE)
)
readr::write_csv(merge_diag, file.path(.dir_tables, "00_merge_diagnostics.csv"))

# Cache merged step-level analysis frame to skip re-reading next time
if (VERBOSE) message("[6/7] Caching merged step-level data to ", CACHE_PATH)
suppressWarnings(saveRDS(an, CACHE_PATH))

# ----------------------------
# 5) Descriptives
# ----------------------------
if (VERBOSE) message("[7/7] Descriptives & plots…")
cell_counts <- an %>% count(cluster, agent_model, recipe, name = "n_steps") %>% arrange(cluster, agent_model, recipe)
readr::write_csv(cell_counts, file.path(.dir_tables, "01_cell_counts.csv"))

na_table <- an %>% summarise(
  n = dplyr::n(),
  n_na_words = sum(!has_words),
  n_na_nuggets = sum(!has_nuggets),
  n_na_energy = sum(!has_energy),
  pct_na_words = mean(!has_words),
  pct_na_nuggets = mean(!has_nuggets),
  pct_na_energy = mean(!has_energy)
)
readr::write_csv(na_table, file.path(.dir_tables, "02_missingness_overview.csv"))

# Descriptive stats per Cluster × Model
by_cm <- an %>% group_by(cluster, agent_model) %>% summarise(
  n = dplyr::n(),
  words_mean = mean(length_words, na.rm=TRUE), words_sd = sd(length_words, na.rm=TRUE), words_median = median(length_words, na.rm=TRUE),
  nuggets_mean = mean(nugget_count, na.rm=TRUE), nuggets_sd = sd(nugget_count, na.rm=TRUE), nuggets_median = median(nugget_count, na.rm=TRUE),
  energy_mean = mean(energy_wh, na.rm=TRUE), energy_sd = sd(energy_wh, na.rm=TRUE), energy_median = median(energy_wh, na.rm=TRUE),
  eff100_mean = mean(nuggets_per_100w, na.rm=TRUE), eff100_sd = sd(nuggets_per_100w, na.rm=TRUE),
  effWh_mean = mean(nuggets_per_wh, na.rm=TRUE), effWh_sd = sd(nuggets_per_wh, na.rm=TRUE),
  .groups = "drop"
)
readr::write_csv(by_cm, file.path(.dir_tables, "03_descriptives_by_cluster_model.csv"))

# ----------------------------
# 6) Plots (ggplot2; loess smooth)
# ----------------------------

p_words <- an %>% filter(has_words) %>%
  ggplot(aes(x = cluster, y = length_words)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  labs(title = "Response length (words) by Cluster × AgentModel",
       x = "Cluster (User Politeness Profile)", y = "Words per step") +
  theme_bw()

ggsave("final_outputs/plots/words_by_cluster_model.png", p_words, width = 12, height = 7, dpi = 180)

p_nuggets <- an %>% filter(has_nuggets) %>%
  ggplot(aes(x = cluster, y = nugget_count)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  labs(title = "Nuggets per step by Cluster × AgentModel",
       x = "Cluster", y = "Nuggets (count)") +
  theme_bw()

ggsave("final_outputs/plots/nuggets_by_cluster_model.png", p_nuggets, width = 12, height = 7, dpi = 180)

p_eff100 <- an %>% filter(has_words, has_nuggets) %>%
  ggplot(aes(x = cluster, y = nuggets_per_100w)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  labs(title = "Efficiency: nuggets per 100 words",
       x = "Cluster", y = "Nuggets per 100 words") +
  theme_bw()

ggsave("final_outputs/plots/efficiency_per_100w_by_cluster_model.png", p_eff100, width = 12, height = 7, dpi = 180)

plot_df_words <- an %>% filter(has_words, has_nuggets)
if (nrow(plot_df_words) > PLOT_SAMPLE_N) plot_df_words <- dplyr::slice_sample(plot_df_words, n = PLOT_SAMPLE_N)

p_scatter_words <- plot_df_words %>%
  ggplot(aes(x = length_words, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free") +
  labs(title = "Nuggets vs. Words by AgentModel (colored by Cluster)", x = "Words", y = "Nuggets") +
  theme_bw()

ggsave("final_outputs/plots/scatter_nuggets_vs_words.png", p_scatter_words, width = 12, height = 7, dpi = 180)

plot_df_energy <- an %>% filter(has_energy, has_nuggets)
if (nrow(plot_df_energy) > PLOT_SAMPLE_N) plot_df_energy <- dplyr::slice_sample(plot_df_energy, n = PLOT_SAMPLE_N)

p_scatter_energy <- plot_df_energy %>%
  ggplot(aes(x = energy_wh, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free") +
  labs(title = "Nuggets vs. Energy (Wh) by AgentModel",
       x = "Energy (Wh)", y = "Nuggets") +
  theme_bw()

ggsave("final_outputs/plots/scatter_nuggets_vs_energy.png", p_scatter_energy, width = 12, height = 7, dpi = 180)

# ----------------------------
# 7) Models for H1–H3 (lme4/lmerTest/MASS)
# ----------------------------
# Simple overdispersion metric for GLMMs
overdisp_ratio <- function(model) {
  rp <- resid(model, type = "pearson")
  sum(rp^2, na.rm = TRUE) / df.residual(model)
}

AN <- an %>% filter(!is.na(cluster), !is.na(agent_model), !is.na(recipe), !is.na(conversation_id))

# H1: Length (log scale)
H1_df <- AN %>% filter(has_words) %>% mutate(log_words = log1p(length_words))
if (nrow(H1_df) < 10) stop("Too few rows with words for H1.")

fit_H1 <- lmer(log_words ~ cluster * agent_model + recipe + (1 | conversation_id), data = H1_df)
H1_anova <- anova(fit_H1)               # Satterthwaite (lmerTest)
H1_emm   <- emmeans(fit_H1, ~ cluster | agent_model)
H1_pairs <- contrast(H1_emm, method = "pairwise", adjust = "tukey")

readr::write_csv(broom::tidy(H1_anova), file.path(.dir_tables, "H1_anova.csv"))
readr::write_csv(as.data.frame(summary(H1_emm)), file.path(.dir_tables, "H1_emmeans.csv"))
readr::write_csv(as.data.frame(H1_pairs), file.path(.dir_tables, "H1_pairs_tukey.csv"))
readr::write_csv(broom.mixed::tidy(fit_H1, effects = "fixed", conf.int = TRUE), file.path(.dir_tables, "H1_fixed_effects.csv"))

# H2: Nuggets (NB GLMM via lme4::glmer.nb)
H2_df <- AN %>% filter(has_nuggets)
if (nrow(H2_df) < 10) stop("Too few rows with nuggets for H2.")

fit_H2 <- if (FAST_MODE) {
  # Quick Poisson first
  glmer(nugget_count ~ cluster * agent_model + recipe + (1 | conversation_id),
        family = poisson, data = H2_df,
        control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
} else {
  glmer.nb(nugget_count ~ cluster * agent_model + recipe + (1 | conversation_id),
           data = H2_df, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
}nugget_count ~ cluster * agent_model + recipe + (1 | conversation_id), data = H2_df)
H2_ovr <- overdisp_ratio(fit_H2)
readr::write_csv(broom.mixed::tidy(fit_H2, effects = "fixed", conf.int = TRUE, exponentiate = TRUE), file.path(.dir_tables, "H2_fixed_effects_rate_ratios.csv"))
readr::write_csv(tibble::tibble(overdispersion_ratio = H2_ovr), file.path(.dir_tables, "H2_overdispersion_ratio.csv"))

H2_emm <- emmeans(fit_H2, ~ cluster | agent_model, type = "response")
H2_pairs <- contrast(H2_emm, method = "pairwise", adjust = "tukey")
readr::write_csv(as.data.frame(summary(H2_emm)), file.path(.dir_tables, "H2_emmeans_resp.csv"))
readr::write_csv(as.data.frame(H2_pairs), file.path(.dir_tables, "H2_pairs_tukey_resp.csv"))

# H3a: Efficiency per word (offset log(words))
H3a_df <- AN %>% filter(has_nuggets, has_words, length_words > 0)
if (nrow(H3a_df) < 10) stop("Too few rows with words+nuggets for H3a.")

fit_H3a <- if (FAST_MODE) {
  glmer(nugget_count ~ cluster * agent_model + recipe + offset(log(pmax(length_words, 1))) + (1 | conversation_id),
        family = poisson, data = H3a_df,
        control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
} else {
  glmer.nb(nugget_count ~ cluster * agent_model + recipe + offset(log(pmax(length_words, 1))) + (1 | conversation_id),
           data = H3a_df, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
}nugget_count ~ cluster * agent_model + recipe + offset(log(pmax(length_words, 1))) + (1 | conversation_id), data = H3a_df)
H3a_ovr <- overdisp_ratio(fit_H3a)
readr::write_csv(broom.mixed::tidy(fit_H3a, effects = "fixed", conf.int = TRUE, exponentiate = TRUE), file.path(.dir_tables, "H3a_fixed_effects_rate_ratios.csv"))
readr::write_csv(tibble::tibble(overdispersion_ratio = H3a_ovr), file.path(.dir_tables, "H3a_overdispersion_ratio.csv"))

H3a_emm <- emmeans(fit_H3a, ~ cluster | agent_model, type = "response")  # returns nuggets/word
H3a_pairs <- contrast(H3a_emm, method = "pairwise", adjust = "tukey")
readr::write_csv(as.data.frame(summary(H3a_emm)), file.path(.dir_tables, "H3a_emmeans_rate_per_word.csv"))
readr::write_csv(as.data.frame(H3a_pairs), file.path(.dir_tables, "H3a_pairs_tukey_rate_per_word.csv"))

# H3b: Efficiency per Wh (offset log(Wh))
H3b_df <- AN %>% filter(has_nuggets, has_energy)
if (nrow(H3b_df) >= 10) {
  fit_H3b <- if (FAST_MODE) {
    glmer(nugget_count ~ cluster * agent_model + recipe + offset(log(energy_wh)) + (1 | conversation_id),
          family = poisson, data = H3b_df,
          control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
  } else {
    glmer.nb(nugget_count ~ cluster * agent_model + recipe + offset(log(energy_wh)) + (1 | conversation_id),
             data = H3b_df, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
  }nugget_count ~ cluster * agent_model + recipe + offset(log(energy_wh)) + (1 | conversation_id), data = H3b_df)
H3b_ovr <- overdisp_ratio(fit_H3b)
readr::write_csv(broom.mixed::tidy(fit_H3b, effects = "fixed", conf.int = TRUE, exponentiate = TRUE), file.path(.dir_tables, "H3b_fixed_effects_rate_ratios.csv"))
readr::write_csv(tibble::tibble(overdispersion_ratio = H3b_ovr), file.path(.dir_tables, "H3b_overdispersion_ratio.csv"))

H3b_emm <- emmeans(fit_H3b, ~ cluster | agent_model, type = "response")  # nuggets/Wh
H3b_pairs <- contrast(H3b_emm, method = "pairwise", adjust = "tukey")
readr::write_csv(as.data.frame(summary(H3b_emm)), file.path(.dir_tables, "H3b_emmeans_rate_per_Wh.csv"))
readr::write_csv(as.data.frame(H3b_pairs), file.path(.dir_tables, "H3b_pairs_tukey_rate_per_Wh.csv"))
} else {
  warning("Too few rows with energy for H3b – skipping.")
}

# ----------------------------
# 8) Session info
# ----------------------------
writeLines(capture.output(sessionInfo()), con = file.path(.dir_tables, "sessionInfo.txt"))

message("Done. Tables → ./final_outputs/tables, Plots → ./final_outputs/plots")
