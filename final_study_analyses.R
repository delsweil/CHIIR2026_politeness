############################################################
# H1–H3 Analysis Pipeline: Politeness → Length, Nuggets, Efficiency
# Data sources (two-layer):
#   (A) Nugget step files          : Final_study_*_agent_nuggets_by_step.csv
#   (B) Turn-level dialog logs     : dialogs_flat_*.csv (nested in Final_study_*/line_*/)
#
# This script merges (A) + (B) to build a step-level dataset with
#   - length_words (agent replies per step)
#   - nugget_count (from nugget annotator)
#   - energy_Wh (sum over agent turns in that step)
# and then runs descriptives → visuals → models for H1–H3.
#
# Author: (you)
# Date: 2025-09-23
############################################################

# ----------------------------
# 0) Setup & packages
# ----------------------------
required_pkgs <- c(
  "tidyverse", "janitor", "fs", "glue", "scales", "patchwork",
  "lme4", "lmerTest", "glmmTMB", "emmeans", "DHARMa", "performance",
  "broom", "broom.mixed", "MuMIn", "clubSandwich", "mgcv"
)

install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
}
install_if_missing(required_pkgs)

library(tidyverse)
library(janitor)
library(fs)
library(glue)
library(scales)
library(patchwork)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(performance)
library(broom)
library(broom.mixed)
library(MuMIn)
library(clubSandwich)
library(mgcv)

set.seed(1234)
options(dplyr.summarise.inform = FALSE)

# ----------------------------
# 1) Paths
# ----------------------------
# Set your base directory that contains Final_study_1 … Final_study_20
BASE_DIR   <- "/home/david/sim_runs/final_20250909_212121"
# Directory with the step-level nugget outputs (20 batch files). If unknown,
# leave as NULL and the script will try to find them under BASE_DIR.
NUGGET_DIR <- "./nugget_outputs"  # set to NULL to auto-search under BASE_DIR

# Output folders
fs::dir_create("outputs/plots")
fs::dir_create("outputs/tables")

# ----------------------------
# 2) Load nugget step files (A)
# ----------------------------
find_nugget_files <- function() {
  # Priority 1: explicit NUGGET_DIR
  if (!is.null(NUGGET_DIR) && dir_exists(NUGGET_DIR)) {
    f <- dir_ls(NUGGET_DIR, regexp = "Final_study_\d+_agent_nuggets_by_step\.csv$", type = "file")
    if (length(f)) return(f)
  }
  # Priority 2: search under BASE_DIR
  dir_ls(BASE_DIR, recurse = TRUE, regexp = "Final_study_\d+_agent_nuggets_by_step\.csv$", type = "file")
}

nugget_files <- find_nugget_files()
if (!length(nugget_files)) stop("Could not find any *agent_nuggets_by_step.csv files. Set NUGGET_DIR or check BASE_DIR.")

read_nug <- function(path) {
  suppressMessages(readr::read_csv(path, show_col_types = FALSE)) %>%
    clean_names() %>% mutate(source_file = basename(path))
}

nug_A <- purrr::map_dfr(nugget_files, read_nug)

# Harmonize column names from your example schema
# Expected columns (after clean_names):
# cluster, conversation_id, recipe_title, step_id, agent_text, agent_turns,
# model_agent, step_description, nugget_count, nuggets, ok, annot_notes

# Coalesce possible variants
coalesce_cols <- function(d, ...) {
  cands <- c(...)
  vals <- rep(NA_character_, nrow(d))
  for (nm in cands) if (nm %in% names(d)) vals <- coalesce(vals, as.character(d[[nm]]))
  vals
}

nug_A <- nug_A %>% mutate(
  cluster         = coalesce_cols(., "cluster", "politeness_cluster", "user_cluster"),
  conversation_id = coalesce_cols(., "conversation_id", "conv_id"),
  recipe_title    = coalesce_cols(., "recipe_title", "recipe", "recipe_name"),
  step_id         = suppressWarnings(as.integer(coalesce_cols(., "step_id", "step", "step_index"))),
  agent_text      = coalesce_cols(., "agent_text", "assistant_text", "agent_response"),
  agent_turns     = suppressWarnings(as.integer(coalesce_cols(., "agent_turns", "n_agent_turns"))),
  model_agent     = coalesce_cols(., "model_agent", "agent_model", "llm_agent"),
  nugget_count    = suppressWarnings(as.numeric(coalesce_cols(., "nugget_count", "nuggets", "num_nuggets")))
) %>%
  mutate(length_words_from_agent_text = ifelse(!is.na(agent_text) & nzchar(agent_text),
                                               stringr::str_count(stringr::str_squish(agent_text), "\S+"), NA_integer_))

# Minimal step frame from (A)
step_A <- nug_A %>% select(cluster, conversation_id, recipe_title, step_id,
                           model_agent, agent_turns, agent_text,
                           length_words_from_agent_text, nugget_count, source_file) %>%
  mutate(
    cluster = as.factor(cluster),
    conversation_id = as.factor(conversation_id),
    recipe = as.factor(recipe_title),
    step_id = as.integer(step_id),
    agent_model = as.factor(model_agent)
  )

# ----------------------------
# 3) Load dialogs_flat (B) and aggregate to step level
# ----------------------------
message("Searching dialogs_flat files under ", BASE_DIR, " …")
flat_files <- dir_ls(BASE_DIR, recurse = TRUE, regexp = "dialogs_flat_.*\.csv$", type = "file")
if (!length(flat_files)) stop("No dialogs_flat_*.csv files found under BASE_DIR.")

read_flat <- function(path) {
  suppressMessages(readr::read_csv(path, show_col_types = FALSE)) %>%
    clean_names() %>% mutate(source_file_flat = basename(path))
}

flat_B <- purrr::map_dfr(flat_files, read_flat)

# Expected columns (after clean_names):
# step_id, need_type, turn, role, text, intent, codes, energy_wh, mean_w, peak_w,
# model_user, model_agent, recipe_title, event_idx, cluster, conversation_id, seed

# Mark agent turns (robust): anything that is not clearly user
flat_B <- flat_B %>% mutate(
  role = tolower(as.character(role)),
  is_agent_turn = !is.na(role) & role != "user"
)

# Token count per turn
flat_B <- flat_B %>% mutate(
  text = as.character(text),
  turn_words = ifelse(!is.na(text) & nzchar(text), stringr::str_count(stringr::str_squish(text), "\S+"), 0)
)

# Aggregate to step × conversation (agent-only)
step_B <- flat_B %>%
  filter(is_agent_turn) %>%
  group_by(cluster, conversation_id, recipe_title, step_id) %>%
  summarise(
    agent_model_flat = dplyr::first(na.omit(model_agent)),
    agent_turns_from_flat = dplyr::n(),
    words_from_flat = sum(turn_words, na.rm = TRUE),
    energy_wh_from_flat = sum(as.numeric(energy_wh), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cluster = as.factor(cluster),
    conversation_id = as.factor(conversation_id),
    recipe = as.factor(recipe_title),
    step_id = as.integer(step_id),
    agent_model_flat = as.factor(agent_model_flat)
  )

# ----------------------------
# 4) Merge (A) + (B) and derive analysis variables
# ----------------------------
by_keys <- c("cluster", "conversation_id", "recipe_title", "step_id")

merged <- step_A %>% full_join(step_B, by = by_keys)

# Resolve agent model, words, turns
merged <- merged %>% mutate(
  agent_model = forcats::fct_coalesce(agent_model, agent_model_flat),
  length_words = coalesce(as.numeric(length_words_from_agent_text), as.numeric(words_from_flat)),
  agent_turns = coalesce(as.integer(agent_turns), as.integer(agent_turns_from_flat)),
  energy_wh   = as.numeric(energy_wh_from_flat)
)

# Flags & derived metrics
merged <- merged %>% mutate(
  has_words   = is.finite(length_words) & length_words >= 0,
  has_nuggets = is.finite(nugget_count) & nugget_count >= 0,
  has_energy  = is.finite(energy_wh)   & energy_wh > 0,
  nuggets_per_100w = ifelse(has_words & has_nuggets & length_words > 0, 100 * nugget_count / length_words, NA_real_),
  nuggets_per_wh   = ifelse(has_energy & has_nuggets, nugget_count / energy_wh, NA_real_),
  recipe = as.factor(recipe_title)
)

# Diagnostics: merge coverage
merge_diag <- merged %>% summarise(
  n_total = n(),
  n_from_A_only = sum(is.na(agent_turns_from_flat)),
  n_from_B_only = sum(is.na(agent_text)),
  n_with_words = sum(has_words),
  n_with_nuggets = sum(has_nuggets),
  n_with_energy = sum(has_energy)
)
readr::write_csv(merge_diag, "outputs/tables/00_merge_diagnostics.csv")

# Keep analysis frame
df_step <- merged %>% select(
  cluster, agent_model, recipe, conversation_id, step_id,
  length_words, nugget_count, energy_wh,
  nuggets_per_100w, nuggets_per_wh,
  has_words, has_nuggets, has_energy
)

# ----------------------------
# 5) Descriptives & sanity tables
# ----------------------------
cell_counts <- df_step %>% count(cluster, agent_model, recipe, name = "n_steps") %>% arrange(cluster, agent_model, recipe)
readr::write_csv(cell_counts, "outputs/tables/01_cell_counts.csv")

na_table <- df_step %>% summarise(
  n = n(),
  n_na_words = sum(!has_words),
  n_na_nuggets = sum(!has_nuggets),
  n_na_energy = sum(!has_energy),
  pct_na_words = mean(!has_words),
  pct_na_nuggets = mean(!has_nuggets),
  pct_na_energy = mean(!has_energy)
)
readr::write_csv(na_table, "outputs/tables/02_missingness_overview.csv")

# Descriptive stats per Cluster × Model
desc_stats <- df_step %>%
  group_by(cluster, agent_model) %>%
  summarise(
    n = n(),
    words_mean = mean(length_words, na.rm=TRUE), words_sd = sd(length_words, na.rm=TRUE), words_median = median(length_words, na.rm=TRUE),
    nuggets_mean = mean(nugget_count, na.rm=TRUE), nuggets_sd = sd(nugget_count, na.rm=TRUE), nuggets_median = median(nugget_count, na.rm=TRUE),
    energy_mean = mean(energy_wh, na.rm=TRUE), energy_sd = sd(energy_wh, na.rm=TRUE), energy_median = median(energy_wh, na.rm=TRUE),
    eff100_mean = mean(nuggets_per_100w, na.rm=TRUE), eff100_sd = sd(nuggets_per_100w, na.rm=TRUE),
    effWh_mean = mean(nuggets_per_wh, na.rm=TRUE), effWh_sd = sd(nuggets_per_wh, na.rm=TRUE)
  ) %>% ungroup()
readr::write_csv(desc_stats, "outputs/tables/03_descriptives_by_cluster_model.csv")

# ----------------------------
# 6) Plots (saved to outputs/plots)
# ----------------------------
message("Saving plots to ./outputs/plots …")

gg_words <- df_step %>% filter(has_words) %>%
  ggplot(aes(x = cluster, y = length_words)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Response length (words) by Cluster × AgentModel",
       x = "Cluster (User Politeness Profile)", y = "Words per step") +
  theme_bw()

ggsave("outputs/plots/words_by_cluster_model.png", gg_words, width = 12, height = 7, dpi = 180)


gg_nuggets <- df_step %>% filter(has_nuggets) %>%
  ggplot(aes(x = cluster, y = nugget_count)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  labs(title = "Nuggets per step by Cluster × AgentModel",
       x = "Cluster", y = "Nuggets (count)") +
  theme_bw()

ggsave("outputs/plots/nuggets_by_cluster_model.png", gg_nuggets, width = 12, height = 7, dpi = 180)


gg_eff100 <- df_step %>% filter(has_words, has_nuggets) %>%
  ggplot(aes(x = cluster, y = nuggets_per_100w)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free_y") +
  labs(title = "Efficiency: nuggets per 100 words",
       x = "Cluster", y = "Nuggets per 100 words") +
  theme_bw()

ggsave("outputs/plots/efficiency_per_100w_by_cluster_model.png", gg_eff100, width = 12, height = 7, dpi = 180)


gg_scatter_words_nuggets <- df_step %>% filter(has_words, has_nuggets) %>%
  ggplot(aes(x = length_words, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Nuggets vs. Words by AgentModel (colored by Cluster)", x = "Words", y = "Nuggets") +
  theme_bw()

ggsave("outputs/plots/scatter_nuggets_vs_words.png", gg_scatter_words_nuggets, width = 12, height = 7, dpi = 180)


gg_scatter_energy <- df_step %>% filter(has_energy, has_nuggets) %>%
  ggplot(aes(x = energy_wh, y = nugget_count, color = cluster)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  facet_wrap(~ agent_model, ncol = 3, scales = "free") +
  labs(title = "Nuggets vs. Energy (Wh) by AgentModel",
       x = "Energy (Wh)", y = "Nuggets") +
  theme_bw()

ggsave("outputs/plots/scatter_nuggets_vs_energy.png", gg_scatter_energy, width = 12, height = 7, dpi = 180)

# ----------------------------
# 7) Models for H1–H3
# ----------------------------
message("Fitting models …")

an_df <- df_step %>% filter(!is.na(cluster), !is.na(agent_model), !is.na(recipe), !is.na(conversation_id))

# H1: Length (log scale)
H1_df <- an_df %>% filter(has_words) %>% mutate(log_words = log1p(length_words))
if (nrow(H1_df) < 10) stop("Too few rows with words for H1.")

fit_H1 <- lmer(log_words ~ cluster * agent_model + recipe + (1 | conversation_id), data = H1_df)
H1_anova <- anova(fit_H1)
H1_emm   <- emmeans(fit_H1, ~ cluster | agent_model)
H1_pairs <- contrast(H1_emm, method = "pairwise", adjust = "tukey")
H1_res   <- performance::check_model(fit_H1)

readr::write_csv(broom::tidy(H1_anova), "outputs/tables/H1_anova.csv")
readr::write_csv(as.data.frame(summary(H1_emm)), "outputs/tables/H1_emmeans.csv")
readr::write_csv(as.data.frame(H1_pairs), "outputs/tables/H1_pairs_tukey.csv")
readr::write_csv(broom.mixed::tidy(fit_H1, effects = "fixed", conf.int = TRUE), "outputs/tables/H1_fixed_effects.csv")

ggsave("outputs/plots/H1_check_model.png", H1_res, width = 10, height = 6, dpi = 150)

# H2: Nuggets (NB GLMM)
H2_df <- an_df %>% filter(has_nuggets)
if (nrow(H2_df) < 10) stop("Too few rows with nuggets for H2.")

fit_H2 <- glmmTMB(nugget_count ~ cluster * agent_model + recipe + (1 | conversation_id),
                  family = nbinom2, data = H2_df)
H2_anova <- anova(fit_H2)
H2_emm   <- emmeans(fit_H2, ~ cluster | agent_model, type = "response")
H2_pairs <- contrast(H2_emm, method = "pairwise", adjust = "tukey")
H2_simres <- DHARMa::simulateResiduals(fit_H2)

readr::write_csv(broom::tidy(H2_anova), "outputs/tables/H2_anova.csv")
readr::write_csv(as.data.frame(summary(H2_emm)), "outputs/tables/H2_emmeans_resp.csv")
readr::write_csv(as.data.frame(H2_pairs), "outputs/tables/H2_pairs_tukey_resp.csv")
readr::write_csv(broom.mixed::tidy(fit_H2, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                 "outputs/tables/H2_fixed_effects_rate_ratios.csv")

DH <- as.data.frame(H2_simres)
if (nrow(DH)) {
  p <- ggplot(DH, aes(x = scaledResiduals)) + geom_histogram(bins = 40) +
    labs(title = "H2 DHARMa residuals") + theme_bw()
  ggsave("outputs/plots/H2_DHARMa_residuals.png", p, width = 8, height = 5, dpi = 150)
}

# H3a: Efficiency per word (offset)
H3a_df <- an_df %>% filter(has_nuggets, has_words, length_words > 0)
if (nrow(H3a_df) < 10) stop("Too few rows with words+nuggets for H3a.")

fit_H3a <- glmmTMB(nugget_count ~ cluster * agent_model + recipe +
                     offset(log(pmax(length_words, 1))) + (1 | conversation_id),
                   family = nbinom2, data = H3a_df)
H3a_anova <- anova(fit_H3a)
H3a_emm   <- emmeans(fit_H3a, ~ cluster | agent_model, type = "response")
H3a_pairs <- contrast(H3a_emm, method = "pairwise", adjust = "tukey")

readr::write_csv(broom::tidy(H3a_anova), "outputs/tables/H3a_anova.csv")
readr::write_csv(as.data.frame(summary(H3a_emm)), "outputs/tables/H3a_emmeans_rate_per_word.csv")
readr::write_csv(as.data.frame(H3a_pairs), "outputs/tables/H3a_pairs_tukey_rate_per_word.csv")
readr::write_csv(broom.mixed::tidy(fit_H3a, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                 "outputs/tables/H3a_fixed_effects_rate_ratios.csv")

# H3b: Efficiency per Wh (offset)
H3b_df <- an_df %>% filter(has_nuggets, has_energy)
if (nrow(H3b_df) < 10) {
  warning("Too few rows with energy for H3b – skipping.")
} else {
  fit_H3b <- glmmTMB(nugget_count ~ cluster * agent_model + recipe +
                       offset(log(energy_wh)) + (1 | conversation_id),
                     family = nbinom2, data = H3b_df)
  H3b_anova <- anova(fit_H3b)
  H3b_emm   <- emmeans(fit_H3b, ~ cluster | agent_model, type = "response")
  H3b_pairs <- contrast(H3b_emm, method = "pairwise", adjust = "tukey")
  
  readr::write_csv(broom::tidy(H3b_anova), "outputs/tables/H3b_anova.csv")
  readr::write_csv(as.data.frame(summary(H3b_emm)), "outputs/tables/H3b_emmeans_rate_per_Wh.csv")
  readr::write_csv(as.data.frame(H3b_pairs), "outputs/tables/H3b_pairs_tukey_rate_per_Wh.csv")
  readr::write_csv(broom.mixed::tidy(fit_H3b, effects = "fixed", conf.int = TRUE, exponentiate = TRUE),
                   "outputs/tables/H3b_fixed_effects_rate_ratios.csv")
}

# ----------------------------
# 8) Session info
# ----------------------------
writeLines(capture.output(sessionInfo()), con = "outputs/tables/sessionInfo.txt")

message("Done. Tables → ./outputs/tables, Plots → ./outputs/plots")
