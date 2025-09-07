##############################################
# Power for H3 (nuggets per Wh) — Fast, single-core
# Conversation-level NB GLM with offset
# Wald scout + LRT confirm + MDE sweep
# Global progress bar, early stopping, robust NA handling
##############################################

suppressPackageStartupMessages({
  library(MASS)     # glm.nb
  library(car)      # Anova (Wald)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(pbapply)  # global progress bar
  library(readr)
})

set.seed(42)

# ========= USER PARAMETERS =========
REPS_LIST      <- c(150, 180, 200)  # N conversations per cluster–recipe–model cell
N_STEPS        <- 6                 # steps per conversation
NSIMS_SCOUT    <- 120               # sims per N for fast scouting (Wald)
NSIMS_CONFIRM  <- 400               # sims for final confirm (LRT)
TARGET_POWER   <- 0.80              # early-stop target
FUTILITY_BND   <- 0.70              # early-stop futility bound
CHECK_EVERY    <- 20                # checkpoint cadence (sims)
# (MDE sweep params set below)
# ===================================

# -------- Factors --------
clusters <- paste0("C", 1:5)
models   <- paste0("M", 1:3)
recipes  <- paste0("R", 1:6)

# -------- Parameters (pilot-based, conservative) --------
pars <- list(
  base_rate = 3.5,                 # nuggets per Wh baseline (orig scale)
  theta_nb  = 9,                   # overdispersion (for generation)
  icc_conversation = 0.10,         # conversation random intercept (in generator)
  
  eff_cluster = c(+0.10, +0.05, 0.00, -0.05, -0.10),
  eff_model   = c(0.00, +0.04, -0.04),
  
  # small interaction (±1–2%)
  eff_interaction = matrix(
    c(0.00, +0.02, -0.02,
      +0.01, 0.00, -0.01,
      0.00, -0.02, +0.02,
      -0.01, 0.00, +0.01,
      0.00, +0.01, -0.01),
    nrow = 5, byrow = TRUE,
    dimnames = list(paste0("C", 1:5), paste0("M", 1:3))
  ),
  
  # step-level distributions (log-normal)
  log_words_mu = log(50), log_words_sd = 0.5,
  log_Wh_mu    = log(0.7), log_Wh_sd    = 0.6
)

# -------- Helper: ICC → log-scale RE SD (approx) --------
var_from_icc <- function(icc){
  sigma2 <- (icc / (1 - icc)) * 1.0   # ICC ≈ sigma^2 / (sigma^2 + 1)
  sqrt(max(sigma2, 1e-8))
}
re_sd <- var_from_icc(pars$icc_conversation)

# -------- Simulator: generate steps, then aggregate to conversation --------
simulate_conversation_level <- function(reps_per_cell, n_steps, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  design <- expand.grid(cluster = clusters, model = models, recipe = recipes,
                        rep = seq_len(reps_per_cell), step = seq_len(n_steps),
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  design$conversation <- with(design, paste(cluster, model, recipe, rep, sep = "_"))
  
  n <- nrow(design)
  # Step-level offsets
  design$words <- round(exp(rnorm(n, pars$log_words_mu, pars$log_words_sd)))
  design$Wh    <- pmax(exp(rnorm(n, pars$log_Wh_mu, pars$log_Wh_sd)), 1e-6)
  
  # Fixed effects (orig scale)
  idx_c <- match(design$cluster, clusters)
  idx_m <- match(design$model,   models)
  eff_c <- pars$eff_cluster[idx_c]
  eff_m <- pars$eff_model[idx_m]
  eff_cm <- mapply(function(i, j) pars$eff_interaction[i, j], idx_c, idx_m)
  rate <- pmax(pars$base_rate * (1 + eff_c) * (1 + eff_m) * (1 + eff_cm), 1e-8)
  
  # Conversation random intercept on log scale
  conv_ids <- unique(design$conversation)
  conv_re  <- rnorm(length(conv_ids), 0, re_sd)
  names(conv_re) <- conv_ids
  design$eta_re  <- conv_re[design$conversation]
  
  # Step means and NB draws
  mu_step <- exp(log(rate) + log(design$Wh) + design$eta_re)
  nuggets_step <- rnbinom(n = length(mu_step), size = pars$theta_nb, mu = mu_step)
  
  dat <- tibble(
    cluster = factor(design$cluster, levels = clusters),
    model   = factor(design$model,   levels = models),
    recipe  = factor(design$recipe,  levels = recipes),
    conversation = factor(design$conversation),
    Wh    = design$Wh,
    nuggets = nuggets_step
  )
  
  # Aggregate to conversation: sums preserve conversation RE variability
  conv <- dat %>%
    group_by(cluster, model, recipe, conversation) %>%
    summarise(
      nuggets_sum = sum(nuggets),
      Wh_sum      = sum(Wh),
      .groups = "drop"
    )
  
  conv
}

# -------- Tests (conversation-level) --------
fit_H3_WALD_conv <- function(conv_df){
  m <- try(suppressWarnings(MASS::glm.nb(
    nuggets_sum ~ cluster * model + recipe + offset(log(Wh_sum)),
    data = conv_df, link = log
  )), silent = TRUE)
  if (inherits(m, "try-error")) return(NA_real_)
  tab <- try(suppressWarnings(car::Anova(m, type = 3)), silent = TRUE)
  if (inherits(tab, "try-error")) return(NA_real_)
  if("cluster:model" %in% rownames(tab)) suppressWarnings(as.numeric(tab["cluster:model", "Pr(>Chisq)"])) else NA_real_
}

fit_H3_LRT_conv <- function(conv_df){
  m_full <- try(suppressWarnings(MASS::glm.nb(
    nuggets_sum ~ cluster * model + recipe + offset(log(Wh_sum)),
    data = conv_df, link = log
  )), silent = TRUE)
  if (inherits(m_full, "try-error")) return(NA_real_)
  
  m_red  <- try(suppressWarnings(MASS::glm.nb(
    nuggets_sum ~ cluster + model + recipe + offset(log(Wh_sum)),
    data = conv_df, link = log
  )), silent = TRUE)
  if (inherits(m_red, "try-error")) return(NA_real_)
  
  lrt <- try(suppressWarnings(anova(m_red, m_full, test = "Chisq")), silent = TRUE)
  if (inherits(lrt, "try-error")) return(NA_real_)
  cc <- intersect(colnames(lrt), c("Pr(>Chi)", "Pr(>Chisq)"))
  if (length(cc) == 0 || nrow(lrt) < 2) return(NA_real_)
  pval <- suppressWarnings(as.numeric(lrt[[cc]][2]))
  if (length(pval) != 1 || is.na(pval)) return(NA_real_)
  pval
}

# -------- Monte-Carlo power (robust, early-stopping, single-core) --------
power_H3_interaction_conv <- function(reps_per_cell, n_steps,
                                      n_sims_max = 200, seed = 1,
                                      use_wald = TRUE,
                                      savefile = NULL,
                                      check_every = 20,
                                      target = 0.80, futility = 0.70) {
  set.seed(seed)
  pfun <- if (use_wald) fit_H3_WALD_conv else fit_H3_LRT_conv
  pvals <- rep(NA_real_, n_sims_max)
  
  mc_ci <- function(p_hat, n, level = 0.95){
    se <- sqrt(p_hat * (1 - p_hat) / n)
    z  <- qnorm(1 - (1 - level)/2)
    c(lower = max(0, p_hat - z*se), upper = min(1, p_hat + z*se), se = se)
  }
  
  done <- n_sims_max
  for (s in seq_len(n_sims_max)) {
    conv <- simulate_conversation_level(reps_per_cell, n_steps)
    p <- try(pfun(conv), silent = TRUE)
    
    if (!inherits(p, "try-error")) {
      p_num <- suppressWarnings(as.numeric(p)[1])
      if (isTRUE(is.finite(p_num))) pvals[s] <- p_num
    }
    
    if (!is.null(savefile) && (s %% check_every == 0)) {
      saveRDS(pvals[1:s], sprintf("%s_partial_%03d.rds", savefile, s))
    }
    
    if (s %% check_every == 0) {
      ph <- mean(pvals[1:s] < 0.05, na.rm = TRUE)
      if (isTRUE(is.finite(ph))) {
        ci <- mc_ci(ph, s)
        if (isTRUE(ci["lower"] > target) || isTRUE(ci["upper"] < futility)) {
          done <- s
          break
        }
      }
    }
  }
  
  if (!is.null(savefile)) saveRDS(pvals[1:done], savefile)
  
  tibble(
    reps_per_cell = reps_per_cell,
    n_steps = n_steps,
    power = mean(pvals[1:done] < 0.05, na.rm = TRUE),
    sims  = done
  )
}

# -------- Helper for CIs --------
power_CI <- function(p_hat, n, level = 0.95){
  se <- sqrt(p_hat * (1 - p_hat) / n)
  z  <- qnorm(1 - (1 - level)/2)
  c(lower = max(0, p_hat - z*se), upper = min(1, p_hat + z*se), se = se)
}

# ================= SCOUT: Wald over REPS_LIST =================
scout_grid <- tibble(reps_per_cell = REPS_LIST, n_steps = N_STEPS)

message("=== SCOUT (Wald, conversation-level) ===")
scout_results <- pbapply::pblapply(seq_len(nrow(scout_grid)), function(i){
  reps_i  <- scout_grid$reps_per_cell[i]
  steps_i <- scout_grid$n_steps[i]
  power_H3_interaction_conv(
    reps_per_cell = reps_i,
    n_steps = steps_i,
    n_sims_max = NSIMS_SCOUT,
    seed = 2000 + i,
    use_wald = TRUE,
    savefile = sprintf("scout_conv_pvals_r%d_s%d.rds", reps_i, steps_i),
    check_every = CHECK_EVERY, target = TARGET_POWER, futility = 0.65
  )
})
scout_results <- bind_rows(scout_results) %>% arrange(reps_per_cell)
print(scout_results, n = Inf)
write_csv(scout_results, "power_scout_conv_results.csv")

# Print CIs
apply(scout_results, 1, function(row){
  r <- as.list(row); r$power <- as.numeric(r$power); r$sims <- as.numeric(r$sims)
  ci <- power_CI(r$power, r$sims)
  cat(sprintf("reps=%s, steps=%s -> power=%.3f, 95%% CI [%.3f, %.3f], sims=%d\n",
              r$reps_per_cell, r$n_steps, r$power, ci["lower"], ci["upper"], r$sims))
})

# ================= CONFIRM: LRT on chosen N =================
N_CONFIRM <- max(REPS_LIST)  # confirm the largest N by default; change if desired

message(sprintf("=== CONFIRM (LRT, conversation-level) for reps=%d, steps=%d ===",
                N_CONFIRM, N_STEPS))
confirm_res <- power_H3_interaction_conv(
  reps_per_cell = N_CONFIRM, n_steps = N_STEPS,
  n_sims_max = NSIMS_CONFIRM, seed = 3000 + N_CONFIRM,
  use_wald = FALSE,  # LRT
  savefile = sprintf("confirm_conv_pvals_r%d_s%d.rds", N_CONFIRM, N_STEPS),
  check_every = 50, target = TARGET_POWER, futility = 0.60
)
print(confirm_res)
ci_confirm <- power_CI(confirm_res$power, confirm_res$sims)
cat(sprintf("CONFIRM (reps=%d): power=%.3f, 95%% CI [%.3f, %.3f], sims=%d\n",
            N_CONFIRM, confirm_res$power, ci_confirm["lower"], ci_confirm["upper"], confirm_res$sims))

# ================= MDE SWEEP (scales × reps) =================
# Scales multiply the cluster×model interaction matrix.
SCALES          <- c(1, 1.5, 2, 3, 4)   # 1x≈±2%, 2x≈±4%, 4x≈±8% max cell shift
REPS_FOR_MDE    <- REPS_LIST
N_STEPS_FOR_MDE <- N_STEPS
NSIMS_MDE       <- 120

# Store the baseline once for restoration/scaling
BASE_INTERACTION <- pars$eff_interaction

# Report max absolute % change for a given scale (human-readable)
interaction_percent <- function(scale = 1){
  round(100 * max(abs(BASE_INTERACTION)) * scale, 1)  # e.g., ±2.0% at scale=1
}

# Run one (scale, reps) cell
mde_one <- function(scale, reps, n_steps = N_STEPS_FOR_MDE, n_sims = NSIMS_MDE,
                    target = TARGET_POWER, futility = FUTILITY_BND, check_every = CHECK_EVERY){
  pars$eff_interaction <<- BASE_INTERACTION * scale
  out <- power_H3_interaction_conv(
    reps_per_cell = reps,
    n_steps = n_steps,
    n_sims_max = n_sims,
    seed = 5000 + round(100*scale) + reps,
    use_wald = TRUE,     # fast Wald for MDE sweep
    savefile = NULL,
    check_every = check_every,
    target = target,
    futility = futility
  )
  out$scale <- scale
  out$int_pct_max <- interaction_percent(scale)  # max ±% change in any cell
  out
}

message("=== MDE SWEEP (Wald; conversation-level) ===")
mde_grid <- expand.grid(scale = SCALES, reps = REPS_FOR_MDE, KEEP.OUT.ATTRS = FALSE)
mde_results_list <- pbapply::pblapply(seq_len(nrow(mde_grid)), function(i){
  s <- mde_grid$scale[i]
  r <- mde_grid$reps[i]
  mde_one(scale = s, reps = r)
})

# Restore baseline interaction
pars$eff_interaction <- BASE_INTERACTION

mde_results <- dplyr::bind_rows(mde_results_list) |>
  dplyr::mutate(
    se = sqrt(power * (1 - power) / sims),
    lower = pmax(0, power - 1.96 * se),
    upper = pmin(1, power + 1.96 * se)
  ) |>
  dplyr::arrange(reps_per_cell, scale)

print(mde_results, n = Inf)
readr::write_csv(mde_results, "mde_sweep_results.csv")

# For each reps, identify the smallest scale whose 95% CI lower bound ≥ .80
mde_summary <- mde_results |>
  dplyr::group_by(reps_per_cell) |>
  dplyr::arrange(scale, .by_group = TRUE) |>
  dplyr::summarise(
    scale_at_80 = { ix <- which(lower >= 0.80); if(length(ix)) scale[ix[1]] else NA_real_ },
    mde_percent = ifelse(is.na(scale_at_80), NA_real_, interaction_percent(scale_at_80)),
    .groups = "drop"
  )

print(mde_summary, n = Inf)
readr::write_csv(mde_summary, "mde_summary.csv")

# ========= Planning totals (informative) =========
# total_conversations = 5 clusters × 3 models × 6 recipes × reps_per_cell
# For reps_per_cell = 200 -> total_conversations = 18,000 (analysis table has 18k rows).
