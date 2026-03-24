# ============================================================
# 06_sem_model.R
# Final SEM model: full vs. parsimonious
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

# Load cleaned dataset
bundle <- readRDS(PATHS$clean_data_rds)

df <- bundle$df
construct_items <- bundle$construct_items

# ------------------------------------------------------------
# Output paths
# ------------------------------------------------------------

sem_fit_path <- file.path(DIRS$results_tables, "sem_recommended_fit.csv")
sem_paths_path <- file.path(DIRS$results_tables, "sem_recommended_standardized_paths.csv")
sem_indirect_path <- file.path(DIRS$results_tables, "sem_recommended_defined_effects.csv")
sem_r2_path <- file.path(DIRS$results_tables, "sem_recommended_r2.csv")
sem_models_path <- file.path(DIRS$results_models, "sem_models.rds")

# ------------------------------------------------------------
# Ordinal items
# ------------------------------------------------------------

ordered_items <- c(
  construct_items$ee,
  construct_items$ae,
  construct_items$me,
  construct_items$ie
)

# ------------------------------------------------------------
# Full SEM model
# ------------------------------------------------------------

model_full <- '
  # Measurement model
  CT =~ ee_1 + ee_2 + ee_3 + ee_4 + ee_5 + ee_6
  DH =~ ee_7 + ee_8 + ee_9 + ee_10 + ee_11 + ee_12
  AI =~ ee_13 + ee_14 + ee_15 + ee_16 + ee_17 + ee_18
  EE =~ CT + DH + AI

  AG =~ ae_1 + ae_2 + ae_3 + ae_4 + ae_5 + ae_6
  AR =~ ae_7 + ae_8 + ae_9 + ae_10 + ae_11 + ae_12
  AL =~ ae_13 + ae_14 + ae_15 + ae_16 + ae_17 + ae_18

  AP =~ me_1 + me_2 + me_3 + me_4 + me_5
  NS =~ me_6 + me_7 + me_8
  CC =~ me_9 + me_10 + me_11 + me_12 + me_13 + me_14

  IE =~ ie_1 + ie_2 + ie_3 + ie_4 + ie_5 + ie_6

  # Structural model
  AG ~ a1*EE
  AR ~ a2*EE
  AL ~ a3*EE
  AP ~ a4*EE
  NS ~ a5*EE
  CC ~ a6*EE

  IE ~ b1*AG + b2*AR + b3*AL + b4*AP + b5*NS + b6*CC + cprime*EE

  # Indirect effects
  ind_AG := a1*b1
  ind_AR := a2*b2
  ind_AL := a3*b3
  ind_AP := a4*b4
  ind_NS := a5*b5
  ind_CC := a6*b6

  ind_AE_total := ind_AG + ind_AR + ind_AL
  ind_ME_total := ind_AP + ind_NS + ind_CC
  total_ind := ind_AE_total + ind_ME_total
  total_effect := cprime + total_ind
'

# ------------------------------------------------------------
# Parsimonious SEM model
# ------------------------------------------------------------

model_pars <- '
  # Measurement model
  CT =~ ee_1 + ee_2 + ee_3 + ee_4 + ee_5 + ee_6
  DH =~ ee_7 + ee_8 + ee_9 + ee_10 + ee_11 + ee_12
  AI =~ ee_13 + ee_14 + ee_15 + ee_16 + ee_17 + ee_18
  EE =~ CT + DH + AI

  AG =~ ae_1 + ae_2 + ae_3 + ae_4 + ae_5 + ae_6
  AR =~ ae_7 + ae_8 + ae_9 + ae_10 + ae_11 + ae_12
  AL =~ ae_13 + ae_14 + ae_15 + ae_16 + ae_17 + ae_18

  AP =~ me_1 + me_2 + me_3 + me_4 + me_5
  NS =~ me_6 + me_7 + me_8
  CC =~ me_9 + me_10 + me_11 + me_12 + me_13 + me_14

  IE =~ ie_1 + ie_2 + ie_3 + ie_4 + ie_5 + ie_6

  # Structural model
  AG ~ a1*EE
  AR ~ a2*EE
  AL ~ a3*EE
  AP ~ a4*EE
  NS ~ a5*EE
  CC ~ a6*EE

  IE ~ b1*AG + b2*AR + b3*AL + b4*AP + b5*NS + b6*CC

  # Indirect effects
  ind_AG := a1*b1
  ind_AR := a2*b2
  ind_AL := a3*b3
  ind_AP := a4*b4
  ind_NS := a5*b5
  ind_CC := a6*b6

  ind_AE_total := ind_AG + ind_AR + ind_AL
  ind_ME_total := ind_AP + ind_NS + ind_CC
  total_ind := ind_AE_total + ind_ME_total
  total_effect := total_ind
'

# ------------------------------------------------------------
# Fit models
# ------------------------------------------------------------

fit_sem <- function(model_syntax) {
  lavaan::sem(
    model = model_syntax,
    data = df,
    estimator = "WLSMV",
    std.lv = TRUE,
    parameterization = "theta",
    ordered = ordered_items
  )
}

fit_sem_recommended_full <- fit_sem(model_full)
fit_sem_recommended_pars <- fit_sem(model_pars)

# ------------------------------------------------------------
# Extract global fit
# ------------------------------------------------------------

extract_fit <- function(fit, model_name) {
  fm <- lavaan::fitMeasures(fit)
  
  tibble(
    model = model_name,
    converged = lavaan::lavInspect(fit, "converged"),
    chisq = unname(fm["chisq"]),
    df = unname(fm["df"]),
    chisq_df = unname(fm["chisq"] / fm["df"]),
    p_value = unname(fm["pvalue"]),
    cfi = unname(fm["cfi"]),
    tli = unname(fm["tli"]),
    rmsea = unname(fm["rmsea"]),
    rmsea_ci_lower = unname(fm["rmsea.ci.lower"]),
    rmsea_ci_upper = unname(fm["rmsea.ci.upper"]),
    srmr = unname(fm["srmr"])
  )
}

sem_recommended_fit <- bind_rows(
  extract_fit(fit_sem_recommended_full, "recommended_full"),
  extract_fit(fit_sem_recommended_pars, "recommended_pars")
)

# ------------------------------------------------------------
# Extract standardized structural paths
# ------------------------------------------------------------

extract_std_paths <- function(fit, model_name) {
  lavaan::standardizedSolution(
    fit,
    ci = TRUE
  ) %>%
    filter(
      op == "~",
      lhs %in% c("AG", "AR", "AL", "AP", "NS", "CC", "IE")
    ) %>%
    transmute(
      model = model_name,
      lhs,
      rhs,
      est_std = est.std,
      se,
      z,
      pvalue,
      ci.lower,
      ci.upper
    )
}

sem_recommended_standardized_paths <- bind_rows(
  extract_std_paths(fit_sem_recommended_full, "recommended_full"),
  extract_std_paths(fit_sem_recommended_pars, "recommended_pars")
)

# ------------------------------------------------------------
# Extract indirect effects
# ------------------------------------------------------------

extract_defined_effects <- function(fit, model_name) {
  lavaan::parameterEstimates(
    fit,
    ci = TRUE,
    standardized = FALSE
  ) %>%
    filter(op == ":=") %>%
    transmute(
      model = model_name,
      effect = lhs,
      est,
      se,
      z,
      pvalue,
      ci.lower,
      ci.upper
    )
}

sem_recommended_defined_effects <- bind_rows(
  extract_defined_effects(fit_sem_recommended_full, "recommended_full"),
  extract_defined_effects(fit_sem_recommended_pars, "recommended_pars")
)

# ------------------------------------------------------------
# Extract R2 of endogenous latent constructs
# ------------------------------------------------------------

extract_r2 <- function(fit, model_name) {
  r2_vals <- lavaan::lavInspect(fit, "r2")
  
  tibble(
    variable = names(r2_vals),
    r2 = as.numeric(r2_vals)
  ) %>%
    filter(variable %in% c("AG", "AR", "AL", "AP", "NS", "CC", "IE")) %>%
    mutate(model = model_name) %>%
    select(model, variable, r2)
}

sem_recommended_r2 <- bind_rows(
  extract_r2(fit_sem_recommended_full, "recommended_full"),
  extract_r2(fit_sem_recommended_pars, "recommended_pars")
)

# ------------------------------------------------------------
# Save results
# ------------------------------------------------------------

write_csv_utf8(sem_recommended_fit, sem_fit_path)
write_csv_utf8(sem_recommended_standardized_paths, sem_paths_path)
write_csv_utf8(sem_recommended_defined_effects, sem_indirect_path)
write_csv_utf8(sem_recommended_r2, sem_r2_path)

saveRDS(list(fit_sem_recommended_full = fit_sem_recommended_full,
             fit_sem_recommended_pars = fit_sem_recommended_pars,
             recommended_full = fit_sem_recommended_full,
             recommended_pars = fit_sem_recommended_pars),
        sem_models_path)

message("SEM fit table saved to: ", sem_fit_path)
message("SEM paths table saved to: ", sem_paths_path)
message("SEM indirect effects saved to: ", sem_indirect_path)
message("SEM R2 table saved to: ", sem_r2_path)
message("SEM models saved to: ", sem_models_path)
