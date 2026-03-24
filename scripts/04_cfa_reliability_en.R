# ============================================================
# 04_cfa_reliability.R
# CFA and reliability of the final measurement models
# Simplified version, but with robust/scaled extraction
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

# ------------------------------------------------------------
# Load cleaned dataset
# ------------------------------------------------------------

bundle <- readRDS(PATHS$clean_data_rds)

df <- bundle$df
variable_key <- bundle$variable_key
construct_items <- bundle$construct_items

manifest_names <- variable_key$analysis_name

# ------------------------------------------------------------
# Final measurement models
# ------------------------------------------------------------

model_ie_uni <- '
  IE =~ ie_1 + ie_2 + ie_3 + ie_4 + ie_5 + ie_6
'

model_me_3f <- '
  AP =~ me_1 + me_2 + me_3 + me_4 + me_5
  NS =~ me_6 + me_7 + me_8
  CC =~ me_9 + me_10 + me_11 + me_12 + me_13 + me_14
'

model_ee_2nd <- '
  CT =~ ee_1 + ee_2 + ee_3 + ee_4 + ee_5 + ee_6
  DH =~ ee_7 + ee_8 + ee_9 + ee_10 + ee_11 + ee_12
  AI =~ ee_13 + ee_14 + ee_15 + ee_16 + ee_17 + ee_18
  EE =~ CT + DH + AI
'

model_ae_3f <- '
  AG =~ ae_1 + ae_2 + ae_3 + ae_4 + ae_5 + ae_6
  AR =~ ae_7 + ae_8 + ae_9 + ae_10 + ae_11 + ae_12
  AL =~ ae_13 + ae_14 + ae_15 + ae_16 + ae_17 + ae_18
'

models <- list(IE_uni = list(construct = "IE",
                             model_type = "unidimensional",
                             syntax = model_ie_uni,
                             ordered_items = construct_items$ie,
                             save_as = "fit_ie.rds"),
  ME_3f = list(
    construct = "ME",
    model_type = "first_order_correlated",
    syntax = model_me_3f,
    ordered_items = construct_items$me,
    save_as = "fit_me.rds"
  ),
  EE_2nd = list(
    construct = "EE",
    model_type = "second_order",
    syntax = model_ee_2nd,
    ordered_items = construct_items$ee,
    save_as = "fit_ee_second_order.rds"
  ),
  AE_3f = list(
    construct = "AE",
    model_type = "first_order_correlated",
    syntax = model_ae_3f,
    ordered_items = construct_items$ae,
    save_as = "fit_ae.rds"
  )
)

# ------------------------------------------------------------
# Estimate models
# ------------------------------------------------------------

fit_cfa <- function(model_syntax, ordered_items) {
  lavaan::cfa(
    model = model_syntax,
    data = df,
    estimator = "WLSMV",
    std.lv = TRUE,
    parameterization = "theta",
    ordered = ordered_items
  )
}

fits <- list(
  IE_uni = fit_cfa(models$IE_uni$syntax, models$IE_uni$ordered_items),
  ME_3f = fit_cfa(models$ME_3f$syntax, models$ME_3f$ordered_items),
  EE_2nd = fit_cfa(models$EE_2nd$syntax, models$EE_2nd$ordered_items),
  AE_3f = fit_cfa(models$AE_3f$syntax, models$AE_3f$ordered_items)
)

# ------------------------------------------------------------
# Helper functions to extract robust/scaled indices
# ------------------------------------------------------------

pick_fit_value <- function(fm, candidates) {
  for (nm in candidates) {
    if (nm %in% names(fm)) {
      val <- unname(fm[[nm]])
      if (length(val) == 1 && is.finite(val)) return(val)
    }
  }
  NA_real_
}

pick_fit_label <- function(fm, candidates) {
  for (nm in candidates) {
    if (nm %in% names(fm)) {
      val <- unname(fm[[nm]])
      if (length(val) == 1 && is.finite(val)) return(nm)
    }
  }
  NA_character_
}

# ------------------------------------------------------------
# Global fit table (prioritizing robust/scaled indices)
# ------------------------------------------------------------

extract_fit_table <- function(fit, model_id, construct, model_type, n_items) {
  fm <- lavaan::fitMeasures(fit)
  
  chisq_candidates <- c("chisq.scaled", "chisq.robust", "chisq")
  df_candidates <- c("df.scaled", "df.robust", "df")
  p_candidates <- c("pvalue.scaled", "pvalue.robust", "pvalue")
  cfi_candidates <- c("cfi.robust", "cfi.scaled", "cfi")
  tli_candidates <- c("tli.robust", "tli.scaled", "tli")
  rmsea_candidates <- c("rmsea.robust", "rmsea.scaled", "rmsea")
  rmsea_lo_candidates <- c("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled", "rmsea.ci.lower")
  rmsea_hi_candidates <- c("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled", "rmsea.ci.upper")
  srmr_candidates <- c("srmr")
  
  chisq_val <- pick_fit_value(fm, chisq_candidates)
  df_val <- pick_fit_value(fm, df_candidates)
  p_val <- pick_fit_value(fm, p_candidates)
  cfi_val <- pick_fit_value(fm, cfi_candidates)
  tli_val <- pick_fit_value(fm, tli_candidates)
  rmsea_val <- pick_fit_value(fm, rmsea_candidates)
  rmsea_lo_val <- pick_fit_value(fm, rmsea_lo_candidates)
  rmsea_hi_val <- pick_fit_value(fm, rmsea_hi_candidates)
  srmr_val <- pick_fit_value(fm, srmr_candidates)
  
  tibble::tibble(
    model_id = model_id,
    construct = construct,
    model_type = model_type,
    n_items = n_items,
    converged = lavaan::lavInspect(fit, "converged"),
    chisq = chisq_val,
    df = df_val,
    chisq_df = chisq_val / df_val,
    p_value = p_val,
    cfi = cfi_val,
    tli = tli_val,
    rmsea = rmsea_val,
    rmsea_ci_lower = rmsea_lo_val,
    rmsea_ci_upper = rmsea_hi_val,
    srmr = srmr_val,
    chisq_source = pick_fit_label(fm, chisq_candidates),
    cfi_source = pick_fit_label(fm, cfi_candidates),
    tli_source = pick_fit_label(fm, tli_candidates),
    rmsea_source = pick_fit_label(fm, rmsea_candidates)
  )
}

cfa_fit_summary <- dplyr::bind_rows(
  extract_fit_table(fits$IE_uni, "IE_uni", "IE", "unidimensional", length(construct_items$ie)),
  extract_fit_table(fits$ME_3f, "ME_3f", "ME", "first_order_correlated", length(construct_items$me)),
  extract_fit_table(fits$EE_2nd, "EE_2nd", "EE", "second_order", length(construct_items$ee)),
  extract_fit_table(fits$AE_3f, "AE_3f", "AE", "first_order_correlated", length(construct_items$ae))
)

# ------------------------------------------------------------
# Standardized loadings
# ------------------------------------------------------------

extract_loadings <- function(fit, model_id, construct, model_type) {
  lavaan::standardizedSolution(fit) %>%
    dplyr::filter(op == "=~") %>%
    dplyr::mutate(
      loading_level = dplyr::if_else(rhs %in% manifest_names, "item_loading", "higher_order_loading"),
      model_id = model_id,
      construct = construct,
      model_type = model_type
    ) %>%
    dplyr::transmute(
      model_id,
      construct,
      model_type,
      loading_level,
      factor = lhs,
      indicator = rhs,
      loading_std = round(est.std, 3),
      se = round(se, 3),
      z_value = round(z, 3),
      p_value = pvalue
    ) %>%
    dplyr::left_join(
      variable_key %>%
        dplyr::select(analysis_name, label_short),
      by = c("indicator" = "analysis_name")
    )
}

cfa_standardized_loadings <- dplyr::bind_rows(
  extract_loadings(fits$IE_uni, "IE_uni", "IE", "unidimensional"),
  extract_loadings(fits$ME_3f, "ME_3f", "ME", "first_order_correlated"),
  extract_loadings(fits$EE_2nd, "EE_2nd", "EE", "second_order"),
  extract_loadings(fits$AE_3f, "AE_3f", "AE", "first_order_correlated")
)

# ------------------------------------------------------------
# Reliability by dimension
# ------------------------------------------------------------

compute_alpha_stats <- function(data, items) {
  out <- psych::alpha(data[, items], warnings = FALSE, check.keys = FALSE)
  
  tibble::tibble(
    alpha = unname(out$total$std.alpha),
    mean_interitem_r = unname(out$total$average_r),
    guttman_g6 = unname(out$total[["G6(smc)"]]),
    ase = unname(out$total$ase)
  )
}

compute_cr_ave <- function(fit, factor_name) {
  std_sol <- lavaan::standardizedSolution(fit)
  
  lambda_tbl <- std_sol %>%
    dplyr::filter(op == "=~", lhs == factor_name, rhs %in% manifest_names) %>%
    dplyr::select(item = rhs, lambda = est.std)
  
  theta_tbl <- std_sol %>%
    dplyr::filter(op == "~~", lhs == rhs, lhs %in% lambda_tbl$item) %>%
    dplyr::select(item = lhs, theta = est.std)
  
  tmp <- dplyr::left_join(lambda_tbl, theta_tbl, by = "item")
  
  lambda <- tmp$lambda
  theta <- tmp$theta
  
  tibble::tibble(
    cr = (sum(lambda)^2) / ((sum(lambda)^2) + sum(theta)),
    ave = sum(lambda^2) / (sum(lambda^2) + sum(theta))
  )
}

reliability_specs <- list(
  IE = list(items = construct_items$ie_factors$IE, fit_id = "IE_uni", construct = "IE"),
  AP = list(items = construct_items$me_factors$AP, fit_id = "ME_3f", construct = "ME"),
  NS = list(items = construct_items$me_factors$NS, fit_id = "ME_3f", construct = "ME"),
  CC = list(items = construct_items$me_factors$CC, fit_id = "ME_3f", construct = "ME"),
  CT = list(items = construct_items$ee_factors$CT, fit_id = "EE_2nd", construct = "EE"),
  DH = list(items = construct_items$ee_factors$DH, fit_id = "EE_2nd", construct = "EE"),
  AI = list(items = construct_items$ee_factors$AI, fit_id = "EE_2nd", construct = "EE"),
  AG = list(items = construct_items$ae_factors$AG, fit_id = "AE_3f", construct = "AE"),
  AR = list(items = construct_items$ae_factors$AR, fit_id = "AE_3f", construct = "AE"),
  AL = list(items = construct_items$ae_factors$AL, fit_id = "AE_3f", construct = "AE")
)

cfa_reliability_summary <- dplyr::bind_rows(
  lapply(names(reliability_specs), function(factor_name) {
    spec <- reliability_specs[[factor_name]]
    fit_obj <- fits[[spec$fit_id]]
    
    alpha_tbl <- compute_alpha_stats(df, spec$items)
    cr_ave_tbl <- compute_cr_ave(fit_obj, factor_name)
    
    tibble::tibble(
      construct = spec$construct,
      factor = factor_name,
      fit_model_id = spec$fit_id,
      n_items = length(spec$items),
      items = paste(spec$items, collapse = ", ")
    ) %>%
      dplyr::bind_cols(alpha_tbl, cr_ave_tbl)
  })
) %>%
  dplyr::mutate(
    alpha = round(alpha, 3),
    mean_interitem_r = round(mean_interitem_r, 3),
    guttman_g6 = round(guttman_g6, 3),
    ase = round(ase, 3),
    cr = round(cr, 3),
    ave = round(ave, 3)
  )

# ------------------------------------------------------------
# Save objects and tables
# ------------------------------------------------------------

saveRDS(fits$IE_uni, file.path(DIRS$results_models, models$IE_uni$save_as))
saveRDS(fits$ME_3f, file.path(DIRS$results_models, models$ME_3f$save_as))
saveRDS(fits$EE_2nd, file.path(DIRS$results_models, models$EE_2nd$save_as))
saveRDS(fits$AE_3f, file.path(DIRS$results_models, models$AE_3f$save_as))

saveRDS(models, file.path(DIRS$results_models, "cfa_model_specs.rds"))

write_csv_utf8(cfa_fit_summary, PATHS$cfa_fit_summary)
write_csv_utf8(cfa_standardized_loadings, PATHS$cfa_loadings)
write_csv_utf8(cfa_reliability_summary, PATHS$cfa_reliability)

message("CFA models saved in: ", DIRS$results_models)
message("CFA tables saved in: ", DIRS$results_tables)
message("Final measurement models estimated: IE_uni, ME_3f, EE_2nd, AE_3f")
message("Fit indices extracted prioritizing robust/scaled values when available.")
