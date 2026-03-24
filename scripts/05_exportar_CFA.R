# ============================================================
# 05_exportar_CFA.R
# Exportación de tablas CFA a CSV y LaTeX
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

library(dplyr)
library(readr)
library(stringr)
library(kableExtra)

# ------------------------------------------------------------
# Verificar insumos requeridos
# ------------------------------------------------------------

required_files <- c(
  PATHS$cfa_fit_summary,
  PATHS$cfa_loadings,
  PATHS$cfa_reliability
)

if (!all(file.exists(required_files))) {
  source("04_cfa_reliability.R")
}

# ------------------------------------------------------------
# Leer archivos
# ------------------------------------------------------------

cfa_fit_summary <- readr::read_csv(PATHS$cfa_fit_summary, show_col_types = FALSE)
cfa_loadings <- readr::read_csv(PATHS$cfa_loadings, show_col_types = FALSE)
cfa_reliability <- readr::read_csv(PATHS$cfa_reliability, show_col_types = FALSE)

# ------------------------------------------------------------
# Funciones auxiliares
# ------------------------------------------------------------

format_p_value <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x < .001 ~ "< .001",
    TRUE ~ formatC(x, format = "f", digits = 3)
  )
}

write_tex_table <- function(data, path, caption, label, align = NULL, escape = TRUE) {
  if (is.null(align)) {
    align <- paste(rep("c", ncol(data)), collapse = "")
  }
  
  tex <- data %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = caption,
      label = label,
      align = align,
      escape = escape,
      linesep = ""
    ) %>%
    kable_styling(latex_options = "hold_position") %>%
    as.character()
  
  writeLines(tex, con = path, useBytes = TRUE)
}

model_names <- c(
  IE_uni = "IE",
  ME_3f = "ME (3F)",
  EE_2nd = "EE (2nd)",
  AE_3f = "AE (3F)"
)

# ------------------------------------------------------------
# 1. Tabla resumen de ajuste CFA
# ------------------------------------------------------------

fit_tbl <- cfa_fit_summary %>%
  mutate(
    Model = recode(model_id, !!!model_names),
    `Chi_sq` = round(chisq, 3),
    df = df,
    `Chi_sq_df` = round(chisq_df, 3),
    `P_value` = format_p_value(p_value),
    CFI = round(cfi, 3),
    TLI = round(tli, 3),
    RMSEA = round(rmsea, 3),
    RMSEA_90_CI = sprintf("[%.3f, %.3f]", rmsea_ci_lower, rmsea_ci_upper),
    SRMR = round(srmr, 3)
  ) %>%
  select(Model, Chi_sq, df, Chi_sq_df, P_value, CFI, TLI, RMSEA, RMSEA_90_CI, SRMR)

write_csv_utf8(fit_tbl, file.path(DIRS$results_tables, "cfa_resumen_ajustada.csv"))

write_tex_table(
  fit_tbl,
  path = file.path(DIRS$results_tables, "cfa_resumen_ajustada.tex"),
  caption = "Ajuste de los modelos de medición retenidos.",
  label = "tab:cfa_measurement_fit",
  align = "lccccccccc"
)

# ------------------------------------------------------------
# 2. Tabla de confiabilidad y validez convergente
# ------------------------------------------------------------

reliability_tbl <- cfa_reliability %>%
  mutate(
    Construct = construct,
    Factor = factor,
    Fit_model = recode(fit_model_id, !!!model_names),
    N_items = n_items,
    Items = items,
    Alpha = round(alpha, 3),
    Mean_interitem_r = round(mean_interitem_r, 3),
    Guttman_g6 = round(guttman_g6, 3),
    ASE = round(ase, 3),
    CR = round(cr, 3),
    AVE = round(ave, 3)
  ) %>%
  select(Construct, Factor, Fit_model, N_items, Items, Alpha, Mean_interitem_r, Guttman_g6, ASE, CR, AVE)

write_csv_utf8(reliability_tbl, file.path(DIRS$results_tables, "cfa_reliability_summary.csv"))

write_tex_table(
  reliability_tbl,
  path = file.path(DIRS$results_tables, "cfa_reliability_summary.tex"),
  caption = "Confiabilidad y validez convergente de las dimensiones retenidas.",
  label = "tab:cfa_reliability",
  align = "lllcccccccc"
)

# ------------------------------------------------------------
# 3. Cargas de ítems por modelo
# ------------------------------------------------------------

item_loadings <- cfa_loadings %>%
  filter(loading_level == "item_loading") %>%
  mutate(
    Model = recode(model_id, !!!model_names),
    Loading = round(loading_std, 3),
    SE = round(se, 3),
    Z = round(z_value, 3),
    P_value = format_p_value(p_value)
  ) %>%
  select(Model, factor, indicator, label_short, Loading, SE, Z, P_value)

# Exportar una tabla por modelo
for (m in unique(item_loadings$Model)) {
  tbl <- item_loadings %>%
    filter(Model == m)
  
  stub <- case_when(
    m == "IE" ~ "cfa_item_loadings_ie",
    m == "ME (3F)" ~ "cfa_item_loadings_me_3f",
    m == "EE (2nd)" ~ "cfa_item_loadings_ee_2nd",
    m == "AE (3F)" ~ "cfa_item_loadings_ae_3f",
    TRUE ~ str_replace_all(str_to_lower(m), "[^a-z0-9]+", "_")
  )
  
  write_csv_utf8(tbl, file.path(DIRS$results_tables, paste0(stub, ".csv")))
  
  write_tex_table(
    tbl,
    path = file.path(DIRS$results_tables, paste0(stub, ".tex")),
    caption = paste("Cargas factoriales estandarizadas de los ítems para", m, "."),
    label = paste0("tab:", stub),
    align = "llllcccc"
  )
}

# ------------------------------------------------------------
# 4. Cargas de segundo orden de EE
# ------------------------------------------------------------

ee_higher_order_tbl <- cfa_loadings %>%
  filter(model_id == "EE_2nd", loading_level == "higher_order_loading") %>%
  mutate(
    Factor = factor,
    Indicator = indicator,
    Loading = round(loading_std, 3),
    SE = round(se, 3),
    Z = round(z_value, 3),
    P_value = format_p_value(p_value)
  ) %>%
  select(Factor, Indicator, Loading, SE, Z, P_value)

write_csv_utf8(
  ee_higher_order_tbl,
  file.path(DIRS$results_tables, "cfa_higher_order_loadings_ee_2nd.csv")
)

write_tex_table(
  ee_higher_order_tbl,
  path = file.path(DIRS$results_tables, "cfa_higher_order_loadings_ee_2nd.tex"),
  caption = "Cargas de segundo orden para educación emprendedora.",
  label = "tab:cfa_higher_order_ee",
  align = "llcccc"
)

message("Tablas exportadas de CFA guardadas en: ", DIRS$results_tables)
