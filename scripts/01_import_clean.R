# ============================================================
# 01_import_clean.R
# Import, rename, and save the cleaned dataset
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

# Read files
raw_df <- readxl::read_excel(PATHS$raw_data)
codebook_raw <- readxl::read_excel(PATHS$codebook)

# Basic validation
if (ncol(raw_df) != nrow(codebook_raw)) {
  stop("El número de columnas de la base (", ncol(raw_df),
       ") no coincide con el número de filas del codebook (", nrow(codebook_raw), ").",
       call. = FALSE)
}

# Clean codebook names
codebook <- codebook_raw %>%
  janitor::clean_names() %>%
  mutate(
    name_clean = janitor::make_clean_names(name),
    construct = na_if(constructo_al_que_pertenece, "")
  )

# Check for duplicated names
if (anyDuplicated(codebook$name_clean) > 0) {
  dup_names <- unique(codebook$name_clean[duplicated(codebook$name_clean)])
  stop("Hay nombres duplicados en el codebook: ", paste(dup_names, collapse = ", "), call. = FALSE)
}

# Rename dataset columns
original_colnames <- names(raw_df)
analysis_names <- codebook$name_clean
names(raw_df) <- analysis_names

# Metadata and item variables
meta_vars <- analysis_names[1:6]
item_vars <- analysis_names[7:length(analysis_names)]

# Check expected item order
expected_blocks <- c(
  paste0("me_", 1:14),
  paste0("ie_", 1:6),
  paste0("ee_", 1:18),
  paste0("ae_", 1:18)
)

if (!identical(item_vars, expected_blocks)) {
  stop(
    "El orden de los ítems no coincide con la secuencia esperada: ME, IE, EE, AE.",
    call. = FALSE
  )
}

# Cleaning and recoding
 df <- raw_df %>%
  mutate(
    fecha_respuesta = as.POSIXct(fecha_respuesta, tz = "UTC"),
    genero = factor(genero, levels = c("Femenino", "Masculino"),
                    labels = c("Mujer", "Hombre")),
    educacion_emprendedora = factor(educacion_emprendedora, levels = c("NO", "SI")),
    edad = as.integer(edad),
    universidad = as.character(universidad),
    carrera = as.character(carrera)
  ) %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Convert items to integers
 df[item_vars] <- lapply(df[item_vars], as.integer)

# Check Likert range from 1 to 7
invalid_item_values <- map_dfr(
  item_vars,
  function(v) {
    vals <- na.omit(df[[v]])
    tibble(
      variable = v,
      valid_range = all(vals %in% 1:7),
      min_value = ifelse(length(vals) == 0, NA_integer_, min(vals)),
      max_value = ifelse(length(vals) == 0, NA_integer_, max(vals))
    )
  }
)

if (any(!invalid_item_values$valid_range)) {
  bad_items <- invalid_item_values %>%
    filter(!valid_range) %>%
    pull(variable)
  
  stop(
    "Hay ítems con valores fuera del rango 1-7: ",
    paste(bad_items, collapse = ", "),
    call. = FALSE
  )
}

# Item map by construct and dimensions
construct_items <- list(
  all_items = item_vars,
  
  me = paste0("me_", 1:14),
  ie = paste0("ie_", 1:6),
  ee = paste0("ee_", 1:18),
  ae = paste0("ae_", 1:18),
  
  me_factors = list(
    AP = paste0("me_", 1:5),
    NS = paste0("me_", 6:8),
    CC = paste0("me_", 9:14)
  ),
  
  ee_factors = list(
    CT = paste0("ee_", 1:6),
    DH = paste0("ee_", 7:12),
    AI = paste0("ee_", 13:18)
  ),
  
  ae_factors = list(
    AG = paste0("ae_", 1:6),
    AR = paste0("ae_", 7:12),
    AL = paste0("ae_", 13:18)
  ),
  
  ie_factors = list(
    IE = paste0("ie_", 1:6)
  )
)

# Variable equivalence table
variable_key <- tibble(
  position = seq_along(analysis_names),
  original_name = original_colnames,
  analysis_name = analysis_names,
  label_short = codebook$label,
  construct = codebook$construct,
  scale = codebook$escala,
  block = case_when(
    analysis_name %in% meta_vars ~ "metadata",
    analysis_name %in% construct_items$me ~ "ME",
    analysis_name %in% construct_items$ie ~ "IE",
    analysis_name %in% construct_items$ee ~ "EE",
    analysis_name %in% construct_items$ae ~ "AE",
    TRUE ~ "other"
  )
)

# Main object
analysis_bundle <- list(
  df = df,
  codebook = codebook,
  variable_key = variable_key,
  construct_items = construct_items
)

# Save files
save_rds_safely(analysis_bundle, PATHS$clean_data_rds)
save_rds_safely(construct_items, PATHS$construct_map)
write_csv_utf8(variable_key, PATHS$variable_key)

writexl::write_xlsx(
  list(
    data = df,
    variable_key = variable_key
  ),
  PATHS$clean_data_xlsx
)

message("Base limpia guardada en: ", PATHS$clean_data_rds)
message("Tabla de equivalencias guardada en: ", PATHS$variable_key)
