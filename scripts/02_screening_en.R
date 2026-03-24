# ============================================================
# 02_screening.R
# Initial descriptive review and factorial adequacy
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

# Load cleaned dataset
bundle <- readRDS(PATHS$clean_data_rds)

df <- bundle$df
construct_items <- bundle$construct_items

# ------------------------------------------------------------
# 1. Sample profile
# ------------------------------------------------------------

sample_profile <- bind_rows(tibble(characteristic = "Sample size",
                                   level = NA_character_,
                                   n = nrow(df),    
                                   percent = NA_real_),
                            
                            df %>%
                              count(genero, name = "n") %>%
                              mutate(characteristic = "Gender",
                                     level = as.character(genero),
                                     percent = round(100 * n / sum(n), 1)) %>%
                              select(characteristic, level, n, percent),
                            
                            df %>%
                              count(educacion_emprendedora, name = "n") %>%
                              mutate(characteristic = "Entrepreneurship education",
                                     level = as.character(educacion_emprendedora),
                                     percent = round(100 * n / sum(n), 1)) %>%
                              select(characteristic, level, n, percent),
                            
                            tibble(characteristic = c("Age", "Age", "Age", "Age"),
                                   level = c("Mean", "SD", "Min", "Max"),
                                   n = c(round(mean(df$edad, na.rm = TRUE), 2),
                                         round(sd(df$edad, na.rm = TRUE), 2),
                                         min(df$edad, na.rm = TRUE),
                                         max(df$edad, na.rm = TRUE)),
                                   percent = NA_real_),
                            
                            df %>%
                              count(universidad, name = "n") %>%
                              mutate(characteristic = "University",
                                     level = as.character(universidad),
                                     percent = round(100 * n / sum(n), 1)) %>%
                              select(characteristic, level, n, percent)
)

# ------------------------------------------------------------
# 2. Descriptive item summary
# ------------------------------------------------------------

item_summary <- tibble(variable = construct_items$all_items) %>%
  mutate(mean = map_dbl(variable, ~ mean(df[[.x]], na.rm = TRUE)),
         sd = map_dbl(variable, ~ sd(df[[.x]], na.rm = TRUE)),
         min = map_dbl(variable, ~ min(df[[.x]], na.rm = TRUE)),
         max = map_dbl(variable, ~ max(df[[.x]], na.rm = TRUE)),
         skewness = map_dbl(variable, ~ psych::skew(df[[.x]], na.rm = TRUE)),
         kurtosis = map_dbl(variable, ~ psych::kurtosi(df[[.x]], na.rm = TRUE))) %>%
  mutate(mean = round(mean, 3),
         sd = round(sd, 3),
         skewness = round(skewness, 3),
         kurtosis = round(kurtosis, 3))

# ------------------------------------------------------------
# 3. KMO and Bartlett by block
# ------------------------------------------------------------

run_kmo_bartlett <- function(data, vars, block_name) {
  rho <- psych::polychoric(data[, vars])$rho
  
  kmo <- psych::KMO(rho)
  bart <- psych::cortest.bartlett(rho, n = nrow(data))
  
  tibble(block = block_name,
         n_items = length(vars),
         kmo_msa = round(kmo$MSA, 3),
         bartlett_chisq = round(as.numeric(bart$chisq), 3),
         bartlett_df = as.integer(bart$df),
         bartlett_p = ifelse(bart$p.value < .001, "< .001", round(bart$p.value, 3)))
}

kmo_bartlett <- bind_rows(run_kmo_bartlett(df, construct_items$me, "ME"),
                          run_kmo_bartlett(df, construct_items$ie, "IE"),
                          run_kmo_bartlett(df, construct_items$ee, "EE"),
                          run_kmo_bartlett(df, construct_items$ae, "AE"))

# ------------------------------------------------------------
# Save results
# ------------------------------------------------------------

write_csv_utf8(sample_profile, PATHS$screening_sample_profile)
write_csv_utf8(item_summary, PATHS$screening_item_summary)
write_csv_utf8(kmo_bartlett, PATHS$screening_kmo_bartlett)

message("Screening tables saved in: ", DIRS$results_tables)
