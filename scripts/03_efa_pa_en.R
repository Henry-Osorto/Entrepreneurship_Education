# ============================================================
# 03_efa_pa.R
# Exploratory factor analysis and parallel analysis
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

# Load cleaned dataset
bundle <- readRDS(PATHS$clean_data_rds)

df <- bundle$df
construct_items <- bundle$construct_items

# ------------------------------------------------------------
# Define blocks
# ------------------------------------------------------------

blocks <- list(ME = list(items = construct_items$me, n_factors = 3),
               IE = list(items = construct_items$ie, n_factors = 1),
               EE = list(items = construct_items$ee, n_factors = 3),
               AE = list(items = construct_items$ae, n_factors = 3))

# ------------------------------------------------------------
# Simple function to run PA + EFA
# ------------------------------------------------------------

run_efa_block <- function(data, items, block_name, n_factors) {
  # Polychoric matrix
  rho <- psych::polychoric(data[, items])$rho
  
  # Parallel analysis
  pa <- psych::fa.parallel(rho,
                           n.obs = nrow(data),
                           fa = "fa",
                           fm = "minres",
                           plot = FALSE)
  
  # EFA with theoretical number of factors
  efa <- psych::fa(r = rho,
                   nfactors = n_factors,
                   n.obs = nrow(data),
                   fm = "minres",
                   rotate = ifelse(n_factors == 1, "none", "oblimin"))
  
  # Parallel analysis summary
  parallel_tbl <- tibble(block = block_name,
                         n_items = length(items),
                         theoretical_factors = n_factors,
                         parallel_suggested_factors = if (!is.null(pa$nfact)) pa$nfact else NA_integer_)
  
  # Factor loadings
  loadings_tbl <- as.data.frame(unclass(efa$loadings)) %>%
    tibble::rownames_to_column("item") %>%
    pivot_longer(cols = -item,
                 names_to = "factor",
                 values_to = "loading") %>%
    mutate(block = block_name,
           loading = round(loading, 3)) %>%
    filter(!is.na(loading))

  list(parallel = parallel_tbl,
       loadings = loadings_tbl,
       efa = efa,
       pa = pa)
}

# ------------------------------------------------------------
# Run blocks
# ------------------------------------------------------------

results <- list(ME = run_efa_block(df, blocks$ME$items, "ME", blocks$ME$n_factors),
                IE = run_efa_block(df, blocks$IE$items, "IE", blocks$IE$n_factors),
                EE = run_efa_block(df, blocks$EE$items, "EE", blocks$EE$n_factors),
                AE = run_efa_block(df, blocks$AE$items, "AE", blocks$AE$n_factors))

# ------------------------------------------------------------
# Combine tables
# ------------------------------------------------------------

efa_parallel_summary <- bind_rows(results$ME$parallel,
                                  results$IE$parallel,
                                  results$EE$parallel,
                                  results$AE$parallel)

efa_loadings_theoretical <- bind_rows(results$ME$loadings,
                                      results$IE$loadings,
                                      results$EE$loadings,
                                      results$AE$loadings)

# ------------------------------------------------------------
# Save results
# ------------------------------------------------------------

write_csv_utf8(efa_parallel_summary, file.path(DIRS$results_tables, "efa_parallel_summary.csv"))

write_csv_utf8(efa_loadings_theoretical, file.path(DIRS$results_tables, "efa_loadings_theoretical.csv"))

saveRDS(results, file.path(DIRS$results_models, "efa_results.rds"))

message("EFA outputs saved in: ", DIRS$results_tables)
