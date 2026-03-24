# ============================================================
# run_all.R
# Run the full analysis pipeline
# ============================================================

cat("\n============================================================\n")
cat("Entrepreneurship Education Project - Full Pipeline\n")
cat("============================================================\n\n")

# 1. Setup
cat("Step 1/8: Running 00_setup.R...\n")
source("scripts/00_setup.R")

# 2. Import and cleaning
cat("Step 2/8: Running 01_import_clean.R...\n")
source("scripts/01_import_clean.R")

# 3. Screening
cat("Step 3/8: Running 02_screening.R...\n")
source("scripts/02_screening.R")

# 4. Exploratory factor analysis / parallel analysis
cat("Step 4/8: Running 03_efa_pa.R...\n")
source("scripts/03_efa_pa.R")

# 5. CFA and reliability
cat("Step 5/8: Running 04_cfa_reliability.R...\n")
source("scripts/04_cfa_reliability.R")

# 6. Export CFA tables
cat("Step 6/8: Running 05_exportar_CFA.R...\n")
source("scripts/05_exportar_CFA.R")

# 7. SEM model
cat("Step 7/8: Running 06_sem_model.R...\n")
source("scripts/06_sem_model.R")

# 8. SEM figures
cat("Step 8/8: Running 07_sem_figures.R...\n")
source("scripts/07_sem_figures.R")

cat("\n============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Results were saved in the project folders.\n")
cat("============================================================\n\n")