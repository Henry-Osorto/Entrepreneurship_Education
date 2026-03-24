# ============================================================
# 00_setup.R
# Basic project setup
# ============================================================

# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(psych)
library(polycor)
library(lavaan)
library(semTools)
library(semptools)
library(fs)

# Project root folder
# Adjust this path if you change computers
PROJECT_ROOT <- "~/Entrepreneurship_Education/"

# Main folders
DIRS <- list(data_raw = file.path(PROJECT_ROOT, "data_raw"),
             data_processed = file.path(PROJECT_ROOT, "data_processed"),
             codebook = file.path(PROJECT_ROOT, "codebook"),
             results = file.path(PROJECT_ROOT, "results"),
             results_tables = file.path(DIRS$results, "tables"),
             results_models = file.path(DIRS$results, "models"),
             results_figures = file.path(DIRS$results, "figures"),
             results_logs = file.path(DIRS$results, "logs"))

# Create folders if they do not exist
walk(DIRS, dir_create)

# File paths
PATHS <- list(raw_data = file.path(DIRS$data_raw, "Entrepreneurship_Education.xlsx"),
              codebook = file.path(DIRS$codebook, "Codebook.xlsx"),
              
              clean_data_rds = file.path(DIRS$data_processed, "analysis_data.rds"),
              clean_data_xlsx = file.path(DIRS$data_processed, "analysis_data.xlsx"),
              variable_key = file.path(DIRS$data_processed, "variable_key.csv"),
              construct_map = file.path(DIRS$data_processed, "construct_map.rds"),
              
              screening_item_summary = file.path(DIRS$results_tables, "screening_item_summary.csv"),
              screening_sample_profile = file.path(DIRS$results_tables, "screening_sample_profile.csv"),
              screening_kmo_bartlett = file.path(DIRS$results_tables, "screening_kmo_bartlett.csv"),
              cfa_fit_summary = file.path(DIRS$results_tables, "cfa_fit_summary.csv"),
              cfa_loadings = file.path(DIRS$results_tables, "cfa_standardized_loadings.csv"),
              cfa_reliability = file.path(DIRS$results_tables, "cfa_reliability_summary.csv"),
              
              session_info = file.path(DIRS$results_logs, "session_info.txt"))

# Simple helper functions
write_csv_utf8 <- function(data, path) {
  readr::write_csv(data, path, na = "")
}

save_rds_safely <- function(object, path) {
  saveRDS(object, path)
}

save_session_info <- function(path = PATHS$session_info) {
  writeLines(capture.output(sessionInfo()), con = path)
}

# Options
options(scipen = 999,
        dplyr.summarise.inform = FALSE,
        stringsAsFactors = FALSE,
        lavaan.verbose = FALSE)

set.seed(1234)

message("Project root: ", PROJECT_ROOT)
message("Raw data file: ", PATHS$raw_data)
message("Codebook file: ", PATHS$codebook)

save_session_info()
