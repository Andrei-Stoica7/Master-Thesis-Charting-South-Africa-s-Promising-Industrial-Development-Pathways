# -----------------------------------------------------------------------------
# MASTER SCRIPT - THESIS
# -----------------------------------------------------------------------------
rm(list = ls())

# Packages
required_packages <- c(
  "dplyr", "data.table", "glue", "readr",
  "readxl", "economiccomplexity",
  "ggplot2", "ggrepel", "grid", "gridExtra"
)

missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing)) install.packages(missing)

lapply(required_packages, library, character.only = TRUE)

# Create output folders
dir.create("data/data_processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Set raw data path
RAW_DATA_PATH <- "data/data_raw"

# Root directory (optional but powerful)
## Change work directory to local file by replacing "X" with directory path
setwd("X")
run_script <- function(path) {
  cat("\n-----------------------------\n")
  cat("Running:", path, "\n")
  cat("-----------------------------\n\n")
  
  local({
    source(path, echo = TRUE)
  })
}

# -----------------------------------------------------------------------------
# 1. PRE-PROCESSING
# -----------------------------------------------------------------------------
run_script("scripts/pre-processing_1_ipap_evaluation.R")
run_script("scripts/pre-processing_2_initial_competitives.R")
run_script("scripts/pre-processing_3_competitives.R")

# -----------------------------------------------------------------------------
# 2. ANALYSIS
# -----------------------------------------------------------------------------
run_script("scripts/analysis_1_ipap_evaluation.R")
run_script("scripts/analysis_2_competitives.R")
run_script("scripts/analysis_3_undeveloped.R")
run_script("scripts/analysis_4_transitory.R")
run_script("scripts/analysis_5_pathways.R")

cat("\n MASTER SCRIPT FINISHED SUCCESSFULLY\n")
