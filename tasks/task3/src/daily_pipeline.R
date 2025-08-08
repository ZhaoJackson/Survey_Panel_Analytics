#!/usr/bin/env Rscript
# =============================================================================
# DAILY QUALITY CONTROL PIPELINE - RStudio Ready
# =============================================================================
# 🎯 PURPOSE: Daily execution of quality control analysis (no data generation)
# 🔧 RStudio USAGE: 
#    1. Open this file in RStudio
#    2. Click "Source" button OR press Ctrl+Shift+S (Cmd+Shift+S on Mac)
#    3. Or run: source("daily_pipeline.R") in RStudio console
# 
# ⚡ EXECUTION TIME: ~5 seconds
# 📊 OUTPUTS: Updated dashboards and quality metrics
# =============================================================================

# Detect and set working directory automatically
current_wd <- getwd()
cat("Current working directory:", current_wd, "\n")

# Navigate to correct directory structure
if (grepl("task3/src$", current_wd)) {
  # Already in src folder - perfect!
  cat("✅ Running from src folder\n")
} else if (grepl("task3$", current_wd)) {
  # In task3 root, need to go to src
  setwd("src")
  cat("📁 Navigated to src folder\n")
} else if (file.exists("tasks/task3/src/quality_control_pipeline.R")) {
  # Running from project root
  setwd("tasks/task3/src") 
  cat("📁 Navigated from project root to tasks/task3/src\n")
} else {
  cat("❌ ERROR: Cannot find quality_control_pipeline.R\n")
  cat("   Please ensure you're running from:\n")
  cat("   - tasks/task3/src/ folder, OR\n") 
  cat("   - tasks/task3/ folder, OR\n")
  cat("   - project root folder\n")
  stop("Cannot locate pipeline files")
}

cat("\n=== STARTING DAILY QUALITY CONTROL PIPELINE ===\n")
cat("🕐 Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Source main pipeline with data generation disabled
.dont_auto_run <- TRUE  # Prevent main() from auto-executing
source("quality_control_pipeline.R")

# Run daily analysis (no data generation)
cat("🔄 Running quality analysis...\n")
run_quality_pipeline(generate_data = FALSE)

cat("✅ Daily pipeline completed successfully!\n")
cat("📊 View results: tasks/task3/outputs/verasight_quality_dashboard.html\n")
cat("🕐 End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

