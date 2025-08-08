# =============================================================================
# 🚀 VERASIGHT QUALITY CONTROL PIPELINE - RECRUITER LAUNCHER
# =============================================================================
# 
# 📋 INSTRUCTIONS FOR RECRUITERS:
# 1. Open this file (RUN_PIPELINE.R) in RStudio
# 2. Click the "Source" button (top-right of script pane)
#    OR press Ctrl+Shift+S (Windows/Linux) / Cmd+Shift+S (Mac)
# 3. Wait ~5 seconds for completion
# 4. View results in outputs/verasight_quality_dashboard.html
#
# 🎯 WHAT THIS DOES:
# - Runs quality control analysis on 110 sample survey projects
# - Generates interactive dashboard with quality metrics
# - Creates multi-level analysis (respondent → project → panel)
# - Shows risk distribution and quality trends
#
# ⚡ EXECUTION TIME: ~5 seconds (daily analysis)
# 📊 OUTPUTS: Interactive HTML dashboard + CSV analysis files
# =============================================================================

cat("\n🚀 VERASIGHT QUALITY CONTROL PIPELINE LAUNCHER\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Function to find the task3 directory using script location
find_task3_directory <- function() {
  # Try to get the script's path
  script_path <- NULL
  
  # Method 1: Command line arguments (works with Rscript)
  tryCatch({
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      script_file <- sub("^--file=", "", file_arg)
      script_path <- dirname(normalizePath(script_file))
      cat("📍 Script location detected via commandArgs:", script_path, "\n")
    }
  }, error = function(e) {
    # Continue to other methods
  })
  
  # Method 2: RStudio API (works when sourcing in RStudio)
  if (is.null(script_path) && requireNamespace("rstudioapi", quietly = TRUE)) {
    tryCatch({
      if (rstudioapi::isAvailable()) {
        doc_path <- rstudioapi::getActiveDocumentContext()$path
        if (nchar(doc_path) > 0) {
          script_path <- dirname(doc_path)
          cat("📍 Script location detected via RStudio API:", script_path, "\n")
        }
      }
    }, error = function(e) {
      # Continue to other methods
    })
  }
  
  # Method 3: Use sys.frame() to get the script path
  if (is.null(script_path)) {
    tryCatch({
      # This works when script is sourced
      frame_files <- sapply(sys.frames(), function(x) x$ofile)
      frame_files <- frame_files[!sapply(frame_files, is.null)]
      if (length(frame_files) > 0) {
        script_path <- dirname(frame_files[[length(frame_files)]])
        cat("📍 Script location detected via sys.frames:", script_path, "\n")
      }
    }, error = function(e) {
      # Continue to fallback method
    })
  }
  
  # Method 4: Check if we're already in the correct directory
  current_dir <- getwd()
  cat("📁 Current working directory:", current_dir, "\n")
  
  if (file.exists("src/daily_pipeline.R") && file.exists("src/quality_control_pipeline.R")) {
    cat("✅ Already in task3 directory\n")
    return(current_dir)
  }
  
  # If we found the script path, use it
  if (!is.null(script_path)) {
    # The script should be in task3 directory, so check if required files exist
    if (file.exists(file.path(script_path, "src/daily_pipeline.R")) && 
        file.exists(file.path(script_path, "src/quality_control_pipeline.R"))) {
      cat("✅ Found task3 directory at script location:", script_path, "\n")
      return(script_path)
    }
  }
  
  # Fallback: Search relative to current directory
  cat("🔍 Searching for task3 directory relative to current location...\n")
  
  # Check various relative paths
  possible_relative_paths <- c(
    ".",  # Current directory
    "task3",  # task3 subdirectory
    "tasks/task3",  # tasks/task3 subdirectory
    "../task3",  # Parent directory task3
    "../../task3",  # Grandparent directory task3
    "../..",  # Go up to project root and look for tasks/task3
    "../../tasks/task3"
  )
  
  for (rel_path in possible_relative_paths) {
    full_path <- normalizePath(file.path(current_dir, rel_path), mustWork = FALSE)
    src_path <- file.path(full_path, "src/daily_pipeline.R")
    
    if (file.exists(src_path)) {
      cat("✅ Found task3 directory at:", full_path, "\n")
      return(full_path)
    }
  }
  
  return(NULL)
}

# Find and navigate to task3 directory
task3_dir <- find_task3_directory()

if (!is.null(task3_dir)) {
  # Ensure we're in the correct directory
  if (normalizePath(getwd()) != normalizePath(task3_dir)) {
    setwd(task3_dir)
    cat("🔄 Changed working directory to:", getwd(), "\n")
  }
  
  # Verify required files exist
  if (!file.exists("src/daily_pipeline.R")) {
    stop("❌ ERROR: src/daily_pipeline.R not found in task3 directory!")
  }
  
  # Execute the pipeline
  cat("🚀 Launching quality control pipeline...\n")
  source("src/daily_pipeline.R")
  
  cat("\n🎉 PIPELINE COMPLETED SUCCESSFULLY!\n")
  cat("📊 View your results:\n")
  cat("   • Interactive Dashboard:", file.path(getwd(), "outputs/verasight_quality_dashboard.html"), "\n")
  cat("   • Or run Shiny app: shiny::runApp('app.R') from project root\n")
  
} else {
  cat("❌ ERROR: Cannot find task3 directory!\n")
  cat("\n🔍 TROUBLESHOOTING:\n")
  cat("   The script could not locate the task3 directory.\n")
  cat("   Please ensure you have the following structure:\n")
  cat("     project_folder/\n")
  cat("     └── tasks/\n")
  cat("         └── task3/\n")
  cat("             ├── RUN_PIPELINE.R (this file)\n")
  cat("             └── src/\n")
  cat("                 ├── daily_pipeline.R\n")
  cat("                 └── quality_control_pipeline.R\n")
  cat("\n💡 MANUAL FIX:\n")
  cat("   1. Navigate to the task3 folder in RStudio:\n")
  cat("      Session → Set Working Directory → To Source File Location\n")
  cat("   2. Then run: source('src/daily_pipeline.R')\n")
  cat("\n📂 Current directory contents:\n")
  print(head(list.files("."), 20))
  stop("Pipeline execution halted")
}

cat(paste(rep("=", 50), collapse=""), "\n") 