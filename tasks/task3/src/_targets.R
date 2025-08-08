# _targets.R for Verasight Quality Control Pipeline
# This file defines the targets workflow for automated pipeline execution

# Load required libraries
library(targets)
library(tarchetypes)

# Source helper functions
source("quality_control_pipeline.R")

# Set global options
tar_option_set(
  packages = c("tidyverse", "lubridate", "plotly", "htmlwidgets", "jsonlite"),
  format = "rds",
  error = "continue"  # Continue pipeline even if individual targets fail
)

# Define output directory (relative to task3 directory)
output_dir <- "outputs/qc_outputs"

# Define the targets pipeline
list(
  # Data ingestion targets
  tar_target(
    name = project_directories,
    command = list.dirs("outputs/sample_survey", recursive = FALSE, full.names = TRUE),
    cue = tar_cue(mode = "always")  # Always check for new projects
  ),
  
  tar_target(
    name = panel_dataset,
    command = create_panel_dataset(),
    cue = tar_cue(mode = "always")  # Always refresh panel data
  ),
  
  # Analysis targets
  tar_target(
    name = respondent_analysis,
    command = analyze_respondent_quality(panel_dataset)
  ),
  
  tar_target(
    name = project_analysis, 
    command = analyze_project_quality(panel_dataset)
  ),
  
  tar_target(
    name = panel_analysis,
    command = analyze_panel_quality(panel_dataset, respondent_analysis, project_analysis)
  ),
  
  # Dashboard targets
  tar_target(
    name = quality_dashboards,
    command = create_quality_dashboard(panel_dataset, respondent_analysis, project_analysis, panel_analysis),
    format = "file"
  ),
  
  # Monitoring targets
  tar_target(
    name = monitoring_report,
    command = create_monitoring_report(panel_analysis, project_analysis, respondent_analysis)
  ),
  
  # File output targets (for external consumption)
  tar_target(
    name = respondent_csv,
    command = {
      filepath <- file.path(output_dir, "respondent_level", "respondent_quality_summary.csv")
      write_csv(respondent_analysis, filepath)
      filepath
    },
    format = "file"
  ),
  
  tar_target(
    name = project_csv,
    command = {
      filepath <- file.path(output_dir, "project_level", "project_quality_summary.csv") 
      write_csv(project_analysis, filepath)
      filepath
    },
    format = "file"
  ),
  
  tar_target(
    name = panel_kpis_csv,
    command = {
      filepath <- file.path(output_dir, "panel_level", "panel_kpis.csv")
      write_csv(panel_analysis$kpis, filepath)
      filepath
    },
    format = "file"
  ),
  
  # Summary target that depends on all outputs
  tar_target(
    name = pipeline_complete,
    command = {
      message("Quality control pipeline completed successfully")
      Sys.time()
    }
  )
) 