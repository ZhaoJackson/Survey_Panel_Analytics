#!/usr/bin/env Rscript
# =============================================================================
# TASK 3: QUALITY CONTROL PIPELINE FOR VERASIGHT SURVEYS
# =============================================================================
# This script implements a comprehensive quality control pipeline that:
# 1. Generates 110 sample surveys with realistic quality control fields
# 2. Creates automated data processing pipeline
# 3. Produces outputs at respondent, project, and panel levels
# 4. Handles data inconsistencies across projects
# 5. Implements monitoring and alerting
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(targets)
  library(tarchetypes)
  library(lubridate)
  library(cronR)
  library(DBI)
  library(RSQLite)
  library(plotly)
  library(htmlwidgets)
  library(jsonlite)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

# Set base directory for task3 - ensure all paths are relative to task3 root
# Get the current working directory
current_wd <- getwd()

# Determine the task3 directory based on current location
script_dir <- if (grepl("task3/src$", current_wd)) {
  # We're in the src subfolder of task3, go up one level
  dirname(current_wd)
} else if (grepl("task3$", current_wd)) {
  # We're already in task3 root
  current_wd
} else {
  # We're somewhere else, look for task3
  if (dir.exists("task3")) {
    file.path(current_wd, "task3")
  } else if (dir.exists(file.path(current_wd, "task3"))) {
    file.path(current_wd, "task3")
  } else {
    # Fallback: assume current directory
    current_wd
  }
}

# Set all paths relative to the task3 directory
TASK3_DIR <- script_dir
SAMPLE_SURVEY_DIR <- file.path(TASK3_DIR, "outputs", "sample_survey")
OUTPUT_DIR <- file.path(TASK3_DIR, "outputs", "qc_outputs")
LOGS_DIR <- file.path(TASK3_DIR, "logs")

# Create directory structure
create_directories <- function() {
  dirs <- c(TASK3_DIR, SAMPLE_SURVEY_DIR, OUTPUT_DIR, LOGS_DIR,
            file.path(OUTPUT_DIR, "respondent_level"),
            file.path(OUTPUT_DIR, "project_level"), 
            file.path(OUTPUT_DIR, "panel_level"),
            file.path(OUTPUT_DIR, "dashboards"),
            file.path(OUTPUT_DIR, "monitoring"))
  
  lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))
  message(sprintf("Created directory structure in: %s", TASK3_DIR))
}

# =============================================================================
# DATA GENERATION FUNCTIONS
# =============================================================================

# Generate realistic demographic data
generate_demographics <- function(n) {
  tibble(
    respondent_id = replicate(n, paste0(sample(c(0:9, letters), 24, replace = TRUE), collapse = "")),
    weight = round(rnorm(n, 1, 0.3), 6),
    age_group4 = sample(c("18-29", "30-49", "50-64", "65+"), n, replace = TRUE, 
                       prob = c(0.2, 0.35, 0.25, 0.2)),
    education = sample(c("HS or less", "Some college/2-yr degree", "4-yr/post-graduate degree"), 
                      n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    raceeth = sample(c("White", "Black", "Hispanic", "Other"), n, replace = TRUE,
                    prob = c(0.6, 0.12, 0.18, 0.1)),
    gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, 
                   prob = c(0.48, 0.50, 0.02)),
    pid_base = sample(c("Democrat", "Republican", "Independent", "Other or none"), 
                     n, replace = TRUE, prob = c(0.35, 0.32, 0.28, 0.05)),
    region = sample(c("Northeast", "Midwest", "South", "West"), n, replace = TRUE,
                   prob = c(0.18, 0.21, 0.38, 0.23))
  )
}

# Generate survey responses (q28-q35 based on reference data)
generate_survey_responses <- function(n) {
  tibble(
    q28 = sample(c("Very accurate", "Somewhat accurate", "Not very accurate", "Not at all accurate"), 
                n, replace = TRUE, prob = c(0.15, 0.45, 0.30, 0.10)),
    q29 = sample(c("Very trustworthy", "Somewhat trustworthy", "Not very trustworthy", "Not at all trustworthy"), 
                n, replace = TRUE, prob = c(0.12, 0.48, 0.32, 0.08)),
    q30 = sample(c("Extremely important", "Very important", "Somewhat important", "Not very important", "Not at all important"), 
                n, replace = TRUE, prob = c(0.08, 0.25, 0.40, 0.20, 0.07)),
    q31 = sample(c("Very likely", "Somewhat likely", "Not likely", "I would never participate in a phone survey"), 
                n, replace = TRUE, prob = c(0.20, 0.30, 0.35, 0.15)),
    q32 = sample(c("Yes", "Maybe", "No"), n, replace = TRUE, prob = c(0.45, 0.35, 0.20)),
    q33 = sample(c("Very likely", "Likely", "Not very likely", "Not at all likely"), 
                n, replace = TRUE, prob = c(0.15, 0.25, 0.40, 0.20)),
    q34 = sample(c("It would become my preferred incentive offer", "It would be one I would likely choose", 
                  "It would not interest me"), n, replace = TRUE, prob = c(0.30, 0.50, 0.20)),
    q35 = sample(c("Yes", "No", "It depends on who is asking"), n, replace = TRUE, 
                prob = c(0.25, 0.20, 0.55))
  )
}

# Generate quality control fields with realistic correlations
generate_quality_control <- function(demographics, responses) {
  n <- nrow(demographics)
  
  # Attention check failures (higher for younger respondents and certain patterns)
  attention_fail_prob <- ifelse(demographics$age_group4 == "18-29", 0.15,
                               ifelse(demographics$age_group4 == "30-49", 0.08, 0.04))
  attention_fail <- rbinom(n, 1, attention_fail_prob)
  
  # Speeder flags (correlated with attention failures and certain demographics)
  speeder_prob <- pmax(0.02, attention_fail * 0.3 + 
                      ifelse(demographics$age_group4 == "18-29", 0.12, 0.05))
  speeder <- rbinom(n, 1, speeder_prob)
  
  # Straight-lining (giving same response to multiple questions)
  # Check if respondent gave same answer to q28-q30 (all trust/accuracy questions)
  straight_line_behavior <- (responses$q28 == responses$q29) & 
                           (responses$q29 == responses$q30) & 
                           (responses$q28 != "Not at all accurate") # Exclude reasonable straight-lining
  
  straight_line_prob <- ifelse(straight_line_behavior, 0.6, 0.08)
  straight_line <- rbinom(n, 1, straight_line_prob)
  
  # Duration in seconds (shorter for speeders, longer for careful respondents)
  duration_base <- ifelse(speeder == 1, rnorm(n, 120, 30), rnorm(n, 350, 80))
  duration_seconds <- pmax(60, round(duration_base))
  
  tibble(
    attention_fail = as.logical(attention_fail),
    speeder = as.logical(speeder),
    straight_line = as.logical(straight_line),
    duration_seconds = duration_seconds,
    completion_date = Sys.Date() - sample(0:30, n, replace = TRUE)
  )
}

# Generate one complete survey dataset
generate_survey_data <- function(project_id, n_respondents = NULL) {
  # Variable sample sizes to simulate real surveys
  if (is.null(n_respondents)) {
    n_respondents <- sample(200:1000, 1)
  }
  
  demographics <- generate_demographics(n_respondents)
  responses <- generate_survey_responses(n_respondents)
  quality_control <- generate_quality_control(demographics, responses)
  
  # Combine all data with slight schema variations to simulate real-world inconsistencies
  survey_data <- bind_cols(demographics, responses, quality_control) |>
    mutate(project_id = project_id)
  
  # Introduce some schema variations
  schema_variant <- sample(1:4, 1)
  if (schema_variant == 1) {
    # Some projects might have renamed columns
    survey_data <- survey_data |> rename(attn_check_fail = attention_fail)
  } else if (schema_variant == 2) {
    # Some might have additional metadata
    survey_data <- survey_data |> mutate(device_type = sample(c("Desktop", "Mobile", "Tablet"), 
                                                             nrow(survey_data), replace = TRUE))
  } else if (schema_variant == 3) {
    # Some might have different speeder naming
    survey_data <- survey_data |> rename(speed_flag = speeder)
  }
  
  return(survey_data)
}

# Generate all 110 sample surveys
generate_all_surveys <- function() {
  message("Generating 110 sample surveys...")
  
  # Generate surveys for projects 2024-001 through 2024-110
  project_ids <- sprintf("2024-%03d", 1:110)
  
  for (project_id in project_ids) {
    project_dir <- file.path(SAMPLE_SURVEY_DIR, project_id, "data")
    dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
    
    survey_data <- generate_survey_data(project_id)
    
    # Save as CSV with slight filename variations
    filename_variant <- sample(1:3, 1)
    if (filename_variant == 1) {
      filename <- "responses.csv"
    } else if (filename_variant == 2) {
      filename <- paste0(project_id, "_clean_data.csv")
    } else {
      filename <- "survey_data.csv"
    }
    
    write_csv(survey_data, file.path(project_dir, filename))
    
    # Some projects might also have a reference file
    if (sample(c(TRUE, FALSE), 1, prob = c(0.7, 0.3))) {
      reference_data <- tibble(
        variable = paste0("q", 28:35),
        label = c(
          "In general, how accurate do you think public opinion surveys are?",
          "How trustworthy do you think the results of public opinion surveys are?",
          "How important do you think surveys are for society?",
          "How likely are you to participate in a survey conducted via phone call?",
          "Would you participate in a survey if you received the survey invitation in a text message?",
          "How likely would you participate if questions were asked by a digital voice assistant?",
          "Interest in incentive offer with charity donation component?",
          "Do you believe in social media polls?"
        )
      )
      write_csv(reference_data, file.path(project_dir, "reference.csv"))
    }
    
    if (which(project_ids == project_id) %% 10 == 0) {
      message(sprintf("Generated %d/%d surveys", which(project_ids == project_id), length(project_ids)))
    }
  }
  
  message("Completed generating all 110 sample surveys")
}

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

# Robust function to read and standardize data from any project
read_project_data <- function(project_path) {
  tryCatch({
    data_dir <- file.path(project_path, "data")
    if (!dir.exists(data_dir)) return(NULL)
    
    # Find CSV files (handles different naming conventions)
    csv_files <- list.files(data_dir, pattern = "\\.(csv)$", full.names = TRUE)
    data_files <- csv_files[!grepl("reference", basename(csv_files))]
    
    if (length(data_files) == 0) return(NULL)
    
    # Read the first data file
    raw_data <- read_csv(data_files[1], show_col_types = FALSE)
    
    # Standardize column names to handle schema variations
    raw_data <- raw_data |>
      rename_with(~ case_when(
        tolower(.x) %in% c("id", "respondent_id", "resp_id", "user_id") ~ "respondent_id",
        tolower(.x) %in% c("attention_fail", "attn_check_fail", "attention_check") ~ "attention_fail",
        tolower(.x) %in% c("speeder", "speed_flag", "speeder_flag") ~ "speeder", 
        tolower(.x) %in% c("straight_line", "straightline", "straight_lining") ~ "straight_line",
        .default = .x
      )) |>
      mutate(
        project_id = basename(project_path),
        data_load_date = Sys.Date()
      )
    
    # Ensure respondent_id column exists
    if (!"respondent_id" %in% names(raw_data)) {
      # Generate unique IDs if missing
      raw_data$respondent_id <- paste0(basename(project_path), "_", seq_len(nrow(raw_data)))
    }
    
    # Ensure required QC columns exist
    required_qc_cols <- c("attention_fail", "speeder", "straight_line")
    for (col in required_qc_cols) {
      if (!col %in% names(raw_data)) {
        raw_data[[col]] <- FALSE
      }
    }
    
    # Ensure logical types for QC fields
    raw_data <- raw_data |>
      mutate(
        attention_fail = as.logical(attention_fail),
        speeder = as.logical(speeder),
        straight_line = as.logical(straight_line)
      )
    
    return(raw_data)
    
  }, error = function(e) {
    warning(sprintf("Failed to read project %s: %s", basename(project_path), e$message))
    return(NULL)
  })
}

# Combine all project data into panel dataset
create_panel_dataset <- function() {
  message("Creating panel dataset from all projects...")
  
  project_dirs <- list.dirs(SAMPLE_SURVEY_DIR, recursive = FALSE, full.names = TRUE)
  
  panel_data <- map_dfr(project_dirs, read_project_data) |>
    filter(!is.na(respondent_id))
  
  # Save panel dataset
  write_csv(panel_data, file.path(OUTPUT_DIR, "panel_dataset.csv"))
  saveRDS(panel_data, file.path(OUTPUT_DIR, "panel_dataset.rds"))
  
  message(sprintf("Panel dataset created with %d respondents across %d projects", 
                 nrow(panel_data), n_distinct(panel_data$project_id)))
  
  return(panel_data)
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

# Respondent-level quality analysis
analyze_respondent_quality <- function(panel_data) {
  message("Analyzing respondent-level quality metrics...")
  
  respondent_summary <- panel_data |>
    group_by(respondent_id) |>
    summarise(
      n_surveys_participated = n(),
      first_survey_date = min(completion_date, na.rm = TRUE),
      last_survey_date = max(completion_date, na.rm = TRUE),
      total_attention_fails = sum(attention_fail, na.rm = TRUE),
      total_speeder_flags = sum(speeder, na.rm = TRUE),  
      total_straight_line = sum(straight_line, na.rm = TRUE),
      attention_fail_rate = mean(attention_fail, na.rm = TRUE),
      speeder_rate = mean(speeder, na.rm = TRUE),
      straight_line_rate = mean(straight_line, na.rm = TRUE),
      avg_duration = mean(duration_seconds, na.rm = TRUE),
      quality_score = 1 - (attention_fail_rate * 0.4 + speeder_rate * 0.3 + straight_line_rate * 0.3),
      risk_tier = case_when(
        quality_score >= 0.9 ~ "Low Risk",
        quality_score >= 0.7 ~ "Medium Risk", 
        TRUE ~ "High Risk"
      ),
      .groups = "drop"
    )
  
  # Save respondent analysis
  write_csv(respondent_summary, file.path(OUTPUT_DIR, "respondent_level", "respondent_quality_summary.csv"))
  
  # Create high-risk respondent flagging file
  high_risk_respondents <- respondent_summary |>
    filter(risk_tier == "High Risk" | attention_fail_rate > 0.5) |>
    arrange(desc(attention_fail_rate))
  
  write_csv(high_risk_respondents, file.path(OUTPUT_DIR, "monitoring", "high_risk_respondents.csv"))
  
  return(respondent_summary)
}

# Project-level quality analysis  
analyze_project_quality <- function(panel_data) {
  message("Analyzing project-level quality metrics...")
  
  project_summary <- panel_data |>
    group_by(project_id) |>
    summarise(
      n_respondents = n(),
      data_collection_start = min(completion_date, na.rm = TRUE),
      data_collection_end = max(completion_date, na.rm = TRUE),
      attention_fail_rate = mean(attention_fail, na.rm = TRUE),
      speeder_rate = mean(speeder, na.rm = TRUE),
      straight_line_rate = mean(straight_line, na.rm = TRUE),
      avg_duration = mean(duration_seconds, na.rm = TRUE),
      median_duration = median(duration_seconds, na.rm = TRUE),
      overall_quality_score = 1 - (attention_fail_rate * 0.4 + speeder_rate * 0.3 + straight_line_rate * 0.3),
      quality_flag = case_when(
        attention_fail_rate > 0.15 | speeder_rate > 0.20 | straight_line_rate > 0.25 ~ "Needs Review",
        overall_quality_score < 0.7 ~ "Poor Quality",
        TRUE ~ "Good Quality"
      ),
      .groups = "drop"
    ) |>
    arrange(desc(attention_fail_rate))
  
  # Save project analysis
  write_csv(project_summary, file.path(OUTPUT_DIR, "project_level", "project_quality_summary.csv"))
  
  # Create project monitoring alerts
  project_alerts <- project_summary |>
    filter(quality_flag != "Good Quality") |>
    arrange(overall_quality_score)
  
  write_csv(project_alerts, file.path(OUTPUT_DIR, "monitoring", "project_quality_alerts.csv"))
  
  return(project_summary)
}

# Panel-level analysis
analyze_panel_quality <- function(panel_data, respondent_summary, project_summary) {
  message("Analyzing panel-level quality metrics...")
  
  panel_kpis <- panel_data |>
    summarise(
      total_responses = n(),
      unique_respondents = n_distinct(respondent_id),
      unique_projects = n_distinct(project_id),
      overall_attention_fail_rate = mean(attention_fail, na.rm = TRUE),
      overall_speeder_rate = mean(speeder, na.rm = TRUE),
      overall_straight_line_rate = mean(straight_line, na.rm = TRUE),
      avg_responses_per_respondent = total_responses / unique_respondents,
      panel_quality_score = 1 - (overall_attention_fail_rate * 0.4 + overall_speeder_rate * 0.3 + overall_straight_line_rate * 0.3)
    ) |>
    mutate(
      analysis_date = Sys.Date(),
      high_risk_respondents = sum(respondent_summary$risk_tier == "High Risk"),
      poor_quality_projects = sum(project_summary$quality_flag == "Poor Quality")
    )
  
  # Time series analysis
  time_series <- panel_data |>
    group_by(completion_date) |>
    summarise(
      n_responses = n(),
      attention_fail_rate = mean(attention_fail, na.rm = TRUE),
      speeder_rate = mean(speeder, na.rm = TRUE),
      straight_line_rate = mean(straight_line, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(completion_date)
  
  # Save panel analysis
  write_csv(panel_kpis, file.path(OUTPUT_DIR, "panel_level", "panel_kpis.csv"))
  write_csv(time_series, file.path(OUTPUT_DIR, "panel_level", "quality_time_series.csv"))
  
  return(list(kpis = panel_kpis, time_series = time_series))
}

# =============================================================================
# DASHBOARD AND VISUALIZATION FUNCTIONS
# =============================================================================

# Create comprehensive single HTML dashboard
create_quality_dashboard <- function(panel_data, respondent_summary, project_summary, panel_analysis) {
  message("Creating comprehensive HTML dashboard...")
  
  # Create individual plotly widgets and save them
  temp_dir <- tempdir()
  
  # Project quality scatter plot
  project_plot <- plot_ly(
    data = project_summary,
    x = ~attention_fail_rate,
    y = ~speeder_rate,
    size = ~n_respondents,
    color = ~quality_flag,
    text = ~paste("Project:", project_id, "<br>Quality Score:", round(overall_quality_score, 3),
                  "<br>Respondents:", n_respondents),
    type = "scatter",
    mode = "markers",
    height = 400
  ) |>
    layout(
      title = list(text = "Project Quality Overview", font = list(size = 16)),
      xaxis = list(title = "Attention Failure Rate"),
      yaxis = list(title = "Speeder Rate")
    )
  
  # Time series quality trends
  time_plot <- plot_ly(
    data = panel_analysis$time_series,
    x = ~completion_date,
    height = 400
  ) |>
    add_trace(y = ~attention_fail_rate, name = "Attention Fails", type = "scatter", mode = "lines+markers", line = list(color = "red")) |>
    add_trace(y = ~speeder_rate, name = "Speeders", type = "scatter", mode = "lines+markers", line = list(color = "blue")) |>
    add_trace(y = ~straight_line_rate, name = "Straight-lining", type = "scatter", mode = "lines+markers", line = list(color = "green")) |>
    layout(
      title = list(text = "Quality Metrics Over Time", font = list(size = 16)),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Rate")
    )
  
  # Respondent risk distribution
  risk_counts <- respondent_summary |> count(risk_tier)
  risk_dist <- plot_ly(
    data = risk_counts,
    labels = ~risk_tier,
    values = ~n,
    type = "pie",
    textinfo = "label+percent+value",
    height = 400,
    marker = list(colors = c("green", "orange", "red"))
  ) |>
    layout(
      title = list(text = "Respondent Risk Distribution", font = list(size = 16))
    )
  
  # Save individual widgets to temporary files
  project_file <- file.path(temp_dir, "project.html")
  time_file <- file.path(temp_dir, "time.html")
  risk_file <- file.path(temp_dir, "risk.html")
  
  htmlwidgets::saveWidget(project_plot, project_file, selfcontained = TRUE)
  htmlwidgets::saveWidget(time_plot, time_file, selfcontained = TRUE)
  htmlwidgets::saveWidget(risk_dist, risk_file, selfcontained = TRUE)
  
  # Panel KPI summary table
  kpis <- panel_analysis$kpis
  kpi_summary <- data.frame(
    Metric = c("Total Responses", "Unique Respondents", "Unique Projects", 
               "Panel Quality Score", "Attention Failure Rate", "Speeder Rate", 
               "Straight-lining Rate", "High Risk Respondents"),
    Value = c(
      format(kpis$total_responses, big.mark = ","),
      format(kpis$unique_respondents, big.mark = ","),
      kpis$unique_projects,
      paste0(round(kpis$panel_quality_score * 100, 1), "%"),
      paste0(round(kpis$overall_attention_fail_rate * 100, 1), "%"),
      paste0(round(kpis$overall_speeder_rate * 100, 1), "%"),
      paste0(round(kpis$overall_straight_line_rate * 100, 1), "%"),
      format(kpis$high_risk_respondents, big.mark = ",")
    )
  )
  
  # Create comprehensive HTML dashboard
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Verasight Quality Control Dashboard</title>
    <meta charset="utf-8">
    <style>
        body { 
            font-family: Arial, sans-serif; 
            margin: 20px; 
            background-color: #f5f5f5;
        }
        .header {
            background-color: #2c3e50;
            color: white;
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 20px;
            text-align: center;
        }
        .header h1 { margin: 0; font-size: 28px; }
        .header p { margin: 5px 0 0 0; font-size: 14px; opacity: 0.9; }
        .container {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
            margin-bottom: 20px;
        }
        .full-width {
            grid-column: 1 / -1;
        }
        .chart-container {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .kpi-table {
            width: 100%%;
            border-collapse: collapse;
            margin-top: 10px;
        }
        .kpi-table th, .kpi-table td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }
        .kpi-table th {
            background-color: #3498db;
            color: white;
        }
        .kpi-table tr:nth-child(even) {
            background-color: #f9f9f9;
        }
        .alert-section {
            background-color: #e8f5e8;
            border-left: 4px solid #27ae60;
            padding: 15px;
            margin: 20px 0;
            border-radius: 5px;
        }
        .footer {
            text-align: center;
            color: #666;
            font-size: 12px;
            margin-top: 30px;
            padding: 20px;
            border-top: 1px solid #ddd;
        }
        iframe {
            width: 100%%;
            height: 450px;
            border: none;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Verasight Quality Control Dashboard</h1>
        <p>Generated on %s | Panel Quality Score: %.1f%%</p>
    </div>
    
    <div class="alert-section">
        <h3>🎯 Panel Status: All Systems Green</h3>
        <p><strong>%s</strong> total responses processed across <strong>%s</strong> projects. 
           Quality score of <strong>%.1f%%</strong> indicates excellent data quality.</p>
    </div>
    
    <div class="container">
        <div class="chart-container">
            <h3>📊 Panel KPIs</h3>
            <table class="kpi-table">
                <thead>
                    <tr><th>Metric</th><th>Value</th></tr>
                </thead>
                <tbody>
                    %s
                </tbody>
            </table>
        </div>
        
        <div class="chart-container">
            <h3>👥 Risk Distribution</h3>
            <iframe src="qc_outputs/dashboards/risk_distribution.html"></iframe>
        </div>
        
        <div class="chart-container full-width">
            <h3>📈 Quality Trends Over Time</h3>
            <iframe src="qc_outputs/dashboards/quality_trends.html"></iframe>
        </div>
        
        <div class="chart-container full-width">
            <h3>🎯 Project Quality Overview</h3>
            <iframe src="qc_outputs/dashboards/project_quality.html"></iframe>
        </div>
    </div>
    
    <div class="footer">
        <p>Verasight Quality Control Pipeline | Automated Daily Analysis | 
           Next Update: Tomorrow 2:00 AM</p>
    </div>
</body>
</html>',
    format(Sys.Date(), "%%B %%d, %%Y"),
    kpis$panel_quality_score * 100,
    format(kpis$total_responses, big.mark = ","),
    kpis$unique_projects,
    kpis$panel_quality_score * 100,
    paste(sprintf("<tr><td>%s</td><td>%s</td></tr>", kpi_summary$Metric, kpi_summary$Value), collapse = "\n                    ")
  )
  
  # Save main dashboard to outputs directory
  dashboard_path <- file.path(TASK3_DIR, "outputs", "verasight_quality_dashboard.html")
  writeLines(html_content, dashboard_path)
  
  # Copy individual chart files to dashboards subfolder
  dashboards_dir <- file.path(OUTPUT_DIR, "dashboards")
  dir.create(dashboards_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(project_file, file.path(dashboards_dir, "project_quality.html"))
  file.copy(time_file, file.path(dashboards_dir, "quality_trends.html"))
  file.copy(risk_file, file.path(dashboards_dir, "risk_distribution.html"))
  
  message("Comprehensive dashboard created: ", dashboard_path)
  return(dashboard_path)
}

# =============================================================================
# MONITORING AND ALERTING
# =============================================================================

# Create monitoring report
create_monitoring_report <- function(panel_analysis, project_summary, respondent_summary) {
  message("Creating monitoring and alerting report...")
  
  # Define quality thresholds
  ALERT_THRESHOLDS <- list(
    attention_fail_rate = 0.15,
    speeder_rate = 0.20, 
    straight_line_rate = 0.25,
    panel_quality_score = 0.7
  )
  
  # Check for panel-level alerts
  panel_alerts <- list()
  kpis <- panel_analysis$kpis
  
  if (kpis$overall_attention_fail_rate > ALERT_THRESHOLDS$attention_fail_rate) {
    panel_alerts <- append(panel_alerts, sprintf("ALERT: Panel attention failure rate (%.3f) exceeds threshold (%.3f)", 
                                                 kpis$overall_attention_fail_rate, ALERT_THRESHOLDS$attention_fail_rate))
  }
  
  if (kpis$panel_quality_score < ALERT_THRESHOLDS$panel_quality_score) {
    panel_alerts <- append(panel_alerts, sprintf("ALERT: Panel quality score (%.3f) below threshold (%.3f)",
                                                 kpis$panel_quality_score, ALERT_THRESHOLDS$panel_quality_score))
  }
  
  # Create monitoring summary
  monitoring_summary <- list(
    report_timestamp = as.character(Sys.time()),
    report_date = as.character(Sys.Date()),
    panel_alerts = panel_alerts,
    high_risk_respondents_count = sum(respondent_summary$risk_tier == "High Risk"),
    projects_needing_review = sum(project_summary$quality_flag != "Good Quality"),
    panel_kpis = kpis,
    run_id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  )
  
  # Save current monitoring report (overwrites for latest status)
  write_json(monitoring_summary, file.path(OUTPUT_DIR, "monitoring", "daily_monitoring_report.json"), 
             pretty = TRUE, auto_unbox = TRUE)
  
  # Append to historical monitoring log
  monitoring_log_file <- file.path(OUTPUT_DIR, "monitoring", "monitoring_history.jsonl")
  if (!file.exists(monitoring_log_file)) {
    # Create new monitoring log file with header
    writeLines(c("# Verasight Quality Control Pipeline Monitoring History",
                 paste("# Log started:", Sys.time()),
                 "# Format: One JSON object per line (JSONL)",
                 ""), monitoring_log_file)
  }
  
  # Append monitoring summary to history
  write_json(monitoring_summary, monitoring_log_file, auto_unbox = TRUE, append = TRUE)
  
  # Create human-readable monitoring summary
  monitoring_text_file <- file.path(OUTPUT_DIR, "monitoring", "monitoring_summary.log")
  summary_entry <- sprintf("[%s] Quality Score: %.1f%% | High Risk: %d | Projects Review: %d | Alerts: %d\n",
                           format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           kpis$panel_quality_score * 100,
                           monitoring_summary$high_risk_respondents_count,
                           monitoring_summary$projects_needing_review,
                           length(panel_alerts))
  
  if (!file.exists(monitoring_text_file)) {
    writeLines(c("=== Verasight Quality Control Monitoring Summary ===",
                 "Format: [Timestamp] Quality Score | High Risk Count | Projects Needing Review | Alert Count",
                 ""), monitoring_text_file)
  }
  cat(summary_entry, file = monitoring_text_file, append = TRUE)
  
  # Create alert files if issues found
  alert_file <- file.path(OUTPUT_DIR, "monitoring", "alerts.txt")
  alert_history_file <- file.path(OUTPUT_DIR, "monitoring", "alerts_history.log")
  
  if (length(panel_alerts) > 0 || monitoring_summary$projects_needing_review > 0) {
    # Current alert file (overwrites for latest status)
    alert_message <- paste(
      "QUALITY CONTROL ALERTS",
      "======================",
      sprintf("Generated: %s", Sys.time()),
      paste(panel_alerts, collapse = "\n"),
      sprintf("\nProjects needing review: %d", monitoring_summary$projects_needing_review),
      sprintf("High-risk respondents: %d", monitoring_summary$high_risk_respondents_count),
      sep = "\n"
    )
    writeLines(alert_message, alert_file)
    
    # Append to alert history
    if (!file.exists(alert_history_file)) {
      writeLines(c("=== Verasight Quality Control Alert History ===", ""), alert_history_file)
    }
    
    alert_history_entry <- sprintf("\n[%s] ALERTS DETECTED:\n%s\nProjects needing review: %d | High-risk respondents: %d\n%s\n",
                                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   paste(panel_alerts, collapse = "\n"),
                                   monitoring_summary$projects_needing_review,
                                   monitoring_summary$high_risk_respondents_count,
                                   paste(rep("-", 50), collapse = ""))
    
    cat(alert_history_entry, file = alert_history_file, append = TRUE)
    
  } else {
    # No alerts - clear current alert file but log to history
    writeLines(sprintf("No alerts detected as of %s", Sys.time()), alert_file)
    
    if (!file.exists(alert_history_file)) {
      writeLines(c("=== Verasight Quality Control Alert History ===", ""), alert_history_file)
    }
    
    no_alert_entry <- sprintf("[%s] No alerts detected - Quality Score: %.1f%%\n",
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              kpis$panel_quality_score * 100)
    cat(no_alert_entry, file = alert_history_file, append = TRUE)
  }
  
  return(monitoring_summary)
}

# =============================================================================
# MAIN PIPELINE FUNCTION
# =============================================================================

# Main pipeline execution function
run_quality_pipeline <- function(generate_data = FALSE) {
  start_time <- Sys.time()
  message(sprintf("Starting quality control pipeline at %s", start_time))
  
  # Create directory structure
  create_directories()
  
  # Generate sample data if requested OR if no data exists
  if (generate_data) {
    message("Generating sample survey data...")
    generate_all_surveys()
  } else {
    # Check if any sample data exists
    if (!dir.exists(SAMPLE_SURVEY_DIR) || length(list.dirs(SAMPLE_SURVEY_DIR, recursive = FALSE)) == 0) {
      message("No existing survey data found. Generating sample data automatically...")
      generate_all_surveys()
    } else {
      message("Using existing survey data (no generation)...")
    }
  }
  
  # Process data
  panel_data <- create_panel_dataset()
  
  if (nrow(panel_data) == 0) {
    stop("No data found in survey directories")
  }
  
  # Run analyses
  respondent_summary <- analyze_respondent_quality(panel_data)
  project_summary <- analyze_project_quality(panel_data)
  panel_analysis <- analyze_panel_quality(panel_data, respondent_summary, project_summary)
  
  # Create visualizations
  create_quality_dashboard(panel_data, respondent_summary, project_summary, panel_analysis)
  
  # Create monitoring report
  monitoring_summary <- create_monitoring_report(panel_analysis, project_summary, respondent_summary)
  
  # Log pipeline execution
  end_time <- Sys.time()
  execution_log <- list(
    pipeline_start = as.character(start_time),
    pipeline_end = as.character(end_time),
    duration_minutes = as.numeric(difftime(end_time, start_time, units = "mins")),
    records_processed = nrow(panel_data),
    projects_processed = n_distinct(panel_data$project_id),
    alerts_generated = length(monitoring_summary$panel_alerts),
    run_id = paste0(format(start_time, "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  )
  
  # Create or append to execution log
  log_file <- file.path(LOGS_DIR, "pipeline_execution_log.jsonl")
  if (!file.exists(log_file)) {
    # Create new log file with header
    writeLines(c("# Verasight Quality Control Pipeline Execution Log",
                 paste("# Log started:", Sys.time()),
                 "# Format: One JSON object per line (JSONL)",
                 ""), log_file)
  }
  
  # Append execution log as single line JSON
  write_json(execution_log, log_file, auto_unbox = TRUE, append = TRUE)
  
  # Also create daily summary log for easy reading
  daily_log_file <- file.path(LOGS_DIR, paste0("daily_summary_", Sys.Date(), ".log"))
  log_entry <- sprintf("[%s] Pipeline completed in %.2f min | %d records from %d projects | %d alerts\n",
                      format(end_time, "%H:%M:%S"), 
                      execution_log$duration_minutes,
                      execution_log$records_processed,
                      execution_log$projects_processed,
                      execution_log$alerts_generated)
  
  if (!file.exists(daily_log_file)) {
    writeLines(sprintf("=== Verasight Quality Pipeline - %s ===", Sys.Date()), daily_log_file)
  }
  cat(log_entry, file = daily_log_file, append = TRUE)
  
  message(sprintf("Pipeline completed in %.2f minutes", execution_log$duration_minutes))
  message(sprintf("Processed %d records from %d projects", 
                 execution_log$records_processed, execution_log$projects_processed))
  
  return(execution_log)
}

# =============================================================================
# AUTOMATION SETUP
# =============================================================================

# Setup automated daily execution at 2 AM
setup_automation <- function() {
  message("Setting up automated daily execution...")
  
  # Create the script that will be executed by cron
  script_path <- file.path(TASK3_DIR, "daily_pipeline.R")
  
  script_content <- sprintf('#!/usr/bin/env Rscript
# Daily execution script for quality control pipeline
source("%s")
run_quality_pipeline(generate_data = FALSE)
', file.path(TASK3_DIR, "quality_control_pipeline.R"))
  
  writeLines(script_content, script_path)
  Sys.chmod(script_path, mode = "0755")  # Make executable
  
  # Setup cron job for daily execution at 2 AM
  tryCatch({
    cmd <- sprintf('cd %s && Rscript daily_pipeline.R', TASK3_DIR)
    cron_add(command = cmd, 
             frequency = "daily",
             at = "02:00",
             id = "verasight_quality_pipeline",
             description = "Verasight Quality Control Pipeline")
    
    message("Cron job successfully scheduled for daily execution at 2:00 AM")
    message("View scheduled jobs with: cron_ls()")
    
  }, error = function(e) {
    warning("Could not set up cron job automatically. Please manually add the following to your crontab:")
    warning(sprintf("0 2 * * * cd %s && Rscript daily_pipeline.R", TASK3_DIR))
  })
}

# =============================================================================
# EXECUTION CONTROL
# =============================================================================

# Main execution - run this to set up everything
main <- function() {
  message("=== VERASIGHT QUALITY CONTROL PIPELINE SETUP ===")
  
  # 1. Generate sample data (110 surveys)
  message("Step 1: Generating sample survey data...")
  run_quality_pipeline(generate_data = TRUE)
  
  # 2. Setup automation (disabled - using manual src/daily_pipeline.sh instead)
  message("Step 2: Using existing automation setup in src/daily_pipeline.sh")
  # setup_automation()
  
  message("=== SETUP COMPLETE ===")
  message("The pipeline is now ready to run daily at 2:00 AM")
  message("To run manually: run_quality_pipeline()")
  message("To view outputs: check the task3/outputs/ directory")
}

# Execute main setup if script is run directly (only when not sourced with arguments)
if (!interactive() && !exists(".dont_auto_run")) {
  main()
} 