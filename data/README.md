# 📊 Data Directory

This directory contains the survey datasets required for the Verasight Panel Analytics dashboard.

## 🔒 Data Security Notice

**The actual survey data files are not included in this repository** for privacy and security reasons. They are listed in `.gitignore` to prevent accidental commits.

## 📁 Required Data Files

To run this project locally, you need the following files in this directory:

### Core Survey Data
- `users.rds` - Panel member demographics and registration information
- `2024-054_responses.rds` - Survey responses with demographic variables and weights
- `2024-054_reference.rds` - Question labels and response coding metadata
- `full-response-db.rds` - Complete longitudinal response database

### Assets
- `www/logo_verasight.png` - Verasight logo for dashboard branding

## 📋 Data Schema Overview

### users.rds
```r
# Key columns:
- respondent_id: Unique user identifier
- signup_date: Registration date (MM/DD/YYYY format)
- utm_source: Recruitment channel
- vf_match: Voter file verification status
```

### 2024-054_responses.rds  
```r
# Key columns:
- respondent_id: Links to users.rds
- weight: Post-stratification survey weight
- age_group4, education, gender, raceeth: Demographics for weighting
- pid_base, region: Additional demographic variables
- q28-q35: Survey question responses
```

### 2024-054_reference.rds
```r
# Key columns:
- variable: Question variable name (e.g., "q28")
- label: Human-readable question text
- value_labels: Response option coding
```

### full-response-db.rds
```r
# Key columns:
- ID: User identifier
- survey: Survey project identifier  
- response_date: When response was submitted
```

## 🚀 Getting Sample Data

If you need sample data for testing:

1. **Contact the project maintainer** for access to anonymized sample datasets
2. **Generate synthetic data** using the data generation scripts in `tasks/task3/src/`
3. **Use the live dashboard** at https://jacksonzzc.shinyapps.io/verasight-panel-insights/

## 🔧 Data Processing Notes

- All data files use R's RDS format for efficient storage and type preservation
- Survey weights are already calculated and included in the responses file
- Missing values are handled gracefully by the analysis scripts
- Data is processed using tidyverse functions for consistency

## 📈 Data Volume

- **Users**: ~100K panel members
- **Responses**: ~500K survey responses  
- **Projects**: 110+ survey projects analyzed
- **Time Range**: Multi-year longitudinal data

## 🛡️ Privacy & Compliance

- All personally identifiable information (PII) has been removed
- Data is anonymized with hashed user identifiers
- Complies with survey research privacy standards
- No raw contact information or sensitive demographics included
