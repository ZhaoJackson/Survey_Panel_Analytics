#!/bin/bash
# Daily Pipeline Automation Script for Verasight Quality Control
# This script ensures proper environment and paths for cron execution

# Set script directory (where this script is located)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TASK3_DIR="$(dirname "$SCRIPT_DIR")"

# Log file for cron debugging
LOG_FILE="$TASK3_DIR/logs/cron_execution.log"

# Create logs directory if it doesn't exist
mkdir -p "$TASK3_DIR/logs"

# Function to log messages
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$LOG_FILE"
}

# Start logging
log_message "=== Daily Pipeline Execution Started ==="
log_message "Script directory: $SCRIPT_DIR"
log_message "Task3 directory: $TASK3_DIR"

# Change to the src directory
cd "$SCRIPT_DIR"
log_message "Changed to directory: $(pwd)"

# Set R environment (in case cron doesn't have proper PATH)
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

# Run the daily pipeline
log_message "Executing daily_pipeline.R..."
if Rscript daily_pipeline.R >> "$LOG_FILE" 2>&1; then
    log_message "✅ Daily pipeline completed successfully"
else
    log_message "❌ Daily pipeline failed with exit code $?"
fi

log_message "=== Daily Pipeline Execution Completed ==="
log_message "" 