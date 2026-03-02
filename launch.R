# ============================================================================
# CAA Collaborative - Robust Launcher
# Based on CAADataBridge launch_mapper.R
# ============================================================================

# Required packages for the combined app
required_packages <- c(
  # Core
  "shiny", "bslib", "tidyverse", "shinyjs",
  # DataBridge
  "shinyWidgets", "shinybusy", "DT", "jsonlite", "ggplot2", "scales",
  "readxl", "readr", "mongolite", "digest",
  # Decision Trees
  "qeML", "gt", "plotly", "lubridate", "writexl", "data.table"
)

# Check for missing packages
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  cat("Missing packages:\n")
  cat(paste(" -", missing, collapse = "\n"), "\n\n")
  cat("Install with:\n")
  cat(sprintf('install.packages(c(%s))\n',
    paste0('"', missing, '"', collapse = ", ")))
  stop("Please install missing packages before launching.")
}

# Always resolve to the directory containing THIS launch.R file
app_dir <- tryCatch({
  # When source()'d, ofile gives us the script path
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # Try RStudio API (works when launched via RStudio's Source button)
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    tryCatch({
      dirname(rstudioapi::getSourceEditorContext()$path)
    }, error = function(e2) NULL)
  } else NULL
})

# Validate: the resolved directory must contain R/host/ (CAACollaborative signature)
if (is.null(app_dir) || !dir.exists(file.path(app_dir, "R", "host"))) {
  # Hard-coded fallback to this project's known location
  app_dir <- "C:/Users/pryac/PR-Repo/CAACollaborative"
  if (!dir.exists(file.path(app_dir, "R", "host"))) {
    stop("Cannot find CAACollaborative app directory. Please setwd() to the project root.")
  }
}

# Set working directory to app root so all relative paths work
setwd(app_dir)

cat("========================================\n")
cat("  CAA Collaborative\n")
cat("========================================\n")
cat(sprintf("  Directory: %s\n", normalizePath(app_dir)))
cat(sprintf("  R version: %s\n", R.version.string))
cat("========================================\n\n")

# Launch
shiny::runApp(app_dir, port = 4545, launch.browser = TRUE)
