#!/usr/bin/env Rscript
# Quick launcher for CValR Column Mapper
#
# Usage:
#   Rscript launch_mapper.R
#   or from R console: source("launch_mapper.R")

library(shiny)

# Improve error visibility when sourcing the app file
options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)
options(warn = 1)

truthy <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(default)
  }
  if (!nzchar(value)) {
    return(default)
  }
  tolower(trimws(value)) %in% c("1", "true", "t", "yes", "y", "on", "merge", "merge_first")
}

launch_merge_mapper <- function(script_dir) {
  merge_file <- file.path(script_dir, "merge_mapper.R")
  if (!file.exists(merge_file)) {
    message(sprintf("Merge Mapper script not found at %s; skipping merge step.", merge_file))
    return(FALSE)
  }

  cat("\n================ Merge Mapper ================\n")
  cat("Launching helper to combine files before ColumnMapper...\n")
  tryCatch(
    {
      source(merge_file, local = new.env(parent = globalenv()))
      cat("\nMerge Mapper closed. Returning to ColumnMapper...\n")
      TRUE
    },
    error = function(e) {
      message(sprintf("Merge Mapper failed: %s", e$message))
      FALSE
    }
  )
}

maybe_launch_merge_first <- function(script_dir) {
  env_choice <- tolower(trimws(Sys.getenv("CVALR_MERGE_MODE", "")))
  if (env_choice %in% c("merge", "merge_first", "merge-mapper", "mergemapper")) {
    return(launch_merge_mapper(script_dir))
  }
  if (env_choice %in% c("skip", "column", "no", "false", "0")) {
    return(FALSE)
  }

  can_prompt <- FALSE
  if ("isatty" %in% ls(baseenv())) {
    can_prompt <- tryCatch(isatty(stdin()), error = function(e) FALSE)
  }
  if (!can_prompt) {
    return(FALSE)
  }

  cat("\nNeed to merge multiple files before mapping?\n")
  cat("Merge Mapper can combine a primary file with comps data, then return here.\n")
  answer <- tolower(trimws(readline("Launch Merge Mapper first? [y/N]: ")))
  if (answer %in% c("y", "yes", "merge", "m")) {
    return(launch_merge_mapper(script_dir))
  }
  FALSE
}

# Lightweight Git freshness check (best-effort; never stops launch)
git_freshness_check <- function(repo_dir, remote = "origin", branch = "main", do_fetch = TRUE) {
  res <- list(status = "unknown", head = NA_character_, remote = NA_character_, ahead = NA_integer_, behind = NA_integer_, message = "")
  git <- Sys.which("git")
  if (!nzchar(git)) {
    res$message <- "git not found on PATH; skipping freshness check"
    return(res)
  }

  safe_git <- function(args, ...) {
    tryCatch(
      {
        out <- suppressWarnings(system2(git, c("-C", repo_dir, args), stdout = TRUE, stderr = TRUE))
        # system2 returns character vector; collapse lines
        paste(out, collapse = "\n")
      },
      error = function(e) ""
    )
  }

  # Ensure we're in a Git work tree
  if (!identical(tolower(trimws(safe_git(c("rev-parse", "--is-inside-work-tree")))), "true")) {
    res$message <- "Not a git work tree; skipping freshness check"
    return(res)
  }

  # Optionally fetch to update remote refs (best-effort)
  if (isTRUE(do_fetch)) {
    invisible(safe_git(c("fetch", "--quiet", remote)))
  }

  head_sha <- trimws(safe_git(c("rev-parse", "--short", "HEAD")))
  remote_ref <- paste0(remote, "/", branch)
  remote_sha <- trimws(safe_git(c("rev-parse", "--short", remote_ref)))

  res$head <- if (nzchar(head_sha)) head_sha else NA_character_
  res$remote <- if (nzchar(remote_sha)) remote_sha else NA_character_

  if (!nzchar(remote_sha)) {
    res$status <- "no-remote"
    res$message <- sprintf("No remote ref %s found; skipping freshness check", remote_ref)
    return(res)
  }

  lr <- trimws(safe_git(c("rev-list", "--left-right", "--count", "HEAD...", remote_ref)))
  # Expected format: "<ahead>\t<behind>"
  parts <- strsplit(lr, "\t| ")[[1]]
  if (length(parts) >= 2) {
    ahead <- suppressWarnings(as.integer(parts[1]))
    behind <- suppressWarnings(as.integer(parts[2]))
    if (is.na(ahead)) ahead <- 0
    if (is.na(behind)) behind <- 0
    res$ahead <- ahead
    res$behind <- behind
    if (ahead == 0 && behind == 0) {
      res$status <- "up-to-date"
      res$message <- sprintf("Repo is up-to-date with %s (%s)", remote_ref, remote_sha)
    } else if (behind > 0 && ahead == 0) {
      res$status <- "behind"
      res$message <- sprintf(
        "WARNING: Local HEAD %s is behind %s by %d commit(s); remote at %s\nRun: git pull",
        head_sha, remote_ref, behind, remote_sha
      )
    } else if (ahead > 0 && behind == 0) {
      res$status <- "ahead"
      res$message <- sprintf(
        "NOTE: Local HEAD %s is ahead of %s by %d commit(s).",
        head_sha, remote_ref, ahead
      )
    } else {
      res$status <- "diverged"
      res$message <- sprintf(
        "WARNING: Local HEAD %s and %s have diverged (ahead %d, behind %d). Consider pull/rebase.",
        head_sha, remote_ref, ahead, behind
      )
    }
  } else {
    res$message <- sprintf("Could not compute ahead/behind for %s", remote_ref)
  }

  res
}

# Load centralized version helper
safe_source <- function(path) tryCatch(sys.source(path, envir = environment()), error = function(e) NULL)
script_dir_guess <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)), error = function(e) getwd())
safe_source(file.path(script_dir_guess, "version.R"))
# Verify required packages are installed before sourcing the app
required_pkgs <- c("shiny", "shinyWidgets", "DT", "jsonlite", "ggplot2", "scales", "readxl", "readr", "tidygeocoder")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing) > 0) {
  cat("Missing required packages: ", paste(missing, collapse = ", "), "\n", sep = "")
  cat("Install with: install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))\n", sep = "")
  stop(sprintf("Missing packages: %s", paste(missing, collapse = ", ")), call. = FALSE)
}

# Check Python/rapidfuzz availability for optimized matching
check_python_setup <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cat("[Performance] reticulate not installed - using R-only matching\n")
    cat("             For faster matching: install.packages('reticulate')\n")
    return(FALSE)
  }

  tryCatch({
    if (!reticulate::py_module_available("rapidfuzz")) {
      cat("[Performance] rapidfuzz not installed - using R-only matching\n")
      cat("             For faster matching: pip install rapidfuzz\n")
      return(FALSE)
    }
    cat("[Performance] rapidfuzz available - optimized matching enabled\n")
    return(TRUE)
  }, error = function(e) {
    cat("[Performance] Python check failed - using R-only matching\n")
    return(FALSE)
  })
}

# Run Python check (informational only - app works without it)
check_python_setup()

# Get the directory where this script is located (robust to source/Rscript/interactive)
get_script_dir <- function() {
  p <- NULL
  # Try common frames (works for source/withCallingHandlers)
  for (i in 1:5) {
    p <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(p)) break
  }
  if (is.null(p)) {
    # Fallback to commandArgs() (Rscript) or current working dir
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
    if (length(file_arg)) p <- file_arg[1]
  }
  if (is.null(p)) {
    return(getwd())
  }
  dirname(normalizePath(p, mustWork = FALSE))
}

script_dir <- get_script_dir()
# Source version helper from the resolved script_dir (robust even if initial guess failed)
safe_source(file.path(script_dir, "version.R"))
# Use split-file app structure (ui.R, server.R, global.R) for modular development
# Previously used single-file: file.path(script_dir, "column_mapper.R")
app_dir <- script_dir

# Offer to launch the Merge Mapper helper before the main app (optional)
maybe_launch_merge_first(script_dir)

# Force local mode by default (unset MONGODB_URI) unless explicitly enabled
# This prevents leaking environment variables from other projects causing auth errors
if (!truthy(Sys.getenv("CVALR_ENABLE_MONGO"))) {
  if (nzchar(Sys.getenv("MONGODB_URI"))) {
    cat("Note: MONGODB_URI detected but CVALR_ENABLE_MONGO not set.\n")
    cat("      Unsetting MONGODB_URI to enforce local-only mode (as per README).\n")
    cat("      To enable MongoDB, set CVALR_ENABLE_MONGO=true\n")
    Sys.unsetenv("MONGODB_URI")
  }
}

# Derive build info (VERSION.md + Git), and export to env for the app
derive_build_info <- function(repo_dir) {
  # Parse VERSION.md
  ver <- "dev"
  dt <- as.character(Sys.Date())
  vfile <- file.path(repo_dir, "VERSION.md")
  if (file.exists(vfile)) {
    lines <- tryCatch(readLines(vfile, warn = FALSE), error = function(e) character())
    if (length(lines)) {
      idx <- grep("Current Version:", lines, fixed = TRUE)
      if (length(idx)) {
        vv <- sub(".*Current Version:\\s*", "", lines[idx[1]])
        vv <- gsub("\\*", "", vv)
        vv <- trimws(vv)
        if (grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", vv)) ver <- vv
      }
      idx2 <- grep("Last Updated:", lines, fixed = TRUE)
      if (length(idx2)) {
        dd <- sub(".*Last Updated:\\s*", "", lines[idx2[1]])
        dd <- gsub("\\*", "", dd)
        dd <- trimws(dd)
        if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dd)) dt <- dd
      }
    }
  }
  # Git short SHA and tag
  sha <- "unknown"
  tag <- ""
  git <- Sys.which("git")
  if (nzchar(git)) {
    sha_out <- tryCatch(system2(git, c("-C", repo_dir, "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) character())
    sha_out <- sha_out[nzchar(sha_out)]
    if (length(sha_out)) sha <- sha_out[1]
    tag_pts <- tryCatch(system2(git, c("-C", repo_dir, "tag", "--points-at", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) character())
    tag_pts <- tag_pts[nzchar(tag_pts)]
    if (length(tag_pts)) {
      tag <- tag_pts[1]
    } else {
      tag_desc <- tryCatch(system2(git, c("-C", repo_dir, "describe", "--tags", "--abbrev=0"), stdout = TRUE, stderr = FALSE), error = function(e) character())
      tag_desc <- tag_desc[nzchar(tag_desc)]
      if (length(tag_desc)) tag <- tag_desc[1]
    }
  }
  # Export to environment for the Shiny app to consume
  Sys.setenv(CVALR_APP_VERSION = ver)
  Sys.setenv(CVALR_APP_VERSION_DATE = dt)
  Sys.setenv(CVALR_APP_BUILD_SHA = sha)
  Sys.setenv(CVALR_APP_BUILD_TAG = tag)
  list(version = ver, date = dt, sha = sha, tag = tag)
}

build <- derive_build_info(script_dir)
ver <- build

# Launch the app with better diagnostics
cat("Launching CValR Column Mapper...\n")
cat(sprintf("Version: %s  Build: %s  Date: %s\n", ver$version, ver$sha, ver$date))
cat("App directory: ", app_dir, "\n", sep = "")
cat("Press Ctrl+C to stop\n\n")

# Pre-parse to surface syntax errors before Shiny wraps them
cat("Pre-parsing app files (global.R, ui.R, server.R)...\n")
tryCatch(
  {
    for (f in c("global.R", "ui.R", "server.R")) {
      fpath <- file.path(app_dir, f)
      if (file.exists(fpath)) {
        parse(file = fpath)
        cat(sprintf("  %s: OK\n", f))
      } else {
        cat(sprintf("  %s: NOT FOUND\n", f))
      }
    }
    cat("Parse OK. Starting Shiny...\n\n")
  },
  error = function(e) {
    cat("Parse error:\n", conditionMessage(e), "\n", sep = "")
    stop(conditionMessage(e), call. = FALSE)
  }
)

# Run a best-effort Git freshness check and print guidance
try(
  {
    check <- git_freshness_check(repo_dir = script_dir, remote = "origin", branch = "main", do_fetch = TRUE)
    if (nzchar(check$message)) {
      cat("[Repo Status] ", check$message, "\n", sep = "")
    }
    # Expose status to the Shiny app for in-UI notification
    if (!is.null(check$status)) Sys.setenv(CVALR_GIT_STATUS_LEVEL = as.character(check$status))
    if (!is.null(check$message)) Sys.setenv(CVALR_GIT_STATUS_MSG = as.character(check$message))
    # Build info already exported above via derive_build_info()
    if (!is.null(ver$tag)) Sys.setenv(CVALR_APP_BUILD_TAG = as.character(ver$tag))
  },
  silent = TRUE
)
tryCatch(
  {
    # Run the split-file app (ui.R, server.R, global.R in app_dir)
    runApp(app_dir, port = getOption("shiny.port", 4545), launch.browser = TRUE)
  },
  error = function(e) {
    cat("\nrunApp error:\n")
    cat(conditionMessage(e), "\n")
    if (exists("traceback", envir = baseenv())) try(traceback(), silent = TRUE)
    stop(conditionMessage(e), call. = FALSE)
  }
)
