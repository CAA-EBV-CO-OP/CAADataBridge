# =============================================================================
# Data Import Module: Pure Functions for Loading MLS Data
# =============================================================================
#
# This module provides pure (non-Shiny) functions for loading property data
# from CSV, Excel, or RDS files, applying initial filters, and detecting
# response variables and auto-excluded columns.
#
# Source logic: ADF-CMS-Tool/app.R lines 640-795
# See: DATA_LAYER_KEY_BLOCKS.md Section 1
#
# Version: 1.1.0
# Phase: 2 (Import & Cleaning - Pure Functions)
#
# Changes in 1.1.0:
# - Added parse_currency_column() to handle "$1,234.56" formatted prices
# - Updated detect_response_variable to find currency-formatted columns
# - Fixed filtering to work with converted numeric columns
# =============================================================================

# Required packages (loaded by app.R)
# - data.table (fread)
# - readxl (read_excel)
# - readr (read_rds)
# - dplyr (filter, %>%)
# - tibble (as_tibble)
# - stringr (str_detect)

# Source schema registry
if (!exists("get_default_schema")) {

  source("R/shared/schema_registry.R")
}

# =============================================================================
# OUTLIER DETECTION HELPERS
# =============================================================================

#' Detect outliers using IQR method (Tukey's Fences)
#'
#' Identifies outliers using the Interquartile Range method, which is robust
#' to skewed distributions common in real estate price data.
#'
#' The IQR method defines outliers as values outside the "fences":
#' - Lower fence: Q1 - (multiplier × IQR)
#' - Upper fence: Q3 + (multiplier × IQR)
#'
#' @param x Numeric vector
#' @param multiplier IQR multiplier (default 3.0 for extreme outliers only).
#'   Common values: 1.5 (standard), 3.0 (extreme only)
#' @param na.rm Logical: remove NA values before calculation (default TRUE)
#' @return List with:
#'   \describe{
#'     \item{is_outlier}{Logical vector (TRUE = outlier)}
#'     \item{lower_fence}{Lower bound value}
#'     \item{upper_fence}{Upper bound value}
#'     \item{q1}{First quartile}
#'     \item{q3}{Third quartile}
#'     \item{iqr}{Interquartile range}
#'     \item{n_outliers}{Count of outliers detected}
#'     \item{n_lower}{Count below lower fence}
#'     \item{n_upper}{Count above upper fence}
#'   }
#' @export
#' @examples
#' prices <- c(200000, 250000, 300000, 350000, 400000, 5000000)  # One extreme
#' result <- detect_outliers_iqr(prices)
#' result$is_outlier  # FALSE FALSE FALSE FALSE FALSE TRUE
#' result$upper_fence  # The calculated upper bound
detect_outliers_iqr <- function(x, multiplier = 3.0, na.rm = TRUE) {

  # Handle edge cases
 if (!is.numeric(x)) {
    stop("Input must be numeric")
  }

  if (all(is.na(x))) {
    return(list(
      is_outlier = rep(NA, length(x)),
      lower_fence = NA,
      upper_fence = NA,
      q1 = NA,
      q3 = NA,
      iqr = NA,
      n_outliers = 0,
      n_lower = 0,
      n_upper = 0
    ))
  }

  # Calculate quartiles and IQR
  q1 <- quantile(x, 0.25, na.rm = na.rm)
  q3 <- quantile(x, 0.75, na.rm = na.rm)
  iqr <- q3 - q1

  # Calculate fences
  lower_fence <- q1 - (multiplier * iqr)
  upper_fence <- q3 + (multiplier * iqr)

  # Identify outliers (NA values are not outliers)
  is_lower <- !is.na(x) & x < lower_fence
  is_upper <- !is.na(x) & x > upper_fence
  is_outlier <- is_lower | is_upper

  list(
    is_outlier = is_outlier,
    lower_fence = as.numeric(lower_fence),
    upper_fence = as.numeric(upper_fence),
    q1 = as.numeric(q1),
    q3 = as.numeric(q3),
    iqr = as.numeric(iqr),
    n_outliers = sum(is_outlier),
    n_lower = sum(is_lower),
    n_upper = sum(is_upper)
  )
}

#' Filter outliers from a data frame based on a numeric column
#'
#' Removes rows where the specified column contains outlier values,
#' using the IQR method for robust outlier detection.
#'
#' @param data Data frame
#' @param column_name Name of numeric column to check for outliers
#' @param multiplier IQR multiplier (default 3.0 for extreme outliers only)
#' @param remove_upper Logical: remove upper outliers (default TRUE)
#' @param remove_lower Logical: remove lower outliers (default FALSE for prices)
#' @return List with:
#'   \describe{
#'     \item{data}{Filtered data frame}
#'     \item{n_removed}{Number of rows removed}
#'     \item{outlier_info}{Full outlier detection results from detect_outliers_iqr()}
#'     \item{message}{Human-readable summary message}
#'   }
#' @export
#' @examples
#' \dontrun{
#' result <- filter_outliers(adf, "PriceSold", multiplier = 3.0)
#' filtered_data <- result$data
#' cat(result$message)
#' }
filter_outliers <- function(data, column_name, multiplier = 3.0,
                            remove_upper = TRUE, remove_lower = FALSE) {

  # Validate inputs
  if (!column_name %in% names(data)) {
    return(list(
      data = data,
      n_removed = 0,
      outlier_info = NULL,
      message = sprintf("Column '%s' not found - no outlier filtering applied", column_name)
    ))
  }

  col_values <- data[[column_name]]

  if (!is.numeric(col_values)) {
    return(list(
      data = data,
      n_removed = 0,
      outlier_info = NULL,
      message = sprintf("Column '%s' is not numeric - no outlier filtering applied", column_name)
    ))
  }

  # Detect outliers
  outlier_info <- detect_outliers_iqr(col_values, multiplier = multiplier)

  # Determine which outliers to remove
  remove_mask <- rep(FALSE, nrow(data))

  if (remove_upper) {
    remove_mask <- remove_mask | (!is.na(col_values) & col_values > outlier_info$upper_fence)
  }

  if (remove_lower) {
    remove_mask <- remove_mask | (!is.na(col_values) & col_values < outlier_info$lower_fence)
  }

  n_removed <- sum(remove_mask)
  filtered_data <- data[!remove_mask, , drop = FALSE]

  # Build message
  if (n_removed > 0) {
    bounds_msg <- c()
    if (remove_upper && outlier_info$n_upper > 0) {
      bounds_msg <- c(bounds_msg, sprintf("%d above $%s",
                                          sum(!is.na(col_values) & col_values > outlier_info$upper_fence),
                                          format(round(outlier_info$upper_fence), big.mark = ",")))
    }
    if (remove_lower && outlier_info$n_lower > 0) {
      bounds_msg <- c(bounds_msg, sprintf("%d below $%s",
                                          sum(!is.na(col_values) & col_values < outlier_info$lower_fence),
                                          format(round(outlier_info$lower_fence), big.mark = ",")))
    }

    message <- sprintf(
      "Removed %d outlier(s) from %s using IQR method (%.1fx): %s",
      n_removed, column_name, multiplier, paste(bounds_msg, collapse = ", ")
    )
  } else {
    message <- sprintf(
      "No outliers detected in %s (IQR bounds: $%s - $%s)",
      column_name,
      format(round(outlier_info$lower_fence), big.mark = ","),
      format(round(outlier_info$upper_fence), big.mark = ",")
    )
  }

  list(
    data = filtered_data,
    n_removed = n_removed,
    outlier_info = outlier_info,
    message = message
  )
}

# =============================================================================
# CURRENCY PARSING HELPERS
# =============================================================================

#' Parse currency-formatted strings to numeric
#'
#' Converts strings like "$1,234.56" or "1,234" to numeric values.
#' Handles dollar signs, commas, spaces, and parentheses (for negatives).
#'
#' @param x Character vector with currency-formatted values
#' @return Numeric vector (NA for unparseable values)
#' @export
#' @examples
#' parse_currency_column(c("$1,234.56", "$2,900,000.00", "500000", NA, ""))
#' # Returns: c(1234.56, 2900000, 500000, NA, NA)
parse_currency_column <- function(x) {
  if (is.numeric(x)) return(x)

  # Convert to character if factor
  x <- as.character(x)

  # Handle NA and empty strings
  x[x == "" | is.na(x)] <- NA

  # Remove currency symbols, commas, spaces
  cleaned <- gsub("[$,\\s]", "", x)

  # Handle parentheses as negative (accounting format)
  is_negative <- grepl("^\\(.*\\)$", cleaned)
  cleaned <- gsub("[()]", "", cleaned)

  # Convert to numeric
  result <- suppressWarnings(as.numeric(cleaned))

  # Apply negative sign where needed
  result[is_negative] <- -result[is_negative]

  result
}

#' Check if a character column contains currency-formatted values
#'
#' Returns TRUE if column appears to contain currency values like "$1,234.56"
#'
#' @param x Character vector to check
#' @param sample_size Number of non-NA values to check (default 100)
#' @return Logical
#' @export
is_currency_column <- function(x, sample_size = 100) {
  if (is.numeric(x)) return(FALSE)
  if (!is.character(x) && !is.factor(x)) return(FALSE)


  x <- as.character(x)

  # Get non-NA, non-empty values
  valid_values <- x[!is.na(x) & x != ""]
  if (length(valid_values) == 0) return(FALSE)

  # Sample if too many values
  if (length(valid_values) > sample_size) {
    valid_values <- sample(valid_values, sample_size)
  }

  # Check for currency patterns: starts with $, or has commas in numbers
 currency_pattern <- "^\\$|^\\(?\\$?[0-9]{1,3}(,[0-9]{3})+(\\.\\d{2})?\\)?$"

  # At least 50% should match currency pattern
  match_rate <- mean(grepl(currency_pattern, valid_values))

  match_rate >= 0.5
}

#' Convert currency columns in a data frame to numeric
#'
#' Finds columns that match price patterns and contain currency formatting,
#' then converts them to numeric.
#'
#' @param data Data frame
#' @param schema Schema registry for price pattern matching
#' @return List with:
#'   \describe{
#'     \item{data}{Data frame with converted columns}
#'     \item{converted}{Character vector of column names that were converted}
#'   }
#' @export
convert_currency_columns <- function(data, schema = get_default_schema()) {

  converted <- character(0)
  price_pattern <- paste(schema$patterns$price, collapse = "|")

  for (col_name in names(data)) {
    col <- data[[col_name]]

    # Skip if already numeric
    if (is.numeric(col)) next

    # Check if column name suggests it's a price AND contains currency formatting
    is_price_name <- grepl(price_pattern, col_name, ignore.case = TRUE)
    has_currency <- is_currency_column(col)

    if (is_price_name && has_currency) {
      data[[col_name]] <- parse_currency_column(col)
      converted <- c(converted, col_name)
    }
  }

  list(data = data, converted = converted)
}

#' Import ADF file with initial preprocessing
#'
#' Pure function that replicates the data import logic from app.R
#' (observeEvent(input$adf_file, ...) handler) without Shiny dependencies.
#'
#' Handles CSV (via fread), Excel (.xlsx), and RDS files.
#' Applies initial outlier filtering based on price threshold.
#' Detects response variable and auto-excluded columns using schema registry.
#'
#' @param path Character string: full path to data file
#' @param price_cap Numeric: maximum price threshold (outliers removed).
#'   Defaults to schema threshold for price_outlier_max.
#' @param price_col Character: name of price column to filter.
#'   Defaults to schema threshold for price_column_name.
#' @param schema Schema registry from get_default_schema().
#'   Used for NA strings, pattern matching, response detection.
#'
#' @return Named list with:
#'   \describe{
#'     \item{adf}{Data frame: imported data after outlier filter}
#'     \item{adf_transformed}{Data frame: working copy (initially identical to adf)}
#'     \item{response_var}{Character: detected response variable name, or NA}
#'     \item{auto_excluded}{Character vector: columns excluded by auto-exclusion patterns}
#'     \item{messages}{Character vector: status messages (replaces Shiny notifications)}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' result <- import_adf_file("data/sample.csv")
#' result$adf  # Imported data
#' result$response_var  # Detected price column
#' result$auto_excluded  # Columns auto-excluded from analysis
#' cat(result$messages, sep = "\n")  # Status messages
#' }
import_adf_file <- function(
  path,
  price_cap = get_threshold("price_outlier_max"),
  price_col = get_threshold("price_column_name"),
  schema = get_default_schema()
) {

  # Initialize return structure
  result <- list(
    adf = NULL,
    adf_transformed = NULL,
    response_var = NA_character_,
    auto_excluded = character(0),
    messages = character(0)
  )

  # Validate inputs
  if (!file.exists(path)) {
    result$messages <- c(result$messages, paste("Error: File not found:", path))
    return(result)
  }

  # =========================================================================
  # STEP 1: Load data based on file extension
  # =========================================================================
  # Source: app.R lines 651-684

  file_ext <- tools::file_ext(path)

  adf <- tryCatch({

    if (file_ext == "csv") {
      # Use fread() from data.table - MUCH more robust for malformed CSVs
      # Handles quoted fields with commas, missing values, etc.
      # Source: app.R lines 654-675

      data <- data.table::fread(
        path,
        data.table = FALSE,       # Return as data.frame, not data.table
        stringsAsFactors = FALSE,
        na.strings = schema$type_patterns$na_strings,  # Use schema NA strings
        fill = TRUE,              # Fill rows with unequal columns
        quote = "\""              # Handle quoted fields properly
      )

      # Convert to tibble for consistency with tidyverse
      data <- tibble::as_tibble(data)

      # Success message
      result$messages <- c(
        result$messages,
        sprintf("Successfully loaded %d rows and %d columns using robust CSV parser",
                nrow(data), ncol(data))
      )

      data

    } else if (file_ext == "xlsx") {
      # Excel loading
      # Source: app.R line 677

      data <- readxl::read_excel(path)

      result$messages <- c(
        result$messages,
        sprintf("Successfully loaded %d rows and %d columns from Excel file",
                nrow(data), ncol(data))
      )

      data

    } else if (file_ext == "rds") {
      # RDS loading
      # Source: app.R line 679

      data <- readr::read_rds(path)

      result$messages <- c(
        result$messages,
        sprintf("Successfully loaded %d rows and %d columns from RDS file",
                nrow(data), ncol(data))
      )

      data

    } else {
      stop("Unsupported file type: '", file_ext, "'. Supported: csv, xlsx, rds")
    }

  }, error = function(e) {
    result$messages <<- c(result$messages, paste("Error loading file:", e$message))
    NULL
  })

  # If loading failed, return early
  if (is.null(adf)) {
    return(result)
  }

  # =========================================================================
  # STEP 1B: Convert currency-formatted columns to numeric
  # =========================================================================
  # Handles columns like PriceSold containing "$1,234,567.00" format
  # Must happen BEFORE response variable detection

  currency_result <- convert_currency_columns(adf, schema)
  adf <- currency_result$data

  if (length(currency_result$converted) > 0) {
    result$messages <- c(
      result$messages,
      sprintf("Converted %d currency column(s) to numeric: %s",
              length(currency_result$converted),
              paste(currency_result$converted, collapse = ", "))
    )
  }

  # =========================================================================
  # STEP 2: Detect response variable (moved earlier to enable filtering)
  # =========================================================================
  # Source: app.R lines 707-720
  # Now delegated to schema_registry.R

  response_var <- detect_response_variable(adf, schema)

  if (is.na(response_var)) {
    result$messages <- c(
      result$messages,
      "Warning: No price column detected. Please manually select response variable."
    )
  } else {
    result$messages <- c(
      result$messages,
      sprintf("Detected response variable: %s", response_var)
    )
  }

  # =========================================================================
  # STEP 3: Filter out records with missing response variable (e.g., listings)
  # =========================================================================
  # Records without a sale price cannot be used for CMS analysis

  if (!is.na(response_var) && response_var %in% names(adf)) {
    original_rows <- nrow(adf)
    response_col <- adf[[response_var]]

    # Build filter based on column type
    if (is.numeric(response_col)) {
      # Numeric column: filter NA and zero/negative values
      valid_rows <- !is.na(response_col) & response_col > 0
    } else {
      # Character column: filter NA, empty strings, and non-positive after parsing
      parsed <- parse_currency_column(response_col)
      valid_rows <- !is.na(parsed) & parsed > 0
    }

    adf <- adf[valid_rows, , drop = FALSE]
    missing_response_count <- original_rows - nrow(adf)

    if (missing_response_count > 0) {
      result$messages <- c(
        result$messages,
        sprintf("Filtered out %d record(s) with missing or zero %s (e.g., active listings)",
                missing_response_count,
                response_var)
      )
    }
  }

  # =========================================================================
  # STEP 4: Apply Charlie's filter - Remove extreme outliers
  # =========================================================================
  # Source: app.R lines 686-699

  if (price_col %in% names(adf) && is.numeric(adf[[price_col]])) {
    original_rows <- nrow(adf)

    # Apply filter: PriceSold < price_cap (only if column is numeric)
    valid_rows <- !is.na(adf[[price_col]]) & adf[[price_col]] < price_cap
    adf <- adf[valid_rows, , drop = FALSE]

    filtered_rows <- nrow(adf)

    if (filtered_rows < original_rows) {
      result$messages <- c(
        result$messages,
        sprintf("Filtered out %d outlier(s) with %s >= $%s (Charlie's methodology)",
                original_rows - filtered_rows,
                price_col,
                scales::comma(price_cap))
      )
    }
  }

  # =========================================================================
  # STEP 5: Initialize transformed data state
  # =========================================================================
  # Source: app.R line 702

  adf_transformed <- adf

  # =========================================================================
  # STEP 6: Apply auto-exclusion patterns
  # =========================================================================
  # Source: app.R lines 723-746
  # Now delegated to schema_registry.R

  all_vars <- names(adf_transformed)

  # Get auto-selected columns (those NOT excluded)
  auto_selected <- apply_auto_exclusion(all_vars, schema)

  # Determine which columns were excluded
  auto_excluded <- setdiff(all_vars, auto_selected)

  if (length(auto_excluded) > 0) {
    result$messages <- c(
      result$messages,
      sprintf("Auto-excluded %d variables based on schema patterns", length(auto_excluded)),
      sprintf("Excluded: %s", paste(head(auto_excluded, 10), collapse = ", "))
    )
  } else {
    result$messages <- c(
      result$messages,
      "No variables auto-excluded"
    )
  }

  # =========================================================================
  # STEP 6: Assemble return value
  # =========================================================================

  result$adf <- adf
  result$adf_transformed <- adf_transformed
  result$response_var <- response_var
  result$auto_excluded <- auto_excluded

  # Add final success message
  result$messages <- c(
    result$messages,
    "Data loaded successfully!"
  )

  result
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get summary statistics for imported data
#'
#' @param import_result Output from import_adf_file()
#' @return Named list with summary statistics
#' @export
summarize_import <- function(import_result) {

  if (is.null(import_result$adf)) {
    return(list(
      success = FALSE,
      error_message = paste(import_result$messages, collapse = "; ")
    ))
  }

  list(
    success = TRUE,
    n_rows = nrow(import_result$adf),
    n_cols = ncol(import_result$adf),
    n_numeric = sum(sapply(import_result$adf, is.numeric)),
    n_character = sum(sapply(import_result$adf, is.character)),
    n_factor = sum(sapply(import_result$adf, is.factor)),
    response_var = import_result$response_var,
    n_auto_excluded = length(import_result$auto_excluded),
    messages = import_result$messages
  )
}

#' Validate import result structure
#'
#' Checks that import_adf_file() returned expected components.
#'
#' @param import_result Output from import_adf_file()
#' @return Logical (TRUE if valid, stops with error otherwise)
#' @export
validate_import_result <- function(import_result) {

  required_names <- c("adf", "adf_transformed", "response_var", "auto_excluded", "messages")
  missing <- setdiff(required_names, names(import_result))

  if (length(missing) > 0) {
    stop("Import result missing components: ", paste(missing, collapse = ", "))
  }

  # Check types
  if (!is.null(import_result$adf) && !is.data.frame(import_result$adf)) {
    stop("import_result$adf must be a data.frame or NULL")
  }

  if (!is.null(import_result$adf_transformed) && !is.data.frame(import_result$adf_transformed)) {
    stop("import_result$adf_transformed must be a data.frame or NULL")
  }

  if (!is.character(import_result$response_var) || length(import_result$response_var) != 1) {
    stop("import_result$response_var must be a single character value (or NA)")
  }

  if (!is.character(import_result$auto_excluded)) {
    stop("import_result$auto_excluded must be a character vector")
  }

  if (!is.character(import_result$messages)) {
    stop("import_result$messages must be a character vector")
  }

  TRUE
}

#' Print import result summary
#'
#' @param import_result Output from import_adf_file()
#' @export
print_import_summary <- function(import_result) {

  cat("\n=== Data Import Summary ===\n\n")

  if (is.null(import_result$adf)) {
    cat("Status: FAILED\n")
    cat("\nMessages:\n")
    cat(paste("  -", import_result$messages), sep = "\n")
    return(invisible(NULL))
  }

  cat("Status: SUCCESS\n")
  cat(sprintf("Rows: %d\n", nrow(import_result$adf)))
  cat(sprintf("Columns: %d\n", ncol(import_result$adf)))
  cat(sprintf("Response Variable: %s\n",
              if (is.na(import_result$response_var)) "Not detected" else import_result$response_var))
  cat(sprintf("Auto-Excluded: %d columns\n", length(import_result$auto_excluded)))

  cat("\nMessages:\n")
  cat(paste("  -", import_result$messages), sep = "\n")

  if (length(import_result$auto_excluded) > 0) {
    cat("\nExcluded Columns:\n")
    cat(paste("  -", head(import_result$auto_excluded, 20)), sep = "\n")
    if (length(import_result$auto_excluded) > 20) {
      cat(sprintf("  ... and %d more\n", length(import_result$auto_excluded) - 20))
    }
  }

  cat("\n")
  invisible(import_result)
}

# =============================================================================
# PACKAGE DOCUMENTATION
# =============================================================================

#' Data Import Module
#'
#' Pure functions for loading MLS property data from CSV, Excel, or RDS files.
#'
#' @section Main Function:
#' \code{\link{import_adf_file}} - Load and preprocess property data
#'
#' @section Helper Functions:
#' \itemize{
#'   \item \code{\link{summarize_import}} - Get summary statistics
#'   \item \code{\link{validate_import_result}} - Validate return structure
#'   \item \code{\link{print_import_summary}} - Print formatted summary
#' }
#'
#' @section Usage:
#' \preformatted{
#' # Basic usage
#' result <- import_adf_file("data/properties.csv")
#'
#' # Access imported data
#' data <- result$adf
#' working_copy <- result$adf_transformed
#'
#' # Check what was detected
#' result$response_var      # Detected price column
#' result$auto_excluded     # Excluded columns
#' result$messages          # Status messages
#'
#' # Print summary
#' print_import_summary(result)
#' }
#'
#' @docType package
#' @name data_import
NULL
