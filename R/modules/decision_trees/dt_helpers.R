# ============================================================================
# Decision Trees - Helper Functions
# Extracted from app.R lines 30-219
# ============================================================================

#' Create a consistent Inspect button ID for a column
#'
#' This ensures all Inspect buttons (in quality cards, classification UI, etc.)
#' use the same ID convention that the server-side handler expects.
#'
#' @param column_name Character string: the raw column name (spaces allowed)
#' @return Character string: the input ID for the Inspect button
make_inspect_id <- function(column_name) {
  paste("inspect", column_name, sep = "_")
}

#' Get column summary statistics
#'
#' Returns appropriate summary statistics based on column type:
#' - Numeric: min, max, mean, median, NA count
#' - Categorical/Text: top 10 most frequent values with counts
#'
#' @param data Data frame containing the column
#' @param colname Character string: name of column to summarize
#' @return List with summary information
get_column_summary <- function(data, colname) {

  if (!colname %in% names(data)) {
    return(list(
      error = TRUE,
      message = paste("Column", colname, "not found in data")
    ))
  }

  col_data <- data[[colname]]
  n_total <- length(col_data)
  n_na <- sum(is.na(col_data))
  n_valid <- n_total - n_na

  summary_info <- list(
    error = FALSE,
    column_name = colname,
    n_total = n_total,
    n_na = n_na,
    n_valid = n_valid,
    pct_na = round(100 * n_na / n_total, 1),
    class = class(col_data)[1]
  )

  if (is.numeric(col_data)) {
    valid_data <- col_data[!is.na(col_data)]

    if (length(valid_data) > 0) {
      summary_info$type <- "numeric"
      summary_info$min <- min(valid_data, na.rm = TRUE)
      summary_info$max <- max(valid_data, na.rm = TRUE)
      summary_info$mean <- mean(valid_data, na.rm = TRUE)
      summary_info$median <- median(valid_data, na.rm = TRUE)
      summary_info$sd <- sd(valid_data, na.rm = TRUE)
    } else {
      summary_info$type <- "numeric"
      summary_info$message <- "All values are NA"
    }

  } else {
    summary_info$type <- "categorical"

    valid_data <- col_data[!is.na(col_data)]

    if (length(valid_data) > 0) {
      freq_table <- table(valid_data, useNA = "no")
      freq_table <- sort(freq_table, decreasing = TRUE)

      top_n <- min(10, length(freq_table))
      top_values <- head(freq_table, top_n)

      summary_info$n_unique <- length(freq_table)
      summary_info$top_values <- data.frame(
        value = names(top_values),
        count = as.integer(top_values),
        pct = round(100 * as.integer(top_values) / length(valid_data), 1),
        stringsAsFactors = FALSE
      )
    } else {
      summary_info$message <- "All values are NA"
      summary_info$n_unique <- 0
    }
  }

  summary_info
}

#' Get subject-defining columns from field classification
#'
#' Returns columns suitable for defining a subject property.
#' Includes core/extended columns but excludes:
#' - Response variable (price being predicted)
#' - Ignored columns (IDs, addresses, remarks)
#' - Temporal market variables (DOM, SPAP, SaleQtr) unless explicitly included
#' - Metadata columns (.Mapper_*, version, SHA, etc.)
#' - Geo coordinates (longitude, latitude - too precise for subject matching)
#'
#' @param field_classification Data frame from classify_variables_for_tree()
#' @param response_var Character: name of response variable to exclude
#' @param include_temporal Logical: whether to include time-based market variables
#' @return Character vector of column names suitable for subject definition
get_subject_columns <- function(field_classification, response_var = NULL, include_temporal = FALSE) {

  if (is.null(field_classification) || nrow(field_classification) == 0) {
    return(character(0))
  }

  candidate_cols <- field_classification$name[
    field_classification$relevance %in% c("core", "extended")
  ]

  if (!is.null(response_var) && response_var != "") {
    candidate_cols <- setdiff(candidate_cols, response_var)
  }

  if (!include_temporal) {
    temporal_cols <- field_classification$name[
      field_classification$role == "time" &
      tolower(field_classification$name) %in% c("dom", "days on market", "spap", "sp/ap", "saleqtr", "sale quarter")
    ]
    candidate_cols <- setdiff(candidate_cols, temporal_cols)
  }

  schema <- get_default_schema()
  exclusion_patterns <- c(
    schema$patterns$metadata,
    schema$patterns$geo_coordinates
  )
  exclusion_regex <- paste(exclusion_patterns, collapse = "|")

  candidate_cols <- candidate_cols[!grepl(exclusion_regex, candidate_cols, ignore.case = TRUE)]

  candidate_cols
}

#' Detect and convert numeric-formatted character columns
#'
#' Identifies character columns that contain numeric data (with optional
#' formatting like commas, dollar signs) and converts them to numeric.
#'
#' @param data Data frame to process
#' @return Data frame with numeric-formatted character columns converted
convert_numeric_formatted_strings <- function(data) {
  char_cols <- names(data)[sapply(data, is.character)]

  for (col in char_cols) {
    sample_vals <- data[[col]][!is.na(data[[col]])]
    if (length(sample_vals) == 0) next

    sample_vals <- head(sample_vals, 100)

    cleaned <- gsub("[,[:space:]]", "", sample_vals)
    numeric_vals <- suppressWarnings(as.numeric(cleaned))

    success_rate <- sum(!is.na(numeric_vals)) / length(cleaned)

    if (success_rate > 0.9) {
      data[[col]] <- as.numeric(gsub("[,[:space:]]", "", data[[col]]))
      message(sprintf("Converted '%s' from character to numeric (%.1f%% numeric values)",
                      col, success_rate * 100))
    }
  }

  data
}
