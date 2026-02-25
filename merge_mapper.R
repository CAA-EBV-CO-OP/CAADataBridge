#!/usr/bin/env Rscript

# CValR Merge Mapper — lightweight helper to combine two datasets before running the
# main ColumnMapper app. Upload two files, choose join keys (single or composite),
# merge, review results, and export an RFC 4180-compliant CSV.

required_pkgs <- c("shiny", "shinyWidgets", "DT", "readr", "readxl", "dplyr")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing)) {
  cat("Missing required packages: ", paste(missing, collapse = ", "), "\n", sep = "")
  cat("Install with: install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))\n", sep = "")
  stop("Missing required packages for Merge Mapper.")
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(readr)
  library(readxl)
  library(dplyr)
  library(tools)
})

options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)
options(warn = 1)

`%||%` <- function(a, b) {
  if (!is.null(a)) {
    if (is.logical(a) && length(a) == 1 && is.na(a)) return(b)
    if (length(a) > 0) return(a)
  }
  b
}

guess_sale_date_column <- function(columns) {
  candidates <- c("DateSold", "SaleDate", "SoldDate", "CloseDate", "ClosingDate", "DateClose")
  for (cand in candidates) {
    if (cand %in% columns) return(cand)
  }
  for (col in columns) {
    if (grepl("date", col, ignore.case = TRUE)) return(col)
  }
  NULL
}

compute_sale_quarter <- function(values) {
  parsed <- suppressWarnings(as.Date(values))
  if (all(is.na(parsed))) return(rep(NA_real_, length(values)))
  years <- as.integer(format(parsed, "%Y"))
  months <- as.integer(format(parsed, "%m"))
  quarters <- ceiling(months / 3)
  sale_qtr <- rep(NA_real_, length(values))
  valid <- !is.na(years) & !is.na(quarters)
  sale_qtr[valid] <- years[valid] + (quarters[valid] / 10)
  sale_qtr
}

merge_address_components <- function(df, columns, separator = " ") {
  if (!length(columns)) return(character(nrow(df)))
  columns <- columns[columns %in% names(df)]
  if (!length(columns)) return(character(nrow(df)))
  apply(df[, columns, drop = FALSE], 1, function(row) {
    vals <- trimws(as.character(row))
    vals[is.na(vals)] <- ""
    vals <- vals[nzchar(vals)]
    if (!length(vals)) return("")
    paste(vals, collapse = separator)
  })
}

classify_kind <- function(vec) {
  if (inherits(vec, "Date")) return("date")
  if (inherits(vec, "POSIXt")) return("datetime")
  if (is.numeric(vec)) return("numeric")
  if (is.logical(vec)) return("logical")
  "text"
}

count_non_empty <- function(vec) {
  if (inherits(vec, c("Date", "POSIXt"))) {
    return(sum(!is.na(vec)))
  }
  if (is.numeric(vec) || is.logical(vec)) {
    return(sum(!is.na(vec)))
  }
  vec_chr <- trimws(as.character(vec))
  vec_chr[is.na(vec_chr)] <- ""
  sum(nzchar(vec_chr), na.rm = TRUE)
}

calc_coverage <- function(data, columns) {
  if (!length(columns)) {
    return(data.frame(Column = character(), NonEmpty = integer(), Coverage = numeric()))
  }
  rows <- lapply(columns, function(col) {
    vec <- data[[col]]
    total <- length(vec)
    non_empty <- count_non_empty(vec)
    pct <- if (total > 0) round((non_empty / total) * 100, 1) else 0
    data.frame(
      Column = col,
      NonEmpty = non_empty,
      Coverage = pct,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

analyze_column_rules <- function(name, values) {
  warnings <- character()
  lower <- tolower(name)
  values_chr <- trimws(as.character(values))
  values_chr[is.na(values_chr)] <- ""

  if (grepl("price|amount|value|ppsf|ppa", lower)) {
    numeric_vals <- suppressWarnings(as.numeric(values_chr))
    invalid <- sum(!is.na(numeric_vals) & numeric_vals <= 0)
    if (invalid > 0) {
      warnings <- c(warnings, sprintf("%s: %d value(s) <= 0", name, invalid))
    }
  }

  if (grepl("date", lower)) {
    non_empty <- values_chr[nzchar(values_chr)]
    parsed <- suppressWarnings(as.Date(non_empty))
    invalid <- sum(is.na(parsed))
    if (invalid > 0) {
      warnings <- c(warnings, sprintf("%s: %d date value(s) failed to parse", name, invalid))
    }
  }

  if (grepl("lat", lower)) {
    numeric_vals <- suppressWarnings(as.numeric(values_chr))
    invalid <- sum(!is.na(numeric_vals) & (numeric_vals < -90 | numeric_vals > 90))
    if (invalid > 0) {
      warnings <- c(warnings, sprintf("%s: %d latitude value(s) outside [-90, 90]", name, invalid))
    }
  }

  if (grepl("lon|lng|long", lower)) {
    numeric_vals <- suppressWarnings(as.numeric(values_chr))
    invalid <- sum(!is.na(numeric_vals) & (numeric_vals < -180 | numeric_vals > 180))
    if (invalid > 0) {
      warnings <- c(warnings, sprintf("%s: %d longitude value(s) outside [-180, 180]", name, invalid))
    }
  }

  if (grepl("sqft|acre|lot|area", lower)) {
    numeric_vals <- suppressWarnings(as.numeric(values_chr))
    invalid <- sum(!is.na(numeric_vals) & numeric_vals <= 0)
    if (invalid > 0) {
      warnings <- c(warnings, sprintf("%s: %d size value(s) <= 0", name, invalid))
    }
  }

  warnings
}

format_for_compare <- function(vec) {
  if (inherits(vec, "Date")) {
    return(format(vec, "%Y-%m-%d"))
  }
  out <- trimws(as.character(vec))
  out[is.na(out)] <- ""
  out
}

safe_head <- function(vec, n = 8) {
  if (length(vec) <= n) {
    vec
  } else {
    c(vec[seq_len(n)], sprintf("… (+%d more)", length(vec) - n))
  }
}

guess_key_column <- function(columns) {
  if (!length(columns)) return(NULL)
  normalized <- tolower(columns)
  patterns <- c("mls", "listingid", "listing_id", "propertyid", "pid", "parcel", "id")
  for (pattern in patterns) {
    match_idx <- which(normalized == pattern)
    if (length(match_idx)) return(columns[match_idx[1]])
  }
  NULL
}

read_dataset_file <- function(file_info) {
  req(file_info)
  ext <- tolower(file_ext(file_info$name))
  datapath <- file_info$datapath

  message(sprintf("Loading %s (%s)", file_info$name, ext))
  if (ext %in% c("csv", "txt")) {
    df <- readr::read_csv(datapath, show_col_types = FALSE, guess_max = 5000)
  } else if (ext %in% c("tsv")) {
    df <- readr::read_tsv(datapath, show_col_types = FALSE, guess_max = 5000)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(datapath, guess_max = 5000)
  } else {
    stop(sprintf("Unsupported file type: %s", ext))
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

normalize_text_vector <- function(vec) {
  out <- vec
  if (inherits(out, "Date")) {
    out <- format(out, "%Y-%m-%d")
  } else if (inherits(out, "POSIXt")) {
    out <- format(out, "%Y-%m-%d %H:%M:%S")
  }
  out <- as.character(out)
  out[is.na(out)] <- ""
  out <- trimws(out)
  tolower(gsub("\\s+", " ", out))
}

prepare_dataset_for_join <- function(df, columns) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  key_col <- ".mm_join_key"

  if (!length(columns)) {
    stop("Please select at least one column to use as the join key.")
  }

  missing <- setdiff(columns, names(df))
  if (length(missing)) {
    stop(sprintf("Column(s) %s not found in dataset.", paste(missing, collapse = ", ")))
  }

  normalized <- lapply(columns, function(col) normalize_text_vector(df[[col]]))
  if (length(normalized) == 1) {
    df[[key_col]] <- normalized[[1]]
  } else {
    combined <- do.call(cbind, normalized)
    df[[key_col]] <- apply(combined, 1, function(row) paste(row, collapse = "||"))
  }

  df
}

dataset_summary_block <- function(df, label) {
  if (is.null(df)) {
    return(tags$div(class = "alert alert-secondary", sprintf("Upload %s data to begin.", label)))
  }

  cols <- names(df)
  tagList(
    tags$div(
      class = "dataset-summary",
      tags$strong(sprintf("%s rows × %s columns", nrow(df), ncol(df))),
      tags$div(
        class = "text-muted",
        sprintf("Showing %s of %s columns:", min(length(cols), 8), length(cols))
      ),
      tags$code(paste(safe_head(cols, 8), collapse = ", "))
    )
  )
}

perform_merge <- function(primary, merge_df, primary_cols, merge_cols, include_cols, join_type, suffix) {
  if (!length(primary_cols)) stop("Select at least one join column from the primary dataset.")
  if (!length(merge_cols)) stop("Select at least one join column from the secondary dataset.")

  join_type <- match.arg(join_type, choices = c("left", "inner", "right", "full"))

  primary_prepared <- prepare_dataset_for_join(primary, primary_cols)
  merge_prepared <- prepare_dataset_for_join(merge_df, merge_cols)

  merge_prepared$.mm_merge_marker <- TRUE

  if (!length(include_cols)) {
    include_cols <- setdiff(names(merge_prepared), c(".mm_join_key", ".mm_merge_marker"))
  }

  missing_includes <- setdiff(include_cols, names(merge_prepared))
  if (length(missing_includes)) {
    stop(sprintf("Selected merge columns missing from dataset: %s", paste(missing_includes, collapse = ", ")))
  }

  duplicate_summary <- function(df) {
    dup <- df %>%
      dplyr::count(.mm_join_key, name = "Occurrences")
    dup <- dup[dup$Occurrences > 1, , drop = FALSE]
    if (!nrow(dup)) return(NULL)
    dup <- as.data.frame(dup, stringsAsFactors = FALSE)
    names(dup)[names(dup) == ".mm_join_key"] <- "JoinKey"
    dup
  }

  dup_primary <- duplicate_summary(primary_prepared)
  dup_secondary <- duplicate_summary(merge_prepared)

  keep_cols <- unique(c(include_cols, ".mm_join_key", ".mm_merge_marker"))
  merge_trimmed <- merge_prepared[, keep_cols, drop = FALSE]

  suffix <- if (nzchar(suffix)) suffix else "_merge"

  merged <- switch(
    join_type,
    left = dplyr::left_join(primary_prepared, merge_trimmed, by = ".mm_join_key", suffix = c("", suffix)),
    inner = dplyr::inner_join(primary_prepared, merge_trimmed, by = ".mm_join_key", suffix = c("", suffix)),
    right = dplyr::right_join(primary_prepared, merge_trimmed, by = ".mm_join_key", suffix = c("", suffix)),
    full = dplyr::full_join(primary_prepared, merge_trimmed, by = ".mm_join_key", suffix = c("", suffix))
  )

  matched <- sum(!is.na(merged$.mm_merge_marker) & merged$.mm_merge_marker, na.rm = TRUE)
  unmatched_primary <- sum(is.na(merged$.mm_merge_marker))

  key_values <- merged$.mm_join_key
  drop_cols <- c(".mm_merge_marker", ".mm_join_key")
  y_join_col <- paste0(".mm_join_key", suffix)
  if (y_join_col %in% names(merged)) {
    drop_cols <- c(drop_cols, y_join_col)
  }
  drop_cols <- intersect(drop_cols, names(merged))
  if (length(drop_cols)) {
    merged <- merged[, setdiff(names(merged), drop_cols), drop = FALSE]
  }

  merged <- as.data.frame(merged, stringsAsFactors = FALSE)

  appended_cols <- setdiff(names(merged), names(primary))
  coverage_df <- calc_coverage(merged, appended_cols)
  quality_warnings <- unlist(lapply(appended_cols, function(col) analyze_column_rules(col, merged[[col]])))

  overlap_cols <- intersect(include_cols, names(primary))
  type_warnings <- quality_warnings
  diff_details <- list()
  if (length(overlap_cols)) {
    for (col in overlap_cols) {
      appended_name <- paste0(col, suffix)
      if (!(appended_name %in% names(merged))) next
      kind_primary <- classify_kind(primary[[col]])
      kind_secondary <- classify_kind(merged[[appended_name]])
      if (!identical(kind_primary, kind_secondary)) {
        type_warnings <- c(
          type_warnings,
          sprintf("%s type mismatch (%s vs %s)", col, kind_primary, kind_secondary)
        )
      }

      primary_vec <- merged[[col]]
      secondary_vec <- merged[[appended_name]]
      primary_fmt <- format_for_compare(primary_vec)
      secondary_fmt <- format_for_compare(secondary_vec)
      mask <- nzchar(primary_fmt) & nzchar(secondary_fmt) & primary_fmt != secondary_fmt
      mask[is.na(mask)] <- FALSE
      diff_count <- sum(mask, na.rm = TRUE)
      if (diff_count > 0) {
        sample_rows <- head(which(mask), 10)
        diff_details[[col]] <- list(
          total = diff_count,
          sample = data.frame(
            JoinKey = key_values[sample_rows],
            Primary = primary_vec[sample_rows],
            Secondary = secondary_vec[sample_rows],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  list(
    data = merged,
    stats = list(
      matched_rows = matched,
      total_rows = nrow(merged),
      primary_rows = nrow(primary),
      secondary_rows = nrow(merge_df),
      unmatched_primary = unmatched_primary,
      columns_added = length(include_cols),
      join_type = join_type
    ),
    diagnostics = list(
      duplicates_primary = dup_primary,
      duplicates_secondary = dup_secondary,
      coverage = coverage_df,
      type_warnings = unique(type_warnings),
      diff_samples = diff_details,
      merged_source = list(
        data = merged,
        coverage = coverage_df
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("Merge Mapper"),
    tags$style(HTML("
      body { background: #f5f7fb; }
      .dataset-summary { background: #fff; border: 1px solid #d8dee9; border-radius: 6px; padding: 12px; margin-top: 6px; }
      .merge-controls { background: #fff; border: 1px solid #d8dee9; border-radius: 6px; padding: 15px; }
      .merge-summary { background: #fff; border: 1px solid #d8dee9; border-radius: 6px; padding: 15px; margin-bottom: 15px; }
      .merge-summary h4 { margin-top: 0; }
      .quality-section { background: #fff; border: 1px solid #d8dee9; border-radius: 6px; padding: 15px; margin-bottom: 20px; }
    "))
  ),
  titlePanel(div("Merge Mapper CValR")),
  fluidRow(
    column(
      width = 6,
      h4("Primary Dataset (Base)"),
      fileInput("primary_file", "Upload primary file (CSV or Excel)", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
      uiOutput("primary_summary"),
      DTOutput("primary_preview")
    ),
    column(
      width = 6,
      h4("Secondary Dataset (Enrichment)"),
      fileInput("secondary_file", "Upload secondary file (CSV or Excel)", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
      uiOutput("secondary_summary"),
      DTOutput("secondary_preview")
    )
  ),
  hr(),
  div(
    class = "merge-controls",
    fluidRow(
      column(
        4,
        selectizeInput(
          "primary_key",
          "Primary join column(s)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select one or more columns")
        )
      ),
      column(
        4,
        selectizeInput(
          "secondary_key",
          "Secondary join column(s)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select one or more columns")
        )
      ),
      column(
        4,
        selectInput(
          "join_type",
          "Join type",
          choices = c(
            "Left (keep all primary rows)" = "left",
            "Inner (matched rows only)" = "inner",
            "Full outer (all rows from both)" = "full",
            "Right (keep all secondary rows)" = "right"
          ),
          selected = "left"
        )
      )
    ),
    fluidRow(
      column(
        6,
        pickerInput(
          "merge_columns_include",
          "Columns to bring in from secondary dataset",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            title = "Select columns to append"
          )
        ),
        helpText("Leave empty to include all non-key columns from the secondary dataset.")
      ),
      column(
        3,
        textInput("merge_suffix", "Suffix for overlapping columns", value = "_comp")
      )
    ),
    fluidRow(
      column(
        6,
        actionButton("run_merge", label = "Run merge", icon = icon("cogs"), class = "btn btn-success")
      ),
      column(
        6,
        align = "right",
        downloadButton("download_merge", "Download merged CSV", class = "btn btn-primary"),
        actionButton("reset_merge", "Clear merged data", class = "btn btn-link")
      )
    )
  ),
  hr(),
  uiOutput("merge_summary"),
  div(
    class = "quality-section",
    h4("Data Quality Diagnostics"),
    fluidRow(
      column(
        6,
        h5("Duplicate join keys (primary dataset)"),
        DTOutput("duplicate_primary_table")
      ),
      column(
        6,
        h5("Duplicate join keys (secondary dataset)"),
        DTOutput("duplicate_secondary_table")
      )
    ),
    h5("Coverage of appended columns"),
    DTOutput("coverage_table"),
    h5("Warnings & rule checks"),
    uiOutput("type_warnings_ui"),
    h5("Conflicting values (primary vs secondary)"),
    uiOutput("conflict_samples_ui")
  ),
  div(
    class = "quality-section",
    h4("Derived Columns (optional)"),
    fluidRow(
      column(
        6,
        checkboxInput("derive_sale_qtr", "Add SaleQtr (year.quarter from a sale date column)", value = FALSE),
        selectInput("sale_qtr_source", "Sale date column", choices = NULL, selected = NULL)
      ),
      column(
        6,
        checkboxInput("derive_address_merge", "Add merged Address column", value = FALSE),
        selectizeInput(
          "address_components",
          "Address components (select 2+)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Choose columns to concatenate")
        ),
        textInput("address_separator", "Separator (default is a single space)", value = " ")
      )
    ),
    actionButton("apply_derived", "Apply derived columns", class = "btn btn-secondary"),
    uiOutput("derived_summary")
  ),
  DTOutput("merged_preview")
)

server <- function(input, output, session) {
  primary_data <- reactiveVal(NULL)
  secondary_data <- reactiveVal(NULL)
    merged_data <- reactiveVal(NULL)
    merge_stats <- reactiveVal(NULL)
    merge_diagnostics <- reactiveVal(list())
    derived_columns <- reactiveVal(list())

  observeEvent(input$primary_file, {
    tryCatch({
      df <- read_dataset_file(input$primary_file)
      primary_data(df)
      updateSelectizeInput(session, "primary_key", choices = names(df), selected = guess_key_column(names(df)))
    }, error = function(e) {
      showNotification(paste("Failed to load primary dataset:", e$message), type = "error", duration = 8)
      primary_data(NULL)
    })
  }, ignoreNULL = TRUE)

  observeEvent(input$secondary_file, {
    tryCatch({
      df <- read_dataset_file(input$secondary_file)
      secondary_data(df)
      guessed <- guess_key_column(names(df))
      updateSelectizeInput(session, "secondary_key", choices = names(df), selected = guessed)
      default_pick <- intersect(names(df), c("Notes", "Design Style", "DesignStyle", "Condition"))
      if (!length(default_pick)) {
        default_pick <- safe_head(setdiff(names(df), guessed %||% character()), 6)
      }
      updatePickerInput(
        session,
        "merge_columns_include",
        choices = names(df),
        selected = default_pick
      )
    }, error = function(e) {
      showNotification(paste("Failed to load secondary dataset:", e$message), type = "error", duration = 8)
      secondary_data(NULL)
    })
  }, ignoreNULL = TRUE)

  output$primary_summary <- renderUI({
    dataset_summary_block(primary_data(), "your primary")
  })
  output$secondary_summary <- renderUI({
    dataset_summary_block(secondary_data(), "your secondary")
  })

  output$primary_preview <- renderDT({
    req(primary_data())
    datatable(
      head(primary_data(), 10),
      options = list(pageLength = 5, scrollX = TRUE),
      class = "display compact nowrap"
    )
  })

  output$secondary_preview <- renderDT({
    req(secondary_data())
    datatable(
      head(secondary_data(), 10),
      options = list(pageLength = 5, scrollX = TRUE),
      class = "display compact nowrap"
    )
  })

  observeEvent(input$run_merge, {
    req(primary_data(), secondary_data())
    showNotification("Running merge...", type = "message", duration = 5)

    tryCatch({
      include_cols <- input$merge_columns_include %||% character()
      result <- perform_merge(
        primary = primary_data(),
        merge_df = secondary_data(),
        primary_cols = input$primary_key,
        merge_cols = input$secondary_key,
        include_cols = include_cols,
        join_type = input$join_type,
        suffix = input$merge_suffix %||% "_comp"
      )

      merged_data(result$data)
      merge_stats(result$stats)
      merge_diagnostics(result$diagnostics %||% list())
      derived_columns(list())

      showNotification(
        sprintf("Merge complete: %d rows (%d matched).", result$stats$total_rows, result$stats$matched_rows),
        type = "message",
        duration = 6
      )

      cols <- names(result$data)
      updateSelectInput(session, "sale_qtr_source", choices = cols, selected = guess_sale_date_column(cols))
      updateSelectizeInput(session, "address_components", choices = cols, selected = character())
    }, error = function(e) {
      showNotification(paste("Merge failed:", e$message), type = "error", duration = 10)
    })
  })

  observeEvent(input$reset_merge, {
    merged_data(NULL)
    merge_stats(NULL)
    merge_diagnostics(list())
    derived_columns(list())
    updateSelectInput(session, "sale_qtr_source", choices = character(), selected = NULL)
    updateSelectizeInput(session, "address_components", choices = character(), selected = character())
    updateCheckboxInput(session, "derive_sale_qtr", value = FALSE)
    updateCheckboxInput(session, "derive_address_merge", value = FALSE)
  })

  output$merge_summary <- renderUI({
    stats <- merge_stats()
    if (is.null(stats)) {
      return(tags$div(
        class = "alert alert-info",
        icon("info-circle"), " Upload files, choose join columns, then click \"Run merge\" to preview results."
      ))
    }

    tagList(
      div(
        class = "merge-summary",
        h4("Merge summary"),
        tags$p(sprintf(
          "%s join completed. %d total rows, %d matched rows, %d unmatched primary rows.",
          tools::toTitleCase(stats$join_type),
          stats$total_rows,
          stats$matched_rows,
          stats$unmatched_primary
        )),
        tags$ul(
          tags$li(sprintf("Primary dataset: %d rows", stats$primary_rows)),
          tags$li(sprintf("Secondary dataset: %d rows", stats$secondary_rows)),
          tags$li(sprintf("Columns appended from secondary: %d", stats$columns_added))
        )
      )
    )
  })

  render_dup_table <- function(dataset = c("primary", "secondary")) {
    dataset <- match.arg(dataset)
    diag <- merge_diagnostics()
    tbl <- if (dataset == "primary") diag$duplicates_primary else diag$duplicates_secondary
    if (is.null(tbl) || !nrow(tbl)) {
      return(datatable(
        data.frame(Message = "No duplicate keys detected"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    datatable(
      head(tbl, 100),
      options = list(dom = "ftip", pageLength = 5, searching = FALSE),
      rownames = FALSE
    )
  }

  output$duplicate_primary_table <- renderDT({
    render_dup_table("primary")
  })

  output$duplicate_secondary_table <- renderDT({
    render_dup_table("secondary")
  })

  output$coverage_table <- renderDT({
    diag <- merge_diagnostics()
    coverage <- diag$coverage
    if (is.null(coverage) || !nrow(coverage)) {
      return(datatable(
        data.frame(Message = "No appended columns to summarize"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    datatable(
      coverage,
      options = list(dom = "ftip", pageLength = 5, searching = FALSE),
      rownames = FALSE
    )
  })

  output$type_warnings_ui <- renderUI({
    diag <- merge_diagnostics()
    warnings <- diag$type_warnings
    if (is.null(warnings) || !length(warnings)) {
      return(tags$div(class = "text-muted", "No warnings detected."))
    }
    tags$ul(
      lapply(warnings, function(msg) tags$li(msg))
    )
  })

  output$conflict_samples_ui <- renderUI({
    diag <- merge_diagnostics()
    samples <- diag$diff_samples
    if (is.null(samples) || !length(samples)) {
      return(tags$div(class = "text-muted", "No conflicting values detected."))
    }
    tagList(
      lapply(names(samples), function(col) {
        info <- samples[[col]]
        sample_df <- info$sample
        display_rows <- apply(sample_df, 1, function(row) {
          sprintf("%s → primary: %s | secondary: %s", row["JoinKey"], row["Primary"], row["Secondary"])
        })
        tagList(
          tags$strong(sprintf("%s (%d differing row%s)", col, info$total, if (info$total == 1) "" else "s")),
          tags$pre(if (length(display_rows)) paste(display_rows, collapse = "\n") else "No sample rows captured.")
        )
      })
    )
  })

  observeEvent(input$apply_derived, {
    df <- merged_data()
    req(df)

    added <- list()
    updated_df <- df

    if (isTRUE(input$derive_sale_qtr)) {
      sale_col <- input$sale_qtr_source
      if (nzchar(sale_col) && sale_col %in% names(updated_df)) {
        updated_df$SaleQtr <- compute_sale_quarter(updated_df[[sale_col]])
        added <- c(added, sprintf("SaleQtr from %s", sale_col))
      } else {
        showNotification("Select a valid sale date column for SaleQtr.", type = "warning", duration = 5)
      }
    }

    if (isTRUE(input$derive_address_merge)) {
      addr_cols <- input$address_components %||% character()
      valid_cols <- addr_cols[addr_cols %in% names(updated_df)]
      if (length(valid_cols) >= 2) {
        sep <- input$address_separator %||% " "
        if (!nzchar(sep)) sep <- " "
        updated_df$Address_Merged <- merge_address_components(updated_df, valid_cols, separator = sep)
        added <- c(added, sprintf("Address_Merged from %s (sep '%s')", paste(valid_cols, collapse = ", "), sep))
      } else {
        showNotification("Select at least two valid address components.", type = "warning", duration = 5)
      }
    }

    merged_data(updated_df)
    derived_columns(list(
      added = added,
      timestamp = Sys.time()
    ))

    if (length(added)) {
      showNotification(sprintf("Added derived column(s): %s", paste(added, collapse = "; ")), type = "message", duration = 6)
    } else {
      showNotification("No derived columns added.", type = "warning", duration = 4)
    }
  })

  output$derived_summary <- renderUI({
    info <- derived_columns()
    if (is.null(info) || !length(info$added)) {
      return(tags$div(class = "text-muted", "No derived columns applied yet."))
    }
    tagList(
      tags$p(
        sprintf("Derived columns applied at %s:", format(info$timestamp, "%Y-%m-%d %H:%M:%S"))
      ),
      tags$ul(lapply(info$added, tags$li))
    )
  })

  output$merged_preview <- renderDT({
    req(merged_data())
    datatable(
      merged_data(),
      options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"),
      class = "display compact nowrap"
    )
  })

  output$download_merge <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("merged_data_", timestamp, ".csv")
    },
    content = function(file) {
      req(merged_data())
      readr::write_csv(merged_data(), file, na = "", quote = "needed", eol = "\n")
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
