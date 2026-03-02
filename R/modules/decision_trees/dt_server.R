# ============================================================================
# Decision Trees - Server Logic
# Extracted from app.R lines 899-5458
# Wrapped as a function for Shiny module use
# ============================================================================

dt_server_logic <- function(input, output, session, mapped_data = NULL) {
  # Namespace function for dynamic UI IDs
  ns <- session$ns

  # Source helpers (make_inspect_id, get_column_summary, etc.)
  source(file.path("R", "modules", "decision_trees", "dt_helpers.R"), local = TRUE)

  rv <- reactiveValues(
    adf = NULL,
    adf_transformed = NULL,      # New: Holds the user-modified data
    field_classification = NULL, # Field role and relevance classification
    data_quality_issues = NULL, # Stores results of proactive validation
    dt_model = NULL,
    dt_model_base = NULL,      # Base model without Condition/DesignStyle
    dt_model_enhanced = NULL,  # Enhanced model with complete data only
    model_source_row_ids = NULL, # Row IDs in rv$adf_transformed used to train rv$dt_model
    cms = NULL,
    subject = NULL,
    subject_node = NULL,
    model_comparison = NULL,    # Stores comparison metrics
    selected_property = NULL,   # Subject property loaded from data
    selected_row_id = NULL,     # Source row ID selected via quick pick
    subject_fresh_load = FALSE, # Flag: TRUE when property just loaded, resets after UI renders
    observed_fix_buttons = character(0), # Track attached observers for dynamic buttons
    observed_inspect_buttons = character(0), # Track attached observers for inspect buttons
    observed_preview_buttons = character(0), # Track attached observers for preview buttons
    autoload_triggered = FALSE # Flag to prevent auto-load from running twice
  )

  # ==========================================================================
  # Auto-load from host (reactive data from DataBridge module)
  # ==========================================================================
  observe({
    if (!is.null(mapped_data)) {
      data <- mapped_data()
      req(data)
      if (rv$autoload_triggered) return()
      rv$autoload_triggered <- TRUE

      tryCatch({
        rv$adf <- data
        rv$model_source_row_ids <- NULL

        showNotification(
          sprintf("Loaded %d records from Column Mapper", nrow(data)),
          type = "message", duration = 8
        )

        # Currency conversion
        if (!is.null(rv$adf)) {
          currency_result <- convert_currency_columns(rv$adf, get_default_schema())
          rv$adf <- currency_result$data
          if (length(currency_result$converted) > 0) {
            showNotification(
              sprintf("Converted %d currency column(s) to numeric", length(currency_result$converted)),
              type = "message", duration = 5
            )
          }
        }

        # Filter missing response values
        if (!is.null(rv$adf)) {
          all_vars <- names(rv$adf)
          if ("PriceSold" %in% all_vars) {
            detected_response <- "PriceSold"
          } else {
            numeric_vars <- all_vars[sapply(rv$adf, is.numeric)]
            detected_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
            if (is.na(detected_response)) {
              detected_response <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
            }
          }

          if (!is.na(detected_response) && detected_response %in% names(rv$adf)) {
            original_rows <- nrow(rv$adf)
            response_col <- rv$adf[[detected_response]]
            if (is.numeric(response_col)) {
              keep_rows <- !is.na(response_col) & response_col > 0
            } else {
              response_col_numeric <- parse_currency_column(response_col)
              keep_rows <- !is.na(response_col_numeric) & response_col_numeric > 0
            }
            rv$adf <- rv$adf[keep_rows, ]
            missing_count <- original_rows - nrow(rv$adf)
            if (missing_count > 0) {
              showNotification(
                sprintf("Filtered %d record(s) with missing %s", missing_count, detected_response),
                type = "warning", duration = 8
              )
            }
          }
        }

        # Outlier filtering
        if (!is.null(rv$adf) && "PriceSold" %in% names(rv$adf) && is.numeric(rv$adf$PriceSold)) {
          schema <- get_default_schema()
          outlier_result <- filter_outliers(rv$adf, "PriceSold",
            multiplier = schema$thresholds$outlier_iqr_multiplier,
            remove_upper = schema$thresholds$outlier_remove_upper,
            remove_lower = schema$thresholds$outlier_remove_lower)
          rv$adf <- outlier_result$data
        }

        # Initialize transformed data and field classification
        rv$adf_transformed <- rv$adf
        if (!is.null(rv$adf_transformed)) {
          all_vars <- names(rv$adf_transformed)
          numeric_vars <- all_vars[sapply(rv$adf_transformed, is.numeric)]
          likely_price <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
          if (is.na(likely_price)) {
            likely_price <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
          }

          updateSelectInput(session, "response_var", choices = all_vars, selected = likely_price)

          rv$field_classification <- classify_variables_for_tree(
            data = rv$adf_transformed,
            response_var = likely_price,
            schema = get_default_schema()
          )

          fc <- rv$field_classification
          rv$selected_vars <- fc$name[fc$relevance %in% c("core", "extended")]
          rv$auto_excluded <- fc$name[fc$relevance == "ignore"]
        }

      }, error = function(e) {
        showNotification(paste("Data load error:", e$message), type = "error", duration = 10)
      })
    }
  })

  # ==========================================================================
  # Auto-load from file (standalone fallback - check ~/.cvalr/)
  # ==========================================================================

  observe({
    # Only run once
    if (rv$autoload_triggered) return()

    # Check fixed autoload location (avoids command line argument issues)
    autoload_path <- file.path(Sys.getenv("USERPROFILE"), ".cvalr", "columnmapper_autoload.csv")

    if (file.exists(autoload_path)) {
      rv$autoload_triggered <- TRUE

      # Delete the file after reading to prevent stale data on next launch
      on.exit(unlink(autoload_path), add = TRUE)

      # Load the CSV file
      tryCatch({
        data <- fread(autoload_path,
                     data.table = FALSE,
                     stringsAsFactors = FALSE,
                     na.strings = c("", "NA", "N/A", "null"),
                     fill = TRUE,
                     quote = "\"")
        data <- as_tibble(data)

        # Store in rv$adf (this triggers the same downstream processing as fileInput)
        rv$adf <- data
        rv$model_source_row_ids <- NULL

        # Show notification that data was auto-loaded
        showNotification(
          sprintf("Auto-loaded %d records from ColumnMapper", nrow(data)),
          type = "message",
          duration = 8
        )

        # Now run the same processing pipeline as fileInput observer
        # Currency conversion
        if (!is.null(rv$adf)) {
          currency_result <- convert_currency_columns(rv$adf, get_default_schema())
          rv$adf <- currency_result$data
          if (length(currency_result$converted) > 0) {
            showNotification(
              sprintf("Converted %d currency column(s) to numeric", length(currency_result$converted)),
              type = "message", duration = 5
            )
          }
        }

        # Filter missing response values
        if (!is.null(rv$adf)) {
          all_vars <- names(rv$adf)
          if ("PriceSold" %in% all_vars) {
            detected_response <- "PriceSold"
          } else {
            numeric_vars <- all_vars[sapply(rv$adf, is.numeric)]
            detected_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
            if (is.na(detected_response)) {
              detected_response <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
            }
          }

          if (!is.na(detected_response) && detected_response %in% names(rv$adf)) {
            original_rows <- nrow(rv$adf)
            response_col <- rv$adf[[detected_response]]
            if (is.numeric(response_col)) {
              keep_rows <- !is.na(response_col) & response_col > 0
            } else {
              response_col_numeric <- parse_currency_column(response_col)
              keep_rows <- !is.na(response_col_numeric) & response_col_numeric > 0
            }
            rv$adf <- rv$adf[keep_rows, ]
            missing_count <- original_rows - nrow(rv$adf)
            if (missing_count > 0) {
              showNotification(
                sprintf("Filtered %d record(s) with missing %s", missing_count, detected_response),
                type = "warning", duration = 8
              )
            }
          }
        }

        # Outlier filtering
        if (!is.null(rv$adf) && "PriceSold" %in% names(rv$adf) && is.numeric(rv$adf$PriceSold)) {
          schema <- get_default_schema()
          outlier_result <- filter_outliers(rv$adf, "PriceSold",
            multiplier = schema$thresholds$outlier_iqr_multiplier,
            remove_upper = schema$thresholds$outlier_remove_upper,
            remove_lower = schema$thresholds$outlier_remove_lower)
          rv$adf <- outlier_result$data
        }

        # Initialize transformed data and run field classification
        rv$adf_transformed <- rv$adf
        if (!is.null(rv$adf_transformed)) {
          all_vars <- names(rv$adf_transformed)
          numeric_vars <- all_vars[sapply(rv$adf_transformed, is.numeric)]
          likely_price <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
          if (is.na(likely_price)) {
            likely_price <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
          }

          updateSelectInput(session, "response_var", choices = all_vars, selected = likely_price)

          rv$field_classification <- classify_variables_for_tree(
            data = rv$adf_transformed,
            response_var = likely_price,
            schema = get_default_schema()
          )

          fc <- rv$field_classification
          rv$selected_vars <- fc$name[fc$relevance %in% c("core", "extended")]
          rv$auto_excluded <- fc$name[fc$relevance == "ignore"]

          # Run data quality validation
          if (!is.null(rv$adf) && nrow(rv$adf) > 0) {
            guessed_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
            guessed_predictors <- if (!is.na(guessed_response)) setdiff(names(rv$adf), guessed_response) else names(rv$adf)
            rv$data_quality_issues <- validate_data_for_tree(
              data = rv$adf, response_var = if (!is.na(guessed_response)) guessed_response else NULL,
              predictors = guessed_predictors, selected_vars = rv$selected_vars,
              field_classification = rv$field_classification
            )
          }
        }

      }, error = function(e) {
        showNotification(paste("Auto-load error:", e$message), type = "error", duration = 10)
      })
    }
  })

  # ==========================================================================
  # Data Loading (manual file upload)
  # ==========================================================================

  observeEvent(input$adf_file, {
    req(input$adf_file)

    # Clear ALL previous analysis results when new data is loaded
    rv$adf <- NULL
    rv$adf_transformed <- NULL
    rv$field_classification <- NULL
    rv$data_quality_issues <- NULL
    rv$dt_model <- NULL
    rv$dt_model_base <- NULL
    rv$dt_model_enhanced <- NULL
    rv$model_source_row_ids <- NULL
    rv$cms <- NULL
    rv$subject <- NULL
    rv$subject_node <- NULL
    rv$model_comparison <- NULL
    rv$selected_property <- NULL  # Clear any previously loaded subject property
    rv$selected_row_id <- NULL

    # Load data based on file extension
    file_ext <- tools::file_ext(input$adf_file$name)

    rv$adf <- tryCatch({
      if (file_ext == "csv") {
        # Use fread() from data.table - MUCH more robust for malformed CSVs
        # Handles quoted fields with commas, missing values, etc.
        data <- fread(input$adf_file$datapath,
                     data.table = FALSE,  # Return as data.frame, not data.table
                     stringsAsFactors = FALSE,
                     na.strings = c("", "NA", "N/A", "null"),
                     fill = TRUE,  # Fill rows with unequal columns
                     quote = "\"")  # Handle quoted fields properly

        # Convert to tibble for consistency with tidyverse
        data <- as_tibble(data)

        # Show success notification
        showNotification(
          sprintf("Successfully loaded %d rows and %d columns using robust CSV parser",
                 nrow(data), ncol(data)),
          type = "message",
          duration = 5
        )

        data
      } else if (file_ext == "xlsx") {
        readxl::read_excel(input$adf_file$datapath)
      } else if (file_ext == "rds") {
        read_rds(input$adf_file$datapath)
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      NULL
    })

    # -------------------------------------------------------------------------
    # Convert currency-formatted columns to numeric (e.g., "$1,234,567.00")
    # -------------------------------------------------------------------------
    # Uses parse_currency_column() from data_import.R module
    if (!is.null(rv$adf)) {
      currency_result <- convert_currency_columns(rv$adf, get_default_schema())
      rv$adf <- currency_result$data

      if (length(currency_result$converted) > 0) {
        showNotification(
          sprintf("Converted %d currency column(s) to numeric: %s",
                  length(currency_result$converted),
                  paste(currency_result$converted, collapse = ", ")),
          type = "message",
          duration = 5
        )
      }
    }

    # -------------------------------------------------------------------------
    # Filter out records with missing response variable (e.g., active listings)
    # -------------------------------------------------------------------------
    # Records without a sale price cannot be used for CMS analysis.
    # Detect the response variable first, then filter.

    if (!is.null(rv$adf)) {
      # Detect likely price column - check for PriceSold first (most common)
      all_vars <- names(rv$adf)

      # Direct check for common column names first
      if ("PriceSold" %in% all_vars) {
        detected_response <- "PriceSold"
      } else {
        # Fallback: look for price-related columns
        numeric_vars <- all_vars[sapply(rv$adf, is.numeric)]
        detected_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]

        # If still not found, check non-numeric columns
        if (is.na(detected_response)) {
          detected_response <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
        }
      }

      if (!is.na(detected_response) && detected_response %in% names(rv$adf)) {
        original_rows <- nrow(rv$adf)

        # Get the column values
        response_col <- rv$adf[[detected_response]]

        # Build filter based on column type (now should be numeric after currency conversion)
        if (is.numeric(response_col)) {
          keep_rows <- !is.na(response_col) & response_col > 0
        } else {
          # Fallback: parse currency format if still character
          response_col_numeric <- parse_currency_column(response_col)
          keep_rows <- !is.na(response_col_numeric) & response_col_numeric > 0
        }

        rv$adf <- rv$adf[keep_rows, ]

        missing_count <- original_rows - nrow(rv$adf)

        if (missing_count > 0) {
          showNotification(
            sprintf("Filtered out %d record(s) with missing or zero %s (e.g., active listings)",
                    missing_count, detected_response),
            type = "warning",
            duration = 10
          )
        }
      }
    }

    # -------------------------------------------------------------------------
    # Apply statistical outlier filter using IQR method
    # -------------------------------------------------------------------------
    # Uses Tukey's fences with configurable multiplier (default 3.0 = extreme only)
    # This adapts to any market's price range automatically
    if (!is.null(rv$adf) && "PriceSold" %in% names(rv$adf) && is.numeric(rv$adf$PriceSold)) {
      schema <- get_default_schema()

      outlier_result <- filter_outliers(
        rv$adf,
        column_name = "PriceSold",
        multiplier = schema$thresholds$outlier_iqr_multiplier,
        remove_upper = schema$thresholds$outlier_remove_upper,
        remove_lower = schema$thresholds$outlier_remove_lower
      )

      rv$adf <- outlier_result$data

      if (outlier_result$n_removed > 0) {
        showNotification(
          outlier_result$message,
          type = "warning",
          duration = 10
        )
      } else {
        # Show the calculated bounds even if no outliers removed (informative)
        showNotification(
          sprintf("Price range analysis: IQR bounds $%s - $%s (no outliers removed)",
                  format(round(outlier_result$outlier_info$lower_fence), big.mark = ","),
                  format(round(outlier_result$outlier_info$upper_fence), big.mark = ",")),
          type = "message",
          duration = 5
        )
      }
    }

    # Initialize the transformed data state, which will be used for all UI displays and modeling
    rv$adf_transformed <- rv$adf

    # Update variable selectors based on the transformed data
    if (!is.null(rv$adf_transformed)) {
      # Response variable options (ALL vars allowed to enable fixing messy data)
      all_vars <- names(rv$adf_transformed)
      
      # Prioritize finding a good default (Numeric + Price/Value/Sold name)
      numeric_vars <- all_vars[sapply(rv$adf_transformed, is.numeric)]
      likely_price <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
      
      # If no numeric price found, look for ANY price column (even text)
      if (is.na(likely_price)) {
        likely_price <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
      }
      
      message(sprintf("DIAGNOSTIC LOG: Found %d numeric columns. Setting response choices to ALL %d columns.", length(numeric_vars), length(all_vars)))
      updateSelectInput(session, "response_var", choices = all_vars,
                       selected = likely_price)

      # ========================================================================
      # NEW: Build field classification table from data layer
      # ========================================================================
      # Use the new field_classification module to assign roles and relevance
      # to each variable based on schema patterns and domain knowledge

      rv$field_classification <- classify_variables_for_tree(
        data = rv$adf_transformed,
        response_var = likely_price,  # Use detected response variable
        schema = get_default_schema()
      )

      fc <- rv$field_classification

      # Variables we want analysts to consider (core + extended)
      # This replaces the old regex-based auto-exclusion
      candidate_vars <- fc$name[fc$relevance %in% c("core", "extended")]

      # Store for variable selection checkboxes
      rv$selected_vars <- candidate_vars

      # Track which were ignored (for diagnostics and UI)
      rv$auto_excluded <- fc$name[fc$relevance == "ignore"]

      message("\n--- FIELD CLASSIFICATION DIAGNOSTIC ---")
      message(sprintf("Total variables: %d", nrow(fc)))
      message(sprintf("  Core:     %d (primary modeling variables)", sum(fc$relevance == "core")))
      message(sprintf("  Extended: %d (analyst choice)", sum(fc$relevance == "extended")))
      message(sprintf("  Ignored:  %d (excluded from modeling)", sum(fc$relevance == "ignore")))
      if(length(rv$auto_excluded) > 0) {
         message(paste("Excluded:", paste(head(rv$auto_excluded, 10), collapse=", ")))
      }
      message("----------------------------------------\n")

      # --- New: Proactive Data Quality Check ---
      # Run validation as soon as data is loaded.
      if (!is.null(rv$adf) && nrow(rv$adf) > 0) {
        # IMPORTANT: We pass the raw data frame to validation so it can find character columns.
        temp_adf <- rv$adf
        
        # Guess response and predictors for the check
        numeric_vars <- names(temp_adf)[sapply(temp_adf, is.numeric)]
        guessed_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
        
        # We now run validation regardless, but the checks inside will be limited
        # if no response var is guessed.
        guessed_predictors <- if (!is.na(guessed_response)) {
          setdiff(names(temp_adf), guessed_response)
        } else {
          names(temp_adf)
        }

        rv$data_quality_issues <- validate_data_for_tree(
          data = temp_adf,
          response_var = if (!is.na(guessed_response)) guessed_response else NULL,
          predictors = guessed_predictors,
          selected_vars = rv$selected_vars,
          field_classification = rv$field_classification
        )

        if (is.na(guessed_response)) {
          # Add a helpful info message if no price column was found
          info_issue <- list(
            column_name = "N/A",
            issue_type = "info",
            message = "<b>Please select your price column.</b><br>To complete the data quality check, please go to the <b>3. Build Tree</b> tab and select your main price or value column from the 'Response Variable' dropdown.",
            action_type = NULL,
            action_label = NULL
          )
          # Prepend this message to the list
          rv$data_quality_issues$info <- c(list(info_issue), rv$data_quality_issues$info)
          nav_select(ns("prepare_data_tabs"), selected = "1. Quality Report")
        } else if (length(rv$data_quality_issues$errors) > 0 || length(rv$data_quality_issues$warnings) > 0) {
          # Automatically select the "Quality Report" tab if real issues are found
          nav_select(ns("prepare_data_tabs"), selected = "1. Quality Report")
          showNotification("Data quality issues detected. Reviewing report.", type = "warning", duration = 8)
        }
      }
      # --- End of Proactive Check ---

      showNotification("Data loaded successfully!", type = "message")
    }
  })

  # --- Global Data Sync Observer ---
  # This ensures that whenever rv$adf_transformed changes (e.g., after a fix),
  # all dependent UI elements (like dropdowns) are updated globally.
  observe({
    req(rv$adf_transformed)
    
    # 1. Update Response Variable Dropdown
    # BUG FIX: Previously only showed numeric vars. Now showing ALL vars so user can select 
    # a messy text Price column and fix it via the validation tools.
    all_vars <- names(rv$adf_transformed)
    
    # Prioritize finding a good default (Numeric + Price/Value/Sold name)
    numeric_vars <- names(rv$adf_transformed)[sapply(rv$adf_transformed, is.numeric)]
    likely_price <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
    
    # If no numeric price found, look for ANY price column (even text)
    if (is.na(likely_price)) {
      likely_price <- all_vars[str_detect(all_vars, regex("price|value|sold", ignore_case = TRUE))][1]
    }
    
    # Get current selection to preserve it if possible
    current_sel <- input$response_var
    selected_val <- if (!is.null(current_sel) && current_sel %in% all_vars) {
      current_sel 
    } else {
      likely_price
    }
    
    updateSelectInput(session, "response_var", 
                      choices = all_vars,
                      selected = selected_val)
  })

  # Re-run validation when user manually selects a response variable
  observeEvent(input$response_var, {
    # Only run if data is loaded and the selection is not empty and is different from the initial guess
    req(rv$adf_transformed, input$response_var != "")

        message(sprintf("--- DIAGNOSTIC LOG: Response variable changed to '%s'. Re-running validation on FULL dataset. ---", input$response_var))

        # Re-run field classification with new response variable
        rv$field_classification <- classify_variables_for_tree(
          data = rv$adf_transformed,
          response_var = input$response_var,
          schema = get_default_schema()
        )
    
        # BUG FIX: Do NOT filter by rv$selected_vars here.
        # When changing response variable, we want to see the status of ALL variables,
        # including those that might have been auto-excluded. This ensures "messy" columns
        # reappear in the report so they can be fixed.
        temp_adf <- rv$adf_transformed
    
        predictors <- setdiff(names(temp_adf), input$response_var)

        # First, convert numeric-formatted character columns to actual numeric
        temp_adf <- convert_numeric_formatted_strings(temp_adf)

        # IMPORTANT: Save converted data back to rv$adf_transformed so conversions persist
        rv$adf_transformed <- temp_adf

        # Then convert remaining character columns to factors for validation
        temp_adf <- temp_adf %>%
          mutate(across(where(is.character), as.factor))

        rv$data_quality_issues <- validate_data_for_tree(
          data = temp_adf,
          response_var = input$response_var,
          predictors = predictors,
          selected_vars = rv$selected_vars,
          field_classification = rv$field_classification
        )  })

  # Re-run validation when underlying data changes (fixes applied)
  # OR when variable selection changes (exclude/include)
  observeEvent(list(rv$adf_transformed, rv$selected_vars, input$selection_mode), {
    req(rv$adf_transformed, input$response_var)
    
    message("--- DIAGNOSTIC LOG: Data or Selection changed. Re-running validation. ---")
    
    # We now pass the FULL dataset to validation, along with the selection list.
    # The validation function handles flagging excluded vars vs validating selected ones.
    temp_adf <- rv$adf_transformed
    all_vars <- names(temp_adf)
    
    predictors <- setdiff(names(temp_adf), input$response_var)

    # First, convert numeric-formatted character columns to actual numeric
    temp_adf <- convert_numeric_formatted_strings(temp_adf)

    # IMPORTANT: Save converted data back to rv$adf_transformed so conversions persist
    rv$adf_transformed <- temp_adf

    # Then convert remaining character columns to factors for validation
    temp_adf <- temp_adf %>%
      mutate(across(where(is.character), as.factor))

    rv$data_quality_issues <- validate_data_for_tree(
      data = temp_adf,
      response_var = input$response_var,
      predictors = predictors,
      selected_vars = rv$selected_vars,
      field_classification = rv$field_classification
    )

    # No forced tab switch or notification here to avoid spamming the user during selection
  }, ignoreInit = TRUE)


  # ==========================================================================
  # Profile Management (Save/Load Settings) - CSV Format
  # ==========================================================================

  # ===== TREE BUILDER SETTINGS PROFILE =====

  # Load tree settings profile
  observeEvent(input$load_tree_profile, {
    req(input$load_tree_profile)

    profile <- tryCatch({
      read.csv(input$load_tree_profile$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Error loading tree settings:", e$message), type = "error")
      NULL
    })

    if (!is.null(profile) && nrow(profile) > 0) {
      # Extract settings
      get_setting <- function(setting_name) {
        row <- profile[profile$setting == setting_name, ]
        if (nrow(row) > 0) row$value[1] else NULL
      }

      # Apply response variable
      response_var <- get_setting("response_var")
      if (!is.null(response_var) && !is.null(rv$adf)) {
        if (response_var %in% names(rv$adf)) {
          updateSelectInput(session, "response_var", selected = response_var)
        }
      }

      # Apply temporal variables toggle
      include_temporal <- get_setting("include_temporal_vars")
      if (!is.null(include_temporal)) {
        updateCheckboxInput(session, "include_temporal_vars",
                          value = as.logical(include_temporal))
      }

      # Apply model comparison toggle
      compare_models <- get_setting("compare_models")
      if (!is.null(compare_models)) {
        updateCheckboxInput(session, "compare_models",
                          value = as.logical(compare_models))
      }

      # Apply selection mode
      selection_mode <- get_setting("selection_mode")
      if (!is.null(selection_mode)) {
        updateRadioButtons(session, "selection_mode", selected = selection_mode)
      }

      # Apply variable selections
      selected_vars_str <- get_setting("selected_variables")
      if (!is.null(selected_vars_str) && selected_vars_str != "" && !is.null(rv$adf)) {
        selected_vars <- strsplit(selected_vars_str, ",")[[1]]
        # Only apply variables that exist in current dataset
        valid_vars <- intersect(selected_vars, names(rv$adf))
        rv$selected_vars <- valid_vars

        # Update checkboxes
        all_vars <- names(rv$adf)
        lapply(all_vars, function(var_name) {
          updateCheckboxInput(session, paste0("var_", make.names(var_name)),
                            value = var_name %in% valid_vars)
        })
      }

      showNotification(
        "Tree settings loaded successfully!",
        type = "message",
        duration = 5
      )
    }
  })

  # Save tree settings profile
  output$save_tree_profile <- downloadHandler(
    filename = function() {
      paste0("TreeSettings_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Collect current settings
      selected_vars_str <- if (!is.null(rv$selected_vars) && length(rv$selected_vars) > 0) {
        paste(rv$selected_vars, collapse = ",")
      } else {
        ""
      }

      profile_df <- data.frame(
        setting = c("response_var", "include_temporal_vars", "compare_models",
                   "selection_mode", "selected_variables", "created_date", "app_version"),
        value = c(
          input$response_var,
          as.character(input$include_temporal_vars),
          as.character(input$compare_models),
          input$selection_mode,
          selected_vars_str,
          as.character(Sys.Date()),
          "3.0"
        ),
        stringsAsFactors = FALSE
      )

      write.csv(profile_df, file, row.names = FALSE)

      showNotification(
        "Tree settings saved! You can reload them later with 'Load Settings'",
        type = "message",
        duration = 5
      )
    }
  )

  # ===== SUBJECT PROPERTY PROFILE =====

  # Load subject profile
  observeEvent(input$load_subject_profile, {
    req(input$load_subject_profile)

    subject <- tryCatch({
      read.csv(input$load_subject_profile$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Error loading subject profile:", e$message), type = "error")
      NULL
    })

    if (!is.null(subject) && nrow(subject) > 0) {
      # Convert to named list for compatibility
      subject_list <- setNames(as.list(subject$value), subject$characteristic)

      # Convert numeric strings back to numeric
      subject_list <- lapply(names(subject_list), function(char_name) {
        val <- subject_list[[char_name]]
        # Try to convert to numeric if possible
        num_val <- suppressWarnings(as.numeric(val))
        if (!is.na(num_val)) num_val else val
      })
      names(subject_list) <- subject$characteristic

      # Store as selected property
      rv$selected_property <- as.data.frame(subject_list, stringsAsFactors = FALSE)
      rv$selected_row_id <- NULL

      showNotification(
        "Subject property loaded successfully!",
        type = "message",
        duration = 5
      )
    }
  })

  # Save subject profile
  output$save_subject_profile <- downloadHandler(
    filename = function() {
      paste0("Subject_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$adf)

      # Collect current subject input values
      # Use field classification to identify subject columns
      # IMPORTANT: Respect the include_temporal_vars checkbox
      subject_cols <- if (!is.null(rv$field_classification)) {
        get_subject_columns(
          rv$field_classification,
          response_var = input$response_var,
          include_temporal = isTRUE(input$include_temporal_vars)
        )
      } else {
        # Fallback if classification not available
        names(rv$adf)
      }

      # Extract values from inputs
      subject_values <- lapply(subject_cols, function(col_name) {
        input_id <- paste0("subj_", col_name)
        val <- input[[input_id]]
        if (is.null(val) || (is.character(val) && val == "")) {
          NA
        } else {
          val
        }
      })

      # Create dataframe
      subject_df <- data.frame(
        characteristic = subject_cols,
        value = unlist(subject_values),
        stringsAsFactors = FALSE
      )

      # Remove rows with NA values
      subject_df <- subject_df[!is.na(subject_df$value), ]

      if (nrow(subject_df) > 0) {
        write.csv(subject_df, file, row.names = FALSE)

        showNotification(
          "Subject profile saved! You can reload it later with 'Load Subject'",
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          "No subject data to save. Please fill in subject characteristics first.",
          type = "warning",
          duration = 5
        )
      }
    }
  )

  # ==========================================================================
  # Tab 1: Data Status
  # ==========================================================================

  output$data_status <- renderUI({
    if (is.null(rv$adf)) {
      div(class = "alert alert-warning", role = "alert",
        icon("upload"), strong(" No data loaded."),
        " Please upload your ADF file (Excel, CSV, or RDS)."
      )
    } else {
      div(class = "alert alert-success", role = "alert",
        icon("check-circle"), strong(" Data loaded successfully!"),
        br(),
        sprintf("%d sales records with %d variables", nrow(rv$adf), ncol(rv$adf))
      )
    }
  })

  # ==========================================================================
  # Tab 2: Prepare Data (New)
  # ==========================================================================

  output$data_quality_report_ui <- renderUI({
    message("\n--- DIAGNOSTIC LOG: Rendering data quality UI ---")
    req(rv$adf)

    # Show a loading state while waiting for the async validation
    if (is.null(rv$data_quality_issues)) {
      return(
        div(
          class = "text-center p-5 text-muted",
          icon("spinner", class = "fa-spin fa-2x mb-3"),
          h4("Analyzing your data..."),
          p("The data quality engine is running checks in the background.")
        )
      )
    }

    issues <- rv$data_quality_issues

    has_errors   <- length(issues$errors)   > 0
    has_warnings <- length(issues$warnings) > 0
    has_info     <- length(issues$info)     > 0

    # If absolutely nothing to report AND no classification, show "all good"
    if (!has_errors && !has_warnings && !has_info && is.null(rv$field_classification)) {
      return(
        div(
          class = "alert alert-success text-center p-5 shadow-sm",
          icon("check-circle", class = "fa-3x mb-3"),
          h4("Excellent! No data quality issues detected."),
          p(
            class = "lead",
            "Based on an initial scan, your data appears to be in good shape for modeling."
          )
        )
      )
    }

    # Do we have field classification ready?
    has_classification <- !is.null(rv$field_classification) &&
      nrow(rv$field_classification) > 0

    # Function to create an issue card, now with buttons
    render_issue_card <- function(issue_list, title, alert_class, icon_name) {
      if (length(issue_list) == 0) return(NULL)

      tagList(
        h4(icon(icon_name), title, class = paste0("text-", alert_class), "mb-3"),
        lapply(issue_list, function(issue) {
          # Check if the issue is in the new structured format or old string format
          is_structured <- is.list(issue) && !is.null(issue$message)

          # Override style for Excluded variables
          current_alert_class <- alert_class
          if (is_structured && isTRUE(issue$is_excluded)) {
            current_alert_class <- "secondary" # Gray for excluded
          }

          # Helper to create buttons consistently
          create_button <- function(action, col, label, icon_name, class_name) {
            # Determine input ID based on action type
            # Inspect has its own prefix, others use 'fix_'
            if (action == "inspect") {
               id <- make_inspect_id(col)
            } else {
               id <- paste("fix", action, col, sep = "_")
            }

            actionButton(
              inputId = ns(id),
              label = label,
              icon = icon(icon_name),
              class = paste("btn-sm", class_name)
            )
          }

          # Render action buttons if available
          action_buttons_ui <- if (is_structured) {
            btns <- list()

            # 1. Inspect Button (Always show for everyone)
            btns[[1]] <- create_button("inspect", issue$column_name, "Inspect", "eye", "btn-secondary")

            # 2. Primary Action Button (if defined)
            if (!is.null(issue$action_type)) {
              btn_icon <- if(!is.null(issue$action_icon)) issue$action_icon else "magic"
              btn_class <- if(!is.null(issue$action_color)) paste0("btn-", issue$action_color) else "btn-success"

              btns[[2]] <- create_button(issue$action_type, issue$column_name, issue$action_label, btn_icon, btn_class)
            }

            # 3. Secondary Action Button (Exclude/Undo)
            # Show if defined AND not already in that state (unless it's Undo, which is the state)
            # Actually, for Undo, action_type is "include-variable", which is primary.
            # For Exclude, it is secondary.
            if (!is.null(issue$action_type_secondary) && !isTRUE(issue$is_excluded)) {
              btn_icon_sec <- if(!is.null(issue$action_icon_secondary)) issue$action_icon_secondary else "trash"
              btn_class_sec <- if(!is.null(issue$action_color_secondary)) paste0("btn-", issue$action_color_secondary) else "btn-danger"

              btns[[3]] <- create_button(issue$action_type_secondary, issue$column_name, issue$action_label_secondary, btn_icon_sec, btn_class_sec)
            }

            if (length(btns) > 0) div(class = "d-flex gap-2 mt-2", btns) else NULL
          } else {
            NULL
          }

          # Get message
          message_html <- if(is_structured) issue$message else issue

          div(class = paste0("alert alert-light border-start border-5 border-", current_alert_class, " shadow-sm mb-3"),
            style = "font-size: 0.95rem;",
            HTML(message_html),
            action_buttons_ui
          )
        })
      )
    }

    # Function to render variable classification status from field_classification
    render_classification_summary <- function() {
      if (!has_classification) return(NULL)

      fc <- rv$field_classification

      core_vars     <- fc[fc$relevance == "core", ]
      extended_vars <- fc[fc$relevance == "extended", ]
      ignored_vars  <- fc[fc$relevance == "ignore", ]

      tagList(
        h4(icon("sitemap"), "Variable Classification", class = "text-primary mb-3"),

        # Core Variables
        if (nrow(core_vars) > 0) {
          div(
            class = "mb-4",
            h5(
              icon("check-circle"),
              sprintf("Core Variables (%d)", nrow(core_vars)),
              class = "text-success"
            ),
            p(
              class = "text-muted small",
              "Essential variables included in modeling by default"
            ),
            div(
              class = "row g-2",
              lapply(seq_len(nrow(core_vars)), function(i) {
                var <- core_vars[i, ]
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-light border-start border-5 border-success shadow-sm mb-2",
                    style = "font-size: 0.9rem; padding: 0.75rem;",
                    div(
                      class = "d-flex justify-content-between align-items-start",
                      div(
                        strong(var$name),
                        div(
                          class = "small text-muted",
                          span(class = "badge bg-success me-1", var$role),
                          var$reason
                        )
                      ),
                      actionButton(
                        inputId = ns(paste0("inspect_", make.names(var$name))),
                        label   = NULL,
                        icon    = icon("eye"),
                        class   = "btn-sm btn-outline-secondary",
                        title   = "Inspect this variable"
                      )
                    )
                  )
                )
              })
            )
          )
        } else { NULL },

        # Extended Variables
        if (nrow(extended_vars) > 0) {
          div(
            class = "mb-4",
            h5(
              icon("sliders-h"),
              sprintf("Extended Variables (%d)", nrow(extended_vars)),
              class = "text-info"
            ),
            p(
              class = "text-muted small",
              "Available for modeling – analyst's discretion"
            ),
            div(
              class = "row g-2",
              lapply(seq_len(nrow(extended_vars)), function(i) {
                var <- extended_vars[i, ]
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-light border-start border-5 border-info shadow-sm mb-2",
                    style = "font-size: 0.9rem; padding: 0.75rem;",
                    div(
                      class = "d-flex justify-content-between align-items-start",
                      div(
                        strong(var$name),
                        div(
                          class = "small text-muted",
                          span(class = "badge bg-info me-1", var$role),
                          var$reason
                        )
                      ),
                      actionButton(
                        inputId = ns(paste0("inspect_", make.names(var$name))),
                        label   = NULL,
                        icon    = icon("eye"),
                        class   = "btn-sm btn-outline-secondary",
                        title   = "Inspect this variable"
                      )
                    )
                  )
                )
              })
            )
          )
        } else { NULL },

        # Ignored Variables
        if (nrow(ignored_vars) > 0) {
          div(
            class = "mb-4",
            h5(
              icon("ban"),
              sprintf("Excluded Variables (%d)", nrow(ignored_vars)),
              class = "text-secondary"
            ),
            p(
              class = "text-muted small",
              "Not suitable for modeling (IDs, addresses, metadata)"
            ),
            div(
              class = "row g-2",
              lapply(seq_len(nrow(ignored_vars)), function(i) {
                var <- ignored_vars[i, ]
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-light border-start border-5 border-secondary shadow-sm mb-2",
                    style = "font-size: 0.9rem; padding: 0.75rem;",
                    div(
                      class = "d-flex justify-content-between align-items-start",
                      div(
                        strong(var$name),
                        div(
                          class = "small text-muted",
                          span(class = "badge bg-secondary me-1", var$role),
                          var$reason
                        )
                      ),
                      actionButton(
                        inputId = ns(paste0("inspect_", make.names(var$name))),
                        label   = NULL,
                        icon    = icon("eye"),
                        class   = "btn-sm btn-outline-secondary",
                        title   = "Inspect this variable"
                      )
                    )
                  )
                )
              })
            )
          )
        } else { NULL }
      )
    }

    # ----- FINAL UI LAYOUT -----
    div(
      class = "p-3",

      # Top-right "Report Updated" badge
      div(
        class = "d-flex justify-content-end mb-2",
        span(
          class = "badge bg-secondary",
          icon("clock"),
          paste(
            "Report Updated:",
            if (!is.null(issues$report_time)) {
              issues$report_time
            } else {
              format(Sys.time(), "%H:%M:%S")
            }
          )
        )
      ),

      # 1) Variable classification block (if we have it)
      if (has_classification) {
        div(class = "mb-4", render_classification_summary())
      } else {
        NULL
      },

      # 2) Data quality issues (if any)
      if (has_errors || has_warnings || has_info) {
        tagList(
          hr(),
          h4(
            icon("exclamation-triangle"),
            "Data Quality Issues",
            class = "text-warning mb-3"
          ),
          render_issue_card(
            issues$errors,
            "Errors (Must be fixed)",
            "danger",
            "times-circle"
          ),
          render_issue_card(
            issues$warnings,
            "Warnings (Review Recommended)",
            "warning",
            "exclamation-triangle"
          ),
          render_issue_card(
            issues$info,
            "Information (For Your Consideration)",
            "info",
            "info-circle"
          )
        )
      } else if (has_classification) {
        # No issues, but we *do* have a classification map
        div(
          class = "alert alert-success text-center p-4 mt-3",
          icon("check-circle", class = "fa-2x mb-2"),
          h5("No Data Quality Issues Detected"),
          p("All variables are properly classified and ready for modeling.")
        )
      } else {
        NULL
      }
    )
  })

  output$transformation_ui_placeholder <- renderUI({
    # Placeholder for Stage 3
    req(rv$adf) # Ensure data is loaded
    div(class = "alert alert-light border p-4",
        h5(icon("cogs"), "Transformation Engine Pending"),
        p("Tools for feature engineering (e.g., extracting month from date), encoding categorical variables, and other data transformations will be built here in a future stage.")
    )
  })

  # --- Existing "Examine ADF" Outputs ---

  output$adf_preview <- renderDT({
    req(rv$adf_transformed)

    df <- if (input$show_all_columns) {
      rv$adf_transformed
    } else {
      rv$adf_transformed %>% select(1:min(10, ncol(rv$adf_transformed)))
    }

    datatable(
      head(df, 10),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })

  output$adf_summary <- renderPrint({
    req(rv$adf_transformed)
    summary(rv$adf_transformed)
  })

  output$price_histogram <- renderPlotly({
    req(rv$adf_transformed, input$response_var)

    plot_ly(rv$adf_transformed, x = ~get(input$response_var), type = "histogram",
            marker = list(color = "#3498db")) %>%
      layout(
        title = paste("Distribution of", input$response_var),
        xaxis = list(title = input$response_var),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )
  })

  output$feature_summary <- renderTable({
    req(rv$adf_transformed)

    data.frame(
      Variable = names(rv$adf_transformed),
      Type = sapply(rv$adf_transformed, function(x) paste(class(x), collapse = ", ")),
      Missing = sapply(rv$adf_transformed, function(x) sum(is.na(x))),
      Unique = sapply(rv$adf_transformed, function(x) length(unique(x)))
    )
  })

  # ==========================================================================
  # Tab 3: Build Decision Tree
  # ==========================================================================

  # Build picker rows for subject quick-pick.
  # Use row IDs as select values so duplicate addresses select a specific record.
  subject_picker_rows <- reactive({
    req(rv$adf_transformed)

    df <- rv$adf_transformed
    address_cols <- names(df)[grepl("address|street", names(df), ignore.case = TRUE)]
    if (length(address_cols) == 0) return(NULL)

    # Pick the best address-like column if multiple candidates exist.
    score_address_col <- function(col_name) {
      vals <- trimws(as.character(df[[col_name]]))
      non_empty <- sum(!is.na(vals) & nzchar(vals))
      alpha_vals <- sum(grepl("[A-Za-z]", vals))
      name_bonus <- if (grepl("^address$|streetaddress|propertyaddress", col_name, ignore.case = TRUE)) 1000 else 0
      non_empty + (alpha_vals * 0.5) + name_bonus
    }
    address_scores <- vapply(address_cols, score_address_col, numeric(1))
    address_col <- address_cols[which.max(address_scores)]

    addresses <- trimws(as.character(df[[address_col]]))
    addresses <- gsub("[\r\n\t]+", " ", addresses)
    addresses <- gsub("\\s+", " ", addresses)
    # Require at least one letter to avoid malformed rows like "$860,000" appearing as addresses.
    valid <- !is.na(addresses) & nzchar(addresses) & grepl("[A-Za-z]", addresses)
    if (!any(valid)) return(NULL)

    picker <- data.frame(
      row_id = which(valid),
      address = addresses[valid],
      stringsAsFactors = FALSE
    )

    # Optional context fields to disambiguate duplicate addresses.
    date_cols <- names(df)[grepl("datesold|sold.?date|sale.?date|close.?date", names(df), ignore.case = TRUE)]
    date_col <- if (length(date_cols) > 0) date_cols[1] else NULL
    if (!is.null(date_col)) {
      raw_dates <- as.character(df[[date_col]][valid])
      parsed_dates <- suppressWarnings(as.Date(raw_dates))
      date_text <- ifelse(!is.na(parsed_dates), format(parsed_dates, "%Y-%m-%d"), raw_dates)
      date_text <- gsub("[\r\n\t]+", " ", date_text)
      date_text <- gsub("\\s+", " ", date_text)
      picker$date_text <- trimws(date_text)
    } else {
      picker$date_text <- NA_character_
    }

    price_priority <- c("PriceSold", "SoldPrice", "SalePrice")
    price_col <- if (any(price_priority %in% names(df))) {
      price_priority[price_priority %in% names(df)][1]
    } else {
      price_cols <- names(df)[grepl("pricesold|sold.?price|sale.?price", names(df), ignore.case = TRUE)]
      if (length(price_cols) > 0) price_cols[1] else NULL
    }
    if (!is.null(price_col)) {
      raw_price <- as.character(df[[price_col]][valid])
      num_price <- suppressWarnings(as.numeric(gsub("[,$[:space:]]", "", raw_price)))
      price_text <- ifelse(
        !is.na(num_price),
        paste0("$", format(round(num_price, 0), big.mark = ",", scientific = FALSE, trim = TRUE)),
        raw_price
      )
      price_text <- gsub("[\r\n\t]+", " ", price_text)
      price_text <- gsub("\\s+", " ", price_text)
      picker$price_text <- trimws(price_text)
    } else {
      picker$price_text <- NA_character_
    }

    # Build consistent labels for all rows.
    context <- ifelse(
      !is.na(picker$date_text) & nzchar(picker$date_text),
      picker$date_text,
      ""
    )
    context <- ifelse(
      !is.na(picker$price_text) & nzchar(picker$price_text),
      ifelse(nzchar(context), paste0(context, " | ", picker$price_text), picker$price_text),
      context
    )
    picker$label <- ifelse(nzchar(context), paste0(picker$address, " | ", context), picker$address)

    # Ensure labels are unique even if context collides.
    duplicate_label <- duplicated(picker$label) | duplicated(picker$label, fromLast = TRUE)
    if (any(duplicate_label)) {
      picker$label[duplicate_label] <- paste0(picker$label[duplicate_label], " (row ", picker$row_id[duplicate_label], ")")
    }

    picker
  })

  # Address picker for quick subject selection
  output$address_picker <- renderUI({
    picker <- subject_picker_rows()
    if (is.null(picker) || nrow(picker) == 0) {
      return(p("No address column found in data", class = "text-muted"))
    }

    choices <- setNames(as.character(picker$row_id), picker$label)

    selectizeInput(
      ns("selected_address"),
      label = NULL,
      choices = c(" " = "", choices),
      selected = "",
      options = list(
        placeholder = "Type to search addresses...",
        maxOptions = max(500, nrow(picker)),
        closeAfterSelect = TRUE,
        onInitialize = I("function() { this.setValue(''); }")
      )
    )
  })

  # When user clicks "Use This Property", populate subject fields
  observeEvent(input$use_selected_property, {
    req(rv$adf_transformed)

    if (is.null(input$selected_address) || input$selected_address == "") {
      showNotification("Please select a property first", type = "warning")
      return()
    }

    selected_df <- NULL
    resolved_row_id <- NA_integer_
    picker <- subject_picker_rows()

    # Primary path: use row ID from picker value (supports duplicate addresses safely).
    selected_row_id <- suppressWarnings(as.integer(input$selected_address))
    if (!is.null(picker) && !is.na(selected_row_id) && selected_row_id %in% picker$row_id) {
      selected_df <- rv$adf_transformed[selected_row_id, , drop = FALSE]
      resolved_row_id <- selected_row_id
    }

    # Fallback path for legacy string selections: match by address text.
    if (is.null(selected_df) || nrow(selected_df) == 0) {
      address_cols <- names(rv$adf_transformed)[grepl("address|street", names(rv$adf_transformed), ignore.case = TRUE)]
      address_col <- address_cols[1]
      trimmed_addresses <- trimws(as.character(rv$adf_transformed[[address_col]]))
      matching_idx <- which(!is.na(trimmed_addresses) & trimmed_addresses == input$selected_address)
      if (length(matching_idx) == 0) {
        matching_idx <- which(!is.na(trimmed_addresses) & tolower(trimmed_addresses) == tolower(input$selected_address))
      }

      if (length(matching_idx) > 0) {
        chosen_idx <- matching_idx[1]
        if (length(matching_idx) > 1) {
          # Prefer the most recent sale record if a sale-date column is available.
          date_cols <- names(rv$adf_transformed)[grepl("datesold|sold.?date|sale.?date|close.?date", names(rv$adf_transformed), ignore.case = TRUE)]
          if (length(date_cols) > 0) {
            parsed_dates <- suppressWarnings(as.Date(rv$adf_transformed[[date_cols[1]]][matching_idx]))
            if (any(!is.na(parsed_dates))) {
              chosen_idx <- matching_idx[which.max(parsed_dates)]
            }
          }
          showNotification(
            "Multiple records found for this address. Loaded one matching record; use the picker context (date/price) to choose a specific sale.",
            type = "warning",
            duration = 6
          )
        }
        selected_df <- rv$adf_transformed[chosen_idx, , drop = FALSE]
        resolved_row_id <- chosen_idx
      }
    }

    if (!is.null(selected_df) && nrow(selected_df) > 0) {
      property_row <- selected_df[1, , drop = FALSE]
      rv$selected_property <- property_row
      rv$selected_row_id <- if (!is.na(resolved_row_id)) resolved_row_id else NULL
      rv$subject_fresh_load <- TRUE  # Flag to ignore cached browser input values

      # DIRECTLY UPDATE INPUT VALUES using updateNumericInput/updateSelectInput.
      subject_cols <- if (!is.null(rv$field_classification)) {
        get_subject_columns(
          rv$field_classification,
          response_var = input$response_var,
          include_temporal = isTRUE(input$include_temporal_vars)
        )
      } else {
        names(rv$adf_transformed)
      }

      for (col_name in subject_cols) {
        if (col_name %in% names(property_row)) {
          input_id <- paste0("subj_", col_name)
          new_value <- property_row[[col_name]]
          col_data <- rv$adf_transformed[[col_name]]

          if (is.numeric(col_data)) {
            updateNumericInput(session, input_id, value = as.numeric(new_value))
          } else if (is.factor(col_data) || is.character(col_data)) {
            updateSelectInput(session, input_id, selected = as.character(new_value))
          }
        }
      }

      address_cols <- names(property_row)[grepl("address|street", names(property_row), ignore.case = TRUE)]
      selected_address <- if (length(address_cols) > 0) as.character(property_row[[address_cols[1]]][1]) else "selected property"

      showNotification(
        if (!is.null(rv$selected_row_id)) {
          paste("Property selected:", selected_address, "(row", rv$selected_row_id, ")")
        } else {
          paste("Property selected:", selected_address)
        },
        type = "message"
      )
    } else {
      rv$selected_row_id <- NULL
      showNotification(
        "Could not find the selected property record. Try selecting again from the quick-pick list.",
        type = "error"
      )
    }
  })

  # When user clicks "Load Example Subject", populate with Charlie's actual subject
  # Reference: Table 1.1 from Charlie Abromaitis's Sept 15, 2023 paper
  # "From the ADF to the CMS with Decision Trees"
  observeEvent(input$load_charlie_subject, {
    req(rv$adf_transformed)

    # Charlie's exact subject property from Sept 15, 2023 paper (Table 1.1)
    # Subject: 3641 Citrus Ave, Walnut Creek, CA
    # DateSold: 2021-08-02, PriceSold: $1,500,000
    # This property finds end node 22 (n=8) via SaleQtr, Condition, SqFt splits

    example_list <- list()
    adf_cols <- names(rv$adf_transformed)

    # Temporal field (CRITICAL: SaleQtr=2021.3 needed for correct node assignment!)
    if ("SaleQtr" %in% adf_cols) example_list$SaleQtr <- 2021.3

    # Numeric fields (Charlie's exact values from Table 1.1)
    if ("SqFt" %in% adf_cols) example_list$SqFt <- 2241
    if ("BR" %in% adf_cols) example_list$BR <- 4
    if ("BA" %in% adf_cols) example_list$BA <- 3
    if ("PB" %in% adf_cols) example_list$PB <- 1
    if ("Bedrooms" %in% adf_cols) example_list$Bedrooms <- 4
    if ("Bathrooms" %in% adf_cols) example_list$Bathrooms <- 3
    if ("YrBlt" %in% adf_cols) example_list$YrBlt <- 1967
    if ("GarSp" %in% adf_cols) example_list$GarSp <- 2
    if ("LotSF" %in% adf_cols) example_list$LotSF <- 8855
    if ("FP" %in% adf_cols) example_list$FP <- 1

    # Categorical fields (CRITICAL: Condition="Excellent" is key split variable!)
    if ("Condition" %in% adf_cols) example_list$Condition <- "Excellent"
    if ("Stories" %in% adf_cols) example_list$Stories <- "Two Story"
    if ("Pool" %in% adf_cols) example_list$Pool <- "No"
    if ("Neighborhood" %in% adf_cols) example_list$Neighborhood <- "Woodlands"

    # Geocoding (if present)
    if ("longitude" %in% adf_cols) example_list$longitude <- -122.00968
    if ("latitude" %in% adf_cols) example_list$latitude <- 37.93736

    if (length(example_list) > 0) {
      rv$selected_property <- as.data.frame(example_list, stringsAsFactors = FALSE)
      rv$selected_row_id <- NULL
      rv$subject_fresh_load <- TRUE  # Flag to ignore cached browser input values

      # DIRECTLY UPDATE INPUT VALUES using updateNumericInput/updateSelectInput
      for (col_name in names(example_list)) {
        input_id <- paste0("subj_", col_name)
        new_value <- example_list[[col_name]]

        if (col_name %in% names(rv$adf_transformed)) {
          col_data <- rv$adf_transformed[[col_name]]

          if (is.numeric(col_data)) {
            updateNumericInput(session, input_id, value = as.numeric(new_value))
          } else if (is.factor(col_data) || is.character(col_data)) {
            updateSelectInput(session, input_id, selected = as.character(new_value))
          }
        }
      }

      showNotification("Charlie's example subject loaded (3641 Citrus Ave - SaleQtr 2021.3, Excellent condition)",
                      type = "message", duration = 5)
    }
  })

  output$subject_inputs <- renderUI({
    req(rv$adf_transformed)

    # Use field classification to identify subject-defining columns
    # IMPORTANT: Respect the include_temporal_vars checkbox so that SaleQtr
    # appears in subject form when temporal variables are enabled for tree building
    subject_cols <- if (!is.null(rv$field_classification)) {
      get_subject_columns(
        rv$field_classification,
        response_var = input$response_var,
        include_temporal = isTRUE(input$include_temporal_vars)
      )
    } else {
      # Fallback if classification not available
      names(rv$adf_transformed)
    }

    # Debug: log render state
    message("=== RENDERING SUBJECT INPUTS ===")
    message("subject_fresh_load: ", rv$subject_fresh_load)
    message("selected_property is NULL: ", is.null(rv$selected_property))
    if (!is.null(rv$selected_property)) {
      message("selected_property columns: ", paste(names(rv$selected_property), collapse = ", "))
    }
    message("================================")

    # Generate input widgets dynamically
    inputs <- lapply(subject_cols, function(col_name) {
      col_data <- rv$adf_transformed[[col_name]]
      input_id <- paste0("subj_", col_name)

      # PRIORITY ORDER for default values:
      # 1. If fresh load: Use selected_property values (ignore browser cache)
      # 2. If NOT fresh load: Check existing input values (preserves manual edits)
      # 3. Fall back to selected_property value
      # 4. Median/empty (fallback for fresh start)
      default_value <- NULL

      if (!is.null(rv$selected_property)) {
        if (isTRUE(rv$subject_fresh_load)) {
          # Fresh load - use selected_property values directly, ignore browser cache
          if (col_name %in% names(rv$selected_property)) {
            default_value <- rv$selected_property[[col_name]]
          }
        } else {
          # Not fresh load - check for user's manual edits first
          existing_value <- tryCatch({
            val <- isolate(input[[input_id]])
            if (!is.null(val)) {
              if (is.numeric(col_data) && is.numeric(val)) {
                val
              } else if ((is.character(col_data) || is.factor(col_data)) && is.character(val) && val != "") {
                val
              } else {
                NULL
              }
            } else {
              NULL
            }
          }, error = function(e) NULL)

          if (!is.null(existing_value)) {
            default_value <- existing_value
          } else if (col_name %in% names(rv$selected_property)) {
            default_value <- rv$selected_property[[col_name]]
          }
        }
      }

      if (is.numeric(col_data)) {
        # Numeric input with range from data
        if (is.null(default_value) || (length(default_value) == 1 && is.na(default_value))) {
          default_value <- median(col_data, na.rm = TRUE)
        }

        numericInput(
          ns(input_id),
          label = col_name,
          value = as.numeric(default_value),
          min = min(col_data, na.rm = TRUE),
          max = max(col_data, na.rm = TRUE)
        )
      } else if (is.factor(col_data) || is.character(col_data)) {
        # Dropdown for categorical
        unique_vals <- unique(as.character(col_data))
        unique_vals <- unique_vals[!is.na(unique_vals)]

        if (length(unique_vals) <= 20) {  # Only show if reasonable number of options
          if (is.null(default_value) || (length(default_value) == 1 && is.na(default_value))) {
            default_value <- ""
          } else {
            default_value <- as.character(default_value)
          }

          selectInput(
            ns(input_id),
            label = col_name,
            choices = c("", unique_vals),
            selected = default_value
          )
        } else {
          NULL  # Skip if too many categories
        }
      } else {
        NULL  # Skip other types
      }
    })

    # Reset fresh load flag after UI has rendered with the new values
    if (isTRUE(rv$subject_fresh_load)) {
      rv$subject_fresh_load <- FALSE
    }

    # Remove NULL entries
    inputs <- inputs[!sapply(inputs, is.null)]

    if (length(inputs) == 0) {
      p("No subject inputs available. Build the tree first.", class = "text-muted")
    } else {
      tagList(
        p(strong("Define your subject property:"), class = "mb-2"),
        helpText("Select a property using the address picker above, or fill in characteristics manually."),
        inputs
      )
    }
  })

  # Display currently loaded subject property
  output$current_subject_display <- renderUI({
    # Only show if a property has been loaded
    if (is.null(rv$selected_property)) {
      return(
        div(class = "alert alert-secondary mb-3",
          icon("info-circle"),
          " No subject property loaded. Use the picker above to select one, or fill in characteristics manually."
        )
      )
    }

    # Try to get address from selected property
    address_cols <- names(rv$selected_property)[grepl("address|street", names(rv$selected_property), ignore.case = TRUE)]
    address <- if (length(address_cols) > 0) {
      as.character(rv$selected_property[[address_cols[1]]])
    } else {
      NULL
    }

    # Try to get price if available
    price_cols <- names(rv$selected_property)[grepl("price|sold", names(rv$selected_property), ignore.case = TRUE)]
    price <- if (length(price_cols) > 0) {
      val <- rv$selected_property[[price_cols[1]]]
      if (is.numeric(val) && !is.na(val)) {
        scales::dollar(val)
      } else {
        NULL
      }
    } else {
      NULL
    }

    # Get lat/long if available
    lat <- if ("latitude" %in% names(rv$selected_property)) {
      val <- rv$selected_property[["latitude"]]
      if (!is.null(val) && !is.na(val)) round(as.numeric(val), 5) else NULL
    } else NULL

    lng <- if ("longitude" %in% names(rv$selected_property)) {
      val <- rv$selected_property[["longitude"]]
      if (!is.null(val) && !is.na(val)) round(as.numeric(val), 5) else NULL
    } else NULL

    # Build geo badge if we have coordinates
    geo_badge <- if (!is.null(lat) && !is.null(lng)) {
      span(class = "ms-2 badge bg-secondary",
           style = "font-size: 10px;",
           icon("map-marker-alt"), sprintf(" %.4f, %.4f", lat, lng))
    } else NULL

    # Build display
    if (!is.null(address) && !is.na(address) && address != "") {
      div(class = "alert alert-info mb-3",
        icon("home"), strong(" Subject Property: "), address,
        if (!is.null(price)) span(class = "ms-2 badge bg-success", price),
        geo_badge,
        actionButton(ns("clear_subject"), "Clear", class = "btn-sm btn-outline-secondary ms-3", icon = icon("times"))
      )
    } else {
      div(class = "alert alert-info mb-3",
        icon("home"), strong(" Subject Property Loaded"),
        if (!is.null(price)) span(class = "ms-2 badge bg-success", price),
        geo_badge,
        actionButton(ns("clear_subject"), "Clear", class = "btn-sm btn-outline-secondary ms-3", icon = icon("times"))
      )
    }
  })

  # Clear subject button handler
  observeEvent(input$clear_subject, {
    rv$selected_property <- NULL
    rv$selected_row_id <- NULL
    showNotification("Subject property cleared", type = "message", duration = 3)
  })

  # ==========================================================================
  # New Multi-Column Layouts
  # ==========================================================================

  # Variable checkboxes in multi-column grid
  output$variable_checkboxes_grid <- renderUI({
    req(rv$adf_transformed)

    all_vars <- names(rv$adf_transformed)
    message(sprintf("DIAGNOSTIC LOG: Rendering checkboxes for %d variables: %s...", length(all_vars), paste(head(all_vars, 5), collapse=", ")))

    # Create checkboxes in 4-column responsive grid
    num_cols <- 4
    vars_per_col <- ceiling(length(all_vars) / num_cols)

    # Determine which vars are currently selected
    selected_vars <- if (!is.null(rv$selected_vars)) rv$selected_vars else character(0)

    # Create columns
    cols <- lapply(1:num_cols, function(col_idx) {
      start_idx <- (col_idx - 1) * vars_per_col + 1
      end_idx <- min(col_idx * vars_per_col, length(all_vars))

      if (start_idx <= length(all_vars)) {
        vars_in_col <- all_vars[start_idx:end_idx]

        div(class = "col-xl-3 col-lg-4 col-md-6 col-sm-12 variable-checkbox-column px-2",
          lapply(vars_in_col, function(var_name) {
            checkbox_id <- paste0("var_", make.names(var_name))
            preview_id <- paste0("preview_", make.names(var_name))

            label_tag <- tags$label(
              var_name,
              class = "variable-name mb-0 text-truncate"
            )
            label_tag$attribs[["for"]] <- ns(checkbox_id)

            div(class = "variable-checkbox d-flex align-items-center mb-2 w-100",
              checkboxInput(
                inputId = ns(checkbox_id),
                label = NULL,
                value = var_name %in% selected_vars
              ),
              actionButton(
                ns(preview_id),
                label = NULL,
                icon = icon("eye"),
                class = "btn btn-sm btn-info variable-preview-btn",
                title = paste("Preview", var_name, "data"),
                tabindex = "-1",
                onclick = "event.stopPropagation();",
                onmousedown = "event.stopPropagation();",
                ontouchstart = "event.stopPropagation();"
              ),
              label_tag
            )
          })
        )
      }
    })

    div(class = "row g-3 variable-checkbox-grid justify-content-start",
      cols
    )
  })

  # Subject inputs in multi-column grid
  output$subject_inputs_grid <- renderUI({
    req(rv$adf_transformed)

    # Use field classification to identify subject-defining columns
    # IMPORTANT: Respect the include_temporal_vars checkbox so that SaleQtr
    # appears in subject form when temporal variables are enabled for tree building
    subject_cols <- if (!is.null(rv$field_classification)) {
      get_subject_columns(
        rv$field_classification,
        response_var = input$response_var,
        include_temporal = isTRUE(input$include_temporal_vars)
      )
    } else {
      # Fallback if classification not available
      names(rv$adf_transformed)
    }

    # Capture fresh_load flag ONCE before the lapply to avoid issues
    is_fresh_load <- isTRUE(rv$subject_fresh_load)

    # Generate input widgets
    inputs <- lapply(subject_cols, function(col_name) {
      col_data <- rv$adf_transformed[[col_name]]
      input_id <- paste0("subj_", col_name)

      # PRIORITY ORDER for default values:
      # 1. If fresh load: Use selected_property values (ignore browser cache)
      # 2. If NOT fresh load: Check existing input values (preserves manual edits)
      # 3. Fall back to selected_property value
      # 4. Median/empty (fallback for fresh start)
      default_value <- NULL

      if (!is.null(rv$selected_property)) {
        if (is_fresh_load) {
          # Fresh load - use selected_property values directly, ignore browser cache
          if (col_name %in% names(rv$selected_property)) {
            default_value <- rv$selected_property[[col_name]]
            message("  Fresh load: Setting ", col_name, " = ", default_value)
          }
        } else {
          # Not fresh load - check for user's manual edits first
          existing_value <- tryCatch({
            val <- isolate(input[[input_id]])
            if (!is.null(val)) {
              if (is.numeric(col_data) && is.numeric(val)) {
                val
              } else if ((is.character(col_data) || is.factor(col_data)) && is.character(val) && val != "") {
                val
              } else {
                NULL
              }
            } else {
              NULL
            }
          }, error = function(e) NULL)

          if (!is.null(existing_value)) {
            default_value <- existing_value
          } else if (col_name %in% names(rv$selected_property)) {
            default_value <- rv$selected_property[[col_name]]
          }
        }
      }

      if (is.numeric(col_data)) {
        if (is.null(default_value) || (length(default_value) == 1 && is.na(default_value))) {
          default_value <- median(col_data, na.rm = TRUE)
        }

        numericInput(
          ns(input_id),
          label = col_name,
          value = as.numeric(default_value),
          min = min(col_data, na.rm = TRUE),
          max = max(col_data, na.rm = TRUE)
        )
      } else if (is.factor(col_data) || is.character(col_data)) {
        unique_vals <- unique(as.character(col_data))
        unique_vals <- unique_vals[!is.na(unique_vals)]

        if (length(unique_vals) <= 20) {
          if (is.null(default_value) || (length(default_value) == 1 && is.na(default_value))) {
            default_value <- ""
          } else {
            default_value <- as.character(default_value)
          }

          selectInput(
            ns(input_id),
            label = col_name,
            choices = c("", unique_vals),
            selected = default_value
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
    })

    # Reset fresh load flag AFTER we've captured the values but within this render cycle
    # Using on.exit ensures it runs after the UI is built
    on.exit({
      if (isTRUE(isolate(rv$subject_fresh_load))) {
        rv$subject_fresh_load <- FALSE
      }
    }, add = TRUE)

    # Remove NULL entries
    inputs <- inputs[!sapply(inputs, is.null)]

    if (length(inputs) == 0) {
      p("No subject inputs available.", class = "text-muted")
    } else {
      # Arrange in 3-column grid
      num_cols <- 3
      inputs_per_col <- ceiling(length(inputs) / num_cols)

      cols <- lapply(1:num_cols, function(col_idx) {
        start_idx <- (col_idx - 1) * inputs_per_col + 1
        end_idx <- min(col_idx * inputs_per_col, length(inputs))

        if (start_idx <= length(inputs)) {
          div(class = "col-md-4",
            inputs[start_idx:end_idx]
          )
        }
      })

      div(class = "row",
        cols
      )
    }
  })

  # ==========================================================================
  # Helper Button Observers
  # ==========================================================================

  # Select All button
  observeEvent(input$select_all_vars, {
    req(rv$adf_transformed)
    all_vars <- names(rv$adf_transformed)

    # Update all checkboxes to TRUE
    lapply(all_vars, function(var_name) {
      updateCheckboxInput(session, paste0("var_", make.names(var_name)), value = TRUE)
    })

    # Update reactive value
    rv$selected_vars <- all_vars

    showNotification("All variables selected", type = "message", duration = 2)
  })

  # Select None button
  observeEvent(input$select_none_vars, {
    req(rv$adf_transformed)
    all_vars <- names(rv$adf_transformed)

    # Update all checkboxes to FALSE
    lapply(all_vars, function(var_name) {
      updateCheckboxInput(session, paste0("var_", make.names(var_name)), value = FALSE)
    })

    # Update reactive value
    rv$selected_vars <- character(0)

    showNotification("All variables deselected", type = "message", duration = 2)
  })

  # Invert Selection button
  observeEvent(input$invert_selection, {
    req(rv$adf_transformed, rv$selected_vars)
    all_vars <- names(rv$adf_transformed)

    # Invert selection
    new_selected <- setdiff(all_vars, rv$selected_vars)

    # Update all checkboxes
    lapply(all_vars, function(var_name) {
      updateCheckboxInput(session, paste0("var_", make.names(var_name)),
                         value = var_name %in% new_selected)
    })

    # Update reactive value
    rv$selected_vars <- new_selected

    showNotification("Selection inverted", type = "message", duration = 2)
  })

  # Observer to track checkbox changes
  observe({
    req(rv$adf_transformed)
    all_vars <- names(rv$adf_transformed)

    # Get current state of all checkboxes
    checked_vars <- character(0)
    for (var_name in all_vars) {
      checkbox_id <- paste0("var_", make.names(var_name))
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        checked_vars <- c(checked_vars, var_name)
      }
    }

    # Update reactive value
    rv$selected_vars <- checked_vars
  })

  # Observer for variable preview buttons (eyeball icon)
  # Uses a tracking mechanism in 'rv' to prevent attaching duplicate observers
  observe({
    # Get all currently present preview buttons
    current_preview_buttons <- names(input)[startsWith(names(input), "preview_")]

    # Identify only the NEW ones that don't have observers yet
    new_buttons <- setdiff(current_preview_buttons, rv$observed_preview_buttons)

    if (length(new_buttons) > 0) {
      # Mark them as observed immediately to prevent race conditions
      rv$observed_preview_buttons <- c(rv$observed_preview_buttons, new_buttons)

      lapply(new_buttons, function(input_id) {
        observeEvent(input[[input_id]], {
          # Don't do anything if the button is just being created (value is 0)
          if (is.null(input[[input_id]]) || input[[input_id]] == 0) return()

          req(rv$adf_transformed)

          # Parse column name from button ID: preview_ColumnName
          # Remove "preview_" prefix
          col_normalized <- sub("^preview_", "", input_id)

          # Find matching column in actual data (handle make.names normalization)
          actual_cols <- names(rv$adf_transformed)
          normalized_cols <- make.names(actual_cols)
          match_idx <- which(normalized_cols == col_normalized)

          if (length(match_idx) == 0) return()
          var_name <- actual_cols[match_idx[1]]

          # Get first 50 values from the column
          values <- head(rv$adf_transformed[[var_name]], 50)

          # Format for display
          values_display <- sapply(values, function(v) {
            if (is.null(v) || is.na(v)) return("[blank]")
            v_str <- as.character(v)
            if (is.na(v_str) || !nzchar(trimws(v_str))) return("[blank]")
            if (nchar(v_str) > 200) {
              v_str <- paste0(substr(v_str, 1, 197), "...")
            }
            v_str
          }, USE.NAMES = FALSE)

          # Show modal with preview
          showModal(modalDialog(
            title = paste("Preview Column:", var_name),
            p(tags$strong("Column:"), var_name),
            p(tags$strong("Total rows:"), nrow(rv$adf_transformed)),
            p(tags$strong("Sample values (first 50 rows):")),
            tags$div(
              class = "preview-content",
              style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
              tags$ol(
                lapply(seq_along(values_display), function(i) {
                  tags$li(values_display[i])
                })
              )
            ),
            easyClose = TRUE,
            footer = modalButton("Close"),
            size = "l"
          ))
        })
      })
    }
  })

  # ==========================================================================
  # Transformation Engine Observers
  # ==========================================================================

  # Dynamic observer for 'inspect_' buttons.
  # Uses a tracking mechanism in 'rv' to prevent attaching duplicate observers,
  # which caused the "sticky variable" bug after transformations.
  observe({
    # Get all currently present inspect buttons
    current_inspect_buttons <- names(input)[startsWith(names(input), "inspect_")]

    # Identify only the NEW ones that don't have observers yet
    new_buttons <- setdiff(current_inspect_buttons, rv$observed_inspect_buttons)

    if (length(new_buttons) > 0) {
      # Mark them as observed immediately to prevent race conditions
      rv$observed_inspect_buttons <- c(rv$observed_inspect_buttons, new_buttons)

      lapply(new_buttons, function(input_id) {
        observeEvent(input[[input_id]], {
          # Don't do anything if the button is just being created (value is 0)
          if (is.null(input[[input_id]]) || input[[input_id]] == 0) return()

          # Parse column name from button ID: inspect_ColumnName
          # Remove "inspect_" prefix
          col_normalized <- sub("^inspect_", "", input_id)

          # Find matching column in actual data (handle make.names normalization)
          actual_cols <- names(rv$adf_transformed)
          normalized_cols <- make.names(actual_cols)
          match_idx <- which(normalized_cols == col_normalized)

          if (length(match_idx) == 0) return()
          column <- actual_cols[match_idx[1]]

          if (!column %in% names(rv$adf_transformed)) return()

           tryCatch({
             # Get field classification info
             fc_info <- NULL
             if (!is.null(rv$field_classification)) {
               fc_row <- rv$field_classification[rv$field_classification$name == column, ]
               if (nrow(fc_row) > 0) {
                 fc_info <- list(
                   role = fc_row$role[1],
                   relevance = fc_row$relevance[1],
                   reason = fc_row$reason[1]
                 )
               }
             }

             # Get column summary statistics
             summary_stats <- get_column_summary(rv$adf_transformed, column)

             # Build classification info UI
             classification_ui <- if (!is.null(fc_info)) {
               div(
                 h5("Field Classification"),
                 tags$table(
                   class = "table table-sm",
                   tags$tr(tags$td(tags$b("Role:")), tags$td(fc_info$role)),
                   tags$tr(tags$td(tags$b("Relevance:")), tags$td(fc_info$relevance)),
                   tags$tr(tags$td(tags$b("Reason:")), tags$td(fc_info$reason))
                 ),
                 hr()
               )
             } else {
               div()
             }

             # Build summary statistics UI
             stats_ui <- if (summary_stats$error) {
               p(class = "text-danger", summary_stats$message)
             } else {
               if (summary_stats$type == "numeric") {
                 # Numeric column
                 if (!is.null(summary_stats$message)) {
                   p(class = "text-warning", summary_stats$message)
                 } else {
                   div(
                     h5("Summary Statistics"),
                     tags$table(
                       class = "table table-sm",
                       tags$tr(tags$td(tags$b("Type:")), tags$td("Numeric")),
                       tags$tr(tags$td(tags$b("Valid values:")), tags$td(sprintf("%d (%.1f%%)", summary_stats$n_valid, 100 - summary_stats$pct_na))),
                       tags$tr(tags$td(tags$b("Missing (NA):")), tags$td(sprintf("%d (%.1f%%)", summary_stats$n_na, summary_stats$pct_na))),
                       tags$tr(tags$td(tags$b("Min:")), tags$td(format(summary_stats$min, big.mark = ","))),
                       tags$tr(tags$td(tags$b("Max:")), tags$td(format(summary_stats$max, big.mark = ","))),
                       tags$tr(tags$td(tags$b("Mean:")), tags$td(format(round(summary_stats$mean, 2), big.mark = ","))),
                       tags$tr(tags$td(tags$b("Median:")), tags$td(format(summary_stats$median, big.mark = ","))),
                       tags$tr(tags$td(tags$b("Std Dev:")), tags$td(format(round(summary_stats$sd, 2), big.mark = ",")))
                     )
                   )
                 }
               } else {
                 # Categorical column
                 if (!is.null(summary_stats$message)) {
                   p(class = "text-warning", summary_stats$message)
                 } else {
                   # Build top values UI
                   top_values_ui <- tags$ul(
                     lapply(1:nrow(summary_stats$top_values), function(i) {
                       val <- summary_stats$top_values$value[i]
                       count <- summary_stats$top_values$count[i]
                       pct <- summary_stats$top_values$pct[i]

                       # Handle special cases
                       display_val <- if (is.na(val) || val == "") "[Empty/NA]" else val

                       tags$li(sprintf("'%s': %d rows (%.1f%%)", display_val, count, pct))
                     })
                   )

                   div(
                     h5("Summary Statistics"),
                     tags$table(
                       class = "table table-sm",
                       tags$tr(tags$td(tags$b("Type:")), tags$td("Categorical/Text")),
                       tags$tr(tags$td(tags$b("Valid values:")), tags$td(sprintf("%d (%.1f%%)", summary_stats$n_valid, 100 - summary_stats$pct_na))),
                       tags$tr(tags$td(tags$b("Missing (NA):")), tags$td(sprintf("%d (%.1f%%)", summary_stats$n_na, summary_stats$pct_na))),
                       tags$tr(tags$td(tags$b("Unique values:")), tags$td(format(summary_stats$n_unique, big.mark = ",")))
                     ),
                     h6("Top 10 Most Frequent Values:"),
                     top_values_ui,
                     if (summary_stats$n_unique > 10) {
                       p(class = "text-muted small", sprintf("... and %d more unique values", summary_stats$n_unique - 10))
                     } else {
                       div()
                     }
                   )
                 }
               }
             }

             # Show modal with all information
             showModal(modalDialog(
               title = paste("Inspecting:", column),
               div(
                 classification_ui,
                 stats_ui,
                 hr(),
                 p(class="small text-muted", "This information helps you understand the column's role in the model and decide whether to include it in your analysis.")
               ),
               footer = modalButton("Close"),
               easyClose = TRUE,
               size = "l"
             ))
           }, error = function(e) {
             showNotification(paste("Error inspecting column:", e$message), type = "error")
           })
        })
      })
    }
  })

  # Dynamic observer for 'fix_' buttons.
  # Uses a tracking mechanism in 'rv' to prevent attaching duplicate observers,
  # which caused performance issues/unresponsiveness.
  observe({
    # Get all currently present fix buttons
    current_fix_buttons <- names(input)[startsWith(names(input), "fix_")]

    # Identify only the NEW ones that don't have observers yet
    new_buttons <- setdiff(current_fix_buttons, rv$observed_fix_buttons)

    if (length(new_buttons) > 0) {
      # Mark them as observed immediately to prevent race conditions
      rv$observed_fix_buttons <- c(rv$observed_fix_buttons, new_buttons)

      lapply(new_buttons, function(input_id) {
        observeEvent(input[[input_id]], {
          # Don't do anything if the button is just being created (value is 0)
          if (is.null(input[[input_id]]) || input[[input_id]] == 0) return()

          # Parse the button ID: fix_action_column
          parts <- strsplit(input_id, "_")[[1]]
          if (length(parts) < 3) return()

          action <- parts[2]
          column <- paste(parts[-(1:2)], collapse = "_") # Re-join column name if it had underscores

          # Ensure the column exists
          if (!column %in% names(rv$adf_transformed)) {
            showNotification(paste("Column '", column, "' not found."), type = "error")
            return()
          }

          # Perform the transformation
          tryCatch({
            if (action == "convert-to-numeric") {
              # Capture previous NAs to count NEW NAs
              prev_na <- sum(is.na(rv$adf_transformed[[column]]))
              
              # Clean aggressively: Remove $, commas, spaces, and parentheses
              # Regex: [$, ()] -> replace with empty string
              cleaned_vals <- gsub("[$, ()]", "", rv$adf_transformed[[column]])
              
              rv$adf_transformed <- rv$adf_transformed %>%
                mutate(!!column := as.numeric(cleaned_vals))
              
              # Stats
              new_vals <- rv$adf_transformed[[column]]
              curr_na <- sum(is.na(new_vals))
              new_na_count <- curr_na - prev_na
              valid_count <- sum(!is.na(new_vals))
              val_range <- range(new_vals, na.rm = TRUE)
              
              showModal(modalDialog(
                title = paste("Conversion Result:", column),
                div(
                  h4("Conversion Successful", class="text-success"),
                  p("Variable converted to R class:", code("numeric")),
                  hr(),
                  p(sprintf("Successfully converted %d values.", valid_count)),
                  if(new_na_count > 0)
                    div(class="alert alert-warning", icon("exclamation-triangle"),
                        sprintf("%d values could not be converted and were set to NA (e.g. non-numeric text).", new_na_count))
                  else
                    div(class="text-success", icon("check"), "No new missing values introduced."),
                  hr(),
                  p(strong("New Data Range:"), sprintf("%s to %s", dollar(val_range[1]), dollar(val_range[2]))),
                  p(class="text-muted", "The Quality Report below has been updated.")
                ),
                footer = modalButton("Close"),
                easyClose = TRUE
              ))

              # Recompute field classification after transformation
              rv$field_classification <- classify_variables_for_tree(
                data = rv$adf_transformed,
                response_var = if (!is.null(input$response_var) && input$response_var != "") {
                  input$response_var
                } else {
                  NA_character_
                },
                schema = get_default_schema()
              )

            } else if (action == "convert-to-factor") {
              rv$adf_transformed <- rv$adf_transformed %>%
                mutate(!!column := as.factor(.data[[column]]))

              # --- SMART FACTOR ASSESSMENT ---
              
              # 1. Identify Response Variable (Price)
              # We need to know "Price" to show if the factor matters.
              # Reuse the guessing logic or use the user's selection if available.
              temp_adf <- rv$adf_transformed
              numeric_vars <- names(temp_adf)[sapply(temp_adf, is.numeric)]
              
              # Try input$response_var first, then fall back to guessing
              response_col <- if (!is.null(input$response_var) && input$response_var != "") {
                input$response_var
              } else {
                numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]
              }
              
              has_response <- !is.na(response_col) && response_col %in% names(temp_adf)

              # 2. Calculate Group Stats
              new_factor <- temp_adf[[column]]
              n_levels <- nlevels(new_factor)
              
              # Assessment Logic
              assessment_badge <- ""
              assessment_msg <- ""
              
              if (n_levels < 2) {
                assessment_badge <- span(class="badge bg-danger", "Low Relevance")
                assessment_msg <- "Only 1 category found. This variable provides no information."
              } else if (n_levels > 50) {
                assessment_badge <- span(class="badge bg-warning text-dark", "Caution: High Cardinality")
                assessment_msg <- paste("Found", n_levels, "categories. Too many groups can cause overfitting.")
              } else {
                # Check balance
                counts <- table(new_factor)
                max_share <- max(prop.table(counts))
                if (max_share > 0.95) {
                   assessment_badge <- span(class="badge bg-warning text-dark", "Caution: Imbalanced")
                   assessment_msg <- paste("One category dominates", sprintf("%.1f%%", max_share*100), "of the data.")
                } else {
                   assessment_badge <- span(class="badge bg-success", "High Potential")
                   assessment_msg <- "Categories appear well-balanced."
                }
              }

              # Build the UI content
              stats_ui <- NULL
              plot_ui <- NULL
              
              if (has_response) {
                # Group Summaries using dplyr
                summary_stats <- temp_adf %>%
                  group_by(.data[[column]]) %>%
                  summarise(
                    Count = n(),
                    Avg_Price = mean(.data[[response_col]], na.rm = TRUE),
                    Median_Price = median(.data[[response_col]], na.rm = TRUE),
                    Std_Dev = sd(.data[[response_col]], na.rm = TRUE)
                  ) %>%
                  arrange(desc(Count)) %>%
                  head(10) # Top 10 for the table
                
                # Create simple HTML table
                stats_table_rows <- lapply(1:nrow(summary_stats), function(i) {
                  row <- summary_stats[i, ]
                  tags$tr(
                    tags$td(as.character(row[[column]])),
                    tags$td(format(row$Count, big.mark=",")),
                    tags$td(dollar(row$Avg_Price)),
                    tags$td(dollar(row$Median_Price))
                  )
                })
                
                stats_ui <- tags$table(class="table table-sm table-striped",
                  tags$thead(
                    tags$tr(tags$th("Category"), tags$th("Count"), tags$th("Avg Price"), tags$th("Median"))
                  ),
                  tags$tbody(stats_table_rows)
                )
                
                # Generate Plotly Boxplot
                # Note: We render this directly in the modal using renderPlotly logic IF possible,
                # but standard shiny modals are static. To make it interactive inside a modal,
                # we usually need an output binding. 
                # TRICK: We can use 'renderUI' in the main app to inject the modal, 
                # OR simpler: just show a static summary for now to ensure stability, 
                # as creating a dynamic output ID on the fly for every column is complex.
                #
                # COMPROMISE for Stability: We will stick to the Table for now. 
                # Interactive plots inside dynamic modals require pre-registered output slots.
                
              } else {
                stats_ui <- p(class="text-muted", "Identify a 'Price' variable to see group statistics.")
              }

              showModal(modalDialog(
                title = paste("Smart Assessment:", column),
                size = "l", # Large modal
                div(
                  div(class="d-flex justify-content-between align-items-center mb-3",
                      h4("Variable converted to R class:", code("factor"), class="m-0"),
                      assessment_badge
                  ),
                  div(class="alert alert-light border",
                      icon("info-circle"), assessment_msg
                  ),
                  hr(),
                  h5("Group Statistics (Top 10 Categories)"),
                  div(style = "max-height: 300px; overflow-y: auto;", 
                    stats_ui
                  ),
                  p(class="text-muted mt-2", "Comparing average prices between groups helps identify if this factor drives value."),
                  hr(),
                  p(class="text-muted", "The Quality Report below has been updated.")
                ),
                footer = modalButton("Close"),
                easyClose = TRUE
              ))

              # Recompute field classification after transformation
              rv$field_classification <- classify_variables_for_tree(
                data = rv$adf_transformed,
                response_var = if (!is.null(input$response_var) && input$response_var != "") {
                  input$response_var
                } else {
                  NA_character_
                },
                schema = get_default_schema()
              )

            } else if (action == "exclude-variable") {
              # Only remove from selected variables (uncheck it).
              # Do NOT delete from dataset, so it stays available in the variables tab.
              
              # Remove from selected variables if present
              if (!is.null(rv$selected_vars)) {
                rv$selected_vars <- setdiff(rv$selected_vars, column)
                
                # Update the checkbox in the UI
                updateCheckboxInput(session, paste0("var_", make.names(column)), value = FALSE)
              }
              
              showModal(modalDialog(
                title = paste("Variable Excluded:", column),
                div(
                  h4("Exclusion Successful", class="text-success"),
                  p(icon("check-square-o"), sprintf("The variable '%s' has been unchecked and removed from the analysis scope.", column)),
                  p(class="small text-muted", "It remains in your dataset and can be re-selected in the '3. Build Tree' tab if needed."),
                  hr(),
                  p(class="text-muted", "The Quality Report below has been updated.")
                ),
                footer = modalButton("Close"),
                easyClose = TRUE
              ))
            } else if (action == "include-variable") {
              # Re-add to selected variables
              if (!is.null(rv$selected_vars)) {
                rv$selected_vars <- unique(c(rv$selected_vars, column))
                
                # Update the checkbox in the UI
                updateCheckboxInput(session, paste0("var_", make.names(column)), value = TRUE)
              }
              
              showModal(modalDialog(
                title = paste("Variable Included:", column),
                div(
                  h4("Inclusion Successful", class="text-success"),
                  p(icon("check-square"), sprintf("The variable '%s' has been added back to the analysis.", column)),
                  hr(),
                  p(class="text-muted", "The Quality Report below has been updated.")
                ),
                footer = modalButton("Close"),
                easyClose = TRUE
              ))
            }

            # --- Re-run Data Quality Check on transformed data ---
            temp_adf <- rv$adf_transformed # Pass the newly transformed data directly
            numeric_vars <- names(temp_adf)[sapply(temp_adf, is.numeric)]
            guessed_response <- numeric_vars[str_detect(numeric_vars, regex("price|value|sold", ignore_case = TRUE))][1]

            if (!is.na(guessed_response) && guessed_response %in% names(temp_adf)) {
              guessed_predictors <- setdiff(names(temp_adf), guessed_response)
              rv$data_quality_issues <- validate_data_for_tree(
                data = temp_adf,
                response_var = guessed_response,
                predictors = guessed_predictors,
                selected_vars = rv$selected_vars,
                field_classification = rv$field_classification
              )
            }

          }, error = function(e) {
            showNotification(paste("Failed to transform '", column, "':", e$message), type = "error")
          })

        }, ignoreInit = TRUE)
      })
    }
  })


  # ==========================================================================
  # Data Validation Function for qeDT()
  # Based on qeML package requirements and decision tree best practices
  # ==========================================================================

  validate_data_for_tree <- function(data, response_var, predictors, selected_vars = NULL, field_classification = NULL) {
    message("\n--- DIAGNOSTIC LOG: Entered validate_data_for_tree function ---")
    issues <- list(
      errors = list(),    # Blocking issues - cannot proceed
      warnings = list(),  # Serious issues - user should know
      info = list(),      # Optional improvements
      report_time = format(Sys.time(), "%H:%M:%S") # Add timestamp for UI feedback
    )
    
    # --- HANDLE EXCLUDED VARIABLES ---
    # If selected_vars is provided, we want to:
    # 1. Skip creating exclusion badges if field_classification is available
    #    (UI shows classification status separately)
    # 2. Exclude them from the actual validation checks (so we don't warn about them).
    # 3. BUT always check the Response Variable.

    data_to_validate <- data

    if (!is.null(selected_vars)) {
      all_cols <- names(data)
      # Identify columns that are present but NOT selected
      excluded_cols <- setdiff(all_cols, selected_vars)
      # Ensure Response Variable is NEVER treated as excluded for validation purposes
      excluded_cols <- setdiff(excluded_cols, response_var)

      # Only create "Excluded" badges if we DON'T have field classification
      # (field_classification is shown in a separate UI section)
      if (is.null(field_classification) && length(excluded_cols) > 0) {
        for (col in excluded_cols) {
          msg <- sprintf("<b>'%s'</b> is excluded from analysis.", col)

          issue <- list(
            column_name = col,
            issue_type = "info",
            message = msg,
            action_type = "include-variable",
            action_label = "Undo Exclusion",
            action_icon = "undo",
            action_color = "secondary",
            is_excluded = TRUE
          )
          issues$info <- c(issues$info, list(issue))
        }
      }

      # Filter data to only Selected + Response for the actual checks
      keep_cols <- intersect(c(selected_vars, response_var), all_cols)
      data_to_validate <- data[, keep_cols, drop = FALSE]
    }

    n_rows <- nrow(data_to_validate)
    minsplit <- 20  # qeDT default
    minbucket <- 7  # qeDT default
    
    # Use the filtered data for all subsequent checks
    data <- data_to_validate
    predictors <- intersect(predictors, names(data))

    # ===== ERROR CHECKS (Blocking) =====

    # 1. Character variables (qeDT crashes on these) -> NOW with ACTIONABLE FIXES & SMART FILTERING
    char_cols <- names(data)[sapply(data, is.character)]
    message(sprintf("DIAGNOSTIC LOG: Found %d character columns to check: %s\n", length(char_cols), paste(char_cols, collapse=", ")))
    
    if (length(char_cols) > 0) {
      for (col in char_cols) {
        message(sprintf("DIAGNOSTIC LOG: Processing character column '%s'.\n", col))
        
        # --- AUTO-EXCLUSION LOGIC ---
        # Check Uniqueness (Cardinality)
        n_unique <- length(unique(data[[col]]))
        pct_unique <- n_unique / n_rows
        
        # If >95% unique, it's likely an ID or Address -> SKIP IT (Don't flag as error)
        if (pct_unique > 0.95 && n_unique > 20) {
             message(sprintf("DIAGNOSTIC LOG: Auto-excluding '%s' (%.1f%% unique). Likely ID/Address.\n", col, pct_unique*100))
             next 
        }
        
        # If only 1 unique value, it's constant -> SKIP IT (or Info)
        if (n_unique <= 1) {
             msg <- sprintf("<b>'%s'</b> is constant (only 1 value). It will be ignored.", col)
             issues$info <- c(issues$info, list(msg))
             next
        }
        
        sample_vals <- paste(head(unique(data[[col]]), 5), collapse=", ")
        
        # Check if column looks numeric
        # Improved Logic: Strip common non-numeric chars ($, , space, parens) first
        col_clean <- gsub("[$, ()]", "", na.omit(data[[col]]))
        col_clean <- col_clean[col_clean != ""] # Remove empty strings
        
        # Check if what remains is numeric
        # strict check: ^-?[0-9.]+$ means ONLY numbers, dots, and optional negative sign allowed.
        is_numeric_like <- all(grepl("^-?[0-9.]+$", col_clean)) && length(col_clean) > 0
        
        # --- NEW: Context-Aware Checks to prevent False Positives ---

        # 1. Alphabet Check: If it has letters (that aren't N/A or NULL), it's NOT numeric.
        # This catches "123 Main St" which has "Main St"
        has_letters <- any(grepl("[a-zA-Z]", data[[col]], ignore.case=TRUE) &
                          !grepl("^(na|null|none|nan)$", data[[col]], ignore.case=TRUE))

        if (has_letters) {
           is_numeric_like <- FALSE
           # message(sprintf("DIAGNOSTIC LOG: Column '%s' has letters, flagged as NOT numeric.\n", col))
        }

        # 2. Use field classification to determine column type (if available)
        is_date_col <- FALSE
        is_likely_price <- FALSE
        is_address_col <- FALSE

        if (!is.null(field_classification)) {
          # Look up column in classification
          fc_row <- field_classification[field_classification$name == col, ]
          if (nrow(fc_row) > 0) {
            col_role <- fc_row$role[1]
            # Date columns have "time" role
            is_date_col <- col_role == "time"
            # Price columns have "price" role
            is_likely_price <- col_role == "price"
            # Address columns have "address" role
            is_address_col <- col_role == "address"
          }
        } else {
          # Fallback to pattern matching if classification not available
          is_date_col <- grepl("date|sold|list|pend", col, ignore.case = TRUE) && !grepl("price|value", col, ignore.case = TRUE)
          is_likely_price <- grepl("price|value|sold", col, ignore.case = TRUE) && !is_date_col
          is_address_col <- grepl("street|ave|blvd|road", col, ignore.case = TRUE)
        }

        if (is_date_col) {
           is_numeric_like <- FALSE
        }

        # Special Case: Price columns should ALMOST ALWAYS be numeric
        if (is_likely_price && !is_numeric_like) {
           # If it's a price column but has junk, force it to be numeric-like
           # so we can offer a "Clean & Convert" option
           # But ONLY if it doesn't look like a pure address
           if (!is_address_col) {
              is_numeric_like <- TRUE
              message(sprintf("DIAGNOSTIC LOG: Column '%s' identified as Price. Forcing numeric check.\n", col))
           }
        }

        action_type <- NULL
        action_label <- NULL
        action_icon <- NULL   # New: Distinct icon
        action_color <- NULL  # New: Distinct color
        suggestion_text <- ""
        assessment_badge <- "" # New: Smart Assessment Badge

        if (is_numeric_like) {
          action_type <- "convert-to-numeric"
          action_label <- "Convert to Numeric"
          action_icon <- "hashtag" # Represents numbers
          action_color <- "primary" # Blue for numeric
          
          # Secondary Action: Exclude
          action_type_secondary <- "exclude-variable"
          action_label_secondary <- "Exclude Variable"
          action_icon_secondary <- "trash"
          action_color_secondary <- "danger"
          
          suggestion_text <- "This column appears to contain numbers (or currency) and should be converted. Or exclude it if not needed."
          assessment_badge <- "<span class='badge bg-primary'>Numeric Candidate</span>"
          
        } else {
          # Only suggest FACTOR if cardinality is reasonable.
          # If it has > 50 unique values and is NOT numeric, it's likely a mess or free text.
          if (n_unique > 50) {
             msg <- sprintf("<b>'%s'</b> is text with high cardinality (%d unique values).<br><small>Too many levels for a Factor. Consider excluding or cleaning.</small>", col, n_unique)
             issues$warnings <- c(issues$warnings, list(msg))
             next # Skip generating an "Action" for this
          }
          
          action_type <- "convert-to-factor"
          action_label <- "Convert to Factor"
          action_icon <- "list" # Represents list/categories
          action_color <- "warning" # Orange/Yellow for factors
          
          # Secondary Action: Exclude
          action_type_secondary <- "exclude-variable"
          action_label_secondary <- "Exclude Variable"
          action_icon_secondary <- "trash"
          action_color_secondary <- "danger"
          
          suggestion_text <- "This column contains text categories and should be converted to a Factor. Or exclude it if not needed."
          
          # --- SMART ASSESSMENT (Pre-Calculate) ---
          # Check balance for factors
          counts <- table(data[[col]])
          max_share <- max(prop.table(counts))
          
          if (max_share > 0.95) {
             assessment_badge <- "<span class='badge bg-warning text-dark'>Caution: Imbalanced</span>"
          } else {
             assessment_badge <- "<span class='badge bg-success'>High Potential</span>"
          }
        }

        # Main message with Badge
        msg <- sprintf("<b>'%s'</b> %s<br><small>%s</small>", col, assessment_badge, suggestion_text)
        
        # Create a structured issue object
        issue <- list(
          column_name = col,
          issue_type = "error",
          message = msg,
          action_type = action_type,
          action_label = action_label,
          action_icon = action_icon, 
          action_color = action_color,
          action_type_secondary = action_type_secondary,
          action_label_secondary = action_label_secondary,
          action_icon_secondary = action_icon_secondary,
          action_color_secondary = action_color_secondary
        )
        
        issues$errors <- c(issues$errors, list(issue))
      }
    }

    # 2. Response variable must be numeric or factor
    if (!is.null(response_var)) {
      if (!is.numeric(data[[response_var]]) && !is.factor(data[[response_var]])) {
        msg <- sprintf("<b>Response variable '%s'</b> is type '%s'.<br>
          <b>Required:</b> Must be numeric (regression) or factor (classification)",
          response_var, class(data[[response_var]])[1])
        issues$errors <- c(issues$errors, list(msg))
      }
    }

    # 3. Extreme high cardinality (essentially unique IDs)
    factor_cols <- names(data)[sapply(data, is.factor)]
    for (col in factor_cols) {
      n_levels <- nlevels(data[[col]])
      cardinality_ratio <- n_levels / n_rows

      if (cardinality_ratio > 0.8) {
        msg <- sprintf("<b>'%s'</b> has %d levels in %d rows (%.0f%% unique).<br>
          <b>Impact:</b> Essentially a unique ID - will cause severe overfitting.<br>
          <b>Recommended:</b> Remove this variable (tree will memorize, not learn).<br>
          <small>Alternative: Extract meaningful features (e.g., category, region)</small>",
          col, n_levels, n_rows, cardinality_ratio * 100)
          
        issue <- list(
          column_name = col,
          issue_type = "error",
          message = msg,
          action_type = "exclude-variable",
          action_label = "Exclude Variable",
          action_icon = "trash",
          action_color = "danger"
        )
        issues$errors <- c(issues$errors, list(issue))
      }
    }

    # 4. Insufficient sample size for tree building
    if (n_rows < minsplit) {
      msg <- sprintf("<b>Sample size too small:</b> %d rows (minimum: %d).<br>
        <b>Result:</b> Tree cannot split - will produce single-node 'tree'.<br>
        <b>Options:</b> Collect more data or decrease minsplit parameter",
        n_rows, minsplit)
      issues$errors <- c(issues$errors, list(msg))
    }

    # 5. Insufficient minority class for classification
    if (!is.null(response_var) && is.factor(data[[response_var]])) {
      
      # --- NEW CHECK: Is this actually a messy numeric variable? ---
      # Often users select a Price column that is text/factor.
      # We need to warn them to convert it, rather than complaining about "minority class".
      
      col_data <- data[[response_var]]
      col_clean <- gsub("[$, ()]", "", na.omit(as.character(col_data)))
      col_clean <- col_clean[col_clean != ""]
      
      is_numeric_like <- all(grepl("^-?[0-9.]+$", col_clean)) && length(col_clean) > 0
      
      if (is_numeric_like) {
        # It IS numeric-like! Flag as a conversion error instead of classification error.
        msg <- sprintf("<b>Response variable '%s'</b> appears to be Numeric (e.g., Price) but is formatted as Text/Category.<br>
          <b>Impact:</b> Model treats this as a Classification problem with %d unique classes (too many!).<br>
          <b>Solution:</b> Convert to Numeric immediately.",
          response_var, nlevels(col_data))
          
        issue <- list(
          column_name = response_var,
          issue_type = "error",
          message = msg,
          action_type = "convert-to-numeric",
          action_label = "Convert to Numeric",
          action_icon = "hashtag",
          action_color = "primary",
          # Secondary Action: Exclude (User might decide this isn't the right target after all)
          action_type_secondary = "exclude-variable",
          action_label_secondary = "Exclude Variable",
          action_icon_secondary = "trash",
          action_color_secondary = "danger"
        )
        issues$errors <- c(issues$errors, list(issue))
        
      } else {
        # It's NOT numeric-like, so proceed with standard classification checks
        class_counts <- table(data[[response_var]])
        min_class_count <- min(class_counts)
        min_class_name <- names(class_counts)[which.min(class_counts)]
  
        if (min_class_count < 10) {
          msg <- sprintf("<b>Minority class '%s'</b> has only %d observations.<br>
            <b>Impact:</b> Insufficient for reliable learning (need 30+ per class).<br>
            <b>Recommended:</b> Collect more data for this class",
            min_class_name, min_class_count)
          issues$errors <- c(issues$errors, list(msg))
        }
      }
    }

    # ===== WARNING CHECKS (Serious Issues) =====

    # 6. High missing rate in predictors
    for (col in predictors) {
      missing_count <- sum(is.na(data[[col]]))
      missing_pct <- (missing_count / n_rows) * 100

      if (missing_pct > 20) {
        msg <- sprintf("<b>'%s'</b> has %.1f%% missing values (%d of %d rows).<br>
          <b>Impact:</b> qeDT will randomly assign at splits (non-deterministic, poor splits).<br>
          <b>Options:</b> Exclude this variable, or accept random splitting.",
          col, missing_pct, missing_count, n_rows)
          
        issue <- list(
          column_name = col,
          issue_type = "warning",
          message = msg,
          action_type = "exclude-variable",
          action_label = "Exclude Variable",
          action_icon = "trash",
          action_color = "danger"
        )
        issues$warnings <- c(issues$warnings, list(issue))
        
      } else if (missing_pct > 5 && missing_pct <= 20) {
        msg <- sprintf("<b>'%s'</b> has %.1f%% missing values (%d of %d rows).<br>
          <small>Moderate missing data - qeDT will use random splitting for these cases</small>",
          col, missing_pct, missing_count, n_rows)
          
        issue <- list(
          column_name = col,
          issue_type = "info",
          message = msg,
          action_type = "exclude-variable",
          action_label = "Exclude Variable",
          action_icon = "trash",
          action_color = "secondary" # Less urgent color for info
        )
        issues$info <- c(issues$info, list(issue))
      }
    }

    # 7. Missing values in response (auto-removed by qeDT)
    if (!is.null(response_var)) {
      response_missing <- sum(is.na(data[[response_var]]))
      if (response_missing > 0) {
        response_missing_pct <- (response_missing / n_rows) * 100
        remaining_rows <- n_rows - response_missing

        if (response_missing_pct > 10) {
          msg <- sprintf("<b>Response variable '%s'</b> has %d missing values (%.1f%%).<br>
            <b>Action:</b> qeDT will automatically remove these rows.<br>
            <b>Result:</b> Effective sample size: %d → %d rows<br>
            <small>⚠ If missingness correlates with predictors, model may be biased</small>",
            response_var, response_missing, response_missing_pct, n_rows, remaining_rows)
          issues$warnings <- c(issues$warnings, list(msg))
        } else {
          msg <- sprintf("<b>Response variable</b> has %d missing values (%.1f%%).<br>
            <small>Will be automatically removed. Effective sample: %d → %d rows</small>",
            response_missing, response_missing_pct, n_rows, remaining_rows)
          issues$info <- c(issues$info, list(msg))
        }
      }
    }

    # 8. Small sample size
    if (n_rows >= minsplit && n_rows < 50) {
      msg <- sprintf("<b>Small sample size:</b> %d rows.<br>
        <b>Impact:</b> High variance, overfitting risk.<br>
        <b>Recommendations:</b><br>
        &nbsp;&nbsp;• Collect more data if possible<br>
        &nbsp;&nbsp;• Increase minbucket/minsplit parameters<br>
        &nbsp;&nbsp;• Limit tree depth (maxdepth parameter)<br>
        &nbsp;&nbsp;• Consider simpler modeling approach",
        n_rows)
      issues$warnings <- c(issues$warnings, list(msg))
    } else if (n_rows >= 50 && n_rows < 100) {
      msg <- sprintf("<b>Relatively small sample:</b> %d rows.<br>
        <small>Consider limiting tree complexity to prevent overfitting</small>",
        n_rows)
      issues$info <- c(issues$info, list(msg))
    }

    # 9. Low observations per predictor
    n_predictors <- length(predictors)
    obs_per_predictor <- n_rows / n_predictors
    if (obs_per_predictor < 20 && n_predictors > 2) {
      msg <- sprintf("<b>Low observations-to-predictor ratio:</b> %.1f obs/predictor.<br>
        (%d observations, %d predictors)<br>
        <b>Rule of thumb:</b> Need 20+ observations per predictor.<br>
        <b>Options:</b> Feature selection, collect more data, limit tree complexity",
        obs_per_predictor, n_rows, n_predictors)
      issues$warnings <- c(issues$warnings, list(msg))
    }

    # 10. Imbalanced response (classification)
    if (!is.null(response_var) && is.factor(data[[response_var]])) {
      class_dist <- prop.table(table(data[[response_var]]))
      max_class_pct <- max(class_dist) * 100
      min_class_pct <- min(class_dist) * 100

      if (max_class_pct > 95) {
        majority_class <- names(class_dist)[which.max(class_dist)]
        minority_class <- names(class_dist)[which.min(class_dist)]
        minority_count <- sum(data[[response_var]] == minority_class, na.rm=TRUE)

        msg <- sprintf("<b>Severely imbalanced response:</b> %.1f%% / %.1f%%<br>
          <b>Impact:</b> Tree will favor '%s' class, may ignore '%s' entirely.<br>
          <b>Options:</b><br>
          &nbsp;&nbsp;1. Accept imbalance (if reflects reality)<br>
          &nbsp;&nbsp;2. Oversample minority class<br>
          &nbsp;&nbsp;3. Undersample majority class<br>
          &nbsp;&nbsp;4. Collect more '%s' cases (need ~30+ for learning)<br>
          <small>Current '%s' count: %d cases</small>",
          max_class_pct, min_class_pct, majority_class, minority_class,
          minority_class, minority_class, minority_count)
        issues$warnings <- c(issues$warnings, list(msg))
      } else if (max_class_pct > 80) {
        msg <- sprintf("<b>Moderately imbalanced response:</b> %.1f%% / %.1f%%<br>
          <small>Monitor minority class performance separately</small>",
          max_class_pct, min_class_pct)
        issues$info <- c(issues$info, list(msg))
      }
    }

    # 11. High cardinality factors (moderate)
    for (col in factor_cols) {
      n_levels <- nlevels(data[[col]])
      cardinality_ratio <- n_levels / n_rows

      if (cardinality_ratio <= 0.8 && n_levels > 50) {
        level_counts <- table(data[[col]])
        rare_levels <- sum(level_counts <= 5)

        msg <- sprintf("<b>'%s'</b> has %d levels (high cardinality).<br>
          <b>Impact:</b> Overfitting risk, computationally expensive (%.0f possible splits).<br>
          <b>Options:</b> Exclude variable, or group rare levels manually.<br>
          <small>%d levels appear ≤5 times (candidates for grouping)</small>",
          col, n_levels, 2^(n_levels-1)-1, rare_levels)
          
        issue <- list(
          column_name = col,
          issue_type = "warning",
          message = msg,
          action_type = "exclude-variable",
          action_label = "Exclude Variable",
          action_icon = "trash",
          action_color = "danger"
        )
        issues$warnings <- c(issues$warnings, list(issue))
        
      } else if (cardinality_ratio <= 0.8 && n_levels > 20) {
        msg <- sprintf("<b>'%s'</b> has %d levels (moderate-high cardinality).<br>
          <small>Consider grouping rare levels if overfitting occurs</small>",
          col, n_levels)
          
        issue <- list(
          column_name = col,
          issue_type = "info",
          message = msg,
          action_type = "exclude-variable",
          action_label = "Exclude Variable",
          action_icon = "trash",
          action_color = "secondary"
        )
        issues$info <- c(issues$info, list(issue))
      }
    }

    # ===== INFO CHECKS (Optional Improvements) =====

    # 12. Near-zero variance predictors
    for (col in predictors) {
      if (is.numeric(data[[col]])) {
        unique_vals <- length(unique(na.omit(data[[col]])))
        if (unique_vals == 1) {
          msg <- sprintf("<b>'%s'</b> is constant (all values = %s).<br>
            <small>Tree will ignore (no information). Optional: Remove for clarity</small>",
            col, unique(data[[col]])[1])
          issues$info <- c(issues$info, list(msg))
        }
      } else if (is.factor(data[[col]])) {
        level_freq <- prop.table(table(data[[col]]))
        max_freq <- max(level_freq)
        if (max_freq > 0.95) {
          dominant_level <- names(level_freq)[which.max(level_freq)]
          msg <- sprintf("<b>'%s'</b> is near-constant (%.1f%% = '%s').<br>
            <small>Tree will likely ignore. Optional: Remove or investigate data quality</small>",
            col, max_freq * 100, dominant_level)
          issues$info <- c(issues$info, list(msg))
        }
      }
    }

    # 13. Unordered factors with many levels
    for (col in factor_cols) {
      if (!is.ordered(data[[col]])) {
        n_levels <- nlevels(data[[col]])
        if (n_levels > 10 && n_levels <= 20) {
          levels_vec <- levels(data[[col]])
          ordinal_patterns <- c("low|medium|high", "small|medium|large",
                               "poor|fair|good", "never|sometimes|often|always")
          possibly_ordinal <- any(sapply(ordinal_patterns, function(p) {
            any(grepl(p, levels_vec, ignore.case = TRUE))
          }))

          if (possibly_ordinal) {
            speedup <- ceiling(2^(n_levels-1) / n_levels)
            msg <- sprintf("<b>'%s'</b> has %d unordered levels, but appears ordinal.<br>
              <small>If levels have natural order, convert to ordered factor for:<br>
              &nbsp;&nbsp;• ~%dx faster computation<br>
              &nbsp;&nbsp;• More interpretable splits<br>
              &nbsp;&nbsp;• Less overfitting risk<br>
              Example: data$%s <- ordered(data$%s, levels=c(...))</small>",
              col, n_levels, speedup, col, col)
            issues$info <- c(issues$info, list(msg))
          }
        }
      }
    }

    message("--- DIAGNOSTIC LOG: Final issues object structure ---")
    message(paste(capture.output(str(issues)), collapse = "\n"))
    message("--- END LOG ---\n\n")
    return(issues)
  }

  # Reactive trigger for actual tree building
  # This allows both "Build Tree" button and "Proceed Anyway" to trigger the same logic
  build_tree_trigger <- reactiveVal(0)

  # Handler for "Build Tree" button - validates first
  observeEvent(input$build_tree, {
    # Replace silent req() with explicit check and user feedback
    if (is.null(rv$adf_transformed)) {
      showNotification("Please upload data first.", type = "error")
      return()
    }
    if (is.null(input$response_var) || input$response_var == "") {
      showNotification("Please select a Response Variable (Price). If dropdown is empty, no numeric columns were found in your data.", type = "error", duration = 10)
      return()
    }

    rv$skip_validation <- FALSE  # Always validate on initial button click
    build_tree_trigger(build_tree_trigger() + 1)  # Trigger the actual build
  })

  # Handler for "Proceed Anyway" button in validation modal
  observeEvent(input$proceed_anyway, {
    message("=== PROCEED ANYWAY CLICKED ===")
    removeModal()  # Close the validation modal
    rv$skip_validation <- TRUE  # Skip validation this time
    message(sprintf("Setting build_tree_trigger from %d to %d", build_tree_trigger(), build_tree_trigger() + 1))
    build_tree_trigger(build_tree_trigger() + 1)  # Trigger the actual build
    message("=== PROCEED ANYWAY COMPLETE ===")
  })

  # ACTUAL TREE BUILDING LOGIC (triggered by either button)
  observeEvent(build_tree_trigger(), {
    message(sprintf("\n=== TREE BUILD OBSERVER TRIGGERED (trigger=%d) ===", build_tree_trigger()))
    message(sprintf("Has adf_transformed: %s", !is.null(rv$adf_transformed)))
    message(sprintf("Has response_var: %s", !is.null(input$response_var) && input$response_var != ""))
    message(sprintf("Response var value: '%s'", input$response_var))

    req(rv$adf_transformed, input$response_var, build_tree_trigger() > 0)
    message("=== PASSED req() CHECK ===")

    withProgress(message = 'Building Decision Tree...', value = 0, {
      message("=== ENTERED withProgress ===")
      incProgress(0.1, detail = "Starting...")

      # Determine which variables to use based on Include/Exclude mode
      all_vars <- names(rv$adf_transformed)
      # Track source row IDs so CMS extraction can map model rows back to original data.
      source_row_ids <- seq_len(nrow(rv$adf_transformed))

      # Get user's variable selection
      user_selected_vars <- if (!is.null(rv$selected_vars)) rv$selected_vars else character(0)

      # Apply Include/Exclude mode logic
      if (input$selection_mode == "include") {
        # INCLUDE mode: Use only selected variables
        vars_to_use <- user_selected_vars
      } else {
        # EXCLUDE mode: Use all except selected variables
        vars_to_use <- setdiff(all_vars, user_selected_vars)
      }

      # Auto-exclude: TRUST THE USER SELECTION (rv$selected_vars)
      # The user has already curated the list in the "Quality Report" tab.
      # We do NOT apply aggressive hard-coded exclusions here anymore.

      final_vars <- vars_to_use

      # Only apply temporal market variable exclusions if requested
      # Use field classification to identify temporal market variables (DOM, SPAP, SaleQtr)
      if (!isTRUE(input$include_temporal_vars)) {
        if (!is.null(rv$field_classification)) {
          # Identify temporal market columns from classification
          temporal_market_cols <- rv$field_classification$name[
            rv$field_classification$role == "time" &
            tolower(rv$field_classification$name) %in% c("dom", "days on market", "spap", "sp/ap", "saleqtr", "sale quarter")
          ]
          # Exclude these from final variables
          final_vars <- setdiff(final_vars, temporal_market_cols)
        } else {
          # Fallback to pattern matching if classification not available
          temporal_patterns <- c("dom", "spap", "saleqtr")
          temporal_exclude <- grepl(paste(temporal_patterns, collapse = "|"), final_vars, ignore.case = TRUE)
          final_vars <- final_vars[!temporal_exclude]
        }
      }

      # Ensure response variable is included
      if (!(input$response_var %in% final_vars)) {
        final_vars <- c(final_vars, input$response_var)
      }

      # Create data subset with final variables
      keep_vars <- intersect(all_vars, final_vars)
      adf_for_tree <- rv$adf_transformed[, keep_vars, drop = FALSE]

      # IMPORTANT: Filter to sold listings if response variable is Sold Price
      # This prevents including Active/Pending/Expired listings with NA sold prices
      response_is_sold_price <- grepl("sold.*price|price.*sold", input$response_var, ignore.case = TRUE)
      has_status_column <- any(grepl("^status$", names(adf_for_tree), ignore.case = TRUE))

      if (response_is_sold_price && has_status_column) {
        # Find the Status column (case-insensitive)
        status_col_idx <- which(grepl("^status$", names(adf_for_tree), ignore.case = TRUE))[1]
        status_col_name <- names(adf_for_tree)[status_col_idx]

        # Count rows before filtering
        rows_before <- nrow(adf_for_tree)

        # Filter to only sold status
        # Common sold status values: "Sold", "Closed", "S", etc.
        sold_mask <- grepl("^(sold|closed|s)$", adf_for_tree[[status_col_name]], ignore.case = TRUE)
        sold_mask[is.na(sold_mask)] <- FALSE
        adf_for_tree <- adf_for_tree[sold_mask, , drop = FALSE]
        source_row_ids <- source_row_ids[sold_mask]

        rows_after <- nrow(adf_for_tree)
        rows_filtered <- rows_before - rows_after

        if (rows_filtered > 0) {
          message(sprintf("*** FILTERED TO SOLD LISTINGS ONLY ***"))
          message(sprintf("Response variable '%s' requires sold listings", input$response_var))
          message(sprintf("Filtered out %d non-sold listings (%d → %d rows)",
                          rows_filtered, rows_before, rows_after))

          showNotification(
            sprintf("Filtered to %d sold listings (removed %d active/pending/expired)",
                    rows_after, rows_filtered),
            type = "message",
            duration = 8
          )
        }

        # Check if we have enough data left
        if (rows_after < 20) {
          showNotification(
            sprintf("Warning: Only %d sold listings found. Need at least 20 for reliable tree building.",
                    rows_after),
            type = "warning",
            duration = 10
          )
        }
      }

      # First, convert numeric-formatted character columns to actual numeric
      adf_for_tree <- convert_numeric_formatted_strings(adf_for_tree)

      # CRITICAL: Force-convert response variable to numeric if it's character/factor
      # This handles cases where the response variable has <90% numeric values
      # but is still intended to be used as a numeric response (e.g., "Sold Price")
      if (input$response_var %in% names(adf_for_tree)) {
        resp_col <- adf_for_tree[[input$response_var]]

        if (is.character(resp_col) || is.factor(resp_col)) {
          message(sprintf("Response variable '%s' is %s, attempting forced numeric conversion",
                          input$response_var, class(resp_col)[1]))

          # Convert to character first (in case it's a factor)
          resp_char <- as.character(resp_col)

          # Strip $, commas, spaces and convert to numeric
          resp_numeric <- suppressWarnings(as.numeric(gsub("[,[:space:]]", "", resp_char)))

          # Calculate success rate
          success_rate <- sum(!is.na(resp_numeric)) / length(resp_numeric)

          if (success_rate > 0.5) {  # Lower threshold for response variable (50% instead of 90%)
            adf_for_tree[[input$response_var]] <- resp_numeric
            message(sprintf("✓ Force-converted response variable '%s' to numeric (%.1f%% success)",
                            input$response_var, success_rate * 100))
          } else {
            message(sprintf("✗ Cannot convert '%s' - only %.1f%% numeric values",
                            input$response_var, success_rate * 100))
          }
        }
      }

      # Then convert remaining character columns to factors (qeML requirement)
      adf_for_tree <- adf_for_tree %>%
        mutate(across(where(is.character), as.factor))

      # Convert ordered factors to regular factors (qeML requirement)
      adf_for_tree <- adf_for_tree %>%
        mutate(across(where(is.ordered), ~factor(as.character(.), ordered = FALSE)))

      # IMPORTANT: Sanitize column names for qeDT (spaces and special chars cause formula errors)
      # Store original response variable name for later use
      original_response_var <- input$response_var

      # Convert column names to valid R identifiers (spaces become dots)
      names(adf_for_tree) <- make.names(names(adf_for_tree))

      # Update response variable name to match sanitized column name
      response_var_for_tree <- make.names(original_response_var)

      message(sprintf("Sanitized column names - Response: '%s' -> '%s'",
                      original_response_var, response_var_for_tree))

      # Only run validation if user hasn't clicked "Proceed Anyway"
      message(sprintf("skip_validation = %s", isTRUE(rv$skip_validation)))
      if (!isTRUE(rv$skip_validation)) {
        message("=== RUNNING VALIDATION ===")
        incProgress(0.1, detail = "Validating data quality...")

        # Run comprehensive data validation
        predictors <- setdiff(names(adf_for_tree), response_var_for_tree)
        validation_issues <- validate_data_for_tree(
          adf_for_tree,
          response_var_for_tree,
          predictors,
          selected_vars = rv$selected_vars,
          field_classification = rv$field_classification
        )

        # Check if there are any issues
        has_errors <- length(validation_issues$errors) > 0
        has_warnings <- length(validation_issues$warnings) > 0
        has_info <- length(validation_issues$info) > 0
        message(sprintf("Validation results - Errors: %d, Warnings: %d, Info: %d",
                        length(validation_issues$errors),
                        length(validation_issues$warnings),
                        length(validation_issues$info)))

        if (has_errors || has_warnings || has_info) {
          message("=== SHOWING VALIDATION MODAL (STOPPING TREE BUILD) ===")
        # Build HTML report
        report_html <- "<div style='max-height: 500px; overflow-y: auto;'>"

        if (has_errors) {
          report_html <- paste0(report_html,
            "<h4 style='color: #d9534f;'>⛔ ERRORS (Must Fix Before Proceeding)</h4>",
            "<div style='background-color: #f2dede; border-left: 4px solid #d9534f; padding: 10px; margin-bottom: 15px; color: #333333;'>",
            paste(validation_issues$errors, collapse = "<hr style='border-color: #d9534f;'>"),
            "</div>")
        }

        if (has_warnings) {
          report_html <- paste0(report_html,
            "<h4 style='color: #f0ad4e;'>⚠ WARNINGS (Recommend Addressing)</h4>",
            "<div style='background-color: #fcf8e3; border-left: 4px solid #f0ad4e; padding: 10px; margin-bottom: 15px; color: #333333;'>",
            paste(validation_issues$warnings, collapse = "<hr style='border-color: #f0ad4e;'>"),
            "</div>")
        }

        if (has_info) {
          report_html <- paste0(report_html,
            "<h4 style='color: #5bc0de;'>ℹ INFORMATION (Optional Improvements)</h4>",
            "<div style='background-color: #d9edf7; border-left: 4px solid #5bc0de; padding: 10px; margin-bottom: 15px;'>",
            paste(validation_issues$info, collapse = "<hr style='border-color: #5bc0de;'>"),
            "</div>")
        }

        report_html <- paste0(report_html, "</div>")

        # Show modal with validation results
        showModal(modalDialog(
          title = if (has_errors) {
            "❌ Data Quality Issues Detected - Cannot Proceed"
          } else if (has_warnings) {
            "⚠ Data Quality Warnings - Review Before Proceeding"
          } else {
            "ℹ Data Quality Suggestions"
          },
          HTML(report_html),
          footer = tagList(
            if (has_errors) {
              tagList(
                p(strong("Action Required:"), "Fix errors in your data before building tree.",
                  style = "color: #d9534f; margin-top: 10px;"),
                modalButton("Close")
              )
            } else {
              tagList(
                p(style = "color: #666; margin-top: 10px; font-size: 0.9em;",
                  if (has_warnings) {
                    "You can proceed, but addressing these issues will improve model quality."
                  } else {
                    "You can proceed. These are optional suggestions for improvement."
                  }),
                modalButton("Cancel"),
                actionButton(ns("proceed_anyway"), "Proceed with Tree Building",
                            class = if (has_warnings) "btn-warning" else "btn-primary")
              )
            }
          ),
          size = "l",
          easyClose = !has_errors
        ))

          return()  # Stop tree building, wait for user decision
        }
      }

      # If we reach here, either validation passed or user acknowledged issues
      message("=== PASSED VALIDATION / USER ACKNOWLEDGED - CONTINUING TO BUILD TREE ===")
      incProgress(0.3, detail = "Preparing data...")

      # qeDT/trafo cannot handle Date/IDate columns; drop them from the modeling data
      date_cols <- sapply(adf_for_tree, function(x) inherits(x, "Date") || inherits(x, "IDate"))
      if (any(date_cols)) {
        message(sprintf("Removing %d Date/IDate columns before qeDT: %s",
                        sum(date_cols), paste(names(adf_for_tree)[date_cols], collapse = ", ")))
        adf_for_tree <- adf_for_tree[, !date_cols, drop = FALSE]
      }

      message(sprintf("Data for tree: %d rows, %d cols", nrow(adf_for_tree), ncol(adf_for_tree)))

      # Remove rows with missing response variable (required for decision trees)
      # IMPORTANT: Use sanitized column name (response_var_for_tree) not original name
      complete_response <- !is.na(adf_for_tree[[response_var_for_tree]])
      if (sum(!complete_response) > 0) {
        message(sprintf("Removing %d rows with NA in response variable '%s'",
                        sum(!complete_response), response_var_for_tree))
        showNotification(
          sprintf("Removed %d rows with missing %s values",
                  sum(!complete_response), original_response_var),
          type = "warning",
          duration = 5
        )
        adf_for_tree <- adf_for_tree[complete_response, ]
        source_row_ids <- source_row_ids[complete_response]
        message(sprintf("Data after NA removal: %d rows, %d cols",
                        nrow(adf_for_tree), ncol(adf_for_tree)))
      }

      # Check if we have enough data left
      if (nrow(adf_for_tree) < 10) {
        showNotification(
          sprintf("Not enough data to build tree. Only %d rows with valid %s values.",
                  nrow(adf_for_tree), input$response_var),
          type = "error",
          duration = 10
        )
        return()
      }

      # Anchor model-training rows to source row IDs.
      # qeDT/ctree may internally drop rows; rownames allow reliable back-mapping.
      rownames(adf_for_tree) <- as.character(source_row_ids)

      incProgress(0.6, detail = "Training model...")
      rv$model_source_row_ids <- NULL

      # Build decision tree(s)
      if (isTRUE(input$compare_models)) {
        # === MODEL COMPARISON MODE ===
        # Build TWO models to show the value of complete data

        # Identify Condition/DesignStyle columns
        condition_cols <- c("Condition", "DesignStyle", "Design Style", "Design_Style")
        has_condition_cols <- condition_cols[condition_cols %in% names(adf_for_tree)]

        if (length(has_condition_cols) > 0) {
          # MODEL A: Base Model (exclude Condition/DesignStyle, use ALL rows)
          # Note: adf_for_tree already has user exclusions and date columns removed
          base_data <- adf_for_tree[, !names(adf_for_tree) %in% has_condition_cols, drop = FALSE]
          base_source_row_ids <- source_row_ids
          rownames(base_data) <- as.character(base_source_row_ids)

          # Remove any remaining Date columns
          base_data <- base_data[, !sapply(base_data, inherits, "Date"), drop = FALSE]

          rv$dt_model_base <- tryCatch({
            qeDT(base_data, response_var_for_tree, holdout = NULL)
          }, error = function(e) {
            showNotification(paste("Error building base model:", e$message), type = "error")
            NULL
          })

          # MODEL B: Enhanced Model (include Condition/DesignStyle, only COMPLETE rows)
          # Find rows with non-NA Condition/DesignStyle in the filtered data
          complete_rows <- complete.cases(adf_for_tree[, has_condition_cols, drop = FALSE])

          if (sum(complete_rows) > 5) {  # Need at least 5 rows
            enhanced_data <- adf_for_tree[complete_rows, , drop = FALSE]
            enhanced_source_row_ids <- source_row_ids[complete_rows]
            rownames(enhanced_data) <- as.character(enhanced_source_row_ids)

            # Remove any remaining Date columns
            enhanced_data <- enhanced_data[, !sapply(enhanced_data, inherits, "Date"), drop = FALSE]

            rv$dt_model_enhanced <- tryCatch({
              qeDT(enhanced_data, response_var_for_tree, holdout = NULL)
            }, error = function(e) {
              showNotification(paste("Error building enhanced model:", e$message), type = "error")
              NULL
            })

            # Store comparison metrics
            rv$model_comparison <- list(
              base_rows = nrow(base_data),
              enhanced_rows = sum(complete_rows),
              total_rows = nrow(adf_for_tree),
              condition_vars = has_condition_cols
            )

            # Set primary model to enhanced for CMS extraction
            rv$dt_model <- rv$dt_model_enhanced
            rv$model_source_row_ids <- enhanced_source_row_ids

            showNotification(
              sprintf("Models built! Base: %d rows, Enhanced: %d rows (with complete data)",
                     nrow(base_data), sum(complete_rows)),
              type = "message",
              duration = 10
            )
          } else {
            showNotification(
              sprintf("Not enough complete rows (%d) for enhanced model. Building single model.",
                     sum(complete_rows)),
              type = "warning"
            )
            rv$dt_model <- rv$dt_model_base
            rv$model_source_row_ids <- base_source_row_ids
          }
        } else {
          showNotification("No Condition/DesignStyle columns found. Building single model.", type = "warning")
          rv$dt_model <- tryCatch({
            qeDT(adf_for_tree, response_var_for_tree, holdout = NULL)
          }, error = function(e) {
            showNotification(paste("Error building tree:", e$message), type = "error")
            NULL
          })
          if (!is.null(rv$dt_model)) {
            rv$model_source_row_ids <- source_row_ids
          }
        }
      } else {
        # === STANDARD MODE ===
        # Build single decision tree
        message("=== BUILDING STANDARD MODE TREE ===")
        message(sprintf("Calling qeDT with response='%s', data dims: %d x %d",
                        response_var_for_tree, nrow(adf_for_tree), ncol(adf_for_tree)))
        rv$dt_model <- tryCatch({
          message("=== CALLING qeDT() ===")
          qeDT(adf_for_tree, response_var_for_tree, holdout = NULL)
        }, error = function(e) {
          message(sprintf("=== ERROR IN qeDT(): %s ===", e$message))
          showNotification(paste("Error building tree:", e$message), type = "error")
          NULL
        })
        if (!is.null(rv$dt_model)) {
          rv$model_source_row_ids <- source_row_ids
        }

        # Optional debug: inspect qeDT result without affecting model assignment
        if (!is.null(rv$dt_model)) {
          message("=== qeDT() RETURNED SUCCESSFULLY ===")
          try({
            cat("\n=== qeDT Result Structure ===\n")
            cat("Class:", class(rv$dt_model), "\n")
            cat("Names:", paste(names(rv$dt_model), collapse = ", "), "\n")
            if ("model" %in% names(rv$dt_model)) {
              cat("Has $model component, class:", class(rv$dt_model$model), "\n")
            }
            cat("=============================\n\n")
          }, silent = TRUE)
        }
        message(sprintf("rv$dt_model is %s", if(is.null(rv$dt_model)) "NULL" else "NOT NULL"))
      }

      incProgress(1, detail = "Complete!")

      message(sprintf("=== FINAL CHECK: rv$dt_model is %s ===",
                      if(is.null(rv$dt_model)) "NULL" else "NOT NULL"))
      if (!is.null(rv$dt_model)) {
        message("=== TREE BUILD SUCCESSFUL - SHOWING NOTIFICATION ===")
        showNotification("Decision Tree built successfully!", type = "message")

        # Switch to "View Tree" tab
        tryCatch({
          nav_select(ns("build_tree_tabs"), selected = "3. View Tree")
        }, error = function(e) {
          # Silently fail if tab switching not supported
          # User can manually click tab
        })
      }
    })
  })

  # Conditional methodology info message
  output$methodology_info <- renderUI({
    if (isTRUE(input$include_temporal_vars)) {
      div(class = "alert alert-info", role = "alert",
        icon("info-circle"), strong(" Methodology:"),
        " Using ", strong("Charlie's approach"), " - temporal/market variables (SaleQtr, DOM, SPAP) are ",
        strong("included"), " to capture market trends within the decision tree."
      )
    } else {
      div(class = "alert alert-warning", role = "alert",
        icon("exclamation-triangle"), strong(" Methodology:"),
        " Temporal/market variables (SaleQtr, DOM, SPAP) are ",
        strong("excluded"), " to allow for separate price indexing later."
      )
    }
  })

  output$tree_results <- renderUI({
    req(rv$dt_model)

    comparison_ui <- if (!is.null(rv$dt_model_base) && !is.null(rv$dt_model_enhanced) && !is.null(rv$model_comparison)) {
      # MODEL COMPARISON VIEW
      tagList(
        div(class = "alert alert-info", role = "alert",
          icon("chart-line"), strong(" Model Comparison Mode Active"),
          sprintf(" - Base: %d rows | Enhanced: %d rows (with complete Condition/DesignStyle)",
                 rv$model_comparison$base_rows, rv$model_comparison$enhanced_rows)
        ),

        h4("Data Quality Impact Analysis"),
        div(class = "row mb-4",
          div(class = "col-md-4",
            div(class = "card text-white bg-primary",
              div(class = "card-body text-center",
                h5("Total Properties", class = "card-title"),
                h2(rv$model_comparison$total_rows, class = "mb-0")
              )
            )
          ),
          div(class = "col-md-4",
            div(class = "card text-white bg-warning",
              div(class = "card-body text-center",
                h5("Complete Data", class = "card-title"),
                h2(rv$model_comparison$enhanced_rows, class = "mb-0"),
                p(class = "mb-0 small",
                  sprintf("%.1f%%", 100 * rv$model_comparison$enhanced_rows / rv$model_comparison$total_rows))
              )
            )
          ),
          div(class = "col-md-4",
            div(class = "card text-white bg-danger",
              div(class = "card-body text-center",
                h5("Missing Data", class = "card-title"),
                h2(rv$model_comparison$total_rows - rv$model_comparison$enhanced_rows, class = "mb-0"),
                p(class = "mb-0 small",
                  sprintf("%.1f%%", 100 * (rv$model_comparison$total_rows - rv$model_comparison$enhanced_rows) / rv$model_comparison$total_rows))
              )
            )
          )
        ),

        div(class = "alert alert-success", role = "alert",
          icon("lightbulb"), strong(" Key Insight: "),
          "The Enhanced Model uses properties with complete ", strong(paste(rv$model_comparison$condition_vars, collapse = " & ")),
          " data. Compare the trees below to see how data quality affects model complexity and feature importance."
        ),

        hr(),

        # Side-by-side tree comparison
        h4("Tree Comparison: Base vs Enhanced"),
        div(class = "row",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-body",
                h5(sprintf("Base Model (%d properties)", rv$model_comparison$base_rows), class = "card-title text-primary"),
                p("Excludes Condition & DesignStyle variables", class = "text-muted small"),
                plotOutput(ns("tree_plot_base"), height = "400px")
              )
            )
          ),
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-body",
                h5(sprintf("Enhanced Model (%d properties)", rv$model_comparison$enhanced_rows), class = "card-title text-success"),
                p("Includes Condition & DesignStyle variables", class = "text-muted small"),
                plotOutput(ns("tree_plot_enhanced"), height = "400px")
              )
            )
          )
        ),

        hr()
      )
    } else {
      # STANDARD VIEW
      tagList(
        div(class = "alert alert-success", role = "alert",
          icon("check"), strong(" Decision Tree built successfully!")
        ),

        h4("Tree Structure"),
        plotOutput(ns("tree_plot"), height = "600px"),

        hr()
      )
    }

    tagList(
      comparison_ui,

      h4("Model Summary"),
      verbatimTextOutput(ns("tree_summary")),

      hr(),

      div(class = "card bg-light",
        div(class = "card-body",
          h5("Reading the Tree", class = "card-title"),
          p("The tree plot above shows you which property characteristics matter most in your market:"),
          tags$ul(
            tags$li(strong("First split"), "= Most important feature (highest variance reduction)"),
            tags$li(strong("Multiple splits on same variable"), "= That feature is critical"),
            tags$li(strong("Terminal nodes (bottom boxes)"), "= Potential CMS groups")
          ),
          p(class = "mb-0",
            "By examining which variables the algorithm chose for splitting, you can see",
            strong("objectively"), "what drives price in your market. This is transparent and defensible.")
        )
      )
    )
  })

  output$tree_plot <- renderPlot({
    req(rv$dt_model)
    plot(rv$dt_model)
  })

  # Comparison model plots
  output$tree_plot_base <- renderPlot({
    req(rv$dt_model_base)
    plot(rv$dt_model_base)
  })

  output$tree_plot_enhanced <- renderPlot({
    req(rv$dt_model_enhanced)
    plot(rv$dt_model_enhanced)
  })

  output$tree_summary <- renderPrint({
    req(rv$dt_model)
    print(rv$dt_model)
  })

  # ==========================================================================
  # Tab 4: Extract CMS
  # ==========================================================================

  # Function to collect subject property from inputs
  get_subject_data <- reactive({
    req(rv$adf_transformed, rv$dt_model)

    coerce_subject_value <- function(col_name, value) {
      if (is.null(value) || (length(value) == 1 && is.na(value))) {
        return(NULL)
      }
      if (!col_name %in% names(rv$adf_transformed)) {
        return(value)
      }

      adf_col <- rv$adf_transformed[[col_name]]
      if (is.factor(adf_col)) {
        return(factor(as.character(value), levels = levels(adf_col)))
      }
      if (is.character(adf_col)) {
        return(as.character(value))
      }
      if (is.integer(adf_col)) {
        return(suppressWarnings(as.integer(value)))
      }
      if (is.numeric(adf_col)) {
        return(suppressWarnings(as.numeric(value)))
      }

      value
    }

    # Build subject data frame.
    # If quick-pick selected a source row, use that exact transformed row as authoritative.
    subject_df <- data.frame(row.names = 1)
    selected_row_active <- !is.null(rv$selected_row_id) &&
      isTRUE(rv$selected_row_id >= 1) &&
      isTRUE(rv$selected_row_id <= nrow(rv$adf_transformed))

    if (selected_row_active) {
      source_row <- rv$adf_transformed[rv$selected_row_id, , drop = FALSE]
      for (col_name in names(source_row)) {
        val <- source_row[[col_name]][1]
        if (is.null(val) || (is.character(val) && val == "") || is.na(val)) {
          next
        }
        coerced <- coerce_subject_value(col_name, val)
        if (!is.null(coerced) && !(length(coerced) == 1 && is.na(coerced))) {
          subject_df[[col_name]] <- coerced
        }
      }
    } else if (!is.null(rv$selected_property) && nrow(rv$selected_property) > 0) {
      for (col_name in names(rv$selected_property)) {
        val <- rv$selected_property[[col_name]][1]
        if (is.null(val) || (is.character(val) && val == "") || is.na(val)) {
          next
        }
        coerced <- coerce_subject_value(col_name, val)
        if (!is.null(coerced) && !(length(coerced) == 1 && is.na(coerced))) {
          subject_df[[col_name]] <- coerced
        }
      }
    }

    # For quick-picked records, keep the selected data row authoritative.
    # For manual/profile subjects, allow subj_* inputs to populate missing columns.
    if (!selected_row_active) {
      subject_inputs <- reactiveValuesToList(input)
      subject_inputs <- subject_inputs[grepl("^subj_", names(subject_inputs))]
      for (input_name in names(subject_inputs)) {
        col_name <- gsub("^subj_", "", input_name)
        value <- subject_inputs[[input_name]]
        if (is.null(value) || (is.character(value) && value == "")) {
          next
        }
        if (col_name %in% names(subject_df)) {
          next
        }
        coerced <- coerce_subject_value(col_name, value)
        if (!is.null(coerced) && !(length(coerced) == 1 && is.na(coerced))) {
          subject_df[[col_name]] <- coerced
        }
      }
    }

    if (ncol(subject_df) == 0) {
      return(NULL)
    }

    # Backfill missing predictors from selected property row.
    # Quick-pick uses real row values, but not every model column has a visible subject input
    # (e.g., high-cardinality categorical fields). Fill those gaps here so node assignment is
    # based on the same record values used in training.
    if (!is.null(rv$selected_property) && nrow(rv$selected_property) > 0) {
      train_cols <- tryCatch({
        names(rv$dt_model$ctout@data@get("input"))
      }, error = function(e) character(0))
      fill_cols <- if (length(train_cols) > 0) train_cols else names(rv$selected_property)

      for (col in fill_cols) {
        if (col %in% names(rv$selected_property) && !col %in% names(subject_df)) {
          coerced <- coerce_subject_value(col, rv$selected_property[[col]][1])
          if (!is.null(coerced) && !(length(coerced) == 1 && is.na(coerced))) {
            subject_df[[col]] <- coerced
          }
        }
      }
    }

    subject_df
  })

  # Extract CMS when subject is defined
  observe({
    req(rv$dt_model, rv$adf_transformed)

    subject_df <- get_subject_data()

    if (!is.null(subject_df) && nrow(subject_df) > 0) {
      tryCatch({
        # Get node assignments for all ADF records
        # qeDT stores the actual ctree model in $ctout
        if ("party" %in% loadedNamespaces()) {
          # Get training data node assignments from ctree model
          adf_nodes <- party::where(rv$dt_model$ctout)

          # Prepare subject data to match training data format
          # Need to align columns AND factor levels with training data
          train_data <- rv$dt_model$ctout@data@get("input")
          train_data_cols <- names(train_data)

          # Filter subject to only include columns used in training
          subject_for_pred <- subject_df[, names(subject_df) %in% train_data_cols, drop = FALSE]

          # Ensure we always have a single-row data frame to add missing cols
          if (ncol(subject_for_pred) == 0) {
            subject_for_pred <- data.frame(row.names = 1)
          }

          # Add any training columns the subject didn't supply (fill with NA of correct type)
          missing_cols <- setdiff(train_data_cols, names(subject_for_pred))
          for (col in missing_cols) {
            train_col <- train_data[[col]]
            if (is.factor(train_col)) {
              subject_for_pred[[col]] <- factor(NA, levels = levels(train_col))
            } else if (is.integer(train_col)) {
              subject_for_pred[[col]] <- as.integer(NA)
            } else {
              subject_for_pred[[col]] <- as.numeric(NA)
            }
          }

          if (ncol(subject_for_pred) > 0) {
            # Debug: print structure comparison
            cat("\n=== Data Structure Comparison ===\n")
            cat("Training data columns:", paste(names(train_data), collapse=", "), "\n")
            cat("Subject data columns:", paste(names(subject_for_pred), collapse=", "), "\n")
            cat("\nTraining data classes:\n")
            print(sapply(train_data, class))
            cat("\nSubject data classes:\n")
            print(sapply(subject_for_pred, class))
            cat("================================\n\n")

            # STEP 1: Convert data types to match training data
            cat("Converting subject data types to match training data...\n")
            for (col in names(subject_for_pred)) {
              if (col %in% names(train_data)) {
                # Convert character to factor if training data is factor
                if (is.factor(train_data[[col]]) && is.character(subject_for_pred[[col]])) {
                  subject_for_pred[[col]] <- as.factor(subject_for_pred[[col]])
                }
                # Convert integer to numeric if training data is numeric
                if (is.numeric(train_data[[col]]) && is.integer(subject_for_pred[[col]])) {
                  subject_for_pred[[col]] <- as.numeric(subject_for_pred[[col]])
                }
              }
            }

            # STEP 2: Align factor levels with training data
            cat("Aligning factor levels...\n")
            for (col in names(subject_for_pred)) {
              if (col %in% names(train_data) && is.factor(train_data[[col]])) {
                # Ensure subject factor has same levels as training data
                subject_for_pred[[col]] <- factor(subject_for_pred[[col]],
                                                  levels = levels(train_data[[col]]))
              }
            }

            # STEP 3: Align numeric/integer storage modes
            cat("Aligning numeric storage modes...\n")
            for (col in names(subject_for_pred)) {
              if (col %in% names(train_data) && !is.factor(train_data[[col]])) {
                storage.mode(subject_for_pred[[col]]) <- storage.mode(train_data[[col]])
              }
            }

            # CRITICAL FIX: Reorder subject columns to match training data exactly
            # The party::predict function requires exact column order matching
            subject_for_pred <- subject_for_pred[, names(train_data), drop = FALSE]

            cat("After reordering - Subject columns:", paste(names(subject_for_pred), collapse=", "), "\n\n")

            # Predict which node the subject belongs to
            subject_node <- predict(rv$dt_model$ctout, newdata = subject_for_pred, type = "node")

            # Extract CMS: all sales in the same terminal node
            cms_indices <- which(adf_nodes == subject_node)

            if (length(cms_indices) > 0) {
              prev_node <- isolate(rv$subject_node)
              prev_cms <- isolate(rv$cms)
              prev_count <- if (!is.null(prev_cms)) nrow(prev_cms) else NA_integer_
              cms_changed <- is.null(prev_node) ||
                prev_node != subject_node ||
                is.na(prev_count) ||
                prev_count != length(cms_indices)

              # Map model-row indices back to the transformed source data used for subject selection.
              source_idx <- integer(0)

              # Preferred: use training rownames (explicit source row IDs) from the fitted model.
              train_row_ids <- suppressWarnings(as.integer(rownames(train_data)))
              if (length(train_row_ids) >= max(cms_indices) && all(!is.na(train_row_ids[cms_indices]))) {
                source_idx <- train_row_ids[cms_indices]
              } else if (!is.null(rv$model_source_row_ids) &&
                         length(rv$model_source_row_ids) >= max(cms_indices)) {
                # Fallback: positional mapping captured at build time.
                source_idx <- rv$model_source_row_ids[cms_indices]
              } else {
                # Last-resort legacy behavior.
                source_idx <- cms_indices
              }

              source_idx <- source_idx[!is.na(source_idx) &
                                         source_idx >= 1 &
                                         source_idx <= nrow(rv$adf_transformed)]
              rv$cms <- rv$adf_transformed[source_idx, , drop = FALSE]
              rv$subject <- subject_df
              rv$subject_node <- subject_node

              if (cms_changed) {
                showNotification(
                  paste("CMS extracted:", length(cms_indices), "comparable sales found!"),
                  type = "message"
                )
              }
            } else {
              showNotification("No comps found in subject's node", type = "warning")
            }
          }
        }
      }, error = function(e) {
        showNotification(paste("Error extracting CMS:", e$message), type = "error")
      })
    }
  })

  output$subject_path <- renderUI({
    req(rv$subject_node, rv$cms)

    tagList(
      p(strong("Subject assigned to Node:", rv$subject_node)),
      p(sprintf("This node contains %d sales from the ADF", nrow(rv$cms))),
      div(class = "alert alert-success", role = "alert",
        icon("check-circle"), strong(" CMS Extracted!"),
        sprintf(" Found %d comparable sales in subject's market segment.", nrow(rv$cms))
      )
    )
  })

  output$cms_summary <- renderUI({
    req(rv$cms, input$response_var)

    price_col <- input$response_var

    if (price_col %in% names(rv$cms)) {
      cms_prices <- rv$cms[[price_col]]

      # Convert to numeric if it's character/factor (handles "$1,000" format)
      if (is.character(cms_prices) || is.factor(cms_prices)) {
        cms_prices <- as.numeric(gsub("[,[:space:]]", "", as.character(cms_prices)))
      }

      tagList(
        h5("Price Statistics"),
        p(strong("Count:"), nrow(rv$cms), "sales"),
        p(strong("Min:"), scales::dollar(min(cms_prices, na.rm = TRUE))),
        p(strong("Median:"), scales::dollar(median(cms_prices, na.rm = TRUE))),
        p(strong("Max:"), scales::dollar(max(cms_prices, na.rm = TRUE))),
        p(strong("IQR:"),
          scales::dollar(quantile(cms_prices, 0.25, na.rm = TRUE)), "-",
          scales::dollar(quantile(cms_prices, 0.75, na.rm = TRUE)))
      )
    }
  })

  output$adf_cms_comparison <- renderPlotly({
    req(rv$adf, rv$cms, input$response_var, input$cms_bins)

    price_col <- input$response_var

    if (price_col %in% names(rv$adf) && price_col %in% names(rv$cms)) {
      # Extract price columns
      adf_prices <- rv$adf[[price_col]]
      cms_prices <- rv$cms[[price_col]]

      # Convert to numeric if character/factor (handles "$1,000" format)
      if (is.character(adf_prices) || is.factor(adf_prices)) {
        adf_prices <- as.numeric(gsub("[,[:space:]]", "", as.character(adf_prices)))
      }
      if (is.character(cms_prices) || is.factor(cms_prices)) {
        cms_prices <- as.numeric(gsub("[,[:space:]]", "", as.character(cms_prices)))
      }

      # Calculate bin parameters based on combined data range
      all_prices <- c(adf_prices, cms_prices)
      price_min <- min(all_prices, na.rm = TRUE)
      price_max <- max(all_prices, na.rm = TRUE)
      bin_size <- (price_max - price_min) / input$cms_bins

      # Create overlapping histograms with explicit bin configuration
      plot_ly(alpha = 0.6) %>%
        add_histogram(
          x = adf_prices,
          name = "ADF",
          autobinx = FALSE,
          xbins = list(
            start = price_min,
            end = price_max,
            size = bin_size
          ),
          marker = list(color = "steelblue")
        ) %>%
        add_histogram(
          x = cms_prices,
          name = "CMS",
          autobinx = FALSE,
          xbins = list(
            start = price_min,
            end = price_max,
            size = bin_size
          ),
          marker = list(color = "darkblue")
        ) %>%
        layout(
          title = "Price Distribution: ADF vs CMS",
          xaxis = list(title = "Price"),
          yaxis = list(title = "Count"),
          barmode = "overlay"
        )
    } else {
      plot_ly() %>% layout(title = "Define subject to see comparison")
    }
  })

  output$cms_data_table <- renderDT({
    req(rv$cms)
    
    # Use CMS with similarity scores if available
    cms_display <- cms_with_similarity()

    # Identify long text columns (remarks, comments, descriptions)
    text_col_patterns <- c("remark", "comment", "description", "notes")
    text_cols <- grep(paste(text_col_patterns, collapse = "|"),
                      names(cms_display), ignore.case = TRUE, value = FALSE)

    # Build columnDefs for text truncation with tooltip
    column_defs <- NULL
    if (length(text_cols) > 0) {
      column_defs <- list(
        list(
          targets = text_cols - 1,  # DT uses 0-based indexing
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display' && data != null && data.length > 50) {",
            "    return '<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>';",
            "  }",
            "  return data;",
            "}"
          )
        )
      )
    }

    datatable(
      cms_display,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = list(
          c(5, 10, 25, 50, 100),
          c("5", "10", "25", "50", "100")
        ),
        dom = 'Bfrt<"d-flex flex-wrap justify-content-end align-items-center gap-3 mt-2"<"d-flex align-items-center gap-2 small text-muted"li><"d-flex justify-content-end"p>>',
        columnDefs = column_defs,
        language = list(
          lengthMenu = "Number of rows to display _MENU_"
        )
      ),
      rownames = FALSE,
      escape = FALSE  # Allow HTML rendering for tooltips
    )
  })

  # --------------------------------------------------------------------------
  # Calculate Similarity Scores for CMS vs Subject
  # --------------------------------------------------------------------------
  calculate_similarity <- function(cms_data, subject, predictor_vars) {
    if (is.null(subject) || nrow(subject) == 0) return(rep(NA, nrow(cms_data)))
    
    # Use only variables that exist in both datasets
    common_vars <- intersect(predictor_vars, intersect(names(cms_data), names(subject)))
    if (length(common_vars) == 0) return(rep(NA, nrow(cms_data)))
    
    scores <- rep(0, nrow(cms_data))
    weight_sum <- 0
    
    for (var in common_vars) {
      subject_val <- subject[[var]][1]
      cms_vals <- cms_data[[var]]
      
      if (is.na(subject_val)) next
      
      # Try to treat as numeric - handle formatted numbers with commas
      cms_numeric <- cms_vals
      subject_numeric <- subject_val
      is_numeric_comparison <- FALSE
      
      if (is.numeric(cms_vals)) {
        is_numeric_comparison <- TRUE
      } else if (is.character(cms_vals) || is.factor(cms_vals)) {
        # Try to parse as number (strip commas, dollar signs, spaces)
        cms_cleaned <- gsub("[,[:space:]]", "", as.character(cms_vals))
        subject_cleaned <- gsub("[,[:space:]]", "", as.character(subject_val))
        cms_as_num <- suppressWarnings(as.numeric(cms_cleaned))
        subject_as_num <- suppressWarnings(as.numeric(subject_cleaned))
        
        # If >80% of non-NA values convert to numeric, treat as numeric
        valid_conversions <- sum(!is.na(cms_as_num)) / sum(!is.na(cms_vals))
        if (!is.na(valid_conversions) && valid_conversions > 0.8 && !is.na(subject_as_num)) {
          cms_numeric <- cms_as_num
          subject_numeric <- subject_as_num
          is_numeric_comparison <- TRUE
        }
      }
      
      if (is_numeric_comparison) {
        # Numeric: calculate normalized absolute difference
        valid_vals <- cms_numeric[!is.na(cms_numeric)]
        if (length(valid_vals) > 1) {
          range_val <- max(valid_vals) - min(valid_vals)
          if (range_val > 0) {
            diff_norm <- abs(cms_numeric - subject_numeric) / range_val
            diff_norm[is.na(diff_norm)] <- 1  # Max penalty for NA
            scores <- scores + diff_norm
            weight_sum <- weight_sum + 1
          }
        }
      } else {
        # Categorical: 0 if match, 1 if different
        match_score <- ifelse(as.character(cms_vals) == as.character(subject_val), 0, 1)
        match_score[is.na(match_score)] <- 1  # Max penalty for NA
        scores <- scores + match_score
        weight_sum <- weight_sum + 1
      }
    }
    
    if (weight_sum > 0) {
      scores <- scores / weight_sum  # Average distance (0 = identical, 1 = max different)
    }
    return(scores)
  }

  # Debug function: calculate similarity breakdown for first row (subject match)
  calculate_similarity_debug <- function(cms_data, subject, predictor_vars) {
    if (is.null(subject) || nrow(subject) == 0) return(NULL)
    
    common_vars <- intersect(predictor_vars, intersect(names(cms_data), names(subject)))
    if (length(common_vars) == 0) return(NULL)
    
    # Find the most similar row (should be subject if it's in the data)
    sim_scores <- calculate_similarity(cms_data, subject, predictor_vars)
    best_idx <- which.min(sim_scores)
    if (length(best_idx) == 0) best_idx <- 1
    
    debug_info <- list()
    
    for (var in common_vars) {
      subject_val <- subject[[var]][1]
      cms_val <- cms_data[[var]][best_idx]
      
      info <- list(
        variable = var,
        subject_value = as.character(subject_val),
        cms_value = as.character(cms_val),
        contribution = NA,
        status = "skipped"
      )
      
      if (is.na(subject_val)) {
        info$status <- "subject NA (skipped)"
        debug_info[[var]] <- info
        next
      }
      
      cms_col <- cms_data[[var]]
      
      # Try to treat as numeric - handle formatted numbers with commas
      is_numeric_comparison <- FALSE
      cms_numeric <- cms_col
      cms_val_numeric <- cms_val
      subject_val_numeric <- subject_val
      
      if (is.numeric(cms_col)) {
        is_numeric_comparison <- TRUE
      } else if (is.character(cms_col) || is.factor(cms_col)) {
        # Try to parse as number (strip commas, dollar signs, spaces)
        cms_cleaned <- gsub("[, ]", "", as.character(cms_col))
        cms_val_cleaned <- gsub("[, ]", "", as.character(cms_val))
        subject_cleaned <- gsub("[, ]", "", as.character(subject_val))
        cms_as_num <- suppressWarnings(as.numeric(cms_cleaned))
        cms_val_as_num <- suppressWarnings(as.numeric(cms_val_cleaned))
        subject_as_num <- suppressWarnings(as.numeric(subject_cleaned))
        
        valid_conversions <- sum(!is.na(cms_as_num)) / sum(!is.na(cms_col))
        if (!is.na(valid_conversions) && valid_conversions > 0.8 && !is.na(subject_as_num)) {
          cms_numeric <- cms_as_num
          cms_val_numeric <- cms_val_as_num
          subject_val_numeric <- subject_as_num
          is_numeric_comparison <- TRUE
        }
      }
      
      if (is_numeric_comparison) {
        valid_vals <- cms_numeric[!is.na(cms_numeric)]
        if (length(valid_vals) > 1) {
          range_val <- max(valid_vals) - min(valid_vals)
          if (range_val > 0) {
            diff_norm <- abs(cms_val_numeric - subject_val_numeric) / range_val
            info$contribution <- round(diff_norm * 100, 2)
            info$status <- if (diff_norm == 0) "exact match" else sprintf("diff: %.2f (range: %.0f-%.0f)", cms_val_numeric - subject_val_numeric, min(valid_vals), max(valid_vals))
          } else {
            # All values in CMS are identical - no variation
            is_match <- cms_val_numeric == subject_val_numeric
            info$contribution <- if (is_match) 0 else 100
            info$status <- if (is_match) "exact match (no variation)" else "mismatch (no variation)"
          }
        } else if (length(valid_vals) == 1) {
          is_match <- cms_val_numeric == subject_val_numeric
          info$contribution <- if (is_match) 0 else 100
          info$status <- if (is_match) "exact match (single)" else "mismatch (single)"
        }
      } else {
        match <- as.character(cms_val) == as.character(subject_val)
        info$contribution <- if (match) 0 else 100
        info$status <- if (match) "exact match" else "mismatch"
      }
      
      debug_info[[var]] <- info
    }
    
    # Also check for variables in predictor_vars but missing from subject
    missing_from_subject <- setdiff(predictor_vars, names(subject))
    for (var in missing_from_subject) {
      if (var %in% names(cms_data)) {
        debug_info[[var]] <- list(
          variable = var,
          subject_value = "(not in subject)",
          cms_value = as.character(cms_data[[var]][best_idx]),
          contribution = NA,
          status = "not compared (missing from subject)"
        )
      }
    }
    
    debug_info
  }

  # Reactive for CMS with similarity scores
  cms_with_similarity <- reactive({
    req(rv$cms)
    
    cms_data <- rv$cms
    
    # Calculate similarity if subject is defined
    if (!is.null(rv$subject) && !is.null(rv$selected_vars)) {
      sim_scores <- calculate_similarity(cms_data, rv$subject, rv$selected_vars)
      cms_data$Similarity <- round(sim_scores * 100, 1)  # Convert to 0-100 scale (0 = most similar)
      
      # Sort by similarity if checkbox is checked
      if (isTRUE(input$sort_by_similarity)) {
        cms_data <- cms_data[order(cms_data$Similarity), ]
      }
    }
    
    cms_data
  })

  # Similarity debug breakdown output
  output$similarity_debug <- renderUI({
    req(rv$cms, rv$subject, rv$selected_vars)
    
    debug_info <- calculate_similarity_debug(rv$cms, rv$subject, rv$selected_vars)
    if (is.null(debug_info) || length(debug_info) == 0) {
      return(div(class = "text-muted", "No debug info available"))
    }
    
    # Build table rows
    rows <- lapply(names(debug_info), function(var) {
      info <- debug_info[[var]]
      contrib_display <- if (is.na(info$contribution)) "-" else sprintf("%.1f", info$contribution)
      status_class <- if (info$status == "exact match") "text-success" else if (grepl("mismatch|diff:", info$status)) "text-warning" else "text-muted"
      
      tags$tr(
        tags$td(info$variable, style = "font-weight: 500;"),
        tags$td(info$subject_value, style = "font-family: monospace; font-size: 11px;"),
        tags$td(info$cms_value, style = "font-family: monospace; font-size: 11px;"),
        tags$td(contrib_display, style = "text-align: right;"),
        tags$td(class = status_class, info$status, style = "font-size: 11px;")
      )
    })
    
    # Calculate total
    contributions <- sapply(debug_info, function(x) if (!is.na(x$contribution)) x$contribution else 0)
    n_compared <- sum(!sapply(debug_info, function(x) is.na(x$contribution)))
    avg_score <- if (n_compared > 0) round(sum(contributions) / n_compared, 1) else NA
    
    div(
      style = "margin-top: 15px;",
      tags$details(
        tags$summary(
          style = "cursor: pointer; font-weight: bold; color: #666;",
          icon("bug"), " Similarity Debug Breakdown",
          if (!is.na(avg_score)) span(class = "badge bg-info ms-2", sprintf("Avg: %.1f", avg_score))
        ),
        div(
          style = "margin-top: 10px; max-height: 300px; overflow-y: auto;",
          tags$table(
            class = "table table-sm table-striped",
            style = "font-size: 12px;",
            tags$thead(
              tags$tr(
                tags$th("Variable"),
                tags$th("Subject Value"),
                tags$th("Best Match Value"),
                tags$th("Score", style = "text-align: right;"),
                tags$th("Status")
              )
            ),
            tags$tbody(rows)
          ),
          div(
            class = "text-muted small mt-2",
            sprintf("Compared %d variables. Score = average normalized difference (0 = identical, 100 = max different)", n_compared)
          ),
          # Data quality tip
          div(
            class = "alert alert-light mt-3",
            style = "font-size: 11px; padding: 10px;",
            tags$strong(icon("lightbulb"), " Data Quality Tip: "),
            "If you selected this subject from your ADF data (using 'Quick Pick'), the similarity score should be 0.0 (exact match). ",
            "If it's not, check the debug breakdown above for mismatches - this often indicates data formatting issues (e.g., commas in numbers, inconsistent text). ",
            "Variables showing 'no variation' are fine - they mean all properties in your CMS have identical values for that field."
          )
        )
      )
    )
  })

  # --------------------------------------------------------------------------
  # Copy MLS#s to Clipboard
  # --------------------------------------------------------------------------
  observeEvent(input$copy_mls_btn, {
    req(rv$cms)
    
    # Use sorted CMS data if similarity sorting is enabled
    cms_data <- cms_with_similarity()

    # Find MLS/ListingId column - try multiple patterns
    mls_patterns <- c("^mls$", "^mls ?#$", "^mls#$", "^mlsnumber$", "^mls ?number$",
                      "^listingid$", "^listing ?id$", "^listingkey$", "^listing ?key$")

    cms_cols <- names(rv$cms)
    mls_col <- NULL

    for (pat in mls_patterns) {
      # Normalize column names for matching (remove special chars except #)
      normalized <- gsub("[^a-zA-Z0-9#]", "", tolower(cms_cols))
      matches <- grep(pat, normalized, ignore.case = TRUE)
      if (length(matches) > 0) {
        mls_col <- cms_cols[matches[1]]
        break
      }
    }

    if (is.null(mls_col)) {
      showNotification("No MLS#/ListingId column found in CMS data", type = "warning")
      return()
    }

    # Get MLS values and format with selected separator (from sorted data)
    mls_values <- cms_data[[mls_col]]
    mls_values <- mls_values[!is.na(mls_values) & mls_values != ""]

    if (length(mls_values) == 0) {
      showNotification("No MLS# values found", type = "warning")
      return()
    }

    separator <- input$mls_separator
    mls_text <- paste(mls_values, collapse = separator)

    # Send to clipboard via JavaScript
    session$sendCustomMessage("copyToClipboard", mls_text)
  })

  # Handle clipboard success/error
  observeEvent(input$clipboard_success, {
    req(rv$cms)
    count <- nrow(rv$cms)
    showNotification(
      sprintf("Copied %d MLS#s to clipboard!", count),
      type = "message",
      duration = 3
    )
  })

  observeEvent(input$clipboard_error, {
    showNotification("Failed to copy to clipboard", type = "error")
  })

  # ==========================================================================
  # Tab 5: Results & Downloads
  # ==========================================================================

  output$key_metrics <- renderUI({
    req(rv$adf, rv$cms, input$response_var)

    price_col <- input$response_var
    adf_count <- nrow(rv$adf)
    cms_count <- nrow(rv$cms)
    reduction_pct <- round((1 - cms_count / adf_count) * 100, 1)

    # Check if subject is in IQR
    subject_in_iqr <- "Unknown"
    if (!is.null(rv$subject) && price_col %in% names(rv$subject)) {
      subject_price <- rv$subject[[price_col]]
      if (!is.null(subject_price) && !is.na(subject_price) && price_col %in% names(rv$cms)) {
        cms_prices <- rv$cms[[price_col]]
        q1 <- quantile(cms_prices, 0.25, na.rm = TRUE)
        q3 <- quantile(cms_prices, 0.75, na.rm = TRUE)
        subject_in_iqr <- if (subject_price >= q1 && subject_price <= q3) {
          "Yes"
        } else {
          "No"
        }
      }
    }

    tagList(
      div(class = "row",
        div(class = "col-md-6 mb-3",
          div(class = "card text-white bg-primary",
            div(class = "card-body",
              h5("ADF Sales", class = "card-title"),
              h2(adf_count, class = "mb-0")
            )
          )
        ),
        div(class = "col-md-6 mb-3",
          div(class = "card text-white bg-success",
            div(class = "card-body",
              h5("CMS Comps", class = "card-title"),
              h2(cms_count, class = "mb-0")
            )
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-6 mb-3",
          div(class = "card text-white bg-info",
            div(class = "card-body",
              h5("Reduction", class = "card-title"),
              h2(paste0(reduction_pct, "%"), class = "mb-0")
            )
          )
        ),
        div(class = "col-md-6 mb-3",
          div(class = if (subject_in_iqr == "Yes") {
            "card text-white bg-success"
          } else if (subject_in_iqr == "No") {
            "card text-white bg-warning"
          } else {
            "card text-white bg-secondary"
          },
            div(class = "card-body",
              h5("Subject in IQR?", class = "card-title"),
              h2(subject_in_iqr, class = "mb-0")
            )
          )
        )
      )
    )
  })

  output$uspap_language <- renderUI({
    req(rv$adf, rv$cms, rv$dt_model)

    adf_count <- nrow(rv$adf)
    cms_count <- nrow(rv$cms)

    # Extract feature importance from the tree
    # Get the variables used in splits (qeDT stores ctree in $ctout)
    tree_model <- rv$dt_model$ctout

    # Extract split variables from the tree structure
    split_vars <- tryCatch({
      # For party/ctree objects
      if ("BinaryTree" %in% class(tree_model)) {
        nodes <- party::nodes(tree_model, unique(party::where(tree_model)))
        split_vars_list <- lapply(nodes, function(node) {
          if (!node$terminal) {
            node$psplit$variableName
          }
        })
        unique(unlist(split_vars_list))
      } else {
        c("key property characteristics")
      }
    }, error = function(e) {
      c("key property characteristics")
    })

    # Format feature list
    if (length(split_vars) > 0 && !all(split_vars == "key property characteristics")) {
      features_text <- paste(split_vars, collapse = ", ")
    } else {
      features_text <- "key property characteristics"
    }

    uspap_text <- sprintf(
      "Comparable sales selection was assisted by Decision Tree machine learning algorithm analyzing %d sales from the MLS export. The algorithm identified %d statistically similar properties based on %s. The Decision Tree methodology objectively partitioned the data into homogeneous market segments by identifying which property characteristics most strongly influence sale price in this market. I reviewed the algorithm's selections and validated them against my market knowledge and experience. Final comparable sales were chosen based on their similarity to the subject property and relevance to this assignment. This objective, transparent methodology supports the credibility of the analysis and provides a defensible, data-driven approach to comparable selection while maintaining appraiser judgment as the final arbiter of appropriateness.",
      adf_count,
      cms_count,
      features_text
    )

    tagList(
      div(class = "card mb-3",
        div(class = "card-body",
          p(uspap_text, id = "uspap_text", style = "line-height: 1.8;"),
          hr(),
          p(class = "text-muted small mb-0",
            strong("Note:"), " This text is a template. Review and modify as needed to fit your specific appraisal report requirements."
          )
        )
      )
    )
  })

  # Copy to clipboard handler
  observeEvent(input$copy_uspap, {
    req(rv$adf, rv$cms, rv$dt_model)

    # Show notification that text is ready to copy
    showNotification(
      "USPAP/CUSPAP text is ready! Use Ctrl+C (or Cmd+C) to copy the text above.",
      type = "message",
      duration = 5
    )
  })

  # Download CMS as Excel workbook
  output$download_cms_excel <- downloadHandler(
    filename = function() {
      paste0("CMS_Export_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$cms, rv$adf, rv$subject)

      # Get CMS with similarity scores included
      cms_export <- cms_with_similarity()

      # Create workbook with multiple sheets
      wb_data <- list(
        "CMS" = cms_export,
        "ADF" = rv$adf,
        "Subject" = rv$subject
      )

      # Add summary sheet
      adf_count <- nrow(rv$adf)
      cms_count <- nrow(rv$cms)
      reduction_pct <- round((1 - cms_count / adf_count) * 100, 1)

      summary_df <- data.frame(
        Metric = c("ADF Sales Count", "CMS Comps Count", "Reduction %", "Subject Node", "Analysis Date"),
        Value = c(adf_count, cms_count, paste0(reduction_pct, "%"),
                 as.character(rv$subject_node), as.character(Sys.Date()))
      )

      wb_data <- c(list("Summary" = summary_df), wb_data)

      # Write to Excel
      writexl::write_xlsx(wb_data, path = file)
    }
  )

  # Download tree plot as PNG
  output$download_tree_plot <- downloadHandler(
    filename = function() {
      paste0("DecisionTree_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      req(rv$dt_model)

      # Save plot as PNG
      png(file, width = 1200, height = 800, res = 120)
      plot(rv$dt_model)
      dev.off()
    }
  )

  # Download full HTML report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("CMS_Report_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      req(rv$adf, rv$cms, rv$dt_model, input$response_var)

      price_col <- input$response_var
      adf_count <- nrow(rv$adf)
      cms_count <- nrow(rv$cms)
      reduction_pct <- round((1 - cms_count / adf_count) * 100, 1)

      # Create HTML report
      html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>Decision Tree CMS Report</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
    h1 { color: #3498db; }
    h2 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
    .metric { background: #ecf0f1; padding: 15px; margin: 10px 0; border-radius: 5px; }
    .metric strong { color: #2980b9; }
    table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
    th { background: #3498db; color: white; padding: 12px; text-align: left; }
    td { padding: 10px; border-bottom: 1px solid #ddd; }
    tr:hover { background: #f5f5f5; }
    .footer { margin-top: 50px; padding-top: 20px; border-top: 2px solid #ddd; color: #7f8c8d; }
  </style>
</head>
<body>
  <h1>Decision Tree CMS Analysis Report</h1>
  <p><strong>Date:</strong> %s</p>

  <h2>Executive Summary</h2>
  <div class="metric"><strong>ADF Sales Analyzed:</strong> %d</div>
  <div class="metric"><strong>CMS Comps Selected:</strong> %d</div>
  <div class="metric"><strong>Reduction:</strong> %s%%</div>
  <div class="metric"><strong>Subject Node:</strong> %s</div>

  <h2>Methodology</h2>
  <p>This analysis used Decision Tree machine learning to objectively identify comparable sales from the Assignment Data Frame (ADF). The algorithm partitioned properties into homogeneous market segments based on characteristics that drive sale prices in this market.</p>

  <h2>CMS Sales Summary</h2>
  <p>The following %d properties were identified as the subject\'s Competitive Market Segment:</p>

  <table>
    <tr>
      <th>Statistic</th>
      <th>Value</th>
    </tr>
    <tr><td>Count</td><td>%d</td></tr>
    <tr><td>Minimum Price</td><td>$%s</td></tr>
    <tr><td>Median Price</td><td>$%s</td></tr>
    <tr><td>Maximum Price</td><td>$%s</td></tr>
    <tr><td>Mean Price</td><td>$%s</td></tr>
  </table>

  <h2>Appraiser Notes</h2>
  <p><em>The Decision Tree algorithm provides objective, data-driven suggestions for comparable sales selection. Final selection of comparables remains the responsibility of the appraiser based on assignment requirements and professional judgment.</em></p>

  <div class="footer">
    <p>Report generated by ADF-to-CMS Mapper | Based on Charlie Abromaitis\'s Decision Tree methodology</p>
  </div>
</body>
</html>
      ',
        format(Sys.Date(), "%B %d, %Y"),
        adf_count,
        cms_count,
        reduction_pct,
        as.character(rv$subject_node),
        cms_count,
        cms_count,
        format(min(rv$cms[[price_col]], na.rm = TRUE), big.mark = ",", scientific = FALSE),
        format(median(rv$cms[[price_col]], na.rm = TRUE), big.mark = ",", scientific = FALSE),
        format(max(rv$cms[[price_col]], na.rm = TRUE), big.mark = ",", scientific = FALSE),
        format(mean(rv$cms[[price_col]], na.rm = TRUE), big.mark = ",", scientific = FALSE)
      )

      writeLines(html_content, file)
    }
  )

  # ==========================================================================
  # Stability Analysis Logic
  # ==========================================================================

  observeEvent(input$run_stability, {
    # Check requirements
    if (is.null(rv$adf_transformed) || is.null(rv$dt_model)) {
      showNotification("Please build the decision tree first.", type = "warning")
      return()
    }

    # Get subject data
    subj_data <- get_subject_data()
    if (is.null(subj_data)) {
      showNotification("Please define the subject property first.", type = "warning")
      return()
    }

    # Identify predictors used
    predictors <- if (!is.null(rv$selected_vars)) rv$selected_vars else names(rv$adf_transformed)
    predictors <- setdiff(predictors, input$response_var)

    # Run analysis
    result <- analyze_cms_stability(
      adf = rv$adf_transformed,
      subject_data = subj_data,
      response_var = input$response_var,
      predictors = predictors,
      iterations = 20
    )

    output$stability_results <- renderUI({
      if (!is.null(result$error)) {
        div(class = "alert alert-danger", result$error)
      } else {
        # Filter to records that appeared at least once
        stable_records <- result$results[result$results$StabilityScore > 0, ]
        avg_score <- mean(stable_records$StabilityScore)

        tagList(
          div(class = "alert alert-light border",
            h5("Stability Report"),
            p(sprintf("Analysis based on %d iterations (90%% subsampling).", result$iterations)),
            div(class = "d-flex justify-content-around",
              div(class = "text-center", h3(sprintf("%.0f%%", avg_score * 100)), p("Average Stability")),
              div(class = "text-center", h3(nrow(stable_records)), p("Total Unique Comps Found")),
              div(class = "text-center", h3(sum(stable_records$StabilityScore >= 0.95)), p("Highly Stable (>95%)"))
            )
          ),

          h5("CMS Composition Frequency"),
          p(class="text-muted small", "This table shows how often each property appears in the Subject's CMS across different tree runs."),

          renderDT({
            # Select key columns + Score
            key_cols <- c("StabilityScore", "Frequency", input$response_var)
            # Try to find common descriptive columns
            possible_desc_cols <- c("Condition", "SqFt", "GLA", "YearBuilt", "YrBlt", "Neighborhood", "Address", "Street")
            extra_cols <- intersect(names(stable_records), possible_desc_cols)
            
            df_display <- stable_records[, c(key_cols, extra_cols), drop=FALSE]

            datatable(df_display,
                      options = list(
                        pageLength = 10, 
                        order = list(list(0, 'desc')), # Sort by Stability Score desc
                        dom = 'tp'
                      ),
                      rownames = FALSE) %>%
              formatPercentage('StabilityScore', 0)
          })
        )
      }
    })
  })


}
