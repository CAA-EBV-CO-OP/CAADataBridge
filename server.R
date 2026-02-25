server <- function(input, output, session) {
  # DEBUG: Confirm new synonym lookup code is loaded
  message("======================================================")
  message("[STARTUP] Server loaded with multi-strategy synonym lookup (v2)")
  message("======================================================")

  # ============================================================================
  # APP CHOOSER STATE AND HANDLERS
  # ============================================================================

  # Track whether user has chosen an app (Column Mapper)
  app_state <- reactiveValues(
    chosen = FALSE  # TRUE when user clicks "Column Mapper"
  )

  # Output for conditionalPanel - controls app chooser visibility
  output$app_chosen <- reactive({
    app_state$chosen
  })
  outputOptions(output, "app_chosen", suspendWhenHidden = FALSE)

  # Greeting text for app chooser

  output$chooser_greeting <- renderText({
    if (current_user$local_mode) {
      "Welcome, Guest"
    } else if (current_user$authenticated && !is.null(current_user$username)) {
      paste("Welcome,", current_user$username)
    } else {
      "Welcome"
    }
  })

  # User chose Column Mapper - set app_chosen to TRUE
  observeEvent(input$choose_mapper, {
    app_state$chosen <- TRUE
  })

  # User chose to sign out from chooser screen
  observeEvent(input$chooser_logout, {
    # Reset authentication state
    current_user$authenticated <- FALSE
    current_user$user_id <- NULL
    current_user$username <- NULL
    current_user$email <- NULL
    current_user$local_mode <- FALSE
    # Reset app choice
    app_state$chosen <- FALSE
  })

  # User chose Decision Trees - launch it and redirect
  observeEvent(input$choose_dt, {
    # Show loading message with spinner (update the panel content)
    shinyjs::html("app_chooser_panel",
      "<div style='text-align: center; padding: 50px;'>
         <h3>Launching Decision Trees...</h3>
         <p>Please wait while the application starts.</p>
         <div style='margin-top: 20px;'>
           <div class='fa fa-spinner fa-spin fa-2x'></div>
         </div>
         <p id='dt_status' style='margin-top: 15px; color: #666;'>Starting R process...</p>
       </div>")

    # Launch Decision Trees app in background on port 4000
    dt_path <- "C:/GitRepos/CAA DEMOS/CA-ADF-CMS-DecisionTrees/ADF-CMS-Tool"

    # Use shell command for better Windows compatibility - setwd first then runApp
    cmd <- sprintf('start /B "" "C:\\Program Files\\R\\R-4.3.1\\bin\\Rscript.exe" -e "setwd(\'%s\'); shiny::runApp(\'.\', port=4000, launch.browser=FALSE)"', dt_path)
    shell(cmd, wait = FALSE)

    # Poll for the app to be ready, then redirect
    shinyjs::runjs("
      var checkCount = 0;
      var maxChecks = 30;  // 30 seconds max wait
      var checkInterval = setInterval(function() {
        checkCount++;
        document.getElementById('dt_status').innerText = 'Waiting for app to start... (' + checkCount + 's)';

        // Try to fetch the app
        fetch('http://localhost:4000', {mode: 'no-cors'})
          .then(function() {
            clearInterval(checkInterval);
            document.getElementById('dt_status').innerText = 'Ready! Redirecting...';
            setTimeout(function() {
              window.location.href = 'http://localhost:4000';
            }, 500);
          })
          .catch(function() {
            // Not ready yet, keep waiting
            if (checkCount >= maxChecks) {
              clearInterval(checkInterval);
              document.getElementById('dt_status').innerText = 'Timeout - please try refreshing or check if Decision Trees is installed correctly.';
            }
          });
      }, 1000);
    ")
  })

  # User chose Report Forms - launch Streamlit app and redirect
  observeEvent(input$choose_reports, {
    # Show loading message with spinner (update the panel content)
    shinyjs::html("app_chooser_panel",
      "<div style='text-align: center; padding: 50px;'>
         <h3>Launching Report Forms...</h3>
         <p>Please wait while the application starts.</p>
         <div style='margin-top: 20px;'>
           <div class='fa fa-spinner fa-spin fa-2x'></div>
         </div>
         <p id='reports_status' style='margin-top: 15px; color: #666;'>Starting Streamlit app...</p>
       </div>")

    # Launch ReportForms Streamlit app in background on port 8502
    reports_path <- "C:/GitRepos/ReportForms"

    # Use shell command to launch Streamlit
    cmd <- sprintf('start /B "" cmd /c "cd /d %s && python -m streamlit run app.py --server.port 8502 --server.headless true"', reports_path)
    shell(cmd, wait = FALSE)

    # Poll for the app to be ready, then redirect
    shinyjs::runjs("
      var checkCount = 0;
      var maxChecks = 30;  // 30 seconds max wait
      var checkInterval = setInterval(function() {
        checkCount++;
        document.getElementById('reports_status').innerText = 'Waiting for app to start... (' + checkCount + 's)';

        // Try to fetch the app
        fetch('http://localhost:8502', {mode: 'no-cors'})
          .then(function() {
            clearInterval(checkInterval);
            document.getElementById('reports_status').innerText = 'Ready! Redirecting...';
            setTimeout(function() {
              window.location.href = 'http://localhost:8502';
            }, 500);
          })
          .catch(function() {
            // Not ready yet, keep waiting
            if (checkCount >= maxChecks) {
              clearInterval(checkInterval);
              document.getElementById('reports_status').innerText = 'Timeout - please try refreshing or check if ReportForms is installed correctly.';
            }
          });
      }, 1000);
    ")
  })

  # ============================================================================
  # USER AUTHENTICATION STATE
  # ============================================================================

  # Current user session info
  current_user <- reactiveValues(
    authenticated = FALSE,
    user_id = NULL,
    username = NULL,
    email = NULL,
    local_mode = FALSE  # TRUE if user skipped login
  )

  # Output for conditionalPanel

  output$user_authenticated <- reactive({
    current_user$authenticated || current_user$local_mode
  })
  outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)

  # Display current username
  output$current_user_display <- renderText({
    if (current_user$local_mode) {
      "Local Mode (profiles not synced)"
    } else if (current_user$authenticated) {
      paste("Signed in as:", current_user$username)
    } else {
      ""
    }
  })

  # MongoDB status indicator on login screen
  output$db_status_indicator <- renderUI({
    if (is_mongodb_available()) {
      tags$span(icon("check-circle", style = "color: #5cb85c;"), " Connected to cloud storage")
    } else {
      tags$span(icon("exclamation-circle", style = "color: #f0ad4e;"), " Cloud storage unavailable - local mode only")
    }
  })

  # Toggle between login and registration forms
  observeEvent(input$show_register, {
    shinyjs::hide("login_form")
    shinyjs::show("register_form")
  })

  observeEvent(input$show_login, {
    shinyjs::show("login_form")
    shinyjs::hide("register_form")
  })

  # Handle login
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)

    result <- authenticate_user(input$login_username, input$login_password)

    if (result$success) {
      current_user$authenticated <- TRUE
      current_user$user_id <- result$user_id
      current_user$username <- result$username
      current_user$email <- result$email
      current_user$local_mode <- FALSE

      # Clear form
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")

      output$auth_message <- renderUI(NULL)
    } else {
      output$auth_message <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Handle registration
  observeEvent(input$register_btn, {
    req(input$register_username, input$register_email, input$register_password)

    # Validate passwords match
    if (input$register_password != input$register_password_confirm) {
      output$auth_message <- renderUI({
        tags$div(class = "alert alert-danger", "Passwords do not match")
      })
      return()
    }

    # Validate password length
    if (nchar(input$register_password) < 6) {
      output$auth_message <- renderUI({
        tags$div(class = "alert alert-danger", "Password must be at least 6 characters")
      })
      return()
    }

    result <- create_user(input$register_username, input$register_email, input$register_password)

    if (result$success) {
      output$auth_message <- renderUI({
        tags$div(class = "alert alert-success", "Account created! You can now sign in.")
      })

      # Switch to login form
      shinyjs::show("login_form")
      shinyjs::hide("register_form")

      # Pre-fill username
      updateTextInput(session, "login_username", value = input$register_username)

      # Clear registration form
      updateTextInput(session, "register_username", value = "")
      updateTextInput(session, "register_email", value = "")
      updateTextInput(session, "register_password", value = "")
      updateTextInput(session, "register_password_confirm", value = "")
    } else {
      output$auth_message <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Handle skip login (local mode)
  observeEvent(input$skip_login, {
    current_user$authenticated <- FALSE
    current_user$local_mode <- TRUE
    current_user$username <- "Guest"
  })

  # Handle logout
  observeEvent(input$logout_btn, {
    current_user$authenticated <- FALSE
    current_user$user_id <- NULL
    current_user$username <- NULL
    current_user$email <- NULL
    current_user$local_mode <- FALSE
    # Reset app choice so user sees chooser on next login
    app_state$chosen <- FALSE
  })

  # ============================================================================
  # MAIN APP STATE
  # ============================================================================

  # Destination schema state
  rv <- reactiveValues(
    dest_headers = character(),
    derived_metrics_last_run = character(),
    # Column analysis and classification results
    column_analysis = NULL,        # Data quality metrics per column
    column_classifications = NULL, # Auto-classification results
    classification_overrides = list(),  # User overrides: col_name -> class
    dismissed_flags = list()       # User-dismissed flags: col_name -> c("FLAG1", "FLAG2", ...)
  )

  # Store the current dataset (either uploaded or sample)
  current_dataset <- reactiveVal(NULL)

  # Reactive value to store loaded mapping profile
  loaded_mapping_profile <- reactiveVal(list())

  # Reactive value to store loaded mapping profile filename
  loaded_profile_filename <- reactiveVal(NULL)

  # Reactive value to store loaded CSV filename
  loaded_csv_filename <- reactiveVal(NULL)

  # Track which profile mappings successfully applied (column exists in dataset)
  # This is a named list: target_field -> TRUE/FALSE
  profile_mapping_success <- reactiveVal(list())

  # Track how each mapping was sourced during the session
  # Each entry: target_field -> list(source = "profile"/"manual"/"suggestion"/"exact", value = c("Col"))
  value_source_tracker <- reactiveVal(list())

  # Cache for suggestions to avoid recalculating on every UI interaction
  suggestion_cache <- reactiveVal(list())
  destination_suggestion_cache <- reactiveVal(list())

  # ID field conflict resolution state (RESO: ListingKey vs ListingId)
  id_analysis_result <- reactiveVal(NULL)
  id_selection_pending <- reactiveVal(FALSE)
  id_conflicts_resolved <- reactiveVal(list())

  # Helper: Find the standard field key for a destination column name
  # Uses multiple lookup strategies:
  # 1. Check destination_suggestion_cache (column was matched during schema load)
  # 2. Direct match against standard field names in logic_target_groups
  # 3. Check if column name matches any synonym in logic_target_groups
  # NOTE: Must be defined BEFORE get_dest_synonyms which calls it
  find_std_field_for_column <- function(dest_col) {
    # Strategy 1: Check cache (original lookup)
    cache <- destination_suggestion_cache()
    for (std_field in names(cache)) {
      info <- cache[[std_field]]
      if (is.list(info) && !is.null(info$column) && identical(info$column, dest_col)) {
        message(sprintf("[SYNONYM-LOOKUP] '%s' found in cache -> %s", dest_col, std_field))
        return(std_field)
      }
    }

    # Strategy 2: Direct match against standard field names (normalized)
    dest_col_norm <- tolower(gsub("[^[:alnum:]]", "", dest_col))
    for (gn in names(logic_target_groups)) {
      for (field_name in names(logic_target_groups[[gn]])) {
        field_norm <- tolower(gsub("[^[:alnum:]]", "", field_name))
        if (field_norm == dest_col_norm) {
          message(sprintf("[SYNONYM-LOOKUP] '%s' matched field name -> %s", dest_col, field_name))
          return(field_name)
        }
      }
    }

    # Strategy 3: Check synonyms in logic_target_groups
    # DEBUG: Check specifically for ListingId
    if (dest_col_norm == "mls") {
      message(sprintf("[SYNONYM-DEBUG] Looking for 'mls' in logic_target_groups..."))
      message(sprintf("[SYNONYM-DEBUG] Number of groups: %d", length(names(logic_target_groups))))
    }
    for (gn in names(logic_target_groups)) {
      for (field_name in names(logic_target_groups[[gn]])) {
        meta <- logic_target_groups[[gn]][[field_name]]
        if (!is.null(meta$synonyms)) {
          syns_norm <- tolower(gsub("[^[:alnum:]]", "", meta$synonyms))
          # DEBUG: Log when checking ListingId specifically
          if (dest_col_norm == "mls" && field_name == "ListingId") {
            message(sprintf("[SYNONYM-DEBUG] Checking ListingId in group '%s'", gn))
            message(sprintf("[SYNONYM-DEBUG] ListingId synonyms raw: %s", paste(head(meta$synonyms, 5), collapse=", ")))
            message(sprintf("[SYNONYM-DEBUG] ListingId synonyms normalized: %s", paste(head(syns_norm, 5), collapse=", ")))
            message(sprintf("[SYNONYM-DEBUG] Is 'mls' in syns_norm? %s", dest_col_norm %in% syns_norm))
          }
          if (dest_col_norm %in% syns_norm) {
            message(sprintf("[SYNONYM-LOOKUP] '%s' (norm: %s) matched synonym for -> %s", dest_col, dest_col_norm, field_name))
            return(field_name)
          }
        }
      }
    }

    # Strategy 4: Check ORIGINAL_SYNONYMS (built before user modifications)
    # This allows finding the field even if user has edited synonyms
    if (exists("ORIGINAL_SYNONYMS") && length(ORIGINAL_SYNONYMS) > 0) {
      if (dest_col_norm %in% names(ORIGINAL_SYNONYMS)) {
        field_name <- ORIGINAL_SYNONYMS[[dest_col_norm]]
        message(sprintf("[SYNONYM-LOOKUP] '%s' (norm: %s) found in ORIGINAL_SYNONYMS -> %s", dest_col, dest_col_norm, field_name))
        return(field_name)
      }
    }

    # Strategy 5: Prefix matching on normalized field names
    # Handles CValR abbreviated columns (e.g., "PriceOrig" -> "PriceOriginal")
    # where the dest column name is a prefix of the logic field name
    prefix_matches <- character(0)
    for (gn in names(logic_target_groups)) {
      for (field_name in names(logic_target_groups[[gn]])) {
        field_norm <- tolower(gsub("[^[:alnum:]]", "", field_name))
        if (nchar(dest_col_norm) >= 4 && startsWith(field_norm, dest_col_norm)) {
          prefix_matches <- c(prefix_matches, field_name)
        }
      }
    }
    if (length(prefix_matches) == 1) {
      message(sprintf("[SYNONYM-LOOKUP] '%s' (norm: %s) prefix-matched -> %s", dest_col, dest_col_norm, prefix_matches[1]))
      return(prefix_matches[1])
    } else if (length(prefix_matches) > 1) {
      # Multiple prefix matches - pick the shortest (closest match)
      best <- prefix_matches[which.min(nchar(prefix_matches))]
      message(sprintf("[SYNONYM-LOOKUP] '%s' (norm: %s) prefix-matched (best of %d) -> %s", dest_col, dest_col_norm, length(prefix_matches), best))
      return(best)
    }

    message(sprintf("[SYNONYM-LOOKUP] '%s' (norm: %s) - NO MATCH FOUND", dest_col, dest_col_norm))
    return(NULL)
  }

  # Helper: get synonym hints for a destination column (globally available)
  # Uses multi-strategy lookup via find_std_field_for_column
  get_dest_synonyms <- function(dest_col) {
    # Find standard field using multi-strategy lookup
    std_field <- find_std_field_for_column(dest_col)

    if (!is.null(std_field)) {
      # Get the standard field's synonyms from logic_target_groups
      for (gn in names(logic_target_groups)) {
        if (std_field %in% names(logic_target_groups[[gn]])) {
          meta <- logic_target_groups[[gn]][[std_field]]
          return(meta$synonyms %||% character())
        }
      }
    }
    return(character())
  }

  # Track excluded columns per field (field -> c(excluded_column_names))
  # Stores columns that user has marked as "not a match" for specific fields
  field_exclusions <- reactiveVal(list())
  dest_exclusion_handlers <- new.env(parent = emptyenv())

  # Track Class 3 review states for mapping cards
  # Key: target field name (destination or CValR standard field)
  # Value: "pending", "accepted", or "skipped"
  class3_review_states <- reactiveVal(list())

  # Store accepted Class 3 mappings (target -> source_col)

  # This is needed because updateSelectizeInput is async - the dropdown values
  # aren't immediately available in input[[ ]] when get_all_selections() runs
  class3_accepted_mappings <- reactiveVal(list())

  # Track which derived metrics user has opted to create during mapping
  # Key: metric name (e.g., "SaleQtr"), Value: TRUE if user wants to derive it
  opted_derived_metrics <- reactiveVal(list())

  # Track if user has accepted the decimal baths split (BA → full, PB → half)
  baths_split_accepted <- reactiveVal(FALSE)

  # Track if user has accepted auto-derivation of SqFtTotal from AboveGrade + BelowGrade
  sqft_derive_accepted <- reactiveVal(FALSE)

  set_value_source <- function(target, source, values) {
    tracker <- value_source_tracker()
    normalized <- normalize_selection_values(values)

    if (!length(normalized) || identical(source, "none") || is.null(source)) {
      clear_value_source(target)
      return(invisible(NULL))
    }

    new_entry <- list(source = source, value = normalized)
    if (is.null(tracker[[target]]) || !identical(tracker[[target]], new_entry)) {
      tracker[[target]] <- new_entry
      value_source_tracker(tracker)
    }
    invisible(NULL)
  }

  clear_value_source <- function(target) {
    tracker <- value_source_tracker()
    if (!is.null(tracker[[target]])) {
      tracker[[target]] <- NULL
      value_source_tracker(tracker)
    }
    invisible(NULL)
  }

  # Trigger to force UI re-render after profile loads
  ui_refresh_trigger <- reactiveVal(0)

  # Track if user explicitly loaded a profile (vs auto-suggestions on CSV load)
  profile_was_loaded <- reactiveVal(FALSE)

  # Track workflow wizard choice
  workflow_wizard_answered <- reactiveVal(FALSE)
  user_has_profile <- reactiveVal(NULL)  # TRUE = has profile, FALSE = no profile
  search_query_raw <- reactive({
    tolower(trimws(input$mapping_search %||% ""))
   })

  search_query_debounced <- debounce(search_query_raw, 400)

  # Show workflow wizard on app startup (only after user is authenticated)
  observe({
    # Only show once when app loads AND user is authenticated
    if (!workflow_wizard_answered() && (current_user$authenticated || current_user$local_mode)) {
      showModal(modalDialog(
        title = "Welcome to CAA Column Mapper!",
        tags$div(
          style = "text-align: center; padding: 20px;",
          tags$h4("Do you have a mapping profile previously created with CAA Column Mapper ready to upload?"),
          tags$p(style = "color: #666; margin-top: 15px;",
                "If you've mapped data before, you can load your saved profile to auto-apply mappings.")
        ),
        footer = tagList(
          actionButton("wizard_no", "No - I'll create one now",
                      class = "btn-default",
                      style = "margin-right: 10px;"),
          actionButton("wizard_yes", "Yes - Load my profile",
                      class = "btn-primary")
        ),
        easyClose = FALSE,
        size = "m"
      ))
      workflow_wizard_answered(TRUE)
    }
  })

  # Handle "Yes - Load profile" response
  observeEvent(input$wizard_yes, {
    user_has_profile(TRUE)
    removeModal()

    # Show guidance notification pointing to sidebar upload
    showNotification(
      HTML("<strong>Step 1: Load Mapping Profile</strong><br/>Use the 'Load Mapping Profile' section in the left sidebar to upload your profile.<br/><strong>Step 2: Upload CSV Data</strong><br/>Then upload your CSV data to apply the mappings automatically."),
      type = "message",
      duration = 12,
      closeButton = TRUE
    )
  })

  # Handle "No - Create profile" response
  observeEvent(input$wizard_no, {
    user_has_profile(FALSE)
    removeModal()
    showNotification(
      "No problem! Upload your CSV data and complete your mappings. Don't forget to save your profile when you're done!",
      type = "message",
      duration = 10
    )
  })

  # Handle CSV upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    tryCatch({
      # Support CSV and Excel uploads
      fpath <- input$upload_csv$datapath
      fname <- input$upload_csv$name
      loaded_csv_filename(fname)
      ext <- tolower(tools::file_ext(fname))
      if (ext %in% c("xlsx", "xls")) {
        uploaded <- as.data.frame(readxl::read_excel(fpath), stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        # Try Latin-1 first (most MLS exports are Windows-encoded), then UTF-8
        # Latin-1 is more permissive and handles most ASCII-extended characters
        uploaded <- tryCatch({
          # Suppress warnings during read attempt
          df <- suppressWarnings(read.csv(fpath, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "latin1"))
          message(sprintf("CSV read with latin1 encoding: %d rows, %d columns", nrow(df), ncol(df)))
          df
        }, error = function(e) {
          message(sprintf("Latin-1 encoding failed (%s), trying UTF-8...", e$message))
          tryCatch({
            read.csv(fpath, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
          }, error = function(e2) {
            message(sprintf("UTF-8 also failed (%s), trying default encoding...", e2$message))
            read.csv(fpath, stringsAsFactors = FALSE, check.names = FALSE)
          })
        })
      }

      # ============================================================
      # SMART FILE DETECTION: Check if this looks like a mapping profile
      # ============================================================
      looks_like_profile <- function(df) {
        cols <- tolower(names(df))
        # Profile signatures:
        # 1. Has Source and Target columns (case-insensitive)
        has_source_target <- "source" %in% cols && "target" %in% cols
        # 2. Few columns (typically 2-6: Source, Target, Exclusions, Class, etc.)
        few_columns <- ncol(df) <= 6
        # 3. Reasonable mapping count (< 200 rows - typical profiles have 20-100 mappings)
        reasonable_rows <- nrow(df) < 200

        return(has_source_target && few_columns && reasonable_rows)
      }

      if (looks_like_profile(uploaded)) {
        # Always reject profile-style files in the CSV data slot
        showModal(modalDialog(
          title = tags$span(style = "color: #d9534f;", icon("exclamation-triangle"), " Wrong File Type"),
          tags$div(
            tags$p(tags$strong("This appears to be a mapping profile, not MLS data.")),
            tags$p("The file has columns named 'Source' and 'Target' which is the format for saved mapping profiles."),
            tags$hr(),
            tags$p("Please use the ", tags$strong("'Load Mapping Profile'"), " section in the sidebar to upload your mapping profile."),
            tags$p(style = "color: #666;", "Mapping profiles typically have fewer than 200 rows and only a handful of columns."),
            tags$hr(),
            tags$p(tags$strong("File details:")),
            tags$ul(
              tags$li(sprintf("Filename: %s", fname)),
              tags$li(sprintf("Columns: %d (%s)", ncol(uploaded), paste(names(uploaded), collapse = ", "))),
              tags$li(sprintf("Rows: %d", nrow(uploaded)))
            )
          ),
          footer = modalButton("OK - I'll upload to the correct section"),
          easyClose = FALSE
        ))
        return()  # Stop processing
      }

      # Check for and handle duplicate column names
      original_names <- names(uploaded)
      duplicate_col_names <- unique(original_names[duplicated(original_names)])

      if (length(duplicate_col_names) > 0) {
        # Analyze each duplicate: is it a true duplicate (same data) or conflicting (different data)?
        true_duplicates <- c()
        conflicting_duplicates <- c()

        for (dup_name in duplicate_col_names) {
          # Get indices of all columns with this name
          dup_indices <- which(original_names == dup_name)

          # Compare all duplicate columns to the first one
          first_col <- uploaded[[dup_indices[1]]]
          is_true_duplicate <- TRUE

          for (idx in dup_indices[-1]) {
            other_col <- uploaded[[idx]]
            # Compare values (handle NA comparison properly)
            if (!identical(first_col, other_col)) {
              # Check if they're equivalent considering NA
              values_match <- all(
                (is.na(first_col) & is.na(other_col)) |
                (!is.na(first_col) & !is.na(other_col) & as.character(first_col) == as.character(other_col))
              )
              if (!values_match) {
                is_true_duplicate <- FALSE
                break
              }
            }
          }

          if (is_true_duplicate) {
            true_duplicates <- c(true_duplicates, dup_name)
          } else {
            conflicting_duplicates <- c(conflicting_duplicates, dup_name)
          }
        }

        # If there are conflicting duplicates, reject the file
        if (length(conflicting_duplicates) > 0) {
          showModal(modalDialog(
            title = tags$span(style = "color: #d9534f;", icon("exclamation-triangle"), " Invalid CSV File"),
            tags$div(
              tags$p(tags$strong("This file contains duplicate column names with different values.")),
              tags$p("The Column Mapper cannot process files with conflicting duplicate columns."),
              tags$hr(),
              tags$p(tags$strong("Conflicting duplicate columns:")),
              tags$p(style = "color: #d9534f; font-family: monospace;",
                paste(conflicting_duplicates[1:min(10, length(conflicting_duplicates))], collapse = ", "),
                if (length(conflicting_duplicates) > 10) sprintf(" ... and %d more", length(conflicting_duplicates) - 10) else ""
              ),
              tags$hr(),
              tags$p(style = "color: #666;",
                "Please clean your data source to ensure each column has a unique name, then re-upload.")
            ),
            footer = modalButton("OK"),
            easyClose = FALSE
          ))
          message(sprintf("REJECTED: CSV has %d conflicting duplicate column(s): %s",
                          length(conflicting_duplicates), paste(conflicting_duplicates, collapse = ", ")))
          return()
        }

        # Remove true duplicate columns (keep only the first instance)
        if (length(true_duplicates) > 0) {
          cols_to_remove <- c()
          for (dup_name in true_duplicates) {
            dup_indices <- which(original_names == dup_name)
            # Mark all but the first for removal
            cols_to_remove <- c(cols_to_remove, dup_indices[-1])
          }
          uploaded <- uploaded[, -cols_to_remove, drop = FALSE]

          # Count how many columns were removed
          removed_count <- length(cols_to_remove)
          dupe_details <- paste(sprintf("%s", true_duplicates), collapse = ", ")

          showNotification(
            HTML(sprintf(
              "<strong>Duplicate Columns Removed</strong><br/>Removed %d duplicate column(s) with identical data: <em>%s</em>",
              removed_count, dupe_details
            )),
            type = "message",
            duration = 8
          )

          message(sprintf("Removed %d true duplicate column(s): %s", removed_count, dupe_details))
        }
      }

      # Remove completely blank rows (all columns are NA or empty/whitespace)
      original_rows <- nrow(uploaded)
      is_blank_row <- apply(uploaded, 1, function(row) {
        # Check if all values are NA or empty/whitespace
        all(is.na(row) | trimws(as.character(row)) == "")
      })
      uploaded <- uploaded[!is_blank_row, , drop = FALSE]
      removed_rows <- original_rows - nrow(uploaded)

      if (removed_rows > 0) {
        message(sprintf("Removed %d blank rows from CSV", removed_rows))
      }

      current_dataset(uploaded)
      value_source_tracker(list())

      # Reset baths split acceptance for new data
      baths_split_accepted(FALSE)
      sqft_derive_accepted(FALSE)

      # Check if a profile was already loaded (before CSV)
      stored_profile <- loaded_mapping_profile()
      has_stored_profile <- !is.null(stored_profile) && length(stored_profile) > 0

      if (!has_stored_profile) {
        # No pre-loaded profile - clear everything for fresh start
        loaded_mapping_profile(list())
        profile_mapping_success(list())
        profile_was_loaded(FALSE)
      }

      # Clear caches when new data is uploaded
      suggestion_cache(list())

      # Hide unmapped section until user clicks update button
      show_unmapped_section(FALSE)

      # Show upload notification with blank row info and next step guidance
      if (has_stored_profile) {
        upload_msg <- sprintf("✓ CSV uploaded with %d rows and %d columns! Applying your pre-loaded profile...", nrow(uploaded), ncol(uploaded))
      } else {
        upload_msg <- sprintf("✓ CSV uploaded with %d rows and %d columns! Next step: Review and map your columns below.", nrow(uploaded), ncol(uploaded))
      }
      if (removed_rows > 0) {
        upload_msg <- sprintf("%s (%d blank rows removed)", upload_msg, removed_rows)
      }
      showNotification(upload_msg, type = "message", duration = 6)

      # Run column analysis and classification with progress indicator
      n_cols <- ncol(uploaded)
      withProgress(message = "Analyzing columns...", value = 0, {
        tryCatch({
          # Step 1: Analyze all columns for data quality metrics
          setProgress(value = 0.1, detail = sprintf("Analyzing %d columns for data quality", n_cols))
          rv$column_analysis <- analyze_all_columns(uploaded)

          # Step 2: Classify columns based on patterns
          setProgress(value = 0.7, detail = "Classifying columns by analytical value")
          rv$column_classifications <- classify_all_columns(uploaded, rv$column_analysis)

          # Preserve classification overrides from pre-loaded profile, otherwise clear
          # Profile overrides are restored after this block if has_stored_profile is TRUE
          if (!has_stored_profile) {
            rv$classification_overrides <- list()
            rv$dismissed_flags <- list()
          }

          setProgress(value = 1.0, detail = "Complete!")
          message(sprintf("Column analysis complete: %d columns analyzed",
                         nrow(rv$column_analysis)))

          # Debug: Show classification counts
          if (!is.null(rv$column_classifications)) {
            class_counts <- table(rv$column_classifications$class, useNA = "ifany")
            message(sprintf("Classification results: %s",
                           paste(sprintf("Class %s: %d", names(class_counts), class_counts), collapse = ", ")))
          }

          # Debug: Verify CLASS_4_PATTERNS is loaded
          message(sprintf("CLASS_4_PATTERNS has %d patterns defined", length(CLASS_4_PATTERNS)))
        }, error = function(e) {
          message(sprintf("Column analysis error: %s", e$message))
          showNotification(sprintf("Column analysis error: %s", e$message), type = "error", duration = 8)
        })
      })

      # If profile was pre-loaded, apply it now to the CSV data
      if (has_stored_profile) {
        message("=== APPLYING PRE-LOADED PROFILE TO CSV ===")
        message(sprintf("Dataset has %d columns", ncol(uploaded)))
        message(sprintf("Profile has %d mappings", length(stored_profile)))

        # Check if we're in destination mode
        # NOTE: Also check rv$dest_headers > 0 alone since checkbox update is async
        # When profile is loaded before CSV, the checkbox might not be updated yet
        is_dest_mode <- length(rv$dest_headers) > 0
        message(sprintf("Destination mode: %s (dest_headers: %d)", is_dest_mode, length(rv$dest_headers)))

        all_cols <- names(uploaded)
        successful_mappings <- 0
        failed_mappings <- 0
        success_tracker <- list()

        # Helper function to normalize target names (same as in profile upload handler)
        normalize_target_for_dest <- function(target_name, dest_headers) {
          if (length(dest_headers) == 0) return(target_name)
          if (target_name %in% dest_headers) return(target_name)
          target_normalized <- tolower(trimws(target_name))
          for (header in dest_headers) {
            if (tolower(trimws(header)) == target_normalized) {
              return(header)
            }
          }
          return(target_name)
        }

        # Apply the profile mappings to UI inputs
        for (target_name in names(stored_profile)) {
          # STEP 1: Resolve field aliases (e.g., MLSNo -> MLS)
          canonical_target <- if (is_dest_mode) {
            # In destination mode, use target_name as-is (it matches dest_headers)
            target_name
          } else {
            # In standard mode, resolve aliases to match config field names
            resolve_field_alias(target_name)
          }

          # STEP 2: Normalize for destination mode (case-insensitive matching)
          normalized_target <- if (is_dest_mode) {
            normalize_target_for_dest(canonical_target, rv$dest_headers)
          } else {
            canonical_target
          }

          input_id <- id_for(normalized_target, dest_mode = is_dest_mode)
          if (!is.null(input_id)) {
            profile_col <- stored_profile[[target_name]]

            if (!is.null(profile_col) && length(profile_col) > 0 && all(nzchar(trimws(profile_col)))) {
              profile_col_clean <- trimws(profile_col)

              # Try exact match (handle both single and multiple columns)
              if (all(profile_col_clean %in% all_cols)) {
                successful_mappings <- successful_mappings + 1
                success_tracker[[normalized_target]] <- TRUE  # Use normalized name for tracking
                updateSelectizeInput(session, input_id, selected = profile_col_clean)
                set_value_source(normalized_target, "profile", profile_col_clean)
              } else {
                # Try case-insensitive match
                close_matches <- lapply(profile_col_clean, function(pc) {
                  matched <- all_cols[tolower(all_cols) == tolower(pc)]
                  if (length(matched) > 0) matched[1] else NA
                })
                close_matches <- unlist(close_matches)
                if (all(!is.na(close_matches))) {
                  successful_mappings <- successful_mappings + 1
                  success_tracker[[normalized_target]] <- TRUE  # Use normalized name for tracking
                  updateSelectizeInput(session, input_id, selected = close_matches)
                  set_value_source(normalized_target, "profile", close_matches)
                } else {
                  failed_mappings <- failed_mappings + 1
                  success_tracker[[normalized_target]] <- FALSE  # Use normalized name for tracking
                }
              }
            } else {
              # Empty/null source in profile - in destination mode, try auto-map if target exists in source
              if (is_dest_mode) {
                # Try exact match first
                if (normalized_target %in% all_cols) {
                  successful_mappings <- successful_mappings + 1
                  success_tracker[[normalized_target]] <- TRUE
                  updateSelectizeInput(session, input_id, selected = normalized_target)
                  set_value_source(normalized_target, "profile", normalized_target)
                  message(sprintf("  AUTO-MAPPED (exact): %s", normalized_target))
                } else {
                  # Try case-insensitive match
                  matched <- all_cols[tolower(all_cols) == tolower(normalized_target)]
                  if (length(matched) > 0) {
                    successful_mappings <- successful_mappings + 1
                    success_tracker[[normalized_target]] <- TRUE
                    updateSelectizeInput(session, input_id, selected = matched[1])
                    set_value_source(normalized_target, "profile", matched[1])
                    message(sprintf("  AUTO-MAPPED (case-insensitive): %s -> %s", normalized_target, matched[1]))
                  } else {
                    message(sprintf("  NO MATCH for empty source: %s", normalized_target))
                  }
                }
              }
            }
          }
        }

        # Save success tracker
        profile_mapping_success(success_tracker)

        # Clean profile to only keep successful mappings
        cleaned_profile <- list()
        for (target_name in names(stored_profile)) {
          if (isTRUE(success_tracker[[target_name]])) {
            cleaned_profile[[target_name]] <- stored_profile[[target_name]]
          }
        }
        loaded_mapping_profile(cleaned_profile)

        # Decimal baths handling after pre-loaded profile apply: if BA has decimal baths AND
        # PB maps to the same source, auto-accept the split. Otherwise block PB suggestions.
        is_dest_mode_baths <- length(rv$dest_headers) > 0
        ba_source_pre <- NULL
        if (is_dest_mode_baths) {
          for (dh in rv$dest_headers) {
            sf <- find_std_field_for_column(dh)
            if (!is.null(sf) && sf == "BA" && isTRUE(success_tracker[[dh]])) {
              ba_source_pre <- stored_profile[[dh]]
              break
            }
          }
        } else {
          if (isTRUE(success_tracker[["BA"]])) ba_source_pre <- stored_profile[["BA"]]
        }
        if (!is.null(ba_source_pre) && length(ba_source_pre) == 1 &&
            ba_source_pre %in% all_cols && is_decimal_baths(uploaded[[ba_source_pre]])) {
          pb_source_pre <- NULL
          if (is_dest_mode_baths) {
            for (dh in rv$dest_headers) {
              sf <- find_std_field_for_column(dh)
              if (!is.null(sf) && sf == "PB" && isTRUE(success_tracker[[dh]])) {
                pb_source_pre <- stored_profile[[dh]]
                break
              }
            }
          } else {
            if (isTRUE(success_tracker[["PB"]])) pb_source_pre <- stored_profile[["PB"]]
          }

          if (!is.null(pb_source_pre) && identical(pb_source_pre, ba_source_pre)) {
            baths_split_accepted(TRUE)
            message(sprintf("Auto-accepted decimal bath split from pre-loaded profile (BA and PB both map to '%s')", ba_source_pre))
          } else {
            sug_cache <- suggestion_cache()
            sug_cache[["PB"]] <- NULL
            suggestion_cache(sug_cache)
            if (is_dest_mode_baths) {
              dest_sug <- destination_suggestion_cache()
              dest_sug[["PB"]] <- NULL
              destination_suggestion_cache(dest_sug)
            }
            message("Blocked independent PB suggestions after pre-loaded profile apply (decimal baths detected on BA)")
          }
        }

        # Show result notification
        if (failed_mappings > 0) {
          showNotification(
            sprintf("Applied profile: %d mappings succeeded, %d columns not found in CSV",
                   successful_mappings, failed_mappings),
            type = "warning",
            duration = 8
          )
        } else {
          showNotification(
            sprintf("Applied profile with %d mappings successfully!",
                   successful_mappings),
            type = "message",
            duration = 5
          )
        }

        # Trigger UI refresh
        ui_refresh_trigger(ui_refresh_trigger() + 1)
      }
    }, error = function(e) {
      showNotification(paste("Error uploading CSV:", e$message), type = "error")
    })
  })

  # Toggle destination schema controls enabled/disabled
  observeEvent(input$use_destination_schema, {
    if (isTRUE(input$use_destination_schema)) {
      shinyjs::runjs("$('#dest_schema_controls').removeClass('disabled-section');")
    } else {
      shinyjs::runjs("$('#dest_schema_controls').addClass('disabled-section');")
      # Clear dest headers when unchecking (return to standard mode)
      rv$dest_headers <- character()
      destination_suggestion_cache(list())
      id_analysis_result(NULL)
      id_conflicts_resolved(list())
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dest_schema_file, {
    req(input$dest_schema_file)
    tryCatch({
      fpath <- input$dest_schema_file$datapath
      fname <- input$dest_schema_file$name
      ext <- tolower(tools::file_ext(fname))

      if (ext %in% c("xlsx", "xls")) {
        dest_df <- as.data.frame(readxl::read_excel(fpath), stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        dest_df <- read.csv(fpath, stringsAsFactors = FALSE, check.names = FALSE)
      }

      if (!ncol(dest_df)) {
        stop("Destination dataset has no columns.")
      }

      headers <- names(dest_df)

      # Reject destination schemas with duplicate column names
      duplicate_headers <- headers[duplicated(headers)]
      if (length(duplicate_headers) > 0) {
        unique_dupes <- unique(duplicate_headers)
        showModal(modalDialog(
          title = tags$span(style = "color: #d9534f;", icon("exclamation-triangle"), " Invalid Destination Schema"),
          tags$div(
            tags$p(tags$strong("This file contains duplicate column names and cannot be used as a destination schema.")),
            tags$p("A valid destination schema must have unique column names."),
            tags$hr(),
            tags$p(tags$strong("Duplicate columns found:")),
            tags$p(style = "color: #d9534f; font-family: monospace;",
              paste(unique_dupes[1:min(10, length(unique_dupes))], collapse = ", "),
              if (length(unique_dupes) > 10) sprintf(" ... and %d more", length(unique_dupes) - 10) else ""
            ),
            tags$hr(),
            tags$p(style = "color: #666;", "Please upload a file with unique column names.")
          ),
          footer = modalButton("OK"),
          easyClose = FALSE
        ))
        return()
      }

      headers <- trimws(headers)
      rv$dest_headers <- headers

      dataset_for_suggestions <- if (nrow(dest_df) > 1000) dest_df[seq_len(1000), , drop = FALSE] else dest_df
      # NOTE: Pass empty exclusions list for destination headers
      # Exclusions apply only to source columns, not fixed destination schema
      dest_suggestions <- suggest_all_mappings(
        logic_target_groups,
        headers,
        dataset_for_suggestions,
        list(),
        list()  # Empty exclusions - destination headers are fixed schema
      )
      destination_suggestion_cache(dest_suggestions)

      # Reserve PB for decimal bath split in dest mode: block independent PB suggestions.
      # PB will be auto-filled when user accepts the split (baths_split_accept observer).
      ba_sug <- dest_suggestions[["BA"]]
      if (!is.null(ba_sug) && is.list(ba_sug) && !is.null(ba_sug$column) && !is.null(dataset_for_suggestions)) {
        ba_src <- ba_sug$column
        if (ba_src %in% names(dataset_for_suggestions) && is_decimal_baths(dataset_for_suggestions[[ba_src]])) {
          dest_suggestions[["PB"]] <- NULL
          destination_suggestion_cache(dest_suggestions)
        }
      }

      updateCheckboxInput(session, "use_destination_schema", value = TRUE)
      shinyjs::runjs("$('#dest_schema_controls').removeClass('disabled-section');")
      showNotification(
        sprintf("✓ Destination schema loaded with %d columns! Next step: Upload your CSV data to map the columns.",
               length(headers)),
        type = "message",
        duration = 8
      )
    }, error = function(e) {
      showNotification(paste("Error loading destination dataset:", e$message), type = "error", duration = 8)
    })
  }, ignoreNULL = TRUE)

  # Observer for preset destination schema templates
  # App now starts in Standard Mode (no destination schema selected by default)
  # User can optionally select a template to enable Destination Schema Mode
  observeEvent(input$preset_destination_schema, {
    req(input$preset_destination_schema != "")

    template_name <- input$preset_destination_schema

    tryCatch({
      # Determine template file path and display name
      template_info <- if (template_name == "reso_core") {
        list(
          path = file.path(getwd(), "templates", "RESO_Residential_Core.csv"),
          name = "RESO Residential Core"
        )
      } else if (template_name == "reso_full") {
        list(
          path = file.path(getwd(), "templates", "RESO_Data_Dictionary_Full.csv"),
          name = "RESO Data Dictionary Full"
        )
      } else if (template_name == "cvalr_legacy") {
        list(
          path = file.path(getwd(), "templates", "CValR_4.0_Standard.csv"),
          name = "CValR 4.0 Standard"
        )
      } else {
        stop("Unknown template: ", template_name)
      }
      template_path <- template_info$path

      if (!file.exists(template_path)) {
        stop("Template file not found: ", template_path)
      }

      # Read template (headers only)
      dest_df <- read.csv(template_path, stringsAsFactors = FALSE, check.names = FALSE, nrows = 1)
      headers <- names(dest_df)
      headers <- trimws(headers)

      rv$dest_headers <- headers

      # Calculate suggestions for destination headers
      dest_suggestions <- suggest_all_mappings(
        logic_target_groups,
        headers,
        NULL,
        list(),
        list()
      )
      destination_suggestion_cache(dest_suggestions)

      # Reserve PB for decimal bath split in dest mode (no-op for header-only templates):
      # Block independent PB suggestions; PB filled on accept.
      ba_sug <- dest_suggestions[["BA"]]
      if (!is.null(ba_sug) && is.list(ba_sug) && !is.null(ba_sug$column)) {
        ba_src <- ba_sug$column
        if (ba_src %in% names(dest_df) && is_decimal_baths(dest_df[[ba_src]])) {
          dest_suggestions[["PB"]] <- NULL
          destination_suggestion_cache(dest_suggestions)
        }
      }

      # Analyze ID columns for potential conflicts (RESO: ListingKey vs ListingId)
      # Build ID field scores from the suggestions
      id_field_scores <- list()
      cat("[ID-ANALYSIS] Starting ID field analysis...\n")
      for (field_name in c("ListingKey", "ListingId")) {
        if (!is.null(dest_suggestions[[field_name]])) {
          col <- dest_suggestions[[field_name]]$column
          score <- dest_suggestions[[field_name]]$score
          cat(sprintf("[ID-ANALYSIS] %s suggestion: column='%s', score=%s\n", field_name, col, score))
          if (!is.null(col) && !is.null(score)) {
            # Store the max score for this column against any ID field
            existing_score <- id_field_scores[[col]] %||% 0
            id_field_scores[[col]] <- max(existing_score, score)
          }
        } else {
          cat(sprintf("[ID-ANALYSIS] %s: NO suggestion found\n", field_name))
        }
      }

      # Analyze all source columns for ID patterns
      dataset <- current_dataset()
      if (!is.null(dataset)) {
        for (col in names(dataset)) {
          cat_result <- categorize_id_column(col)
          if (cat_result != "Unknown") {
            if (is.null(id_field_scores[[col]])) {
              # Column matches ID pattern but wasn't in suggestions - give it a base score
              id_field_scores[[col]] <- 85
            }
            cat(sprintf("[ID-ANALYSIS] Column '%s' categorized as %s, score=%d\n", col, cat_result, id_field_scores[[col]]))
          }
        }
        id_analysis <- analyze_id_columns(names(dataset), id_field_scores)
        cat(sprintf("[ID-ANALYSIS] Result: %d candidates, single_id=%s, needs_prompt=%s\n",
            nrow(id_analysis$candidates), id_analysis$single_id, id_analysis$needs_prompt))
        if (nrow(id_analysis$candidates) > 0) {
          for (i in 1:nrow(id_analysis$candidates)) {
            cat(sprintf("[ID-ANALYSIS] Candidate %d: %s (score=%d, category=%s)\n",
                i, id_analysis$candidates$column[i], id_analysis$candidates$score[i], id_analysis$candidates$category[i]))
          }
        }
        id_analysis_result(id_analysis)
      } else {
        cat("[ID-ANALYSIS] No dataset available\n")
      }

      updateCheckboxInput(session, "use_destination_schema", value = TRUE)
      shinyjs::runjs("$('#dest_schema_controls').removeClass('disabled-section');")

      showNotification(
        sprintf("✓ %s template loaded with %d fields!", template_info$name, length(headers)),
        type = "message",
        duration = 6
      )
    }, error = function(e) {
      showNotification(paste("Error loading template:", e$message), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$clear_destination_schema, {
    rv$dest_headers <- character()
    destination_suggestion_cache(list())
    id_analysis_result(NULL)
    id_conflicts_resolved(list())
    updateCheckboxInput(session, "use_destination_schema", value = FALSE)
    shinyjs::runjs("$('#dest_schema_controls').addClass('disabled-section');")
    showNotification("Destination schema cleared.", type = "message", duration = 4)
  })

  # ============================================================================
  # ID ANALYSIS TRIGGER ON DATASET LOAD
  # Runs ID analysis when dataset is loaded AND schema is selected
  # ============================================================================
  observeEvent(current_dataset(), {
    dataset <- current_dataset()
    if (is.null(dataset)) return()

    # Only run if schema is selected and we haven't already resolved
    dest_headers <- rv$dest_headers
    if (length(dest_headers) == 0) {
      cat("[ID-ANALYSIS-DATASET] No schema selected yet, skipping\n")
      return()
    }

    resolved <- id_conflicts_resolved()
    if (!is.null(resolved$ListingKey)) {
      cat("[ID-ANALYSIS-DATASET] Already resolved, skipping\n")
      return()
    }

    cat("[ID-ANALYSIS-DATASET] Dataset loaded with schema selected, running ID analysis...\n")

    # Get suggestion cache
    dest_suggestions <- suggestion_cache()

    # Build ID field scores from the suggestions
    id_field_scores <- list()
    for (field_name in c("ListingKey", "ListingId")) {
      if (!is.null(dest_suggestions[[field_name]])) {
        col <- dest_suggestions[[field_name]]$column
        score <- dest_suggestions[[field_name]]$score
        cat(sprintf("[ID-ANALYSIS-DATASET] %s suggestion: column='%s', score=%s\n", field_name, col, score))
        if (!is.null(col) && !is.null(score)) {
          existing_score <- id_field_scores[[col]] %||% 0
          id_field_scores[[col]] <- max(existing_score, score)
        }
      }
    }

    # Analyze all source columns for ID patterns
    for (col in names(dataset)) {
      cat_result <- categorize_id_column(col)
      if (cat_result != "Unknown") {
        if (is.null(id_field_scores[[col]])) {
          id_field_scores[[col]] <- 85
        }
        cat(sprintf("[ID-ANALYSIS-DATASET] Column '%s' categorized as %s, score=%d\n",
            col, cat_result, id_field_scores[[col]]))
      }
    }

    id_analysis <- analyze_id_columns(names(dataset), id_field_scores)
    cat(sprintf("[ID-ANALYSIS-DATASET] Result: %d candidates, single_id=%s, needs_prompt=%s\n",
        nrow(id_analysis$candidates), id_analysis$single_id, id_analysis$needs_prompt))

    if (nrow(id_analysis$candidates) > 0) {
      for (i in 1:nrow(id_analysis$candidates)) {
        cat(sprintf("[ID-ANALYSIS-DATASET] Candidate %d: %s (score=%d, category=%s)\n",
            i, id_analysis$candidates$column[i], id_analysis$candidates$score[i],
            id_analysis$candidates$category[i]))
      }
    }

    id_analysis_result(id_analysis)
  }, ignoreInit = TRUE)

  # ============================================================================
  # ID FIELD CONFLICT RESOLUTION MODAL (RESO: ListingKey vs ListingId)
  # ============================================================================

  # Observer to show ID field selection modal when conflicts detected
  observeEvent(id_analysis_result(), {
    analysis <- id_analysis_result()
    cat("[ID-MODAL] Observer triggered\n")
    if (is.null(analysis)) {
      cat("[ID-MODAL] Analysis is NULL, returning\n")
      return()
    }
    cat(sprintf("[ID-MODAL] Analysis has %d candidates, single_id=%s, needs_prompt=%s\n",
        nrow(analysis$candidates), analysis$single_id, analysis$needs_prompt))

    # Check if already resolved
    resolved <- id_conflicts_resolved()
    if (!is.null(resolved$ListingKey)) {
      cat(sprintf("[ID-MODAL] Already resolved to '%s', returning\n", resolved$ListingKey))
      return()
    }

    # Single ID column: auto-assign to ListingKey and notify
    if (analysis$single_id && nrow(analysis$candidates) == 1) {
      cat("[ID-MODAL] Single ID column detected, auto-assigning\n")
      selected_col <- analysis$candidates$column[1]
      selected_category <- analysis$candidates$category[1]

      # Update suggestion cache to map this column to ListingKey and UniqueID
      cache <- suggestion_cache()
      cache[["ListingKey"]] <- list(column = selected_col, score = 100)
      cache[["UniqueID"]] <- list(column = selected_col, score = 100)
      suggestion_cache(cache)

      # Mark as resolved
      id_conflicts_resolved(list(ListingKey = selected_col, auto = TRUE))

      # If the only ID candidate is a ListingId-type (MLS#), show a prominent modal
      # so the user clearly understands the fallback
      is_mls_fallback <- identical(selected_category, "ListingId")

      if (is_mls_fallback) {
        cat(sprintf("[ID-MODAL] MLS fallback: '%s' (ListingId) used as ListingKey\n", selected_col))
        showModal(modalDialog(
          title = tagList(icon("info-circle"), " Unique ID: Using MLS Number"),
          tags$div(
            style = "font-size: 14px;",
            tags$p(
              "Your data does not contain a dedicated unique identifier column (e.g. ListingKey, Matrix_Unique_ID, or Record ID)."
            ),
            tags$div(
              style = "background: #fff3e0; padding: 12px; border-radius: 6px; border-left: 4px solid #ff9800; margin: 12px 0;",
              tags$div(
                style = "font-weight: 600; color: #e65100; margin-bottom: 4px;",
                icon("exchange"), sprintf(" \"%s\" will be used as the Unique Identifier", selected_col)
              ),
              tags$p(
                style = "font-size: 13px; color: #555; margin: 4px 0 0 0;",
                "MLS numbers are typically unique within a single board, but may not be unique ",
                "across multiple boards or over time. This is usually fine for single-board datasets."
              )
            ),
            tags$div(
              style = "background: #e8f5e9; padding: 10px; border-radius: 6px; border-left: 4px solid #4caf50; margin: 12px 0;",
              tags$span(style = "font-weight: 600; color: #2e7d32;", icon("check-circle"),
                sprintf(" \"%s\" will map to both ListingKey and UniqueID", selected_col)),
              tags$p(
                style = "font-size: 12px; color: #555; margin: 4px 0 0 0;",
                "CValR only needs one unique identifier. You do not need to map the UniqueID field separately."
              )
            ),
            tags$p(
              style = "font-size: 12px; color: #777; margin-top: 10px;",
              "You can change this later by mapping a different column to ListingKey on the mapping page."
            )
          ),
          footer = modalButton("OK, Got It"),
          size = "m",
          easyClose = TRUE
        ))
      } else {
        showNotification(
          HTML(sprintf(
            "<strong>Unique ID Auto-Mapped</strong><br/>Your data has one ID column ('%s'). It has been mapped to <em>ListingKey</em> as your unique identifier.",
            selected_col
          )),
          type = "message",
          duration = 8
        )
      }
      return()
    }

    # Multiple ID columns: show selection modal
    if (analysis$needs_prompt && nrow(analysis$candidates) >= 2) {
      cat("[ID-MODAL] Multiple ID columns detected, showing selection modal\n")
      candidates <- analysis$candidates
      id_selection_pending(TRUE)

      # Get dataset for data quality analysis
      dataset <- current_dataset()

      # Build data quality info for each candidate
      candidate_info <- lapply(1:nrow(candidates), function(i) {
        col_name <- candidates$column[i]
        col_data <- if (!is.null(dataset) && col_name %in% names(dataset)) dataset[[col_name]] else NULL

        if (!is.null(col_data)) {
          # Calculate metrics
          total_rows <- length(col_data)
          non_empty <- sum(!is.na(col_data) & trimws(as.character(col_data)) != "")
          fill_rate <- round(100 * non_empty / total_rows, 1)
          unique_vals <- length(unique(col_data[!is.na(col_data) & trimws(as.character(col_data)) != ""]))

          # Get sample values (first 4 unique non-empty values)
          unique_samples <- unique(col_data[!is.na(col_data) & trimws(as.character(col_data)) != ""])
          samples <- head(unique_samples, 4)
          sample_text <- paste(samples, collapse = ", ")
          if (length(unique_samples) > 4) sample_text <- paste0(sample_text, ", ...")

          list(
            fill_rate = fill_rate,
            unique_count = unique_vals,
            total_rows = total_rows,
            samples = sample_text,
            is_unique = unique_vals == non_empty && non_empty == total_rows
          )
        } else {
          list(fill_rate = NA, unique_count = NA, total_rows = NA, samples = "N/A", is_unique = FALSE)
        }
      })

      # Build radio button choices with data quality cards
      choice_cards <- lapply(1:nrow(candidates), function(i) {
        col_name <- candidates$column[i]
        category <- candidates$category[i]
        info <- candidate_info[[i]]

        # Uniqueness badge
        unique_badge <- if (!is.na(info$is_unique) && info$is_unique) {
          tags$span(class = "badge", style = "background: #28a745; margin-left: 8px;", "100% Unique")
        } else if (!is.na(info$unique_count) && !is.na(info$total_rows)) {
          unique_pct <- round(100 * info$unique_count / info$total_rows, 0)
          tags$span(class = "badge", style = "background: #6c757d; margin-left: 8px;",
                    sprintf("%d%% Unique", unique_pct))
        } else NULL

        tags$div(
          class = "id-candidate-card",
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; margin-bottom: 10px; background: #fff;",
          tags$div(
            style = "display: flex; align-items: center;",
            tags$input(type = "radio", name = "id_unique_choice", value = col_name,
                       checked = if (i == 1) "checked" else NULL,
                       style = "margin-right: 12px; transform: scale(1.2);"),
            tags$div(
              style = "flex: 1;",
              tags$div(
                style = "font-weight: 600; font-size: 14px;",
                col_name,
                tags$span(style = "color: #6c757d; font-weight: normal; margin-left: 8px;",
                          sprintf("(%s)", category)),
                unique_badge
              ),
              if (!is.na(info$fill_rate)) {
                tags$div(
                  style = "font-size: 12px; color: #495057; margin-top: 6px;",
                  tags$span(icon("chart-bar", style = "color: #17a2b8;"),
                            sprintf(" Fill: %s%%", info$fill_rate)),
                  tags$span(style = "margin-left: 15px;",
                            icon("fingerprint", style = "color: #6f42c1;"),
                            sprintf(" %s unique values", format(info$unique_count, big.mark = ",")))
                )
              },
              if (!is.na(info$fill_rate)) {
                tags$div(
                  style = "font-size: 11px; color: #6c757d; margin-top: 4px; font-family: monospace;
                           background: #f8f9fa; padding: 4px 8px; border-radius: 3px; overflow: hidden;
                           text-overflow: ellipsis; white-space: nowrap;",
                  icon("eye", style = "margin-right: 4px;"),
                  info$samples
                )
              }
            )
          )
        )
      })

      # Default selection (first candidate)
      default_selection <- candidates$column[1]

      showModal(modalDialog(
        title = tags$span(
          icon("question-circle", style = "color: #f0ad4e;"),
          " Select Unique Identifier Column"
        ),
        tags$div(
          tags$p("Multiple ID columns detected in your data. Per RESO standards:"),
          tags$ul(
            tags$li(tags$strong("ListingKey"), " = THE unique identifier (guaranteed unique per listing)"),
            tags$li(tags$strong("ListingId"), " = MLS number (may NOT be unique across different MLSs)")
          ),
          tags$hr(),
          tags$p(tags$strong("Which column is your unique identifier?")),
          tags$div(
            style = "max-height: 300px; overflow-y: auto; padding: 5px;",
            choice_cards
          ),
          tags$p(
            style = "font-size: 12px; color: #666; margin-top: 10px;",
            icon("info-circle"),
            " The selected column will map to ListingKey. Other ID columns can still map to ListingId."
          ),
          # JavaScript to sync radio button selection to Shiny
          tags$script(HTML(sprintf("
            $(document).ready(function() {
              Shiny.setInputValue('id_unique_choice', '%s');
              $('input[name=\"id_unique_choice\"]').on('change', function() {
                Shiny.setInputValue('id_unique_choice', $(this).val());
              });
            });
          ", default_selection)))
        ),
        footer = tagList(
          actionButton("id_choice_confirm", "Use Selected Column", class = "btn-primary"),
          actionButton("id_choice_skip", "Skip (use auto-suggestion)", class = "btn-default")
        ),
        easyClose = FALSE,
        size = "m"
      ))
    }
  }, ignoreInit = TRUE)

  # Handle ID selection confirmation
  observeEvent(input$id_choice_confirm, {
    selected_column <- input$id_unique_choice
    analysis <- id_analysis_result()

    if (is.null(selected_column) || is.null(analysis)) {
      removeModal()
      id_selection_pending(FALSE)
      return()
    }

    # Update suggestion cache: selected column -> ListingKey
    cache <- suggestion_cache()
    cache[["ListingKey"]] <- list(column = selected_column, score = 100)

    # Find non-selected columns and exclude them from ListingKey
    exclusions <- field_exclusions()
    excluded_from_key <- exclusions[["ListingKey"]] %||% character(0)

    for (i in 1:nrow(analysis$candidates)) {
      col <- analysis$candidates$column[i]
      if (col != selected_column) {
        excluded_from_key <- c(excluded_from_key, col)
      }
    }

    exclusions[["ListingKey"]] <- unique(excluded_from_key)
    field_exclusions(exclusions)
    suggestion_cache(cache)

    # Mark as resolved
    id_conflicts_resolved(list(ListingKey = selected_column, auto = FALSE))

    removeModal()
    id_selection_pending(FALSE)

    # Trigger UI refresh
    ui_refresh_trigger(ui_refresh_trigger() + 1)

    showNotification(
      HTML(sprintf("<strong>'%s'</strong> set as unique identifier (ListingKey)", selected_column)),
      type = "message",
      duration = 5
    )
  })

  # Handle skip (use auto-suggestion)
  observeEvent(input$id_choice_skip, {
    # Mark as resolved with auto-suggestion
    cache <- suggestion_cache()
    auto_selected <- if (!is.null(cache[["ListingKey"]])) cache[["ListingKey"]]$column else NULL

    id_conflicts_resolved(list(ListingKey = auto_selected, auto = TRUE, skipped = TRUE))

    removeModal()
    id_selection_pending(FALSE)

    showNotification("Using auto-suggested ID column mapping.", type = "message", duration = 4)
  })

  output$destination_schema_status <- renderUI({
    headers <- rv$dest_headers
    use_dest <- isTRUE(input$use_destination_schema)

    if (!length(headers)) {
      if (use_dest) {
        return(tags$div(
          style = "color: #d9534f; font-size: 12px; margin-top: 8px;",
          icon("exclamation-triangle"),
          " Upload a destination dataset to activate the destination schema."
        ))
      }
      return(tags$div(
        style = "color: #666; font-size: 12px; margin-top: 8px;",
        icon("info-circle"),
        " Using built-in CValR standard fields for mapping.",
        tags$br(),
        "Check the box above to use a custom destination schema."
      ))
    }

    summary_block <- tagList(
      tags$div(
        icon("check-circle", class = "text-success"),
        sprintf("Loaded %d destination column%s.", length(headers), if (length(headers) == 1) "" else "s")
      ),
      tags$div(
        icon("arrow-right"),
        "Your source data will be mapped directly to these destination columns."
      ),
      if (isTRUE(input$restrict_to_destination_only)) {
        tags$div(
          icon("filter"),
          "Output limited to destination columns only (derived metrics still included if enabled)."
        )
      } else {
        tags$div(
          icon("filter"),
          "Output will include destination columns plus all mapped fields you opted to keep."
        )
      },
      tags$div(
        icon("lock"),
        "Destination structure is read-only once uploaded."
      )
    )

    list_items <- lapply(headers, function(dest_name) {
      tags$li(tags$strong(dest_name))
    })

    tags$div(
      class = "destination-status",
      style = "margin-top: 8px; font-size: 12px;",
      summary_block,
      tags$hr(style = "margin: 8px 0;"),
      tags$div(
        style = "max-height: 180px; overflow-y: auto;",
        tags$ul(style = "padding-left: 18px; margin: 0;", list_items)
      )
    )
  })

  # Dynamic workflow sidebar based on user choice
  output$workflow_sidebar <- renderUI({
    user_choice <- user_has_profile()

    if (is.null(user_choice)) {
      # Wizard not answered yet - show placeholder
      tagList(
        helpText(style = "text-align: center; color: #666; font-style: italic;",
                "Please answer the welcome question to begin...")
      )
    } else if (user_choice == TRUE) {
      # User HAS a profile - show profile-first workflow
      tagList(
        h4("1. Load Mapping Profile"),
        uiOutput("profile_upload_ui"),
        hr(),
        h4("2. Data Upload"),
        fileInput("upload_csv", "Upload Your CSV/Excel", accept = c(".csv", ".xlsx", ".xls")),
        if (!is.null(loaded_csv_filename())) {
          div(style = "margin-top: -10px; margin-bottom: 10px; color: #5cb85c;",
              icon("check-circle"), strong(" Loaded: "), loaded_csv_filename())
        },
        helpText("Profile will auto-apply when you upload your CSV data."),
        hr(),
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: bold; font-size: 16px; padding: 8px; background: #f0f0f0; border-radius: 4px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(icon("list"), " Available Columns in Upload"),
            icon("chevron-down", class = "expand-icon")
          ),
          verbatimTextOutput("available_columns")
        ),
        hr(),
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: bold; font-size: 16px; padding: 8px; background: #f0f0f0; border-radius: 4px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(icon("check-circle"), " Current Mappings"),
            icon("chevron-down", class = "expand-icon")
          ),
          helpText("Shows fields mapped from profile or manually selected"),
          verbatimTextOutput("current_mappings")
        )
      )
    } else {
      # User does NOT have a profile - show simplified workflow
      tagList(
        h4("1. Data Upload"),
        fileInput("upload_csv", "Upload Your CSV/Excel", accept = c(".csv", ".xlsx", ".xls")),
        if (!is.null(loaded_csv_filename())) {
          div(style = "margin-top: -10px; margin-bottom: 10px; color: #5cb85c;",
              icon("check-circle"), strong(" Loaded: "), loaded_csv_filename())
        },
        helpText("Upload your CSV data and map the columns below."),
        hr(),
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: bold; font-size: 16px; padding: 8px; background: #f0f0f0; border-radius: 4px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(icon("list"), " Available Columns in Upload"),
            icon("chevron-down", class = "expand-icon")
          ),
          verbatimTextOutput("available_columns")
        ),
        hr(),
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: bold; font-size: 16px; padding: 8px; background: #f0f0f0; border-radius: 4px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(icon("check-circle"), " Current Mappings"),
            icon("chevron-down", class = "expand-icon")
          ),
          helpText("Shows fields mapped from profile or manually selected"),
          verbatimTextOutput("current_mappings")
        )
      )
    }
  })

  # Save Mappings Section - displayed in main panel (moved from sidebar)
  output$save_mappings_section <- renderUI({
    # Only show when CSV data is uploaded
    req(current_dataset())

    tags$div(
      style = "margin: 20px 0;",
      tags$div(
        class = "save-profile-highlight",
        style = "background: #fcf8e3; border: 2px solid #f0ad4e; border-radius: 8px; padding: 15px;",
        fluidRow(
          column(6,
            tags$div(
              style = "display: flex; align-items: center; margin-bottom: 10px;",
              icon("save", style = "color: #f0ad4e; font-size: 20px; margin-right: 10px;"),
              tags$h4(style = "margin: 0; color: #8a6d3b;", "Save Your Mappings")
            ),
            helpText(style = "color: #8a6d3b;",
                    "Save your mappings as a profile for future use")
          ),
          column(3,
            selectInput("mapping_format", "Export Format:",
                        choices = c("RDS" = "rds", "CSV" = "csv", "JSON" = "json"),
                        selected = "csv")
          ),
          column(3,
            tags$div(
              style = "margin-top: 25px;",
              downloadButton("download_mapping", "Save Mapping Profile",
                            class = "btn-warning",
                            style = "width: 100%; font-weight: bold;")
            )
          )
        )
      )
    )
  })

  # Next Step: Decision Trees section - shown after mapping is applied
  output$next_step_decision_trees <- renderUI({
    # Only show when mapped data exists
    mapped_data <- logic_mapped_data()
    if (is.null(mapped_data) || nrow(mapped_data) == 0) {
      return(NULL)
    }

    tags$div(
      style = "margin: 20px 0;",
      tags$div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px; padding: 20px; color: white;",
        fluidRow(
          column(8,
            tags$div(
              style = "display: flex; align-items: center; margin-bottom: 10px;",
              icon("arrow-right", style = "font-size: 24px; margin-right: 12px;"),
              tags$h4(style = "margin: 0; font-weight: bold;", "Next Step: Decision Trees")
            ),
            tags$p(style = "margin: 0; opacity: 0.9;",
                   "Your mapped data is ready! Open Decision Trees to analyze CMS sales and generate ADF decision reports.")
          ),
          column(4,
            tags$div(
              style = "display: flex; flex-direction: column; gap: 10px; align-items: flex-end;",
              actionButton("launch_decision_trees",
                          tagList(icon("sitemap"), " Open Decision Trees"),
                          class = "btn-lg",
                          style = "background: white; color: #667eea; border: none; font-weight: bold; width: 100%;"),
              tags$small(style = "opacity: 0.8;", "Launches on port 4000")
            )
          )
        )
      )
    )
  })

  # Handler for switching to Decision Trees (from nav bar or next step button)
  launch_decision_trees_handler <- function() {
    # Show loading modal
    showModal(modalDialog(
      title = NULL,
      tags$div(
        style = "text-align: center; padding: 30px;",
        tags$h3("Launching Decision Trees..."),
        tags$p("Please wait while the application starts."),
        tags$div(style = "margin-top: 20px;",
          tags$i(class = "fa fa-spinner fa-spin fa-2x")
        ),
        tags$p(id = "dt_launch_status", style = "margin-top: 15px; color: #666;", "Starting R process...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    # Launch Decision Trees app in background on port 4000
    dt_path <- "C:/GitRepos/CAA DEMOS/CA-ADF-CMS-DecisionTrees/ADF-CMS-Tool"
    cmd <- sprintf('start /B "" "C:\\Program Files\\R\\R-4.3.1\\bin\\Rscript.exe" -e "setwd(\'%s\'); shiny::runApp(\'.\', port=4000, launch.browser=FALSE)"', dt_path)
    shell(cmd, wait = FALSE)

    # Poll for the app to be ready, then redirect
    shinyjs::runjs("
      var checkCount = 0;
      var maxChecks = 30;
      var checkInterval = setInterval(function() {
        checkCount++;
        var statusEl = document.getElementById('dt_launch_status');
        if (statusEl) statusEl.innerText = 'Waiting for app to start... (' + checkCount + 's)';

        fetch('http://localhost:4000', {mode: 'no-cors'})
          .then(function() {
            clearInterval(checkInterval);
            if (statusEl) statusEl.innerText = 'Ready! Redirecting...';
            setTimeout(function() {
              window.location.href = 'http://localhost:4000';
            }, 500);
          })
          .catch(function() {
            if (checkCount >= maxChecks) {
              clearInterval(checkInterval);
              if (statusEl) statusEl.innerText = 'Timeout - please try http://localhost:4000 manually or check if Decision Trees is installed correctly.';
            }
          });
      }, 1000);
    ")
  }

  # Nav bar switch to Decision Trees
  observeEvent(input$switch_to_dt, {
    launch_decision_trees_handler()
  })

  # Next step button switch to Decision Trees
  observeEvent(input$launch_decision_trees, {
    launch_decision_trees_handler()
  })

  # Nav bar switch to Report Forms
  observeEvent(input$switch_to_reports, {
    # Launch ReportForms Streamlit app in background on port 8502
    reports_path <- "C:/GitRepos/ReportForms"
    cmd <- sprintf('start /B "" cmd /c "cd /d %s && python -m streamlit run app.py --server.port 8502 --server.headless true"', reports_path)
    shell(cmd, wait = FALSE)

    # Redirect after a short delay to let it start
    shinyjs::runjs("
      setTimeout(function() {
        window.location.href = 'http://localhost:8502';
      }, 2000);
    ")
  })

  # Conditional UI for profile upload
  output$profile_upload_ui <- renderUI({
    profile_name <- loaded_profile_filename()

    if (is.null(current_dataset())) {
      # No CSV loaded yet - profile can be loaded first (recommended!)
      tagList(
        fileInput("upload_mapping", "Load Mapping Profile",
                  accept = c(".rds", ".csv", ".json")),
        if (!is.null(profile_name)) {
          div(style = "margin-top: -10px; margin-bottom: 10px; color: #5cb85c;",
            icon("check-circle"), strong(" Loaded: "), profile_name)
        },
        helpText(style = "color: #5bc0de;",
                icon("info-circle"), " Load your mapping profile first (recommended), then upload CSV to auto-apply mappings")
      )
    } else {
      # CSV loaded - profile can be loaded to override mappings
      tagList(
        fileInput("upload_mapping", "Load Mapping Profile",
                  accept = c(".rds", ".csv", ".json")),
        if (!is.null(profile_name)) {
          div(style = "margin-top: -10px; margin-bottom: 10px; color: #5cb85c;",
            icon("check-circle"), strong(" Loaded: "), profile_name)
        },
        helpText(style = "color: #5cb85c;",
                icon("check-circle"), " CSV loaded - load a profile to apply saved mappings")
      )
    }
  })

  # Handle mapping profile upload
  observeEvent(input$upload_mapping, {
    req(input$upload_mapping)

    tryCatch({
      file_path <- input$upload_mapping$datapath
      file_ext <- tolower(tools::file_ext(input$upload_mapping$name))
      fname <- input$upload_mapping$name

      # ============================================================
      # SMART FILE DETECTION: Check if this looks like MLS data (not a profile)
      # ============================================================
      if (file_ext == "csv") {
        # Try to read the CSV to check its structure
        test_df <- tryCatch({
          read.csv(file_path, stringsAsFactors = FALSE, nrows = 10)
        }, error = function(e) NULL)

        if (!is.null(test_df)) {
          looks_like_data <- function(df) {
            cols <- tolower(names(df))
            # Profile signatures - if it has these, it's probably a profile
            has_source_target <- "source" %in% cols && "target" %in% cols
            # Data signatures - many columns and/or many rows
            many_columns <- ncol(df) > 6
            # Check full row count for data files
            full_df <- tryCatch({
              read.csv(file_path, stringsAsFactors = FALSE)
            }, error = function(e) df)
            many_rows <- nrow(full_df) >= 200
            # It's probably MLS data if it has many columns, many rows, and no Source/Target
            return(!has_source_target && (many_columns || many_rows))
          }

          if (looks_like_data(test_df)) {
            # This looks like MLS data, not a mapping profile - reject it
            showModal(modalDialog(
              title = tags$span(style = "color: #d9534f;", icon("exclamation-triangle"), " Wrong File Type"),
              tags$div(
                tags$p(tags$strong("This appears to be MLS data, not a mapping profile.")),
                tags$p("The file has many columns and rows, which is typical of MLS data files, not mapping profiles."),
                tags$hr(),
                tags$p("Please use the ", tags$strong("'Upload Your CSV Data'"), " section in the sidebar to upload your MLS data."),
                tags$p(style = "color: #666;", "Mapping profiles typically have columns like 'Source' and 'Target' with fewer than 200 rows."),
                tags$hr(),
                tags$p(tags$strong("File details:")),
                tags$ul(
                  tags$li(sprintf("Filename: %s", fname)),
                  tags$li(sprintf("Columns: %d", ncol(test_df)))
                )
              ),
              footer = modalButton("OK - I'll upload to the correct section"),
              easyClose = FALSE
            ))
            return()  # Stop processing
          }
        }
      }
      # ============================================================

      # Store the uploaded filename for display
      loaded_profile_filename(input$upload_mapping$name)

      default_payload <- list(
        schema_type = "cvalr",
        source_mappings = list(),
        destination_headers = character(),
        destination_assignments = character(0),
        restrict_to_destination_only = TRUE,
        classification_overrides = list(),
        dismissed_flags = list()
      )

      profile_payload <- default_payload

      if (file_ext == "rds") {
        raw_profile <- readRDS(file_path)
        if (is.list(raw_profile) && !is.null(raw_profile$schema_type) && !is.null(raw_profile$source_mappings)) {
          profile_payload$schema_type <- raw_profile$schema_type %||% "cvalr"
          profile_payload$source_mappings <- raw_profile$source_mappings %||% list()
          profile_payload$exclusions <- raw_profile$exclusions %||% list()
          profile_payload$classification_overrides <- raw_profile$classification_overrides %||% list()
          profile_payload$dismissed_flags <- raw_profile$dismissed_flags %||% list()
          dest_headers <- raw_profile$destination_headers %||% character()
          if (is.list(dest_headers)) dest_headers <- unlist(dest_headers)
          profile_payload$destination_headers <- dest_headers
          dest_assign <- raw_profile$destination_assignments %||% character(0)
          if (is.list(dest_assign)) dest_assign <- unlist(dest_assign)
          profile_payload$destination_assignments <- dest_assign
          profile_payload$restrict_to_destination_only <- isTRUE(raw_profile$restrict_to_destination_only)
        } else {
          profile_payload$source_mappings <- raw_profile
          profile_payload$exclusions <- list()
          profile_payload$classification_overrides <- list()
          profile_payload$dismissed_flags <- list()
        }
      } else if (file_ext == "csv") {
        df <- read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE)
        # Note: Using ;;; delimiter to avoid conflicts with pipe characters in column names
        source_vals <- lapply(as.character(df$Source), function(src) {
          cols <- trimws(strsplit(src, ";;;", fixed = TRUE)[[1]])
          if (length(cols) == 1) cols[1] else cols
        })
        target_vals <- trimws(as.character(df$Target))
        profile_payload$source_mappings <- setNames(source_vals, target_vals)

        # Parse exclusions column if present
        if ("Exclusions" %in% names(df)) {
          exc_vals <- lapply(as.character(df$Exclusions), function(exc_str) {
            exc_str <- trimws(exc_str)
            if (is.na(exc_str) || !nzchar(exc_str)) return(character(0))
            # Split on ;;; delimiter (avoids conflicts with pipe in column names)
            trimws(strsplit(exc_str, ";;;", fixed = TRUE)[[1]])
          })
          profile_payload$exclusions <- setNames(exc_vals, target_vals)
        } else {
          profile_payload$exclusions <- list()
        }

        if ("Destination" %in% names(df)) {
          dest_vals <- trimws(as.character(df$Destination))
          mask <- nzchar(dest_vals)
          if (any(mask)) {
            dest_assign <- dest_vals[mask]
            names(dest_assign) <- target_vals[mask]
            profile_payload$destination_assignments <- dest_assign
            profile_payload$destination_headers <- unique(dest_assign)
            profile_payload$schema_type <- "destination"
          }
        }

        # Check SchemaType column for destination mode (used in newer CSV exports)
        # This handles profiles where Target columns ARE the destination headers
        if ("SchemaType" %in% names(df)) {
          schema_types <- trimws(as.character(df$SchemaType))
          if (any(schema_types == "destination", na.rm = TRUE)) {
            profile_payload$schema_type <- "destination"
            # In destination mode, the Target column values are the destination headers
            # Only include targets that have actual source mappings or classifications
            dest_targets <- target_vals[schema_types == "destination"]
            if (length(dest_targets) > 0) {
              profile_payload$destination_headers <- unique(dest_targets)
              message(sprintf("CSV profile: Detected destination mode with %d destination columns from SchemaType",
                              length(profile_payload$destination_headers)))
            }
          }
        }

        # Parse Classification column if present (user override decisions)
        if ("Classification" %in% names(df)) {
          override_vals <- as.character(df$Classification)
          override_list <- list()
          for (i in seq_along(override_vals)) {
            class_val <- trimws(override_vals[i])
            if (nzchar(class_val) && !is.na(class_val)) {
              class_num <- suppressWarnings(as.integer(class_val))
              if (!is.na(class_num) && class_num %in% c(1L, 2L, 3L, 4L)) {
                override_list[[target_vals[i]]] <- class_num
              }
            }
          }
          profile_payload$classification_overrides <- override_list
        } else {
          profile_payload$classification_overrides <- list()
        }

        # Parse DismissedFlags column if present
        if ("DismissedFlags" %in% names(df)) {
          dismissed_list <- list()
          for (i in seq_along(target_vals)) {
            flags_str <- trimws(as.character(df$DismissedFlags[i]))
            if (nzchar(flags_str) && !is.na(flags_str)) {
              flags <- trimws(strsplit(flags_str, ";;;", fixed = TRUE)[[1]])
              flags <- flags[nzchar(flags)]
              if (length(flags) > 0) {
                dismissed_list[[target_vals[i]]] <- flags
              }
            }
          }
          profile_payload$dismissed_flags <- dismissed_list
        } else {
          profile_payload$dismissed_flags <- list()
        }
      } else if (file_ext == "json") {
        raw_profile <- jsonlite::fromJSON(file_path, simplifyVector = TRUE)
        if (is.list(raw_profile) && !is.null(raw_profile$schema_type) && !is.null(raw_profile$source_mappings)) {
          profile_payload$schema_type <- raw_profile$schema_type %||% "cvalr"
          profile_payload$source_mappings <- raw_profile$source_mappings %||% list()
          profile_payload$exclusions <- raw_profile$exclusions %||% list()
          profile_payload$classification_overrides <- raw_profile$classification_overrides %||% list()
          profile_payload$dismissed_flags <- raw_profile$dismissed_flags %||% list()
          dest_headers <- raw_profile$destination_headers %||% character()
          if (is.list(dest_headers)) dest_headers <- unlist(dest_headers)
          profile_payload$destination_headers <- dest_headers
          dest_assign <- raw_profile$destination_assignments %||% character(0)
          if (is.list(dest_assign)) dest_assign <- unlist(dest_assign)
          profile_payload$destination_assignments <- dest_assign
          profile_payload$restrict_to_destination_only <- isTRUE(raw_profile$restrict_to_destination_only)
        } else {
          profile_payload$source_mappings <- raw_profile
          profile_payload$exclusions <- list()
          profile_payload$classification_overrides <- list()
          profile_payload$dismissed_flags <- list()
        }
      } else {
        stop("Unsupported file format. Please use .rds, .csv, or .json")
      }

      profile_payload$destination_headers <- unique(profile_payload$destination_headers)

      profile <- profile_payload$source_mappings %||% list()

      # Normalize exclusion keys using field aliases (convert display names to internal names)
      raw_exclusions <- profile_payload$exclusions %||% list()
      normalized_exclusions <- list()
      for (field_name in names(raw_exclusions)) {
        canonical_name <- resolve_field_alias(field_name)
        normalized_exclusions[[canonical_name]] <- raw_exclusions[[field_name]]
        if (canonical_name != field_name && length(raw_exclusions[[field_name]]) > 0) {
          message(sprintf("Normalized exclusion key: '%s' -> '%s'", field_name, canonical_name))
        }
      }

      # Store the profile
      loaded_mapping_profile(profile)
      field_exclusions(normalized_exclusions)

      # Restore classification overrides from profile (user's previous decisions)
      profile_overrides <- profile_payload$classification_overrides %||% list()
      if (length(profile_overrides) > 0) {
        rv$classification_overrides <- profile_overrides
        message(sprintf("Restored %d classification overrides from profile", length(profile_overrides)))
      }

      # Restore dismissed flags from profile
      profile_dismissed <- profile_payload$dismissed_flags %||% list()
      if (length(profile_dismissed) > 0) {
        rv$dismissed_flags <- profile_dismissed
        message(sprintf("Restored %d dismissed flag entries from profile", length(profile_dismissed)))
      }

      # DEBUG: Log final normalized exclusions
      if (length(normalized_exclusions) > 0) {
        message("=== NORMALIZED EXCLUSIONS STORED ===")
        for (fld in names(normalized_exclusions)) {
          if (length(normalized_exclusions[[fld]]) > 0) {
            message(sprintf("  %s: [%s]", fld, paste(normalized_exclusions[[fld]], collapse=", ")))
          }
        }
      }

      profile_was_loaded(TRUE)
      value_source_tracker(list())

      destination_mode_loaded <- identical(profile_payload$schema_type, "destination") &&
        length(profile_payload$destination_headers) > 0

      if (destination_mode_loaded) {
        headers <- profile_payload$destination_headers
        # CRITICAL: Preserve existing destination headers if already loaded
        # This ensures profile loading works regardless of upload order
        if (length(rv$dest_headers) > 0) {
          # Destination schema was loaded first - keep existing headers
          headers <- rv$dest_headers
          message("Preserving existing destination headers (destination loaded before profile)")
        } else {
          # Profile loaded first - use headers from profile
          rv$dest_headers <- headers
          message("Using destination headers from profile")
        }

        # NOTE: Pass empty exclusions list for destination headers
        # Exclusions apply only to source columns, not fixed destination schema
        dest_suggestions <- suggest_all_mappings(
          logic_target_groups,
          headers,
          NULL,
          list(),
          list()  # Empty exclusions - destination headers are fixed schema
        )
        destination_suggestion_cache(dest_suggestions)

        updateCheckboxInput(session, "use_destination_schema", value = TRUE)
        updateCheckboxInput(session, "restrict_to_destination_only",
                            value = isTRUE(profile_payload$restrict_to_destination_only))
      } else {
        rv$dest_headers <- character()
        destination_suggestion_cache(list())
      }

      # Clear suggestion cache - profile takes precedence over auto-suggestions
      suggestion_cache(list())

      # Check if CSV data is loaded
      if (is.null(current_dataset())) {
        # No CSV yet - just store profile for later application
        showNotification(
          sprintf("✓ Profile loaded with %d mappings! Next step: Upload your CSV data to apply the mappings.",
                 length(profile)),
          type = "message",
          duration = 8
        )
        return()
      }

      # CSV is loaded - validate and apply profile mappings
      all_cols <- names(current_dataset())

      successful_mappings <- 0
      failed_mappings <- 0
      empty_mappings <- 0

      # Track which mappings succeeded
      success_tracker <- list()

      # Helper function to normalize target names to match existing destination headers
      # This ensures tracker keys match rv$dest_headers regardless of load order
      normalize_target_for_dest <- function(target_name, dest_headers) {
        if (length(dest_headers) == 0) return(target_name)  # No normalization needed

        # Try exact match first
        if (target_name %in% dest_headers) return(target_name)

        # Try case-insensitive match with whitespace normalization
        target_normalized <- tolower(trimws(target_name))
        for (header in dest_headers) {
          header_normalized <- tolower(trimws(header))
          if (target_normalized == header_normalized) {
            message(sprintf("Normalized target '%s' to '%s'", target_name, header))
            return(header)
          }
        }

        # No match found - return original (will likely fail, but that's expected)
        return(target_name)
      }

      # VALIDATION: Check for unrecognized profile fields
      unrecognized_fields <- character()
      aliased_fields <- character()

      for (target_name in names(profile)) {
        canonical_target <- if (destination_mode_loaded) {
          target_name
        } else {
          resolve_field_alias(target_name)
        }

        # Track if alias was applied
        if (!destination_mode_loaded && canonical_target != target_name) {
          aliased_fields <- c(aliased_fields, sprintf("%s→%s", target_name, canonical_target))
        }

        # Check if canonical target exists in config or destination headers
        field_exists <- if (destination_mode_loaded) {
          canonical_target %in% rv$dest_headers
        } else {
          # Check if field exists in logic_target_groups
          any(sapply(logic_target_groups, function(grp) canonical_target %in% names(grp)))
        }

        if (!field_exists) {
          unrecognized_fields <- c(unrecognized_fields, target_name)
        }
      }

      # Show notification for aliased fields (informational)
      if (length(aliased_fields) > 0) {
        message(sprintf("Applied %d field alias(es): %s",
                       length(aliased_fields),
                       paste(aliased_fields, collapse=", ")))
      }

      # Show warning for unrecognized fields
      if (length(unrecognized_fields) > 0) {
        showNotification(
          HTML(sprintf("<strong>Profile Compatibility:</strong><br/>%d field(s) not recognized and will be skipped:<br/><em>%s</em>",
                      length(unrecognized_fields),
                      paste(unrecognized_fields, collapse=", "))),
          type = "warning",
          duration = 10
        )
      }

      # Auto-apply the loaded mappings to the UI inputs
      for (target_name in names(profile)) {
        # STEP 1: Resolve field aliases (e.g., MLSNo -> MLS)
        canonical_target <- if (destination_mode_loaded) {
          # In destination mode, use target_name as-is (it matches dest_headers)
          target_name
        } else {
          # In standard mode, resolve aliases to match config field names
          resolve_field_alias(target_name)
        }

        # STEP 2: Normalize for destination mode (case-insensitive matching)
        normalized_target <- if (destination_mode_loaded) {
          normalize_target_for_dest(canonical_target, rv$dest_headers)
        } else {
          canonical_target
        }

        input_id <- id_for(normalized_target, dest_mode = destination_mode_loaded)
        if (!is.null(input_id)) {
          profile_col <- profile[[target_name]]

          # Check if the profile's column exists in current dataset
          if (!is.null(profile_col) && length(profile_col) > 0 && all(nzchar(trimws(profile_col)))) {
            # Trim whitespace from profile column name
            profile_col_clean <- trimws(profile_col)

            # Try exact match first (handle both single and multiple columns)
            if (all(profile_col_clean %in% all_cols)) {
              successful_mappings <- successful_mappings + 1
              success_tracker[[normalized_target]] <- TRUE  # Use normalized name for tracking
              updateSelectizeInput(session, input_id, selected = profile_col_clean)
              set_value_source(normalized_target, "profile", profile_col_clean)
            } else {
              # Try case-insensitive match for each column
              close_matches <- lapply(profile_col_clean, function(pc) {
                matched <- all_cols[tolower(all_cols) == tolower(pc)]
                if (length(matched) > 0) matched[1] else NA
              })
              close_matches <- unlist(close_matches)

              if (all(!is.na(close_matches))) {
                successful_mappings <- successful_mappings + 1
                success_tracker[[normalized_target]] <- TRUE  # Use normalized name for tracking
                updateSelectizeInput(session, input_id, selected = close_matches)
                set_value_source(normalized_target, "profile", close_matches)
              } else {
                # Try fuzzy match (ignoring spaces, underscores, special chars) for each column
                fuzzy_matches <- lapply(profile_col_clean, function(pc) {
                  profile_norm <- tolower(gsub("[^a-z0-9]", "", pc))
                  cols_norm <- tolower(gsub("[^a-z0-9]", "", all_cols))
                  fuzzy_match_idx <- which(cols_norm == profile_norm)
                  if (length(fuzzy_match_idx) > 0) all_cols[fuzzy_match_idx[1]] else NA
                })
                fuzzy_matches <- unlist(fuzzy_matches)

                if (all(!is.na(fuzzy_matches))) {
                  successful_mappings <- successful_mappings + 1
                  success_tracker[[normalized_target]] <- TRUE  # Use normalized name for tracking
                  updateSelectizeInput(session, input_id, selected = fuzzy_matches)
                  set_value_source(normalized_target, "profile", fuzzy_matches)
                } else {
                  failed_mappings <- failed_mappings + 1
                  success_tracker[[normalized_target]] <- FALSE  # Use normalized name for tracking
                }
              }
            }
          } else {
            # Empty/null value in profile - in destination mode, try to auto-map if target name exists in source
            if (destination_mode_loaded) {
              # In destination mode, destination column names often match source column names
              # Try exact match first
              if (normalized_target %in% all_cols) {
                successful_mappings <- successful_mappings + 1
                success_tracker[[normalized_target]] <- TRUE
                updateSelectizeInput(session, input_id, selected = normalized_target)
                set_value_source(normalized_target, "profile", normalized_target)
                message(sprintf("Auto-mapped destination column '%s' (exact match in source)", normalized_target))
              } else {
                # Try case-insensitive match
                matched <- all_cols[tolower(all_cols) == tolower(normalized_target)]
                if (length(matched) > 0) {
                  successful_mappings <- successful_mappings + 1
                  success_tracker[[normalized_target]] <- TRUE
                  updateSelectizeInput(session, input_id, selected = matched[1])
                  set_value_source(normalized_target, "profile", matched[1])
                  message(sprintf("Auto-mapped destination column '%s' -> '%s' (case-insensitive match)", normalized_target, matched[1]))
                } else {
                  # No match found - this is expected for destination columns not in source
                  empty_mappings <- empty_mappings + 1
                }
              }
            } else {
              # In CValR mode, empty source values are just skipped
              empty_mappings <- empty_mappings + 1
            }
          }
        } else {
          message(sprintf("⚠ Target '%s' not recognized (invalid target field)", target_name))
        }
      }

      message(sprintf("=== RESULTS: %d successful, %d failed, %d empty ===",
                     successful_mappings, failed_mappings, empty_mappings))

      # Save the success tracker
      profile_mapping_success(success_tracker)

      # CRITICAL FIX: Remove failed mappings from loaded_mapping_profile
      # This prevents the badge logic from treating failed profile mappings as valid
      cleaned_profile <- list()
      for (target_name in names(profile)) {
        if (isTRUE(success_tracker[[target_name]])) {
          cleaned_profile[[target_name]] <- profile[[target_name]]
        }
      }
      loaded_mapping_profile(cleaned_profile)
      profile_was_loaded(TRUE)  # Mark that user explicitly loaded a profile
      message(sprintf("Cleaned profile: kept %d successful mappings, removed %d failed mappings",
                     length(cleaned_profile), failed_mappings))

      # IMPORTANT: Clear all selectize inputs for fields NOT in the profile
      # This prevents old auto-suggestions from being treated as profile mappings
      if (destination_mode_loaded) {
        # Clear destination fields not in profile
        for (dest_col in rv$dest_headers) {
          if (!isTRUE(success_tracker[[dest_col]])) {
            input_id <- id_for(dest_col, dest_mode = TRUE)
            if (!is.null(input_id)) {
              updateSelectizeInput(session, input_id, selected = character(0))
              clear_value_source(dest_col)
              message(sprintf("Cleared %s (not in profile or failed to apply)", dest_col))
            }
          }
        }
      } else {
        # Clear standard fields not in profile
        for (gn in names(logic_target_groups)) {
          grp <- logic_target_groups[[gn]]
          for (tgt in names(grp)) {
            # If this field is NOT in the profile (or failed to apply), clear it
            if (!isTRUE(success_tracker[[tgt]])) {
              input_id <- id_for(tgt, dest_mode = FALSE)
              if (!is.null(input_id)) {
                updateSelectizeInput(session, input_id, selected = character(0))
                clear_value_source(tgt)
                message(sprintf("Cleared %s (not in profile or failed to apply)", tgt))
              }
            }
          }
        }
      }

      # Decimal baths handling after profile load: if BA has decimal baths AND
      # PB maps to the same source, auto-accept the split. Otherwise block PB suggestions.
      dataset <- current_dataset()
      if (!is.null(dataset)) {
        ba_source <- NULL
        if (destination_mode_loaded) {
          for (dh in rv$dest_headers) {
            sf <- find_std_field_for_column(dh)
            if (!is.null(sf) && sf == "BA" && isTRUE(success_tracker[[dh]])) {
              ba_source <- profile[[dh]]
              break
            }
          }
        } else {
          if (isTRUE(success_tracker[["BA"]])) ba_source <- profile[["BA"]]
        }
        if (!is.null(ba_source) && length(ba_source) == 1 &&
            ba_source %in% names(dataset) && is_decimal_baths(dataset[[ba_source]])) {
          # Check if PB is also mapped to the same source in the profile
          pb_source <- NULL
          if (destination_mode_loaded) {
            for (dh in rv$dest_headers) {
              sf <- find_std_field_for_column(dh)
              if (!is.null(sf) && sf == "PB" && isTRUE(success_tracker[[dh]])) {
                pb_source <- profile[[dh]]
                break
              }
            }
          } else {
            if (isTRUE(success_tracker[["PB"]])) pb_source <- profile[["PB"]]
          }

          if (!is.null(pb_source) && identical(pb_source, ba_source)) {
            # Both BA and PB map to the same decimal baths source — auto-accept the split
            baths_split_accepted(TRUE)
            message(sprintf("Auto-accepted decimal bath split from profile (BA and PB both map to '%s')", ba_source))
          } else {
            # Only BA has decimal baths, PB not yet mapped — block independent PB suggestions
            sug_cache <- suggestion_cache()
            sug_cache[["PB"]] <- NULL
            suggestion_cache(sug_cache)
            if (destination_mode_loaded) {
              dest_sug <- destination_suggestion_cache()
              dest_sug[["PB"]] <- NULL
              destination_suggestion_cache(dest_sug)
            }
            message("Blocked independent PB suggestions after profile load (decimal baths detected on BA)")
          }
        }
      }

      # Show notification with validation results
      if (failed_mappings > 0) {
        showNotification(
          sprintf("Loaded %s profile: %d mappings applied, %d columns not found in current dataset",
                 toupper(file_ext), successful_mappings, failed_mappings),
          type = "warning",
          duration = 8
        )
      } else {
        showNotification(
          sprintf("✓ Profile loaded with %d mappings applied! Next step: Review your mappings below and make any adjustments needed.",
                 successful_mappings),
          type = "message",
          duration = 8
        )
      }

      # Trigger UI refresh to ensure badges update correctly
      ui_refresh_trigger(ui_refresh_trigger() + 1)
    }, error = function(e) {
      showNotification(paste("Error loading mapping profile:", e$message), type = "error")
    })
  })

  # Get all column names from current dataset
  all_columns <- reactive({
    names(current_dataset())
  })

 # Get mappable columns (Class 1, 2 & 3) for the mapping UI
  # Class 4 (PII/internal) is excluded entirely from mapping options
  mappable_columns <- reactive({
    all_cols <- names(current_dataset())
    if (is.null(all_cols) || length(all_cols) == 0) return(character(0))

    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides

    if (is.null(classifications)) {
      # No classification yet - return all columns
      return(all_cols)
    }

    # Determine effective class for each column (override takes precedence)
    mappable <- sapply(all_cols, function(col_name) {
      # Check for user override first
      if (!is.null(overrides[[col_name]])) {
        effective_class <- overrides[[col_name]]
      } else {
        # Use auto-classification
        idx <- which(classifications$column_name == col_name)
        if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
          effective_class <- classifications$class[idx[1]]
        } else {
          # Unclassified columns are mappable (user hasn't decided yet)
          effective_class <- NA
        }
      }

      # Class 1, 2 & 3 are mappable, unclassified (NA) is also mappable
      # Only Class 4 (PII/internal) is excluded from mapping UI
      is.na(effective_class) || effective_class %in% c(1, 2, 3)
    })

    all_cols[mappable]
  })

  # Track which columns are currently mapped
  get_all_selections <- reactive({
    # Force reactivity on any input change
    reactiveValuesToList(input)

    selections <- list()

    # Check if we're in destination mode
    dest_mode <- isTRUE(input$use_destination_schema) && length(rv$dest_headers) > 0

    if (dest_mode) {
      dataset_loaded_dest <- !is.null(current_dataset())
      # Destination mode: collect selections from dest column selectors
      for (dest_col in rv$dest_headers) {
        input_id <- paste0("dest_", make.names(dest_col), "_selector")
        sel <- input[[input_id]]
        if (!is.null(sel) && length(sel) > 0 && all(nzchar(sel))) {
          selections[[dest_col]] <- sel
        }
      }
    } else {
      # Standard mode: collect selections from standard field selectors
      for (gn in names(logic_target_groups)) {
        grp <- logic_target_groups[[gn]]
        for (tgt in names(grp)) {
          sel <- input[[id_for(tgt)]]
          if (!is.null(sel) && length(sel) > 0 && all(nzchar(sel))) {
            selections[[tgt]] <- sel
          }
        }
      }
    }

    # Merge in accepted Class 3 mappings that may not yet be reflected in input values
    # (updateSelectizeInput is async, so input values aren't immediately available)
    accepted_c3 <- class3_accepted_mappings()
    if (length(accepted_c3) > 0) {
      for (target in names(accepted_c3)) {
        # Only add if not already in selections (input hasn't caught up yet)
        if (is.null(selections[[target]]) || !nzchar(selections[[target]][1])) {
          selections[[target]] <- accepted_c3[[target]]
        }
      }
    }

    selections
  })

  # Display available columns
  output$available_columns <- renderText({
    if (is.null(current_dataset())) {
      return("(No data loaded)")
    }
    paste(all_columns(), collapse = "\n")
  })

  # Display current mappings
  output$current_mappings <- renderText({
    if (is.null(current_dataset())) {
      return("(No data loaded)")
    }

    # Get current selections (reactive, not isolated - updates on changes)
    mappings <- get_all_selections()

    if (length(mappings) == 0) {
      return("No mappings yet")
    }

    # Get profile success status to distinguish profile from suggestions
    success_status <- profile_mapping_success()
    tracked_sources <- value_source_tracker()

    # Build formatted output with status indicators
    mapping_lines <- character()
    for (target_field in names(mappings)) {
      source_col <- mappings[[target_field]]
      # Convert vector to string for display (handles multiple columns like Address)
      source_col_str <- paste(source_col, collapse = ", ")

      entry <- tracked_sources[[target_field]]
      source_label <- entry$source %||% ""

      if (!nzchar(source_label)) {
        if (isTRUE(success_status[[target_field]])) {
          source_label <- "profile"
        } else {
          source_label <- "manual"
        }
      }

      status_tag <- switch(
        source_label,
        profile = "[Profile]",
        suggestion = "[Suggested]",
        exact = "[Exact]",
        manual = "[Manual]",
        "[Mapped]"
      )

      mapping_lines <- c(mapping_lines,
                        sprintf("%s %s → %s", status_tag, target_field, source_col_str))
    }

    paste(mapping_lines, collapse = "\n")
  })


  observeEvent(
    {
      list(
        current_dataset(),
        loaded_mapping_profile(),
        profile_mapping_success()
      )
    },
    {
      dataset <- current_dataset()
      if (is.null(dataset)) {
        suggestion_cache(list())
        return()
      }

      all_cols <- names(dataset)
      loaded_profile <- loaded_mapping_profile()
      success_status <- profile_mapping_success()

      successful_profile_mappings <- list()
      for (tgt in names(loaded_profile)) {
        if (!is.null(success_status[[tgt]]) && isTRUE(success_status[[tgt]])) {
          successful_profile_mappings[[tgt]] <- loaded_profile[[tgt]]
        }
      }

      dataset_for_inspection <- if (nrow(dataset) > 1000) NULL else dataset

      # Show progress for suggestion calculation (can be slow for 300+ columns)
      withProgress(message = "Calculating suggestions...", value = 0, {
        setProgress(value = 0.2, detail = sprintf("Scoring %d columns against target fields", length(all_cols)))
        all_suggestions <- suggest_all_mappings(
          logic_target_groups,
          all_cols,
          dataset_for_inspection,
          successful_profile_mappings,
          field_exclusions(),
          rv$column_classifications  # Pass classifications to filter out Class 4 columns
        )
        setProgress(value = 1.0, detail = "Complete!")
      })

      suggestion_cache(all_suggestions)

      # Reserve PB for decimal bath split: if BA's suggested source has decimal values,
      # clear PB's suggestion so it doesn't get an independent match
      ba_sug <- all_suggestions[["BA"]]
      if (!is.null(ba_sug) && is.list(ba_sug) && !is.null(ba_sug$column) && !is.null(dataset)) {
        ba_src <- ba_sug$column
        if (ba_src %in% names(dataset) && is_decimal_baths(dataset[[ba_src]])) {
          all_suggestions[["PB"]] <- NULL  # Block independent PB suggestions; filled on accept
          suggestion_cache(all_suggestions)
        }
      }

      # Debug: Log suggestion calculation
      message(sprintf("=== SUGGESTIONS CALCULATED: %d targets with suggestions ===", length(all_suggestions)))
      if (length(all_suggestions) > 0) {
        sample_suggestions <- head(names(all_suggestions), 3)
        for (tgt in sample_suggestions) {
          sug <- all_suggestions[[tgt]]
          if (!is.null(sug) && is.list(sug) && !is.null(sug$column) && !is.null(sug$score)) {
            message(sprintf("  %s -> %s (score %d)", tgt, sug$column, sug$score))
          }
        }
      }

      # Increment UI refresh trigger to show suggestion badges
      # This ensures the UI re-renders after suggestions are calculated
      # Important for showing yellow "Suggested" badges when CSV is uploaded without a profile
      old_trigger <- ui_refresh_trigger()
      ui_refresh_trigger(old_trigger + 1)
      message(sprintf("=== UI REFRESH TRIGGER: %d -> %d ===", old_trigger, old_trigger + 1))
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  output$mapping_ui <- renderUI({
    # Depend on UI refresh trigger to ensure re-render after profile loads
    trigger_val <- ui_refresh_trigger()
    message(sprintf("=== RENDER UI TRIGGERED (ui_refresh_trigger = %d) ===", trigger_val))

    # Read value_source_tracker once at the top to avoid inconsistent reads
    tracked_sources <- value_source_tracker()


    # Check if dataset is loaded
    if (is.null(current_dataset())) {
      return(div(
        style = "padding: 40px; text-align: center; color: #666;",
        h4("No Data Loaded"),
        p("Please upload a CSV file to begin mapping columns."),
        p("Use the 'Upload Your CSV' option in the sidebar to get started.")
      ))
    }

    # Use mappable_columns() for the dropdown choices (Class 1, 2 & 3)
    # Only Class 4 (PII/internal) columns are excluded from mapping options
    all_cols <- mappable_columns()

    dest_mode <- isTRUE(input$use_destination_schema) && length(rv$dest_headers) > 0
    dest_headers <- rv$dest_headers
    dataset_loaded_dest <- !is.null(current_dataset())

    # Use isolate to prevent circular dependency
    # The UI won't re-render on every selection change (that would be too disruptive)
    # But the JavaScript will dynamically check mapped columns when rendering
    current_selections <- isolate(get_all_selections())
    mapped_columns <- unique(unlist(current_selections))

    success_status <- profile_mapping_success()
    cache <- suggestion_cache()

    search_query <- search_query_debounced()
    if (is.null(search_query)) search_query <- ""

    # Branch: Destination mode uses destination columns as targets
    if (dest_mode) {
      # Map destination columns directly - no intermediate standard fields
      dest_suggestions_cache <- destination_suggestion_cache()

      # Helper: suggest source column for a destination column
      suggest_for_dest <- function(dest_col) {
        # The destination_suggestion_cache is keyed by STANDARD FIELD, not dest column
        # So we need to find which standard field(s) suggest this dest column
        matching_std_field <- NULL
        matching_std_meta <- NULL
        for (std_field in names(dest_suggestions_cache)) {
          info <- dest_suggestions_cache[[std_field]]
          if (is.list(info) && !is.null(info$column)) {
            if (identical(info$column, dest_col)) {
              matching_std_field <- std_field
              # Also get the metadata for this standard field
              for (gn in names(logic_target_groups)) {
                if (std_field %in% names(logic_target_groups[[gn]])) {
                  matching_std_meta <- logic_target_groups[[gn]][[std_field]]
                  break
                }
              }
              break
            }
          }
        }

        if (!is.null(matching_std_field)) {
          # Check if cache has a suggestion for this standard field
          cached_suggestion <- cache[[matching_std_field]]
          if (!is.null(cached_suggestion)) {
            return(cached_suggestion)
          }

          # Cache miss (source column may have been assigned to another standard field)
          # Search source columns using this standard field's synonyms
          if (!is.null(matching_std_meta) && length(matching_std_meta$synonyms) > 0) {
            synonyms <- matching_std_meta$synonyms
            syn_norm <- tolower(gsub("[^[:alnum:]]", "", synonyms))

            # Try exact synonym match against source columns
            for (col in all_cols) {
              col_norm <- tolower(gsub("[^[:alnum:]]", "", col))
              if (col_norm %in% syn_norm) {
                return(list(column = col, score = 90, source = "synonym_fallback"))
              }
            }

            # Try word-based synonym match (e.g., "StoriesTotal" contains "stories")
            for (col in all_cols) {
              col_lower <- tolower(col)
              for (syn in synonyms) {
                syn_lower <- tolower(syn)
                # Check if column contains synonym as word/prefix
                if (grepl(paste0("(^|[^a-z])", syn_lower), col_lower)) {
                  return(list(column = col, score = 80, source = "synonym_partial"))
                }
              }
            }
          }
        }

        # Fallback: try matching dest column name directly against source columns
        # First try literal exact match (character-for-character, case-sensitive)
        for (col in all_cols) {
          if (identical(col, dest_col)) {
            return(list(column = col, score = 100, source = "literal_exact"))
          }
        }

        # Then try whitespace-trimmed exact match (case-sensitive)
        dest_trimmed <- trimws(dest_col)
        for (col in all_cols) {
          if (trimws(col) == dest_trimmed) {
            return(list(column = col, score = 100, source = "trimmed_exact"))
          }
        }

        # Then try fully normalized match (case-insensitive, whitespace-trimmed)
        dest_lower <- tolower(dest_trimmed)
        for (col in all_cols) {
          if (tolower(trimws(col)) == dest_lower) {
            return(list(column = col, score = 100, source = "normalized_match"))
          }
        }
        return(NULL)
      }

      # Helper: get synonym hints for a destination column (now uses global function)

      # Helper: get full metadata for a destination column
      get_dest_metadata <- function(dest_col) {
        # Use find_std_field_for_column to map dest column name -> standard field
        std_field <- find_std_field_for_column(dest_col)
        if (!is.null(std_field)) {
          for (gn in names(logic_target_groups)) {
            if (std_field %in% names(logic_target_groups[[gn]])) {
              return(logic_target_groups[[gn]][[std_field]])
            }
          }
        }
        return(NULL)
      }

      # Apply search filter first
      filtered_dest_headers <- if (nzchar(search_query)) {
        Filter(function(h) grepl(search_query, tolower(h), fixed = TRUE), dest_headers)
      } else {
        dest_headers
      }

      # Apply class filter for destination mode (checkboxes send array of selected classes)
      class_filter <- input$mapping_class_filter
      # Default to c("1", "2", "3") if NULL (matches default checked state)
      if (is.null(class_filter) || length(class_filter) == 0) {
        class_filter <- c("1", "2", "3")
      }
      classifications <- rv$column_classifications
      overrides <- rv$classification_overrides

      # Helper to get effective class for a source column
      get_source_class <- function(col_name) {
        if (!is.null(overrides[[col_name]])) {
          return(overrides[[col_name]])
        }
        if (!is.null(classifications)) {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
            return(classifications$class[idx[1]])
          }
        }
        return(NA)
      }

      # Filter destination columns based on their mapped source column's class
      # Only filter if not all options selected (1, 2, 3, missing)
      show_missing <- "missing" %in% class_filter
      selected_classes <- as.integer(class_filter[class_filter %in% c("1", "2", "3")])

      # PERFORMANCE OPTIMIZATION: Pre-compute lookups once before filter/render loop
      # 1. Build source class lookup hash map (O(1) access instead of O(n) linear search)
      source_class_lookup <- new.env(hash = TRUE, parent = emptyenv())
      classifications <- rv$column_classifications
      if (!is.null(classifications) && nrow(classifications) > 0) {
        for (i in seq_len(nrow(classifications))) {
          col_name <- classifications$column_name[i]
          col_class <- classifications$class[i]
          if (!is.null(col_name) && !is.na(col_name) && nzchar(col_name) && !is.na(col_class)) {
            source_class_lookup[[col_name]] <- col_class
          }
        }
      }

      # Fast class lookup function using the pre-built hash
      fast_get_class <- function(col_name) {
        if (is.null(col_name) || !nzchar(col_name)) return(NA)
        cls <- source_class_lookup[[col_name]]
        if (is.null(cls)) NA else cls
      }

      # 2. Pre-compute suggestions for all destination columns
      dest_suggestion_lookup <- new.env(hash = TRUE, parent = emptyenv())
      for (dest_col in filtered_dest_headers) {
        if (nzchar(dest_col)) {
          dest_suggestion_lookup[[dest_col]] <- suggest_for_dest(dest_col)
        }
      }

      # 3. Get tracker once (not per-column)
      tracker <- value_source_tracker()

      filtered_dest_headers <- Filter(function(dest_col) {
        # Priority order for determining source column:
        # 1. value_source_tracker (profile-applied mappings - highest priority)
        # 2. input[[input_id]] (user selections in UI)
        # 3. suggest_for_dest (auto-suggestions - lowest priority)

        source_col <- NULL

        # 1. Check pre-fetched tracker (profile-applied mappings have priority)
        if (!is.null(tracker[[dest_col]]) && !is.null(tracker[[dest_col]]$value)) {
          tracked_val <- tracker[[dest_col]]$value
          if (length(tracked_val) > 0 && nzchar(tracked_val[1])) {
            source_col <- tracked_val[1]
          }
        }

        # 2. Check current UI selection (overrides tracker if user changed it)
        input_id <- paste0("dest_", make.names(dest_col), "_selector")
        selected <- isolate(input[[input_id]])
        if (!is.null(selected) && length(selected) > 0 && nzchar(selected[1])) {
          source_col <- selected[1]
        }

        # 3. Fall back to pre-computed suggestion
        if (is.null(source_col) || !nzchar(source_col)) {
          suggestion_info <- dest_suggestion_lookup[[dest_col]]
          if (!is.null(suggestion_info) && is.list(suggestion_info)) {
            source_col <- suggestion_info$column
          }
        }

        # No source column = "No Match" (unless auto-derived from decimal baths)
        if (is.null(source_col) || !nzchar(source_col)) {
          # Check if this dest field will be auto-derived from decimal baths
          std_fld <- find_std_field_for_column(dest_col)
          if (!is.null(std_fld) && std_fld %in% c("BA", "PB", "BathroomsTotalInteger") &&
              !is.null(current_dataset())) {
            for (sc in names(current_dataset())) {
              if (is_decimal_baths(current_dataset()[[sc]])) {
                return(TRUE)  # Always show auto-derived bath fields
              }
            }
          }
          return(show_missing)
        }

        # Get the source column's class using fast hash lookup
        source_class <- fast_get_class(source_col)

        # Class 4 columns (empty/PII) = treat as "No Match" (no valid mapping available)
        if (!is.na(source_class) && source_class == 4) {
          return(show_missing)
        }

        # If no classes selected, show nothing
        if (length(selected_classes) == 0 && !show_missing) {
          return(FALSE)
        }

        # Check if source class is in selected classes
        if (is.na(source_class)) {
          # Unclassified - show if "No Match" is checked (treat as unknown)
          return(show_missing)
        }

        return(source_class %in% selected_classes)
      }, filtered_dest_headers)

      group_panels <- list(wellPanel(
        tags$h5("Destination Columns"),
        tags$div(
          style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(450px,1fr)); gap:15px;",
          lapply(filtered_dest_headers, function(dest_col) {
            input_id <- paste0("dest_", make.names(dest_col), "_selector")

            # Get metadata to check if this field supports multiple columns
            meta <- get_dest_metadata(dest_col)
            is_multi <- !is.null(meta) && isTRUE(meta$multiple)

            # Fallback: treat "Address" fields as multi-column by default
            message(sprintf("DEST MODE PROCESSING: dest_col='%s', is_multi=%s, meta=%s", dest_col, is_multi, !is.null(meta)))
            if (!is_multi && grepl("address", tolower(dest_col))) {
              is_multi <- TRUE
              message(sprintf("DEST MODE: Address field detected, forcing is_multi=TRUE for %s", dest_col))
            }

            # Use pre-computed suggestion for this destination column
            suggestion_info <- dest_suggestion_lookup[[dest_col]]
            if (!is.null(suggestion_info) && is.list(suggestion_info)) {
              suggested <- suggestion_info$column %||% ""
              suggestion_score <- suggestion_info$score %||% 0
              value_source <- suggestion_info$source %||% "suggestion"
            } else {
              suggested <- ""
              suggestion_score <- 0
              value_source <- "none"
            }

            # Check if value came from profile (use tracked_sources from parent scope)
            profile_source <- tracked_sources[[dest_col]]
            came_from_profile <- !is.null(profile_source) && identical(profile_source$source, "profile")

            # Get exclusions early to prevent re-applying excluded values
            dest_col_exclusions <- current_exclusions

            # Check for existing selection - prioritize tracker over input (timing issue fix)
            # But skip excluded values to prevent infinite reactive loops
            if (came_from_profile && !is.null(profile_source$value) &&
                !any(normalize_selection_values(profile_source$value) %in% dest_col_exclusions)) {
              # Use value from tracker (profile was loaded)
              selected_val <- profile_source$value
            } else {
              # Fall back to input or suggestion
              fallback_val <- isolate(input[[input_id]]) %||% suggested
              # Don't use excluded values
              if (length(dest_col_exclusions) > 0 && length(fallback_val) > 0 &&
                  all(normalize_selection_values(fallback_val) %in% dest_col_exclusions)) {
                selected_val <- ""
              } else {
                selected_val <- fallback_val
              }
            }

            has_selection <- !is.null(selected_val) && length(selected_val) > 0 && nzchar(selected_val[1])
            selected_primary <- if (has_selection) as.character(selected_val)[1] else ""

            # Get the source column's class for Class 3 review logic
            # Use get_source_class (not fast_get_class) to ensure proper lookup
            source_col_class <- if (nzchar(selected_primary)) {
              get_source_class(selected_primary)
            } else if (!is.null(suggested) && length(suggested) > 0 && nzchar(suggested[1])) {
              get_source_class(suggested)
            } else {
              NA
            }
            is_class3_source <- !is.na(source_col_class) && source_col_class == 3

            # Get review state for Class 3 cards
            # Use make.names() for consistent key matching with button handlers
            review_states <- class3_review_states()
            safe_dest_key <- make.names(dest_col)
            review_state <- review_states[[safe_dest_key]] %||% "pending"

            # Override value_source if selection is an exact match with destination column
            # This fixes issue where cached suggestions don't have source field
            if (has_selection && !came_from_profile) {
              # Check for literal exact match (character-for-character)
              if (identical(selected_primary, dest_col)) {
                value_source <- "literal_exact"
              } else {
                # Check for normalized match (ignores case and special characters)
                normalized_sel <- tolower(gsub("[^[:alnum:]]", "", selected_primary))
                normalized_dest <- tolower(gsub("[^[:alnum:]]", "", dest_col))
                if (nzchar(normalized_sel) && identical(normalized_sel, normalized_dest)) {
                  value_source <- "normalized_match"
                }
              }
            }

            # Get synonyms for this destination column
            synonyms <- get_dest_synonyms(dest_col)
            message(sprintf("[CARD-RENDER] dest_col='%s' -> synonyms count: %d", dest_col, length(synonyms)))

            # Pre-resolve decimal baths auto-derive for dest mode BA/PB
            std_field_for_badge <- find_std_field_for_column(dest_col)
            is_baths_derived_dest <- FALSE
            baths_src_dest <- NULL
            if (!is.null(std_field_for_badge) &&
                std_field_for_badge %in% c("BA", "PB") &&
                !is.null(current_dataset())) {
              for (src_col_name in names(current_dataset())) {
                col_vals <- current_dataset()[[src_col_name]]
                if (is_decimal_baths(col_vals)) {
                  baths_src_dest <- src_col_name
                  is_baths_derived_dest <- TRUE
                  break
                }
              }
              message(sprintf("[BATHS-DERIVE-DEST] %s (std=%s): found decimal baths source = '%s'",
                      dest_col, std_field_for_badge,
                      if (is.null(baths_src_dest)) "NONE" else baths_src_dest))
            }

            # Status badge (dark mode compatible colors)
            # Class 3 sources get special handling based on review state
            status <- if (is_class3_source && review_state == "skipped") {
              # Skipped Class 3
              span("SKIPPED", class = "skipped-badge")
            } else if (is_class3_source && review_state == "pending" && (has_selection || nzchar(suggested))) {
              # Pending Class 3 review - needs user action
              span("REVIEW", class = "review-badge")
            } else if (has_selection) {
              # Normal selection states (Class 1, 2, or accepted Class 3)
              if (came_from_profile) {
                span(sprintf("Mapped: %s", selected_primary), class = "pill ok",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
              } else if (value_source == "literal_exact") {
                span(sprintf("Exact: %s", selected_primary), class = "pill ok",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
              } else if (value_source == "normalized_match") {
                span(sprintf("Match: %s", selected_primary), class = "pill match",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#1565c0; color:#fff; border:1px solid #42a5f5;")
              } else if (!is.null(suggestion_info) && nzchar(suggested)) {
                span(sprintf("Suggested: %s (score %d)", selected_primary, round(suggestion_score)), class = "pill warn",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#ff9800; color:#fff; border:1px solid #ffa726;")
              } else {
                span(sprintf("Mapped: %s", selected_primary), class = "pill ok",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
              }
            } else {
              if (is_baths_derived_dest) {
                derive_label <- if (isTRUE(baths_split_accepted())) "Auto-Derived" else "Auto-Derived (pending)"
                derive_bg <- if (isTRUE(baths_split_accepted())) "#1565c0" else "#7b1fa2"
                derive_border <- if (isTRUE(baths_split_accepted())) "#42a5f5" else "#ab47bc"
                span(derive_label, class = "pill ok",
                     style = sprintf("display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:%s; color:#fff; border:1px solid %s;", derive_bg, derive_border),
                     title = sprintf("Will be derived from decimal baths split of '%s'", baths_src_dest))
              } else if (!is.null(std_field_for_badge) && identical(std_field_for_badge, "SqFtTotal") && isTRUE(sqft_derive_accepted())) {
                span("Auto-Derived", class = "pill ok",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#1565c0; color:#fff; border:1px solid #42a5f5;",
                     title = "Will be derived from AboveGradeFinishedArea + BelowGradeFinishedArea")
              } else {
                span("Missing", class = "pill bad",
                     style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#d32f2f; color:#fff; border:1px solid #f44336;")
              }
            }

            # Check if this destination column is a derivable metric
            # Show address-style clickable suggestions for applicable source columns
            derived_metric_ui <- NULL
            derived_info <- get_derived_metric_for_dest(dest_col, current_selections)
            if (isTRUE(derived_info$is_derivable) && derived_info$metric_name == "SaleQtr") {
              # Get source columns from the uploaded CSV
              source_cols <- if (!is.null(current_dataset())) names(current_dataset()) else character(0)

              # Date column patterns to look for
              date_patterns <- c(
                "closedate", "closingdate", "dateclose", "dateclosed",
                "solddate", "datesold", "saledate", "salesdate",
                "contractdate", "closeddate", "sellingdate"
              )

              # Find date columns in source data that could be used for calculation
              found_date_cols <- character(0)
              for (col in source_cols) {
                col_norm <- tolower(gsub("[^[:alnum:]]", "", col))
                if (any(sapply(date_patterns, function(p) grepl(p, col_norm) || col_norm == p))) {
                  found_date_cols <- c(found_date_cols, col)
                }
              }

              # Check if user already selected a date column
              if (has_selection && nzchar(selected_primary)) {
                # User has selected a column - show confirmation
                derived_metric_ui <- tags$div(
                  style = "margin-top: 8px; padding: 8px 10px; background: #e8f5e9; border-radius: 6px; border-left: 3px solid #4caf50;",
                  tags$div(
                    style = "font-size: 12px; color: #2e7d32;",
                    icon("check-circle"), " ",
                    tags$strong("Will calculate ", derived_info$label),
                    tags$span(style = "margin-left: 4px;", sprintf("from: %s", selected_primary))
                  ),
                  tags$small(style = "display:block; color:#558b2f; margin-top:4px;",
                             derived_info$description),
                  tags$small(style = "display:block; color:#666; margin-top:2px; font-style:italic;",
                             "Preview will show calculated values")
                )
              } else if (length(found_date_cols) > 0) {
                # No selection yet - show clickable date column suggestions
                safe_dest <- make.names(dest_col)
                suggestion_buttons <- lapply(found_date_cols, function(col) {
                  actionLink(
                    inputId = paste0("add_derived_dest_", safe_dest, "_", make.names(col)),
                    label = col,
                    style = "display: inline-block; margin: 2px 4px 2px 0; padding: 2px 8px; background: #e3f2fd; border-radius: 12px; font-size: 11px; color: #1565c0; text-decoration: none; cursor: pointer;",
                    title = sprintf("Use %s to calculate %s", col, dest_col)
                  )
                })

                derived_metric_ui <- tags$div(
                  style = "margin-top: 8px; padding: 8px 10px; background: #fff3e0; border-radius: 6px; border-left: 3px solid #ff9800;",
                  tags$div(
                    style = "font-size: 12px; color: #e65100; margin-bottom: 6px;",
                    icon("calculator"), " ",
                    tags$strong(derived_info$label),
                    tags$span(style = "margin-left: 4px; color: #666;", "- Can be calculated from a date column")
                  ),
                  tags$div(
                    style = "font-size: 11px; color: #666; margin-bottom: 4px;",
                    icon("lightbulb"), " ",
                    tags$strong("Click to use:"),
                    " (detected date columns)"
                  ),
                  tags$div(suggestion_buttons),
                  tags$small(style = "display:block; color:#666; margin-top:6px;",
                             derived_info$description)
                )
              } else {
                # No date columns found - show info message
                derived_metric_ui <- tags$div(
                  style = "margin-top: 8px; padding: 8px 10px; background: #ffebee; border-radius: 6px; border-left: 3px solid #f44336;",
                  tags$div(
                    style = "font-size: 12px; color: #c62828;",
                    icon("calculator"), " ",
                    tags$strong(derived_info$label),
                    tags$span(style = "margin-left: 4px;", "- No date columns detected")
                  ),
                  tags$small(style = "display:block; color:#666; margin-top:4px;",
                             sprintf("Map a date column (e.g., %s) to calculate", paste(head(derived_info$missing_fields, 3), collapse=", ")))
                )
              }
            }

            # Check if selected source column is empty or mostly empty
            empty_warning <- NULL
            if (has_selection && !is.null(current_dataset())) {
              src_col <- selected_primary
              if (src_col %in% names(current_dataset())) {
                col_values <- current_dataset()[[src_col]]
                non_empty <- sum(!is.na(col_values) & nzchar(trimws(as.character(col_values))))
                total <- length(col_values)
                empty_pct <- 1 - (non_empty / total)

                if (non_empty == 0) {
                  empty_warning <- tags$div(
                    style = "margin-top: 4px; padding: 4px 8px; background: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                    tags$span(style = "color: #856404; font-size: 11px;",
                              icon("exclamation-triangle"), " Column is completely empty")
                  )
                } else if (empty_pct > 0.9) {
                  empty_warning <- tags$div(
                    style = "margin-top: 4px; padding: 4px 8px; background: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                    tags$span(style = "color: #856404; font-size: 11px;",
                              icon("exclamation-triangle"), sprintf(" Column is %d%% empty", round(empty_pct * 100)))
                  )
                }
              }
            }

            current_exclusions <- field_exclusions()[[dest_col]] %||% character(0)
            num_excluded <- length(current_exclusions)

            # Safety net: clear excluded selections locally (do NOT modify
            # value_source_tracker here — it causes infinite reactive loops)
            if (has_selection && length(current_exclusions) > 0) {
              if (selected_primary %in% current_exclusions) {
                old_selection <- selected_primary
                selected_val <- if (is_multi) character(0) else ""
                has_selection <- FALSE
                selected_primary <- ""
                message(sprintf("[EXCLUSION-CLEAR] Cleared excluded selection '%s' from '%s' (dest mode)",
                                old_selection, dest_col))
              } else if (is_multi && length(selected_val) > 0) {
                # For multi-select, filter out any excluded columns
                filtered_val <- setdiff(selected_val, current_exclusions)
                if (length(filtered_val) < length(selected_val)) {
                  removed <- setdiff(selected_val, filtered_val)
                  selected_val <- filtered_val
                  has_selection <- length(filtered_val) > 0
                  selected_primary <- if (has_selection) as.character(filtered_val)[1] else ""
                  message(sprintf("[EXCLUSION-CLEAR] Removed %d excluded cols from '%s': %s",
                                  length(removed), dest_col, paste(removed, collapse=", ")))
                }
              }
            }

            # Dropdown
            suggested_cols <- if (!is.null(suggested) && length(suggested) > 0 && nzchar(suggested[1])) c(suggested) else character(0)
            other_cols <- sort(setdiff(all_cols, suggested_cols))

            selectize_input <- selectizeInput(
              inputId = input_id,
              label = "Your column",
              choices = c("None" = "", suggested_cols, other_cols),
              selected = selected_val,
              multiple = is_multi,
              options = {
                opts <- list(
                  placeholder = "Choose from your columns…",
                  maxOptions = 1000,
                  maxItems = if (is_multi) NULL else 1
                )
                if (is_multi) {
                  opts$plugins <- c("drag_drop", "remove_button")
                }
                opts
              }
            )

            # Sum-combine preview (dest mode) — uses reactive uiOutput
            # keyed by dest column name to avoid re-rendering entire dest UI
            sum_combine_ui_dest <- if (!is.null(meta) && isTRUE(meta$combine == "sum") && is_multi) {
              uiOutput(paste0("sum_preview_dest_", make.names(dest_col)))
            } else {
              NULL
            }

            # SqFtTotal auto-derive UI (dest mode)
            # SqFtTotal auto-derive (dest mode) — uses the same reactive uiOutput
            # as standard mode since the panel logic is shared
            sqft_derive_ui_dest <- if (!is.null(meta) && !is.null(meta$derive_option) &&
                                       isTRUE(meta$derive_option$enabled) &&
                                       identical(meta$derive_option$transform, "sum_grade_areas")) {
              uiOutput("sqft_derive_panel_dest")
            } else {
              NULL
            }

            # Separator input (for multi-column fields like Address)
            separator_input <- if (is_multi && !isTRUE(meta$combine == "sum")) {
              div(
                style = "margin-top: 6px;",
                tagList(
                  textInput(
                    inputId = paste0("sep_dest_", make.names(dest_col)),
                    label = "Separator",
                    value = isolate(input[[paste0("sep_dest_", make.names(dest_col))]]) %||% " ",
                    width = "120px",
                    placeholder = "Default: space (e.g., ', ' or ' - ')"
                  ),
                  tags$small("Combines multiple columns with this separator", style = "display:block; color:#999; margin-top:-6px;")
                )
              )
            } else {
              NULL
            }

            # Address component auto-suggest UI (destination mode)
            # Always show for Address fields - helps user discover related columns
            address_suggest_ui_dest <- if (is_multi && grepl("address", tolower(dest_col))) {
              message(sprintf("ADDRESS AUTO-SUGGEST: dest_col=%s, has_selection=%s, is_multi=%s", dest_col, has_selection, is_multi))
              # Common address component patterns - use exact match or regex patterns
              # Format: list of (exact_matches, regex_pattern)
              # exact_matches: normalized column names that match exactly
              # regex_pattern: regex to match column names (anchored to avoid false positives)
              address_patterns <- list(
                StreetNumber = list(
                  exact = c("streetnumber", "streetnumeric", "streetnumbernumeric", "streetno", "houseno", "housenumber", "addressnumber"),
                  regex = "^street(number|no|num)"
                ),
                StreetName = list(
                  exact = c("streetname", "street"),
                  regex = "^streetname$"
                ),
                StreetDirPrefix = list(
                  exact = c("streetdirprefix", "streetdirectionprefix", "dirprefix"),
                  regex = "^street.*dir.*pre"
                ),
                StreetDirSuffix = list(
                  exact = c("streetdirsuffix", "streetdirectionsuffix", "dirsuffix"),
                  regex = "^street.*dir.*suf"
                ),
                StreetSuffix = list(
                  exact = c("streetsuffix", "streettype", "streetsuffixtype"),
                  regex = "^streetsuffix$|^streettype$"
                ),
                StreetSuffixModifier = list(
                  exact = c("streetsuffixmodifier", "streetsuffixmod", "suffixmodifier"),
                  regex = "^streetsuffix(modifier|mod)$"
                ),
                UnitNumber = list(
                  exact = c("unitnumber", "unit", "aptnumber", "apt", "suite", "suitenumber", "apartmentnumber"),
                  regex = "^(unit|apt|suite|apartment)(number|no|num)?$"
                ),
                City = list(
                  exact = c("city"),
                  regex = "^city$"
                ),
                StateOrProvince = list(
                  exact = c("stateorprovince", "state", "province", "stateprovince"),
                  regex = "^state(orprovince)?$|^province$"
                ),
                PostalCode = list(
                  exact = c("postalcode", "zipcode", "zip"),
                  regex = "^(postal|zip)code$|^zip$"
                ),
                PostalCodePlus4 = list(
                  exact = c("postalcodeplus4", "zipplus4", "zip4"),
                  regex = "^(postal|zip)(code)?plus4$|^zip4$"
                )
              )

              # Get source columns from the uploaded CSV
              source_cols <- if (!is.null(current_dataset())) names(current_dataset()) else character(0)

              # Find unmapped address columns in source data
              selected_cols <- normalize_selection_values(selected_val)
              selected_lower <- tolower(gsub("[^[:alnum:]]", "", selected_cols))

              suggested_additions <- list()
              for (pattern_name in names(address_patterns)) {
                pattern_info <- address_patterns[[pattern_name]]
                exact_matches <- pattern_info$exact
                regex_pattern <- pattern_info$regex

                for (col in source_cols) {
                  col_norm <- tolower(gsub("[^[:alnum:]]", "", col))
                  # Skip if already selected
                  if (col_norm %in% selected_lower) next

                  # Check exact match first, then regex
                  is_match <- (col_norm %in% exact_matches) ||
                              grepl(regex_pattern, col_norm, perl = TRUE)

                  if (is_match) {
                    suggested_additions[[pattern_name]] <- col
                    break
                  }
                }
              }

              message(sprintf("ADDRESS AUTO-SUGGEST: found %d suggestions: %s", length(suggested_additions), paste(names(suggested_additions), collapse=", ")))
              if (length(suggested_additions) > 0) {
                suggestion_buttons <- lapply(names(suggested_additions), function(name) {
                  col <- suggested_additions[[name]]
                  safe_dest <- make.names(dest_col)
                  actionLink(
                    inputId = paste0("add_addr_dest_", safe_dest, "_", make.names(col)),
                    label = col,
                    style = "display: inline-block; margin: 2px 4px 2px 0; padding: 2px 8px; background: #e3f2fd; border-radius: 12px; font-size: 11px; color: #1565c0; text-decoration: none;",
                    title = sprintf("Add %s to %s mapping", col, dest_col)
                  )
                })

                tags$div(
                  style = "margin-top: 8px; padding: 8px; background: #f5f5f5; border-radius: 6px; border-left: 3px solid #2196F3;",
                  tags$div(
                    style = "font-size: 11px; color: #666; margin-bottom: 4px;",
                    icon("lightbulb"), " ",
                    tags$strong("Suggested additions:"),
                    " (click to add)"
                  ),
                  tags$div(suggestion_buttons)
                )
              } else {
                NULL
              }
            } else {
              NULL
            }

            preview_button <- actionButton(
              inputId = paste0("preview_dest_", make.names(dest_col)),
              label = icon("eye"),
              class = "btn btn-sm btn-info preview-btn",
              title = "Preview selected column data",
              style = if (!has_selection) "padding:4px 8px; pointer-events:none; opacity:0.45;" else "padding:4px 8px;"
            )
            if (!has_selection) {
              preview_button$attribs$disabled <- "disabled"
            }

            # Detail text with synonyms
            edit_id <- paste0("edit_synonyms_", make.names(dest_col))
            
            detail_text <- if (length(synonyms) > 0) {
              tags$div(
                style = "color:#666; margin:4px 0; font-size: 85%;",
                tags$span("Also known as:", paste(head(synonyms, 5), collapse = ", ")),
                actionLink(
                    inputId = edit_id,
                    label = icon("pencil"),
                    style = "margin-left: 5px; color: #999;",
                    title = "Edit synonyms"
                )
              )
            } else {
              tags$div(
                style = "color:#666; margin:4px 0; font-size: 85%;",
                tags$span("Destination column - will appear in final output"),
                actionLink(
                    inputId = edit_id,
                    label = icon("pencil"),
                    style = "margin-left: 5px; color: #999;",
                    title = "Add synonyms"
                )
              )
            }

            # ID field tip for ListingKey/UniqueID in destination mode
            id_field_tip_dest <- if (!is.null(std_field_for_badge) &&
                std_field_for_badge %in% c("ListingKey", "UniqueID")) {
              tags$div(
                style = "font-size: 11px; color: #1565c0; margin: 4px 0 6px 0; padding: 4px 8px; background: #e3f2fd; border-radius: 4px;",
                icon("info-circle"),
                " CValR uses MLS# as its unique identifier. If you only have one ID column, map it to both MLS# (ListingId) and this field."
              )
            } else NULL

            # Class 3 review UI - shown only for pending Class 3 cards
            class3_review_ui <- if (is_class3_source && review_state == "pending" && (has_selection || nzchar(suggested))) {
              source_col_display <- if (nzchar(selected_primary)) selected_primary else suggested
              safe_dest <- make.names(dest_col)
              tags$div(
                style = "margin-top: 10px; padding: 10px; background: #fff3e0; border-radius: 6px; border-left: 3px solid #f0ad4e;",
                tags$div(
                  style = "margin-bottom: 8px; font-size: 12px;",
                  tags$strong("Suggested: "), source_col_display,
                  tags$span(" (Class 3 - Marginal)", style = "color: #f0ad4e;")
                ),
                tags$div(
                  style = "margin-bottom: 8px;",
                  tags$strong("Accept mapping as:", style = "font-size: 12px;"),
                  radioButtons(
                    inputId = paste0("class3_choice_", safe_dest),
                    label = NULL,
                    choices = c(
                      "Keep Class 3 (Marginal)" = "3",
                      "Upgrade to Class 2 (Supplemental)" = "2",
                      "Upgrade to Class 1 (Essential)" = "1"
                    ),
                    selected = "3",
                    inline = FALSE
                  )
                ),
                tags$div(
                  style = "display: flex; gap: 8px;",
                  actionButton(
                    inputId = paste0("accept_class3_", safe_dest),
                    label = "Accept",
                    class = "btn-success btn-sm"
                  ),
                  actionButton(
                    inputId = paste0("skip_class3_", safe_dest),
                    label = "Skip",
                    class = "btn-default btn-sm"
                  )
                )
              )
            } else if (is_class3_source && review_state == "skipped") {
              # Skipped state - show reconsider link
              safe_dest <- make.names(dest_col)
              source_col_display <- if (!is.null(suggested) && length(suggested) > 0 && nzchar(suggested[1])) suggested else "(none)"
              tags$div(
                style = "margin-top: 10px; padding: 8px; background: #f5f5f5; border-radius: 6px; color: #666;",
                tags$span("Skipped suggestion: ", source_col_display),
                actionLink(
                  inputId = paste0("reconsider_class3_", safe_dest),
                  label = "Reconsider",
                  style = "margin-left: 10px;"
                )
              )
            } else {
              NULL
            }

            exclusion_info_dest <- if (num_excluded > 0) {
              tags$div(
                style = "margin-top: 6px; padding: 6px; background-color: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                tags$small(
                  style = "color: #856404; display: flex; align-items: center; justify-content: space-between;",
                  tags$span(
                    icon("filter"),
                    sprintf(" %d column%s excluded", num_excluded, if(num_excluded > 1) "s" else "")
                  ),
                  actionLink(
                    inputId = paste0("manage_exclusions_dest_", make.names(dest_col)),
                    label = "Review/Restore",
                    style = "font-size: 11px; color: #856404; text-decoration: underline;"
                  )
                )
              )
            } else {
              NULL
            }

            exclude_button_dest <- actionButton(
              inputId = paste0("exclude_dest_", make.names(dest_col)),
              label = if (num_excluded > 0)
                sprintf("None of These Match (%d hidden)", num_excluded)
              else
                "None of These Match",
              class = "btn btn-sm btn-warning",
              icon = icon("ban"),
              title = paste(
                "Hide all currently available columns for this destination field.",
                "Use when none of today's headers are a match."
              ),
              style = paste0(
                "margin-top: 6px; width: 100%;",
                if (!dataset_loaded_dest) " pointer-events:none; opacity:0.5;" else ""
              )
            )

            exclude_helper_dest <- tags$small(
              style = "display:block; color:#666; margin-top:4px;",
              "Tip: This hides every column currently shown in the dropdown. ",
              "They can be restored later via Review/Restore if needed."
            )

            # Decimal baths split option for destination mode bath fields (BA/PB)
            is_bath_field_dest <- !is.null(std_field_for_badge) && std_field_for_badge %in% c("BA", "PB")
            baths_split_ui_dest <- if (is_bath_field_dest &&
                                       has_selection && !is.null(current_dataset())) {
              src_col <- selected_primary
              if (nzchar(src_col) && src_col %in% names(current_dataset())) {
                col_values <- current_dataset()[[src_col]]
                if (is_decimal_baths(col_values)) {
                  # Get sample values for preview
                  num_vals <- suppressWarnings(as.numeric(col_values))
                  sample_vals <- head(num_vals[!is.na(num_vals) & num_vals != floor(num_vals)], 3)

                  # Generate preview text
                  preview_examples <- if (length(sample_vals) > 0) {
                    sapply(sample_vals, function(v) {
                      full <- floor(v)
                      half <- round((v - full) * 10)
                      total <- full + half
                      sprintf("%.1f → %d full, %d half (%d total)", v, full, half, total)
                    })
                  } else {
                    c("2.2 → 2 full, 2 half (4 total)")
                  }

                  is_accepted <- isTRUE(baths_split_accepted())

                  # Find actual dest column names for bath fields
                  bath_dest_names <- list(ba = NULL, pb = NULL)
                  for (dh in rv$dest_headers) {
                    sf <- find_std_field_for_column(dh)
                    if (!is.null(sf)) {
                      if (sf == "BA") bath_dest_names$ba <- dh
                      else if (sf == "PB") bath_dest_names$pb <- dh
                    }
                  }
                  ba_label <- bath_dest_names$ba %||% "BA"
                  pb_label <- bath_dest_names$pb %||% "PB"

                  preview_rows_dest <- if (length(sample_vals) > 0) {
                    lapply(sample_vals, function(v) {
                      full <- floor(v)
                      half <- round((v - full) * 10)
                      tags$tr(
                        tags$td(style = "padding: 2px 8px 2px 0; font-weight: 600;", sprintf("%.1f", v)),
                        tags$td(style = "padding: 2px 8px; color: #999;", "\u2192"),
                        tags$td(style = "padding: 2px 8px; color: #2e7d32; font-weight: 600;", sprintf("%d", full)),
                        tags$td(style = "padding: 2px 8px; color: #999;", "full"),
                        tags$td(style = "padding: 2px 8px; color: #1565c0; font-weight: 600;", sprintf("%d", half)),
                        tags$td(style = "padding: 2px 8px; color: #999;", "half")
                      )
                    })
                  } else { list() }

                  div(
                    style = "margin-top: 8px; padding: 10px; background: #e8f5e9; border-radius: 6px; border-left: 3px solid #4caf50;",
                    tags$div(
                      style = "font-weight: 600; color: #2e7d32; margin-bottom: 6px;",
                      icon("check-circle"), " Decimal Bath System Detected"
                    ),
                    tags$p(
                      style = "font-size: 12px; color: #444; margin: 4px 0;",
                      sprintf("Integer = full baths (%s), decimal digit = half bath count (%s).", ba_label, pb_label)
                    ),
                    if (length(preview_rows_dest) > 0) {
                      tags$div(
                        style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
                        tags$table(style = "border-collapse: collapse; font-size: 12px;",
                          tags$tr(style = "color: #888; font-size: 10px; font-weight: 600;",
                            tags$td(style = "padding: 2px 8px 2px 0;", "Source"),
                            tags$td(style = "padding: 2px 8px;", ""),
                            tags$td(style = "padding: 2px 8px; color: #2e7d32;", ba_label),
                            tags$td(style = "padding: 2px 8px;", ""),
                            tags$td(style = "padding: 2px 8px; color: #1565c0;", pb_label),
                            tags$td(style = "padding: 2px 8px;", "")
                          ),
                          tagList(preview_rows_dest)
                        )
                      )
                    },
                    if (is_accepted) {
                      tags$div(
                        style = "color: #2e7d32; font-size: 12px;",
                        icon("check"), " Bath conversion will be applied on export",
                        actionLink(
                          inputId = "baths_split_undo",
                          label = "(undo)",
                          style = "margin-left: 8px; font-size: 11px; color: #666;"
                        )
                      )
                    } else {
                      actionButton(
                        inputId = "baths_split_accept",
                        label = tagList(icon("columns"), " Accept Bath Conversion"),
                        class = "btn-sm btn-success",
                        style = "width: 100%;"
                      )
                    }
                  )
                } else {
                  NULL
                }
              } else {
                NULL
              }
            } else {
              NULL
            }

            # Auto-derive indicator for dest mode BA/PB when decimal baths found
            baths_derive_ui_dest <- NULL
            if (is_baths_derived_dest && !has_selection) {
              std_field_derive <- std_field_for_badge
              portion_label <- switch(std_field_derive,
                "BA" = "Full Baths (integer part)",
                "PB" = "Half Baths (decimal digit)",
                "Unknown"
              )

              col_values <- current_dataset()[[baths_src_dest]]
              num_vals <- suppressWarnings(as.numeric(col_values))
              # Pick unique decimal values for diverse preview
              decimal_vals <- num_vals[!is.na(num_vals) & num_vals != floor(num_vals)]
              sample_vals <- head(unique(decimal_vals), 3)
              # Show preview for each sample
              preview_rows <- if (length(sample_vals) > 0) {
                lapply(sample_vals, function(v) {
                  full <- floor(v)
                  half <- round((v - full) * 10)
                  if (std_field_derive == "PB") {
                    tags$div(style = "margin: 2px 0;",
                      tags$span(sprintf("%.1f", v), style = "font-weight: 600; color: #333;"),
                      tags$span(" → ", style = "color: #999;"),
                      tags$span(sprintf("%d half bath%s", half, if (half != 1) "s" else ""),
                        style = "font-weight:700; color:#2e7d32;"))
                  } else {
                    tags$div(style = "margin: 2px 0;",
                      tags$span(sprintf("%.1f", v), style = "font-weight: 600; color: #333;"),
                      tags$span(" → ", style = "color: #999;"),
                      tags$span(sprintf("%d full bath%s", full, if (full != 1) "s" else ""),
                        style = "font-weight:700; color:#2e7d32;"))
                  }
                })
              } else { list() }

              is_accepted <- isTRUE(baths_split_accepted())

              # Preview header label varies by card type
              preview_header <- switch(std_field_derive,
                "PB" = "Half baths extracted:",
                "How values split:"
              )

              baths_derive_ui_dest <- div(
                style = "margin-top: 8px; padding: 10px; background: #e8f5e9; border-radius: 6px; border-left: 3px solid #4caf50;",
                tags$div(
                  style = "font-weight: 600; color: #2e7d32; margin-bottom: 6px;",
                  icon("link"), " Auto-Derived from Decimal Baths"
                ),
                tags$p(
                  style = "font-size: 12px; color: #444; margin: 4px 0;",
                  sprintf("Source column \"%s\" has decimal baths. This card gets the %s.", baths_src_dest, tolower(portion_label))
                ),
                if (length(preview_rows) > 0) {
                  tags$div(
                    style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
                    tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", preview_header),
                    tagList(preview_rows)
                  )
                },
                if (is_accepted) {
                  tags$div(
                    style = "color: #2e7d32; font-size: 12px;",
                    icon("check"), " Will be populated on export"
                  )
                } else {
                  actionButton(
                    inputId = "baths_split_accept",
                    label = tagList(icon("columns"), " Accept Bath Split"),
                    class = "btn-sm btn-success",
                    style = "width: 100%;"
                  )
                }
              )
            }

            div(
              class = "card mapping-card",
              style = "border:1px solid #ddd; border-radius:8px; padding:10px;",
              tags$div(
                style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin-bottom:6px;",
                tags$div(tags$strong(dest_col), " ", status),
                preview_button
              ),
              detail_text,
              id_field_tip_dest,
              selectize_input,
              separator_input,
              sum_combine_ui_dest,
              baths_split_ui_dest,
              baths_derive_ui_dest,
              sqft_derive_ui_dest,
              address_suggest_ui_dest,
              exclusion_info_dest,
              exclude_button_dest,
              exclude_helper_dest,
              class3_review_ui,
              derived_metric_ui,
              empty_warning
            )
          })
        )
      ))
    } else {
      # Standard mode: use CValR standard fields as targets

      # Get class filter settings (shared with destination mode)
      class_filter <- input$mapping_class_filter
      if (is.null(class_filter) || length(class_filter) == 0) {
        class_filter <- c("1", "2", "3")
      }
      classifications <- rv$column_classifications
      overrides <- rv$classification_overrides
      show_missing <- "missing" %in% class_filter
      selected_classes <- as.integer(class_filter[class_filter %in% c("1", "2", "3")])

      # Helper to get effective class for a source column
      get_source_class_cvalr <- function(col_name) {
        if (!is.null(overrides[[col_name]])) {
          return(overrides[[col_name]])
        }
        if (!is.null(classifications)) {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
            return(classifications$class[idx[1]])
          }
        }
        return(NA)
      }

      group_panels <- lapply(names(logic_target_groups), function(gn) {
        grp <- logic_target_groups[[gn]]
        target_names <- names(grp)

        # Apply search filter
        if (nzchar(search_query)) {
          target_names <- Filter(function(tgt) {
            meta <- grp[[tgt]]
          search_terms <- c(
            tolower(tgt),
            tolower(meta$label %||% ""),
            tolower(meta$type %||% ""),
            tolower(meta$synonyms %||% "")
          )
          any(grepl(search_query, search_terms, fixed = TRUE))
        }, target_names)
      }

      # Apply class filter based on suggested/selected source column
      target_names <- Filter(function(tgt) {
        input_id <- id_for(tgt)

        # Check current selection
        current_sel <- isolate(input[[input_id]])
        source_col <- if (!is.null(current_sel) && length(current_sel) > 0 && nzchar(current_sel[1])) {
          current_sel[1]
        } else {
          # Check suggestion cache
          cache <- suggestion_cache()
          suggestion_info <- cache[[tgt]]
          if (!is.null(suggestion_info) && is.list(suggestion_info) && !is.null(suggestion_info$column)) {
            suggestion_info$column
          } else {
            NULL
          }
        }

        # No source column = "Missing" (unless auto-derived from decimal baths)
        if (is.null(source_col) || !nzchar(source_col)) {
          # Check if this field will be auto-derived from decimal baths split
          if (tgt == "PB" && !is.null(current_dataset())) {
            # PB can be auto-derived if BA has decimal baths mapped
            ba_src_filter <- isolate(input[[id_for("BA")]])
            if (is.null(ba_src_filter) || !nzchar(ba_src_filter)) {
              ba_profile <- loaded_mapping_profile()[["BA"]]
              if (!is.null(ba_profile) && length(ba_profile) == 1 && nzchar(ba_profile)) {
                ba_src_filter <- ba_profile
              }
            }
            if (is.null(ba_src_filter) || !nzchar(ba_src_filter)) {
              ba_cache <- cache[["BA"]]
              if (!is.null(ba_cache) && is.list(ba_cache) &&
                  !is.null(ba_cache$column) && nzchar(ba_cache$column) &&
                  (ba_cache$score %||% 0) >= SUGGEST_THRESHOLD) {
                ba_src_filter <- ba_cache$column
              }
            }
            if (!is.null(ba_src_filter) && length(ba_src_filter) == 1 &&
                ba_src_filter %in% names(current_dataset()) &&
                is_decimal_baths(current_dataset()[[ba_src_filter]])) {
              # PB auto-derived from BA's decimal baths split - always show
              return(TRUE)
            }
          }
          return(show_missing)
        }

        # Get the source column's class
        source_class <- get_source_class_cvalr(source_col)

        # Class 4 columns (empty/PII) = treat as "Missing" (no valid mapping available)
        # This ensures targets pointing to empty/excluded columns still show when "Missing" is checked
        if (!is.na(source_class) && source_class == 4) {
          return(show_missing)
        }

        # If no classes selected, show nothing
        if (length(selected_classes) == 0 && !show_missing) {
          return(FALSE)
        }

        if (is.na(source_class)) {
          # Unclassified - show if "missing" is checked
          return(show_missing)
        }

        return(source_class %in% selected_classes)
      }, target_names)

      if (!length(target_names)) {
        return(NULL)
      }

      wellPanel(
        tags$h5(if (dest_mode) sprintf("%s → Destination", gn) else gn),
        tags$div(
          style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(450px,1fr)); gap:15px;",
          lapply(target_names, function(tgt) {
              meta <- grp[[tgt]]
              input_id <- id_for(tgt)

              display_label <- if (dest_mode && tgt %in% names(logic_to_dest)) {
                logic_to_dest[[tgt]]
              } else {
                meta$label
              }
              if (is.null(display_label) || !nzchar(display_label)) {
                display_label <- meta$label
              }

              # Check if this is a multi-select field
              is_multi <- isTRUE(meta$multiple) || identical(tgt, "Address")
              empty_selection <- if (is_multi) character(0) else ""

              # Get suggested column from cache (already calculated globally)
              cache <- suggestion_cache()
              suggestion_info <- cache[[tgt]]

              if (!is.null(suggestion_info) && is.list(suggestion_info)) {
                suggested <- suggestion_info$column %||% empty_selection
                suggestion_score <- suggestion_info$score %||% 0
              } else {
                suggested <- empty_selection
                suggestion_score <- 0
              }
              dataset_loaded <- !is.null(current_dataset())

              if (is_multi && length(suggested) == 1 && identical(suggested, "")) {
                suggested <- character(0)
              }

              # Get current selection from input (use isolate to prevent re-rendering loop)
              current_sel <- isolate(input[[input_id]])
              loaded_profile <- loaded_mapping_profile()
              profile_values <- loaded_profile[[tgt]] %||% empty_selection

              # Get exclusions early so we can check before applying profile/suggestion values
              tgt_exclusions <- field_exclusions()[[tgt]] %||% character(0)

              selected_val <- empty_selection
              value_source <- "none"

              has_current_selection <- length(normalize_selection_values(current_sel)) > 0

              # Filter out excluded columns from current selection
              if (has_current_selection && length(tgt_exclusions) > 0) {
                current_norm <- normalize_selection_values(current_sel)
                if (all(current_norm %in% tgt_exclusions)) {
                  has_current_selection <- FALSE
                }
              }

              if (has_current_selection) {
                selected_val <- current_sel
              } else if (isTRUE(success_status[[tgt]]) &&
                         length(normalize_selection_values(profile_values)) > 0 &&
                         all(normalize_selection_values(profile_values) %in% all_cols) &&
                         !any(normalize_selection_values(profile_values) %in% tgt_exclusions)) {
                selected_val <- profile_values
              } else if (suggestion_score >= SUGGEST_THRESHOLD &&
                         length(normalize_selection_values(suggested)) > 0 &&
                         !any(normalize_selection_values(suggested) %in% tgt_exclusions)) {
                selected_val <- suggested
              } else {
                selected_val <- empty_selection
              }

              selected_norm <- normalize_selection_values(selected_val)
              has_selection <- length(selected_norm) > 0

              selected_primary <- if (has_selection) as.character(selected_val)[1] else ""

              # Get the source column's class for Class 3 review logic
              suggested_primary <- if (!is.null(suggested) && length(suggested) > 0 && nzchar(suggested[1])) as.character(suggested)[1] else ""
              source_col_class_cvalr <- if (nzchar(selected_primary)) {
                get_source_class_cvalr(selected_primary)
              } else if (nzchar(suggested_primary)) {
                get_source_class_cvalr(suggested_primary)
              } else {
                NA
              }
              is_class3_source_cvalr <- !is.na(source_col_class_cvalr) && source_col_class_cvalr == 3

              # Get review state for Class 3 cards (use tgt as key)
              # Use make.names() for consistent key matching with button handlers
              review_states_cvalr <- class3_review_states()
              safe_tgt_key <- make.names(tgt)
              review_state_cvalr <- review_states_cvalr[[safe_tgt_key]] %||% "pending"

              # Check for literal exact match (character-for-character, case-sensitive)
              is_literal_exact <- !is_multi && nzchar(selected_primary) && nzchar(tgt) &&
                                  identical(selected_primary, tgt)

              # Check for normalized exact match (ignores case and special characters)
              normalized_selected <- tolower(gsub("[^[:alnum:]]", "", selected_primary))
              normalized_target <- tolower(gsub("[^[:alnum:]]", "", tgt))
              is_normalized_match <- !is_multi && nzchar(normalized_selected) && nzchar(normalized_target) &&
                                     identical(normalized_selected, normalized_target)

              # Check if selected matches target via synonyms
              is_synonym_match <- FALSE
              if (!is_multi && nzchar(selected_primary) && length(meta$synonyms) > 0) {
                selected_norm_syn <- tolower(gsub("[^[:alnum:]]", "", selected_primary))
                syn_norm <- tolower(gsub("[^[:alnum:]]", "", meta$synonyms))
                is_synonym_match <- selected_norm_syn %in% syn_norm
              }

              suggestion_norm <- normalize_selection_values(suggested)

              if (has_selection) {
                profile_norm <- normalize_selection_values(profile_values)
                # Priority order: profile > literal_exact > synonym_match > normalized_match > suggestion > manual
                if (isTRUE(success_status[[tgt]]) && length(profile_norm) > 0 &&
                    selections_match(selected_val, profile_values)) {
                  value_source <- "profile"
                } else if (is_literal_exact) {
                  value_source <- "literal_exact"
                } else if (is_synonym_match) {
                  value_source <- "literal_exact"  # Treat synonym match as exact
                } else if (is_normalized_match) {
                  value_source <- "normalized_match"
                } else if (length(suggestion_norm) > 0 &&
                           identical(selected_norm, suggestion_norm) &&
                           suggestion_score >= SUGGEST_THRESHOLD) {
                  value_source <- "suggestion"
                } else {
                  value_source <- "manual"
                }
                set_value_source(tgt, value_source, selected_val)

                # Debug: Log badge determination for first few fields
                if (tgt %in% c("MLS", "Address", "PriceListed", "Beds", "BA")) {
                  message(sprintf("  [BADGE] %s: selected='%s', value_source='%s', score=%d",
                                tgt, selected_primary, value_source, suggestion_score))
                }
              } else {
                clear_value_source(tgt)

                # Debug: Log missing selection
                if (tgt %in% c("MLS", "Address", "PriceListed", "Beds", "BA")) {
                  message(sprintf("  [BADGE] %s: NO SELECTION (suggested='%s', score=%d)",
                                tgt, if(is_multi && length(suggested)>0) suggested[1] else suggested, suggestion_score))
                }
              }

              # Build choices with badges
              # For each column, determine if it's mapped, suggested, or unmapped
              choices_with_info <- setNames(all_cols, all_cols)

              # Determine status for this field based on selected_val
              is_selected <- has_selection

              # Define selected_str before the conditional branches (used later for empty warnings)
              selected_str <- if (has_selection) selected_primary else NULL

              # Pre-resolve: check if BA has decimal baths for PB auto-derive
              is_baths_derived <- FALSE
              baths_src_resolved <- NULL
              if (tgt == "PB" && !is.null(current_dataset())) {
                # Check BA's mapped source for decimal baths
                baths_src_resolved <- isolate(input[[id_for("BA")]])
                if (is.null(baths_src_resolved) || !nzchar(baths_src_resolved)) {
                  ba_profile <- loaded_mapping_profile()[["BA"]]
                  if (!is.null(ba_profile) && length(ba_profile) == 1 && nzchar(ba_profile)) {
                    baths_src_resolved <- ba_profile
                  }
                }
                if (is.null(baths_src_resolved) || !nzchar(baths_src_resolved)) {
                  ba_cache <- cache[["BA"]]
                  if (!is.null(ba_cache) && is.list(ba_cache) &&
                      !is.null(ba_cache$column) && nzchar(ba_cache$column) &&
                      (ba_cache$score %||% 0) >= SUGGEST_THRESHOLD) {
                    baths_src_resolved <- ba_cache$column
                  }
                }
                if (!is.null(baths_src_resolved) && length(baths_src_resolved) == 1 &&
                    baths_src_resolved %in% names(current_dataset())) {
                  is_baths_derived <- is_decimal_baths(current_dataset()[[baths_src_resolved]])
                }
                message(sprintf("[BATHS-DERIVE] PB auto-derive: BA source='%s', is_decimal=%s",
                        if (is.null(baths_src_resolved)) "NULL" else baths_src_resolved, is_baths_derived))
              }

              # Determine status badge color and text based on value_source
              # Class 3 sources get special handling based on review state
              status <- if (is_class3_source_cvalr && review_state_cvalr == "skipped") {
                # Skipped Class 3
                span("SKIPPED", class = "skipped-badge")
              } else if (is_class3_source_cvalr && review_state_cvalr == "pending" && (has_selection || nzchar(suggested_primary))) {
                # Pending Class 3 review - needs user action
                span("REVIEW", class = "review-badge")
              } else if (is_selected) {
                if (is_multi) {
                  label_text <- switch(
                    value_source,
                    profile = sprintf("Profile (%d fields)", length(selected_val)),
                    literal_exact = sprintf("Exact (%d fields)", length(selected_val)),
                    normalized_match = sprintf("Match (%d fields)", length(selected_val)),
                    suggestion = sprintf("Suggested (%d fields)", length(selected_val)),
                    manual = sprintf("Manual (%d fields)", length(selected_val)),
                    sprintf("Merging %d fields", length(selected_val))
                  )
                  pill_style <- if (identical(value_source, "suggestion")) {
                    "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#ff9800; color:#fff; border:1px solid #ffa726;"
                  } else if (identical(value_source, "normalized_match")) {
                    "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#1565c0; color:#fff; border:1px solid #42a5f5;"
                  } else {
                    "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;"
                  }
                  span(label_text, class = "pill", style = pill_style)
                } else {
                  selected_str <- selected_primary

                  duplicate_count <- sum(vapply(current_selections, function(sel) {
                    if (is.null(sel) || !length(sel)) {
                      FALSE
                    } else if (length(sel) > 1) {
                      any(sel == selected_str)
                    } else {
                      identical(sel[[1]], selected_str)
                    }
                  }, logical(1)))
                  duplicate_hint <- duplicate_count > 1

                  if (value_source == "profile") {
                    badge <- sprintf("Mapped: %s", selected_str)
                    if (duplicate_hint) {
                      span(paste0(badge, " ⚠ DUPLICATE"), class = "pill ok",
                           style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#ff9800; color:#fff; border:1px solid #ffa726;",
                           title = sprintf("Warning: '%s' is mapped to %d fields", selected_str, duplicate_count))
                    } else {
                      span(badge, class = "pill ok",
                           style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
                    }
                  } else if (value_source == "manual") {
                    badge <- sprintf("Manual: %s", selected_str)
                    if (duplicate_hint) {
                      span(paste0(badge, " ⚠ DUPLICATE"), class = "pill ok",
                           style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#ff9800; color:#fff; border:1px solid #ffa726;",
                           title = sprintf("Warning: '%s' is mapped to %d fields", selected_str, duplicate_count))
                    } else {
                      span(badge, class = "pill ok",
                           style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
                    }
                  } else if (value_source == "literal_exact") {
                    span(sprintf("Exact: %s", selected_str), class = "pill ok",
                         style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
                  } else if (value_source == "normalized_match") {
                    span(sprintf("Match: %s", selected_str), class = "pill match",
                         style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#1565c0; color:#fff; border:1px solid #42a5f5;")
                  } else if (value_source == "suggestion") {
                    label <- sprintf("Suggested: %s", selected_str)
                    if (!is.null(suggestion_info) && !is.null(suggestion_info$score)) {
                      label <- sprintf("Suggested: %s (score %d)", selected_str, round(suggestion_info$score))
                    }
                    span(label, class = "pill warn",
                         style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#ff9800; color:#fff; border:1px solid #ffa726;")
                  } else {
                    span(sprintf("Mapped: %s", selected_str), class = "pill ok",
                         style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#2e7d32; color:#fff; border:1px solid #4caf50;")
                  }
                }
              } else {
                if (is_baths_derived) {
                  # Auto-derived from decimal baths - show blue/green indicator instead of red Missing
                  derive_label <- if (isTRUE(baths_split_accepted())) "Auto-Derived" else "Auto-Derived (pending)"
                  derive_bg <- if (isTRUE(baths_split_accepted())) "#1565c0" else "#7b1fa2"
                  derive_border <- if (isTRUE(baths_split_accepted())) "#42a5f5" else "#ab47bc"
                  span(derive_label, class = "pill ok",
                       style = sprintf("display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:%s; color:#fff; border:1px solid %s;", derive_bg, derive_border),
                       title = sprintf("Will be derived from decimal baths split of '%s'", baths_src_resolved))
                } else if (identical(tgt, "SqFtTotal") && isTRUE(sqft_derive_accepted())) {
                  span("Auto-Derived", class = "pill ok",
                       style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#1565c0; color:#fff; border:1px solid #42a5f5;",
                       title = "Will be derived from AboveGradeFinishedArea + BelowGradeFinishedArea")
                } else if (length(meta$synonyms) > 0) {
                  # No mapping (red) - show helpful hint about what we're looking for
                  hint_text <- paste("Looking for:", paste(head(meta$synonyms, 2), collapse=" or "))
                  span("Missing", class = "pill bad",
                       style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#d32f2f; color:#fff; border:1px solid #f44336;",
                       title = hint_text)
                } else {
                  span("Missing", class = "pill bad",
                       style = "display:inline-block; padding:2px 6px; border-radius:10px; font-size:11px; margin-left:6px; font-weight:bold; background:#d32f2f; color:#fff; border:1px solid #f44336;")
                }
              }

              # Debug: Log what status was created
              if (tgt %in% c("MLS", "Address", "PriceListed", "Beds", "BA")) {
                message(sprintf("  [STATUS] %s: class(status)='%s', is.list=%s, is_selected=%s",
                              tgt, class(status)[1], is.list(status), is_selected))
              }

              # Build the JavaScript render function with substituted values
                mapped_cols_str <- paste0("|", paste(mapped_columns, collapse = "|"), "|")
                suggested_str <- ""
                if (!is.null(suggested) && length(suggested) > 0) {
                  cand <- as.character(suggested)[1]
                  if (!is.na(cand) && nzchar(cand)) {
                    suggested_str <- cand
                  }
                }

                current_str <- ""
                if (!is.null(selected_val) && length(selected_val) > 0) {
                  cand_cur <- as.character(selected_val)[1]
                  if (!is.na(cand_cur) && nzchar(cand_cur)) {
                    current_str <- cand_cur
                  }
                }

                js_render <- sprintf("{
                  option: function(item, escape) {
                    var suggested_col = '%s';
                    var current_sel = '%s';

                    // Dynamically get all currently mapped columns
                    var mappedCols = [];
                    var currentSelectizeId = this.$input.attr('id');

                    $('select[id^=\"map_\"]').each(function() {
                      if ($(this).attr('id') !== currentSelectizeId) {
                        var val = $(this).val();
                        if (val && val !== '') {
                          if (Array.isArray(val)) {
                            mappedCols = mappedCols.concat(val);
                          } else {
                            mappedCols.push(val);
                          }
                        }
                      }
                    });

                    var badge = '';
                    var itemValue = item.value || item.label;

                    if (itemValue && itemValue !== '') {
                      if (mappedCols.indexOf(itemValue) !== -1) {
                        badge = '<span class=\"mapped-badge\">MAPPED</span>';
                      } else if (itemValue === suggested_col) {
                        badge = '<span class=\"suggested-badge\">★ SUGGESTED</span>';
                      }
                    }

                    return '<div>' + escape(item.label) + ' ' + badge + '</div>';
                  }
                }", suggested_str, current_str)

                card_header <- tagList(tags$strong(display_label))
                if (dest_mode) {
                  card_header <- tagList(
                    card_header,
                    tags$span(
                      style = "display:block; font-size:11px; color:#888;",
                      sprintf("Logic field: %s (%s)", meta$label, tgt)
                    )
                  )
                } else {
                  card_header <- tagList(card_header, tags$span(paste0(" (", tgt, ") ")))
                }
                if (isTRUE(meta$required)) {
                  card_header <- tagList(card_header, tags$span("•", style = "color:#d9534f; margin-left:4px;"))
                }
                card_header <- tagList(card_header, " ", status)

                selectize_input <- selectizeInput(
                  inputId = input_id,
                  label = "Your column",
                  choices = {
                    suggested_cols <- character(0)
                    if (!is.null(suggested) && length(suggested) > 0) {
                      suggested_cols <- trimws(as.character(suggested))
                      suggested_cols <- suggested_cols[!is.na(suggested_cols) & nzchar(suggested_cols)]
                      if (length(suggested_cols) > 1) {
                        suggested_cols <- unique(suggested_cols)
                      }
                    }
                    # Filter out excluded columns for this target field
                    excluded_for_target <- field_exclusions()[[tgt]] %||% character(0)
                    if (length(excluded_for_target) > 0) {
                      message(sprintf("[DROPDOWN] %s: excluding [%s] from %d total cols",
                                      tgt, paste(excluded_for_target, collapse=", "), length(all_cols)))
                    }
                    available_cols <- setdiff(all_cols, excluded_for_target)
                    other_cols <- sort(setdiff(available_cols, suggested_cols))
                    c("None" = "", suggested_cols, other_cols)
                  },
                  selected = selected_val,
                  multiple = is_multi,
                  options = {
                    opts <- list(
                      placeholder = "Choose from your columns…",
                      maxOptions = 1000,
                      maxItems = if (is_multi) NULL else 1,
                      dropdownParent = "body",
                      render = I(js_render)
                    )
                    if (is_multi) {
                      opts$plugins <- c("drag_drop", "remove_button")
                    }
                    opts
                  }
                )

                address_sep <- if (identical(tgt, "Address")) {
                  div(
                    style = "margin-top: 6px;",
                    tagList(
                      textInput(
                        inputId = paste0("sep_", tgt),
                        label = "Separator",
                        value = isolate(input[[paste0("sep_", tgt)]]) %||% " ",
                        width = "120px",
                        placeholder = "Default: space (e.g., ', ' or ' - ')"
                      ),
                      tags$small("Default: space", style = "display:block; color:#999; margin-top:-6px;")
                    )
                  )
                } else {
                  NULL
                }

                # Sum-combine preview — uses a reactive uiOutput so it updates
                # when the selectize value changes without re-rendering the whole card
                sum_combine_ui <- if (isTRUE(meta$combine == "sum") && is_multi) {
                  uiOutput(paste0("sum_preview_", tgt))
                } else {
                  NULL
                }

                # Decimal baths split — shown on BA card when source has coded decimals (e.g. 3.2 = 3 full + 2 half)
                baths_split_ui <- if (identical(tgt, "BA") && has_selection && !is.null(current_dataset())) {
                  src_col <- selected_primary
                  if (nzchar(src_col) && src_col %in% names(current_dataset())) {
                    col_values <- current_dataset()[[src_col]]
                    if (is_decimal_baths(col_values)) {
                      num_vals <- suppressWarnings(as.numeric(col_values))
                      sample_vals <- head(num_vals[!is.na(num_vals) & num_vals != floor(num_vals)], 3)
                      preview_rows <- if (length(sample_vals) > 0) {
                        lapply(sample_vals, function(v) {
                          full <- floor(v)
                          half <- round((v - full) * 10)
                          tags$tr(
                            tags$td(style = "padding: 2px 8px 2px 0; font-weight: 600;", sprintf("%.1f", v)),
                            tags$td(style = "padding: 2px 8px; color: #999;", "\u2192"),
                            tags$td(style = "padding: 2px 8px; color: #2e7d32; font-weight: 600;", sprintf("%d", full)),
                            tags$td(style = "padding: 2px 8px; color: #999;", "full"),
                            tags$td(style = "padding: 2px 8px; color: #1565c0; font-weight: 600;", sprintf("%d", half)),
                            tags$td(style = "padding: 2px 8px; color: #999;", "half")
                          )
                        })
                      } else { list() }
                      is_accepted <- isTRUE(baths_split_accepted())
                      div(
                        style = "margin-top: 8px; padding: 10px; background: #e8f5e9; border-radius: 6px; border-left: 3px solid #4caf50;",
                        tags$div(
                          style = "font-weight: 600; color: #2e7d32; margin-bottom: 6px;",
                          icon("check-circle"), " Decimal Bath System Detected"
                        ),
                        tags$p(
                          style = "font-size: 12px; color: #444; margin: 4px 0;",
                          "Integer = full baths, decimal digit = half bath count."
                        ),
                        if (length(preview_rows) > 0) {
                          tags$div(
                            style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
                            tags$table(style = "border-collapse: collapse; font-size: 12px;",
                              tags$tr(style = "color: #888; font-size: 10px; font-weight: 600;",
                                tags$td(style = "padding: 2px 8px 2px 0;", "Source"),
                                tags$td(style = "padding: 2px 8px;", ""),
                                tags$td(style = "padding: 2px 8px; color: #2e7d32;", "BA"),
                                tags$td(style = "padding: 2px 8px;", ""),
                                tags$td(style = "padding: 2px 8px; color: #1565c0;", "PB"),
                                tags$td(style = "padding: 2px 8px;", "")
                              ),
                              tagList(preview_rows)
                            )
                          )
                        },
                        if (is_accepted) {
                          tags$div(
                            style = "color: #2e7d32; font-size: 12px;",
                            icon("check"), " Bath split will be applied on export",
                            actionLink(inputId = "baths_split_undo", label = "(undo)",
                                       style = "margin-left: 8px; font-size: 11px; color: #666;")
                          )
                        } else {
                          actionButton(inputId = "baths_split_accept",
                                       label = tagList(icon("columns"), " Accept Bath Split"),
                                       class = "btn-sm btn-success", style = "width: 100%;")
                        }
                      )
                    } else { NULL }
                  } else { NULL }
                } else { NULL }

                # PB auto-derive indicator — shown when BA has decimal baths mapped
                baths_derive_ui <- NULL
                if (tgt == "PB" && is_baths_derived && !is.null(baths_src_resolved)) {
                  col_values <- current_dataset()[[baths_src_resolved]]
                  num_vals <- suppressWarnings(as.numeric(col_values))
                  decimal_vals <- num_vals[!is.na(num_vals) & num_vals != floor(num_vals)]
                  sample_vals <- head(unique(decimal_vals), 3)
                  preview_rows <- if (length(sample_vals) > 0) {
                    lapply(sample_vals, function(v) {
                      half <- round((v - floor(v)) * 10)
                      tags$div(style = "margin: 2px 0;",
                        tags$span(sprintf("%.1f", v), style = "font-weight: 600; color: #333;"),
                        tags$span(" → ", style = "color: #999;"),
                        tags$span(sprintf("%d half bath%s", half, if (half != 1) "s" else ""),
                          style = "font-weight:700; color:#2e7d32;"))
                    })
                  } else { list() }
                  is_accepted <- isTRUE(baths_split_accepted())
                  baths_derive_ui <- div(
                    style = "margin-top: 8px; padding: 10px; background: #e8f5e9; border-radius: 6px; border-left: 3px solid #4caf50;",
                    tags$div(style = "font-weight: 600; color: #2e7d32; margin-bottom: 6px;",
                      icon("link"), " Auto-Derived from BA's Decimal Split"),
                    tags$p(style = "font-size: 12px; color: #444; margin: 4px 0;",
                      sprintf("BA source \"%s\" has decimal baths. PB gets the half bath count.", baths_src_resolved)),
                    if (length(preview_rows) > 0) {
                      tags$div(style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
                        tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", "Half baths extracted:"),
                        tagList(preview_rows))
                    },
                    if (is_accepted) {
                      tags$div(style = "color: #2e7d32; font-size: 12px;", icon("check"), " Will be populated on export")
                    } else {
                      tags$div(style = "color: #f57c00; font-size: 12px;", icon("info-circle"),
                        " Accept bath split on the BA card to enable")
                    }
                  )
                }

                # SqFtTotal auto-derive UI — uses reactive uiOutput to avoid
                # creating a dependency on AboveGrade/BelowGrade inputs inside mapping_ui
                sqft_derive_ui <- if (identical(tgt, "SqFtTotal")) {
                  uiOutput("sqft_derive_panel")
                } else {
                  NULL
                }

                # Address component auto-suggest UI
                address_suggest_ui <- if (identical(tgt, "Address") && has_selection) {
                  # Common address component patterns to look for
                  address_patterns <- list(
                    StreetDirPrefix = c("streetdirprefix", "streetdirectionprefix", "dirprefix", "streetdirpre"),
                    StreetDirSuffix = c("streetdirsuffix", "streetdirectionsuffix", "dirsuffix", "streetdirsuf"),
                    UnitNumber = c("unitnumber", "unit", "aptnumber", "apt", "suite", "suitenumber", "apartment"),
                    City = c("city", "municipality", "town"),
                    StateOrProvince = c("stateorprovince", "state", "province", "st"),
                    PostalCode = c("postalcode", "zipcode", "zip", "postal"),
                    PostalCodePlus4 = c("postalcodeplus4", "zipplus4", "zip4", "plus4")
                  )

                  # Find unmapped address columns in source data
                  selected_cols <- normalize_selection_values(selected_val)
                  selected_lower <- tolower(gsub("[^[:alnum:]]", "", selected_cols))

                  suggested_additions <- list()
                  for (pattern_name in names(address_patterns)) {
                    patterns <- address_patterns[[pattern_name]]
                    for (col in all_cols) {
                      col_norm <- tolower(gsub("[^[:alnum:]]", "", col))
                      # Check if this column matches any pattern and isn't already selected
                      if (any(sapply(patterns, function(p) grepl(p, col_norm, fixed = TRUE))) &&
                          !col_norm %in% selected_lower) {
                        suggested_additions[[pattern_name]] <- col
                        break
                      }
                    }
                  }

                  if (length(suggested_additions) > 0) {
                    suggestion_buttons <- lapply(names(suggested_additions), function(name) {
                      col <- suggested_additions[[name]]
                      actionLink(
                        inputId = paste0("add_address_", make.names(col)),
                        label = col,
                        style = "display: inline-block; margin: 2px 4px 2px 0; padding: 2px 8px; background: #e3f2fd; border-radius: 12px; font-size: 11px; color: #1565c0; text-decoration: none;",
                        title = sprintf("Add %s to Address mapping", col)
                      )
                    })

                    tags$div(
                      style = "margin-top: 8px; padding: 8px; background: #f5f5f5; border-radius: 6px; border-left: 3px solid #2196F3;",
                      tags$div(
                        style = "font-size: 11px; color: #666; margin-bottom: 4px;",
                        icon("lightbulb"), " ",
                        tags$strong("Suggested additions:"),
                        " (click to add)"
                      ),
                      tags$div(suggestion_buttons)
                    )
                  } else {
                    NULL
                  }
                } else {
                  NULL
                }

                # Check if this field has exclusions
                current_exclusions <- field_exclusions()[[tgt]] %||% character(0)
                num_excluded <- length(current_exclusions)

                # Note: excluded selections are already filtered out upstream (before
                # profile/suggestion values are applied), so we only need a safety net
                # here for multi-select edge cases. Do NOT call clear_value_source()
                # inside renderUI — it modifies value_source_tracker() which this render
                # depends on, causing an infinite reactive loop.
                if (has_selection && length(current_exclusions) > 0) {
                  if (!is_multi && selected_primary %in% current_exclusions) {
                    old_selection <- selected_primary
                    selected_val <- ""
                    has_selection <- FALSE
                    is_selected <- FALSE
                    selected_str <- NULL
                    selected_primary <- ""
                    message(sprintf("[EXCLUSION-CLEAR] Cleared excluded selection '%s' from '%s' (standard mode)",
                                    old_selection, tgt))
                  } else if (is_multi && length(selected_val) > 0) {
                    # For multi-select, filter out any excluded columns
                    filtered_val <- setdiff(selected_val, current_exclusions)
                    if (length(filtered_val) < length(selected_val)) {
                      removed <- setdiff(selected_val, filtered_val)
                      selected_val <- filtered_val
                      has_selection <- length(filtered_val) > 0
                      is_selected <- has_selection
                      selected_str <- if (has_selection) as.character(filtered_val)[1] else NULL
                      selected_primary <- if (has_selection) as.character(filtered_val)[1] else ""
                      message(sprintf("[EXCLUSION-CLEAR] Removed %d excluded cols from '%s': %s",
                                      length(removed), tgt, paste(removed, collapse=", ")))
                    }
                  }
                }

                # Check if selected source column is empty or mostly empty
                empty_warning <- NULL
                if (is_selected && !is.null(selected_str) && !is.null(current_dataset())) {
                  src_col <- selected_str
                  if (!is.null(src_col) && src_col %in% names(current_dataset())) {
                    col_values <- current_dataset()[[src_col]]
                    non_empty <- sum(!is.na(col_values) & nzchar(trimws(as.character(col_values))))
                    total <- length(col_values)
                    empty_pct <- 1 - (non_empty / total)

                    if (non_empty == 0) {
                      empty_warning <- tags$div(
                        style = "margin-top: 4px; padding: 4px 8px; background: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                        tags$span(style = "color: #856404; font-size: 11px;",
                                  icon("exclamation-triangle"), " Column is completely empty")
                      )
                    } else if (empty_pct > 0.9) {
                      empty_warning <- tags$div(
                        style = "margin-top: 4px; padding: 4px 8px; background: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                        tags$span(style = "color: #856404; font-size: 11px;",
                                  icon("exclamation-triangle"), sprintf(" Column is %d%% empty", round(empty_pct * 100)))
                      )
                    }
                  }
                }

                detail_text <- if (dest_mode) {
                  tags$small(
                    style = "display:block; color:#666; margin:4px 0;",
                    sprintf("Standard field reference: %s (%s)", meta$label, tgt)
                  )
                } else {
                  std_edit_id <- paste0("edit_synonyms_std_", make.names(tgt))
                  if (length(meta$synonyms) > 0) {
                    tags$div(
                      style = "color:#666; margin:4px 0; font-size: 85%;",
                      tags$span("Also known as:", paste(head(meta$synonyms, 5), collapse = ", ")),
                      actionLink(
                        inputId = std_edit_id,
                        label = icon("pencil"),
                        style = "margin-left: 5px; color: #999;",
                        title = "Edit synonyms"
                      )
                    )
                  } else {
                    tags$div(
                      style = "color:#666; margin:4px 0; font-size: 85%;",
                      tags$span(" "),
                      actionLink(
                        inputId = std_edit_id,
                        label = icon("pencil"),
                        style = "margin-left: 5px; color: #999;",
                        title = "Add synonyms"
                      )
                    )
                  }
                }

                # Add RESO definition if available (destination mode with RESO template)
                reso_def_ui <- NULL
                if (dest_mode) {
                  reso_def <- get_reso_definition(display_label)
                  if (!is.null(reso_def) && nzchar(reso_def)) {
                    display_def <- if (nchar(reso_def) > 150) {
                      paste0(substr(reso_def, 1, 147), "...")
                    } else {
                      reso_def
                    }
                    reso_def_ui <- tags$div(
                      style = "font-size: 11px; color: #5c6bc0; margin: 4px 0 6px 0; padding: 4px 8px; background: #e8eaf6; border-radius: 4px; font-style: italic;",
                      icon("info-circle"), " ",
                      display_def
                    )
                  }
                }

                # ID field tip for ListingKey/UniqueID in standard mode
                id_field_tip <- if (tgt %in% c("ListingKey", "UniqueID")) {
                  tags$div(
                    style = "font-size: 11px; color: #1565c0; margin: 4px 0 6px 0; padding: 4px 8px; background: #e3f2fd; border-radius: 4px;",
                    icon("info-circle"),
                    " CValR uses MLS# as its unique identifier. If you only have one ID column, map it to both MLS# (ListingId) and this field."
                  )
                } else NULL

                # Add exclusion indicator if columns are excluded
                exclusion_info <- if (num_excluded > 0) {
                  tags$div(
                    style = "margin-top: 6px; padding: 6px; background-color: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
                    tags$small(
                      style = "color: #856404; display: flex; align-items: center; justify-content: space-between;",
                      tags$span(
                        icon("filter"),
                        sprintf(" %d column%s excluded", num_excluded, if(num_excluded > 1) "s" else "")
                      ),
                      actionLink(
                        inputId = paste0("manage_exclusions_", tgt),
                        label = "Review/Restore",
                        style = "font-size: 11px; color: #856404; text-decoration: underline;"
                      )
                    )
                  )
                } else {
                  NULL
                }

                preview_button <- actionButton(
                  inputId = paste0("preview_", tgt),
                  label = icon("eye"),
                  class = "btn btn-sm btn-info preview-btn",
                  title = "Preview selected column data",
                  style = "padding: 4px 8px;"
                )
                if (!has_selection) {
                  preview_button$attribs$style <- paste0(preview_button$attribs$style %||% "", " pointer-events:none; opacity:0.45;")
                  preview_button$attribs$disabled <- "disabled"
                }

                # "None of These Match" button - always visible so users know the option exists
                exclude_button <- actionButton(
                  inputId = paste0("exclude_", tgt),
                  label = if (num_excluded > 0)
                    sprintf("None of These Match (%d hidden)", num_excluded)
                  else
                    "None of These Match",
                  class = "btn btn-sm btn-warning",
                  icon = icon("ban"),
                  title = paste(
                    "Hide all currently available columns for this field.",
                    "Use when none of today's headers are a match."
                  ),
                  style = paste0(
                    "margin-top: 6px; width: 100%;",
                    if (!dataset_loaded) " pointer-events:none; opacity:0.5;" else ""
                  )
                )

                exclude_helper <- tags$small(
                  style = "display:block; color:#666; margin-top:4px;",
                  "Tip: This hides every column currently shown in the dropdown. ",
                  "They can be restored later via Review/Restore if needed."
                )

                # Class 3 review UI - shown only for pending Class 3 cards (CValR mode)
                class3_review_ui_cvalr <- if (is_class3_source_cvalr && review_state_cvalr == "pending" && (has_selection || nzchar(suggested_primary))) {
                  source_col_display <- if (nzchar(selected_primary)) selected_primary else suggested_primary
                  safe_tgt <- make.names(tgt)
                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background: #fff3e0; border-radius: 6px; border-left: 3px solid #f0ad4e;",
                    tags$div(
                      style = "margin-bottom: 8px; font-size: 12px;",
                      tags$strong("Suggested: "), source_col_display,
                      tags$span(" (Class 3 - Marginal)", style = "color: #f0ad4e;")
                    ),
                    tags$div(
                      style = "margin-bottom: 8px;",
                      tags$strong("Accept mapping as:", style = "font-size: 12px;"),
                      radioButtons(
                        inputId = paste0("class3_choice_", safe_tgt),
                        label = NULL,
                        choices = c(
                          "Keep Class 3 (Marginal)" = "3",
                          "Upgrade to Class 2 (Supplemental)" = "2",
                          "Upgrade to Class 1 (Essential)" = "1"
                        ),
                        selected = "3",
                        inline = FALSE
                      )
                    ),
                    tags$div(
                      style = "display: flex; gap: 8px;",
                      actionButton(
                        inputId = paste0("accept_class3_", safe_tgt),
                        label = "Accept",
                        class = "btn-success btn-sm"
                      ),
                      actionButton(
                        inputId = paste0("skip_class3_", safe_tgt),
                        label = "Skip",
                        class = "btn-default btn-sm"
                      )
                    )
                  )
                } else if (is_class3_source_cvalr && review_state_cvalr == "skipped") {
                  # Skipped state - show reconsider link
                  safe_tgt <- make.names(tgt)
                  source_col_display <- if (nzchar(suggested_primary)) suggested_primary else "(none)"
                  tags$div(
                    style = "margin-top: 10px; padding: 8px; background: #f5f5f5; border-radius: 6px; color: #666;",
                    tags$span("Skipped suggestion: ", source_col_display),
                    actionLink(
                      inputId = paste0("reconsider_class3_", safe_tgt),
                      label = "Reconsider",
                      style = "margin-left: 10px;"
                    )
                  )
                } else {
                  NULL
                }

                div(
                  class = "card mapping-card",
                  style = "border:1px solid #ddd; border-radius:8px; padding:10px;",
                  tags$div(
                    style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin-bottom:6px;",
                    tags$div(card_header),
                    preview_button
                  ),
                  detail_text,
                  reso_def_ui,
                  id_field_tip,
                  exclusion_info,
                  tagList(
                    selectize_input,
                    address_sep,
                    sum_combine_ui,
                    baths_split_ui,
                    baths_derive_ui,
                    sqft_derive_ui,
                    address_suggest_ui,
                    exclude_button,
                    exclude_helper
                  ),
                  class3_review_ui_cvalr,
                  empty_warning
                )
              })
            )
          )
        })

      group_panels <- Filter(Negate(is.null), group_panels)

      if (!length(group_panels)) {
        # Show helpful message with info about active filters
        search_active <- nzchar(search_query)
        class_filter <- input$mapping_class_filter
        if (is.null(class_filter) || length(class_filter) == 0) {
          class_filter <- c("1", "2", "3")
        }

        hint_message <- if (search_active) {
          tagList(
            p(sprintf("Search term '%s' found no results.", search_query)),
            actionButton("clear_mapping_search", "Clear Search",
                        class = "btn-default btn-sm",
                        style = "margin-top: 10px;")
          )
        } else {
          p("Try selecting different class filters or use the search box.")
        }

        group_panels <- list(
          div(
            style = "padding: 30px; text-align: center; color: #666;",
            h4("No matching fields"),
            hint_message
          )
        )
      }
    }  # Close else block

    header_text <- if (dest_mode) {
      HTML("Select which of <b>your columns</b> feed each destination column. Required fields marked ",
           tags$span("•", style = "color:#d9534f;"), ".")
    } else {
      HTML("Pick which of <b>your columns</b> maps to each field. Required fields marked ",
           tags$span("•", style = "color:#d9534f;"), ".")
    }

    tagList(header_text, group_panels)
  })

  # Reactive value to control when to show unmapped columns
  show_unmapped_section <- reactiveVal(FALSE)

  # UI for unmapped columns section (separate renderUI)
  output$unmapped_columns_ui <- renderUI({
    # Only show if button was clicked
    if (!show_unmapped_section()) {
      return(NULL)
    }

    req(current_dataset())
    all_cols <- all_columns()

    # Exclude Class 4 columns from unmapped section - they are excluded from export
    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides
    if (!is.null(classifications)) {
      class4_cols <- sapply(all_cols, function(col_name) {
        if (!is.null(overrides[[col_name]])) {
          return(overrides[[col_name]] == 4)
        }
        idx <- which(classifications$column_name == col_name)
        if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
          return(classifications$class[idx[1]] == 4)
        }
        FALSE
      })
      all_cols <- all_cols[!class4_cols]
    }

    # Get current mappings to determine which columns are actually mapped
    # Don't use isolate() here - we want this to update when selections change
    current_selections <- get_all_selections()
    mapped_source_cols <- unique(unlist(current_selections))

    # Find all unmapped columns (excluding Class 4 which are auto-excluded)
    unmapped_cols <- setdiff(all_cols, mapped_source_cols)

    tagList(
      hr(),
      h4("Unmapped Columns from Your Data"),
      helpText("These columns were not mapped to any target field. Choose to Include or Exclude each one."),

      {

        if (length(unmapped_cols) == 0) {
          p(style = "color: #666; font-style: italic;", "All columns have been mapped!")
        } else {
          wellPanel(
            class = "unmapped-well",
            style = "background: #f8f9fa;",
            tags$div(
              style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(350px,1fr)); gap:15px;",
              lapply(unmapped_cols, function(col_name) {
                checkbox_id <- paste0("include_", make.names(col_name))
                preview_id <- paste0("preview_unmapped_", make.names(col_name))

                div(
                  class = "card unmapped-card",
                  style = "border:1px solid #ddd; border-radius:8px; padding:10px; background: white;",
                  tags$div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                    tags$strong(col_name),
                    actionButton(
                      inputId = preview_id,
                      label = icon("eye"),
                      class = "btn-sm btn-info",
                      title = "Preview column data",
                      style = "padding: 4px 8px;"
                    )
                  ),
                  checkboxInput(
                    inputId = checkbox_id,
                    label = "Include this column in output",
                    value = TRUE  # Default to include
                  )
                )
              })
            )
          )
        }
      }
    )
  })

  # Observer for "Update Unmapped Columns" button
  observeEvent(input$update_unmapped, {
    show_unmapped_section(TRUE)
    showNotification("Unmapped columns section updated based on current mappings.", type = "message")
  })

  # Manual refresh button - force UI re-render of badges
  observeEvent(input$refresh_badges, {
    ui_refresh_trigger(ui_refresh_trigger() + 1)
    showNotification("Mapping badges refreshed successfully", type = "message", duration = 2)
  })

  # UI for unused target fields section
  # Shows CValR standard fields that weren't matched - only relevant in non-destination mode
  output$unused_target_fields_ui <- renderUI({
    # Only show if unmapped section is visible
    if (!show_unmapped_section()) {
      return(NULL)
    }

    # Hide this section in destination schema mode - CValR fields are not relevant
    # when user has their own destination schema
    dest_mode <- isTRUE(input$use_destination_schema) && length(rv$dest_headers) > 0
    if (dest_mode) {
      return(NULL)
    }

    req(current_dataset())

    # Get current mappings to determine which target fields are unused
    current_selections <- get_all_selections()
    mapped_target_fields <- names(current_selections)

    # Get all target fields from all groups
    all_target_fields <- unlist(lapply(logic_target_groups, function(grp) names(grp)))

    # Find unused target fields
    unused_targets <- setdiff(all_target_fields, mapped_target_fields)

    if (length(unused_targets) == 0) {
      return(NULL)  # Don't show section if all fields are mapped
    }

    tagList(
      hr(),
      h4("Available Target Fields Not Matched"),
      helpText("These standardized fields weren't mapped to any columns in your data. Consider updating your data exports to include these fields."),

      wellPanel(
        style = "background: #fffbf0; border: 1px solid #ffc107;",
        tags$div(
          style = "display:grid; grid-template-columns:repeat(auto-fit,minmax(300px,1fr)); gap:10px;",
          lapply(unused_targets, function(tgt) {
            # Find which group this target belongs to
            group_name <- ""
            meta <- NULL
            for (gn in names(logic_target_groups)) {
              if (tgt %in% names(logic_target_groups[[gn]])) {
                group_name <- gn
                meta <- logic_target_groups[[gn]][[tgt]]
                break
              }
            }

            if (is.null(meta)) return(NULL)

            div(
              style = "padding: 8px; background: white; border-radius: 4px; border: 1px solid #ddd;",
              tags$div(
                tags$strong(meta$label, style = "color: #856404;"),
                " (", tgt, ")",
                if (isTRUE(meta$required)) {
                  tags$span(" •", style = "color:#d9534f;", title = "Required field")
                }
              ),
              if (length(meta$synonyms) > 0) {
                tags$small(
                  style = "display:block; color:#666; margin-top: 4px;",
                  paste("Also known as:", paste(meta$synonyms, collapse = ", "))
                )
              }
            )
          })
        )
      )
    )
  })

  # Observers for preview buttons - dynamically created for each target field
  lapply(names(logic_target_groups), function(gn) {
    grp <- logic_target_groups[[gn]]
    lapply(names(grp), function(tgt) {
      meta <- grp[[tgt]]
      button_id <- paste0("preview_", tgt)
      input_id <- id_for(tgt)

      observeEvent(input[[button_id]], {
        # Get the currently selected column for this field
        selected_col <- isolate(input[[input_id]])

        if (is.null(selected_col) || length(selected_col) == 0 || !all(nzchar(selected_col))) {
          showModal(modalDialog(
            title = paste("Preview:", meta$label),
            p("No column selected yet. Please select a column from the dropdown first."),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }

        # Get the dataset
        dataset <- current_dataset()
        if (is.null(dataset) || !all(selected_col %in% names(dataset))) {
          showModal(modalDialog(
            title = paste("Preview:", meta$label),
            p(paste0("Column '", paste(selected_col, collapse = ", "), "' not found in dataset.")),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }

        # Get sample data (first 50 rows, truncate long strings)
        # Handle multiple columns (for fields like Address that can combine columns)
        if (length(selected_col) > 1) {
          # Combine multiple columns with " " separator
          values <- apply(dataset[, selected_col, drop = FALSE], 1, function(row) {
            paste(row[nzchar(trimws(row))], collapse = " ")
          })
        } else {
          values <- dataset[[selected_col]]
        }
        values_sample <- head(values, 50)

        # Convert to character and truncate
        values_display <- sapply(values_sample, function(v) {
          if (is.null(v) || is.na(v)) return("[blank]")
          v_str <- as.character(v)
          if (is.na(v_str) || !nzchar(trimws(v_str))) return("[blank]")
          if (nchar(v_str) > 200) {
            v_str <- paste0(substr(v_str, 1, 197), "...")
          }
          v_str
        }, USE.NAMES = FALSE)

        # Show modal with data
        col_display <- paste(selected_col, collapse = ", ")
        showModal(modalDialog(
          title = paste("Preview:", meta$label, "→", col_display),
          p(tags$strong(if(length(selected_col) > 1) "Columns:" else "Column:"), col_display),
          p(tags$strong("Total rows:"), nrow(dataset)),
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
      }, ignoreInit = TRUE)
    })
  })

  # Observers for unmapped column preview buttons
  observe({
    req(current_dataset())
    dataset <- current_dataset()
    all_cols <- all_columns()
    mapped_columns <- unique(unlist(isolate(get_all_selections())))
    unmapped_cols <- setdiff(all_cols, mapped_columns)

    lapply(unmapped_cols, function(col_name) {
      preview_id <- paste0("preview_unmapped_", make.names(col_name))

      observeEvent(input[[preview_id]], {
        # Get sample data
        values <- dataset[[col_name]]
        values_sample <- head(values, 50)

        # Convert to character and truncate
        values_display <- sapply(values_sample, function(v) {
          if (is.null(v) || is.na(v)) return("[blank]")
          v_str <- as.character(v)
          if (is.na(v_str) || !nzchar(trimws(v_str))) return("[blank]")
          if (nchar(v_str) > 200) {
            v_str <- paste0(substr(v_str, 1, 197), "...")
          }
          v_str
        }, USE.NAMES = FALSE)

        # Show modal with data
        showModal(modalDialog(
          title = paste("Preview Unmapped Column:", col_name),
          p(tags$strong("Column:"), col_name),
          p(tags$strong("Total rows:"), nrow(dataset)),
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
      }, ignoreInit = TRUE)
    })
  })

  # Observers for mapping card preview buttons (standard mode)
  # Get all standard field target names upfront
  all_targets <- unlist(lapply(logic_target_groups, names), use.names = FALSE)

  lapply(all_targets, function(target_name) {
    preview_id <- paste0("preview_", target_name)
    selector_id <- paste0("map_", target_name)

    observeEvent(input[[preview_id]], {
      # IMPORTANT: Require the button click value to be > 0
      # This prevents the modal from triggering on initialization
      req(input[[preview_id]] > 0)
      req(current_dataset())
      dataset <- current_dataset()

      # Get the selected source column(s) from the selector
      selected_cols <- input[[selector_id]]

      if (is.null(selected_cols) || length(selected_cols) == 0 || !any(nzchar(selected_cols))) {
        showNotification("No column selected to preview", type = "warning")
        return()
      }

      # Filter to valid columns that exist in dataset
      valid_cols <- selected_cols[selected_cols %in% names(dataset)]
      if (length(valid_cols) == 0) {
        showNotification("Selected column(s) not found in dataset", type = "error")
        return()
      }

      # Build preview content for all selected columns (in order)
      if (length(valid_cols) == 1) {
        # Single column - show simple preview
        col_to_preview <- valid_cols[1]
        values <- dataset[[col_to_preview]]
        values_sample <- head(values, 50)

        values_display <- sapply(values_sample, function(v) {
          if (is.null(v) || is.na(v)) return("[blank]")
          v_str <- as.character(v)
          if (is.na(v_str) || !nzchar(trimws(v_str))) return("[blank]")
          if (nchar(v_str) > 200) {
            v_str <- paste0(substr(v_str, 1, 197), "...")
          }
          v_str
        }, USE.NAMES = FALSE)

        showModal(modalDialog(
          title = paste("Preview Column:", col_to_preview),
          p(tags$strong("Mapping target:"), target_name),
          p(tags$strong("Source column:"), col_to_preview),
          p(tags$strong("Total rows:"), nrow(dataset)),
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
      } else {
        # Multiple columns - check combine mode from metadata
        target_meta <- NULL
        for (gn in names(logic_target_groups)) {
          if (target_name %in% names(logic_target_groups[[gn]])) {
            target_meta <- logic_target_groups[[gn]][[target_name]]
            break
          }
        }
        combine_mode <- if (!is.null(target_meta)) target_meta$combine else NULL

        n_preview <- min(50, nrow(dataset))

        if (identical(combine_mode, "sum")) {
          # Sum mode: show statistics and per-row sum calculations
          # Per-column statistics
          col_stats <- lapply(valid_cols, function(col) {
            vals <- suppressWarnings(as.numeric(dataset[[col]]))
            non_na <- vals[!is.na(vals)]
            list(
              column = col,
              min = if (length(non_na) > 0) min(non_na) else NA,
              max = if (length(non_na) > 0) max(non_na) else NA,
              mean = if (length(non_na) > 0) round(mean(non_na), 1) else NA,
              fill_rate = sprintf("%d%%", round(100 * length(non_na) / nrow(dataset)))
            )
          })

          # Sum column stats
          sum_vals <- rowSums(
            sapply(valid_cols, function(col) suppressWarnings(as.numeric(dataset[[col]]))),
            na.rm = TRUE
          )
          # Set all-NA rows to NA for stats
          all_na_mask <- apply(sapply(valid_cols, function(col) is.na(suppressWarnings(as.numeric(dataset[[col]])))), 1, all)
          sum_vals[all_na_mask] <- NA
          non_na_sums <- sum_vals[!is.na(sum_vals)]

          stats_table <- tags$table(
            style = "width: 100%; border-collapse: collapse; font-size: 12px; margin-bottom: 12px;",
            tags$thead(
              tags$tr(style = "border-bottom: 2px solid #ddd;",
                tags$th("Column", style = "text-align:left; padding:4px 8px;"),
                tags$th("Min", style = "text-align:right; padding:4px 8px;"),
                tags$th("Max", style = "text-align:right; padding:4px 8px;"),
                tags$th("Mean", style = "text-align:right; padding:4px 8px;"),
                tags$th("Fill Rate", style = "text-align:right; padding:4px 8px;")
              )
            ),
            tags$tbody(
              lapply(col_stats, function(s) {
                tags$tr(style = "border-bottom: 1px solid #eee;",
                  tags$td(s$column, style = "padding:4px 8px;"),
                  tags$td(if (is.na(s$min)) "—" else format(s$min, big.mark = ","), style = "text-align:right; padding:4px 8px;"),
                  tags$td(if (is.na(s$max)) "—" else format(s$max, big.mark = ","), style = "text-align:right; padding:4px 8px;"),
                  tags$td(if (is.na(s$mean)) "—" else format(s$mean, big.mark = ","), style = "text-align:right; padding:4px 8px;"),
                  tags$td(s$fill_rate, style = "text-align:right; padding:4px 8px;")
                )
              }),
              tags$tr(style = "border-top: 2px solid #1565c0; font-weight: bold; color: #1565c0;",
                tags$td("SUM", style = "padding:4px 8px;"),
                tags$td(if (length(non_na_sums) > 0) format(min(non_na_sums), big.mark = ",") else "—", style = "text-align:right; padding:4px 8px;"),
                tags$td(if (length(non_na_sums) > 0) format(max(non_na_sums), big.mark = ",") else "—", style = "text-align:right; padding:4px 8px;"),
                tags$td(if (length(non_na_sums) > 0) format(round(mean(non_na_sums), 1), big.mark = ",") else "—", style = "text-align:right; padding:4px 8px;"),
                tags$td(sprintf("%d%%", round(100 * length(non_na_sums) / nrow(dataset))), style = "text-align:right; padding:4px 8px;")
              )
            )
          )

          # Sample row calculations
          sample_n <- min(20, nrow(dataset))
          sample_rows <- lapply(seq_len(sample_n), function(i) {
            vals <- suppressWarnings(sapply(valid_cols, function(col) as.numeric(dataset[[col]][i])))
            total <- sum(vals, na.rm = TRUE)
            if (all(is.na(vals))) total <- NA
            parts <- paste(sapply(seq_along(valid_cols), function(j) {
              sprintf("%s", ifelse(is.na(vals[j]), "NA", format(vals[j], big.mark = ",")))
            }), collapse = " + ")
            total_str <- if (is.na(total)) "[all NA]" else format(total, big.mark = ",")
            tags$li(style = "margin: 2px 0;",
              tags$span(parts, style = "color: #555;"),
              tags$span(" = ", style = "color: #999;"),
              tags$span(total_str, style = "font-weight: 700; color: #1565c0;")
            )
          })

          showModal(modalDialog(
            title = sprintf("Preview Sum-Combine: %s", target_name),
            tags$div(
              tags$p(tags$strong("Mapping target:"), target_name),
              tags$p(tags$strong("Columns being summed:"), paste(valid_cols, collapse = " + ")),
              tags$p(tags$strong("Total rows:"), nrow(dataset)),
              tags$hr(),
              tags$h5(icon("calculator"), " Column Statistics"),
              stats_table,
              tags$h5(icon("list-ol"), " Sample Calculations (first ", sample_n, " rows)"),
              tags$div(
                class = "preview-content",
                style = "max-height: 300px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
                tags$ol(sample_rows)
              )
            ),
            easyClose = TRUE,
            footer = modalButton("Close"),
            size = "l"
          ))
        } else {
          # Concat mode: existing behavior
          combined_values <- apply(dataset[1:n_preview, valid_cols, drop = FALSE], 1, function(row) {
            parts <- sapply(row, function(v) {
              if (is.null(v) || is.na(v)) return("")
              v_str <- trimws(as.character(v))
              if (is.na(v_str)) return("")
              v_str
            })
            paste(parts[nzchar(parts)], collapse = " ")
          })

          showModal(modalDialog(
            title = sprintf("Preview Multi-Column Mapping: %s", target_name),
            tags$div(
              tags$p(tags$strong("Mapping target:"), target_name),
              tags$p(tags$strong("Selected columns (in order):"), paste(valid_cols, collapse = " → ")),
              tags$p(tags$strong("Total rows:"), nrow(dataset)),
              tags$hr(),
              tags$h5(icon("link"), " Combined/Merged Preview"),
              tags$p(style = "color: #666; font-size: 12px;",
                     "How the columns will appear when merged (space-separated). Reorder in the dropdown to change merge order."),
              tags$div(
                class = "preview-content",
                style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
                tags$ol(
                  lapply(combined_values, function(v) tags$li(if(nzchar(v)) v else "[blank]"))
                )
              )
            ),
            easyClose = TRUE,
            footer = modalButton("Close"),
            size = "l"
          ))
        }
      }
    }, ignoreInit = TRUE)
  })

  # Observers for "None of These Match" exclusion buttons (standard mode)
  lapply(all_targets, function(target_name) {
    exclude_btn_id <- paste0("exclude_", target_name)
    selector_id <- paste0("map_", target_name)

      observeEvent(input[[exclude_btn_id]], {
        req(current_dataset())

        # Get all columns currently available in the dataset
        all_cols <- names(current_dataset())

      # Get already excluded columns for this target
      current_exclusions <- field_exclusions()
      already_excluded <- current_exclusions[[target_name]] %||% character(0)

      # Get new columns to exclude (all available cols minus already excluded)
      new_exclusions <- setdiff(all_cols, already_excluded)

      if (length(new_exclusions) == 0) {
        showNotification(
          "All columns are already excluded for this field",
          type = "warning",
          duration = 3
        )
        return()
      }

        current_selection <- normalize_selection_values(isolate(input[[selector_id]]) %||% character(0))

        # Show confirmation modal before excluding
        showModal(modalDialog(
          title = sprintf("Exclude Columns from '%s'?", target_name),
          tags$div(
            tags$p(
              style = "font-size: 14px;",
            sprintf("This will exclude %d column%s from appearing in this field's dropdown:",
                    length(new_exclusions),
                    if(length(new_exclusions) > 1) "s" else "")
          ),
          tags$div(
            style = "max-height: 200px; overflow-y: auto; padding: 10px; background-color: #e9ecef; border: 1px solid #ced4da; border-radius: 4px; margin: 10px 0;",
            tags$ul(
              style = "margin: 0; padding-left: 20px; color: #212529;",
              lapply(head(new_exclusions, 20), function(col) tags$li(col))
            ),
            if (length(new_exclusions) > 20) {
              tags$p(style = "color: #666; font-style: italic;",
                     sprintf("...and %d more", length(new_exclusions) - 20))
            } else {
              NULL
            }
          ),
            tags$p(
              style = "font-size: 13px; color: #856404; background-color: #fff3cd; padding: 8px; border-radius: 4px;",
              icon("info-circle"),
              " You can restore these columns later using the 'Review/Restore' link."
            ),
            if (length(current_selection) > 0) {
              tags$p(
                style = "font-size: 12px; color: #c0392b; margin-top: 8px;",
                icon("exclamation-triangle"),
                sprintf(" Currently selected column%s (%s) will also be hidden.",
                        if (length(current_selection) > 1) "s" else "",
                        paste(current_selection, collapse = ", "))
              )
            } else NULL
          ),
          footer = tagList(
            actionButton(
            paste0("confirm_exclude_", target_name),
            "Yes, Exclude These Columns",
            class = "btn-warning"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))

      # Store context for confirmation
      rv$pending_exclusion <- list(
        target = target_name,
        columns = new_exclusions,
        already_excluded = already_excluded
      )
    }, ignoreInit = TRUE)

    # Handler for exclusion confirmation
    confirm_btn_id <- paste0("confirm_exclude_", target_name)
    observeEvent(input[[confirm_btn_id]], {
      pending <- rv$pending_exclusion
      if (is.null(pending) || pending$target != target_name) return()

      # Add new exclusions to the list
      current_exclusions <- field_exclusions()
      current_exclusions[[target_name]] <- c(pending$already_excluded, pending$columns)

      # Update the reactive
      field_exclusions(current_exclusions)

      # Trigger UI refresh to update dropdowns
      ui_refresh_trigger(ui_refresh_trigger() + 1)

      # Clear pending
      rv$pending_exclusion <- NULL

      # Close modal
      removeModal()

      # Show confirmation
      showNotification(
        sprintf("Excluded %d column(s) from '%s'",
                length(pending$columns),
                target_name),
        type = "message",
        duration = 5
      )

      message(sprintf("[EXCLUSION] User excluded %d columns from '%s'",
                      length(pending$columns), target_name))
    }, ignoreInit = TRUE)
  })

  # Observers for destination-mode exclusions (registered lazily per dest header)
  observe({
    dest_cols <- rv$dest_headers %||% character(0)
    if (!length(dest_cols)) return()

    for (dest_col in dest_cols) {
      local({
        local_col <- dest_col
        local_safe <- make.names(local_col)

        if (isTRUE(dest_exclusion_handlers[[local_safe]])) return()
        dest_exclusion_handlers[[local_safe]] <- TRUE

        exclude_id <- paste0("exclude_dest_", local_safe)
        confirm_id <- paste0("confirm_exclude_dest_", local_safe)
        selector_id <- paste0("dest_", local_safe, "_selector")
        manage_id <- paste0("manage_exclusions_dest_", local_safe)

        observeEvent(input[[exclude_id]], {
        req(current_dataset())
        all_cols <- names(current_dataset())
        current_exclusions <- field_exclusions()
        already_excluded <- current_exclusions[[local_col]] %||% character(0)
        new_exclusions <- setdiff(all_cols, already_excluded)

        if (length(new_exclusions) == 0) {
          showNotification(
            "All columns are already excluded for this field",
            type = "warning",
            duration = 3
          )
          return()
        }

        current_selection <- normalize_selection_values(isolate(input[[selector_id]]) %||% character(0))

        showModal(modalDialog(
          title = sprintf("Exclude Columns from '%s'?", local_col),
          tags$div(
            tags$p(
              style = "font-size: 14px;",
              sprintf("This will exclude %d column%s from appearing in this field's dropdown:",
                      length(new_exclusions),
                      if(length(new_exclusions) > 1) "s" else "")
            ),
            tags$div(
              style = "max-height: 200px; overflow-y: auto; padding: 10px; background-color: #e9ecef; border: 1px solid #ced4da; border-radius: 4px; margin: 10px 0;",
              tags$ul(
                style = "margin: 0; padding-left: 20px; color: #212529;",
                lapply(head(new_exclusions, 20), function(col) tags$li(col))
              ),
              if (length(new_exclusions) > 20) {
                tags$p(style = "color: #666; font-style: italic;",
                       sprintf("...and %d more", length(new_exclusions) - 20))
              } else {
                NULL
              }
            ),
            tags$p(
              style = "font-size: 13px; color: #856404; background-color: #fff3cd; padding: 8px; border-radius: 4px;",
              icon("info-circle"),
              " You can restore these columns later using the 'Review/Restore' link."
            ),
            if (length(current_selection) > 0) {
              tags$p(
                style = "font-size: 12px; color: #c0392b; margin-top: 8px;",
                icon("exclamation-triangle"),
                sprintf(" Currently selected column%s (%s) will also be hidden.",
                        if (length(current_selection) > 1) "s" else "",
                        paste(current_selection, collapse = ", "))
              )
            } else NULL
          ),
          footer = tagList(
            actionButton(
              confirm_id,
              "Yes, Exclude These Columns",
              class = "btn-warning"
            ),
            modalButton("Cancel")
          ),
          easyClose = TRUE,
          size = "m"
        ))

        rv$pending_exclusion <- list(
          target = local_col,
          columns = new_exclusions,
          already_excluded = already_excluded
        )
        }, ignoreInit = TRUE)

        observeEvent(input[[confirm_id]], {
        pending <- rv$pending_exclusion
        if (is.null(pending) || pending$target != local_col) return()

        current_exclusions <- field_exclusions()
        current_exclusions[[local_col]] <- c(pending$already_excluded, pending$columns)
        field_exclusions(current_exclusions)
        ui_refresh_trigger(ui_refresh_trigger() + 1)
        rv$pending_exclusion <- NULL
        removeModal()
        showNotification(
          sprintf("Excluded %d column(s) from '%s'",
                  length(pending$columns),
                  local_col),
          type = "message",
          duration = 5
        )

        message(sprintf("[EXCLUSION] User excluded %d columns from '%s' (destination mode)",
                        length(pending$columns), local_col))
        }, ignoreInit = TRUE)

        observeEvent(input[[manage_id]], {
        current_exclusions <- field_exclusions()
        excluded_cols <- current_exclusions[[local_col]] %||% character(0)

        if (length(excluded_cols) == 0) {
          showNotification("No exclusions for this field", type = "message")
          return()
        }

        checkbox_list <- lapply(excluded_cols, function(col) {
          tags$div(
            style = "padding: 4px 0;",
            checkboxInput(
              inputId = paste0("restore_dest_", local_safe, "_", make.names(col)),
              label = tags$span(style = "color: #212529; font-weight: normal;", col),
              value = FALSE
            )
          )
        })

        showModal(modalDialog(
          title = sprintf("Manage Exclusions for '%s'", local_col),
          tags$p(style = "color: #212529;", "These columns are currently excluded from this field's dropdown:"),
          tags$div(
            style = "max-height: 300px; overflow-y: auto; padding: 10px; background-color: #ffffff; border: 1px solid #ced4da; border-radius: 4px;",
            checkbox_list
          ),
          footer = tagList(
            actionButton("restore_selected", "Restore Selected", class = "btn-primary"),
            actionButton("restore_all", "Restore All", class = "btn-warning"),
            modalButton("Cancel")
          ),
          easyClose = TRUE,
          size = "m"
        ))

          rv$current_exclusion_context <- list(
          target = local_col,
          excluded_cols = excluded_cols,
          target_type = "dest",
          safe_id = local_safe
        )
        }, ignoreInit = TRUE)
      })
    }
  })

  # Observers for "Review/Restore" exclusion management links
  lapply(all_targets, function(target_name) {
    manage_link_id <- paste0("manage_exclusions_", target_name)

    observeEvent(input[[manage_link_id]], {
      current_exclusions <- field_exclusions()
      excluded_cols <- current_exclusions[[target_name]] %||% character(0)

      if (length(excluded_cols) == 0) {
        showNotification("No exclusions for this field", type = "message")
        return()
      }

      # Create checkboxes for each excluded column
      checkbox_list <- lapply(excluded_cols, function(col) {
        tags$div(
          style = "padding: 4px 0;",
          checkboxInput(
            inputId = paste0("restore_", target_name, "_", make.names(col)),
            label = tags$span(style = "color: #212529; font-weight: normal;", col),
            value = FALSE
          )
        )
      })

      showModal(modalDialog(
        title = sprintf("Manage Exclusions for '%s'", target_name),
        tags$p(style = "color: #212529;", "These columns are currently excluded from this field's dropdown:"),
        tags$div(
          style = "max-height: 300px; overflow-y: auto; padding: 10px; background-color: #ffffff; border: 1px solid #ced4da; border-radius: 4px;",
          checkbox_list
        ),
        footer = tagList(
          actionButton("restore_selected", "Restore Selected", class = "btn-primary"),
          actionButton("restore_all", "Restore All", class = "btn-warning"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))

      # Store context for restore buttons
      rv$current_exclusion_context <- list(
        target = target_name,
        excluded_cols = excluded_cols,
        target_type = "standard",
        safe_id = target_name
      )
    }, ignoreInit = TRUE)
  })

  # Handler for "Restore Selected" button in exclusion management modal
  observeEvent(input$restore_selected, {
    ctx <- rv$current_exclusion_context
    if (is.null(ctx)) return()

    target_name <- ctx$target
    excluded_cols <- ctx$excluded_cols
    target_type <- ctx$target_type %||% "standard"
    safe_id <- ctx$safe_id %||% target_name

    # Find which checkboxes are checked
    cols_to_restore <- character()
    for (col in excluded_cols) {
      checkbox_id <- if (identical(target_type, "dest")) {
        paste0("restore_dest_", safe_id, "_", make.names(col))
      } else {
        paste0("restore_", safe_id, "_", make.names(col))
      }
      if (isTRUE(input[[checkbox_id]])) {
        cols_to_restore <- c(cols_to_restore, col)
      }
    }

    if (length(cols_to_restore) == 0) {
      showNotification("No columns selected to restore", type = "warning")
      return()
    }

    # Remove selected columns from exclusions
    current_exclusions <- field_exclusions()
    current_exclusions[[target_name]] <- setdiff(current_exclusions[[target_name]], cols_to_restore)

    # Clean up empty exclusion lists
    if (length(current_exclusions[[target_name]]) == 0) {
      current_exclusions[[target_name]] <- NULL
    }

    field_exclusions(current_exclusions)
    ui_refresh_trigger(ui_refresh_trigger() + 1)

    removeModal()
    showNotification(
      sprintf("Restored %d column(s) to '%s'", length(cols_to_restore), target_name),
      type = "message",
      duration = 3
    )

    message(sprintf("[EXCLUSION] User restored %d columns to '%s'", length(cols_to_restore), target_name))
  }, ignoreInit = TRUE)

  # Handler for "Restore All" button in exclusion management modal
  observeEvent(input$restore_all, {
    ctx <- rv$current_exclusion_context
    if (is.null(ctx)) return()

    target_name <- ctx$target
    excluded_cols <- ctx$excluded_cols

    # Remove all exclusions for this field
    current_exclusions <- field_exclusions()
    current_exclusions[[target_name]] <- NULL

    field_exclusions(current_exclusions)
    ui_refresh_trigger(ui_refresh_trigger() + 1)

    removeModal()
    showNotification(
      sprintf("Restored all %d column(s) to '%s'", length(excluded_cols), target_name),
      type = "message",
      duration = 3
    )

    message(sprintf("[EXCLUSION] User restored all %d columns to '%s'", length(excluded_cols), target_name))
  }, ignoreInit = TRUE)

  # Observer for address component auto-suggest clicks
  # Dynamically watches for clicks on "add_address_*" buttons
  observe({
    req(current_dataset())
    all_cols <- names(current_dataset())

    # Watch for any input that starts with "add_address_"
    for (col in all_cols) {
      local({
        local_col <- col
        btn_id <- paste0("add_address_", make.names(local_col))

        observeEvent(input[[btn_id]], {
          req(input[[btn_id]] > 0)

          # Get current Address selection
          current_address_cols <- input$map_Address
          if (is.null(current_address_cols)) {
            current_address_cols <- character(0)
          }

          # Add the clicked column if not already present
          if (!local_col %in% current_address_cols) {
            new_selection <- c(current_address_cols, local_col)
            updateSelectizeInput(session, "map_Address", selected = new_selection)

            # Update value source tracker
            set_value_source("Address", "manual", new_selection)

            showNotification(
              sprintf("Added '%s' to Address mapping", local_col),
              type = "message",
              duration = 3
            )

            message(sprintf("[ADDRESS] User added suggested column '%s' to Address field", local_col))
          }
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })

  # Observer for address component auto-suggest clicks (destination mode)
  observe({
    req(current_dataset())
    req(length(rv$dest_headers) > 0)

    all_cols <- names(current_dataset())
    dest_headers <- rv$dest_headers

    # Find address-like destination columns
    address_dest_cols <- dest_headers[grepl("address", tolower(dest_headers))]

    for (dest_col in address_dest_cols) {
      safe_dest <- make.names(dest_col)
      selector_id <- paste0("dest_", safe_dest, "_selector")

      for (col in all_cols) {
        local({
          local_col <- col
          local_dest_col <- dest_col
          local_selector_id <- selector_id
          btn_id <- paste0("add_addr_dest_", safe_dest, "_", make.names(local_col))

          observeEvent(input[[btn_id]], {
            req(input[[btn_id]] > 0)

            # Get current selection for this destination column
            current_selection <- input[[local_selector_id]]
            if (is.null(current_selection)) {
              current_selection <- character(0)
            }

            # Add the clicked column if not already present
            if (!local_col %in% current_selection) {
              new_selection <- c(current_selection, local_col)
              updateSelectizeInput(session, local_selector_id, selected = new_selection)

              showNotification(
                sprintf("Added '%s' to %s mapping", local_col, local_dest_col),
                type = "message",
                duration = 3
              )

              message(sprintf("[ADDRESS-DEST] User added suggested column '%s' to '%s' field", local_col, local_dest_col))
            }
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
    }
  })

  # Observer for derived metric suggestion clicks (destination mode - e.g., SaleQtr date columns)
  observe({
    req(current_dataset())
    req(length(rv$dest_headers) > 0)

    all_cols <- names(current_dataset())
    dest_headers <- rv$dest_headers

    # Find derivable destination columns (e.g., SaleQtr)
    derivable_dest_cols <- dest_headers[tolower(gsub("[^[:alnum:]]", "", dest_headers)) %in% c("saleqtr", "salequarter")]

    for (dest_col in derivable_dest_cols) {
      safe_dest <- make.names(dest_col)
      selector_id <- paste0("dest_", safe_dest, "_selector")

      for (col in all_cols) {
        local({
          local_col <- col
          local_dest_col <- dest_col
          local_selector_id <- selector_id
          btn_id <- paste0("add_derived_dest_", safe_dest, "_", make.names(local_col))

          observeEvent(input[[btn_id]], {
            req(input[[btn_id]] > 0)

            # Set this column as the selection for the destination field
            updateSelectizeInput(session, local_selector_id, selected = local_col)

            showNotification(
              sprintf("Selected '%s' to calculate %s", local_col, local_dest_col),
              type = "message",
              duration = 3
            )

            message(sprintf("[DERIVED-METRIC] User selected '%s' to calculate '%s'", local_col, local_dest_col))
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
    }
  })

  # Observer for decimal baths split accept button
  observeEvent(input$baths_split_accept, {
    baths_split_accepted(TRUE)
    use_dest <- isTRUE(rv$use_dest_schema)
    if (use_dest && length(rv$dest_headers) > 0) {
      ba_col <- NULL; pb_col <- NULL
      for (dh in rv$dest_headers) {
        sf <- find_std_field_for_column(dh)
        if (!is.null(sf)) {
          if (sf == "BA") ba_col <- dh
          else if (sf == "PB") pb_col <- dh
        }
      }
      fields_list <- paste(c(
        if (!is.null(ba_col)) paste0("• ", ba_col, " (full baths)"),
        if (!is.null(pb_col)) paste0("• ", pb_col, " (half baths)")
      ), collapse = "<br/>")
      notif_msg <- sprintf("<strong>Bath Split Enabled</strong><br/>On export, decimal baths will be split into:<br/>%s", fields_list)
    } else {
      notif_msg <- "<strong>Bath Split Enabled</strong><br/>On export, decimal baths will be split into:<br/>• BA (full baths)<br/>• PB (half baths)"
    }
    showNotification(HTML(notif_msg), type = "message", duration = 10)
    message("[BATHS-SPLIT] User accepted decimal baths split")

    # Auto-fill PB with BA's source column now that split is accepted
    ba_source <- NULL
    if (use_dest && length(rv$dest_headers) > 0) {
      # Dest mode: find BA's current source from its input
      if (!is.null(ba_col)) {
        ba_input_id <- id_for(ba_col, dest_mode = TRUE)
        if (!is.null(ba_input_id)) ba_source <- input[[ba_input_id]]
      }
      if (!is.null(ba_source) && length(ba_source) == 1 && nzchar(ba_source) && !is.null(pb_col)) {
        pb_input_id <- id_for(pb_col, dest_mode = TRUE)
        if (!is.null(pb_input_id)) {
          # Only fill if PB is currently empty
          pb_current <- input[[pb_input_id]]
          if (is.null(pb_current) || length(pb_current) == 0 || !nzchar(pb_current[1])) {
            updateSelectizeInput(session, pb_input_id, selected = ba_source)
            set_value_source(pb_col, "decimal_split", ba_source)
            message(sprintf("[BATHS-SPLIT] Auto-filled PB ('%s') with BA source '%s'", pb_col, ba_source))
          }
        }
      }
    } else {
      # Standard mode: read BA's current input
      ba_input_id <- id_for("BA", dest_mode = FALSE)
      if (!is.null(ba_input_id)) ba_source <- input[[ba_input_id]]
      if (!is.null(ba_source) && length(ba_source) == 1 && nzchar(ba_source)) {
        pb_input_id <- id_for("PB", dest_mode = FALSE)
        if (!is.null(pb_input_id)) {
          pb_current <- input[[pb_input_id]]
          if (is.null(pb_current) || length(pb_current) == 0 || !nzchar(pb_current[1])) {
            updateSelectizeInput(session, pb_input_id, selected = ba_source)
            set_value_source("PB", "decimal_split", ba_source)
            message(sprintf("[BATHS-SPLIT] Auto-filled PB with BA source '%s'", ba_source))
          }
        }
      }
    }
    # Set suggestion cache so PB shows the suggestion badge on next render
    if (!is.null(ba_source) && length(ba_source) == 1 && nzchar(ba_source)) {
      sug_cache <- suggestion_cache()
      sug_cache[["PB"]] <- list(column = ba_source, score = 100, source = "decimal_split")
      suggestion_cache(sug_cache)
    }
    ui_refresh_trigger(ui_refresh_trigger() + 1)
  }, ignoreInit = TRUE)

  # Observer for decimal baths split undo
  observeEvent(input$baths_split_undo, {
    baths_split_accepted(FALSE)
    # Clear PB's auto-derived mapping
    use_dest <- isTRUE(rv$use_dest_schema)
    if (use_dest && length(rv$dest_headers) > 0) {
      for (dh in rv$dest_headers) {
        sf <- find_std_field_for_column(dh)
        if (!is.null(sf) && sf == "PB") {
          pb_input_id <- id_for(dh, dest_mode = TRUE)
          if (!is.null(pb_input_id)) {
            updateSelectizeInput(session, pb_input_id, selected = character(0))
            clear_value_source(dh)
          }
          break
        }
      }
    } else {
      pb_input_id <- id_for("PB", dest_mode = FALSE)
      if (!is.null(pb_input_id)) {
        updateSelectizeInput(session, pb_input_id, selected = character(0))
        clear_value_source("PB")
      }
    }
    sug_cache <- suggestion_cache()
    sug_cache[["PB"]] <- NULL
    suggestion_cache(sug_cache)
    showNotification("Bath split disabled. Values will remain as-is.", type = "warning", duration = 3)
    message("[BATHS-SPLIT] User undid decimal baths split")
    ui_refresh_trigger(ui_refresh_trigger() + 1)
  }, ignoreInit = TRUE)

  # Observer for SqFtTotal auto-derive accept
  observeEvent(input$sqft_derive_accept, {
    sqft_derive_accepted(TRUE)
    showNotification(
      HTML("<strong>SqFtTotal Auto-Derive Enabled</strong><br/>On export, SqFtTotal = AboveGradeFinishedArea + BelowGradeFinishedArea"),
      type = "message",
      duration = 8
    )
    message("[SQFT-DERIVE] User accepted SqFtTotal auto-derivation")
  }, ignoreInit = TRUE)

  # Observer for SqFtTotal auto-derive undo
  observeEvent(input$sqft_derive_undo, {
    sqft_derive_accepted(FALSE)
    showNotification(
      "SqFtTotal auto-derivation disabled.",
      type = "warning",
      duration = 3
    )
    message("[SQFT-DERIVE] User undid SqFtTotal auto-derivation")
  }, ignoreInit = TRUE)

  # Reactive renderUI outputs for sum-combine preview panels.
  # These are embedded as uiOutput() placeholders in the mapping cards,
  # so they update reactively when selectize values change WITHOUT
  # re-rendering the entire mapping_ui (which would cause infinite loops).
  for (.gn in names(logic_target_groups)) {
    .grp <- logic_target_groups[[.gn]]
    for (.tgt in names(.grp)) {
      .meta <- .grp[[.tgt]]
      if (isTRUE(.meta$multiple) && isTRUE(.meta$combine == "sum")) {
        local({
          local_tgt <- .tgt
          local_input_id <- id_for(.tgt)

          output[[paste0("sum_preview_", local_tgt)]] <- renderUI({
            sel_cols <- input[[local_input_id]]
            ds <- current_dataset()
            if (is.null(sel_cols) || length(sel_cols) < 2 || is.null(ds)) return(NULL)
            valid_cols <- sel_cols[sel_cols %in% names(ds)]
            if (length(valid_cols) < 2) return(NULL)

            n_preview <- min(3, nrow(ds))
            preview_rows <- lapply(seq_len(n_preview), function(i) {
              vals <- suppressWarnings(sapply(valid_cols, function(col) as.numeric(ds[[col]][i])))
              total <- sum(vals, na.rm = TRUE)
              parts <- paste(sapply(seq_along(valid_cols), function(j) {
                sprintf("%s (%s)", valid_cols[j], ifelse(is.na(vals[j]), "NA", format(vals[j], big.mark = ",")))
              }), collapse = " + ")
              tags$div(
                style = "margin: 3px 0; font-size: 12px;",
                tags$span(parts, style = "color: #555;"),
                tags$span(" = ", style = "color: #999;"),
                tags$span(format(total, big.mark = ","), style = "font-weight: 700; color: #1565c0;")
              )
            })
            div(
              style = "margin-top: 8px; padding: 10px; background: #e3f2fd; border-radius: 6px; border-left: 3px solid #1565c0;",
              tags$div(
                style = "font-weight: 600; color: #0d47a1; margin-bottom: 6px;",
                icon("calculator"), " Sum Combine Preview"
              ),
              tags$p(
                style = "font-size: 12px; color: #444; margin: 4px 0;",
                sprintf("Selected columns will be summed: %s", paste(valid_cols, collapse = " + "))
              ),
              tags$div(
                style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px;",
                tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", "Sample calculations:"),
                tagList(preview_rows)
              )
            )
          })
        })
      }
    }
  }

  # Standard mode synonym edit/save observers.
  # Registered once per field at startup (outside renderUI to avoid duplication).
  for (.gn_syn in names(logic_target_groups)) {
    .grp_syn <- logic_target_groups[[.gn_syn]]
    for (.tgt_syn in names(.grp_syn)) {
      local({
        local_tgt <- .tgt_syn
        local_edit_id <- paste0("edit_synonyms_std_", make.names(local_tgt))
        local_save_id <- paste0("save_synonyms_std_", make.names(local_tgt))

        # Edit button opens modal
        observeEvent(input[[local_edit_id]], {
          req(input[[local_edit_id]] > 0)
          current_syns <- character(0)
          for (gn in names(logic_target_groups)) {
            grp <- logic_target_groups[[gn]]
            if (local_tgt %in% names(grp)) {
              current_syns <- grp[[local_tgt]]$synonyms %||% character(0)
              break
            }
          }
          syn_text <- paste(current_syns, collapse = ", ")

          showModal(modalDialog(
            title = paste("Edit Synonyms for", local_tgt),
            tags$div(
              style = "margin-bottom: 15px;",
              tags$p("Enter synonyms separated by commas. These help the auto-mapper identify this column in future datasets."),
              textAreaInput(
                inputId = paste0("synonyms_input_std_", make.names(local_tgt)),
                label = "Synonyms (comma-separated)",
                value = syn_text,
                rows = 4,
                width = "100%",
                placeholder = "e.g., mls, listing id, mls number"
              )
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(local_save_id, "Save Changes", class = "btn-primary")
            ),
            easyClose = FALSE
          ))
        }, ignoreInit = TRUE)

        # Save button in modal
        observeEvent(input[[local_save_id]], {
          req(input[[local_save_id]] > 0)
          input_id <- paste0("synonyms_input_std_", make.names(local_tgt))
          new_text <- input[[input_id]]

          if (!is.null(new_text)) {
            new_syns <- trimws(unlist(strsplit(new_text, ",")))
            new_syns <- new_syns[nzchar(new_syns)]

            success <- save_user_synonyms(local_tgt, new_syns)
            if (success) {
              removeModal()
              showNotification(
                sprintf("Saved %d synonyms for %s. Changes will apply to new mappings.", length(new_syns), local_tgt),
                type = "message"
              )
              # Trigger UI refresh so updated synonyms show immediately
              ui_refresh_trigger(ui_refresh_trigger() + 1)
            } else {
              showNotification("Failed to save synonyms", type = "error")
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  }

  # Reactive renderUI for SqFtTotal auto-derive panel.
  # Watches AboveGrade/BelowGrade inputs reactively without coupling to mapping_ui.
  output$sqft_derive_panel <- renderUI({
    above_sel <- input[[id_for("AboveGradeFinishedArea")]]
    below_sel <- input[[id_for("BelowGradeFinishedArea")]]
    sqft_sel  <- input[[id_for("SqFtTotal")]]
    ds <- current_dataset()
    is_accepted <- sqft_derive_accepted()

    # Only show when SqFtTotal has no direct selection
    has_sqft <- !is.null(sqft_sel) && length(sqft_sel) > 0 && any(nzchar(sqft_sel))
    if (has_sqft || is.null(ds)) return(NULL)

    above_mapped <- !is.null(above_sel) && length(above_sel) > 0 && any(nzchar(above_sel))
    below_mapped <- !is.null(below_sel) && length(below_sel) > 0 && any(nzchar(below_sel))
    if (!above_mapped && !below_mapped) return(NULL)

    # Build formula string
    parts <- character(0)
    if (above_mapped) parts <- c(parts, "AboveGradeFinishedArea")
    if (below_mapped) parts <- c(parts, "BelowGradeFinishedArea")
    formula_str <- paste("SqFtTotal =", paste(parts, collapse = " + "))

    # Preview sample sums
    n_preview <- min(3, nrow(ds))
    preview_rows <- lapply(seq_len(n_preview), function(i) {
      above_val <- 0; below_val <- 0; above_str <- "\u2014"; below_str <- "\u2014"
      if (above_mapped) {
        above_cols <- above_sel[above_sel %in% names(ds)]
        if (length(above_cols) > 0) {
          above_val <- sum(suppressWarnings(sapply(above_cols, function(c) as.numeric(ds[[c]][i]))), na.rm = TRUE)
          above_str <- format(above_val, big.mark = ",")
        }
      }
      if (below_mapped) {
        below_cols <- below_sel[below_sel %in% names(ds)]
        if (length(below_cols) > 0) {
          below_val <- sum(suppressWarnings(sapply(below_cols, function(c) as.numeric(ds[[c]][i]))), na.rm = TRUE)
          below_str <- format(below_val, big.mark = ",")
        }
      }
      total <- above_val + below_val
      parts_display <- character(0)
      if (above_mapped) parts_display <- c(parts_display, sprintf("Above(%s)", above_str))
      if (below_mapped) parts_display <- c(parts_display, sprintf("Below(%s)", below_str))
      tags$div(
        style = "margin: 3px 0; font-size: 12px;",
        tags$span(paste(parts_display, collapse = " + "), style = "color: #555;"),
        tags$span(" = ", style = "color: #999;"),
        tags$span(format(total, big.mark = ","), style = "font-weight: 700; color: #1565c0;")
      )
    })

    div(
      style = "margin-top: 8px; padding: 10px; background: #e3f2fd; border-radius: 6px; border-left: 3px solid #1565c0;",
      tags$div(
        style = "font-weight: 600; color: #0d47a1; margin-bottom: 6px;",
        icon("calculator"), " Auto-Derive Available"
      ),
      tags$p(
        style = "font-size: 12px; color: #444; margin: 4px 0;",
        tags$code(formula_str)
      ),
      tags$div(
        style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
        tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", "Preview:"),
        tagList(preview_rows)
      ),
      if (isTRUE(is_accepted)) {
        tags$div(
          style = "color: #1565c0; font-size: 12px;",
          icon("check"), " SqFtTotal will be derived on export",
          actionLink(
            inputId = "sqft_derive_undo",
            label = "(undo)",
            style = "margin-left: 8px; font-size: 11px; color: #666;"
          )
        )
      } else {
        actionButton(
          inputId = "sqft_derive_accept",
          label = tagList(icon("calculator"), " Accept Auto-Derive"),
          class = "btn-sm btn-primary",
          style = "width: 100%;"
        )
      }
    )
  })

  # Reactive renderUI for SqFtTotal auto-derive panel (dest mode).
  # Watches dest mode AboveGrade/BelowGrade inputs reactively.
  output$sqft_derive_panel_dest <- renderUI({
    ds <- current_dataset()
    dest_headers <- rv$dest_headers
    is_accepted <- sqft_derive_accepted()
    if (is.null(ds) || length(dest_headers) == 0) return(NULL)

    # Find the dest columns that map to AboveGrade/BelowGrade
    above_dest <- NULL; below_dest <- NULL; sqft_dest <- NULL
    for (dc in dest_headers) {
      std <- find_std_field_for_column(dc)
      if (!is.null(std)) {
        if (identical(std, "AboveGradeFinishedArea")) above_dest <- dc
        if (identical(std, "BelowGradeFinishedArea")) below_dest <- dc
        if (identical(std, "SqFtTotal")) sqft_dest <- dc
      }
    }

    # Check if SqFtTotal has a direct selection (if so, don't show derive)
    if (!is.null(sqft_dest)) {
      sqft_sel <- input[[paste0("dest_", make.names(sqft_dest), "_selector")]]
      has_sqft <- !is.null(sqft_sel) && length(sqft_sel) > 0 && any(nzchar(sqft_sel))
      if (has_sqft) return(NULL)
    }

    above_sel <- if (!is.null(above_dest)) input[[paste0("dest_", make.names(above_dest), "_selector")]] else NULL
    below_sel <- if (!is.null(below_dest)) input[[paste0("dest_", make.names(below_dest), "_selector")]] else NULL
    above_mapped <- !is.null(above_sel) && length(above_sel) > 0 && any(nzchar(above_sel))
    below_mapped <- !is.null(below_sel) && length(below_sel) > 0 && any(nzchar(below_sel))
    if (!above_mapped && !below_mapped) return(NULL)

    parts <- character(0)
    if (above_mapped) parts <- c(parts, "AboveGradeFinishedArea")
    if (below_mapped) parts <- c(parts, "BelowGradeFinishedArea")
    formula_str <- paste("SqFtTotal =", paste(parts, collapse = " + "))

    n_preview <- min(3, nrow(ds))
    preview_rows <- lapply(seq_len(n_preview), function(i) {
      above_val <- 0; below_val <- 0; above_str <- "\u2014"; below_str <- "\u2014"
      if (above_mapped) {
        above_cols <- above_sel[above_sel %in% names(ds)]
        if (length(above_cols) > 0) {
          above_val <- sum(suppressWarnings(sapply(above_cols, function(c) as.numeric(ds[[c]][i]))), na.rm = TRUE)
          above_str <- format(above_val, big.mark = ",")
        }
      }
      if (below_mapped) {
        below_cols <- below_sel[below_sel %in% names(ds)]
        if (length(below_cols) > 0) {
          below_val <- sum(suppressWarnings(sapply(below_cols, function(c) as.numeric(ds[[c]][i]))), na.rm = TRUE)
          below_str <- format(below_val, big.mark = ",")
        }
      }
      total <- above_val + below_val
      parts_display <- character(0)
      if (above_mapped) parts_display <- c(parts_display, sprintf("Above(%s)", above_str))
      if (below_mapped) parts_display <- c(parts_display, sprintf("Below(%s)", below_str))
      tags$div(
        style = "margin: 3px 0; font-size: 12px;",
        tags$span(paste(parts_display, collapse = " + "), style = "color: #555;"),
        tags$span(" = ", style = "color: #999;"),
        tags$span(format(total, big.mark = ","), style = "font-weight: 700; color: #1565c0;")
      )
    })

    div(
      style = "margin-top: 8px; padding: 10px; background: #e3f2fd; border-radius: 6px; border-left: 3px solid #1565c0;",
      tags$div(
        style = "font-weight: 600; color: #0d47a1; margin-bottom: 6px;",
        icon("calculator"), " Auto-Derive Available"
      ),
      tags$p(
        style = "font-size: 12px; color: #444; margin: 4px 0;",
        tags$code(formula_str)
      ),
      tags$div(
        style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px; margin-bottom: 8px;",
        tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", "Preview:"),
        tagList(preview_rows)
      ),
      if (isTRUE(is_accepted)) {
        tags$div(
          style = "color: #1565c0; font-size: 12px;",
          icon("check"), " SqFtTotal will be derived on export",
          actionLink(inputId = "sqft_derive_undo", label = "(undo)",
                     style = "margin-left: 8px; font-size: 11px; color: #666;")
        )
      } else {
        actionButton(inputId = "sqft_derive_accept",
                     label = tagList(icon("calculator"), " Accept Auto-Derive"),
                     class = "btn-sm btn-primary", style = "width: 100%;")
      }
    )
  })

  # Reactive renderUI outputs for dest mode sum-combine previews.
  # Created dynamically when dest headers are loaded.
  observe({
    req(length(rv$dest_headers) > 0)
    for (dc in rv$dest_headers) {
      local({
        local_dc <- dc
        local_input_id <- paste0("dest_", make.names(local_dc), "_selector")
        output_id <- paste0("sum_preview_dest_", make.names(local_dc))

        # Check if this dest column maps to a sum-combine field
        std_field <- find_std_field_for_column(local_dc)
        if (is.null(std_field)) return()
        local_meta <- NULL
        for (gn in names(logic_target_groups)) {
          if (std_field %in% names(logic_target_groups[[gn]])) {
            local_meta <- logic_target_groups[[gn]][[std_field]]
            break
          }
        }
        if (is.null(local_meta) || !isTRUE(local_meta$multiple) || !isTRUE(local_meta$combine == "sum")) return()

        output[[output_id]] <- renderUI({
          sel_cols <- input[[local_input_id]]
          ds <- current_dataset()
          if (is.null(sel_cols) || length(sel_cols) < 2 || is.null(ds)) return(NULL)
          valid_cols <- sel_cols[sel_cols %in% names(ds)]
          if (length(valid_cols) < 2) return(NULL)

          n_preview <- min(3, nrow(ds))
          preview_rows <- lapply(seq_len(n_preview), function(i) {
            vals <- suppressWarnings(sapply(valid_cols, function(col) as.numeric(ds[[col]][i])))
            total <- sum(vals, na.rm = TRUE)
            parts <- paste(sapply(seq_along(valid_cols), function(j) {
              sprintf("%s (%s)", valid_cols[j], ifelse(is.na(vals[j]), "NA", format(vals[j], big.mark = ",")))
            }), collapse = " + ")
            tags$div(
              style = "margin: 3px 0; font-size: 12px;",
              tags$span(parts, style = "color: #555;"),
              tags$span(" = ", style = "color: #999;"),
              tags$span(format(total, big.mark = ","), style = "font-weight: 700; color: #1565c0;")
            )
          })
          div(
            style = "margin-top: 8px; padding: 10px; background: #e3f2fd; border-radius: 6px; border-left: 3px solid #1565c0;",
            tags$div(
              style = "font-weight: 600; color: #0d47a1; margin-bottom: 6px;",
              icon("calculator"), " Sum Combine Preview"
            ),
            tags$p(
              style = "font-size: 12px; color: #444; margin: 4px 0;",
              sprintf("Selected columns will be summed: %s", paste(valid_cols, collapse = " + "))
            ),
            tags$div(
              style = "background: #fff; padding: 8px 10px; border-radius: 4px; font-size: 12px;",
              tags$div(style = "font-weight: 600; font-size: 11px; color: #666; margin-bottom: 4px;", "Sample calculations:"),
              tagList(preview_rows)
            )
          )
        })
      })
    }
  })

  # Observers for mapping card preview buttons (destination mode)
  # Use observe to track destination headers dynamically
  observe({
    req(length(rv$dest_headers) > 0)
    dest_headers <- rv$dest_headers

    # Register handlers for each destination header
    lapply(dest_headers, function(dest_col) {
      preview_id <- paste0("preview_dest_", make.names(dest_col))
      selector_id <- paste0("dest_", make.names(dest_col), "_selector")

      # Check if handler already exists for this ID
      local({
        local_dest_col <- dest_col
        local_preview_id <- preview_id
        local_selector_id <- selector_id

        observeEvent(input[[local_preview_id]], {
          # IMPORTANT: Require the button click value to be > 0
          # This prevents the modal from triggering on initialization
          req(input[[local_preview_id]] > 0)
          req(current_dataset())
          dataset <- current_dataset()

          # Get the selected source column(s) from the selector
          selected_cols <- input[[local_selector_id]]

          if (is.null(selected_cols) || length(selected_cols) == 0 || !any(nzchar(selected_cols))) {
            showNotification("No column selected to preview", type = "warning")
            return()
          }

          # Filter to valid columns that exist in dataset
          valid_cols <- selected_cols[selected_cols %in% names(dataset)]
          if (length(valid_cols) == 0) {
            showNotification("Selected column(s) not found in dataset", type = "error")
            return()
          }

          # Build preview content for all selected columns (in order)
          if (length(valid_cols) == 1) {
            # Single column - show simple preview
            col_to_preview <- valid_cols[1]
            values <- dataset[[col_to_preview]]
            values_sample <- head(values, 50)

            # Check if this destination is a derivable metric - if so, show calculated values
            derived_info <- get_derived_metric_for_dest(local_dest_col, list())
            is_derivable <- isTRUE(derived_info$is_derivable)
            calculated_values <- NULL
            calculation_label <- NULL

            message(sprintf("[PREVIEW] dest_col=%s, is_derivable=%s, metric_name=%s",
                            local_dest_col, is_derivable,
                            if (!is.null(derived_info$metric_name)) derived_info$metric_name else "NULL"))

            if (is_derivable && derived_info$metric_name == "SaleQtr") {
              message(sprintf("[PREVIEW] SaleQtr: col_to_preview=%s, first_value=%s",
                              col_to_preview, as.character(values_sample[1])))
              # Calculate SaleQtr from the date column
              # Try multiple date formats to handle various MLS date formats
              parsed_dates <- tryCatch({
                # Convert to character and handle NAs
                date_strings <- as.character(values_sample)
                date_strings[is.na(date_strings) | !nzchar(trimws(date_strings))] <- NA

                # Strip time portion if present (e.g., "11/19/2025 12:00:00 AM" -> "11/19/2025")
                date_only <- sub(" .*", "", date_strings)

                # Count how many non-NA values we have
                non_na_count <- sum(!is.na(date_only))

                # Try m/d/Y format FIRST (most common in MLS data)
                result <- suppressWarnings(as.Date(date_only, format = "%m/%d/%Y"))

                # If m/d/Y didn't work well, try ISO format
                if (non_na_count > 0 && sum(!is.na(result)) < non_na_count * 0.5) {
                  result2 <- suppressWarnings(as.Date(date_only))
                  if (sum(!is.na(result2)) > sum(!is.na(result))) result <- result2
                }

                # If still not working, try d/m/Y (European format)
                if (non_na_count > 0 && sum(!is.na(result)) < non_na_count * 0.5) {
                  result3 <- suppressWarnings(as.Date(date_only, format = "%d/%m/%Y"))
                  if (sum(!is.na(result3)) > sum(!is.na(result))) result <- result3
                }

                message(sprintf("[PREVIEW] Date parsing: non_na=%d, parsed=%d, first_date_only='%s', first_result=%s",
                                non_na_count, sum(!is.na(result)),
                                if (length(date_only) > 0) date_only[1] else "NULL",
                                if (length(result) > 0 && !is.na(result[1])) as.character(result[1]) else "NA"))
                result
              }, error = function(e) {
                message(sprintf("Date parsing error: %s", e$message))
                rep(NA, length(values_sample))
              })

              valid_mask <- !is.na(parsed_dates)
              calculated_values <- rep(NA_real_, length(values_sample))
              message(sprintf("[PREVIEW] After parsing: valid_count=%d", sum(valid_mask)))
              if (any(valid_mask)) {
                years <- as.integer(format(parsed_dates[valid_mask], "%Y"))
                months <- as.integer(format(parsed_dates[valid_mask], "%m"))
                quarters <- ceiling(months / 3)
                calculated_values[valid_mask] <- years + (quarters / 10)
                message(sprintf("[PREVIEW] Calculated first value: %s", calculated_values[1]))
              }
              calculation_label <- "Year.Quarter format"
            } else if (is_derivable && derived_info$metric_name %in% c("PricePerSqFt", "PricePerLotSF")) {
              # These need two columns, so can't preview from single column selection
              is_derivable <- FALSE
            }

            # Format display values
            if (!is.null(calculated_values)) {
              # Show both original and calculated
              values_display <- sapply(seq_along(values_sample), function(i) {
                orig <- values_sample[i]
                calc <- calculated_values[i]
                orig_str <- if (is.null(orig) || is.na(orig) || !nzchar(trimws(as.character(orig)))) "[blank]" else as.character(orig)
                calc_str <- if (is.na(calc)) "[blank]" else as.character(calc)
                sprintf("%s → %s", orig_str, calc_str)
              }, USE.NAMES = FALSE)
            } else {
              values_display <- sapply(values_sample, function(v) {
                if (is.null(v) || is.na(v)) return("[blank]")
                v_str <- as.character(v)
                if (is.na(v_str) || !nzchar(trimws(v_str))) return("[blank]")
                if (nchar(v_str) > 200) {
                  v_str <- paste0(substr(v_str, 1, 197), "...")
                }
                v_str
              }, USE.NAMES = FALSE)
            }

            # Build modal content
            modal_content <- tagList(
              p(tags$strong("Destination column:"), local_dest_col),
              p(tags$strong("Source column:"), col_to_preview),
              p(tags$strong("Total rows:"), nrow(dataset))
            )

            if (!is.null(calculated_values)) {
              modal_content <- tagList(
                modal_content,
                tags$div(
                  style = "margin: 10px 0; padding: 8px 12px; background: #e8f5e9; border-radius: 4px; border-left: 3px solid #4caf50;",
                  icon("calculator", style = "color: #2e7d32;"), " ",
                  tags$strong("Calculated preview: "), calculation_label,
                  tags$br(),
                  tags$small(style = "color: #558b2f;", "Showing: Original → Calculated")
                )
              )
            }

            modal_content <- tagList(
              modal_content,
              p(tags$strong("Sample values (first 50 rows):")),
              tags$div(
                class = "preview-content",
                style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
                tags$ol(
                  lapply(seq_along(values_display), function(i) {
                    tags$li(values_display[i])
                  })
                )
              )
            )

            showModal(modalDialog(
              title = if (!is.null(calculated_values)) {
                paste("Preview Calculated:", local_dest_col)
              } else {
                paste("Preview Column:", col_to_preview)
              },
              modal_content,
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          } else {
            # Multiple columns - show combined/merged preview only
            n_preview <- min(50, nrow(dataset))

            # Build combined/merged preview (how it will look when assembled)
            combined_values <- apply(dataset[1:n_preview, valid_cols, drop = FALSE], 1, function(row) {
              parts <- sapply(row, function(v) {
                if (is.null(v) || is.na(v)) return("")
                v_str <- trimws(as.character(v))
                if (is.na(v_str)) return("")
                v_str
              })
              paste(parts[nzchar(parts)], collapse = " ")
            })

            showModal(modalDialog(
              title = sprintf("Preview Multi-Column Mapping: %s", local_dest_col),
              tags$div(
                tags$p(tags$strong("Destination column:"), local_dest_col),
                tags$p(tags$strong("Selected columns (in order):"), paste(valid_cols, collapse = " → ")),
                tags$p(tags$strong("Total rows:"), nrow(dataset)),
                tags$hr(),
                tags$h5(icon("link"), " Combined/Merged Preview"),
                tags$p(style = "color: #666; font-size: 12px;",
                       "How the columns will appear when merged (space-separated). Reorder in the dropdown to change merge order."),
                tags$div(
                  class = "preview-content",
                  style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
                  tags$ol(
                    lapply(combined_values, function(v) tags$li(if(nzchar(v)) v else "[blank]"))
                  )
                )
              ),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }
        }, ignoreInit = TRUE, priority = 100)

        local_edit_id <- paste0("edit_synonyms_", make.names(dest_col))
        local_save_id <- paste0("save_synonyms_", make.names(dest_col))
        
        # Observer for Edit button (opens modal)
        observeEvent(input[[local_edit_id]], {
          req(input[[local_edit_id]] > 0)
          
          # Get current synonyms
          current_syns <- get_dest_synonyms(local_dest_col)
          syn_text <- paste(current_syns, collapse = ", ")
          
          showModal(modalDialog(
            title = paste("Edit Synonyms for", local_dest_col),
            tags$div(
              style = "margin-bottom: 15px;",
              tags$p("Enter synonyms separated by commas. These help the auto-mapper identify this column in future datasets."),
              textAreaInput(
                inputId = paste0("synonyms_input_", make.names(local_dest_col)),
                label = "Synonyms (comma-separated)",
                value = syn_text,
                rows = 4,
                width = "100%",
                placeholder = "e.g., mls, listing id, mls number"
              )
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(local_save_id, "Save Changes", class = "btn-primary")
            ),
            easyClose = FALSE
          ))
        }, ignoreInit = TRUE)
        
        # Observer for Save button (in modal)
        observeEvent(input[[local_save_id]], {
          req(input[[local_save_id]] > 0)
          
          # Get input value
          input_id <- paste0("synonyms_input_", make.names(local_dest_col))
          new_text <- input[[input_id]]
          
          if (!is.null(new_text)) {
            # Parse synonyms
            new_syns <- trimws(unlist(strsplit(new_text, ",")))
            new_syns <- new_syns[nzchar(new_syns)]
            
            # Find standard logic key using multi-strategy lookup
            # (cache first, then direct field name match, then synonym match)
            std_key <- find_std_field_for_column(local_dest_col)
            
            if (!is.null(std_key)) {
                # Save to file and memory using the STANDARD key
                success <- save_user_synonyms(std_key, new_syns)
                
                if (success) {
                  removeModal()
                  showNotification(
                    sprintf("Saved %d synonyms for %s (logic field: %s). Changes will apply to new mappings.", length(new_syns), local_dest_col, std_key),
                    type = "message"
                  )
                } else {
                  showNotification("Failed to save synonyms", type = "error")
                }
            } else {
                showNotification(sprintf("Could not identify the logic field for '%s'. Ensure it is matched to a standard field.", local_dest_col), type = "error")
            }
          }
        }, ignoreInit = TRUE)
      })
    })
  })

  # Reactive value to store the mapped data
  mapped_data <- reactiveVal(NULL)
  logic_mapped_data <- reactiveVal(NULL)

  observeEvent(input$show_mappings, {
    mappings <- get_all_selections()
    showModal(modalDialog(
      title = "Current Column Mappings",
      if (length(mappings) == 0) {
        p("No mappings configured yet")
      } else {
        tags$ul(
          lapply(names(mappings), function(target) {
            # Convert vector to string for display (handles multiple columns like Address)
            mapped_cols <- paste(mappings[[target]], collapse = ", ")
            tags$li(tags$strong(target), ": ", mapped_cols)
          })
        )
      },
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Apply mappings to the data
  observeEvent(input$apply_mappings, {
    mappings <- get_all_selections()

    # Debug: Log accepted Class 3 mappings and what get_all_selections returned
    accepted_c3 <- class3_accepted_mappings()
    message(sprintf("=== APPLY MAPPINGS DEBUG ==="))
    message(sprintf("class3_accepted_mappings has %d entries: %s",
                    length(accepted_c3),
                    paste(names(accepted_c3), collapse = ", ")))
    message(sprintf("get_all_selections returned %d mappings: %s",
                    length(mappings),
                    paste(names(mappings), collapse = ", ")))
    if (length(mappings) > 0) {
      for (tgt in names(mappings)) {
        message(sprintf("  Mapping: %s -> %s", tgt, paste(mappings[[tgt]], collapse = ", ")))
      }
    }

    # Check if we're in destination schema mode
    use_dest_schema <- isTRUE(input$use_destination_schema) && length(rv$dest_headers) > 0

    if (length(mappings) == 0) {
      showNotification("No mappings to apply!", type = "warning")
      return()
    }

    # Start with a copy of the current dataset
    original_data <- current_dataset()
    if (is.null(original_data)) {
      showNotification("No dataset loaded. Upload source data first.", type = "error")
      return()
    }

    # Wrap the entire apply operation in progress indicator
    withProgress(message = "Applying mappings...", value = 0, {
      setProgress(value = 0.1, detail = sprintf("Processing %d mappings", length(mappings)))

    result_data <- original_data

    # Get all columns
    all_cols <- names(result_data)
    mapped_source_cols <- unique(unlist(mappings))

    # Find unmapped columns
    unmapped_cols <- setdiff(all_cols, mapped_source_cols)

    # Check which unmapped columns user wants to include
    included_unmapped <- character(0)
    for (col_name in unmapped_cols) {
      checkbox_id <- paste0("include_", make.names(col_name))
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]] == TRUE) {
        included_unmapped <- c(included_unmapped, col_name)
      }
    }

    # Debug: Check decimal baths status
    message(sprintf("[BATHS-DEBUG] baths_split_accepted = %s", baths_split_accepted()))
    message(sprintf("[BATHS-DEBUG] 'BA' in mappings = %s", "BA" %in% names(mappings)))
    if ("BA" %in% names(mappings)) {
      message(sprintf("[BATHS-DEBUG] BA source = '%s'", mappings[["BA"]]))
    }

    # Apply each mapping (rename or merge columns)
    for (target_name in names(mappings)) {
      source_name <- mappings[[target_name]]

      if (identical(target_name, "Address") &&
          length(source_name) == 1 &&
          source_name %in% names(result_data)) {
        address_values <- result_data[[source_name]]
        if (address_missing_street_number(address_values)) {
          candidate <- find_street_number_column(
            result_data,
            exclude_cols = unique(c(source_name, mapped_source_cols))
          )
          if (!is.null(candidate) && candidate$column %in% names(result_data)) {
            source_name <- c(candidate$column, source_name)
            mappings[[target_name]] <- source_name
            mapped_source_cols <- unique(c(mapped_source_cols, source_name))
            message(sprintf("INFO: Auto-prefixed street number column '%s' with '%s' for target '%s'",
                            candidate$column, mappings[[target_name]][2], target_name))
          }
        } else {
          # If selected address looks like a number-only column, try to append a street name column
          vals <- result_data[[source_name]]
          vals_chr <- trimws(as.character(vals))
          smp <- head(vals_chr[nzchar(vals_chr)], 150)
          number_only_ratio <- if (length(smp)) mean(grepl("^[[:space:]]*#?[0-9]+$", smp)) else 0
          if (!is.nan(number_only_ratio) && number_only_ratio >= 0.6) {
            name_candidate <- find_street_name_column(
              result_data,
              exclude_cols = unique(c(source_name, mapped_source_cols))
            )
            if (!is.null(name_candidate) && name_candidate$column %in% names(result_data)) {
              source_name <- c(source_name, name_candidate$column)
              mappings[[target_name]] <- source_name
              mapped_source_cols <- unique(c(mapped_source_cols, source_name))
              message(sprintf("INFO: Auto-appended street name column '%s' to numeric '%s' for target '%s'",
                              name_candidate$column, mappings[[target_name]][1], target_name))
            }
          }
        }
      }

      # Handle multiple source columns (e.g., Address from multiple fields)
      if (length(source_name) > 1) {
        # Check if all source columns exist
        if (all(source_name %in% names(result_data))) {
          # Look up combine mode from metadata
          target_combine <- NULL
          for (gn in names(logic_target_groups)) {
            if (target_name %in% names(logic_target_groups[[gn]])) {
              target_combine <- logic_target_groups[[gn]][[target_name]]$combine
              break
            }
          }
          # Also check dest mode metadata
          if (is.null(target_combine) && use_dest_schema) {
            dest_meta <- get_dest_metadata(target_name)
            if (!is.null(dest_meta)) target_combine <- dest_meta$combine
          }

          if (identical(target_combine, "sum")) {
            # Sum mode: convert to numeric and sum
            numeric_cols <- lapply(source_name, function(col) {
              suppressWarnings(as.numeric(as.character(result_data[[col]])))
            })
            sum_values <- do.call(cbind, numeric_cols)
            all_na_mask <- apply(is.na(sum_values), 1, all)
            result_values <- rowSums(sum_values, na.rm = TRUE)
            result_values[all_na_mask] <- NA
            result_data[[target_name]] <- result_values
            message(sprintf("✓ Sum-combined '%s' into '%s' (%d non-NA values)",
                           paste(source_name, collapse = " + "), target_name, sum(!is.na(result_values))))
          } else {
            # Concat mode: Merge multiple columns with a configurable separator
            sep <- " "

            # Check for separator input (destination mode vs standard mode)
            sep_input <- if (use_dest_schema) {
              # Destination mode: use dest separator
              input[[paste0("sep_dest_", make.names(target_name))]]
            } else {
              # Standard mode: only Address field has custom separator
              if (identical(target_name, "Address")) {
                input[[paste0("sep_", target_name)]]
              } else {
                NULL
              }
            }

            if (!is.null(sep_input)) sep <- as.character(sep_input)
            if (!nzchar(sep)) sep <- " "

            merged_data <- apply(result_data[, source_name, drop = FALSE], 1, function(row) {
              # Remove empty/NA values and join
              valid_values <- row[!is.na(row) & nzchar(trimws(as.character(row)))]
              paste(valid_values, collapse = sep)
            })
            # Add the merged column
            result_data[[target_name]] <- merged_data
            message(sprintf("✓ Merged '%s' into '%s' with separator '%s'", paste(source_name, collapse = ", "), target_name, sep))
          }
        } else {
          missing <- source_name[!source_name %in% names(result_data)]
          message(sprintf("✗ FAILED: Source columns '%s' not found for target '%s'",
                         paste(missing, collapse = ", "), target_name))
        }
      } else {
        # Single source column - check if this is a derived metric that needs calculation
        target_norm <- tolower(gsub("[^[:alnum:]]", "", target_name))
        is_derived_metric <- target_norm %in% c("saleqtr", "salequarter")

        if (is_derived_metric && target_norm %in% c("saleqtr", "salequarter")) {
          # SaleQtr: Create a calculated column from the date source
          # Look for source in result_data OR check if it was already renamed
          actual_source <- if (source_name %in% names(result_data)) {
            source_name
          } else {
            # Source might have been renamed - check rename_map
            renamed_to <- NULL
            for (other_target in names(mappings)) {
              if (identical(mappings[[other_target]], source_name) && other_target %in% names(result_data)) {
                renamed_to <- other_target
                break
              }
            }
            renamed_to
          }

          if (!is.null(actual_source) && actual_source %in% names(result_data)) {
            # Calculate SaleQtr from the date column - wrapped in tryCatch for non-date sources
            tryCatch({
              date_values <- result_data[[actual_source]]
              date_strings <- as.character(date_values)
              date_strings[is.na(date_strings) | !nzchar(trimws(date_strings))] <- NA

              # Strip time portion if present
              date_only <- sub(" .*", "", date_strings)

              # Try multiple date formats
              parsed_dates <- suppressWarnings(as.Date(date_only, format = "%m/%d/%Y"))
              if (sum(!is.na(parsed_dates)) < sum(!is.na(date_only)) * 0.5) {
                parsed_dates2 <- suppressWarnings(as.Date(date_only))
                if (sum(!is.na(parsed_dates2)) > sum(!is.na(parsed_dates))) parsed_dates <- parsed_dates2
              }

              # Check if we got any valid dates - if not, the source isn't a date column
              if (all(is.na(parsed_dates))) {
                message(sprintf("⚠ SaleQtr source '%s' does not contain date values - copying as-is", actual_source))
                # Just copy the source values since they aren't dates
                result_data[[target_name]] <- date_values
              } else {
                # Calculate Year.Quarter
                calculated <- rep(NA_real_, length(date_values))
                valid_mask <- !is.na(parsed_dates)
                if (any(valid_mask)) {
                  years <- as.integer(format(parsed_dates[valid_mask], "%Y"))
                  months <- as.integer(format(parsed_dates[valid_mask], "%m"))
                  quarters <- ceiling(months / 3)
                  calculated[valid_mask] <- years + (quarters / 10)
                }

                result_data[[target_name]] <- calculated
                message(sprintf("✓ Calculated '%s' from '%s' (Year.Quarter format)", target_name, actual_source))
              }
            }, error = function(e) {
              # If date parsing completely fails, just copy the source values
              message(sprintf("⚠ SaleQtr calculation failed for '%s': %s - copying as-is", actual_source, e$message))
              result_data[[target_name]] <<- result_data[[actual_source]]
            })
          } else {
            message(sprintf("✗ FAILED: Source column '%s' not found for derived metric '%s'", source_name, target_name))
          }
        } else if (!source_name %in% names(result_data) && target_name %in% names(result_data)) {
          # Source was already consumed (e.g., by BA decimal split which created PB)
          message(sprintf("✓ Skipped '%s': already populated by a prior split", target_name))
        } else if (source_name %in% names(result_data)) {
          # Check for decimal baths split transformation
          # Triggers on BA card in both standard mode (target="BA") and destination mode (via field lookup)
          is_baths_target <- identical(target_name, "BA") ||
                            (use_dest_schema && {
                              std_field_for_baths <- find_std_field_for_column(target_name)
                              !is.null(std_field_for_baths) && std_field_for_baths %in% c("BA", "PB")
                            })

          if (is_baths_target && isTRUE(baths_split_accepted())) {
            source_values <- result_data[[source_name]]

            if (is_decimal_baths(source_values)) {
              split_result <- split_decimal_baths(source_values)

              if (use_dest_schema) {
                # Destination mode: find actual template column names for BA/PB
                dest_headers <- rv$dest_headers
                ba_dest_col <- NULL; pb_dest_col <- NULL
                for (dh in dest_headers) {
                  sf <- find_std_field_for_column(dh)
                  if (!is.null(sf)) {
                    if (sf == "BA") ba_dest_col <- dh
                    else if (sf == "PB") pb_dest_col <- dh
                  }
                }
                if (!is.null(ba_dest_col)) {
                  result_data[[ba_dest_col]] <- split_result$full_baths
                  mappings[[ba_dest_col]] <- source_name
                }
                if (!is.null(pb_dest_col)) {
                  result_data[[pb_dest_col]] <- split_result$half_baths
                  mappings[[pb_dest_col]] <- source_name
                }
                mappings[[target_name]] <- NULL
                result_data[[source_name]] <- NULL
                message(sprintf("✓ Bath split (dest mode): '%s' → %s (full), %s (half)",
                               source_name,
                               if (!is.null(ba_dest_col)) ba_dest_col else "N/A",
                               if (!is.null(pb_dest_col)) pb_dest_col else "N/A"))
              } else {
                # Standard mode: BA and PB
                result_data[["BA"]] <- split_result$full_baths
                result_data[["PB"]] <- split_result$half_baths
                result_data[[source_name]] <- NULL
                mappings[["BA"]] <- source_name
                mappings[["PB"]] <- source_name
                message(sprintf("✓ Bath split: '%s' → BA (%d), PB (%d)",
                               source_name, sum(!is.na(split_result$full_baths)), sum(!is.na(split_result$half_baths))))
              }
            } else {
              # Not decimal baths, just rename normally
              names(result_data)[names(result_data) == source_name] <- target_name
              message(sprintf("✓ Renamed '%s' to '%s' (no decimal values to split)", source_name, target_name))
            }
          } else {
            # Regular rename
            # Debug: show sample values BEFORE rename for key columns
            if (target_name %in% c("PriceSold", "DateSold", "SqFt", "BR", "Neighborhood")) {
              sample_vals <- head(result_data[[source_name]], 5)
              non_empty <- sum(!is.na(sample_vals) & nzchar(as.character(sample_vals)))
              message(sprintf("  DEBUG: Source '%s' sample values (non-empty: %d/5): [%s]",
                             source_name, non_empty, paste(sample_vals, collapse = ", ")))
            }
            names(result_data)[names(result_data) == source_name] <- target_name
            message(sprintf("✓ Renamed '%s' to '%s'", source_name, target_name))
          }
        } else {
          # Source column not found — it may have been renamed by a prior mapping.
          # Check if another target already consumed this source (duplicate mapping).
          already_renamed_to <- NULL
          for (prev_target in names(mappings)) {
            if (identical(mappings[[prev_target]], source_name) && prev_target %in% names(result_data)) {
              already_renamed_to <- prev_target
              break
            }
          }
          if (!is.null(already_renamed_to)) {
            # Duplicate mapping: copy the data from the already-renamed column
            result_data[[target_name]] <- result_data[[already_renamed_to]]
            message(sprintf("✓ Duplicated '%s' (from '%s') to '%s' (shared source: '%s')",
                           already_renamed_to, source_name, target_name, source_name))
          } else {
            message(sprintf("✗ FAILED: Source column '%s' not found for target '%s'", source_name, target_name))
          }
        }
      }
    }

    message(sprintf("Columns after renaming: %s", paste(names(result_data), collapse = ", ")))
    message(sprintf("[BATHS-DEBUG] Final mappings: %s", paste(names(mappings), collapse = ", ")))

    # If ListingKey exists but ListingId was not separately mapped, duplicate ListingKey
    # into ListingId so CValR receives both fields. This handles the common case where
    # only one ID column (e.g. "MLS#") exists and was used as the unique identifier.
    if ("ListingKey" %in% names(result_data) &&
        !("ListingId" %in% names(result_data)) &&
        !("ListingId" %in% names(mappings))) {
      result_data[["ListingId"]] <- result_data[["ListingKey"]]
      mappings[["ListingId"]] <- "ListingKey"
      listing_id_auto_detected <- "ListingId"
      message("ListingId auto-populated: Duplicated ListingKey into ListingId (single ID column)")
    }

    # Keep only mapped columns + included unmapped columns
    columns_to_keep <- c(names(mappings), included_unmapped)
    message(sprintf("[BATHS-DEBUG] columns_to_keep: %s", paste(columns_to_keep, collapse = ", ")))

    merged_source_columns <- character(0)
    if (length(mappings) > 0) {
      merged_source_columns <- unique(unlist(mappings[sapply(mappings, length) > 1]))
    }

    # Always retain address components if multiple columns were used
    address_sources <- mappings[["Address"]]
    if (!is.null(address_sources) && length(address_sources) > 1) {
      columns_to_keep <- c(columns_to_keep, address_sources)
    }

    # Optionally keep original source columns for other merged fields
    if (isTRUE(input$keep_merged_sources) && length(merged_source_columns) > 0) {
      columns_to_keep <- c(columns_to_keep, merged_source_columns)
    }

    # ============================================================================
    # Classification-based column filtering
    # - Class 3 (Marginal): Auto-include as pass-through
    # - Class 4 (Exclude): Auto-exclude from export
    # ============================================================================
    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides
    original_cols <- names(original_data)

    class3_passthrough <- character(0)
    class4_excluded <- character(0)

    if (!is.null(classifications)) {
      for (col_name in original_cols) {
        # Determine effective class (override takes precedence)
        if (!is.null(overrides[[col_name]])) {
          effective_class <- overrides[[col_name]]
        } else {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
            effective_class <- classifications$class[idx[1]]
          } else {
            effective_class <- NA
          }
        }

        if (!is.na(effective_class)) {
          if (effective_class == 3) {
            class3_passthrough <- c(class3_passthrough, col_name)
          } else if (effective_class == 4) {
            class4_excluded <- c(class4_excluded, col_name)
          }
        }
      }

      # Add Class 3 columns to keep list (pass-through)
      columns_to_keep <- c(columns_to_keep, class3_passthrough)

      # Log classification-based filtering
      if (length(class3_passthrough) > 0) {
        message(sprintf("Class 3 pass-through columns (%d): %s",
                       length(class3_passthrough), paste(head(class3_passthrough, 10), collapse = ", ")))
      }
      if (length(class4_excluded) > 0) {
        message(sprintf("Class 4 excluded columns (%d): %s",
                       length(class4_excluded), paste(head(class4_excluded, 10), collapse = ", ")))
      }
    }

    columns_to_keep <- unique(columns_to_keep)

    # Remove Class 4 columns from the keep list (they should never be exported)
    columns_to_keep <- setdiff(columns_to_keep, class4_excluded)

    # Apply export class filter (user can choose to exclude Class 1, 2, or 3)
    export_classes <- as.integer(input$export_class_filter %||% c("1", "2", "3"))

    # In destination schema mode, also keep destination header columns that exist in source data
    # BUT only if they pass the export class filter
    if (use_dest_schema && length(rv$dest_headers) > 0) {
      dest_cols_in_source <- intersect(rv$dest_headers, names(result_data))

      # Always filter dest_cols_in_source by export class filter
      if (!is.null(classifications) && length(export_classes) > 0) {
        dest_cols_filtered <- character(0)
        dest_cols_excluded <- character(0)
        for (col_name in dest_cols_in_source) {
          # Get effective class for this column
          if (!is.null(overrides[[col_name]])) {
            col_class <- overrides[[col_name]]
          } else {
            idx <- which(classifications$column_name == col_name)
            if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
              col_class <- classifications$class[idx[1]]
            } else {
              col_class <- NA  # Unclassified
            }
          }
          # Only keep if class is explicitly in allowed export classes
          # Class 4 and unclassified (NA) columns are excluded
          if (!is.na(col_class) && col_class %in% export_classes) {
            dest_cols_filtered <- c(dest_cols_filtered, col_name)
          } else {
            dest_cols_excluded <- c(dest_cols_excluded, col_name)
          }
        }
        dest_cols_in_source <- dest_cols_filtered
        message(sprintf("Destination schema mode: Keeping %d destination columns (class %s), excluded %d",
                        length(dest_cols_in_source), paste(export_classes, collapse = ","),
                        length(dest_cols_excluded)))
      } else {
        message(sprintf("Destination schema mode: Added %d destination columns to keep list", length(dest_cols_in_source)))
      }

      columns_to_keep <- unique(c(columns_to_keep, dest_cols_in_source))
    }

    # Add ListingId source column to keep list if option is checked
    # This ensures MLS# passes through even when destination schema doesn't include it
    listing_id_auto_detected <- NULL
    if (isTRUE(input$include_listing_id)) {
      # First check if ListingId is explicitly mapped
      if ("ListingId" %in% names(mappings) && length(mappings[["ListingId"]]) > 0) {
        listing_id_src <- mappings[["ListingId"]][1]
        if (listing_id_src %in% names(result_data)) {
          columns_to_keep <- unique(c(columns_to_keep, listing_id_src))
          listing_id_auto_detected <- listing_id_src
          message(sprintf("ListingId option: Added mapped column '%s' to columns_to_keep", listing_id_src))
        }
      } else {
        # Auto-detect MLS-like columns in source data when ListingId isn't mapped
        # This handles destination schema mode where ListingId isn't a target field
        mls_patterns <- c("^mls$", "^mls#$", "^mls ?#$", "^mlsnumber$", "^mls ?number$",
                          "^listingid$", "^listing ?id$", "^listingkey$", "^listing ?key$",
                          "^ml ?#$", "^mlsid$")
        source_cols <- names(result_data)
        for (pat in mls_patterns) {
          matches <- grep(pat, tolower(gsub("[^a-zA-Z0-9#]", "", source_cols)), ignore.case = TRUE)
          if (length(matches) > 0) {
            listing_id_src <- source_cols[matches[1]]
            columns_to_keep <- unique(c(columns_to_keep, listing_id_src))
            listing_id_auto_detected <- listing_id_src
            message(sprintf("ListingId option: Auto-detected '%s' as MLS column, added to columns_to_keep", listing_id_src))
            break
          }
        }
        if (is.null(listing_id_auto_detected)) {
          message("ListingId option: No MLS-like column found in source data")
        }
      }
    }

    # Always apply export class filter to ensure only selected classes are exported
    # This filters columns based on their classification (1, 2, or 3)
    if (!is.null(classifications) && length(export_classes) > 0) {
      # Build list of columns to KEEP based on class filter
      class_filtered_keep <- character(0)
      class_filtered_out <- character(0)
      unclassified_excluded <- character(0)

      # Explicitly mapped columns should always be kept regardless of classification
      # After renaming, target names (e.g. "DateSold") won't match the original
      # classification table (which has "Close Date"), so they'd be excluded as
      # "unclassified" without this protection
      explicitly_mapped_cols <- if (length(mappings) > 0) {
        names(mappings)
      } else {
        character(0)
      }

      # Use the auto-detected or mapped ListingId column from earlier
      listing_id_source <- listing_id_auto_detected
      if (!is.null(listing_id_source)) {
        message(sprintf("ListingId option: Will preserve column '%s' in class filtering", listing_id_source))
      }

      for (col_name in columns_to_keep) {
        # Get effective class for this column
        if (!is.null(overrides[[col_name]])) {
          col_class <- overrides[[col_name]]
        } else {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
            col_class <- classifications$class[idx[1]]
          } else {
            col_class <- NA  # Unclassified
          }
        }

        # Keep column if:
        # 1. It's a derived/metadata column (starts with . or is in DERIVED_METRIC_COLUMNS)
        # 2. It's an explicitly mapped destination column (user chose to map it)
        # 3. It's the ListingId source column and include_listing_id is checked
        # 4. Its class is explicitly in the selected export classes
        # Exclude: unclassified (NA) and Class 4 columns that weren't explicitly mapped
        is_metadata_col <- grepl("^\\.", col_name) || col_name %in% DERIVED_METRIC_COLUMNS
        is_mapped_col <- col_name %in% explicitly_mapped_cols
        is_listing_id_col <- !is.null(listing_id_source) && col_name == listing_id_source

        if (is_metadata_col) {
          class_filtered_keep <- c(class_filtered_keep, col_name)
        } else if (is_mapped_col) {
          # Always keep explicitly mapped columns (renamed targets won't match classifications)
          class_filtered_keep <- c(class_filtered_keep, col_name)
        } else if (is_listing_id_col) {
          # Keep the ListingId source column when option is checked (for CMS identification)
          class_filtered_keep <- c(class_filtered_keep, col_name)
        } else if (is.na(col_class)) {
          # Unclassified columns - EXCLUDE them (they may be system columns or unmapped)
          unclassified_excluded <- c(unclassified_excluded, col_name)
        } else if (col_class %in% export_classes) {
          class_filtered_keep <- c(class_filtered_keep, col_name)
        } else {
          class_filtered_out <- c(class_filtered_out, col_name)
        }
      }

      if (length(class_filtered_out) > 0) {
        message(sprintf("Export class filter: Excluded %d columns not in selected classes (%s): %s",
                        length(class_filtered_out),
                        paste(export_classes, collapse = ", "),
                        paste(head(class_filtered_out, 20), collapse = ", ")))
      }
      if (length(unclassified_excluded) > 0) {
        message(sprintf("Export class filter: Excluded %d unclassified columns: %s",
                        length(unclassified_excluded),
                        paste(head(unclassified_excluded, 20), collapse = ", ")))
      }

      columns_to_keep <- class_filtered_keep
      message(sprintf("Export class filter: Keeping %d columns in classes %s",
                      length(columns_to_keep), paste(export_classes, collapse = ", ")))
    }

    message(sprintf("Columns to keep (%d): %s", length(columns_to_keep), paste(head(columns_to_keep, 20), collapse = ", ")))
    message(sprintf("result_data columns before filtering (%d): %s", ncol(result_data), paste(head(names(result_data), 20), collapse = ", ")))

    # Debug: Check which columns from columns_to_keep are NOT in result_data
    missing_from_result <- setdiff(columns_to_keep, names(result_data))
    if (length(missing_from_result) > 0) {
      message(sprintf("WARNING: %d columns in columns_to_keep are NOT in result_data: %s",
                      length(missing_from_result), paste(head(missing_from_result, 10), collapse = ", ")))
    }

    # Debug: Check which columns from result_data are NOT in columns_to_keep (will be dropped)
    will_be_dropped <- setdiff(names(result_data), columns_to_keep)
    if (length(will_be_dropped) > 0) {
      message(sprintf("INFO: %d columns from result_data will be dropped (not in columns_to_keep): %s",
                      length(will_be_dropped), paste(head(will_be_dropped, 10), collapse = ", ")))
    }

    result_data <- result_data[, names(result_data) %in% columns_to_keep, drop = FALSE]
    message(sprintf("Columns after filtering (%d): %s", ncol(result_data), paste(head(names(result_data), 20), collapse = ", ")))

    # Transform date columns to yyyy-mm-dd format
    # Only process columns that have "date" in their name
    transformed_dates <- 0

    for (col_name in names(result_data)) {
      # Only process if column name contains "date" (case insensitive)
      if (!grepl("date", col_name, ignore.case = TRUE)) next

      tryCatch({
        col_data <- result_data[[col_name]]

        # Skip if column is already all NA or empty
        if (all(is.na(col_data) | col_data == "")) next

        # Try to parse as date
        parsed_date <- as.Date(col_data, tryFormats = c(
          "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
          "%Y/%m/%d", "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y",
          "%Y%m%d", "%m/%d/%y", "%d/%m/%y"
        ))

        # Only update if parsing was successful (not all NA)
        if (!all(is.na(parsed_date))) {
          result_data[[col_name]] <- format(parsed_date, "%Y-%m-%d")
          transformed_dates <- transformed_dates + 1
        }
      }, error = function(e) {
        # If parsing fails, leave the column as-is
      })
    }

    # ============================================================================
    # Calculate Derived Metrics (PricePerSqFt, PricePerLotSF)
    # ============================================================================
    setProgress(value = 0.5, detail = "Calculating derived metrics (PricePerSqFt, etc.)")

    derived_metrics_created <- character(0)

    column_missing_or_empty <- function(data, column_name) {
      if (!(column_name %in% names(data))) return(TRUE)
      col <- data[[column_name]]
      if (is.null(col)) return(TRUE)
      if (is.numeric(col)) {
        return(all(is.na(col)))
      }
      col_chr <- trimws(as.character(col))
      col_chr[is.na(col_chr)] <- ""
      return(!any(nzchar(col_chr)))
    }

    resolve_first_available <- function(primary, fallbacks = character()) {
      candidates <- unique(c(primary, fallbacks))
      for (candidate in candidates) {
        col <- resolve_field_column(candidate)
        if (!is.null(col)) return(col)
        if (candidate %in% names(result_data)) return(candidate)
      }
      NULL
    }

    resolve_field_column <- function(field_name) {
      if (field_name %in% names(result_data)) {
        return(field_name)
      }
      if (use_dest_schema) {
        dest_assign <- rv$dest_assignments %||% character()
        dest_col <- NULL
        if (length(dest_assign) && field_name %in% names(dest_assign)) {
          dest_col <- dest_assign[[field_name]]
        }
        if (!is.null(dest_col) && nzchar(dest_col) && dest_col %in% names(result_data)) {
          return(dest_col)
        }
      }
      NULL
    }

    # --- Price Per Square Foot ---
    price_col <- resolve_field_column("PriceSold")
    sqft_col <- resolve_first_available("SqFt", c("SqFtTotal", "SqFtFinished", "SqFtMain"))
    if (!column_missing_or_empty(result_data, DERIVED_METRIC_PRICE_PER_SQFT)) {
      message("↪ PricePerSqFt already supplied in data; skipping derived calculation.")
    } else if (!is.null(price_col) && !is.null(sqft_col)) {
      tryCatch({
        # Clean commas and currency symbols before converting to numeric
        price <- as.numeric(gsub("[^0-9.-]", "", as.character(result_data[[price_col]])))
        sqft <- as.numeric(gsub("[^0-9.-]", "", as.character(result_data[[sqft_col]])))

        # Only calculate for valid values (Price > 0 and SqFt > 0)
        valid_mask <- !is.na(price) & !is.na(sqft) & price > 0 & sqft > 0

        # Initialize PricePerSqFt column with NA
        ppsf_values <- rep(NA_real_, nrow(result_data))

        # Calculate PricePerSqFt for valid records (rounded to nearest dollar)
        ppsf_values[valid_mask] <- round(price[valid_mask] / sqft[valid_mask])
        result_data[[DERIVED_METRIC_PRICE_PER_SQFT]] <- ppsf_values

        valid_count <- sum(valid_mask)
        invalid_count <- nrow(result_data) - valid_count

        message(sprintf("✓ Calculated PricePerSqFt for %d properties (%d invalid/skipped)",
                       valid_count, invalid_count))
        derived_metrics_created <- c(derived_metrics_created, DERIVED_METRIC_PRICE_PER_SQFT)
      }, error = function(e) {
        message(sprintf("✗ Failed to calculate PricePerSqFt: %s", e$message))
      })
    } else {
      message("↪ PricePerSqFt not calculated (PriceSold or SqFt column missing).")
    }

    # --- Price Per Lot Square Foot ---
    lot_col <- resolve_first_available("LotSF", c("LotS", "LotSize"))
    if (!column_missing_or_empty(result_data, DERIVED_METRIC_PRICE_PER_LOTSF)) {
      message("↪ PricePerLotSF already supplied in data; skipping derived calculation.")
    } else if (!is.null(price_col) && !is.null(lot_col)) {
      tryCatch({
        # Clean commas and currency symbols before converting to numeric
        price <- as.numeric(gsub("[^0-9.-]", "", as.character(result_data[[price_col]])))
        lot_size_raw <- as.numeric(gsub("[^0-9.-]", "", as.character(result_data[[lot_col]])))

        # Auto-detect units and convert to square feet
        # ≤ 500 = acres (convert to sq ft)
        # ≥ 2,000 = square feet (use as-is)
        # 500 < x < 2,000 = ambiguous gray zone (leave as-is, log warning)

        lot_size_sqft <- lot_size_raw
        acres_count <- 0
        sqft_count <- 0
        gray_zone_count <- 0

        for (i in seq_along(lot_size_raw)) {
          if (is.na(lot_size_raw[i])) next

          if (lot_size_raw[i] <= 500) {
            # Treat as acres, convert to square feet
            lot_size_sqft[i] <- lot_size_raw[i] * 43560
            acres_count <- acres_count + 1
          } else if (lot_size_raw[i] >= 2000) {
            # Already in square feet
            sqft_count <- sqft_count + 1
          } else {
            # Gray zone (500-2000): ambiguous, leave unchanged
            gray_zone_count <- gray_zone_count + 1
          }
        }

        # Calculate price per lot square foot
        # PricePerLotSF = Price / LotSqFt
        valid_mask <- !is.na(price) & !is.na(lot_size_sqft) & price > 0 & lot_size_sqft > 0

        # Initialize PricePerLotSF column with NA
        ppa_values <- rep(NA_real_, nrow(result_data))

        # Calculate price per lot square foot (convert acres to sqft above, rounded to nearest dollar)
        ppa_values[valid_mask] <- round(price[valid_mask] / lot_size_sqft[valid_mask])
        result_data[[DERIVED_METRIC_PRICE_PER_LOTSF]] <- ppa_values

        valid_count <- sum(valid_mask)
        invalid_count <- nrow(result_data) - valid_count

        message(sprintf("✓ Calculated PricePerLotSF for %d properties (%d interpreted as acres, %d already sq ft, %d in gray zone, %d invalid/skipped)",
                       valid_count, acres_count, sqft_count, gray_zone_count, invalid_count))

        if (gray_zone_count > 0) {
          message(sprintf("⚠ %d lot size values in ambiguous range (500-2000), treating as-is", gray_zone_count))
        }

        derived_metrics_created <- c(derived_metrics_created, DERIVED_METRIC_PRICE_PER_LOTSF)
      }, error = function(e) {
        message(sprintf("✗ Failed to calculate PricePerLotSF: %s", e$message))
      })
    } else {
      message("↪ PricePerLotSF not calculated (PriceSold or LotSF column missing).")
    }

    # --- SqFtTotal Auto-Derive (from AboveGrade + BelowGrade) ---
    if (isTRUE(sqft_derive_accepted())) {
      sqft_col_name <- if ("SqFtTotal" %in% names(result_data)) "SqFtTotal" else NULL
      # Only derive if SqFtTotal is missing or empty
      sqft_needs_derive <- is.null(sqft_col_name) || column_missing_or_empty(result_data, "SqFtTotal")

      if (sqft_needs_derive) {
        above_col <- resolve_field_column("AboveGradeFinishedArea")
        below_col <- resolve_field_column("BelowGradeFinishedArea")

        if (!is.null(above_col) || !is.null(below_col)) {
          tryCatch({
            above_vals <- if (!is.null(above_col)) {
              suppressWarnings(as.numeric(as.character(result_data[[above_col]])))
            } else {
              rep(0, nrow(result_data))
            }
            below_vals <- if (!is.null(below_col)) {
              suppressWarnings(as.numeric(as.character(result_data[[below_col]])))
            } else {
              rep(0, nrow(result_data))
            }

            # Sum with NA handling: if both are NA, result is NA
            both_na <- is.na(above_vals) & is.na(below_vals)
            above_vals[is.na(above_vals)] <- 0
            below_vals[is.na(below_vals)] <- 0
            sqft_total <- above_vals + below_vals
            sqft_total[both_na] <- NA

            result_data[["SqFtTotal"]] <- sqft_total
            valid_count <- sum(!is.na(sqft_total))
            message(sprintf("✓ Derived SqFtTotal from %s (%d valid values)",
                           paste(c(above_col, below_col)[!sapply(list(above_col, below_col), is.null)], collapse = " + "),
                           valid_count))
          }, error = function(e) {
            message(sprintf("✗ Failed to derive SqFtTotal: %s", e$message))
          })
        } else {
          message("↪ SqFtTotal derivation skipped: neither AboveGrade nor BelowGrade found in output")
        }
      } else {
        message("↪ SqFtTotal already has data; skipping auto-derivation")
      }
    }

    # --- Sale Quarter (SaleQtr) ---
    # Check if SaleQtr column exists and whether it contains date values that need conversion
    sale_qtr_col_exists <- DERIVED_METRIC_SALE_QTR %in% names(result_data)
    sale_qtr_needs_calculation <- FALSE
    sale_date_source_col <- NULL

    if (sale_qtr_col_exists) {
      # Check if the existing SaleQtr column contains date values (not year.quarter format)
      # Year.quarter format is like 2024.3 (numeric, typically < 3000 and has decimal)
      # Date values would parse as dates
      existing_values <- result_data[[DERIVED_METRIC_SALE_QTR]]
      non_na_values <- existing_values[!is.na(existing_values) & nzchar(trimws(as.character(existing_values)))]

      if (length(non_na_values) > 0) {
        # Sample a few values to check the format
        sample_vals <- head(non_na_values, 10)

        # FIRST: Check if values look like year.quarter format (numeric between 1900.1 and 2100.4)
        # This should be checked BEFORE date parsing to avoid misinterpreting numeric values
        numeric_vals <- suppressWarnings(as.numeric(sample_vals))
        # Year.quarter values have specific decimal parts: .1, .2, .3, or .4
        decimal_parts <- round((numeric_vals %% 1) * 10) / 10
        looks_like_year_qtr <- !is.na(numeric_vals) &
                              numeric_vals >= 1900 & numeric_vals <= 2100 &
                              decimal_parts %in% c(0.1, 0.2, 0.3, 0.4)
        year_qtr_rate <- sum(looks_like_year_qtr) / length(sample_vals)

        if (year_qtr_rate > 0.5) {
          # Already in year.quarter format - skip recalculation
          message("↪ SaleQtr already in year.quarter format; skipping derived calculation.")
        } else {
          # Not in year.quarter format - check if it looks like date strings
          # Only consider string values that look like dates (contain / or -)
          sample_strings <- as.character(sample_vals)
          looks_like_date_string <- grepl("[/-]", sample_strings)

          if (sum(looks_like_date_string) > length(sample_vals) * 0.5) {
            # Contains date-like strings - use this column as the source and recalculate
            sale_qtr_needs_calculation <- TRUE
            sale_date_source_col <- DERIVED_METRIC_SALE_QTR
            message("↪ SaleQtr column contains date values - will calculate year.quarter format")
          } else {
            message("↪ SaleQtr column exists but format unclear; skipping derived calculation.")
          }
        }
      } else {
        # Column exists but is empty/NA - try to find another date source
        sale_qtr_needs_calculation <- TRUE
      }
    } else {
      sale_qtr_needs_calculation <- TRUE
    }

    if (sale_qtr_needs_calculation) {
      # Find a date column to use for calculation
      if (is.null(sale_date_source_col)) {
        sale_date_source_col <- resolve_first_available(
          "DateSold",
          c("SaleDate", "SoldDate", "CloseDate", "ClosingDate", "DateClose")
        )
      }

      if (!is.null(sale_date_source_col)) {
        tryCatch({
          sale_dates_raw <- result_data[[sale_date_source_col]]
          date_strings <- as.character(sale_dates_raw)
          date_strings[is.na(date_strings) | !nzchar(trimws(date_strings))] <- NA
          date_only <- sub(" .*", "", date_strings)

          # Try multiple date formats (match the mapping-loop approach)
          parsed_sale_dates <- suppressWarnings(as.Date(date_only, format = "%m/%d/%Y"))
          if (sum(!is.na(parsed_sale_dates)) < sum(!is.na(date_only)) * 0.5) {
            parsed_sale_dates2 <- suppressWarnings(as.Date(date_only, format = "%Y-%m-%d"))
            if (sum(!is.na(parsed_sale_dates2)) > sum(!is.na(parsed_sale_dates))) parsed_sale_dates <- parsed_sale_dates2
          }
          if (sum(!is.na(parsed_sale_dates)) < sum(!is.na(date_only)) * 0.5) {
            parsed_sale_dates3 <- suppressWarnings(as.Date(date_only, format = "%d/%m/%Y"))
            if (sum(!is.na(parsed_sale_dates3)) > sum(!is.na(parsed_sale_dates))) parsed_sale_dates <- parsed_sale_dates3
          }

          valid_dates_mask <- !is.na(parsed_sale_dates)

          sale_qtr_values <- rep(NA_real_, length(parsed_sale_dates))
          if (any(valid_dates_mask)) {
            sale_years <- as.integer(format(parsed_sale_dates[valid_dates_mask], "%Y"))
            sale_months <- as.integer(format(parsed_sale_dates[valid_dates_mask], "%m"))
            sale_quarters <- ceiling(sale_months / 3)
            sale_qtr_values[valid_dates_mask] <- sale_years + (sale_quarters / 10)
          }

          result_data[[DERIVED_METRIC_SALE_QTR]] <- sale_qtr_values
          derived_metrics_created <- c(derived_metrics_created, DERIVED_METRIC_SALE_QTR)
          message(sprintf("✓ Calculated SaleQtr for %d properties using %s", sum(valid_dates_mask), sale_date_source_col))
        }, error = function(e) {
          message(sprintf("⚠ SaleQtr calculation failed for '%s': %s - skipping", sale_date_source_col, e$message))
        })
      } else {
        message("↪ SaleQtr not calculated (Sale Date column missing).")
      }
    }

    # --- Address Assembly (FullAddress and StreetAddress) ---
    # Auto-detect address component columns in the data
    addr_cols <- detect_address_columns(names(result_data))
    message(sprintf("Address detection found: %d components (%s)",
                   length(addr_cols),
                   paste(names(addr_cols), collapse = ", ")))

    # Only attempt assembly if we have at least street number and street name
    has_street_basics <- !is.null(addr_cols$street_number) && !is.null(addr_cols$street_name)

    if (!column_missing_or_empty(result_data, DERIVED_METRIC_FULL_ADDRESS)) {
      message("↪ FullAddress already supplied in data; skipping derived calculation.")
    } else if (has_street_basics) {
      tryCatch({
        # Assemble full address (with city, state, zip)
        full_addresses <- assemble_address(
          data = result_data,
          street_number_col = addr_cols$street_number,
          street_dir_prefix_col = addr_cols$street_dir_prefix,
          street_name_col = addr_cols$street_name,
          street_suffix_col = addr_cols$street_suffix,
          street_dir_suffix_col = addr_cols$street_dir_suffix,
          unit_number_col = addr_cols$unit_number,
          city_col = addr_cols$city,
          state_col = addr_cols$state,
          postal_code_col = addr_cols$postal_code,
          postal_code_plus4_col = addr_cols$postal_code_plus4,
          include_city_state_zip = TRUE,
          include_zip_plus4 = TRUE
        )

        result_data[[DERIVED_METRIC_FULL_ADDRESS]] <- full_addresses
        valid_count <- sum(nzchar(full_addresses))
        message(sprintf("✓ Assembled FullAddress for %d properties", valid_count))
        derived_metrics_created <- c(derived_metrics_created, DERIVED_METRIC_FULL_ADDRESS)
      }, error = function(e) {
        message(sprintf("✗ Failed to assemble FullAddress: %s", e$message))
      })
    } else {
      message("↪ FullAddress not assembled (StreetNumber or StreetName column missing).")
    }

    # Street-only address (without city/state/zip)
    if (!column_missing_or_empty(result_data, DERIVED_METRIC_STREET_ADDRESS)) {
      message("↪ StreetAddress already supplied in data; skipping derived calculation.")
    } else if (has_street_basics) {
      tryCatch({
        street_addresses <- assemble_address(
          data = result_data,
          street_number_col = addr_cols$street_number,
          street_dir_prefix_col = addr_cols$street_dir_prefix,
          street_name_col = addr_cols$street_name,
          street_suffix_col = addr_cols$street_suffix,
          street_dir_suffix_col = addr_cols$street_dir_suffix,
          unit_number_col = addr_cols$unit_number,
          include_city_state_zip = FALSE
        )

        result_data[[DERIVED_METRIC_STREET_ADDRESS]] <- street_addresses
        valid_count <- sum(nzchar(street_addresses))
        message(sprintf("✓ Assembled StreetAddress for %d properties", valid_count))
        derived_metrics_created <- c(derived_metrics_created, DERIVED_METRIC_STREET_ADDRESS)
      }, error = function(e) {
        message(sprintf("✗ Failed to assemble StreetAddress: %s", e$message))
      })
    } else {
      message("↪ StreetAddress not assembled (StreetNumber or StreetName column missing).")
    }

    if (length(derived_metrics_created) > 0) {
      message(sprintf("=== Derived Metrics: %s ===", paste(derived_metrics_created, collapse = ", ")))
    }
    rv$derived_metrics_last_run <- derived_metrics_created

    # ============================================================================
    # End Derived Metrics Calculation
    # ============================================================================

    # ============================================================================
    # Auto-Normalization: DwellingType, Status & SubArea
    # ============================================================================

    # DwellingType: Normalize US MLS / RESO values to CValR standard
    if ("DwellingType" %in% names(result_data)) {
      dwelling_result <- normalize_dwelling_type(result_data[["DwellingType"]])
      result_data[["DwellingType"]] <- dwelling_result$values
      if (length(dwelling_result$warnings) > 0) {
        showNotification(
          sprintf("DwellingType column: %d unrecognized value(s) passed through unchanged: %s",
                  length(dwelling_result$warnings),
                  paste(dwelling_result$warnings, collapse = ", ")),
          type = "warning",
          duration = 8
        )
      }
      message(sprintf("✓ DwellingType normalization applied (%d unique values mapped)",
                      sum(!is.na(CVALR_DWELLING_MAP[tolower(trimws(as.character(result_data[["DwellingType"]])))]))))
    }

    # Status: Normalize MLS abbreviations (ACT→Active, SLD→Sold, PEN→Pending, etc.)
    if ("Status" %in% names(result_data)) {
      status_result <- normalize_status(result_data[["Status"]])
      result_data[["Status"]] <- status_result$values
      if (length(status_result$warnings) > 0) {
        showNotification(
          sprintf("Status column: %d unrecognized value(s) passed through unchanged: %s",
                  length(status_result$warnings),
                  paste(status_result$warnings, collapse = ", ")),
          type = "warning",
          duration = 8
        )
      }
      message(sprintf("✓ Status normalization applied (%d unique values mapped)",
                      sum(!is.na(CVALR_STATUS_MAP[tolower(trimws(as.character(result_data[["Status"]])))]))))
    }

    # SubArea: Coerce to character (numeric codes like 148 should be text)
    if ("SubArea" %in% names(result_data)) {
      result_data[["SubArea"]] <- as.character(result_data[["SubArea"]])
      message("✓ SubArea coerced to character")
    }

    # ============================================================================
    # End Auto-Normalization
    # ============================================================================

    logic_result <- result_data
    if (use_dest_schema) {
      dest_assign <- rv$dest_assignments %||% character()
      if (length(dest_assign)) {
        for (std_field in names(dest_assign)) {
          dest_col <- dest_assign[[std_field]]
          if (!is.null(dest_col) && nzchar(dest_col) && dest_col %in% names(result_data)) {
            logic_result[[std_field]] <- result_data[[dest_col]]
          }
        }
      }
    }

    final_data <- result_data
    derived_cols_available <- derived_metrics_created
    logic_columns_added <- 0
    projected_dest_columns <- 0
    pass_through_added <- 0

    if (use_dest_schema) {
      # Simplified destination mode: mappings already contain dest_col → source_col
      # result_data already has columns renamed from source → dest
      dest_headers <- rv$dest_headers
      projected_dest_columns <- sum(names(mappings) %in% dest_headers)

      # Filter Class 4 columns from destination headers - they should be fully excluded
      # (both header and data) regardless of destination schema
      dest_headers_filtered <- setdiff(dest_headers, class4_excluded)
      if (length(class4_excluded) > 0) {
        excluded_from_dest <- intersect(dest_headers, class4_excluded)
        if (length(excluded_from_dest) > 0) {
          message(sprintf("Class 4 columns excluded from destination schema (%d): %s",
                         length(excluded_from_dest), paste(head(excluded_from_dest, 10), collapse = ", ")))
        }
      }

      # Also filter destination headers based on export class filter
      # When class filter is active, only include destination columns that:
      # 1. Have a mapping to an allowed class source column, OR
      # 2. Are already in result_data (meaning they passed the earlier filtering)
      if (length(export_classes) < 3 && !is.null(classifications)) {
        dest_class_filtered <- character(0)
        for (dest_col in dest_headers_filtered) {
          # Check if this destination column has data in result_data
          # (meaning it was mapped or passed through with an allowed class)
          if (dest_col %in% names(result_data)) {
            # Column exists with data - keep it
            next
          }

          # Column doesn't exist in result_data - check if it should be excluded
          # Check if source column with same name exists and what class it is
          if (dest_col %in% names(original_data)) {
            if (!is.null(overrides[[dest_col]])) {
              col_class <- overrides[[dest_col]]
            } else {
              idx <- which(classifications$column_name == dest_col)
              if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
                col_class <- classifications$class[idx[1]]
              } else {
                col_class <- NA  # Unclassified
              }
            }
            # If source column class is NOT in allowed classes, exclude this destination column
            if (!is.na(col_class) && col_class %in% c(1, 2, 3) && !(col_class %in% export_classes)) {
              dest_class_filtered <- c(dest_class_filtered, dest_col)
            } else if (is.na(col_class)) {
              # Unclassified source column - exclude when filtering is active
              # (user wants specific classes, not "everything else")
              dest_class_filtered <- c(dest_class_filtered, dest_col)
            }
          } else {
            # Destination column doesn't exist in source data at all
            # When class filter is active, exclude these (they'd just be NA padding)
            dest_class_filtered <- c(dest_class_filtered, dest_col)
          }
        }
        if (length(dest_class_filtered) > 0) {
          dest_headers_filtered <- setdiff(dest_headers_filtered, dest_class_filtered)
          message(sprintf("Export class filter: Excluded %d destination columns (no matching allowed-class source): %s",
                          length(dest_class_filtered),
                          paste(head(dest_class_filtered, 10), collapse = ", ")))
        }
      }

      if (isTRUE(input$restrict_to_destination_only)) {
        # ============================================================
        # RESTRICT MODE: Output ONLY the columns the user actually
        # mapped (names(mappings)), plus derived metrics and ListingId
        # if those checkboxes are on. Nothing else gets through.
        # ============================================================
        mapped_cols <- intersect(names(mappings), names(result_data))

        derived_included <- character()
        if (isTRUE(input$include_derived_metrics) && length(derived_cols_available)) {
          derived_included <- intersect(derived_cols_available, names(result_data))
        }

        listing_id_col <- character()
        if (isTRUE(input$include_listing_id) && !is.null(listing_id_auto_detected)) {
          if (listing_id_auto_detected %in% names(result_data)) {
            listing_id_col <- listing_id_auto_detected
          }
        }

        ordered_cols <- unique(c(mapped_cols, derived_included, listing_id_col))
        message(sprintf("RESTRICT MODE: %d mapped + %d derived + %d listing_id = %d total output columns",
                        length(mapped_cols), length(derived_included), length(listing_id_col), length(ordered_cols)))
        message(sprintf("RESTRICT MODE columns: %s", paste(ordered_cols, collapse = ", ")))
      } else {
        # NON-RESTRICT: all dest columns (padded with NA if missing) + everything else
        padded_with_na <- character(0)
        for (col in dest_headers_filtered) {
          if (!(col %in% names(result_data))) {
            result_data[[col]] <- rep(NA_character_, nrow(result_data))
            padded_with_na <- c(padded_with_na, col)
          }
        }
        if (length(padded_with_na) > 0) {
          message(sprintf("Destination columns padded with NA (%d): %s",
                          length(padded_with_na), paste(head(padded_with_na, 15), collapse = ", ")))
        }
        pass_through_cols <- included_unmapped[included_unmapped %in% names(result_data)]
        pass_through_added <- length(pass_through_cols)
        other_cols <- setdiff(names(result_data), dest_headers_filtered)
        logic_columns_added <- length(other_cols)
        ordered_cols <- c(dest_headers_filtered, other_cols)
      }

      final_data <- result_data[, ordered_cols, drop = FALSE]
    }

    logic_mapped_data(logic_result)

    metrics_added <- rv$derived_metrics_last_run %||% character()
    if (!isTRUE(input$include_derived_metrics) && length(metrics_added)) {
      drop_cols <- intersect(names(final_data), metrics_added)
      if (length(drop_cols)) {
        final_data <- final_data[, setdiff(names(final_data), drop_cols), drop = FALSE]
      }
    }

    # Optionally embed mapper build info columns (version, sha, tag)
    mapper_cols <- c(".Mapper_Version", ".Mapper_SHA", ".Mapper_Tag", ".Mapper_Date")
    if (isTRUE(input$embed_build_info)) {
      info_cols <- list(
        .Mapper_Version = APP_VERSION,
        .Mapper_SHA = ver$sha %||% "unknown",
        .Mapper_Tag = ver$tag %||% "",
        .Mapper_Date = APP_VERSION_DATE
      )
      for (nm in names(info_cols)) {
        if (!(nm %in% names(final_data))) {
          final_data[[nm]] <- info_cols[[nm]]
        } else {
          final_data[[nm]] <- info_cols[[nm]]
        }
      }
    } else {
      keep <- setdiff(names(final_data), mapper_cols)
      final_data <- final_data[, keep, drop = FALSE]
    }

    # FINAL SAFETY NET: If restrict is on, hard-filter to only mapped + derived + build columns.
    # This runs regardless of dest/standard mode code path.
    if (isTRUE(input$restrict_to_destination_only)) {
      allowed <- unique(c(
        intersect(names(mappings), names(final_data)),                          # mapped columns
        if (isTRUE(input$include_derived_metrics)) intersect(derived_metrics_created, names(final_data)) else character(),
        if (isTRUE(input$include_listing_id) && !is.null(listing_id_auto_detected)) listing_id_auto_detected else character(),
        if (isTRUE(input$embed_build_info)) intersect(c(".Mapper_Version", ".Mapper_SHA", ".Mapper_Tag", ".Mapper_Date"), names(final_data)) else character()
      ))
      before_count <- ncol(final_data)
      final_data <- final_data[, intersect(allowed, names(final_data)), drop = FALSE]
      message(sprintf("RESTRICT FINAL FILTER: %d -> %d columns", before_count, ncol(final_data)))

      # Rename ColumnMapper standard field names to CValR 4.0 expected column names.
      # Verified against CValR 4.0 DataImport.R ColNameMappings.
      cvalr_rename <- c(
        ListingId    = "MLS #",
        ListingKey   = "Listing Key Numeric",
        PriceSold    = "Sold Price",
        PriceOriginal = "Original List Price",
        PriceListed  = "List Price",
        DateSold     = "Sold Date",
        DateListed   = "On Market Date",
        DatePending  = "Purchase Contract Date",
        OffMarketDate = "Off Market Date",
        PostalCode   = "Postal Code",
        SubArea      = "Minor / Sub Area",
        Region       = "Major Area / Region",
        YrBlt        = "Year Built",
        StyleStoreys = "Architectural Style",
        Beds         = "Bedrooms Total",
        BA           = "Bathrooms Full",
        PB           = "Bathrooms Half",
        EnsBa        = "Ensuite Bathrooms",
        SqFtTotal    = "Total Finished SqFt",
        AboveGradeFinishedArea = "Above Grade Finished Sqft",
        BelowGradeFinishedArea = "Below Grade Finished SqFt",
        BasementYN   = "Basement YN",
        BasementFeatures = "Basement",
        GarSp        = "Garage Spaces",
        GarageYN     = "Garage YN",
        PoolYN       = "Pool YN",
        LotS         = "Lot Acres",
        WaterfrontYN = "Waterfront YN",
        WaterFrontage = "Waterfrontage",
        WaterInfluence = "Waterfront Features",
        SewerType    = "Sewer",
        WaterSupply  = "Water Source",
        PID          = "Parcel ID",
        LegalDescription = "Tax Legal Description",
        RemarksPublic = "Public Remarks",
        RemarksSales = "REALTOR\u00ae Remarks",
        SpecialListingConditions = "Special Listing Conditions",
        DwellingType = "Property Sub Type",
        Construction = "Construction Materials",
        StrataFees   = "Strata Fees",
        TotalKitchens = "Total Kitchens",
        LotDimensions = "Lot Size Dimensions L x W",
        TitleChangeDate = "Title/Ownership Change Date"
      )
      col_names <- names(final_data)
      renamed_count <- 0
      for (i in seq_along(col_names)) {
        new_name <- cvalr_rename[col_names[i]]
        if (!is.na(new_name)) {
          col_names[i] <- new_name
          renamed_count <- renamed_count + 1
        }
      }
      names(final_data) <- col_names
      message(sprintf("RESTRICT RENAME: %d columns renamed to CValR dest template names", renamed_count))

      # Safety net: CValR requires "MLS #". If we have "Listing Key Numeric" but
      # no "MLS #", duplicate it so CValR gets both identifiers.
      if ("Listing Key Numeric" %in% names(final_data) && !("MLS #" %in% names(final_data))) {
        final_data[["MLS #"]] <- final_data[["Listing Key Numeric"]]
        message("CValR safety net: Duplicated 'Listing Key Numeric' into 'MLS #'")
      }

      message(sprintf("RESTRICT FINAL COLUMNS: %s", paste(names(final_data), collapse = ", ")))
    }

    mapped_data(final_data)

    # Debug output
    message("=== MAPPED DATA SUMMARY ===")
    message(sprintf("Total rows (final output): %d", nrow(final_data)))
    message(sprintf("Output columns: %s", paste(names(final_data), collapse = ", ")))
    if ("DateSold" %in% names(logic_result)) {
      message(sprintf("Logic DateSold sample (first 3): %s", paste(head(logic_result$DateSold, 3), collapse = ", ")))
    }
    if ("PriceSold" %in% names(logic_result)) {
      message(sprintf("Logic PriceSold sample (first 3): %s", paste(head(logic_result$PriceSold, 3), collapse = ", ")))
    }

    setProgress(value = 1.0, detail = "Complete!")

    base_msg <- sprintf("Successfully applied %d mappings, included %d unmapped columns, and transformed %d date columns!",
                        length(mappings), length(included_unmapped), transformed_dates)
    if (use_dest_schema) {
      base_msg <- sprintf("%s Projected %d destination column%s and passed through %d source column%s.",
                          base_msg,
                          projected_dest_columns,
                          if (projected_dest_columns == 1) "" else "s",
                          pass_through_added,
                          if (pass_through_added == 1) "" else "s")
      if (!isTRUE(input$restrict_to_destination_only)) {
        base_msg <- sprintf("%s Added %d standardized column%s because 'Restrict to destination' is off.",
                            base_msg,
                            logic_columns_added,
                            if (logic_columns_added == 1) "" else "s")
      }
    }

    }) # End withProgress

    showNotification(base_msg, type = "message")
  })

  # ============================================================================
  # DATA ENRICHMENT: GEOCODING
  # ============================================================================

  output$enrich_data_section <- renderUI({
    data <- mapped_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)

    has_lat <- "Latitude" %in% names(data)
    has_lng <- "Longitude" %in% names(data)

    if (!has_lat && !has_lng) {
      return(tags$div(
        style = "margin: 15px 0; padding: 12px 16px; background: #f0f0f0; border-left: 4px solid #999; border-radius: 4px;",
        tags$strong("Geocoding: "),
        "No Latitude/Longitude columns mapped. Map these columns to enable geocoding."
      ))
    }

    # Count rows missing coordinates
    lat_vals <- if (has_lat) data[["Latitude"]] else rep(NA, nrow(data))
    lng_vals <- if (has_lng) data[["Longitude"]] else rep(NA, nrow(data))
    missing_coords <- is.na(lat_vals) | is.na(lng_vals) |
                      lat_vals == 0 | lng_vals == 0 |
                      lat_vals == "" | lng_vals == ""
    n_missing <- sum(missing_coords)

    if (n_missing == 0) {
      return(tags$div(
        style = "margin: 15px 0; padding: 12px 16px; background: #dff0d8; border-left: 4px solid #5cb85c; border-radius: 4px;",
        tags$strong("Geocoding: "),
        sprintf("All %d rows have coordinates. No geocoding needed.", nrow(data))
      ))
    }

    # Check for usable address columns
    addr_cols <- intersect(c("FullAddress", "Address", "UnparsedAddress", "StreetAddress"), names(data))
    has_city <- "City" %in% names(data)
    has_state <- "StateOrProvince" %in% names(data)

    if (length(addr_cols) == 0) {
      return(tags$div(
        style = "margin: 15px 0; padding: 12px 16px; background: #fcf8e3; border-left: 4px solid #f0ad4e; border-radius: 4px;",
        tags$strong("Geocoding: "),
        sprintf("%d of %d rows are missing coordinates, but no address column is available for geocoding.",
                n_missing, nrow(data)),
        tags$br(),
        tags$small("Map an address column (FullAddress, Address, StreetAddress) to enable geocoding.")
      ))
    }

    tags$div(
      style = "margin: 15px 0; padding: 12px 16px; background: #fcf8e3; border-left: 4px solid #f0ad4e; border-radius: 4px;",
      tags$strong("Geocoding: "),
      sprintf("%d of %d rows are missing coordinates.", n_missing, nrow(data)),
      tags$br(),
      tags$small(
        style = "color: #666;",
        sprintf("Address source: %s%s",
                addr_cols[1],
                if (has_city || has_state)
                  paste0(" + ", paste(intersect(c("City", "StateOrProvince"), names(data)), collapse = ", "))
                else "")
      ),
      tags$br(), tags$br(),
      actionButton(
        "enrich_geocode",
        sprintf("Geocode %d Row%s (ArcGIS)", n_missing, if (n_missing == 1) "" else "s"),
        icon = icon("globe"),
        class = "btn-warning btn-sm"
      )
    )
  })

  observeEvent(input$enrich_geocode, {
    if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
      showNotification(
        "tidygeocoder package is required for geocoding. Install with: install.packages('tidygeocoder')",
        type = "error", duration = 10
      )
      return()
    }

    data <- mapped_data()
    if (is.null(data) || nrow(data) == 0) return()

    has_lat <- "Latitude" %in% names(data)
    has_lng <- "Longitude" %in% names(data)
    if (!has_lat && !has_lng) return()

    # Identify rows needing geocoding
    lat_vals <- if (has_lat) data[["Latitude"]] else rep(NA, nrow(data))
    lng_vals <- if (has_lng) data[["Longitude"]] else rep(NA, nrow(data))
    missing_mask <- is.na(lat_vals) | is.na(lng_vals) |
                    lat_vals == 0 | lng_vals == 0 |
                    lat_vals == "" | lng_vals == ""
    missing_idx <- which(missing_mask)

    if (length(missing_idx) == 0) {
      showNotification("All rows already have coordinates.", type = "message")
      return()
    }

    # Build geocode addresses
    addr_cols <- intersect(c("FullAddress", "Address", "UnparsedAddress", "StreetAddress"), names(data))
    has_city <- "City" %in% names(data)
    has_state <- "StateOrProvince" %in% names(data)

    subset_data <- data[missing_idx, , drop = FALSE]

    if ("FullAddress" %in% names(subset_data) && any(nzchar(as.character(subset_data[["FullAddress"]])))) {
      subset_data$.geocode_addr <- as.character(subset_data[["FullAddress"]])
    } else {
      # Build from components
      addr_base <- if (length(addr_cols) > 0) as.character(subset_data[[addr_cols[1]]]) else ""
      city_part <- if (has_city) as.character(subset_data[["City"]]) else ""
      state_part <- if (has_state) as.character(subset_data[["StateOrProvince"]]) else ""
      subset_data$.geocode_addr <- trimws(paste(addr_base, city_part, state_part))
    }

    # Filter to rows with usable addresses
    has_addr <- nzchar(subset_data$.geocode_addr) & !is.na(subset_data$.geocode_addr)
    if (sum(has_addr) == 0) {
      showNotification("No usable addresses found for geocoding.", type = "warning")
      return()
    }

    geocode_subset <- subset_data[has_addr, , drop = FALSE]
    n_to_geocode <- nrow(geocode_subset)
    geocode_rows <- missing_idx[has_addr]

    withProgress(message = "Geocoding addresses...", value = 0, {
      tryCatch({
        setProgress(value = 0.1, detail = sprintf("Sending %d addresses to ArcGIS...", n_to_geocode))

        geocoded <- tidygeocoder::geocode(
          geocode_subset,
          address = .geocode_addr,
          method = "arcgis",
          quiet = TRUE
        )

        setProgress(value = 0.8, detail = "Merging results...")

        # tidygeocoder returns columns named 'lat' and 'long'
        gc_lat <- geocoded[["lat"]]
        gc_lng <- geocoded[["long"]]

        success_count <- sum(!is.na(gc_lat) & !is.na(gc_lng))
        fail_count <- n_to_geocode - success_count

        # Merge back into mapped_data
        updated_data <- data
        if (has_lat) {
          updated_data[["Latitude"]][geocode_rows] <- ifelse(
            !is.na(gc_lat), gc_lat, updated_data[["Latitude"]][geocode_rows]
          )
        }
        if (has_lng) {
          updated_data[["Longitude"]][geocode_rows] <- ifelse(
            !is.na(gc_lng), gc_lng, updated_data[["Longitude"]][geocode_rows]
          )
        }

        # Ensure numeric type
        if (has_lat) updated_data[["Latitude"]] <- as.numeric(updated_data[["Latitude"]])
        if (has_lng) updated_data[["Longitude"]] <- as.numeric(updated_data[["Longitude"]])

        mapped_data(updated_data)

        # Also update logic_mapped_data if it exists
        logic_data <- logic_mapped_data()
        if (!is.null(logic_data)) {
          if (has_lat && "Latitude" %in% names(logic_data)) {
            logic_data[["Latitude"]][geocode_rows] <- updated_data[["Latitude"]][geocode_rows]
          }
          if (has_lng && "Longitude" %in% names(logic_data)) {
            logic_data[["Longitude"]][geocode_rows] <- updated_data[["Longitude"]][geocode_rows]
          }
          logic_mapped_data(logic_data)
        }

        setProgress(value = 1.0, detail = "Done!")

        msg <- sprintf("Geocoding complete: %d of %d addresses resolved.", success_count, n_to_geocode)
        if (fail_count > 0) {
          msg <- sprintf("%s %d could not be geocoded.", msg, fail_count)
        }
        showNotification(msg, type = if (fail_count == 0) "message" else "warning", duration = 8)

      }, error = function(e) {
        showNotification(
          sprintf("Geocoding failed: %s", e$message),
          type = "error", duration = 10
        )
      })
    })
  })

  # ============================================================================
  # COLUMN CLASSIFICATION UI OUTPUTS
  # ============================================================================

  # Summary badge showing class counts
  output$classification_summary_badge <- renderUI({
    analysis <- rv$column_analysis
    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides

    if (is.null(analysis) || is.null(classifications)) {
      return(tags$span(
        style = "color: #888; font-size: 13px;",
        "Load data to analyze columns"
      ))
    }

    # Apply overrides to get accurate counts
    effective_classes <- classifications$class
    names(effective_classes) <- classifications$column_name
    if (!is.null(overrides) && length(overrides) > 0) {
      for (col_name in names(overrides)) {
        if (col_name %in% names(effective_classes)) {
          effective_classes[col_name] <- overrides[[col_name]]
        }
      }
    }

    # Count by class (with overrides applied)
    class_counts <- table(factor(effective_classes, levels = c(1, 2, 3, 4)))
    # Count issues respecting dismissed flags
    dismissed <- rv$dismissed_flags
    issue_count <- sum(sapply(seq_len(nrow(analysis)), function(i) {
      f <- analysis$flags[[i]]
      col_name <- analysis$column_name[i]
      if (is.null(f) || length(f) == 0 || !any(nzchar(f))) return(FALSE)
      active_flags <- f[nzchar(f)]
      if (!is.null(dismissed[[col_name]])) {
        active_flags <- setdiff(active_flags, dismissed[[col_name]])
      }
      length(active_flags) > 0
    }))

    tags$div(
      style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
      tags$span(
        class = "badge",
        style = "background: #5cb85c; color: white; padding: 4px 8px;",
        sprintf("Class 1: %d", class_counts["1"])
      ),
      tags$span(
        class = "badge",
        style = "background: #5bc0de; color: white; padding: 4px 8px;",
        sprintf("Class 2: %d", class_counts["2"])
      ),
      tags$span(
        class = "badge",
        style = "background: #f0ad4e; color: white; padding: 4px 8px;",
        sprintf("Class 3: %d", class_counts["3"])
      ),
      tags$span(
        class = "badge",
        style = "background: #d9534f; color: white; padding: 4px 8px;",
        sprintf("Class 4: %d", class_counts["4"])
      ),
      if (issue_count > 0) {
        tags$span(
          class = "badge",
          style = "background: #555; color: white; padding: 4px 8px;",
          sprintf("Issues: %d", issue_count)
        )
      }
    )
  })

  # Filtered classification data reactive
  filtered_classification_data <- reactive({
    analysis <- rv$column_analysis
    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides

    if (is.null(analysis) || is.null(classifications)) {
      return(NULL)
    }

    # Merge analysis and classification data
    merged <- merge(
      analysis[, c("column_name", "fill_rate", "unique_count", "inferred_type", "flags", "sample_values")],
      classifications[, c("column_name", "class", "reason", "confidence")],
      by = "column_name",
      all.x = TRUE
    )

    # Apply any user overrides
    for (col_name in names(overrides)) {
      idx <- which(merged$column_name == col_name)
      if (length(idx) > 0) {
        merged$class[idx] <- overrides[[col_name]]
        merged$reason[idx] <- paste(merged$reason[idx], "(User Override)")
      }
    }

    # Add has_issues flag for filtering (respecting dismissed flags)
    dismissed <- rv$dismissed_flags
    merged$has_issues <- sapply(seq_len(nrow(merged)), function(i) {
      col_name <- merged$column_name[i]
      f <- merged$flags[[i]]
      if (is.null(f) || length(f) == 0 || !any(nzchar(f))) return(FALSE)
      # Remove dismissed flags for this column
      active_flags <- f[nzchar(f)]
      if (!is.null(dismissed[[col_name]])) {
        active_flags <- setdiff(active_flags, dismissed[[col_name]])
      }
      length(active_flags) > 0
    })

    # Apply filter (single dropdown now handles class + special views)
    filter_class <- input$classification_filter_class

    if (!is.null(filter_class) && filter_class != "all") {
      if (filter_class %in% c("1", "2", "3", "4")) {
        # Filter by class number
        class_match <- merged$class == as.integer(filter_class)
        class_match[is.na(class_match)] <- FALSE
        merged <- merged[class_match, , drop = FALSE]
      } else if (filter_class == "unclassified") {
        # Show columns with no class assigned
        merged <- merged[is.na(merged$class), , drop = FALSE]
      } else if (filter_class == "has_issues") {
        # Show columns with any issues (flags)
        merged <- merged[merged$has_issues, , drop = FALSE]
      }
    }

    merged
  })

  # Classification table output
  output$classification_table <- DT::renderDataTable({
    # Explicitly depend on overrides to force re-render when they change
    overrides_trigger <- rv$classification_overrides

    data <- tryCatch({
      filtered_classification_data()
    }, error = function(e) {
      message(sprintf("Classification filter error: %s", e$message))
      NULL
    })

    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No columns to display. Load data or adjust filters."),
        options = list(dom = "t")
      ))
    }

    # Format for display with NA handling
    display_data <- tryCatch({
      # Calculate fill_pct as numeric for styling
      fill_pct <- ifelse(is.na(data$fill_rate), 0, data$fill_rate * 100)

      data.frame(
        Column = data$column_name,
        Class = sapply(data$class, function(c) {
          if (is.na(c)) return("Unclassified")
          switch(as.character(c),
            "1" = "1 (Essential)",
            "2" = "2 (Supplemental)",
            "3" = "3 (Marginal)",
            "4" = "4 (Exclude)",
            as.character(c)
          )
        }),
        `Fill %` = fill_pct,
        Unique = ifelse(is.na(data$unique_count), 0, data$unique_count),
        Type = ifelse(is.na(data$inferred_type), "unknown", data$inferred_type),
        Issues = sapply(seq_len(nrow(data)), function(i) {
          f <- data$flags[[i]]
          col_name <- data$column_name[i]
          if (is.null(f) || length(f) == 0 || all(is.na(f)) || all(f == "")) return("")
          active_flags <- f[!is.na(f) & f != ""]
          # Remove dismissed flags
          dismissed <- rv$dismissed_flags
          if (!is.null(dismissed[[col_name]])) {
            active_flags <- setdiff(active_flags, dismissed[[col_name]])
          }
          paste(active_flags, collapse = ", ")
        }),
        Reason = ifelse(is.na(data$reason), "", data$reason),
        Sample = sapply(data$sample_values, function(s) {
          if (is.null(s) || length(s) == 0) return("")
          s <- s[!is.na(s)]
          if (length(s) == 0) return("")
          paste(head(s, 3), collapse = "; ")
        }),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, error = function(e) {
      message(sprintf("Classification display format error: %s", e$message))
      data.frame(Message = sprintf("Error formatting data: %s", e$message))
    })

    # Build the datatable with error handling
    tryCatch({
      dt <- DT::datatable(
        display_data,
        class = "display compact stripe",
        selection = "multiple",
        options = list(
          pageLength = 15,
          lengthMenu = list(c(15, 25, 50, 100), c("15", "25", "50", "100")),
          stateSave = TRUE,
          scrollX = TRUE,
          scrollY = "500px",
          scrollCollapse = TRUE,
          dom = "lftip",
          drawCallback = DT::JS("function() { $(this.api().table().container()).find('.dataTables_scrollBody').scrollTop(0); }"),
          columnDefs = list(
            list(width = "150px", targets = 0),
            list(width = "100px", targets = 1),
            list(width = "70px", targets = 2),
            list(width = "70px", targets = 3),
            list(width = "80px", targets = 4),
            list(width = "120px", targets = 5),
            list(width = "200px", targets = 6),
            list(width = "200px", targets = 7)
          ),
          order = list(list(1, "asc"), list(2, "desc"))
        ),
        rownames = FALSE
      )

      # Format Fill % as percentage with 1 decimal place
      dt <- DT::formatRound(dt, "Fill %", digits = 1)

      # Color code the Class column
      dt <- DT::formatStyle(dt, "Class",
        backgroundColor = DT::styleEqual(
          c("1 (Essential)", "2 (Supplemental)", "3 (Marginal)", "4 (Exclude)", "Unclassified"),
          c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da", "#e0e0e0")
        )
      )

      # Color code Fill % by value
      dt <- DT::formatStyle(dt, "Fill %",
        backgroundColor = DT::styleInterval(
          c(5, 20),
          c("#f8d7da", "#fff3cd", "transparent")
        )
      )

      dt
    }, error = function(e) {
      message(sprintf("DT rendering error: %s", e$message))
      DT::datatable(
        data.frame(Error = sprintf("Table render error: %s", e$message)),
        options = list(dom = "t")
      )
    })
  })

  # Duplicate warning UI (deprecated - duplicates now handled at import)
  output$duplicate_warning <- renderUI({
    NULL
  })

  # Bulk action: Set low-value columns to Class 3
  observeEvent(input$apply_bulk_class3, {
    tryCatch({
      analysis <- rv$column_analysis
      classifications <- rv$column_classifications

      if (is.null(analysis) || is.null(classifications)) {
        showNotification("No data loaded to classify", type = "warning")
        return()
      }

      overrides <- rv$classification_overrides
      if (is.null(overrides)) overrides <- list()

      # Find columns that should be Class 3:
      # - VERY_LOW_FILL (<5%)
      # - NO_VARIANCE (only 1 unique value)
      # - Already identified duplicates

      columns_to_override <- character()

      for (i in seq_len(nrow(analysis))) {
        col_name <- analysis$column_name[i]
        flags <- analysis$flags[[i]]

        # Skip if already Class 3 (check override first, then classification)
        current_class <- if (!is.null(overrides[[col_name]])) {
          overrides[[col_name]]
        } else {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0) classifications$class[idx[1]] else NA
        }

        if (!is.na(current_class) && current_class == 3) next

        # Check for low-value indicators
        if ("VERY_LOW_FILL" %in% flags || "NO_VARIANCE" %in% flags) {
          overrides[[col_name]] <- 3
          columns_to_override <- c(columns_to_override, col_name)
        }
      }

      rv$classification_overrides <- overrides

      if (length(columns_to_override) > 0) {
        showNotification(
          sprintf("Set %d low-value columns to Class 3 (Marginal)", length(unique(columns_to_override))),
          type = "message",
          duration = 4
        )
      } else {
        showNotification("No additional columns to reclassify as Class 3", type = "message", duration = 3)
      }
    }, error = function(e) {
      message(sprintf("Bulk class 3 error: %s", e$message))
      showNotification(sprintf("Error: %s", e$message), type = "error", duration = 6)
    })
  })

  # Helper function to set selected rows to a specific class
  set_selected_to_class <- function(target_class) {
    tryCatch({
      selected_rows <- input$classification_table_rows_selected

      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("No rows selected. Click on rows in the table to select them first.",
                        type = "warning", duration = 4)
        return()
      }

      # Get the filtered data to find column names
      filtered_data <- filtered_classification_data()
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        showNotification("No data available", type = "warning")
        return()
      }

      # Get column names from selected rows
      selected_columns <- filtered_data$column_name[selected_rows]

      if (length(selected_columns) == 0) {
        showNotification("Could not identify selected columns", type = "warning")
        return()
      }

      # Update overrides
      overrides <- rv$classification_overrides
      if (is.null(overrides)) overrides <- list()

      for (col_name in selected_columns) {
        overrides[[col_name]] <- target_class
      }

      rv$classification_overrides <- overrides

      class_names <- c("1" = "Class 1 (Essential)", "2" = "Class 2 (Supplemental)",
                       "3" = "Class 3 (Marginal)", "4" = "Class 4 (Exclude)")

      # Stay on current filter - table will refresh automatically via reactive dependency
      # Reclassified columns will disappear from current view (moved to new class)
      showNotification(
        sprintf("Set %d column(s) to %s",
                length(selected_columns), class_names[as.character(target_class)]),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      message(sprintf("Set selected class error: %s", e$message))
      showNotification(sprintf("Error: %s", e$message), type = "error", duration = 6)
    })
  }

  # Button handlers for setting selected rows to each class
  observeEvent(input$set_selected_class1, {
    set_selected_to_class(1)
  })

  observeEvent(input$set_selected_class2, {
    set_selected_to_class(2)
  })

  observeEvent(input$set_selected_class3, {
    set_selected_to_class(3)
  })

  observeEvent(input$set_selected_class4, {
    set_selected_to_class(4)
  })

  # Track Class 3 columns that have been mapped (to offer upgrade prompt)
  class3_mapped_columns <- reactiveVal(character(0))

  # Observer to detect when a Class 3 column is selected in a mapping dropdown
  observe({
    # Get all current selections
    selections <- get_all_selections()
    if (length(selections) == 0) return()

    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides
    if (is.null(classifications)) return()

    # Helper to get effective class
    get_class <- function(col_name) {
      if (!is.null(overrides[[col_name]])) return(overrides[[col_name]])
      idx <- which(classifications$column_name == col_name)
      if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
        return(classifications$class[idx[1]])
      }
      return(NA)
    }

    # Find all currently mapped columns that are Class 3
    all_mapped <- unique(unlist(selections))
    all_mapped <- all_mapped[!is.na(all_mapped) & nzchar(all_mapped)]

    class3_currently_mapped <- Filter(function(col) {
      cls <- get_class(col)
      !is.na(cls) && cls == 3
    }, all_mapped)

    # Check if there are new Class 3 columns mapped (not previously seen)
    previously_mapped <- class3_mapped_columns()
    new_class3 <- setdiff(class3_currently_mapped, previously_mapped)

    if (length(new_class3) > 0) {
      # Update tracked list
      class3_mapped_columns(unique(c(previously_mapped, new_class3)))

      # Show prompt for the first new Class 3 column
      col_to_prompt <- new_class3[1]

      showModal(modalDialog(
        title = tags$span(icon("arrow-up"), " Upgrade Classification?"),
        tags$div(
          tags$p(
            "You've mapped ",
            tags$strong(col_to_prompt),
            ", which is classified as ",
            tags$span("Class 3 (Marginal)", style = "background: #f0ad4e; color: white; padding: 2px 6px; border-radius: 3px;"),
            "."
          ),
          tags$p("Since you're using this column in your mapping, would you like to upgrade it to Class 2 (Supplemental)?"),
          tags$hr(),
          tags$small(
            style = "color: #666;",
            "Class 2 columns are considered valuable for analysis. ",
            "Class 3 columns will still be included in exports but are marked as lower priority."
          )
        ),
        footer = tagList(
          actionButton("upgrade_class3_yes", "Yes, upgrade to Class 2", class = "btn-primary"),
          actionButton("upgrade_class3_no", "No, keep as Class 3", class = "btn-default"),
          tags$input(type = "hidden", id = "upgrade_class3_column", value = col_to_prompt)
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }
  })

  # Handle Class 3 upgrade confirmation
  observeEvent(input$upgrade_class3_yes, {
    col_name <- input$upgrade_class3_column
    if (!is.null(col_name) && nzchar(col_name)) {
      overrides <- rv$classification_overrides
      if (is.null(overrides)) overrides <- list()
      overrides[[col_name]] <- 2
      rv$classification_overrides <- overrides

      showNotification(
        sprintf("Upgraded '%s' to Class 2 (Supplemental)", col_name),
        type = "message",
        duration = 4
      )
    }
    removeModal()
  })

  # Handle Class 3 upgrade decline
  observeEvent(input$upgrade_class3_no, {
    removeModal()
  })

  # ============================================================================
  # Class 3 Review Card Observers (Accept/Skip/Reconsider/Bulk)
  # ============================================================================

  # Generic observer pattern for Accept buttons (dynamically generated IDs)
  observe({
    # Find all accept_class3_* buttons that have been clicked
    # We need to watch for any input that starts with "accept_class3_"
    input_names <- names(input)
    accept_buttons <- input_names[grepl("^accept_class3_", input_names)]

    lapply(accept_buttons, function(btn_id) {
      observeEvent(input[[btn_id]], {
        # Extract the target field name from button ID
        target_field <- sub("^accept_class3_", "", btn_id)

        # Get the chosen classification from radio button
        choice_id <- paste0("class3_choice_", target_field)
        new_class <- input[[choice_id]] %||% "3"

        # Update review state to "accepted"
        states <- class3_review_states()
        states[[target_field]] <- "accepted"
        class3_review_states(states)

        # Find the actual source column name from caches
        dest_cache <- destination_suggestion_cache()
        cvalr_cache <- suggestion_cache()

        source_col <- NULL
        is_dest_mode <- FALSE
        if (!is.null(dest_cache[[target_field]])) {
          entry <- dest_cache[[target_field]]
          # Handle both list format (with $column) and atomic vector format
          source_col <- if (is.list(entry)) entry$column else as.character(entry)[1]
          is_dest_mode <- TRUE
        } else if (!is.null(cvalr_cache[[target_field]])) {
          entry <- cvalr_cache[[target_field]]
          # Handle both list format (with $column) and atomic vector format
          source_col <- if (is.list(entry)) entry$column else as.character(entry)[1]
          is_dest_mode <- FALSE
        }

        # Update the dropdown selection to actually apply the mapping
        if (!is.null(source_col) && nzchar(source_col)) {
          # Store in accepted mappings for immediate availability (async fix)
          accepted_mappings <- class3_accepted_mappings()
          accepted_mappings[[target_field]] <- source_col
          class3_accepted_mappings(accepted_mappings)

          if (is_dest_mode) {
            input_id <- paste0("dest_", target_field, "_selector")
          } else {
            input_id <- paste0("mapping_", target_field)
          }
          updateSelectizeInput(session, input_id, selected = source_col)
          message(sprintf("[Accept Class 3] Updated dropdown '%s' to '%s'", input_id, source_col))
        }

        # Apply classification override if changed
        if (new_class != "3") {
          overrides <- rv$classification_overrides
          if (is.null(overrides)) overrides <- list()

          if (!is.null(source_col) && nzchar(source_col)) {
            overrides[[source_col]] <- as.integer(new_class)
            rv$classification_overrides <- overrides

            class_names <- c("1" = "Class 1 (Essential)", "2" = "Class 2 (Supplemental)")
            showNotification(
              sprintf("Accepted mapping and upgraded '%s' to %s", source_col, class_names[[new_class]]),
              type = "message",
              duration = 4
            )
          } else {
            showNotification("Accepted mapping", type = "message", duration = 3)
          }
        } else {
          showNotification("Accepted mapping as Class 3", type = "message", duration = 3)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Generic observer pattern for Skip buttons
  observe({
    input_names <- names(input)
    skip_buttons <- input_names[grepl("^skip_class3_", input_names)]

    lapply(skip_buttons, function(btn_id) {
      observeEvent(input[[btn_id]], {
        target_field <- sub("^skip_class3_", "", btn_id)

        # Update review state to "skipped"
        states <- class3_review_states()
        states[[target_field]] <- "skipped"
        class3_review_states(states)

        showNotification("Skipped mapping", type = "warning", duration = 3)
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Generic observer pattern for Reconsider links
  observe({
    input_names <- names(input)
    reconsider_links <- input_names[grepl("^reconsider_class3_", input_names)]

    lapply(reconsider_links, function(link_id) {
      observeEvent(input[[link_id]], {
        target_field <- sub("^reconsider_class3_", "", link_id)

        # Reset review state to "pending"
        states <- class3_review_states()
        states[[target_field]] <- "pending"
        class3_review_states(states)

        showNotification("Restored for review", type = "message", duration = 3)
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ============================================================================
  # Derived Metric Observers (Calculate/Undo buttons in mapping cards)
  # ============================================================================

  # Generic observer pattern for "Calculate [Metric]" buttons
  observe({
    input_names <- names(input)
    derive_buttons <- input_names[grepl("^derive_metric_", input_names)]

    lapply(derive_buttons, function(btn_id) {
      observeEvent(input[[btn_id]], {
        # Extract the metric name from button ID
        metric_name <- sub("^derive_metric_", "", btn_id)
        # Reverse make.names transformation (. becomes original character)
        # For our simple metric names, make.names doesn't change them

        # Add to opted derived metrics
        opted <- opted_derived_metrics()
        opted[[metric_name]] <- TRUE
        opted_derived_metrics(opted)

        # Trigger UI refresh
        ui_refresh_trigger(ui_refresh_trigger() + 1)

        req_info <- DERIVED_METRIC_REQUIREMENTS[[metric_name]]
        metric_label <- if (!is.null(req_info)) req_info$label else metric_name

        showNotification(
          sprintf("%s will be calculated when you apply mappings", metric_label),
          type = "message",
          duration = 4
        )
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Generic observer pattern for "Undo" links (undo derive choice)
  observe({
    input_names <- names(input)
    undo_buttons <- input_names[grepl("^undo_derive_", input_names)]

    lapply(undo_buttons, function(btn_id) {
      observeEvent(input[[btn_id]], {
        # Extract the metric name from button ID
        metric_name <- sub("^undo_derive_", "", btn_id)

        # Remove from opted derived metrics
        opted <- opted_derived_metrics()
        opted[[metric_name]] <- NULL
        opted_derived_metrics(opted)

        # Trigger UI refresh
        ui_refresh_trigger(ui_refresh_trigger() + 1)

        showNotification("Removed from calculation list", type = "message", duration = 3)
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Clear mapping search button (shown when search returns no results)
  observeEvent(input$clear_mapping_search, {
    updateTextInput(session, "mapping_search", value = "")
  })

  # Bulk Accept All Class 3 button
  observeEvent(input$accept_all_class3, {
    classifications <- rv$column_classifications
    overrides <- rv$classification_overrides

    if (is.null(classifications)) {
      showNotification("No classifications available. Please upload data first.", type = "warning", duration = 4)
      return()
    }

    # Helper to get effective class
    get_class <- function(col_name) {
      if (!is.null(overrides[[col_name]])) return(overrides[[col_name]])
      idx <- which(classifications$column_name == col_name)
      if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
        return(classifications$class[idx[1]])
      }
      return(NA)
    }

    # Find all Class 3 columns
    class3_cols <- classifications$column_name[!is.na(classifications$class) & classifications$class == 3]
    # Also include any overridden to Class 3
    for (col in names(overrides)) {
      if (overrides[[col]] == 3 && !(col %in% class3_cols)) {
        class3_cols <- c(class3_cols, col)
      }
    }
    # Exclude any that were overridden away from Class 3
    class3_cols <- Filter(function(col) {
      cls <- get_class(col)
      !is.na(cls) && cls == 3
    }, class3_cols)

    message(sprintf("[Accept All Class 3] Found %d Class 3 columns: %s",
                    length(class3_cols), paste(head(class3_cols, 5), collapse = ", ")))

    if (length(class3_cols) == 0) {
      showNotification("No Class 3 columns found in your data.", type = "warning", duration = 4)
      return()
    }

    # Get current states
    states <- class3_review_states()

    # Find pending Class 3 cards - check destination headers, dropdown values, AND caches
    dest_cache <- destination_suggestion_cache()
    cvalr_cache <- suggestion_cache()

    pending_class3 <- list()

    # Check destination mode targets - look at BOTH cache AND actual dropdown values
    dest_headers <- rv$dest_headers
    if (length(dest_headers) > 0) {
      for (dest_col in dest_headers) {
        # Skip if already processed (accepted/skipped)
        # Use make.names() for consistent key matching with button handlers
        safe_dest_key <- make.names(dest_col)
        current_state <- states[[safe_dest_key]] %||% "pending"
        if (current_state != "pending") next

        # Try to find source column from multiple sources:
        source_col <- NULL

        # 1. Check suggestion cache first
        suggestion <- dest_cache[[dest_col]]
        if (!is.null(suggestion)) {
          # Handle both list format (with $column) and atomic vector format
          cached_col <- if (is.list(suggestion)) suggestion$column else as.character(suggestion)[1]
          if (!is.null(cached_col) && nzchar(cached_col)) {
            source_col <- cached_col
          }
        }

        # 2. Check actual dropdown input value (handles exact name matches)
        if (is.null(source_col) || !nzchar(source_col)) {
          input_id <- paste0("dest_", make.names(dest_col), "_selector")
          input_val <- input[[input_id]]
          if (!is.null(input_val) && length(input_val) > 0 && nzchar(input_val[1])) {
            source_col <- input_val[1]
          }
        }

        # 3. Check if destination column name exists in source data (exact name match)
        if (is.null(source_col) || !nzchar(source_col)) {
          dataset <- current_dataset()
          if (!is.null(dataset) && dest_col %in% names(dataset)) {
            source_col <- dest_col
          }
        }

        # If we found a source and it's Class 3, add to pending list
        if (!is.null(source_col) && nzchar(source_col) && source_col %in% class3_cols) {
          # Use safe_dest_key for consistent key matching with button handlers
          pending_class3[[safe_dest_key]] <- source_col
        }
      }
    }

    # Check CValR mode targets
    for (target in names(cvalr_cache)) {
      suggestion <- cvalr_cache[[target]]
      if (!is.null(suggestion)) {
        # Handle both list format (with $column) and atomic vector format
        source_col <- if (is.list(suggestion)) suggestion$column else as.character(suggestion)[1]
        if (!is.null(source_col) && nzchar(source_col) && source_col %in% class3_cols) {
          # Use make.names() for consistent key matching with button handlers
          safe_target_key <- make.names(target)
          current_state <- states[[safe_target_key]] %||% "pending"
          if (current_state == "pending") {
            pending_class3[[safe_target_key]] <- source_col
          }
        }
      }
    }

    message(sprintf("[Accept All Class 3] Found %d pending Class 3 mappings", length(pending_class3)))
    message(sprintf("[Accept All Class 3] Pending targets: %s", paste(names(pending_class3), collapse = ", ")))

    if (length(pending_class3) == 0) {
      # Show modal explaining the situation
      showModal(modalDialog(
        title = tags$span(icon("info-circle"), " No Pending Class 3 Mappings"),
        tags$div(
          tags$p("There are no Class 3 mappings waiting to be accepted."),
          tags$p("This could mean:"),
          tags$ul(
            tags$li("All Class 3 suggestions have already been accepted"),
            tags$li("No Class 3 columns have auto-matched to any target fields"),
            tags$li("You're viewing a filtered subset of cards")
          ),
          tags$hr(),
          tags$p(
            tags$strong("Class 3 columns in your data: "),
            if (length(class3_cols) > 0) paste(head(class3_cols, 10), collapse = ", ") else "None",
            if (length(class3_cols) > 10) sprintf(" ... and %d more", length(class3_cols) - 10) else ""
          )
        ),
        footer = modalButton("OK"),
        easyClose = TRUE,
        size = "m"
      ))
      return()
    }

    # Accept all pending Class 3 mappings and update dropdown selections
    # Also store in class3_accepted_mappings for immediate availability in get_all_selections()
    accepted_mappings <- class3_accepted_mappings()

    for (target in names(pending_class3)) {
      states[[target]] <- "accepted"
      source_col <- pending_class3[[target]]

      # Store the mapping for immediate availability (async fix)
      accepted_mappings[[target]] <- source_col

      # Update the dropdown selection to actually apply the mapping
      # The dropdown ID format depends on the mode (destination vs CValR)
      if (target %in% dest_headers) {
        # Destination mode: dest_{target}_selector
        input_id <- paste0("dest_", make.names(target), "_selector")
      } else {
        # CValR mode: uses id_for() pattern
        input_id <- paste0("mapping_", make.names(target))
      }

      # Force update the selectize input to apply the suggestion as a real selection
      updateSelectizeInput(session, input_id, selected = source_col)
      message(sprintf("[Accept All Class 3] Updated dropdown '%s' to '%s'", input_id, source_col))
    }
    class3_review_states(states)
    class3_accepted_mappings(accepted_mappings)

    # Show confirmation modal
    showModal(modalDialog(
      title = tags$span(icon("check-circle"), style = "color: #5cb85c;", " Class 3 Mappings Accepted"),
      tags$div(
        tags$p(sprintf("Accepted %d Class 3 mapping(s) as-is.", length(pending_class3))),
        tags$p("These columns will be included in your export with their original Class 3 classification."),
        tags$hr(),
        tags$p(tags$strong("Accepted mappings:")),
        tags$ul(
          lapply(head(names(pending_class3), 10), function(target) {
            source <- pending_class3[[target]]
            tags$li(sprintf("%s → %s", source, target))
          }),
          if (length(pending_class3) > 10) {
            tags$li(tags$em(sprintf("... and %d more", length(pending_class3) - 10)))
          }
        )
      ),
      footer = modalButton("OK"),
      easyClose = TRUE,
      size = "m"
    ))
  })

  # Column detail modal - triggered by clicking column name in classification table
  observeEvent(input$classification_table_cell_clicked, {
    info <- input$classification_table_cell_clicked
    # Only respond to clicks on Column name (column index 0)
    if (is.null(info) || is.null(info$col) || info$col != 0) return()
    if (is.null(info$value) || !nzchar(info$value)) return()

    col_name <- info$value
    dataset <- current_dataset()
    analysis <- rv$column_analysis
    classifications <- rv$column_classifications

    if (is.null(dataset) || is.null(analysis)) return()

    # Get analysis row for this column
    analysis_row <- analysis[analysis$column_name == col_name, ]
    if (nrow(analysis_row) == 0) return()

    class_row <- if (!is.null(classifications)) {
      classifications[classifications$column_name == col_name, ]
    } else {
      data.frame()
    }

    # Get column data for value distribution
    col_data <- if (col_name %in% names(dataset)) dataset[[col_name]] else NULL

    # Get active flags for this column (excluding dismissed ones)
    raw_flags <- if (!is.null(analysis_row$flags[[1]])) {
      f <- analysis_row$flags[[1]]
      f[!is.na(f) & nzchar(f)]
    } else {
      character(0)
    }
    dismissed_flags <- rv$dismissed_flags[[col_name]] %||% character(0)
    active_flags <- setdiff(raw_flags, dismissed_flags)

    # Build value distribution
    value_dist <- NULL
    numeric_stats <- NULL
    if (!is.null(col_data)) {
      # Remove NAs for distribution
      non_na <- col_data[!is.na(col_data)]
      if (length(non_na) > 0) {
        # Top values by frequency
        val_table <- sort(table(non_na), decreasing = TRUE)
        top_n <- min(10, length(val_table))
        value_dist <- data.frame(
          Value = names(val_table)[1:top_n],
          Count = as.integer(val_table[1:top_n]),
          Pct = sprintf("%.1f%%", 100 * as.integer(val_table[1:top_n]) / length(non_na)),
          stringsAsFactors = FALSE
        )

        # Numeric stats if applicable
        if (is.numeric(col_data)) {
          numeric_stats <- list(
            min = min(non_na, na.rm = TRUE),
            max = max(non_na, na.rm = TRUE),
            mean = mean(non_na, na.rm = TRUE),
            median = median(non_na, na.rm = TRUE),
            sd = sd(non_na, na.rm = TRUE)
          )
        }
      }
    }

    # Build modal content
    modal_content <- tagList(
      # Basic info section
      tags$div(
        class = "row",
        style = "margin-bottom: 15px;",
        tags$div(
          class = "col-sm-6",
          tags$strong("Fill Rate: "), sprintf("%.1f%%", analysis_row$fill_rate[1] * 100), tags$br(),
          tags$strong("Unique Values: "), analysis_row$unique_count[1], tags$br(),
          tags$strong("Inferred Type: "), analysis_row$inferred_type[1]
        ),
        tags$div(
          class = "col-sm-6",
          tags$strong("Classification: "),
          if (nrow(class_row) > 0 && !is.na(class_row$class[1])) {
            class_labels <- c("1" = "Class 1 (Essential)", "2" = "Class 2 (Supplemental)",
                             "3" = "Class 3 (Marginal)", "4" = "Class 4 (Exclude)")
            class_labels[as.character(class_row$class[1])]
          } else {
            "Unclassified"
          },
          tags$br(),
          tags$strong("Reason: "),
          if (nrow(class_row) > 0) class_row$reason[1] else "N/A"
        )
      ),

      # Issues/Flags section with dismiss buttons
      if (length(active_flags) > 0 || length(dismissed_flags) > 0) {
        tags$div(
          class = if (length(active_flags) > 0) "alert alert-info" else "well",
          style = "margin-bottom: 15px; padding: 10px;",
          tags$strong(icon("flag"), " Data Quality Flags"), tags$br(), tags$br(),
          if (length(active_flags) > 0) {
            tagList(
              tags$div(
                style = "margin-bottom: 8px;",
                tags$em("Active flags (click to dismiss):"),
                tags$div(
                  style = "margin-top: 5px;",
                  lapply(active_flags, function(flag) {
                    tags$button(
                      class = "btn btn-warning btn-xs",
                      style = "margin: 2px;",
                      onclick = sprintf("Shiny.setInputValue('dismiss_flag', {col: '%s', flag: '%s', rand: Math.random()}, {priority: 'event'})",
                                       gsub("'", "\\\\'", col_name), flag),
                      icon("times"), " ", flag
                    )
                  })
                )
              )
            )
          },
          if (length(dismissed_flags) > 0) {
            tagList(
              tags$div(
                style = if (length(active_flags) > 0) "margin-top: 10px; padding-top: 10px; border-top: 1px solid #ccc;" else "",
                tags$em("Dismissed flags (click to restore):"),
                tags$div(
                  style = "margin-top: 5px;",
                  lapply(dismissed_flags, function(flag) {
                    tags$button(
                      class = "btn btn-default btn-xs",
                      style = "margin: 2px; text-decoration: line-through; opacity: 0.7;",
                      onclick = sprintf("Shiny.setInputValue('restore_flag', {col: '%s', flag: '%s', rand: Math.random()}, {priority: 'event'})",
                                       gsub("'", "\\\\'", col_name), flag),
                      icon("undo"), " ", flag
                    )
                  })
                )
              )
            )
          }
        )
      },

      # Numeric stats if applicable
      if (!is.null(numeric_stats)) {
        tags$div(
          class = "well",
          style = "margin-bottom: 15px; padding: 10px;",
          tags$strong("Numeric Statistics"), tags$br(),
          tags$table(
            class = "table table-condensed",
            style = "margin-bottom: 0;",
            tags$tr(
              tags$td(tags$strong("Min")), tags$td(format(numeric_stats$min, big.mark = ",")),
              tags$td(tags$strong("Max")), tags$td(format(numeric_stats$max, big.mark = ","))
            ),
            tags$tr(
              tags$td(tags$strong("Mean")), tags$td(format(round(numeric_stats$mean, 2), big.mark = ",")),
              tags$td(tags$strong("Median")), tags$td(format(numeric_stats$median, big.mark = ","))
            ),
            tags$tr(
              tags$td(tags$strong("Std Dev")), tags$td(format(round(numeric_stats$sd, 2), big.mark = ",")),
              tags$td(""), tags$td("")
            )
          )
        )
      },

      # Value distribution
      if (!is.null(value_dist) && nrow(value_dist) > 0) {
        tags$div(
          tags$strong("Top Values by Frequency"),
          tags$div(
            style = "max-height: 250px; overflow-y: auto; margin-top: 8px;",
            tags$table(
              class = "table table-striped table-condensed",
              tags$thead(
                tags$tr(
                  tags$th("Value"),
                  tags$th("Count"),
                  tags$th("Pct")
                )
              ),
              tags$tbody(
                lapply(1:nrow(value_dist), function(i) {
                  tags$tr(
                    tags$td(style = "max-width: 300px; overflow: hidden; text-overflow: ellipsis;",
                           as.character(value_dist$Value[i])),
                    tags$td(value_dist$Count[i]),
                    tags$td(value_dist$Pct[i])
                  )
                })
              )
            )
          )
        )
      } else {
        tags$p(class = "text-muted", "No value distribution available")
      }
    )

    showModal(modalDialog(
      title = tagList(icon("columns"), sprintf(" Column Details: %s", col_name)),
      modal_content,
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Dismiss flag handler
 observeEvent(input$dismiss_flag, {
    req(input$dismiss_flag)
    col_name <- input$dismiss_flag$col
    flag <- input$dismiss_flag$flag

    if (is.null(col_name) || is.null(flag)) return()

    dismissed <- rv$dismissed_flags
    if (is.null(dismissed[[col_name]])) {
      dismissed[[col_name]] <- character(0)
    }
    if (!flag %in% dismissed[[col_name]]) {
      dismissed[[col_name]] <- c(dismissed[[col_name]], flag)
      rv$dismissed_flags <- dismissed
      showNotification(
        sprintf("Dismissed '%s' flag for %s", flag, col_name),
        type = "message",
        duration = 3
      )
      removeModal()
    }
  })

  # Restore flag handler
  observeEvent(input$restore_flag, {
    req(input$restore_flag)
    col_name <- input$restore_flag$col
    flag <- input$restore_flag$flag

    if (is.null(col_name) || is.null(flag)) return()

    dismissed <- rv$dismissed_flags
    if (!is.null(dismissed[[col_name]])) {
      dismissed[[col_name]] <- setdiff(dismissed[[col_name]], flag)
      if (length(dismissed[[col_name]]) == 0) {
        dismissed[[col_name]] <- NULL
      }
      rv$dismissed_flags <- dismissed
      showNotification(
        sprintf("Restored '%s' flag for %s", flag, col_name),
        type = "message",
        duration = 3
      )
      removeModal()
    }
  })

  # Bulk action: Set PII/internal columns to Class 4

  observeEvent(input$apply_bulk_class4, {
    tryCatch({
      analysis <- rv$column_analysis
      classifications <- rv$column_classifications

      if (is.null(analysis) || is.null(classifications)) {
        showNotification("No data loaded to classify", type = "warning")
        return()
      }

      overrides <- rv$classification_overrides
      if (is.null(overrides)) overrides <- list()

      # Find columns that should be Class 4:
      # - Match CLASS_4_PATTERNS (PII, internal IDs, contact info)

      columns_to_override <- character()

      for (i in seq_len(nrow(analysis))) {
        col_name <- analysis$column_name[i]

        # Skip if already Class 4
        current_class <- if (!is.null(overrides[[col_name]])) {
          overrides[[col_name]]
        } else {
          idx <- which(classifications$column_name == col_name)
          if (length(idx) > 0) classifications$class[idx[1]] else NA
        }

        if (!is.na(current_class) && current_class == 4) next

        # Check against CLASS_4_PATTERNS
        for (pattern_name in names(CLASS_4_PATTERNS)) {
          pattern_info <- CLASS_4_PATTERNS[[pattern_name]]
          if (grepl(pattern_info$pattern, col_name, ignore.case = TRUE, perl = TRUE)) {
            overrides[[col_name]] <- 4
            columns_to_override <- c(columns_to_override, col_name)
            break
          }
        }
      }

      rv$classification_overrides <- overrides

      if (length(columns_to_override) > 0) {
        showNotification(
          sprintf("Set %d PII/internal columns to Class 4", length(unique(columns_to_override))),
          type = "message",
          duration = 4
        )
      } else {
        showNotification("No additional columns to reclassify as Class 4", type = "message", duration = 3)
      }
    }, error = function(e) {
      message(sprintf("Bulk class 4 error: %s", e$message))
      showNotification(sprintf("Error: %s", e$message), type = "error", duration = 6)
    })
  })

  # Preview the mapped data
  output$mapped_data_preview <- DT::renderDataTable({
    req(mapped_data())
    table_data <- mapped_data()
    numeric_cols <- vapply(table_data, is.numeric, logical(1))
    if (any(numeric_cols)) {
      table_data[numeric_cols] <- lapply(table_data[numeric_cols], function(col) {
        format(col, trim = TRUE, scientific = FALSE, digits = 15)
      })
    }
    text_cols <- vapply(
      table_data,
      function(col) is.character(col) || is.factor(col),
      logical(1)
    )
    if (any(text_cols)) {
      table_data[text_cols] <- lapply(table_data[text_cols], function(col) {
        cleaned <- gsub("[\\r\\n\\t]+", " ", as.character(col), perl = TRUE)
        trimmed <- trimws(gsub("\\s{2,}", " ", cleaned))
        trimmed
      })
    }

    DT::datatable(
      table_data,
      class = "display nowrap compact",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "ftip",
        autoWidth = FALSE
      )
    )
  })

  # Scatter plot: Sold Price vs Sold Date with 3rd degree polynomial
  output$sales_scatter_plot <- renderPlot({
    # Check if mapped data exists
    logic_data <- logic_mapped_data()
    if (is.null(logic_data)) {
      # Show instruction message
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' above\nto generate the sales analysis plot",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for date field
    date_col <- NULL
    date_candidates <- c("DateSold", "CloseDate", "SaleDate", "DateClosed", "SettlementDate", "SoldDate")
    for (dc in date_candidates) {
      if (dc %in% names(logic_data)) {
        date_col <- dc
        break
      }
    }

    # Flexible column detection for price field
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice", "ListPrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    # Check if required columns exist
    if (is.null(date_col) || is.null(price_col)) {
      # Show message if required fields are missing
      plot.new()
      text(0.5, 0.5, "Map a date field (DateSold, CloseDate, etc.) and\na price field (PriceSold, ClosePrice, etc.)\nthen click 'Apply Mappings & Finalize'",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Prepare data - remove NA values and convert types
    plot_data <- data.frame(
      DateSold = logic_data[[date_col]],
      PriceSold = logic_data[[price_col]],
      stringsAsFactors = FALSE
    )

    # Remove rows with missing values
    plot_data <- plot_data[complete.cases(plot_data), ]

    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No complete data available for DateSold and PriceSold",
           cex = 1.5, col = "#666666")
      return()
    }

    # Convert DateSold to Date with multiple format support
    tryCatch({
      if (!inherits(plot_data$DateSold, "Date")) {
        # First try the standard format we use when saving
        plot_data$DateSold <- as.Date(as.character(plot_data$DateSold), format = "%Y-%m-%d")

        # If that didn't work, try multiple formats
        if (all(is.na(plot_data$DateSold))) {
          plot_data$DateSold <- as.Date(logic_data$DateSold, tryFormats = c(
            "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
            "%Y/%m/%d", "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y",
            "%Y%m%d", "%m/%d/%y", "%d/%m/%y"
          ))
        }
      }
    }, error = function(e) {
      message("Error parsing dates: ", e$message)
    })

    # Convert PriceSold to numeric if needed
    plot_data$PriceSold <- as.numeric(gsub("[^0-9.-]", "", as.character(plot_data$PriceSold)))

    # Remove any rows that failed conversion
    plot_data <- plot_data[complete.cases(plot_data) & plot_data$PriceSold > 0, ]

    message(sprintf("Plot 1: Parsed %d valid data points from %d total rows", nrow(plot_data), nrow(logic_data)))

    if (nrow(plot_data) < 4) {
      plot.new()
      text(0.5, 0.5, "Need at least 4 data points for polynomial trend",
           cex = 1.5, col = "#666666")
      return()
    }

    # Create the scatter plot with linear and 3rd degree polynomial
    ggplot(plot_data, aes(x = DateSold, y = PriceSold)) +
      geom_point(aes(color = "Data Points"), alpha = 0.6, size = 3) +
      geom_smooth(aes(color = "Linear Trend"),
                  method = "lm",
                  formula = y ~ x,
                  se = TRUE,
                  linewidth = 1.2) +
      geom_smooth(aes(color = "3rd Degree Polynomial"),
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  se = TRUE,
                  linewidth = 1.2) +
      scale_color_manual(
        name = "Legend",
        values = c(
          "Data Points" = "#2E86AB",
          "Linear Trend" = "#06A77D",
          "3rd Degree Polynomial" = "#A23B72"
        )
      ) +
      labs(
        title = "Sold Price vs Sold Date",
        x = "Date Sold",
        y = "Sold Price ($)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold")
      ) +
      scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.05))
      )
  })

  # Plot 2: Price Distribution Histogram
  output$price_distribution_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for price field
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice", "ListPrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    if (is.null(price_col)) {
      plot.new()
      text(0.5, 0.5, "Map a price field (PriceSold, ClosePrice, etc.)\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Robust numeric conversion: strip currency symbols/commas
    prices <- as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col]])))
    prices <- prices[!is.na(prices) & prices > 0]

    if (length(prices) < 5) {
      plot.new()
      text(0.5, 0.5, "Need at least 5 sales to create histogram",
           cex = 1.2, col = "#666666")
      return()
    }

    ggplot(data.frame(Price = prices), aes(x = Price)) +
      geom_histogram(bins = 30, fill = "#FF9800", color = "#E65100", alpha = 0.7) +
      geom_vline(aes(xintercept = median(prices), color = "Median"),
                 linetype = "dashed", linewidth = 1.2) +
      geom_vline(aes(xintercept = mean(prices), color = "Mean"),
                 linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(name = "", values = c("Median" = "#D32F2F", "Mean" = "#1976D2")) +
      labs(title = "Distribution of Sale Prices",
           x = "Sale Price ($)", y = "Number of Properties") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold")) +
      scale_x_continuous(labels = scales::dollar_format())
  })

  # Plot 3: Price by Neighborhood Box Plot
  output$price_by_neighborhood_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    message(sprintf("[Chart 3] logic_mapped_data is %s, columns: %s",
                   if (is.null(logic_data)) "NULL" else sprintf("data.frame (%d rows)", nrow(logic_data)),
                   if (is.null(logic_data)) "none" else paste(names(logic_data), collapse = ", ")))
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for price field
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice", "ListPrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    # Flexible column detection for neighborhood field
    neighborhood_col <- NULL
    neighborhood_candidates <- c("Neighbourhood", "Neighborhood", "Area", "Subdivision", "Community", "City")
    for (nc in neighborhood_candidates) {
      if (nc %in% names(logic_data)) {
        neighborhood_col <- nc
        break
      }
    }

    message(sprintf("[Chart 3] Found price_col=%s, neighborhood_col=%s",
                   if (is.null(price_col)) "NONE" else price_col,
                   if (is.null(neighborhood_col)) "NONE" else neighborhood_col))

    if (is.null(price_col) || is.null(neighborhood_col)) {
      plot.new()
      text(0.5, 0.5, "Map a price field (PriceSold, ClosePrice, etc.) and\na location field (Neighbourhood, Area, etc.)\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Debug: show sample raw values before conversion
    raw_prices <- head(logic_data[[price_col]], 10)
    message(sprintf("[Chart 3] Raw price sample (first 10): %s", paste(raw_prices, collapse = ", ")))

    plot_data <- data.frame(
      Price = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col]]))),
      Neighborhood = as.character(logic_data[[neighborhood_col]]),
      stringsAsFactors = FALSE
    )

    # Debug: count valid prices
    valid_prices <- sum(!is.na(plot_data$Price) & plot_data$Price > 0)
    message(sprintf("[Chart 3] Valid prices (non-NA, >0): %d out of %d", valid_prices, nrow(plot_data)))

    plot_data <- plot_data[complete.cases(plot_data) & plot_data$Price > 0, ]
    message(sprintf("[Chart 3] Rows after filtering: %d", nrow(plot_data)))

    if (nrow(plot_data) < 5) {
      plot.new()
      text(0.5, 0.5, "Need more data with neighborhoods to create box plot",
           cex = 1.2, col = "#666666")
      return()
    }

    # Limit to top 15 neighborhoods by count to keep readable
    top_neighborhoods <- names(sort(table(plot_data$Neighborhood), decreasing = TRUE)[1:min(15, length(unique(plot_data$Neighborhood)))])
    plot_data <- plot_data[plot_data$Neighborhood %in% top_neighborhoods, ]

    # Order by median price
    plot_data$Neighborhood <- reorder(plot_data$Neighborhood, plot_data$Price, FUN = median)

    ggplot(plot_data, aes(x = Neighborhood, y = Price, fill = Neighborhood)) +
      geom_boxplot(alpha = 0.7, outlier.color = "#D32F2F", outlier.size = 2) +
      stat_summary(fun = median, geom = "text", aes(label = scales::dollar(after_stat(y))),
                   vjust = -0.5, size = 3, fontface = "bold") +
      labs(title = "Price Distribution by Neighborhood",
           subtitle = "(Showing top 15 neighborhoods by sales count)",
           x = "Neighborhood", y = "Sale Price ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::dollar_format(), limits = c(0, NA))
  })

  # Plot 4: Sales Volume Over Time
  output$sales_volume_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for date field
    date_col <- NULL
    date_candidates <- c("DateSold", "CloseDate", "SaleDate", "DateClosed", "SettlementDate", "SoldDate")
    for (dc in date_candidates) {
      if (dc %in% names(logic_data)) {
        date_col <- dc
        break
      }
    }

    if (is.null(date_col)) {
      plot.new()
      text(0.5, 0.5, "Map a date field (DateSold, CloseDate, etc.)\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Convert dates with multiple format support
    tryCatch({
      # First try the standard format we use when saving
      dates <- as.Date(as.character(logic_data[[date_col]]), format = "%Y-%m-%d")

      # If that didn't work, try multiple formats
      if (all(is.na(dates))) {
        dates <- as.Date(as.character(logic_data[[date_col]]), tryFormats = c(
          "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
          "%Y/%m/%d", "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y",
          "%Y%m%d", "%m/%d/%y", "%d/%m/%y"
        ))
      }
    }, error = function(e) {
      message("Error parsing dates in Plot 4: ", e$message)
    })

    dates <- dates[!is.na(dates)]
    message(sprintf("Plot 4: Parsed %d valid dates from %d total rows", length(dates), nrow(logic_data)))

    if (length(dates) < 5) {
      plot.new()
      text(0.5, 0.5, "Need at least 5 sales with dates to create volume chart",
           cex = 1.2, col = "#666666")
      return()
    }

    # Aggregate by month
    monthly_data <- data.frame(Date = dates)
    monthly_data$YearMonth <- format(monthly_data$Date, "%Y-%m")
    volume_data <- as.data.frame(table(monthly_data$YearMonth))
    colnames(volume_data) <- c("YearMonth", "Count")
    volume_data$Date <- as.Date(paste0(volume_data$YearMonth, "-01"))

    ggplot(volume_data, aes(x = Date, y = Count)) +
      geom_bar(aes(fill = "Sales Volume"), stat = "identity", alpha = 0.7) +
      geom_smooth(aes(color = "LOESS Trend (Local Regression)"),
                  method = "loess", se = TRUE, linewidth = 1.2) +
      scale_fill_manual(name = "Legend", values = c("Sales Volume" = "#9C27B0")) +
      scale_color_manual(name = "", values = c("LOESS Trend (Local Regression)" = "#F44336")) +
      labs(title = "Monthly Sales Volume",
           subtitle = "Trend: LOESS Smooth (Local Regression)",
           x = "Month", y = "Number of Sales") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.position = "top",
            legend.title = element_text(face = "bold")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })

  # Plot 5: Price per Square Foot
  output$price_per_sqft_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    message(sprintf("[Chart 5] logic_mapped_data is %s, columns: %s",
                   if (is.null(logic_data)) "NULL" else sprintf("data.frame (%d rows)", nrow(logic_data)),
                   if (is.null(logic_data)) "none" else paste(names(logic_data), collapse = ", ")))
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for price field
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice", "ListPrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    # Flexible column detection for sqft field
    sqft_col <- NULL
    sqft_candidates <- c("SqFtTotal", "SqFt", "LivingArea", "GLA", "TotalSqFt", "SquareFeet", "LivingAreaSqFt")
    for (sc in sqft_candidates) {
      if (sc %in% names(logic_data)) {
        sqft_col <- sc
        break
      }
    }

    # Check if PricePerSqFt already exists (derived metric) or if we need source fields
    has_price_per_sqft <- DERIVED_METRIC_PRICE_PER_SQFT %in% names(logic_data)
    has_source_fields <- !is.null(sqft_col) && !is.null(price_col)

    if (!has_price_per_sqft && !has_source_fields) {
      plot.new()
      text(0.5, 0.5, "Map a price field (PriceSold, ClosePrice, etc.) and\na sqft field (SqFtTotal, LivingArea, etc.)\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Use derived PricePerSqFt if available, otherwise calculate from source fields
    if (has_price_per_sqft) {
      # Derived metric already calculated - use it directly
      plot_data <- data.frame(
        Price = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col %||% "PriceSold"]]))),
        SqFt = if (!is.null(sqft_col)) as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[sqft_col]]))) else NA_real_,
        PricePerSqFt = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[DERIVED_METRIC_PRICE_PER_SQFT]]))),
        stringsAsFactors = FALSE
      )
      plot_data <- plot_data[complete.cases(plot_data) & plot_data$Price > 0 & plot_data$SqFt > 0, ]
    } else {
      # Calculate PricePerSqFt on-the-fly (backward compatibility)
      plot_data <- data.frame(
        Price = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col]]))),
        SqFt = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[sqft_col]]))),
        stringsAsFactors = FALSE
      )
      plot_data <- plot_data[complete.cases(plot_data) & plot_data$Price > 0 & plot_data$SqFt > 0, ]
      plot_data$PricePerSqFt <- plot_data$Price / plot_data$SqFt
    }

    if (nrow(plot_data) < 5) {
      plot.new()
      text(0.5, 0.5, "Need at least 5 sales with price and square footage",
           cex = 1.2, col = "#666666")
      return()
    }

    ggplot(plot_data, aes(x = SqFt, y = Price)) +
      geom_point(aes(color = PricePerSqFt), size = 3, alpha = 0.6) +
      geom_smooth(aes(linetype = "Linear Trend (1st Order)"),
                  method = "lm", formula = y ~ x, se = TRUE,
                  color = "#E91E63", linewidth = 1.2) +
      scale_color_gradient2(name = "$/SqFt",
                           low = "#4CAF50", mid = "#FFC107", high = "#F44336",
                           midpoint = median(plot_data$PricePerSqFt),
                           labels = scales::dollar_format()) +
      scale_linetype_manual(name = "Trend Line", values = c("Linear Trend (1st Order)" = "solid")) +
      labs(title = "Price vs Square Footage",
           subtitle = paste0("Median: ", scales::dollar(median(plot_data$PricePerSqFt)), "/sqft | Trend: Linear (1st Order)"),
           x = "Total Square Feet", y = "Sale Price ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.box = "vertical") +
      scale_y_continuous(labels = scales::dollar_format(), limits = c(0, NA)) +
      scale_x_continuous(labels = scales::comma_format()) +
      guides(color = guide_colorbar(order = 1),
             linetype = guide_legend(order = 2))
  })

  # Plot 6: Price per Square Foot vs Square Footage
  output$price_per_sqft_vs_sqft_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    message(sprintf("[Chart 6] logic_mapped_data is %s, columns: %s",
                   if (is.null(logic_data)) "NULL" else sprintf("data.frame (%d rows)", nrow(logic_data)),
                   if (is.null(logic_data)) "none" else paste(names(logic_data), collapse = ", ")))
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Flexible column detection for price field
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice", "ListPrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    # Flexible column detection for sqft field
    sqft_col <- NULL
    sqft_candidates <- c("SqFtTotal", "SqFt", "LivingArea", "GLA", "TotalSqFt", "SquareFeet", "LivingAreaSqFt")
    for (sc in sqft_candidates) {
      if (sc %in% names(logic_data)) {
        sqft_col <- sc
        break
      }
    }

    # Check if PricePerSqFt already exists (derived metric) or if we need source fields
    has_price_per_sqft <- DERIVED_METRIC_PRICE_PER_SQFT %in% names(logic_data)
    has_source_fields <- !is.null(sqft_col) && !is.null(price_col)

    if (!has_price_per_sqft && !has_source_fields) {
      plot.new()
      text(0.5, 0.5, "Map a price field (PriceSold, ClosePrice, etc.) and\na sqft field (SqFtTotal, LivingArea, etc.)\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Use derived PricePerSqFt if available, otherwise calculate from source fields
    if (has_price_per_sqft) {
      # Derived metric already calculated - use it directly
      plot_data <- data.frame(
        SqFt = if (!is.null(sqft_col)) as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[sqft_col]]))) else NA_real_,
        PricePerSqFt = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[DERIVED_METRIC_PRICE_PER_SQFT]]))),
        stringsAsFactors = FALSE
      )
      plot_data <- plot_data[complete.cases(plot_data) & plot_data$PricePerSqFt > 0 & plot_data$SqFt > 0, ]
    } else {
      # Calculate PricePerSqFt on-the-fly (backward compatibility)
      plot_data <- data.frame(
        Price = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col]]))),
        SqFt = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[sqft_col]]))),
        stringsAsFactors = FALSE
      )
      plot_data <- plot_data[complete.cases(plot_data) & plot_data$Price > 0 & plot_data$SqFt > 0, ]
      plot_data$PricePerSqFt <- plot_data$Price / plot_data$SqFt
    }

    if (nrow(plot_data) < 5) {
      plot.new()
      text(0.5, 0.5, "Need at least 5 sales with price and square footage",
           cex = 1.2, col = "#666666")
      return()
    }

    ggplot(plot_data, aes(x = SqFt, y = PricePerSqFt)) +
      geom_point(aes(color = "Data Points"), size = 3, alpha = 0.6) +
      geom_smooth(aes(color = "LOESS Smooth (Local Regression)"),
                  method = "loess", formula = y ~ x, se = TRUE,
                  linewidth = 1.2, fill = "#BBDEFB") +
      scale_color_manual(
        name = "Legend",
        values = c(
          "Data Points" = "#2196F3",
          "LOESS Smooth (Local Regression)" = "#1565C0"
        )
      ) +
      labs(title = "Price per Square Foot vs Property Size",
           subtitle = paste0("Median: ", scales::dollar(median(plot_data$PricePerSqFt)), "/sqft | ",
                           "Total Properties: ", nrow(plot_data), " | Trend: LOESS Smooth"),
           x = "Total Square Feet", y = "Price per Square Foot ($/sqft)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.position = "top",
            legend.title = element_text(face = "bold")) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_x_continuous(labels = scales::comma_format())
  })

  # Plot 7: Price by Bedrooms
  output$price_by_bedrooms_plot <- renderPlot({
    logic_data <- logic_mapped_data()
    message(sprintf("[Chart 7] logic_mapped_data is %s, columns: %s",
                   if (is.null(logic_data)) "NULL" else sprintf("data.frame (%d rows)", nrow(logic_data)),
                   if (is.null(logic_data)) "none" else paste(names(logic_data), collapse = ", ")))
    if (is.null(logic_data)) {
      plot.new()
      text(0.5, 0.5, "Click 'Apply Mappings & Finalize' to view analysis",
           cex = 1.3, col = "#666666", font = 2)
      return()
    }

    # Check for price field (standard or destination schema variants)
    price_col <- NULL
    price_candidates <- c("PriceSold", "ClosePrice", "SoldPrice", "SalePrice")
    for (pc in price_candidates) {
      if (pc %in% names(logic_data)) {
        price_col <- pc
        break
      }
    }

    # Check for bedroom field (standard or destination schema variants)
    beds_col <- NULL
    beds_candidates <- c("Beds", "BedroomsTotal", "Bedrooms", "BR", "TotalBedrooms")
    for (bc in beds_candidates) {
      if (bc %in% names(logic_data)) {
        beds_col <- bc
        break
      }
    }

    # Check if required fields exist
    if (is.null(price_col) || is.null(beds_col)) {
      plot.new()
      text(0.5, 0.5, "Map 'PriceSold' and 'Beds' fields\nto view this analysis",
           cex = 1.2, col = "#d9534f", font = 2)
      return()
    }

    # Prepare data
    plot_data <- data.frame(
      Price = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[price_col]]))),
      Bedrooms = as.numeric(gsub("[^0-9.-]", "", as.character(logic_data[[beds_col]]))),
      stringsAsFactors = FALSE
    )

    plot_data <- plot_data[complete.cases(plot_data) & plot_data$Price > 0 & plot_data$Bedrooms > 0, ]

    if (nrow(plot_data) < 5) {
      plot.new()
      text(0.5, 0.5, "Need at least 5 sales with bedroom data to create box plot",
           cex = 1.2, col = "#666666")
      return()
    }

    # Filter to reasonable bedroom counts (1-10) to avoid outliers
    plot_data <- plot_data[plot_data$Bedrooms >= 1 & plot_data$Bedrooms <= 10, ]

    # Convert to factor for better box plot display
    plot_data$Bedrooms <- factor(plot_data$Bedrooms, levels = sort(unique(plot_data$Bedrooms)))

    # Calculate median prices for subtitle
    median_overall <- median(plot_data$Price)

    ggplot(plot_data, aes(x = Bedrooms, y = Price, fill = Bedrooms)) +
      geom_boxplot(alpha = 0.7, outlier.color = "#D32F2F", outlier.size = 2) +
      # Median label (above box)
      stat_summary(fun = median, geom = "text", aes(label = scales::dollar(after_stat(y))),
                   vjust = -0.5, size = 3, fontface = "bold") +
      # 25th percentile label (bottom of box)
      stat_summary(fun = function(x) quantile(x, 0.25), geom = "text",
                   aes(label = scales::dollar(after_stat(y))),
                   hjust = 1.2, size = 2.5, color = "#555555", fontface = "plain") +
      # 75th percentile label (top of box)
      stat_summary(fun = function(x) quantile(x, 0.75), geom = "text",
                   aes(label = scales::dollar(after_stat(y))),
                   hjust = 1.2, size = 2.5, color = "#555555", fontface = "plain") +
      labs(title = "Price Distribution by Number of Bedrooms",
           subtitle = paste0("Overall Median: ", scales::dollar(median_overall), " | Labels show Q1 (25th), Median (50th), Q3 (75th) percentiles"),
           x = "Number of Bedrooms", y = "Sale Price ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold")) +
      scale_y_continuous(labels = scales::dollar_format(), limits = c(0, NA))
  })

  # Download handler for CSV export
  output$download_mapped <- downloadHandler(
    filename = function() {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sha <- ver$sha %||% "dev"
      ver_str <- APP_VERSION %||% "0.0.0"
      paste0("mapped_data_", ts, "_v", ver_str, "_", sha, ".csv")
    },
    content = function(file) {
      tryCatch({
        data_to_export <- mapped_data()
        if (is.null(data_to_export)) {
          data_to_export <- current_dataset()  # Fallback to current dataset
        }

        if (is.null(data_to_export)) {
          stop("No data available to export. Please upload data and apply mappings first.")
        }

      # Remove completely blank rows (all columns are NA or empty string)
      if (nrow(data_to_export) > 0) {
        # Check each row - keep if ANY column has non-empty data
        row_has_data <- apply(data_to_export, 1, function(row) {
          any(!is.na(row) & nzchar(trimws(as.character(row))))
        })
        data_to_export <- data_to_export[row_has_data, , drop = FALSE]

        message(sprintf("Export: Removed %d blank rows, keeping %d rows with data",
                       sum(!row_has_data), sum(row_has_data)))
      }

      # Convert date columns to yyyy-mm-dd format
      # Only process columns that have "date" in their name
      converted_dates <- 0
      for (col_name in names(data_to_export)) {
        # Only process if column name contains "date" (case insensitive)
        if (!grepl("date", col_name, ignore.case = TRUE)) next

        tryCatch({
          col_data <- data_to_export[[col_name]]

          # Skip if column is already all NA or empty
          if (all(is.na(col_data) | col_data == "")) next

          # Try to parse as date
          parsed_date <- as.Date(col_data, tryFormats = c(
            "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
            "%Y/%m/%d", "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y",
            "%Y%m%d", "%m/%d/%y", "%d/%m/%y"
          ))

          # Only update if parsing was successful (not all NA)
          if (!all(is.na(parsed_date))) {
            data_to_export[[col_name]] <- format(parsed_date, "%Y-%m-%d")
            converted_dates <- converted_dates + 1
            message(sprintf("Export: Converted %s to yyyy-mm-dd format", col_name))
          }
        }, error = function(e) {
          # If parsing fails, leave the column as-is
          message(sprintf("Export: Could not parse date column %s, leaving as-is", col_name))
        })
      }

      if (converted_dates > 0) {
        message(sprintf("Export: Converted %d date columns to yyyy-mm-dd format", converted_dates))
      }

      # Respect embed_build_info checkbox at download time as well
      mapper_cols <- c(".Mapper_Version", ".Mapper_SHA", ".Mapper_Tag", ".Mapper_Date")
      if (isTRUE(input$embed_build_info)) {
        # Ensure mapper columns exist and are up to date
        info_cols <- list(
          .Mapper_Version = APP_VERSION,
          .Mapper_SHA = ver$sha %||% "unknown",
          .Mapper_Tag = ver$tag %||% "",
          .Mapper_Date = APP_VERSION_DATE
        )
        for (nm in names(info_cols)) {
          if (!(nm %in% names(data_to_export))) {
            data_to_export[[nm]] <- info_cols[[nm]]
          } else {
            data_to_export[[nm]] <- info_cols[[nm]]
          }
        }
      } else {
        # Strip mapper columns if present
        keep <- setdiff(names(data_to_export), mapper_cols)
        data_to_export <- data_to_export[, keep, drop = FALSE]
      }

      metrics_added <- rv$derived_metrics_last_run %||% character()
      if (!isTRUE(input$include_derived_metrics) && length(metrics_added)) {
        drop_cols <- intersect(names(data_to_export), metrics_added)
        if (length(drop_cols)) {
          data_to_export <- data_to_export[, setdiff(names(data_to_export), drop_cols), drop = FALSE]
        }
      }

        # Use RFC 4180 compliant CSV export with self-validation
        export_csv_robust(data_to_export, file)
        message("CSV export completed successfully (RFC 4180 format)")
      }, error = function(e) {
        message(sprintf("Error during CSV export: %s", conditionMessage(e)))
        # Write error message to file so user sees it's not a valid CSV
        write(paste("ERROR:", conditionMessage(e)), file)
      })
    },
    contentType = "text/csv"
  )

  # Download handler for saving mapping profile
  output$download_mapping <- downloadHandler(
    filename = function() {
      format_choice <- input$mapping_format
      if (is.null(format_choice)) format_choice <- "rds"
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sha <- ver$sha
      ver_str <- APP_VERSION
      paste0("mapping_profile_", ts, "_v", ver_str, "_", sha, ".", format_choice)
    },
    content = function(file) {
      mappings <- get_all_selections()
      if (length(mappings) == 0) {
        showNotification("No mappings to save!", type = "warning")
        return()
      }

      destination_mode <- isTRUE(input$use_destination_schema) && length(rv$dest_headers) > 0

      format_choice <- input$mapping_format
      if (is.null(format_choice)) format_choice <- "rds"

      tryCatch({
        if (format_choice == "rds") {
          profile_payload <- list(
            schema_type = if (destination_mode) "destination" else "cvalr",
            source_mappings = mappings,
            exclusions = field_exclusions(),
            classification_overrides = rv$classification_overrides,
            dismissed_flags = rv$dismissed_flags,
            destination_headers = if (destination_mode) rv$dest_headers else character(),
            restrict_to_destination_only = isTRUE(input$restrict_to_destination_only)
          )
          saveRDS(profile_payload, file)
          showNotification("Mapping profile saved as RDS!", type = "message")
        } else if (format_choice == "csv") {
          # Get all fields (mapped + fields with exclusions only + fields with classification overrides + dismissed flags)
          all_exclusions <- field_exclusions()
          all_overrides <- rv$classification_overrides
          all_dismissed <- rv$dismissed_flags
          classifications <- rv$column_classifications

          # In destination mode, filter mappings to only include destination headers
          # This prevents CValR standard field aliases from appearing in the profile
          if (destination_mode && length(rv$dest_headers) > 0) {
            dest_headers_set <- rv$dest_headers
            # Filter mappings to only destination headers
            mappings <- mappings[names(mappings) %in% dest_headers_set]
            # Filter overrides to only destination headers
            all_overrides <- all_overrides[names(all_overrides) %in% dest_headers_set]
            # Filter exclusions to only destination headers
            all_exclusions <- all_exclusions[names(all_exclusions) %in% dest_headers_set]
            # Filter dismissed flags to only destination headers
            all_dismissed <- all_dismissed[names(all_dismissed) %in% dest_headers_set]
          }

          all_fields <- unique(c(names(mappings), names(all_exclusions), names(all_overrides), names(all_dismissed)))

          # Helper to get effective class (override takes precedence, then auto-classification)
          get_effective_class <- function(col_name) {
            # Check override first
            if (!is.null(all_overrides[[col_name]])) {
              return(as.character(all_overrides[[col_name]]))
            }
            # Check auto-classification
            if (!is.null(classifications)) {
              idx <- which(classifications$column_name == col_name)
              if (length(idx) > 0 && !is.na(classifications$class[idx[1]])) {
                return(as.character(classifications$class[idx[1]]))
              }
            }
            return("")  # Unclassified
          }

          # Convert list to dataframe (handle multiple source columns per target)
          # Note: Using ;;; delimiter to avoid conflicts with pipe characters in column names
          df <- data.frame(
            Target = all_fields,
            Source = sapply(all_fields, function(tgt) {
              sources <- mappings[[tgt]] %||% character()
              if (length(sources) == 0) "" else paste(sources, collapse = ";;;")
            }),
            Exclusions = sapply(all_fields, function(tgt) {
              exc <- all_exclusions[[tgt]] %||% character()
              if (length(exc) == 0) "" else paste(exc, collapse = ";;;")
            }),
            Classification = sapply(all_fields, function(tgt) {
              get_effective_class(tgt)
            }),
            DismissedFlags = sapply(all_fields, function(tgt) {
              flags <- all_dismissed[[tgt]] %||% character()
              if (length(flags) == 0) "" else paste(flags, collapse = ";;;")
            }),
            SchemaType = if (destination_mode) "destination" else "cvalr",
            stringsAsFactors = FALSE
          )
          # Use RFC 4180 compliant CSV export with self-validation
          export_csv_robust(df, file)
          showNotification("Mapping profile saved as CSV (RFC 4180 format)!", type = "message")
        } else if (format_choice == "json") {
          profile_payload <- list(
            schema_type = if (destination_mode) "destination" else "cvalr",
            source_mappings = mappings,
            exclusions = field_exclusions(),
            classification_overrides = rv$classification_overrides,
            dismissed_flags = rv$dismissed_flags,
            destination_headers = if (destination_mode) rv$dest_headers else character(),
            restrict_to_destination_only = isTRUE(input$restrict_to_destination_only)
          )
          json_text <- jsonlite::toJSON(profile_payload, pretty = TRUE, auto_unbox = TRUE)
          writeLines(json_text, file)
          showNotification("Mapping profile saved as JSON!", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error saving mapping:", e$message), type = "error")
      })
    },
    contentType = "application/octet-stream"
  )
}
