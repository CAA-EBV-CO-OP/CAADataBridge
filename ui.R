ui <- fluidPage(
  # Add busy indicator for long-running operations
  shinybusy::add_busy_spinner(
    spin = "fading-circle",
    color = "#3c8dbc",
    position = "top-right",
    height = "50px",
    width = "50px",
    margins = c(10, 10)
  ),

  # Login/Registration UI - shown when user is not authenticated
  # This now appears FIRST, before the app chooser
  conditionalPanel(
    condition = "!output.user_authenticated",
    div(
      style = "max-width: 400px; margin: 100px auto; padding: 30px;",
      div(
        style = "text-align: center; margin-bottom: 30px;",
        tags$h2("CAA Collaborative"),
        tags$p("Sign in to access your tools and profiles", style = "color: #666;")
      ),

      # Login form
      div(
        id = "login_form",
        textInput("login_username", "Username", placeholder = "Enter username"),
        passwordInput("login_password", "Password", placeholder = "Enter password"),
        div(
          style = "margin-top: 20px;",
          actionButton("login_btn", "Sign In", class = "btn-primary", style = "width: 100%;")
        ),
        div(
          style = "margin-top: 15px; text-align: center;",
          actionLink("show_register", "Don't have an account? Register here")
        ),
        div(
          style = "margin-top: 10px; text-align: center;",
          actionLink("skip_login", "Continue without account (local mode)", style = "color: #999; font-size: 12px;")
        )
      ),

      # Registration form (hidden by default)
      shinyjs::hidden(
        div(
          id = "register_form",
          textInput("register_username", "Username", placeholder = "Choose a username"),
          textInput("register_email", "Email", placeholder = "Enter your email"),
          passwordInput("register_password", "Password", placeholder = "Choose a password"),
          passwordInput("register_password_confirm", "Confirm Password", placeholder = "Confirm password"),
          div(
            style = "margin-top: 20px;",
            actionButton("register_btn", "Create Account", class = "btn-success", style = "width: 100%;")
          ),
          div(
            style = "margin-top: 15px; text-align: center;",
            actionLink("show_login", "Already have an account? Sign in")
          )
        )
      ),

      # Status messages
      div(
        style = "margin-top: 20px;",
        uiOutput("auth_message")
      ),

      # MongoDB status indicator
      div(
        style = "margin-top: 30px; text-align: center; font-size: 11px; color: #999;",
        uiOutput("db_status_indicator")
      )
    )
  ),

  # App Chooser - shown AFTER login, BEFORE main app
  # This uses a conditionalPanel so it only shows when authenticated but app not yet chosen
  conditionalPanel(
    condition = "output.user_authenticated && !output.app_chosen",
    div(
      id = "app_chooser_panel",
      style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0;
               background: rgba(255,255,255,0.98); z-index: 9999;
               display: flex; justify-content: center; align-items: center; flex-direction: column;",
      div(
        style = "text-align: center; max-width: 800px; padding: 20px;",
        # User greeting
        tags$h2(style = "margin-bottom: 5px;", textOutput("chooser_greeting", inline = TRUE)),
        tags$p("Select an application to continue", style = "color: #666; margin-bottom: 30px;"),

        # App choice buttons with descriptions
        div(
          style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; margin-top: 10px;",
          # Column Mapper card
          div(
            style = "text-align: center; max-width: 280px;",
            actionButton("choose_mapper",
                         tags$span(icon("columns"), " Column Mapper"),
                         class = "btn-primary btn-lg",
                         style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold;"),
            tags$p(
              style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
              "Map MLS data columns to ",
              tags$strong("RESO Standards"),
              " and other common schemas"
            )
          ),
          # Decision Trees card
          div(
            style = "text-align: center; max-width: 280px;",
            actionButton("choose_dt",
                         tags$span(icon("sitemap"), " Decision Trees"),
                         class = "btn-info btn-lg",
                         style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold;"),
            tags$p(
              style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
              "Apply ADF/CMS decision logic",
              tags$strong("including educational and use context")
            )
          ),
          # Report Forms card
          div(
            style = "text-align: center; max-width: 280px;",
            actionButton("choose_reports",
                         tags$span(icon("file-alt"), " Report Forms"),
                         class = "btn-success btn-lg",
                         style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold;"),
            tags$p(
              style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
              "Generate ",
              tags$strong("appraisal reports"),
              " with narrative sections"
            )
          ),
          # CAA Data Field Mapper card (external React app)
          div(
            style = "text-align: center; max-width: 280px;",
            tags$a(
              href = "http://localhost:3000",
              target = "_blank",
              class = "btn btn-warning btn-lg",
              style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold; display: inline-flex; align-items: center; justify-content: center; gap: 8px; text-decoration: none;",
              icon("wand-magic-sparkles"), " Data Field Mapper"
            ),
            tags$p(
              style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
              "AI-powered ",
              tags$strong("RESO field mapping"),
              " with Gemini integration"
            )
          )
        ),

        # Visual workflow guide
        tags$hr(style = "margin: 30px 0; border-color: #ccc;"),
        tags$div(
          style = "text-align: left; background: #e8f4f8; border: 1px solid #b8d4e3; border-radius: 8px; padding: 20px;",
          tags$h4(icon("route"), " Workflow Guide", style = "margin-top: 0; color: #2c5282; font-weight: bold;"),

          # Flow chart - structured layout with L-connector
          tags$div(
            style = "margin-top: 20px; position: relative;",

            # Question row
            tags$div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
              tags$span(style = "background: #38a169; color: white; border-radius: 50%; width: 28px; height: 28px; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 13px; flex-shrink: 0;", "1"),
              tags$span(style = "color: #1a202c; font-size: 14px; font-weight: 500;", "Is your data already mapped to standard format?")
            ),

            # Vertical connector from question
            tags$div(style = "margin-left: 14px; border-left: 2px solid #a0aec0; height: 12px;"),

            # NO branch row
            tags$div(
              style = "display: flex; align-items: center; gap: 6px; margin-left: 14px;",
              tags$span(style = "background: #e53e3e; color: white; border-radius: 4px; padding: 2px 8px; font-size: 11px; font-weight: bold;", "No"),
              tags$span(style = "color: #2b6cb0; font-size: 14px;", HTML("&#8594;")),
              tags$a(
                href = "#",
                onclick = "document.getElementById('choose_mapper').click(); return false;",
                style = "background: #2b6cb0; color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; text-decoration: none; cursor: pointer; display: inline-flex; align-items: center; gap: 4px; font-size: 13px;",
                icon("columns"), " Column Mapper"
              ),
              tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
              tags$span(style = "color: #2d3748; font-size: 12px;", "Save profile & export")
            ),

            # L-connector row: vertical gray line + blue L going right to Decision Trees position
            tags$div(
              style = "display: flex; margin-left: 14px; height: 45px;",
              # Gray vertical line continuing down
              tags$div(style = "border-left: 2px solid #a0aec0; height: 100%;"),
              # Blue L-connector: starts after "Save profile", goes down then right to Decision Trees
              tags$div(
                style = "position: relative; margin-left: 255px; height: 100%;",
                # Vertical part of L (blue) - extends to bottom
                tags$div(style = "position: absolute; left: 0; top: 0; border-left: 2px solid #2b6cb0; height: 100%;"),
                # Horizontal part of L (blue) at bottom - short to reach Decision Trees
                tags$div(style = "position: absolute; left: 0; bottom: 0; border-top: 2px solid #2b6cb0; width: 30px;"),
                # Arrow pointing right at Decision Trees
                tags$span(style = "position: absolute; left: 28px; bottom: -7px; color: #2b6cb0; font-size: 12px;", HTML("&#9654;"))
              )
            ),

            # YES branch row with Decision Trees positioned to align with L-connector
            tags$div(
              style = "display: flex; align-items: center; gap: 6px; margin-left: 14px;",
              tags$span(style = "background: #38a169; color: white; border-radius: 4px; padding: 2px 8px; font-size: 11px; font-weight: bold;", "Yes"),
              # Green horizontal arrow
              tags$div(style = "width: 230px; height: 2px; background: #38a169;"),
              tags$span(style = "color: #38a169; font-size: 12px; margin-left: -4px;", HTML("&#9654;")),
              # Decision Trees button - positioned to receive both arrows
              tags$a(
                href = "#",
                onclick = "document.getElementById('choose_dt').click(); return false;",
                style = "background: #319795; color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; text-decoration: none; cursor: pointer; display: inline-flex; align-items: center; gap: 4px; font-size: 13px;",
                icon("sitemap"), " Decision Trees"
              ),
              tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
              tags$span(style = "color: #2d3748; font-size: 12px;", "ADF/CMS logic"),
              tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
              # Report Forms button
              tags$a(
                href = "#",
                onclick = "document.getElementById('choose_reports').click(); return false;",
                style = "background: #38a169; color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; text-decoration: none; cursor: pointer; display: inline-flex; align-items: center; gap: 4px; font-size: 13px;",
                icon("file-alt"), " Report Forms"
              )
            )
          ),

          tags$p(
            style = "margin-top: 18px; font-size: 13px; color: #2d3748; background: #fff; padding: 10px; border-radius: 4px; border-left: 3px solid #4299e1;",
            icon("lightbulb", style = "color: #d69e2e;"), " ",
            tags$strong("Tip:"), " Most users start with Column Mapper to standardize their MLS data, then use Decision Trees for analysis, and finally Report Forms to generate appraisal reports."
          )
        ),

        # Sign out link at bottom
        tags$div(
          style = "margin-top: 30px;",
          actionLink("chooser_logout", "Sign out and switch accounts", style = "color: #999; font-size: 12px;")
        )
      )
    )
  ),

  # Main app UI - shown when user is authenticated AND has chosen Column Mapper
  conditionalPanel(
    condition = "output.user_authenticated && output.app_chosen",

    # App navigation bar at top
    div(
      style = "background-color: #2c3e50; padding: 0; margin-bottom: 10px; border-bottom: 2px solid #1a252f;",
      class = "app-nav-bar",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; max-width: 100%; padding: 0 15px;",
        # Left side: App tabs
        div(
          style = "display: flex; align-items: stretch;",
          # Column Mapper tab (active)
          tags$div(
            style = "background: #3498db; color: white; padding: 12px 20px; font-weight: bold; display: flex; align-items: center; gap: 8px;",
            icon("table-columns"),
            "Column Mapper"
          ),
          # Decision Trees tab (clickable)
          actionLink(
            "switch_to_dt",
            tagList(icon("sitemap"), "Decision Trees"),
            style = "background: #34495e; color: #bdc3c7; padding: 12px 20px; display: flex; align-items: center; gap: 8px; text-decoration: none; border: none;",
            class = "app-nav-tab"
          ),
          # Report Forms tab (clickable)
          actionLink(
            "switch_to_reports",
            tagList(icon("file-alt"), "Report Forms"),
            style = "background: #34495e; color: #bdc3c7; padding: 12px 20px; display: flex; align-items: center; gap: 8px; text-decoration: none; border: none;",
            class = "app-nav-tab"
          )
        ),
        # Right side: User info and logout
        div(
          style = "display: flex; align-items: center; gap: 15px; color: #ecf0f1;",
          div(
            style = "display: flex; align-items: center; gap: 8px; font-size: 13px;",
            icon("user-circle"),
            textOutput("current_user_display", inline = TRUE)
          ),
          actionButton("logout_btn", "Sign Out", class = "btn-sm",
                       style = "background: #e74c3c; color: white; border: none;")
        )
      )
    ),

    titlePanel(
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      tags$span(format_build_label()),
      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$span(icon("sun"), style = "font-size: 14px;"),
        materialSwitch("dark_mode", NULL, value = TRUE, status = "primary", inline = TRUE),
        tags$span(icon("moon"), style = "font-size: 14px;")
      )
    )
  ),

  # Hidden input to track last selected column
  tags$input(id = "last_selected_column", type = "hidden", value = ""),

  tags$head(
    tags$title(format_build_label()),
    tags$style(HTML("
      /* Disabled section for destination schema controls */
      #dest_schema_controls.disabled-section {
        opacity: 0.45;
        pointer-events: none;
      }

      /* Light mode (default) */
      body {
        transition: background-color 0.3s ease, color 0.3s ease;
      }

      /* Dark mode styles */
      body.dark-mode {
        background-color: #1a1a1a;
        color: #e0e0e0;
      }
      body.dark-mode .well {
        background-color: #2a2a2a;
        border-color: #444;
      }
      body.dark-mode .container-fluid {
        background-color: #1a1a1a;
      }
      body.dark-mode h1, body.dark-mode h2, body.dark-mode h3,
      body.dark-mode h4, body.dark-mode h5, body.dark-mode h6 {
        color: #e0e0e0;
      }
      body.dark-mode .form-control {
        background-color: #2a2a2a;
        color: #e0e0e0;
        border-color: #444;
      }
      body.dark-mode .selectize-input {
        background-color: #2a2a2a;
        color: #e0e0e0;
        border-color: #444;
      }
      body.dark-mode .selectize-input input {
        background-color: #2a2a2a !important;
        color: #e0e0e0 !important;
      }
      body.dark-mode .selectize-input input::placeholder {
        color: #888 !important;
      }
      body.dark-mode .selectize-dropdown {
        background-color: #2a2a2a;
        color: #e0e0e0;
        border-color: #444;
      }
      body.dark-mode .selectize-dropdown-content .option {
        color: #e0e0e0;
      }
      body.dark-mode .selectize-dropdown-content .option:hover {
        background-color: #3a3a3a;
      }
      body.dark-mode hr {
        border-color: #444;
      }
      body.dark-mode .help-block {
        color: #aaa;
      }
      body.dark-mode pre {
        background-color: #2a2a2a;
        color: #e0e0e0;
        border-color: #444;
      }
      body.dark-mode .btn-default {
        background-color: #3a3a3a;
        color: #e0e0e0;
        border-color: #555;
      }
      body.dark-mode .btn-default:hover {
        background-color: #4a4a4a;
      }
      body.dark-mode .modal-content {
        background-color: #2a2a2a;
        color: #e0e0e0;
      }
      body.dark-mode .modal-header {
        border-bottom-color: #444;
      }
      body.dark-mode .modal-footer {
        border-top-color: #444;
      }
      body.dark-mode .checkbox label {
        color: #e0e0e0;
      }
      body.dark-mode .dataTable {
        background-color: #2a2a2a;
        color: #e0e0e0;
      }
      body.dark-mode .dataTables_wrapper {
        color: #e0e0e0;
      }

      /* DataTable height and text wrapping controls */
      .dataTables_wrapper {
        height: auto;
      }
      .dataTables_scrollBody {
        max-height: 400px !important;
      }
      table.dataTable td {
        word-wrap: break-word;
        word-break: break-word;
        max-width: 300px;
        white-space: normal !important;
      }
      table.dataTable tbody tr {
        height: auto;
      }

      /* Dark mode scrollbar styling */
      body.dark-mode .dataTables_scrollBody::-webkit-scrollbar {
        width: 10px;
        height: 10px;
      }
      body.dark-mode .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #2a2a2a;
      }
      body.dark-mode .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: #555;
        border-radius: 5px;
      }
      body.dark-mode .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
        background: #666;
      }

      body.dark-mode .start-here-box {
        background: #1a3a4a !important;
        border-color: #3a7a9f !important;
      }
      body.dark-mode .start-here-box h4 {
        color: #5ab4e8 !important;
      }
      body.dark-mode .btn-success {
        background-color: #2d6a2d;
        border-color: #2d6a2d;
      }
      body.dark-mode .btn-success:hover {
        background-color: #3d7a3d;
      }
      body.dark-mode .btn-primary {
        background-color: #2d4a6a;
        border-color: #2d4a6a;
      }
      body.dark-mode .btn-primary:hover {
        background-color: #3d5a7a;
      }
      body.dark-mode .btn-info {
        background-color: #2d5a6a;
        border-color: #2d5a6a;
      }
      body.dark-mode .btn-info:hover {
        background-color: #3d6a7a;
      }
      body.dark-mode .preview-content {
        background-color: #1a1a1a !important;
        color: #e0e0e0 !important;
        border: 1px solid #444 !important;
      }
      body.dark-mode .preview-content li {
        color: #e0e0e0;
      }
      .mapping-card {
        background-color: #ffffff;
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
      body.dark-mode .mapping-card {
        background-color: #1f2833;
        border-color: #36485a;
      }
      body.dark-mode .mapping-card small,
      body.dark-mode .mapping-card .help-block {
        color: #aebfd0;
      }
      .destination-box {
        transition: background-color 0.3s ease, color 0.3s ease, border-color 0.3s ease;
      }
      body.dark-mode .destination-box {
        background-color: #1f2833 !important;
        border-color: #36485a !important;
        color: #dce6f3;
      }
      body.dark-mode .destination-box .help-block,
      body.dark-mode .destination-status {
        color: #aebfd0;
      }
      .destination-status ul li {
        margin-bottom: 4px;
      }
      body.dark-mode .modal-body p {
        color: #e0e0e0;
      }
      body.dark-mode .modal-body strong {
        color: #5ab4e8;
      }
      body.dark-mode .unmapped-well {
        background-color: #2a2a2a !important;
      }
      body.dark-mode .unmapped-card {
        background-color: #1a1a1a !important;
        border-color: #444 !important;
      }
      body.dark-mode .unmapped-card strong {
        color: #e0e0e0;
      }
      body.dark-mode .save-profile-highlight {
        background-color: #3d3520 !important;
        border-color: #8a6d3b !important;
      }
      body.dark-mode .save-profile-highlight h4 {
        color: #f0ad4e !important;
      }
      body.dark-mode .save-profile-highlight .help-block {
        color: #f0ad4e !important;
      }

      /* Analytics section styles */
      .analytics-container {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      body.dark-mode .analytics-container {
        background: #2a2a2a;
      }
      .analytics-info-blue {
        background: #e3f2fd;
        border-left: 4px solid #2196F3;
        padding: 12px;
        margin-bottom: 15px;
        color: #1a1a1a;
      }
      body.dark-mode .analytics-info-blue {
        background: #1a2a3a;
        border-left: 4px solid #2196F3;
        color: #e0e0e0;
      }
      .analytics-info-orange {
        background: #fff3e0;
        border-left: 4px solid #FF9800;
        padding: 12px;
        margin-bottom: 15px;
        color: #1a1a1a;
      }
      body.dark-mode .analytics-info-orange {
        background: #3a2a1a;
        border-left: 4px solid #FF9800;
        color: #e0e0e0;
      }
      .analytics-info-green {
        background: #e8f5e9;
        border-left: 4px solid #4CAF50;
        padding: 12px;
        margin-bottom: 15px;
        color: #1a1a1a;
      }
      body.dark-mode .analytics-info-green {
        background: #1a3a1a;
        border-left: 4px solid #4CAF50;
        color: #e0e0e0;
      }
      .analytics-info-purple {
        background: #f3e5f5;
        border-left: 4px solid #9C27B0;
        padding: 12px;
        margin-bottom: 15px;
        color: #1a1a1a;
      }
      body.dark-mode .analytics-info-purple {
        background: #2a1a3a;
        border-left: 4px solid #9C27B0;
        color: #e0e0e0;
      }
      .analytics-info-pink {
        background: #fce4ec;
        border-left: 4px solid #E91E63;
        padding: 12px;
        margin-bottom: 15px;
        color: #1a1a1a;
      }
      body.dark-mode .analytics-info-pink {
        background: #3a1a2a;
        border-left: 4px solid #E91E63;
        color: #e0e0e0;
      }

      /* Version footer styles */
      .version-footer {
        text-align: center;
        padding: 20px;
        color: #666;
        font-size: 0.9em;
      }
      body.dark-mode .version-footer {
        color: #999;
      }
      body.dark-mode .version-footer a {
        color: #5ab4e8;
      }

      /* Column Classification Panel styles */
      .column-classification-panel {
        margin-top: 20px;
      }
      .column-classification-panel > div:first-child {
        transition: background-color 0.3s ease;
      }
      body.dark-mode .column-classification-panel > div:first-child {
        background: #2a2a2a !important;
        border-color: #444;
      }
      body.dark-mode .column-classification-panel h4 {
        color: #e0e0e0;
      }
      .classification-content {
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
      body.dark-mode .classification-content {
        background: #1a1a1a !important;
        border-color: #444 !important;
      }
      body.dark-mode .classification-content .help-text {
        color: #aaa;
      }
      body.dark-mode .alert-warning {
        background-color: #3d3520 !important;
        border-color: #8a6d3b !important;
        color: #f0ad4e !important;
      }

      /* Badge styles */
      .pill {
        display: inline-block;
        padding: 2px 6px;
        border-radius: 10px;
        font-size: 11px;
        margin-left: 6px;
        font-weight: bold;
      }
      .ok {
        background: #eaf7ea;
        color: #2e7d32;
        border: 1px solid #b6e0b6;
      }
      body.dark-mode .ok {
        background: #1b4d1b;
        color: #90ee90;
        border: 1px solid #2e7d32;
      }
      .match {
        background: #e3f2fd;
        color: #1565c0;
        border: 1px solid #90caf9;
      }
      body.dark-mode .match {
        background: #0d47a1;
        color: #90caf9;
        border: 1px solid #1565c0;
      }
      .warn {
        background: #fff7e6;
        color: #8a6d3b;
        border: 1px solid #ffe1b3;
      }
      body.dark-mode .warn {
        background: #4d3d1a;
        color: #ffd966;
        border: 1px solid #8a6d3b;
      }
      .bad {
        background: #fdecea;
        color: #a94442;
        border: 1px solid #f5c6cb;
      }
      body.dark-mode .bad {
        background: #4d1a1a;
        color: #ff6b6b;
        border: 1px solid #a94442;
      }
      #mapped_data_preview table.dataTable th,
      #mapped_data_preview table.dataTable td {
        white-space: nowrap !important;
        text-overflow: ellipsis;
        overflow: hidden;
        color: #1a1a1a;
        line-height: 1.3;
      }
      #mapped_data_preview table.dataTable th {
        max-width: 50ch;
      }
      #mapped_data_preview table.dataTable td {
        max-width: 50ch;
      }
      body.dark-mode #mapped_data_preview table.dataTable th,
      body.dark-mode #mapped_data_preview table.dataTable td {
        color: #f5f5f5;
        background-color: #111 !important;
      }
      .mapped-badge {
        background: #28a745;
        color: white;
        padding: 2px 6px;
        border-radius: 3px;
        font-size: 10px;
        margin-left: 8px;
        font-weight: bold;
      }
      .unmapped-badge {
        background: #dc3545;
        color: white;
        padding: 2px 6px;
        border-radius: 3px;
        font-size: 10px;
        margin-left: 8px;
        font-weight: bold;
      }
      .suggested-badge {
        background: #ffc107;
        color: #000;
        padding: 2px 6px;
        border-radius: 3px;
        font-size: 10px;
        margin-left: 8px;
        font-weight: bold;
      }
      .review-badge {
        background: #f0ad4e;
        color: white;
        padding: 2px 6px;
        border-radius: 3px;
        font-size: 10px;
        margin-left: 8px;
        font-weight: bold;
      }
      .skipped-badge {
        background: #999;
        color: white;
        padding: 2px 6px;
        border-radius: 3px;
        font-size: 10px;
        margin-left: 8px;
        font-weight: bold;
      }

      /* Fix contrast for verbatimTextOutput in both modes */
      pre, .shiny-text-output {
        color: #212529 !important;
        background-color: #f8f9fa !important;
        border: 1px solid #ced4da !important;
      }

      body.dark-mode pre,
      body.dark-mode .shiny-text-output {
        color: #e0e0e0 !important;
        background-color: #2d2d2d !important;
        border: 1px solid #444 !important;
      }

      /* Fix contrast for collapsible summary headers */
      details summary {
        color: #212529 !important;
        transition: background-color 0.2s ease;
      }

      details summary:hover {
        background: #e2e6ea !important;
      }

      body.dark-mode details summary {
        color: #e0e0e0 !important;
        background: #2d2d2d !important;
      }

      body.dark-mode details summary:hover {
        background: #3a3a3a !important;
      }

      /* Rotate chevron when details is open */
      details[open] summary .expand-icon {
        transform: rotate(180deg);
      }

      .expand-icon {
        transition: transform 0.2s ease;
        color: #6c757d;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Dark mode toggle functionality
        // Load saved preference from localStorage; default to dark on first visit
        var stored = localStorage.getItem('darkMode');
        var darkMode = (stored === null) ? true : (stored === 'true');
        if (stored === null && darkMode === true) {
          // Persist the default so subsequent loads are consistent per-origin
          localStorage.setItem('darkMode', 'true');
        }
        if (darkMode) {
          $('body').addClass('dark-mode');
          // Update the switch to match (with small delay for Shiny initialization)
          setTimeout(function() {
            Shiny.setInputValue('dark_mode', true);
          }, 100);
        }

        // Listen for dark mode toggle changes
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'dark_mode') {
            if (event.value === true) {
              $('body').addClass('dark-mode');
              localStorage.setItem('darkMode', 'true');
            } else {
              $('body').removeClass('dark-mode');
              localStorage.setItem('darkMode', 'false');
            }
          }

          // Check if this is a selectize input (mapping field)
          if (event.name && event.name.startsWith('map_')) {
            // Update the last selected column with the new value
            if (event.value && event.value !== '') {
              Shiny.setInputValue('last_selected_column', event.value, {priority: 'event'});
            }

            // Enable/disable preview button based on selection
            var targetName = event.name.replace('map_', '');
            // Use getElementById to handle potential special characters in IDs
            var previewBtn = $(document.getElementById('preview_' + targetName));
            if (previewBtn.length) {
              if (event.value && event.value !== '') {
                previewBtn.prop('disabled', false);
                previewBtn.css({'pointer-events': 'auto', 'opacity': '1'});
              } else {
                previewBtn.prop('disabled', true);
                previewBtn.css({'pointer-events': 'none', 'opacity': '0.45'});
              }
            }
          }

          // Handle destination mode selectors (dest_*_selector)
          if (event.name && event.name.startsWith('dest_') && event.name.endsWith('_selector')) {
            var destCol = event.name.replace('dest_', '').replace('_selector', '');
            // Use getElementById to avoid jQuery selector issues with special characters (like dots from make.names)
            var destPreviewBtn = $(document.getElementById('preview_dest_' + destCol));
            if (destPreviewBtn.length) {
              if (event.value && event.value !== '') {
                destPreviewBtn.prop('disabled', false);
                destPreviewBtn.css({'pointer-events': 'auto', 'opacity': '1'});
              } else {
                destPreviewBtn.prop('disabled', true);
                destPreviewBtn.css({'pointer-events': 'none', 'opacity': '0.45'});
              }
            }
          }
        });

        // Class filter checkboxes - send combined state to Shiny
        function updateClassFilter() {
          var selected = [];
          if ($('#filter_class_1').is(':checked')) selected.push('1');
          if ($('#filter_class_2').is(':checked')) selected.push('2');
          if ($('#filter_class_3').is(':checked')) selected.push('3');
          if ($('#filter_class_missing').is(':checked')) selected.push('missing');
          Shiny.setInputValue('mapping_class_filter', selected, {priority: 'event'});
        }

        // Bind checkbox change events
        $(document).on('change', '#filter_class_1, #filter_class_2, #filter_class_3, #filter_class_missing', function() {
          updateClassFilter();
        });

        // Initialize on page load (after Shiny is ready)
        $(document).on('shiny:connected', function() {
          updateClassFilter();
        });
      });
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      tags$div(
        class = "start-here-box",
        style = "background: #d9edf7; border: 2px solid #31708f; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
        tags$h4(
          style = "margin: 0; color: #31708f;",
          icon("arrow-down"), " UPLOAD ORDER"
        ),
        helpText(style = "margin: 5px 0 0 0; color: #31708f; font-weight: bold;",
          "1) Profile → 2) Destination (optional) → 3) Data")
      ),
      uiOutput("workflow_sidebar"),
      tags$div(
        class = "destination-box",
        style = "background: #f8f9fa; border: 1px solid #cbd3da; border-radius: 6px; padding: 12px; margin-bottom: 20px; margin-top: 15px;",
        h4(style = "margin-top: 0;", icon("exchange-alt"), " Destination Schema"),
        helpText(
          "The mapper uses CValR standard fields by default. ",
          "Check the box below to upload a custom destination schema or select a different template."
        ),
        checkboxInput("use_destination_schema", "Use destination schema", value = FALSE),
        tags$div(
          id = "dest_schema_controls",
          class = "disabled-section",
          fileInput(
            "dest_schema_file",
            "Upload destination dataset (CSV or Excel)",
            accept = c(".csv", ".xlsx", ".xls")
          ),
          tags$div(
            style = "margin: 10px 0; padding: 8px; background: #f8f9fa; border-radius: 4px;",
            tags$strong("Or load a built-in template:"),
            selectInput(
              "preset_destination_schema",
              label = NULL,
              choices = c(
                "Select a template..." = "",
                "RESO Residential Core (64 fields)" = "reso_core",
                "RESO Data Dictionary Full (342 fields)" = "reso_full",
                "CValR 4.0 Standard (51 fields)" = "cvalr_legacy"
              ),
              selected = "",
              width = "100%"
            )
          ),
          helpText(
            tags$span(icon("info-circle"), style = "margin-right: 4px;"),
            "Limit the export to destination-only columns using the checkbox near the Download button below."
          ),
          actionLink("clear_destination_schema", "Clear destination schema")
        ),
        uiOutput("destination_schema_status")
      )
    ),

    mainPanel(
      fluidRow(
        column(8, h3("Map Your Columns")),
        column(4,
          div(style = "margin-top: 15px; text-align: right;",
            actionBttn("refresh_badges", "Refresh Mappings",
                      style = "fill", color = "warning",
                      icon = icon("sync"), size = "sm")
          )
        )
      ),

      # Column Classification Panel (collapsible) - BEFORE mapping section
      tags$div(
        class = "column-classification-panel",
        style = "margin-bottom: 20px;",
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; cursor: pointer; padding: 10px; background: #f8f9fa; border-radius: 5px;",
          onclick = "$(this).next('.classification-content').slideToggle(); $(this).find('.toggle-icon').toggleClass('fa-chevron-down fa-chevron-up');",
          tags$div(
            tags$h4(
              style = "margin: 0; display: inline-flex; align-items: center; gap: 8px;",
              icon("layer-group"),
              "Column Classification",
              tags$span(
                class = "toggle-icon fa fa-chevron-up",
                style = "font-size: 14px; margin-left: 10px;"
              )
            )
          ),
          uiOutput("classification_summary_badge")
        ),
        tags$div(
          class = "classification-content",
          style = "padding: 15px; border: 1px solid #ddd; border-top: none; border-radius: 0 0 5px 5px;",
          helpText(
            "Column classification helps identify which columns are most useful for analysis. ",
            tags$strong("Class 1 (Essential):"), " Core analytical fields. ",
            tags$strong("Class 2 (Supplemental):"), " Supporting calculations. ",
            tags$strong("Class 3 (Marginal):"), " Potentially useful. ",
            tags$strong("Class 4 (Exclude):"), " PII/internal - never export."
          ),
          fluidRow(
            column(4,
              selectInput(
                "classification_filter_class",
                "Filter by Class:",
                choices = c(
                  "All Classes" = "all",
                  "Class 1 (Essential)" = "1",
                  "Class 2 (Supplemental)" = "2",
                  "Class 3 (Marginal)" = "3",
                  "Class 4 (Exclude)" = "4",
                  "Unclassified" = "unclassified",
                  "Has Issues" = "has_issues"
                ),
                selected = "all"
              )
            ),
            column(8,
              tags$div(
                style = "margin-top: 25px; display: flex; gap: 8px; flex-wrap: wrap; align-items: center;",
                actionBttn("apply_bulk_class3", "Low-Value → 3",
                          style = "bordered", color = "warning", size = "sm",
                          icon = icon("arrow-down")),
                actionBttn("apply_bulk_class4", "PII/Internal → 4",
                          style = "bordered", color = "royal", size = "sm",
                          icon = icon("user-shield")),
                tags$span(style = "border-left: 1px solid #ccc; margin: 0 8px;"),
                tags$span(style = "line-height: 32px; color: #666;", "Set selected:"),
                actionBttn("set_selected_class1", "1",
                          style = "bordered", color = "success", size = "sm"),
                actionBttn("set_selected_class2", "2",
                          style = "bordered", color = "primary", size = "sm"),
                actionBttn("set_selected_class3", "3",
                          style = "bordered", color = "warning", size = "sm"),
                actionBttn("set_selected_class4", "4",
                          style = "bordered", color = "danger", size = "sm")
              )
            )
          ),
          DT::dataTableOutput("classification_table"),
          tags$div(
            style = "margin-top: 10px;",
            uiOutput("duplicate_warning")
          )
        )
      ),
      hr(),

      # Mapping Section
      div(
        class = "mapping-search-bar",
        style = "display: flex; gap: 15px; align-items: flex-end; flex-wrap: wrap;",
        div(
          style = "flex: 1; min-width: 200px;",
          textInput(
            "mapping_search",
            label = "Search mapping fields",
            value = "",
            placeholder = "e.g. price, bath, postal",
            width = "100%"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          tags$label("Filter by suggested source class:",
                     style = "font-weight: bold; margin-bottom: 2px;",
                     title = "Show target fields based on the classification of their suggested source column"),
          div(
            style = "display: flex; gap: 12px; flex-wrap: wrap; align-items: center;",
            tags$label(
              style = "display: inline-flex; align-items: center; gap: 4px; cursor: pointer; white-space: nowrap;",
              tags$input(type = "checkbox", id = "filter_class_1", checked = "checked"),
              tags$span("1", style = "background: #5cb85c; color: white; padding: 2px 6px; border-radius: 3px; font-size: 11px; font-weight: bold;"),
              "Essential"
            ),
            tags$label(
              style = "display: inline-flex; align-items: center; gap: 4px; cursor: pointer; white-space: nowrap;",
              tags$input(type = "checkbox", id = "filter_class_2", checked = "checked"),
              tags$span("2", style = "background: #337ab7; color: white; padding: 2px 6px; border-radius: 3px; font-size: 11px; font-weight: bold;"),
              "Supplemental"
            ),
            tags$label(
              style = "display: inline-flex; align-items: center; gap: 4px; cursor: pointer; white-space: nowrap;",
              tags$input(type = "checkbox", id = "filter_class_3", checked = "checked"),
              tags$span("3", style = "background: #f0ad4e; color: white; padding: 2px 6px; border-radius: 3px; font-size: 11px; font-weight: bold;"),
              "Marginal"
            ),
            tags$label(
              style = "display: inline-flex; align-items: center; gap: 4px; cursor: pointer; white-space: nowrap;",
              tags$input(type = "checkbox", id = "filter_class_missing", checked = "checked"),
              tags$span("?", style = "background: #999; color: white; padding: 2px 6px; border-radius: 3px; font-size: 11px; font-weight: bold;"),
              "No Match"
            ),
            # Separator and bulk action
            tags$span(style = "border-left: 1px solid #ccc; height: 20px; margin: 0 8px;"),
            actionButton("accept_all_class3", "Accept All Class 3",
                        class = "btn-warning btn-sm",
                        style = "padding: 2px 8px; font-size: 11px;")
          )
        )
      ),
      uiOutput("mapping_ui"),
      hr(),
      fluidRow(
        column(6,
          actionBttn("update_unmapped", "Update Unmapped Columns",
                     style = "fill", color = "warning", icon = icon("refresh"),
                     size = "md")
        ),
        column(6,
          p(style = "font-size: 13px; color: #666; margin-top: 8px;",
            "Click to show columns that haven't been mapped yet")
        )
      ),
      uiOutput("unmapped_columns_ui"),
      uiOutput("unused_target_fields_ui"),
      div(
        style = "margin: 20px 0 10px;",
        checkboxInput(
          "keep_merged_sources",
          "Keep original source columns when fields are merged (recommended for Address components)",
          value = TRUE
        )
      ),
      # Export class filter checkboxes
      fluidRow(
        column(12,
          tags$label("Include classes in export:", style = "font-weight: bold; margin-right: 15px;"),
          checkboxGroupInput(
            "export_class_filter",
            label = NULL,
            choices = c(
              "1 Essential" = "1",
              "2 Supplemental" = "2",
              "3 Marginal" = "3"
            ),
            selected = c("1", "2", "3"),
            inline = TRUE
          )
        )
      ),
      hr(),
      fluidRow(
        column(3, actionBttn("show_mappings", "Show All Mappings", style = "fill", color = "primary")),
        column(3, actionBttn("apply_mappings", "Apply Mappings & Finalize", style = "fill", color = "success")),
        column(3, downloadButton("download_mapped", "Download Mapped CSV", class = "btn-primary")),
        column(
          3,
          # Column filtering options (grouped)
          tags$div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 8px; margin-bottom: 8px; background: #f9f9f9;",
            tags$strong("Column Filtering", style = "font-size: 11px; color: #666; display: block; margin-bottom: 6px;"),
            checkboxInput(
              "restrict_to_destination_only",
              "Restrict to destination schema columns",
              value = TRUE
            ),
            tags$div(
              style = "margin-left: 20px; margin-top: -5px;",
              checkboxInput(
                "include_listing_id",
                tags$span("Exception: Include MLS#/ListingId", style = "font-size: 12px;"),
                value = TRUE
              )
            )
          ),
          # Other export options
          checkboxInput("embed_build_info", "Embed mapper build info in output", value = TRUE),
          checkboxInput(
            "include_derived_metrics",
            sprintf(
              "Include derived metrics (%s)",
              paste(DERIVED_METRIC_COLUMNS, collapse = ", ")
            ),
            value = TRUE
          )
        )
      ),

      # Save Mappings Section - moved from sidebar to main panel
      uiOutput("save_mappings_section"),

      # Next Step: Decision Trees - shown after mapping is applied
      uiOutput("next_step_decision_trees"),

      # Data Enrichment: Geocoding, status normalization feedback
      uiOutput("enrich_data_section"),

      hr(),
      h4("Mapped Data Preview"),
      tags$div(
        id = "mapped-data-container",
        style = "height: 450px; overflow-y: auto;",
        DT::dataTableOutput("mapped_data_preview")
      ),
      hr(),
      h3("Real Estate Market Analytics", style = "margin-top: 30px;"),
      helpText("Educational visualizations to understand your real estate data. Each plot includes an explanation of its usefulness and limitations."),
      hr(),

      # Plot 1: Price vs Date Trend
      tags$div(
        class = "analytics-container",
        h4("1. Sales Price Trend Over Time"),
        tags$div(
          class = "analytics-info-blue",
          tags$strong("What it shows:"), " How median/average sale prices change over time with trend lines.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Identifies market trends (rising, falling, or stable prices). The linear trend shows overall direction, while the polynomial captures seasonal variations or market cycles.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Past trends don't guarantee future performance. External factors (interest rates, economic changes) not shown. Outliers can skew averages."
        ),
        plotOutput("sales_scatter_plot", height = "500px")
      ),

      hr(),

      # Plot 2: Price Distribution
      tags$div(
        class = "analytics-container",
        h4("2. Price Distribution Histogram"),
        tags$div(
          class = "analytics-info-orange",
          tags$strong("What it shows:"), " The frequency of sales at different price points (how many properties sold in each price range).",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Reveals the most common price ranges, identifies market segments, shows if prices are normally distributed or skewed. Helps buyers/sellers understand where most market activity occurs.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Doesn't account for property characteristics (size, location, condition). A $500K condo and $500K house are counted equally."
        ),
        plotOutput("price_distribution_plot", height = "400px")
      ),

      hr(),

      # Plot 3: Price by Neighborhood
      tags$div(
        class = "analytics-container",
        h4("3. Price Comparison by Neighborhood"),
        tags$div(
          class = "analytics-info-green",
          tags$strong("What it shows:"), " Box plots comparing price distributions across different neighborhoods. The box shows the middle 50% of prices, the line is the median.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Quickly identify expensive vs affordable areas. See price variability (wide boxes = diverse housing). Spot outliers (luxury homes or distressed sales). Essential for location-based pricing.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Doesn't control for property size, age, or condition. Small sample sizes in some neighborhoods can be misleading. Neighborhood boundaries may be arbitrary."
        ),
        plotOutput("price_by_neighborhood_plot", height = "500px")
      ),

      hr(),

      # Plot 4: Sales Volume
      tags$div(
        class = "analytics-container",
        h4("4. Market Activity: Sales Volume Over Time"),
        tags$div(
          class = "analytics-info-purple",
          tags$strong("What it shows:"), " Number of properties sold each month, showing market activity levels.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Identifies busy vs slow seasons (spring/summer typically higher). Shows market health - high volume suggests strong demand. Helps sellers time listings. Reveals market cycles.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " High volume doesn't always mean high prices. Doesn't show pending sales or new listings. Seasonal patterns vary by region."
        ),
        plotOutput("sales_volume_plot", height = "400px")
      ),

      hr(),

      # Plot 5: Price per Square Foot
      tags$div(
        class = "analytics-container",
        h4("5. Price per Square Foot Analysis"),
        tags$div(
          class = "analytics-info-pink",
          tags$strong("What it shows:"), " Relationship between property size and total price, with price-per-sqft trend line.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Best metric for comparing properties of different sizes. Shows if larger homes command premium or discount per sqft. Helps identify good value (below trend line) vs overpriced (above trend line).",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Doesn't account for location, age, condition, or lot size. Very small or very large properties may skew results. Luxury features don't scale linearly with square footage."
        ),
        plotOutput("price_per_sqft_plot", height = "500px")
      ),

      hr(),

      # Plot 6: Price per Square Foot vs Square Footage
      tags$div(
        class = "analytics-container",
        h4("6. Price per Square Foot vs Property Size"),
        tags$div(
          class = "analytics-info-blue",
          tags$strong("What it shows:"), " How price per square foot changes with property size.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Reveals if larger properties are priced at a premium or discount per square foot. Flat trend means consistent $/sqft across sizes. Downward trend suggests economies of scale (larger = cheaper per sqft). Upward trend indicates size premium.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Doesn't account for location, quality, or features. Mix of property types (condos vs houses) can skew results."
        ),
        plotOutput("price_per_sqft_vs_sqft_plot", height = "500px")
      ),

      hr(),

      # Plot 7: Price by Bedrooms
      tags$div(
        class = "analytics-container",
        h4("7. Price Distribution by Number of Bedrooms"),
        tags$div(
          class = "analytics-info-green",
          tags$strong("What it shows:"), " Price distribution across different bedroom counts. The box shows the middle 50% of prices, the line is the median price.",
          tags$br(), tags$br(),
          tags$strong("Usefulness:"), " Reveals the bedroom premium - how much each additional bedroom adds to the price. Helps buyers understand value gaps between 2BR, 3BR, 4BR properties. Essential for pricing similar-sized homes in different configurations.",
          tags$br(), tags$br(),
          tags$strong("Limitations:"), " Doesn't account for location, property size, or condition. A 2BR mansion and 2BR condo are counted equally. Wide boxes indicate high variability within that bedroom count."
        ),
        plotOutput("price_by_bedrooms_plot", height = "500px")
      ),

      # Version Footer
      hr(),
      tags$div(
        class = "version-footer",
        tags$p(
          format_build_label(),
          tags$br(),
          sprintf("Last Updated: %s", APP_VERSION_DATE),
          tags$br(),
          tags$a(href = "https://github.com/CAA-EBV-CO-OP/ColumnMapperCValR",
                 target = "_blank",
                 "GitHub Repository")
        )
      )
    )
  )
  )  # Close conditionalPanel for authenticated content

  # Include shinyjs for show/hide functionality
  ,shinyjs::useShinyjs()
)

