# =============================================================================
# Decision Trees UI Module
# File: modules/decision_trees/dt_ui.R
#
# UI definition for the CA-ADF Decision Tree Tool.
# Extracted from the monolithic app.R and converted to a Shiny module.
# All input/output IDs are wrapped in ns() for namespace isolation.
#
# Source: CA-ADF-CMS-DecisionTrees/app.R (lines 225-893)
# =============================================================================

# --- CSS Styles for Variable Checkboxes ---
variable_checkbox_styles <- tags$style(HTML('
  .variable-checkbox-grid {
    text-align: left;
  }
  .variable-checkbox-column {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
  }
  .variable-checkbox {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 2px 0;
  }
  .variable-checkbox .shiny-input-container,
  .variable-checkbox .form-group,
  .variable-checkbox .checkbox,
  .variable-checkbox .form-check {
    margin: 0;
    padding: 0;
    width: auto;
    display: flex;
    align-items: center;
    flex: none;
  }
  .variable-checkbox .form-check-label {
    margin: 0;
    padding: 0;
    display: flex;
    align-items: center;
  }
  .variable-checkbox .form-check-input,
  .variable-checkbox input[type="checkbox"] {
    width: 20px;
    height: 20px;
    margin: 0;
    flex-shrink: 0;
    cursor: pointer;
    border: 2px solid #4b5563 !important;
    border-radius: 4px;
    background-color: #fff;
    box-shadow: none;
    transition: none;
  }
  .variable-checkbox .form-check-input:hover,
  .variable-checkbox input[type="checkbox"]:hover {
    border-color: #2563eb !important;
  }
  .variable-checkbox .form-check-input:focus,
  .variable-checkbox input[type="checkbox"]:focus {
    box-shadow: 0 0 0 0.15rem rgba(13, 110, 253, 0.25);
    outline: none;
  }
  .variable-checkbox .form-check-input:checked,
  .variable-checkbox input[type="checkbox"]:checked {
    background-color: #0d6efd;
    border-color: #0d6efd !important;
  }
  .variable-checkbox .variable-preview-btn {
    padding: 2px 6px;
    font-size: 0.8rem;
    flex-shrink: 0;
  }
  .variable-checkbox .variable-name {
    user-select: none;
    cursor: pointer;
    flex: 1;
  }
'))

# =============================================================================
# dt_ui_content
#
# Returns the UI content for the Decision Trees module as a tagList().
# The host application provides the outer page_navbar; this function returns
# the tab panels and supporting elements.
#
# @param ns  Namespace function from the module (e.g., NS("decision_trees"))
# @return    A tagList containing all Decision Tree UI elements
# =============================================================================
dt_ui_content <- function(ns) {
  tagList(
    tags$head(variable_checkbox_styles),

    # Sub-navigation for Decision Trees tabs
    navset_tab(
      id = ns("dt_tabs"),

    # ========================================================================
    # Tab 1: The Problem & Upload Data
    # ========================================================================
    nav_panel(
      "1. The Problem",
      layout_sidebar(
        sidebar = sidebar(
          title = "Upload Your Data",
          fileInput(ns("adf_file"),
                    "Upload ADF (Excel or CSV)",
                    accept = c(".xlsx", ".csv", ".rds")),
          hr(),
          h5("Subject Property (Optional)"),
          p("You can define subject later", class = "text-muted small")
        ),

        # Main content
        div(
          h2("From the ADF to the CMS with Decision Trees"),
          hr(),

          div(class = "card mb-3",
            div(class = "card-body",
              h4("The Challenge", class = "card-title"),
              p(class = "lead",
                "How do we objectively select comparable sales from a large MLS export?"
              ),
              tags$ul(
                tags$li("MLS exports often contain 200-300 sales"),
                tags$li("We need only 10-20 meaningful comps"),
                tags$li("Manual selection takes 30-60 minutes"),
                tags$li("Subjective bias can creep in")
              )
            )
          ),

          div(class = "card mb-3",
            div(class = "card-body bg-light",
              h4("The Solution: Decision Trees", class = "card-title"),
              p("Decision Trees provide an", strong("objective, data-driven"),
                "method to partition your Assignment Data Frame (ADF) into",
                strong("homogeneous groups"), "called Competitive Market Segments (CMS)."),

              h5("Why Decision Trees?", class = "mt-3"),
              tags$ul(
                tags$li(strong("Objective:"), "Algorithm decides which features matter most"),
                tags$li(strong("Feature Importance:"), "Shows which characteristics drive price"),
                tags$li(strong("Transparent:"), "Creates rules you can explain and defend"),
                tags$li(strong("EBV Aligned:"), "Respects Evidence-Based Valuation principles"),
                tags$li(strong("USPAP/CUSPAP Aligned:"), "When properly documented")
              )
            )
          ),

          div(class = "card mb-3",
            div(class = "card-body",
              h4("How It Works (Overview)", class = "card-title"),
              p("The algorithm follows these steps:"),
              tags$ol(
                tags$li(strong("Analyze ADF:"), "Examines all sales and their characteristics"),
                tags$li(strong("Find Splits:"), "Identifies features that best predict price"),
                tags$li(strong("Create Rules:"), "Builds decision rules (e.g., 'If Condition=Excellent AND Bathrooms>2...')"),
                tags$li(strong("Assign Subject:"), "Subject follows rules to find its CMS"),
                tags$li(strong("Extract CMS:"), "Returns only sales in subject's terminal node")
              ),

              div(class = "alert alert-info mt-3", role = "alert",
                icon("lightbulb"), strong(" Key Insight:"),
                " Each terminal node in the tree represents a potential CMS. The algorithm finds which one matches your subject."
              )
            )
          ),

          # Data upload status
          uiOutput(ns("data_status"))
        )
      )
    ),

    # ========================================================================
    # Tab 2: Prepare Data
    # ========================================================================
    nav_panel(
      "2. Prepare Data",
      div(
        h2("Prepare, Analyze, and Transform Your Data"),
        hr(),
        p(class = "lead",
          "This section allows you to review data quality, explore your ADF, and apply transformations to prepare it for modeling."
        ),

        navset_card_tab(
          id = ns("prepare_data_tabs"),

          # Tab 1: Data Quality Report
          nav_panel(
            "1. Quality Report",
            div(class = "p-3",
              h4("Automated Data Health Analysis"),

              # --- Data Conversion Guide (Helper) ---
              accordion(
                accordion_panel(
                  "Check First: Understanding Data Types & Conversion",
                  icon = icon("question-circle"),
                  div(class = "text-muted small",
                    p("This tool scans your data for columns that need formatting before they can be used in a Decision Tree."),
                    tags$ul(
                      tags$li(strong("Numeric (#):"), " Use for measurements (Price, SqFt, Year). Allows the model to use math (e.g., 'Price > 500k')."),
                      tags$li(strong("Factor (List):"), " Use for categories (City, Style, Zip Code). Groups identical text together so the model can compare them."),
                      tags$li(strong("Note:"), " Converting to a Factor does NOT delete your text. It simply organizes it into groups. It is safe to do.")
                    ),
                    p("Use the ", icon("eye"), " Inspect button to preview your data before converting.")
                  )
                ),
                open = FALSE # Closed by default to save space
              ),
              hr(),

              # Placeholder for the new Data Quality Report UI
              uiOutput(ns("data_quality_report_ui"))
            )
          ),

          # Tab 2: Explore ADF
          nav_panel(
            "2. Explore ADF",
            layout_sidebar(
              sidebar = sidebar(
                title = "Exploration Options",
                helpText("Explore your Assignment Data Frame"),
                checkboxInput(ns("show_all_columns"), "Show all columns", FALSE),
                downloadButton(ns("download_adf_summary"), "Download Summary")
              ),
              div(class = "p-3",
                # Data preview
                h4("Data Preview (First 10 Rows)"),
                DTOutput(ns("adf_preview")),
                hr(),
                # Summary statistics
                h4("Summary Statistics"),
                verbatimTextOutput(ns("adf_summary")),
                hr(),
                # Price distribution
                h4("Price Distribution"),
                plotlyOutput(ns("price_histogram"), height = "400px"),
                hr(),
                # Feature summary
                h4("Feature Overview"),
                tableOutput(ns("feature_summary"))
              )
            )
          ),

          # Tab 3: Transformations (Placeholder)
          nav_panel(
            "3. Transformations",
            div(class = "p-3",
              h4("Data Transformation Engine"),
              p("This section will house the tools for feature engineering and column encoding. (Coming in Stage 3)"),
              # Placeholder for transformation UI
              uiOutput(ns("transformation_ui_placeholder"))
            )
          )
        )
      )
    ),

    # ========================================================================
    # Tab 3: Build Decision Tree
    # ========================================================================
    nav_panel(
      "3. Build Tree",
      layout_sidebar(
        sidebar = sidebar(
          title = "Tree Settings",

          selectInput(ns("response_var"),
                     "Response Variable (Price)",
                     choices = NULL),

          hr(),

          # Toggle for temporal/market variables
          checkboxInput(ns("include_temporal_vars"),
                       HTML("<strong>Include Temporal/Market Variables</strong>"),
                       value = FALSE),
          helpText(
            HTML("<small><strong>SaleQtr, DOM, SPAP</strong><br>
                 \u2713 <em>Charlie's approach:</em> Include to capture market trends within tree<br>
                 \u2717 <em>Alternative:</em> Exclude for separate price indexing later</small>")
          ),

          hr(),

          # Model comparison toggle
          checkboxInput(ns("compare_models"),
                       HTML("<strong>Compare Models (Data Quality Analysis)</strong>"),
                       value = FALSE),
          helpText(
            HTML("<small>
                 \u2713 Build TWO models to show the value of complete data<br>
                 \u2022 <strong>Base Model:</strong> All rows, excludes Condition/DesignStyle<br>
                 \u2022 <strong>Enhanced Model:</strong> Only rows with complete Condition/DesignStyle data<br>
                 \u2022 Shows improvement from collecting complete data
                 </small>")
          ),

          hr(),

          # Tree Builder Settings Profile management
          div(class = "card mb-3 bg-light",
            div(class = "card-body",
              h6("Tree Builder Settings", class = "card-title"),
              helpText("Save/Load your variable selections and settings"),
              fileInput(ns("load_tree_profile"),
                       "Load Settings",
                       accept = ".csv",
                       buttonLabel = "Browse",
                       placeholder = "No profile selected"),
              downloadButton(ns("save_tree_profile"), "Save Settings", class = "btn-sm btn-success w-100 mt-2")
            )
          ),

          hr(),

          # Subject Property Profile management
          div(class = "card mb-3 bg-info text-white",
            div(class = "card-body",
              h6("Subject Property", class = "card-title"),
              helpText("Save/Load subject property characteristics"),
              fileInput(ns("load_subject_profile"),
                       "Load Subject",
                       accept = ".csv",
                       buttonLabel = "Browse",
                       placeholder = "No subject selected"),
              downloadButton(ns("save_subject_profile"), "Save Subject", class = "btn-sm btn-light w-100 mt-2")
            )
          ),

          hr(),

          actionButton(ns("build_tree"),
                      "Build Decision Tree",
                      class = "btn-primary btn-lg w-100",
                      icon = icon("tree"))
        ),

        # Main content with tabs
        div(
          h2("Build and Describe the Decision Tree"),
          hr(),

          # Tabbed interface for input and results
          navset_card_tab(
            id = ns("build_tree_tabs"),

            # Tab 1: Select Variables
            nav_panel(
              "1. Select Variables",
              div(class = "p-3",
                # Prominent Include/Exclude mode toggle
                div(class = "card mb-4",
                  div(class = "card-body text-center p-4",
                    h5("Variable Selection Mode", class = "card-title mb-3"),
                    radioButtons(ns("selection_mode"),
                                label = NULL,
                                choices = c(
                                  "\u2713 Check to INCLUDE in tree (recommended)" = "include",
                                  "\u2717 Check to EXCLUDE from tree" = "exclude"
                                ),
                                selected = "include",
                                inline = FALSE),
                    helpText(
                      HTML("<strong>Include Mode:</strong> Select only the variables you want to analyze (more intuitive)<br>
                           <strong>Exclude Mode:</strong> Traditional approach - deselect variables you don't want")
                    )
                  )
                ),

                # Helper buttons
                div(class = "mb-3 text-center",
                  actionButton(ns("select_all_vars"), "Select All", class = "btn-sm btn-outline-primary me-2"),
                  actionButton(ns("select_none_vars"), "Select None", class = "btn-sm btn-outline-secondary me-2"),
                  actionButton(ns("invert_selection"), "Invert Selection", class = "btn-sm btn-outline-info")
                ),

                hr(),

                # Multi-column checkbox display
                h5("Available Variables"),
                helpText("Select variables based on the mode chosen above"),
                uiOutput(ns("variable_checkboxes_grid"))
              )
            ),

            # Tab 2: Define Subject
            nav_panel(
              "2. Define Subject",
              div(class = "p-3",
                h4("Subject Property Definition"),
                p("Define your subject property to extract its CMS after building the tree."),

                # Quick pick from existing data
                div(class = "card mb-3",
                  div(class = "card-body bg-light",
                    h6("Quick Pick from Data", class = "card-title"),
                    helpText("Type to search and select a property from your data"),
                    uiOutput(ns("address_picker")),
                    actionButton(ns("use_selected_property"), "Use This Property", class = "btn-sm btn-info mt-2"),
                    # Data quality validation tip
                    div(class = "mt-2 small",
                      style = "background: #e8f5e9; padding: 8px; border-radius: 4px; border-left: 3px solid #4caf50;",
                      icon("check-circle", style = "color: #4caf50;"), " ",
                      tags$strong("Data Quality Check:"), " ",
                      "Pick any property from your data, build the tree, then check the CMS table. ",
                      "That property should appear with Similarity = 0.0 (exact match). ",
                      "If not, open the debug breakdown to identify data issues."
                    )
                  )
                ),

                # Charlie's example subject preset
                div(class = "card mb-3",
                  div(class = "card-body bg-success text-white",
                    h6("Charlie's Example Subject", class = "card-title"),
                    helpText("Load a typical subject property based on Charlie Abromaitis's study"),
                    actionButton(ns("load_charlie_subject"), "Load Example Subject", class = "btn-sm btn-light", icon = icon("user"))
                  )
                ),

                hr(),

                # Display currently loaded subject (if any)
                uiOutput(ns("current_subject_display")),

                # Multi-column subject input fields
                h5("Subject Characteristics"),
                uiOutput(ns("subject_inputs_grid"))
              )
            ),

            # Tab 3: View Tree
            nav_panel(
              "3. View Tree",
              div(class = "p-3",
                # Conditional methodology info
                uiOutput(ns("methodology_info")),

                # Tree output
                uiOutput(ns("tree_results"))
              )
            )
          )
        )
      )
    ),

    # ========================================================================
    # Tab 4: Extract CMS
    # ========================================================================
    nav_panel(
      "4. Extract CMS",
      div(
        div(class = "mb-4",
          layout_columns(
            col_widths = c(7, 5),
            gap = "1.5rem",

            div(
              h2("Identify Subject's Competitive Market Segment"),
              hr(),

              p(class = "lead mb-4",
                "The subject follows the decision rules to find its CMS."
              ),

              div(class = "card",
                div(class = "card-body",
                  h4("Subject Assignment Path", class = "card-title"),
                  uiOutput(ns("subject_path"))
                )
              )
            ),

            div(class = "card h-100",
              div(class = "card-body",
                h4("CMS Summary", class = "card-title"),
                uiOutput(ns("cms_summary"))
              )
            )
          )
        ),

        # Stability Analysis Block
        div(class = "mb-4",
          div(class = "card border-info",
            div(class = "card-header bg-info text-white",
                icon("shield-alt"), " Advanced Analysis: Stability Check"),
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-center",
                  div(
                    h5("Is your CMS robust?", class = "card-title"),
                    p(class = "card-text text-muted",
                      "Decision trees can be unstable. This tool runs the algorithm 20 times on subsampled data to see if the same properties are consistently selected.")
                  ),
                  actionButton(ns("run_stability"), "Run Stability Analysis", class = "btn-info btn-lg", icon = icon("play"))
              ),
              hr(),
              uiOutput(ns("stability_results"))
            )
          )
        ),

        div(class = "mb-4",
          layout_columns(
            col_widths = 12,
            gap = "1.5rem",

            # Full width comparison
            div(class = "card",
              div(class = "card-body",
                h4("Comparison: ADF vs CMS", class = "card-title"),
                sliderInput(ns("cms_bins"), "Histogram Bins:",
                            min = 3, max = 30, value = 10, step = 1,
                            width = "300px"),
                plotlyOutput(ns("adf_cms_comparison"))
              )
            )
          )
        ),

        layout_columns(
          col_widths = 12,
          gap = "1.5rem",

          # CMS data table
          div(class = "card",
            div(class = "card-body",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                h4("CMS Sales Data", class = "card-title", style = "margin: 0;"),
                # Copy MLS# controls with similarity sorting
                div(
                  style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "margin: 0;", checkboxInput(ns("sort_by_similarity"), tags$span(icon("sort-amount-down"), " Sort by similarity"), value = TRUE)),
                  radioButtons(
                    ns("mls_separator"),
                    NULL,
                    choices = c("Space" = " ", "Comma" = ", "),
                    selected = ", ",
                    inline = TRUE
                  ),
                  actionButton(
                    ns("copy_mls_btn"),
                    tags$span(icon("copy"), " Copy MLS#s"),
                    class = "btn-sm btn-outline-secondary"
                  )
                )
              ),
              # Hidden text area for clipboard copy (populated by server)
              # NOTE: The id here is namespaced. Server-side JS references
              # may need to use the full namespaced id (e.g., session$ns("mls_list_text")).
              tags$textarea(
                id = ns("mls_list_text"),
                style = "position: absolute; left: -9999px;",
                readonly = "readonly"
              ),
              # JavaScript for clipboard copy
              # NOTE: This custom message handler is global and does not need namespacing.
              # The server side should use session$sendCustomMessage('copyToClipboard', text).
              tags$script(HTML("
                Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
                  navigator.clipboard.writeText(text).then(function() {
                    Shiny.setInputValue('clipboard_success', Math.random());
                  }).catch(function(err) {
                    console.error('Failed to copy: ', err);
                    Shiny.setInputValue('clipboard_error', Math.random());
                  });
                });
              ")),
              DTOutput(ns("cms_data_table")),
              uiOutput(ns("similarity_debug"))
            )
          )
        )
      )
    ),

    # ========================================================================
    # Tab 5: Results & Download
    # ========================================================================
    nav_panel(
      "5. Results",
      layout_columns(
        col_widths = c(12, 6, 6),

        div(
          h2("Results Summary & Export"),
          hr()
        ),

        # Key metrics
        div(
          div(class = "card",
            div(class = "card-body",
              h4("Key Metrics", class = "card-title"),
              uiOutput(ns("key_metrics"))
            )
          )
        ),

        # USPAP / CUSPAP language
        div(
          div(class = "card",
            div(class = "card-body bg-light",
              h4("USPAP/CUSPAP Report language sample *Appraiser to confirm.", class = "card-title"),
              uiOutput(ns("uspap_language")),
              hr(),
              actionButton(ns("copy_uspap"), "Copy to Clipboard", icon = icon("copy"))
            )
          )
        ),

        # Downloads
        div(
          div(class = "card",
            div(class = "card-body",
              h4("Download Results", class = "card-title"),
              downloadButton(ns("download_cms_excel"), "Download CMS (Excel)", class = "btn-success btn-lg w-100 mb-2"),
              downloadButton(ns("download_tree_plot"), "Download Tree Plot (PNG)", class = "btn-info w-100 mb-2"),
              downloadButton(ns("download_report"), "Download Full Report (HTML)", class = "btn-primary w-100")
            )
          )
        )
      )
    ),

    # ========================================================================
    # About Tab
    # ========================================================================
    nav_panel(
      "About",
      div(class = "container mt-4",
        h2("About This Tool"),
        hr(),

        p(class = "lead",
          "This application implements the Decision Tree methodology developed by",
          strong("Charlie Abromaitis"), "for the CAA community."
        ),

        h4("Methodology Source"),
        p("Based on:", em("From the ADF to the CMS with Decision Trees"), "(2023)"),

        h4("R Package"),
        p("Uses the", code("qeML"), "package by Norm Matloff"),
        p(tags$a(href = "https://github.com/matloff/qeML",
                 "github.com/matloff/qeML",
                 target = "_blank")),

        h4("Credits"),
        tags$ul(
          tags$li("Algorithm: Charlie Abromaitis"),
          tags$li("qeML Package: Norm Matloff"),
          tags$li("Sample Data: Bruce Hahn"),
          tags$li("Community: CAA Groups.io")
        ),

        h4("USPAP/CUSPAP Compliance"),
        p("When properly documented in your appraisal report, this methodology is",
          strong("USPAP/CUSPAP aligned"), "as it provides:"),
        tags$ul(
          tags$li("Objective, transparent process"),
          tags$li("Documented methodology"),
          tags$li("Appraiser review and validation"),
          tags$li("Evidence-Based Valuation alignment")
        ),

        div(class = "alert alert-info", role = "alert",
          icon("info-circle"), strong(" Remember:"),
          " AI is a TOOL, not a replacement for appraiser judgment. You review, validate, and decide."
        )
      )
    )
    ) # Close navset_tab
  )
}
