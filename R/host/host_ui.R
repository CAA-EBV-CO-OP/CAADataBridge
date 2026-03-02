# ============================================================================
# Host UI - Top-level navigation with App Chooser landing page
# Modeled after the original CAADataBridge app chooser screen
# ============================================================================

host_ui <- page_navbar(
  id = "main_navbar",
  title = "CAA Collaborative",
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db",
    base_font = font_google("Source Sans Pro")
  ),

  # Dark mode toggle
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),

  # ========================================================================
  # Home / App Chooser (Landing Page)
  # ========================================================================
  nav_panel(
    title = tagList(icon("home"), "Home"),
    value = "home",
    div(
      style = "text-align: center; max-width: 900px; margin: 40px auto; padding: 20px;",

      # User greeting
      tags$h2(style = "margin-bottom: 5px;", "Welcome to CAA Collaborative"),
      tags$p("Select an application to continue", style = "color: #666; margin-bottom: 30px;"),

      # App choice buttons with descriptions
      div(
        style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; margin-top: 10px;",

        # Column Mapper card
        div(
          style = "text-align: center; max-width: 280px;",
          actionButton("home_choose_mapper",
                       tags$span(icon("table-columns"), " Column Mapper"),
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
          actionButton("home_choose_dt",
                       tags$span(icon("sitemap"), " Decision Trees"),
                       class = "btn-info btn-lg",
                       style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold;"),
          tags$p(
            style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
            "Apply ADF/CMS decision logic ",
            tags$strong("including educational and use context")
          )
        ),

        # Report Forms card (placeholder)
        div(
          style = "text-align: center; max-width: 280px;",
          actionButton("home_choose_reports",
                       tags$span(icon("file-alt"), " Report Forms"),
                       class = "btn-success btn-lg",
                       style = "width: 220px; height: 70px; font-size: 18px; font-weight: bold;"),
          tags$p(
            style = "color: #333; font-size: 13px; margin-top: 12px; line-height: 1.4;",
            "Generate ",
            tags$strong("appraisal reports"),
            " with narrative sections"
          )
        )
      ),

      # Visual workflow guide
      tags$hr(style = "margin: 30px 0; border-color: #ccc;"),
      tags$div(
        style = "text-align: left; background: #e8f4f8; border: 1px solid #b8d4e3; border-radius: 8px; padding: 20px;",
        tags$h4(icon("route"), " Workflow Guide", style = "margin-top: 0; color: #2c5282; font-weight: bold;"),

        # Flow chart
        tags$div(
          style = "margin-top: 20px; position: relative;",

          # Question row
          tags$div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
            tags$span(style = "background: #38a169; color: white; border-radius: 50%; width: 28px; height: 28px; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 13px; flex-shrink: 0;", "1"),
            tags$span(style = "color: #1a202c; font-size: 14px; font-weight: 500;", "Is your data already mapped to standard format?")
          ),

          # Vertical connector
          tags$div(style = "margin-left: 14px; border-left: 2px solid #a0aec0; height: 12px;"),

          # NO branch row
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; margin-left: 14px;",
            tags$span(style = "background: #e53e3e; color: white; border-radius: 4px; padding: 2px 8px; font-size: 11px; font-weight: bold;", "No"),
            tags$span(style = "color: #2b6cb0; font-size: 14px;", HTML("&#8594;")),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('home_choose_mapper', Math.random()); return false;",
              style = "background: #2b6cb0; color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; text-decoration: none; cursor: pointer; display: inline-flex; align-items: center; gap: 4px; font-size: 13px;",
              icon("table-columns"), " Column Mapper"
            ),
            tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
            tags$span(style = "color: #2d3748; font-size: 12px;", "Save profile & export")
          ),

          # L-connector
          tags$div(
            style = "display: flex; margin-left: 14px; height: 45px;",
            tags$div(style = "border-left: 2px solid #a0aec0; height: 100%;"),
            tags$div(
              style = "position: relative; margin-left: 255px; height: 100%;",
              tags$div(style = "position: absolute; left: 0; top: 0; border-left: 2px solid #2b6cb0; height: 100%;"),
              tags$div(style = "position: absolute; left: 0; bottom: 0; border-top: 2px solid #2b6cb0; width: 30px;"),
              tags$span(style = "position: absolute; left: 28px; bottom: -7px; color: #2b6cb0; font-size: 12px;", HTML("&#9654;"))
            )
          ),

          # YES branch row
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; margin-left: 14px;",
            tags$span(style = "background: #38a169; color: white; border-radius: 4px; padding: 2px 8px; font-size: 11px; font-weight: bold;", "Yes"),
            tags$div(style = "width: 230px; height: 2px; background: #38a169;"),
            tags$span(style = "color: #38a169; font-size: 12px; margin-left: -4px;", HTML("&#9654;")),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('home_choose_dt', Math.random()); return false;",
              style = "background: #319795; color: white; padding: 4px 12px; border-radius: 4px; font-weight: bold; text-decoration: none; cursor: pointer; display: inline-flex; align-items: center; gap: 4px; font-size: 13px;",
              icon("sitemap"), " Decision Trees"
            ),
            tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
            tags$span(style = "color: #2d3748; font-size: 12px;", "ADF/CMS logic"),
            tags$span(style = "color: #4a5568; font-size: 14px;", HTML("&#8594;")),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('home_choose_reports', Math.random()); return false;",
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
      )
    )
  ),

  # ========================================================================
  # Column Mapper Tab
  # ========================================================================
  nav_panel(
    title = tagList(icon("table-columns"), "Column Mapper"),
    value = "mapper_tab",
    databridgeUI("mapper")
  ),

  # ========================================================================
  # Decision Trees Tab
  # ========================================================================
  nav_panel(
    title = tagList(icon("sitemap"), "Decision Trees"),
    value = "dt_tab",
    decisionTreesUI("dt")
  ),

  # ========================================================================
  # About Tab
  # ========================================================================
  nav_panel(
    "About",
    value = "about_tab",
    div(class = "container mt-4",
      h2("About CAA Collaborative"),
      hr(),

      p(class = "lead",
        "An integrated platform for real estate appraisal data analysis,",
        "combining Column Mapping and Decision Tree tools."
      ),

      h4("Credits"),
      tags$ul(
        tags$li("Decision Tree Algorithm: Charlie Abromaitis"),
        tags$li("qeML Package: Norm Matloff"),
        tags$li("Sample Data: Bruce Hahn"),
        tags$li("Community: CAA Groups.io")
      ),

      div(class = "alert alert-info", role = "alert",
        icon("info-circle"), strong(" Remember:"),
        " AI is a TOOL, not a replacement for appraiser judgment. You review, validate, and decide."
      )
    )
  )
)
