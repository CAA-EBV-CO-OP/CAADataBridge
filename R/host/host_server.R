# ============================================================================
# Host Server - Orchestration, navigation, and data routing
# ============================================================================

host_server <- function(input, output, session) {

  # Store root session so modules can navigate the host navbar
  session$userData$root_session <- session

  # ==========================================================================
  # App Chooser Navigation - switch tabs when user clicks an app card
  # ==========================================================================

  observeEvent(input$home_choose_mapper, {
    nav_select("main_navbar", selected = "mapper_tab")
  })

  observeEvent(input$home_choose_dt, {
    nav_select("main_navbar", selected = "dt_tab")
  })

  observeEvent(input$home_choose_reports, {
    showModal(modalDialog(
      title = tagList(icon("clock"), " Report Forms Coming Soon"),
      tags$p(
        "The Report Forms module is not active yet in this sandbox.",
        style = "margin-bottom: 8px;"
      ),
      tags$p(
        "Use Column Mapper and Decision Trees for now. We'll enable Report Forms in a future update.",
        style = "color: #555; margin-bottom: 0;"
      ),
      easyClose = TRUE,
      footer = modalButton("Got it")
    ))
  })

  # ==========================================================================
  # Launch DataBridge module, get mapped data back
  # ==========================================================================
  bridge_results <- databridgeServer("mapper")

  # ==========================================================================
  # Pass mapped data into Decision Trees module
  # ==========================================================================
  dt_results <- decisionTreesServer("dt", bridge_results$mapped_data)

  # ==========================================================================
  # Future: pass DT results to Reports module
  # report_results <- reportsServer("reports", dt_results$cms_data)
  # ==========================================================================
}
