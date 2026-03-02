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
    showNotification("Report Forms coming soon!", type = "message", duration = 5)
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
