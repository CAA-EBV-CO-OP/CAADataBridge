# ============================================================================
# DataBridge Module - Shiny Module Wrapper
# Wraps the Column Mapper as a Shiny module
# ============================================================================

#' DataBridge Module UI
#' @param id Module namespace ID
databridgeUI <- function(id) {
  ns <- NS(id)
  source(file.path("R", "modules", "databridge", "databridge_global.R"), local = TRUE)
  source(file.path("R", "modules", "databridge", "databridge_ui.R"), local = TRUE)
  databridge_ui_content(ns)
}

#' DataBridge Module Server
#' @param id Module namespace ID
#' @return List with mapped_data and field_map reactives
databridgeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Source global helpers into module scope
    source(file.path("R", "modules", "databridge", "databridge_global.R"), local = TRUE)
    # Source and run server logic
    source(file.path("R", "modules", "databridge", "databridge_server.R"), local = TRUE)
    databridge_server_logic(input, output, session)
  })
}
