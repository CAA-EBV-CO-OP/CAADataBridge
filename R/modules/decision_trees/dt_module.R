# ============================================================================
# Decision Trees Module - Shiny Module Wrapper
# Wraps the CA-ADF Decision Tree Tool as a Shiny module
# ============================================================================

# Required libraries for Decision Trees module
library(qeML)
library(gt)
library(plotly)
library(lubridate)
library(DT)
library(scales)
library(writexl)
library(readxl)
library(data.table)

#' Decision Trees Module UI
#' @param id Module namespace ID
decisionTreesUI <- function(id) {
  ns <- NS(id)
  source(file.path("R", "modules", "decision_trees", "dt_helpers.R"), local = TRUE)
  source(file.path("R", "modules", "decision_trees", "dt_ui.R"), local = TRUE)
  dt_ui_content(ns)
}

#' Decision Trees Module Server
#' @param id Module namespace ID
#' @param mapped_data Reactive expression returning a data frame from DataBridge (or NULL)
#' @return List with cms_data reactive for downstream consumers
decisionTreesServer <- function(id, mapped_data = NULL) {
  moduleServer(id, function(input, output, session) {
    # Source server logic and run it
    source(file.path("R", "modules", "decision_trees", "dt_server.R"), local = TRUE)
    dt_server_logic(input, output, session, mapped_data)
  })
}
