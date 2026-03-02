# ============================================================================
# CAA Collaborative - Host Application Entrypoint
# Combines CAADataBridge (Column Mapper) and Decision Trees into one app
# ============================================================================

# Core Shiny framework
library(shiny)
library(bslib)
library(tidyverse)
library(shinyjs)

# Source shared utilities
for (f in list.files("R/shared", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# Source module definitions
source("R/modules/databridge/databridge_module.R")
source("R/modules/decision_trees/dt_module.R")

# Source host app
source("R/host/host_ui.R")
source("R/host/host_server.R")

# Launch
shinyApp(ui = host_ui, server = host_server)
