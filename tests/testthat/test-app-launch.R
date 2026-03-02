# Smoke test: verify the CAADataBridge Shiny app launches without errors
library(shinytest2)

test_that("CAADataBridge app launches successfully", {
  app <- AppDriver$new(
    app_dir = ".",
    name = "app-launch-smoke-test",
    timeout = 30000
  )

  # Verify the app is running and responds
  app$expect_values()

  app$stop()
})
