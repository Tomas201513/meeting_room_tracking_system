# Meeting Room Booking System - Main Entry Point
# Run this file to launch the application

# Source global configuration (packages, credentials, DB functions)
source("global.R")

# Source UI and Server
source("ui.R")
source("server.R")

# Run the app with authentication wrapper
shinyApp(
  ui = secure_app(ui),
  server = server
)
