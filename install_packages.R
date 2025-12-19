# Install required packages for Meeting Room Booking System

required_packages <- c(
  "shiny",
  "bslib",
  "toastui",
  "DT",
  "RSQLite",
  "shinyWidgets",
  "shinymanager"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg)
  } else {
    message(paste(pkg, "is already installed"))
  }
}

invisible(lapply(required_packages, install_if_missing))

message("\n========================================")
message("All packages installed!")
message("========================================")
message("\nTo run the app: shiny::runApp()")
message("\nLogin credentials:")
message("  Admin: admin / admin")
message("  User:  user / user")
message("========================================")
