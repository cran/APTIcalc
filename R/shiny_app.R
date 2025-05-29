#' Run APTI 'Shiny' App
#'
#' Launches a 'Shiny' app for interactive APTI calculation and visualization.
#'
#' @return Launches a Shiny application.
#' @examples
#' if (interactive()) {
#'   run_apti_app()
#' }
#' @export
run_apti_app <- function() {
  app_dir <- system.file("shiny", package = "APTIcalc")
  if (app_dir == "") {
    stop("Could not find 'Shiny' app directory. Try re-installing `APTIcalc`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
