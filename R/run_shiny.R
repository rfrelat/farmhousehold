# runShiny ---------------------------------------
#' Run shiny app made using cuspra package functions
#'
#' @export
runExplo <- function() {
  appDir <- system.file("app_explo", package = "farmhousehold")
  if (appDir == "") {
    stop("Could not find the directory. Try re-installing `farmhousehold`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

runCluster <- function() {
  appDir <- system.file("app_cluster", package = "farmhousehold")
  if (appDir == "") {
    stop("Could not find the directory. Try re-installing `farmhousehold`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
