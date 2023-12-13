# runShiny ---------------------------------------
#' Interactive exploration of households diversity
#'
#' Locally run shiny app to explore the diversity
#' among the farm households
#'
#' @export
runExplo <- function() {
  appDir <- system.file("app_explo", package = "farmhousehold")
  if (appDir == "") {
    stop("Could not find the directory. Try re-installing `farmhousehold`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


#' Interactive clustering of households
#'
#' Locally run shiny app to make clusters
#' among the farm households
#'
#' @export
runCluster <- function() {
  appDir <- system.file("app_cluster", package = "farmhousehold")
  if (appDir == "") {
    stop("Could not find the directory. Try re-installing `farmhousehold`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
