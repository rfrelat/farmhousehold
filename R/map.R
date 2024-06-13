.pkgenv <- new.env(parent=emptyenv())
#' Create a tmap of households gps location
#'
#' This function creates a tmap and highlight one selected variable
#'
#' @param x farmhousehold object or dataframe with, at least, gps_lat, gps_lon and the variable of interest
#' @param var the name of the variable to be shown
#' @param j parameter to jitter the coordinates
#' @param logS whether the highly skewed variable are log(x+1) transformed
#' @param interplot decide if the plot is interactive or not
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot the source of income
#' tmap_ind(hhdb_rhomis, "large_region")
#'
tmap_ind <- function(x, var, j=0.05, logS=TRUE, interplot=TRUE){
  if(inherits(x, "farmhousehold")){
    x <- x$hhinfo
  }
  # remove NA
  x <- x[complete.cases(x[,c("gps_lat", "gps_lon", var)]),]

  leg <- var

  xy <- data.frame(
    "z"=x[,var],
    "x"=jitter(x$gps_lon, amount = j),
    "y"=jitter(x$gps_lat, amount = j))


  if(is.numeric(x[,var]) & logS){
    sk <- moments::skewness(x[,var], na.rm=TRUE)
    if (abs(sk)>3){
      xy$z <- round(log(xy$z+1), 3)
      leg <- paste0("log(", var, ")")
    }
  }

  if (nrow(x)>0){
    sfx <- sf::st_as_sf(xy, coords = c("x", "y"), crs=4326)
    x_bb <- sf::st_bbox(sfx)

    if (interplot){
      tmap::tmap_mode("view")
      fig <- tmap::tm_basemap("OpenStreetMap.Mapnik")
    } else {
      data("World", package = "tmap", envir = .pkgenv)
      fig <- tmap::tm_shape(.pkgenv$World, bbox = x_bb)+
        tmap::tm_polygons()
    }
    fig <- fig +  tmap::tm_shape(sfx)+
      tmap::tm_dots(col = "z", title=leg)

    if(!is.numeric(x[,var])){
      fig <- fig + tmap::tmap_options(max.categories = lunique(x[,var]))
    }

    return(fig)
  }
}
