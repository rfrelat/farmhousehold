.pkgenv <- new.env(parent=emptyenv())

tmap_hh <- function(x, interplot=FALSE, j=0.05){

  # remove NA
  x <- x[complete.cases(x[,c("gps_lat", "gps_lon")]),]

  leg <- paste0(
    "Country: ", x$country, "/n",
    "Project: ", x$project, "<br>",
    "Köppen: ", x$koeppen, "\n",
    "FS: ", x$farming_system
  )
  xy <- data.frame(
    "leg"=x$country,
    "x"=jitter(x$gps_lon, amount = j),
    "y"=jitter(x$gps_lat, amount = j))
  if (nrow(x)>0){
    sfx <- st_as_sf(xy, coords = c("x", "y"), crs=4326)
    x_bb <- st_bbox(sfx)

    if (interplot){
      tmap_mode("view")
      fig <- tm_basemap("OpenStreetMap.Mapnik")
    } else {
      data("World", package = "tmap", envir = .pkgenv)
      fig <- tm_shape(.pkgenv$World, bbox = x_bb)+
        tm_polygons()
    }
    fig <- fig +  tm_shape(sfx)+
      tm_dots(group = "Households")
    return(fig)
  }
}


tmap_sub <- function(x0,x, col="red", interplot=FALSE){

  xy <- data.frame(
    "col"=ifelse(x0$hhid%in%x$hhid, col, "black"),
    "x"=x0$gps_lon,
    "y"=x0$gps_lat)

  #remove NA and duplicated coordinates
  xy <- xy[complete.cases(xy) & !duplicated(xy),]

  if (nrow(x)>0){
    sfx <- st_as_sf(xy, coords = c("x", "y"), crs=4326)
    x_bb <- st_bbox(sfx)

    if (interplot){
      tmap_mode("view")
      fig <- tm_basemap("OpenStreetMap.Mapnik")
    } else {
      data("World", package = "tmap", envir = .pkgenv)
      fig <- tm_shape(.pkgenv$World, bbox = x_bb)+
        tm_polygons()
    }
    fig <- fig +  tm_shape(sfx)+
      tm_dots(col="col")
    return(fig)
  }
}

tmap_ind <- function(x, var, interplot=FALSE, j=0.05){
  # remove NA
  x <- x[complete.cases(x[,c("gps_lat", "gps_lon", var)]),]

  leg <- paste0(
    paste0(var, ": "), x[,var], "/n",
    "Country: ", x$country, "/n",
    "Project: ", x$project, "<br>",
    "Köppen: ", x$koeppen, "\n",
    "FS: ", x$farming_system
  )
  xy <- data.frame(
    var=x[,var],
    "x"=jitter(x$gps_lon, amount = j),
    "y"=jitter(x$gps_lat, amount = j))

  if (nrow(x)>0){
    sfx <- st_as_sf(xy, coords = c("x", "y"), crs=4326)
    x_bb <- st_bbox(sfx)

    if (interplot){
      tmap_mode("view")
      fig <- tm_basemap("OpenStreetMap.Mapnik")
    } else {
      data("World", package = "tmap", envir = .pkgenv)
      fig <- tm_shape(.pkgenv$World, bbox = x_bb)+
        tm_polygons()
    }
    fig <- fig +  tm_shape(sfx)+
      tm_dots(col = "var")
    return(fig)
  }
}

lymap_ind <- function(x, var, zoom=NULL, j=0.05){
  # remove NA
  x <- x[complete.cases(x[,c(var, "gps_lat", "gps_lon")]),]

  # find the zoom for the map
  if(is.null(zoom)){
    zoom <- findzoom(x$gps_lon, x$gps_lat)
  }
  leg <- paste0(
    paste0(var, ": "), x[,var], "<br>",
    "Country: ", x$country, "<br>",
    "Project: ", x$project, "<br>",
    "Koppen: ", x$koeppen, "\n",
    "FS: ", x$farming_system
  )

  if (nrow(x)>0){
    defaultW <- getOption("warn")
    options(warn = -1)
    fig <- plot_ly(x,
                   lon = jitter(x$gps_lon, amount = j),
                   lat = jitter(x$gps_lat, amount = j),
                   color = x[,var],
                   hoverinfo = "text",
                   type = 'scattermapbox',
                   hovertext = leg,
                   mode="markers")
    fig <- fig %>%  layout(
      mapbox = list(style = 'open-street-map',
                    zoom = zoom,
                    center = list(lon = mean(range(x$gps_lon, na.rm=TRUE)),
                                  lat = mean(range(x$gps_lat, na.rm=TRUE)))),
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude")
    ) %>% config(
      modeBarButtons = list(list("toImage")),
      scrollZoom = TRUE,
      displaylogo = FALSE
    ) %>% add_annotations(
      text = "Locations are approximated to the nearest 0.01 degree <br> and jittered to safeguard the privacy of households.",
      x = 0,
      y = 0,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
    options(warn = defaultW)
    return(fig)
  }
}

tmap_diff <- function(x1, x2, sel=NULL, interplot=TRUE, j=0.05){
  # remove NA
  keep <- complete.cases(x1[,c(sel, "gps_lat", "gps_lon")])
  x1 <- x1[keep,]
  x2 <- x2[keep,]

  leg <- paste0(
    "Original: ", x1[,sel], "---",
    "Scenario: ", x2[,sel]
  )
  xy <- data.frame(
    "leg"=leg,
    "d"=x2[,sel]-x1[,sel],
    "perc"=ifelse(x1[,sel]>0, (x2[,sel]-x1[,sel])/x1[,sel], 0)*100,
    "absd"=abs(x2[,sel]-x1[,sel]),
    "x"=jitter(x1$gps_lon, amount = j),
    "y"=jitter(x1$gps_lat, amount = j))

  if (nrow(x1)>0){
    sfx <- st_as_sf(xy, coords = c("x", "y"),
                    crs=st_crs(4326))
    x_bb <- st_bbox(sfx)

    if (interplot){
      tmap_mode("view")
      fig <- tm_basemap("OpenStreetMap.Mapnik")
    } else {
      data("World", package = "tmap", envir = .pkgenv)
      fig <- tm_shape(.pkgenv$World, bbox = x_bb)+
        tm_polygons()
    }
    lim <- c(0.1, quantile(xy$absd, probs=0.95))
    fig <- fig +  tm_shape(sfx)+
      tm_dots(col= "perc", midpoint =0,
              #size="absd",size.lim=lim,
              group=NULL, #"households",
              popup.vars=c("d", "perc"))
    return(fig)
  }
}


tmap_diff3 <- function(x0, x1, x2, x3, sel=NULL, interplot=TRUE, j=0.05){
  # remove NA
  keep <- complete.cases(x1[,c(sel, "gps_lat", "gps_lon")])
  x0 <- x0[keep,]
  x1 <- x1[keep,]
  x2 <- x2[keep,]
  x3 <- x3[keep,]

  leg <- paste0(
    "Original: ", round(x0[,sel]), "---",
    "CA: ", round(x1[,sel]), "---",
    "CA+legumes: ", round(x2[,sel]), "---",
    "Mbili Mbili: ", round(x3[,sel])
  )
  xy <- data.frame(
    "leg"=leg,
    "d1"=x1[,sel]-x0[,sel],
    "d2"=x2[,sel]-x0[,sel],
    "d2"=x2[,sel]-x0[,sel],
    "CA"=ifelse(x0[,sel]>0, (x1[,sel]-x0[,sel])/x0[,sel], 0)*100,
    "CAleg"=ifelse(x0[,sel]>0, (x2[,sel]-x0[,sel])/x0[,sel], 0)*100,
    "Mbili"=ifelse(x0[,sel]>0, (x3[,sel]-x0[,sel])/x0[,sel], 0)*100,
    "x"=jitter(x1$gps_lon, amount = j),
    "y"=jitter(x1$gps_lat, amount = j))

  if (nrow(x1)>0){
    sfx <- st_as_sf(xy, coords = c("x", "y"),
                    crs=st_crs(4326))
    x_bb <- st_bbox(sfx)

    tmap_mode("view")
    fig <- tm_basemap("OpenStreetMap.Mapnik")

    facets = c("CA", "CAleg", "Mbili")
    fig <- fig +  tm_shape(sfx)+
      tm_dots(col= facets, midpoint =0,
              group=NULL)
    fig <- fig + tm_facets(nrow = 1, sync = TRUE)
    return(fig)
  }
}

tmap_adm <- function(x0, x1, adm, sel=NULL, interplot=TRUE,
                     j=0.05, labadm="epa", info=c("condition", "impact")){

  info <- match.arg(info)

  #adm <- as(adm, "Spatial")
  adm <- sf::st_as_sf(adm)
  # remove NA
  keep <- complete.cases(x0[,c(sel,labadm)])#, "gps_lat", "gps_lon")
  x0 <- x0[keep,]
  x1 <- x1[keep,]

  dsel <- x1[,sel]-x0[,sel]
  psel <- ifelse(x0[,sel]>0, (x1[,sel]-x0[,sel])/x0[,sel], 0)*100
  #statistics per adm
  if (info=="condition"){
    var <- tapply(dsel!=0, x0[,labadm], sum) / table(x0[,labadm])*100
    leg <- paste(var, "% household reached")
  } else {
    var <- tapply(psel[psel!=0], x0[psel!=0,labadm], median)
    leg <- paste(sel, round(var,2), "median impact (in %)")
  }

  mepa <- match(adm$ADMIN3, names(var))
  adm$var <- as.numeric(var)[mepa]
  adm[,1] <- paste(adm$ADMIN3, ":", leg[mepa])

  x_bb <- st_bbox(adm)
  tit <- ifelse(info=="condition", "% household reached", "median impact (%)")
  if (interplot){
    tmap_mode("view")
    fig <- tm_basemap("OpenStreetMap.Mapnik")
  } else {
    data("World", package = "tmap", envir = .pkgenv)
    fig <- tm_shape(.pkgenv$World, bbox = x_bb)+
      tm_polygons()
  }
  fig <- fig +  tm_shape(adm)+
    tm_polygons("var", showNA=TRUE, title=tit)
  return(fig)
}



findzoom <- function(x, y){
  cutzoom <- c(0, 1.2, 2.6, 5, 12,  26, 70, 360)
  labzoom <- c(7, 6, 5, 4, 3, 2, 1)
  maxr <- max(c(diff(range(x, na.rm = TRUE)), diff(range(y, na.rm = TRUE))))
  z <- as.character(cut(maxr, breaks = cutzoom, labels = labzoom))
  return(as.numeric(z))
}
