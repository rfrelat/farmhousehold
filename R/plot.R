#' Show the crop or livestock adoption
#'
#' This function make a barplot showing the most commun
#' crop or livestock in the dataset
#' @param tab either crop or lvst table with columns `hhid` and `name`
#' @param idlist full list of households id
#' @param th threshold to show all items with more than th% (default=10)
#' @param seg segmentation of households (if any) NEED CHECKS
#' @param interplot wether interactive plot with plotly or not
#' @param colcat colors of the items
#' @param maradd extra margin for plotting the crop names (when not interactive plot)
#' @keywords barplot, adoption
#' @export
#' @examples
#' data(rhomis)
#' bar_div(crop)
#'
bar_div <- function(tab, idlist=NULL, th=10, seg=NULL,
                     interplot=FALSE, colcat="#1F77B4",
                     marx=4, ...){

  # check id
  if (is.null(idlist)){
    idlist <- sort(unique(tab$hhid))
  }
  if(any(duplicated(idlist))){
    warning("Duplicated id were found and removed from idlist")
    idlist <- idlist[!duplicated(idlist)]
  }
  if(any(!tab$hhid%in%idlist)){
    warning("Missing id in idlist, that were removed from tab")
    tab <- tab[tab$hhid%in%idlist]
  }

  if(!is.null(seg) & length(seg)!= length(idlist)){
    stop("Segmentation list has different length than the idlist")
  }

  # create table
  hhname <- table(tab$name, tab$hhid)
  if (is.null(seg)){ #no segmentation
    nhh_name <- rowSums(hhname>0, na.rm = TRUE)
    df <- data.frame("name"=names(nhh_name),
                     "nhh"=as.numeric(nhh_name),
                     "perc"=as.numeric(nhh_name)/length(idlist)*100,
                     "col"=colcat)
    no <- sum(!idlist%in%tab$hhid)
    none <- data.frame("name"="none",
                         "nhh"=as.numeric(no),
                         "perc"=as.numeric(no)/length(idlist)*100,
                         "col"="black")
    df <- rbind(df, none)
    # keep only the one above the threshold
    df <- df[df$perc>th,]
    df <- df[order(df$perc),]
    if (!interplot){
      par(mar=par()$mar+c(0,marx,0,0))
      barplot(df$perc, horiz=TRUE, xlab="%",
              names=df$name, las=1, col=df$col)
      par(mar=par()$mar-c(0,marx,0,0))
      invisible(df)
    } else {
      df$leg <- paste0(df$name, "<br>N=", df$nhh, "(", round(df$perc), "%)")
      fig <- plot_ly(df, y = ~name, x = ~perc,
                     type="bar",
                     marker = list(color = ~col),
                     orientation = 'h',
                     hoverinfo = 'text',
                     hovertext = ~leg
      ) %>% layout(
        title = "",
        xaxis = list(title = "% of households"),
        yaxis = list(title = "",
                     categoryorder = "total ascending"),
        hovermode = 'y'
      )
      fig <- fig %>%
        config(modeBarButtons = list(list("toImage")),
               displaylogo = FALSE)
      return(fig)
    }
  } else { #with segmentation
    seg <- as.factor(seg)
    seglstk <- seg[match(colnames(hhname), idlist)]
    df <- rowsum(apply(hhname>0,1,as.numeric),seglstk, na.rm=TRUE)
    nolstk <- table(seg[!idlist%in%colnames(hhname)])
    # df <- cbind(df, "none"=nolstk)

    dfp <- df/as.numeric(table(seg))*100
    # set threshold to keep livestock
    keeplstk <- colSums(dfp>th)>1
    df <- df[,keeplstk]
    df <- df[,order(colSums(df), decreasing = TRUE)]
    dfp <- data.frame(df/as.numeric(table(seg)))
    nolstkp <- nolstk/as.numeric(table(seg))
    if (!interplot){
      par(mar=par()$mar+c(marx,0,0,5))
      barx <- barplot(t(dfp),
                      ylim=c(-max(nolstkp), max(rowSums(dfp))),
                      col=pals::brewer.set1(ncol(dfp)), las=2)
      barplot( -nolstkp, col="black", add=TRUE, las=2)
      legend(max(barx)+0.9, max(rowSums(dfp))*0.8,
             legend = c(rev(names(dfp)), "None"),
              fill = c(rev(pals::brewer.set1(ncol(dfp))), "black"), xpd=NA)
      par(mar=par()$mar-c(marx, 0,0,5))
      invisible(dfp)
    } else {
      fig <- plot_ly(dfp,
                     type="bar", name="Production")
      for (i in 1:ncol(dfp)){
        fig <- fig %>%
          add_bars(x=1:nrow(dfp), y= dfp[,i],
                   name = names(dfp)[i],
                   hoverinfo = 'text',
                   hovertext = paste0(names(dfp)[i], "<br>N=", df[,i], "(", round(dfp[,i]*100), "%)")
          )
      }
      fig <- fig %>% add_trace(x=1:nrow(dfp),y = -nolstkp,
                               name = 'None',
                               marker = list(color = 'black'),
                               hoverinfo = 'text',
                               hovertext = paste("None<br>N=", nolstk, "(", round(nolstkp*100), "%)"))

      fig <- fig %>% layout(barmode = 'relative',
                            hovermode = 'x unified',
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=1:nrow(dfp),
                                        ticktext=row.names(dfp),
                                        title = "Segmentation"))
      fig <- fig %>%
        config(modeBarButtons = list(list("toImage")),
               displaylogo = FALSE)
      return(fig)
    }
  }
}


# crop production
bar_crop <- function(tab, idlist=NULL, th=10,
                    interplot=FALSE, colcat="#1F77B4", top = 10,
                    marx=4, unit=c("n", "kg", "yield", "ha"), ...){

  unit <- match.arg(unit)

  # check id
  if (is.null(idlist)){
    idlist <- unique(tab$hhid)
  }
  if(any(duplicated(idlist))){
    warning("Duplicated id were found and removed from idlist")
    idlist <- idlist[!duplicated(idlist)]
  }
  if(any(!tab$hhid%in%idlist)){
    warning("Missing id in idlist, that were removed from tab")
    tab <- tab[tab$hhid%in%idlist,]
  }

  # create table
  n <- tapply(tab$hhid, tab$name, lunique)
  ha <- tapply(tab$land_area_ha, tab$name, sum, na.rm=TRUE)
  kg <- tapply(tab$harvest_kg, tab$name, sum, na.rm=TRUE)
  yi <- ifelse(NAto0(tab$land_area_ha)==0, NA,
                  tab$harvest_kg/tab$land_area_ha)
  yi[NAto0(yi)==0] <- NA
  yield <- tapply(yi, tab$name, median, na.rm=TRUE)

  nha <-   rowSums(tapply(tab$land_area_ha, list(tab$name, tab$hhid), sum, na.rm=TRUE)>0, na.rm = TRUE)
  nkg <-   rowSums(tapply(tab$harvest_kg, list(tab$name, tab$hhid), sum, na.rm=TRUE)>0, na.rm = TRUE)
  nyield <-   rowSums(tapply(yi, list(tab$name, tab$hhid), sum, na.rm=TRUE)>0, na.rm = TRUE)
  df <- data.frame("name"=names(n),
                   "n"=as.numeric(n),
                   "phh"=as.numeric(n)/length(idlist)*100,
                   "kg"=as.numeric(kg),
                   "pkg"=as.numeric(nkg)/length(idlist)*100,
                   "ha"=as.numeric(ha),
                   "pha"=as.numeric(nha)/length(idlist)*100,
                   "yield"=as.numeric(yield),
                   "pyi"=as.numeric(nyield)/length(idlist)*100,
                   "col"=colcat)
  no <- sum(!idlist%in%tab$hhid)
  none <- data.frame("name"="none",
                     "n"=as.numeric(no),
                     "phh"=as.numeric(no)/length(idlist)*100,
                     "kg"=NA,
                     "pkg"=NA,
                     "ha"=NA,
                     "pha"=NA,
                     "yield"=NA,
                     "pyi"=NA,
                     "col"="black")
  df <- rbind(df, none)

  # keep only the one above the threshold
  top <- ifelse(top>nrow(df), nrow(df), top)
  df <- df[order(df[,unit], decreasing = TRUE)[1:top],]

  if (!interplot){
    par(mar=par()$mar+c(0,marx,0,0))
    barplot(df[,unit], horiz=TRUE, xlab=unit,
            names=df$name, las=1, col=df$col)
    par(mar=par()$mar-c(0,marx,0,0))
    invisible(df)
  } else {
    df$unit <- df[,unit]
    df$leg <- paste0(df$name, "<br>N=", df$n, " (", round(df$phh), "%hh)",
                     "<br>kg=", round(df$kg), " (", round(df$pkg), "%hh)",
                     "<br>ha=", round(df$ha)," (", round(df$pha), "%hh)",
                     "<br>kg/ha=", round(df$yield), "(", round(df$pyi), "%hh)")
    fig <- plot_ly(df, y = ~name, x = ~unit,
                   type="bar",
                   marker = list(color = ~col),
                   orientation = 'h',
                   hoverinfo = 'text',
                   hovertext = ~leg
    ) %>% layout(
      title = "",
      xaxis = list(title = unit),
      yaxis = list(title = "",
                   categoryorder = "total ascending"),
      hovermode = 'y'
    )
    fig <- fig %>%
      config(modeBarButtons = list(list("toImage")),
             displaylogo = FALSE)
    return(fig)
  }


}


bar_hist <- function(x, breaks = c(0, 0.5, 1, 2, 5, 10),
                     lab = NULL, xunit="ha",  pal=NULL,
                     interplot=FALSE, seg=NULL){
  if(!is.null(seg) & length(seg)!= length(x)){
    stop("Segmentation list has different length than the variable")
  }

  bks <- sort(unique(c(-1, breaks, max(x, na.rm=TRUE)*2)))
  if(is.null(lab) | length(lab)!= length(bks)-1){
    lab <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep="-")
    lab <- c(paste0("<=",breaks[1]), lab, paste0(">",breaks[length(breaks)]))
  }

  if(is.null(pal) | length(pal)!= length(lab)){
    pal <- pals::brewer.blues(length(lab))
  }

  xcat <- cut(x, breaks = bks, labels = lab)

  if(is.null(seg)){
    df <- data.frame(table(xcat))
    df$perc <- df$Freq /sum(df$Freq)*100
    df$col <- pal
    if (!interplot){
      #par(mar=par()$mar+c(0,marx,0,0))
      barplot(df$perc, ylab="% households", xlab=xunit,
              names=df$xcat, las=1, col=df$col)
      #par(mar=par()$mar-c(0,marx,0,0))
      invisible(df)
    } else {
      df$leg <- paste0(df$xcat, xunit, "<br>N=", df$Freq, "(", round(df$perc), "%)")
      fig <- plotly::plot_ly(df, x = ~xcat, y = ~perc,
                     type="bar",
                     marker = list(color = ~col),
                     orientation = 'v',
                     hoverinfo = 'text',
                     hovertext = ~leg
      ) %>% plotly::layout(
        title = "",
        yaxis = list(title = "% of households"),
        xaxis = list(title = xunit),
        hovermode = 'x'
      ) %>% plotly::config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE)
      return(fig)
    }
  } else {
    df <- table(seg, xcat)
    #compute percentage
    perc <- df/rowSums(df)*100
    # transform in data.frame
    perc <- matrix(perc, nrow = nrow(perc), ncol = ncol(perc))
    perc <- as.data.frame(perc)
    dimnames(perc) <- dimnames(df)

    if (!interplot){
      par(mar=par()$mar+c(0,0,0,5))
      barx <- barplot(t(perc), col=pal, las=2, ylab="%")
      legend(max(barx)+0.9, max(rowSums(perc))*0.8,
             legend = rev(names(perc)),
             fill = rev(pal), xpd=NA)
      par(mar=par()$mar-c(0, 0,0,5))
      invisible(perc)
    } else {
      fig <- plot_ly(perc,
                     type="bar", name="Production")
      for (i in 1:ncol(perc)){
        fig <- fig %>%
          add_bars(x=1:nrow(perc),y= perc[,i],
                   marker = list(color = pal[i],
                                 line = list(color = 'black',
                                             width = 1)),
                   name = names(perc)[i],
                   hoverinfo = 'text',
                   hovertext = paste(names(perc)[i], "ha: ", round(perc[,i]), "%"))
      }
      fig <- fig %>% add_trace(x=1:nrow(perc),y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste(row.names(perc),":", rowSums(df), "hh, (", round(rowSums(df)/sum(df)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            yaxis= list(title = "% of households"),
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=1:nrow(perc),
                                        ticktext=row.names(perc),
                                        title = "Segmentation"))
      fig <- fig %>%
        config(modeBarButtons = list(list("toImage")),
               displaylogo = FALSE)
      return(fig)
    }
  }
}


bar_box <- function(x, interplot=FALSE, seg=NULL,
                    lab= "Number of months food insecure"){
  if(!is.null(seg) & length(seg)!= length(x)){
    stop("Segmentation list has different length than the variable")
  }

  if(is.null(seg)){
    df <- data.frame(table(x))
    df$perc <- df$Freq /sum(df$Freq)*100
    if (!interplot){
      barplot(df$perc, las=1, xlab= lab,
              ylab="% of households",
              names=df$x, col="#1F77B4")
      invisible(df)
    } else {
      df$col <- "#1F77B4"
      df$leg <- paste0(df$x, "<br>N=", df$Freq, "(", round(df$perc), "%)")
      fig <- plot_ly(df, x = ~x, y = ~perc,
                     type="bar",
                     marker = list(color = ~col),
                     orientation = 'v',
                     hoverinfo = 'text',
                     hovertext = ~leg
      ) %>% layout(
        title = "",
        yaxis = list(title = "% of households"),
        xaxis = list(title = lab),
        hovermode = 'x'
      ) %>% config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE)
      return(fig)
    }
  } else {
    if (!interplot){
      boxplot(x~seg, col="#1F77B4",
              ylab= lab,
              xlab= "Segmentation")
      invisible(tapply(x, seg, summary))
    } else {
      fig <- plot_ly(x= seg, y = ~x,
                     type = "box"
      ) %>% layout(
        title = "",
        yaxis = list(title = lab),
        xaxis = list(title = ""),
        hovermode = 'x'
      ) %>% config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE)
      return(fig)
    }
  }
}

bar_hdds <- function(x, pal= NULL, mar4=6,
                    interplot=FALSE, seg=NULL){
  #get the column starting with G#num
  col <- grep("^(G[0-9][0-9]*_)", names(x), ignore.case = TRUE)
  if(is.null(pal) | length(pal)!= length(col)){
    pal <- pals::cols25(length(col))
    # pals::brewer.set1(length(col)
    #rcartocolor::carto_pal(10, "Safe")
  }

  if(!is.null(seg) & length(seg)!= nrow(x)){
    stop("Segmentation list has different length than diet information")
  }

  hdds <- rowSums(x[,col], na.rm = TRUE)
  #remove households without hdds information
  diet <- x[hdds>0,col]
  #replace NA per 0
  diet[is.na(diet)] <- 0

  if(is.null(seg)){
    sumhdds <- rowSums(diet)
    nf <- table(sumhdds)
    ncat <- rowsum(diet, sumhdds, na.rm = TRUE)
    # meanhd <- apply(ncat/as.numeric(nf),1,sum)
    df <- data.frame(ncat/as.numeric(nf))
    #with width depending on number of households
    df$width <- as.numeric((nf+0.1)/max(nf+0.1))
    df$xtick <-  (cumsum(df$width)-df$width/2) +0.6
    if (!interplot){
      par(mar=par()$mar+c(0,0,0,mar4))
      barx <- barplot(t(df[,1:length(col)]),
                      width = df$width, col=pal,
                      xlab="HDDS")
      legend(max(barx)*1.03, 0.8*max(sumhdds),
             legend = gsub("_", " ", rev(names(df)[1:length(col)])),
             fill = rev(pal), xpd=NA, cex=0.8)
      par(mar=par()$mar-c(0,0,0,mar4))
      invisible(df)
    } else {
      fig <- plot_ly(df,
                     type="bar", name="Production")
      for (i in 1:length(col)){
        fig <- fig %>%
          add_bars(x=~xtick,y= df[,i],
                   marker = list(color = pal[i],
                                 line = list(color = 'black',
                                             width = 1)),
                   name = names(df)[i],
                   width = ~width,
                   hoverinfo = 'text',
                   hovertext = paste(names(df)[i], ":", round(df[,i]*100), "%"))
      }
      fig <- fig %>% add_trace(x=~xtick,y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste("HDDS",row.names(df),":", as.numeric(nf), "hh, (", round(as.numeric(nf)/sum(nf)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=df$xtick,
                                        ticktext=row.names(df),
                                        title = "HDDS"))
      fig <- fig %>% config(modeBarButtons = list(list("toImage")),
                            displaylogo = FALSE)
      return(fig)
    }
  } else {
    seg <- seg[hdds>0]
    nf <- table(seg)
    ncat <- rowsum(diet, seg, na.rm = TRUE)
    df <- data.frame(ncat/as.numeric(nf))
    meanhd <- apply(ncat/as.numeric(nf),1,sum)

    if (!interplot){
      par(mar=par()$mar+c(0,0,0,mar4))
      barx <- barplot(t(df[,1:length(col)]),
                      col=pal,
                      ylab="HDDS")
      legend(max(barx)*1.03, 0.8*max(meanhd),
             legend = gsub("_", " ", rev(names(df)[1:length(col)])),
             fill = rev(pal), xpd=NA, cex=0.8)
      par(mar=par()$mar-c(0,0,0,mar4))
      invisible(df)
    } else {
      fig <- plot_ly(df,
                     type="bar", name="HDDS")
      for (i in 1:length(col)){
        fig <- fig %>%
          add_bars(x=row.names(df),y= df[,i],
                   marker = list(color = pal[i],
                                 line = list(color = 'black',
                                             width = 1)),
                   name = names(df)[i],
                   hoverinfo = 'text',
                   hovertext = paste(names(df)[i], ":", round(df[,i]*100), "%"))
      }
      fig <- fig %>% add_trace(x=row.names(df),y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste("HDDS",row.names(df),":", as.numeric(nf), "hh, (", round(as.numeric(nf)/sum(nf)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            xaxis= list(title = "Segmentation"))
      fig <- fig %>% config(modeBarButtons = list(list("toImage")),
                            displaylogo = FALSE)
      return(fig)
    }
  }
}


bar_factor <- function(x, xlab = NULL, pal=NULL, mar4=6,
                     interplot=FALSE, seg=NULL){
  if(!is.null(seg) & length(seg)!= length(x)){
    stop("Segmentation list has different length than the variable")
  }

  x <- as.factor(x)
  if(is.null(pal) | length(pal)!= nlevels(x)){
    pal <- rev(pals::brewer.rdylgn(nlevels(x)+1))[-2]
  }

  if(is.null(seg)){
    df <- data.frame(table(x))
    df$perc <- df$Freq /sum(df$Freq)*100
    df$col <- pal
    if (!interplot){
      #par(mar=par()$mar+c(0,marx,0,0))
      barplot(df$perc, ylab="% households", xlab=xlab,
              names=df$xcat, las=1, col=df$col)
      #par(mar=par()$mar-c(0,marx,0,0))
      invisible(df)
    } else {
      df$leg <- paste0(df$x, "<br>N=", df$Freq, "(", round(df$perc), "%)")
      fig <- plot_ly(df, x = ~x, y = ~perc,
                     type="bar",
                     marker = list(color = ~col),
                     orientation = 'v',
                     hoverinfo = 'text',
                     hovertext = ~leg
      ) %>% layout(
        title = "",
        yaxis = list(title = "% of households"),
        xaxis = list(title = xlab),
        hovermode = 'x'
      ) %>% config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE)
      return(fig)
    }
  } else {
    df <- table(seg, x)
    #compute percentage
    perc <- df/rowSums(df)*100
    # transform in data.frame
    perc <- matrix(perc, nrow = nrow(perc), ncol = ncol(perc))
    perc <- as.data.frame(perc)
    dimnames(perc) <- dimnames(df)

    if (!interplot){
      par(mar=par()$mar+c(0,0,0,mar4))
      barx <- barplot(t(perc), col=pal, las=2, ylab="%")
      legend(max(barx)+0.9, max(rowSums(perc))*0.8,
             legend = rev(names(perc)),
             fill = rev(pal), xpd=NA)
      par(mar=par()$mar-c(0, 0,0,mar4))
      invisible(perc)
    } else {
      fig <- plot_ly(perc,
                     type="bar", name="Production")
      for (i in 1:ncol(perc)){
        fig <- fig %>%
          add_bars(x=1:nrow(perc),y= perc[,i],
                   marker = list(color = pal[i],
                                 line = list(color = 'black',
                                             width = 1)),
                   name = names(perc)[i],
                   hoverinfo = 'text',
                   hovertext = paste(names(perc)[i], "ha: ", round(perc[,i]), "%"))
      }
      fig <- fig %>% add_trace(x=1:nrow(perc),y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste(row.names(perc),":", rowSums(df), "hh, (", round(rowSums(df)/sum(df)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            yaxis= list(title = "% of households"),
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=1:nrow(perc),
                                        ticktext=row.names(perc),
                                        title = "Segmentation"))
      fig <- fig %>%
        config(modeBarButtons = list(list("toImage")),
               displaylogo = FALSE)
      return(fig)
    }
  }
}

bar_income <- function(x, qmax=0.95, seg=NULL, mar4=6, interplot=FALSE){
  if(!is.null(seg) & length(seg)!= nrow(x)){
    stop("Segmentation list has different length than the variable")
  }

  keep <- NAto0(x$hh_size_members)>0 & NAto0(x$currency_conversion_lcu_to_ppp)>0
  x <- subset(x, keep)
  inccat <- data.frame(
    "cc"=NAto0(x$crop_value_lcu)/(x$hh_size_members*365),
    "cs"=NAto0(x$crop_income_lcu)/(x$hh_size_members*365),
    "lc"=NAto0(x$lstk_value_lcu)/(x$hh_size_members*365),
    "ls"=NAto0(x$lstk_income_lcu)/(x$hh_size_members*365),
    "off"=NAto0(x$off_farm_lcu)/(x$hh_size_mae*365)
  )
  # conversion lcu to usd ppp
  inccat <- inccat/x$currency_conversion_lcu_to_ppp
  inccat[is.na(inccat)] <- 0

  legP <- c("Crop consumed", "Crop sold",
            "Livestock consumed", "Livestock sold",
            "Off farm")
  pal <- c("green", "orange","red", "purple", "black")

  # compute percentages
  tot <- rowSums(inccat, na.rm = TRUE)
  pinccat <- inccat/tot
  pinccat[is.na(pinccat)] <- 0

  if(is.null(seg)){
    leg <- paste0(x$hh_size_members, "persons <br>",
                  round(x$land_cultivated_ha,1), "ha <br>",
                  round(x$livestock_tlu,1), "TLU <br>",
                  round(tot,2), "USD/person/day")

    o1 <- order(tot)
    leg <- leg[o1]
    inccat <- inccat[o1,]
    pinccat <- pinccat[o1,]
    #remove the extreme ones
    ry <- c(0, quantile(tot, probs = qmax))

    if (!interplot){
      barplot(t(inccat), las=1, xlab= "households",
              ylab="USD/person/day", ylim=ry,
              border=NA, names=rep("", nrow(inccat)),
              col=pal)
      legend("topleft", legend = rev(legP), fill=rev(pal))
      invisible(inccat)
    } else {
      fig <- plot_ly(inccat, y = ~cc, type = 'bar',
                     name = 'Crop consumed',
                     hoverinfo = 'text',
                     hovertext = paste(round(pinccat$cc*100), "%"),
                     hoverlabel =list(bgcolor="white"),
                     marker = list(color = pal[1]))
      fig <- fig %>%
        add_trace(y = ~cs, name = 'Crop sold',
                  marker = list(color =pal[2]),
                  hoverinfo = 'text',
                  hovertext = paste(round(pinccat$cs*100), "%")) %>%
        add_trace(y = ~lc, name = 'Livestock consumed',
                  marker = list(color = pal[3]),
                  hoverinfo = 'text',
                  hovertext = paste(round(pinccat$lc*100), "%")) %>%
        add_trace(y = ~ls, name = 'Livestock sold',
                  marker = list(color = pal[4]),
                  hoverinfo = 'text',
                  hovertext = paste(round(pinccat$ls*100), "%")) %>%
        add_trace(y = ~off, name = 'Off farm',
                  marker = list(color = pal[5]),
                  hoverinfo = 'text',
                  hovertext = paste(round(pinccat$off*100), "%")) %>%
        add_trace(y = 0, name = ' ',
                  marker = list(color = 'white'),
                  hoverinfo = 'text',
                  hovertext = leg)
      fig <- fig %>%
        layout(
          title = "",
          xaxis = list(title = "households"), #categoryorder = "total ascending"
          yaxis = list(title = "USD/person/day",
                       range = ry),
          barmode = 'stack',
          hovermode = 'x unified') %>%
        config(modeBarButtons = list(list("toImage")),
               displaylogo = FALSE)
      return(fig)
    }
  } else {
    seg <-seg[keep]
    df <- rowsum(pinccat,seg)
    df <- df/rowSums(df)*100
    names(df) <- legP
    if (!interplot){
      par(mar=par()$mar+c(0,0,0,mar4))
      barx <- barplot(t(df),las=1, xlab= "Segmentation",
                      ylab="% value of production",
                      col=pal)
      legend(max(barx)*1.1, 80, xpd=NA,
             legend = rev(legP), fill=rev(pal))
      par(mar=par()$mar-c(0,0,0,mar4))
      invisible(df)
    } else {
      fig <- plot_ly(df,
                     type="bar", name="Income")
      for (i in 1:ncol(df)){
        fig <- fig %>%
          add_bars(x=1:nrow(df),y= df[,i],
                   name = names(df)[i],
                   marker = list(color = pal[i]),
                   hoverinfo = 'text',
                   hovertext = paste(names(df)[i], ":", round(df[,i]), "%")
          )
      }
      fig <- fig %>% add_trace(x=1:nrow(df),y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste(row.names(df),":", as.numeric(table(x$segmentation)), "hh"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=1:nrow(df),
                                        ticktext=row.names(df),
                                        title = "Segmentation"))
      fig <- fig %>% config(modeBarButtons = list(list("toImage")),
                            displaylogo = FALSE)
      return(fig)
    }
  }
}
