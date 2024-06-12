#' Barplot of the crop or livestock adoption
#'
#' This function make a barplot showing the most popular
#' crop or livestock in the dataset
#' @param tab either crop or lvst table with columns hhid and name
#' @param idlist full list of households id
#' @param th threshold to show all items with more than th (default=10)
#' @param seg segmentation of households (if any)
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param colcat colors of the items
#' @param marx extra margin for plotting the crop names (when not interactive plot)
#' @keywords barplot, adoption
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' bar_div(hhdb_rhomis$crop, idlist=hhdb_rhomis$hhinfo$hhid)
#' bar_div(hhdb_rhomis$lstk, idlist=hhdb_rhomis$hhinfo$hhid)
#'
bar_div <- function(tab, idlist=NULL, th=10, seg=NULL,
                    interplot=FALSE, colcat="#1F77B4",
                    marx=4){

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
    tab <- tab[tab$hhid%in%idlist,]
  }

  if(!is.null(seg) & length(seg)!= length(idlist)){
    stop("Segmentation list has different length than the idlist")
  }

  if(!is.null(seg) & any(is.na(seg))){
    warning("NA in segmentation list, that were removed from tab")
    idlist <- idlist[!is.na(seg)]
    seg <- seg[!is.na(seg)]
    tab <- tab[tab$hhid%in%idlist,]
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
    # fill in with 0 if missing group
    if (nrow(df)!=length(nolstk)){
      df <- df[match(names(nolstk), row.names(df)),]
      row.names(df) <- names(nolstk)
      df[is.na(df)] <- 0
    }


    dfp <- df/as.numeric(table(seg))*100
    # set threshold to keep livestock
    if (nrow(dfp)==1){
      keeplstk <- dfp>th
      df <- df[,keeplstk]
      df <- df[order(df, decreasing = TRUE)]
      # if (length(df>2)){
      pal <- pals::brewer.set1(max(c(length(df), 3)))
      # } else {
      #   pal <- rainbow(2)
      # }

      dfp <- data.frame(t(df/as.numeric(table(seg))))
      row.names(dfp) <- names(nolstk)
    } else {
      keeplstk <- colSums(dfp>th)>=1
      df <- df[,keeplstk]
      df <- df[,order(colSums(df), decreasing = TRUE)]
      pal <- pals::brewer.set1(max(c(length(df), 3)))
      dfp <- data.frame(df/as.numeric(table(seg)))
    }

    nolstkp <- nolstk/as.numeric(table(seg))
    if (!interplot){
      par(mar=par()$mar+c(marx,0,0,marx*1.3))
      barx <- barplot(t(dfp),
                      ylim=c(-max(nolstkp), max(rowSums(dfp))),
                      col=pal, las=2)
      barplot( -nolstkp, col="black", add=TRUE, las=2)
      legend(max(barx)+0.9, max(rowSums(dfp))*0.8,
             legend = c(rev(names(dfp)), "None"),
              fill = c(rev(pal), "black"), xpd=NA)
      par(mar=par()$mar-c(marx,0,0,marx*1.3))
      invisible(dfp)
    } else {
      fig <- plot_ly(dfp,
                     type="bar", name="Production")
      for (i in 1:ncol(dfp)){
        if (nrow(dfp)==1){
          legi=paste0(names(dfp)[i], "<br>N=", df[i], "(", round(dfp[i]*100), "%)")
        } else {
          legi=paste0(names(dfp)[i], "<br>N=", df[,i], "(", round(dfp[,i]*100), "%)")
        }
        fig <- fig %>%
          add_bars(x=1:nrow(dfp), y= dfp[,i],
                   name = names(dfp)[i],
                   hoverinfo = 'text',
                   hovertext = legi
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



#' Distribution of numerical values as histogram
#'
#' This function show the distribution of quantities
#' such as land cultivated, livestock herd size, family size, ...
#' @param x vector with the variable of interest
#' @param breaks breaks to build the histogram, by default 6 breaks for positive values
#' @param lab the name of the classes (should be one length shorter than break)
#' @param xunit the label of the axis
#' @param pal colors of the numerical categories  (should be one length shorter than break)
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param seg segmentation of households (if any)
#' @keywords hist
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #distribution of land cultivated
#' bar_hist(hhdb_rhomis$hhinfo$land_cultivated_ha)
#'
#' #land cultivated per HDDS score
#' bar_hist(hhdb_rhomis$hhinfo$land_cultivated_ha,
#'          seg=hhdb_rhomis$hhinfo$hdds_score)
#'
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


#' Distribution of count values
#'
#' This function show the distribution of count values
#' such as the HDDS score or the number of months with food insecurity
#' @param x vector with the variable of interest
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param seg segmentation of households (if any)
#' @param lab the label on the axis
#' @keywords boxplot
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #distribution of number of month with food insecurity
#' bar_box(hhdb_rhomis$hhinfo$foodshortage_count)
#'
#' #distribution of HDDS score per large region
#' bar_box(hhdb_rhomis$hhinfo$hdds_score, lab="HDDS",
#'         seg=hhdb_rhomis$hhinfo$large_region)
#'
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

#' Distribution of scores
#'
#' This function show the distribution of scores that are built on multiple categories
#' such as the HDDS or the FIES
#' @param tab the dataframe with information on the score
#' @param score name of the score and prefix of columns of interest
#' @param pal colors of the categories
#' @param marx extra margin for plotting the names (when not interactive plot)
#' @param rm0 remove households with a score of 0 (are they hidden NA?)
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param seg segmentation of households (if any)
#' @keywords diet diversity
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot HDDS score
#' bar_score(hhdb_rhomis, score="HDDS")
#'
#' #plot FIES per large region
#' bar_score(hhdb_rhomis, score="FIES",
#'           seg=hhdb_rhomis$hhinfo$large_region)
#'
bar_score <- function(tab, score="HDDS", pal= NULL, marx=6, rm0=FALSE,
                      interplot=FALSE, seg=NULL){

  if(inherits(tab, "farmhousehold")){
    tab <- tab$hhinfo
  }

  #get the column starting with "score"
  col <- names(tab)[grep(paste0(score, "_"), names(tab), ignore.case = TRUE)]
  #but not the score itself
  col <- col[-grep("_score$", col, ignore.case = TRUE)]
  #nor season
  col <- col[-grep("_season$", col, ignore.case = TRUE)]
  col <- col[-grep("_month$", col, ignore.case = TRUE)]
  if(length(col)== 0){
    stop("Can not find the columns of the score")
  }

  if(is.null(pal) | length(pal)!= length(col)){
    pal <- pals::cols25(length(col))
  }

  if(!is.null(seg) & length(seg)!= nrow(tab)){
    stop("Segmentation list has different length than diet information")
  }

  if(!is.null(seg) & any(is.na(seg))){
    warning("NA in segmentation list, that were removed from tab")
    tab <- tab[!is.na(seg),]
    seg <- seg[!is.na(seg)]
  }

  sumscore <- rowSums(tab[,col], na.rm = TRUE)
  sumna <- rowSums(is.na(tab[,col]), na.rm = TRUE)
  #remove households with score of 0
  if (rm0){
    diet <- tab[sumscore>0,col]
    if(!is.null(seg)){ seg <- seg[sumscore>0]}
  } else {
    diet <- tab[sumna<length(col),col]
    if(!is.null(seg)){ seg <- seg[sumna<length(col)]}
  }

  #replace NA per 0
  diet[is.na(diet)] <- 0
  colnames(diet) <- gsub(paste0("^",score, "_"), "", colnames(diet))

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
      par(mar=par()$mar+c(0,0,0,marx))
      barx <- barplot(t(df[,1:length(col)]),
                      width = df$width, col=pal,
                      xlab=score)
      legend(max(barx)*1.03, 0.8*max(sumhdds),
             legend = gsub("_", " ", rev(names(df)[1:length(col)])),
             fill = rev(pal), xpd=NA, cex=0.8)
      par(mar=par()$mar-c(0,0,0,marx))
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
                               hovertext = paste(score,row.names(df),":", as.numeric(nf), "hh, (", round(as.numeric(nf)/sum(nf)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            xaxis= list(showline = TRUE,
                                        linecolor = '#000',
                                        tickvals=df$xtick,
                                        ticktext=row.names(df),
                                        title = score))
      fig <- fig %>% config(modeBarButtons = list(list("toImage")),
                            displaylogo = FALSE)
      return(fig)
    }
  } else {
    seg <- as.character(seg)
    nf <- table(seg)
    ncat <- rowsum(diet, seg, na.rm = TRUE)
    df <- data.frame(ncat/as.numeric(nf))
    meanhd <- apply(ncat/as.numeric(nf),1,sum)

    if (!interplot){
      par(mar=par()$mar+c(0,0,0,marx))
      barx <- barplot(t(df[,1:length(col)]),
                      col=pal,
                      ylab=score)
      legend(max(barx)*1.03, 0.8*max(meanhd),
             legend = gsub("_", " ", rev(names(df)[1:length(col)])),
             fill = rev(pal), xpd=NA, cex=0.8)
      par(mar=par()$mar-c(0,0,0,marx))
      invisible(df)
    } else {
      fig <- plot_ly(df,
                     type="bar", name=score)
      for (i in 1:length(col)){
        fig <- fig %>%
          add_bars(x=row.names(df),y= df[,i],
                   marker = list(color = pal[i],
                                 line = list(color = 'black',
                                             width = 1)),
                   name = colnames(df)[i],
                   hoverinfo = 'text',
                   hovertext = paste(colnames(df)[i], ":", round(df[,i]*100), "%"))
      }
      fig <- fig %>% add_trace(x=row.names(df),y = 0, name = ' ',
                               marker = list(color = 'white'),
                               hoverinfo = 'text',
                               hovertext = paste(score,row.names(df),":", as.numeric(nf), "hh, (", round(as.numeric(nf)/sum(nf)*100), "%)"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            xaxis= list(title = "Segmentation"))
      fig <- fig %>% config(modeBarButtons = list(list("toImage")),
                            displaylogo = FALSE)
      return(fig)
    }
  }
}


#' Show the months with food insecurity
#'
#' This function show the months with food insecurity
#' @param x the vector containing the months with food shortage
#' @param sep the separation between months, by default " "
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param lab the label on the axis
#' @keywords months food security
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot the month with food shortage score
#' bar_months(hhdb_rhomis$hhinfo$foodshortage_months)
#'
bar_months <- function(x, sep=" ", interplot=FALSE,
                    lab= "Month with food insecurity"){
  months <- unlist(strsplit(x, " "))
  months <- factor(tolower(months), levels = tolower(month.abb))

  df <- data.frame(table(months))
  df$perc <- df$Freq /length(x)*100
  if (!interplot){
    barplot(df$perc, las=1, xlab= lab,
            ylab="% of households",
            names=df$months, col="#1F77B4")
    invisible(df)
  } else {
    df$col <- "#1F77B4"
    df$leg <- paste0(df$months, "<br>N=", df$Freq, "(", round(df$perc), "%)")
    fig <- plot_ly(df, x = ~months, y = ~perc,
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
}

#' Show the sources of income per household
#'
#' This function show the value of production per sources
#' @param tab the dataframe with information on the income activities, usually hhinfo
#' @param qmax percentile that is not shown on y axis
#' @param marx extra margin for plotting the names (when not interactive plot)
#' @param seg segmentation of households (if any)
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @param maxbar number of bars to be plotted. If too high, the barplot is hardly readable.
#' @keywords sources income
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot the source of income
#' bar_income(hhdb_rhomis)
#'
#' #plot the source of income per large region
#' bar_income(hhdb_rhomis, seg=hhdb_rhomis$hhinfo$large_region)
#'
bar_income <- function(tab, qmax=0.95, marx=6, seg=NULL, interplot=FALSE, maxbar=100){
  if(inherits(tab, "farmhousehold")){
    tab <- tab$hhinfo
  }

  if(!is.null(seg) & length(seg)!= nrow(tab)){
    stop("Segmentation list has different length than the variable")
  }
  if(!is.null(seg) & any(is.na(seg))){
    warning("NA in segmentation list, that were removed from tab")
    tab <- tab[!is.na(seg),]
    seg <- seg[!is.na(seg)]
  }

  keep <- NAto0(tab$hh_size_members)>0 & NAto0(tab$currency_conversion_lcu_to_ppp)>0
  stab <- subset(tab, keep)
  inccat <- data.frame(
    "cc"=NAto0(stab$crop_value_lcu)/(stab$hh_size_members*365),
    "cs"=NAto0(stab$crop_income_lcu)/(stab$hh_size_members*365),
    "lc"=NAto0(stab$lstk_value_lcu)/(stab$hh_size_members*365),
    "ls"=NAto0(stab$lstk_income_lcu)/(stab$hh_size_members*365),
    "off"=NAto0(stab$off_farm_lcu)/(stab$hh_size_members*365)
  )
  # conversion lcu to usd ppp
  inccat[is.na(inccat)] <- 0
  inccat <- inccat/as.numeric(stab$currency_conversion_lcu_to_ppp)


  legP <- c("Crop consumed", "Crop sold",
            "Livestock consumed", "Livestock sold",
            "Off farm")
  pal <- c("green", "orange","red", "purple", "black")

  # compute percentages
  tot <- rowSums(inccat, na.rm = TRUE)
  pinccat <- inccat/tot
  pinccat[is.na(pinccat)] <- 0

  if(is.null(seg)){
    o1 <- order(tot)
    inccat <- inccat[o1,]
    pinccat <- pinccat[o1,]
    if (nrow(stab)<2*maxbar){
      leg <- paste0(stab$hh_size_members, "persons <br>",
                    round(stab$land_cultivated_ha,1), "ha <br>",
                    round(stab$livestock_tlu,1), "TLU <br>",
                    round(tot,2), "USD/person/day")
      leg <- leg[o1]
    } else {
      #simplify and group households to lower the number of bars
      hhgroup <- cut(1:nrow(stab), seq(1, nrow(stab), length.out=maxbar),
                     include.lowest = TRUE)

      inccat <- rowsum(inccat,hhgroup)/as.numeric(table(hhgroup))
      tot <- rowSums(inccat, na.rm = TRUE)
      pinccat <- inccat/tot
    }
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
                  hovertext = paste(round(pinccat$off*100), "%"))
      fig <- fig %>%
        layout(
          title = "",
          xaxis = list(title = "households by income"), #categoryorder = "total ascending"
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
      par(mar=par()$mar+c(0,0,0,marx))
      barx <- barplot(t(df),las=1, xlab= "Segmentation",
                      ylab="% value of production",
                      col=pal)
      legend(max(barx)*1.1, 80, xpd=NA,
             legend = rev(legP), fill=rev(pal))
      par(mar=par()$mar-c(0,0,0,marx))
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
                               hovertext = paste(row.names(df),":", as.numeric(table(seg)), "hh"))
      fig <- fig %>% layout(barmode = 'stack',
                            hovermode = 'x unified',
                            yaxis= list(title = "%"),
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


#' Select the threshold for numeric variables
#'
#' This function show the threshold of a numeric variable
#' @param x the vector containing the variable of interest
#' @param lab the label on the axis
#' @param th the threshold
#' @param corskew correct the highly skewed variable
#' @param pal color of the below and above threshold
#' @keywords density threshold
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot the source of income
#' plot_density(hhdb_rhomis$hhinfo$hh_size_mae, th=5)
#'
plot_density <- function(x, th, lab="", corskew=TRUE, pal=c("red", "blue")){
  #make sure there is no NA or infinite values
  x <- x[complete.cases(x)&is.finite(x)]

  #check if skewed
  skewY <- moments::skewness(x, na.rm = TRUE)
  if(skewY>5 & corskew) {
    x <- x**(1/4)
    th <- th**(1/4)
    lab <- paste(lab, "(root transformed)")
  }

  d <- density(x, na.rm = TRUE)
  plot(d, main="", xaxt="n", xlab=lab,
       xlim=range(x, na.rm=TRUE), xaxs="i")
  #separate in two
  d1 <- cbind(d$x[d$x<=th], d$y[d$x<=th])
  d1 <- rbind(d1, c(d1[nrow(d1),1], 0))
  d2 <- cbind(d$x[d$x>th], d$y[d$x>th])
  d2 <- rbind(d2, c(d2[1,1], 0))
  polygon(d1,
          col=pal[1], border=pal[1])
  polygon(d2,
          col=pal[2], border=pal[2])

  if(skewY>5 & corskew) {
    xax <- axis(1, labels=FALSE)
    axis(1, at = xax, labels = round(xax**(4)))
  } else {
    axis(1)
  }
}

#' Show the pie chart of the number of households per group
#'
#' This function show the threshold of a numeric variable
#' @param x the vector containing the groups
#' @param interplot decide if the plot is interactive (with plotly) or not
#' @keywords pie
#' @export
#' @examples
#' data(hhdb_rhomis)
#'
#' #plot the source of income
#' pie_seg(hhdb_rhomis$hhinfo$large_region)
#'
#'
pie_seg <- function(x, interplot=FALSE){
  df <- data.frame(table(x, useNA="ifany"))
  df$x <- as.character(df$x)
  df$x[is.na(df$x)] <- "NA"
  df$Perc <- df$Freq/sum(df$Freq)
  df$text <- paste(df$x, "<br>n=", df$Freq, "<br>", round(df$Perc*100,1), "%")

  if(!interplot){
    pie(df$Freq, labels = df$x)
    invisible(df)
  } else {
    p1 <- plot_ly(df, labels = ~x, values = ~Freq,
                  type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  direction ='clockwise', sort=FALSE,
                  hoverinfo = 'text',
                  text = ~text) %>%
      layout(title = 'Segmentation') %>%
      config(modeBarButtons = list(list("toImage")),
             displaylogo = FALSE)
    return(p1)
  }
}

