shinyServer(function(input, output, session) {

  # Tab 1 : DATASET --------------------------
  info <- eventReactive(input$load, {
    req(input$file)

    # Changes in read.table
    load(input$file$datapath)
    return(hhinfo)
    # vars <- names(f)
    # # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "scale1","Select State variable",
                      choices = vars, selected = vars[1])
    updateSelectInput(session, "stA","Select Stressor alpha",
                      choices = vars, selected = vars[2])
    updateSelectInput(session, "stB","Select Stressor beta",
                      choices = vars, selected = vars[3])
    # f
  })

  output$insub1 <- renderUI({
    hhinfo <- info()
    if (is.factor(hhinfo[,input$scale1])){
      listO <- c("All", levels(hhinfo[,input$scale1]))
    } else {
      listO <- c("All", sort(unique(hhinfo[,input$scale1])))
    }
    selectInput('sub1', input$scale1, listO, selected="All")
  })

  hhS1 <- reactive({
    hhinfo <- info()
    req(input$sub1)
    if (input$sub1 != "All"){
      sel <- hhinfo[,input$scale1]==input$sub1
    } else {
      sel <- rep(TRUE, nrow(hhinfo))
    }
    return(hhinfo[sel,])
  })

  output$inscale2 <- renderUI({
    hhinfo <- info()
    sc2 <- scaleChoices[!scaleChoices%in%input$scale1 & scaleChoices%in%names(hhinfo)]
    selectInput('scale2', "Subsetting 2:", sc2)
  })

  output$insub2 <- renderUI({
    tabS <- hhS1()
    if (is.factor(tabS[,input$scale2])){
      listO2 <- c("All", levels(tabS[,input$scale2]))
    } else {
      listO2 <- c("All", sort(unique(tabS[,input$scale2])))
    }
    selectInput('sub2', input$scale2, listO2, selected="All")
  })

  hhInput <- reactive({
    req(input$sub2)
    tabS <- hhS1()
    if (input$sub2 != "All"){
      sel <- tabS[,input$scale2]==input$sub2
    } else {
      sel <- rep(TRUE, nrow(tabS))
    }
    return(tabS[sel,])
  })

  cropInput <- reactive({
    load(input$file$datapath)
      return(crop[crop$hhid%in% hhInput()$hhid,])
  })

  lstkInput <- reactive({
    load(input$file$datapath)
    return(lstk[lstk$hhid%in% hhInput()$hhid,])
  })

  output$tmapdata <- renderTmap({
    tmap_hh(hhInput(), interplot=TRUE)
  })

  output$nhh <- renderText({
    paste("N=", nrow(hhInput()), "households were selected.")
  })

  # Tab 2 : CROP --------------------------------
  output$distha <- renderPlotly({
    bar_hist(hhInput()$land_cultivated_ha, interplot=TRUE)
  })

  output$popcrop <- renderPlotly({
    bar_div(cropInput(), hhInput()$hhid, interplot = TRUE)
  })

  # Tab 3 : LIVESTOCK --------------------------------
  output$disttlu <- renderPlotly({
    bar_hist(hhInput()$livestock_tlu, interplot=TRUE, xunit = "tlu")
  })

  output$poplstk<- renderPlotly({
    bar_div(lstkInput(), hhInput()$hhid, interplot = TRUE)
  })

  # Tab 4: FOOD SECURITY ----------------------------
  output$monthfi<- renderPlotly({
    bar_box(hhInput()$foodshortage_count, interplot = TRUE)
  })

  output$disthfias<- renderPlotly({
    bar_factor(hhInput()$hfias_status, interplot = TRUE)
  })

  output$hdds<- renderPlotly({
    bar_hdds(hhInput(), interplot = TRUE)
  })

  # Tab 5 : ECONOMIC --------------------------
  output$divincome<- renderPlotly({
    x <- hhInput()
    income_div <- NAto0(x$off_farm_div)+NAto0(x$farm_income_div)
    bar_box(income_div, lab = "Number of income sources", interplot = TRUE)
  })

  output$income<- renderPlotly({
    bar_income(hhInput(), interplot = TRUE)
  })


  # OTHER -------------------------------------------
  output$geninc<- renderPlotly({
    bargen(segInput(), seg = input$seggen)
  })

  output$gencons<- renderPlotly({
    bargen(segInput(), type="consumed", seg = input$seggen)
  })


  output$cropprod <- renderPlotly({
    barcropgen(tabInput())
  })

  output$lstkprod <- renderPlotly({
    barlstkgen(tabInput())
  })

  output$segPlot <- renderPlot({
    plot0seg(segInput())
  })

  output$waffle <- renderPlot({
    wafle_hfias(indInput())
  })

  output$month <- renderPlot({
    barmonth(tabInput())
  })

  output$class <- renderPlot({
    class_hfias(tabInput())
  })

  output$seg3d <- renderPlotly({
    plot3dseg(segInput(), var = ifelse(input$segdata==1, "ppp", "fa"))
  })

  output$segBox <- renderPlot({
    boxseg(segInput(), input$xvar)
  })

  output$scenPlot <- renderPlot({
    #par(mfrow=laymfrow(varY), mar=c(2,2,1,0.5))
    par(mfrow=c(3,2), mar=c(3,3,3,0.5))
    varP <- c("crop_yield", "farm_income_ppp",
              "perc_sold_kg", "gender_income_perc",
              "food_availaibility_kcal_mae")

    if (input$visuscenario == "bar"){
      for (i in varP){
        if (i %in% logV & !input$baroffset){
          bardiff(sceInput(), indInput(), i, offset=!input$baroffset,
                  main=paste0("log(",i,")"), logy = TRUE)
        } else {
          bardiff(sceInput(), indInput(), i, offset=!input$baroffset, main=i)
        }
      }
    }
    if (input$visuscenario == "box"){
      for (i in varP){
        if (input$boxseg){
          boxdiff(sceInput(), indInput(), var = i,
                  seg=segInput()$segmentation, main=i)
        } else {
          boxdiff(sceInput(), indInput(), var = i, main=i)
        }
      }
    }
    if (input$visuscenario == "bin"){
      for (i in varP){
        if (input$boxseg){
          bindiff(sceInput(), indInput(), var = i,
                  seg=segInput()$segmentation, main=i)
        } else {
          bindiff(sceInput(), indInput(), var = i, main=i)
        }
      }
      plot.new()
      legend("center", legend = c("+", "=", "-"),
             fill=rev(c("#f1a340", "grey", "#998ec3")),
             title="Effect", cex=2)
    }
  })

  # Farm exploration outputs
  output$crophh <- renderPlotly({
      return(barcrop01(tabInput(), top=7))
  })

  output$crop0 <- renderPlot({
    return(barcrop00(tabInput()))
  })

  # Farm exploration outputs
  output$lstkhh <- renderPlotly({
    return(barlstk01(tabInput(), tlu_conversion, top=5))
  })

  output$lstk0 <- renderPlot({
    return(barlstk00(tabInput()))
  })

  # Scatter plot
  output$scatter <- renderPlotly({
    # i0 <- indInput()
    # if(input$zvar=="segmentation" || input$xvar=="segmentation"){
    #   if(input$segdata==1){
    #     seg=segmentation_siaf(i0, type = input$segtype)
    #   } else {
    #     seg=segmentation_fa(i0, type = input$segtype)
    #   }
    #   i0$segmentation <- seg
    # }
    if (input$zvar=="none"){
      return(scatterly(segInput(), input$xvar, input$yvar, z = NULL))
    } else {
      return(scatterly(segInput(), input$xvar, input$yvar, input$zvar))
    }
  })

  output$spabox <- renderPlotly({
      return(boxregion(segInput(), input$xvar))
  })

  output$foodavai <- renderPlotly({
    barfood(indInput())
  })

  # output$alluhh <- render_parcats({
  #   parcats_hh(indInput())
  # })

  output$mapplot <- renderPlotly({
    mapind(segInput(), sel = input$mapvar)
  })

  output$plotseg1 <- renderPlot({
    req(input$segType)
    df <- segInput()
    if (input$segType=="User-defined"){
      if(input$var2!="none"){
        if(input$var3!="none"){
          par(mfrow=c(3,1))
        } else {
          par(mfrow=c(2,1))
        }}
      plot_density(df, input$var1, input$thvar1)
      if(input$var2!="none"){
        plot_density(df, input$var2, input$thvar2)
        if(input$var3!="none"){
          plot_density(df, input$var3, input$thvar3)
        }
      }
    }
    if (input$segType%in%c("Dorward", "Farm orientation")){
      par(mfrow=c(3,1))
      plot_density(df, "tot_income_ppp_person", input$thPPP)
      plot_density(df, "perc_sold_kg", input$thSale)
      plot_density(df, "perc_off_farm", input$thOff)
    }
    if (input$segType=="Data-driven"){
      if(input$var2=="none"){
        out <- segmentation_data(df, input$var1,
                                 nclu=input$nClu, type=input$cluType)$df
      } else if(input$var3=="none") {
        out <- segmentation_data(df, input$var1, input$var2,
                                 nclu=input$nClu, type=input$cluType)$df
      } else {
        out <- segmentation_data(df, input$var1,
                                 input$var2, input$var3,
                                 input$nClu, input$cluType)$df
      }
      out <- out[complete.cases(out),]
      if (input$cluType=="hclust"){
        distdf <- dist(out, method = "euclidean")
        den <- hclust(distdf,method = "ward.D2")
        plot(den, hang=-1, ax = T, ann=F, xlab="", sub="", labels=FALSE)
        #Visualize the cutting
        rect.hclust(den, k=input$nClu, border="red")
      } else{
        wss <- (nrow(out)-1)*sum(apply(out,2,var))
        for (i in 1:10) {
          wss[i] <- sum(kmeans(out, centers=i, nstart = 10)$withinss)
        }
        plot(seq_along(wss), wss, type = "b", pch = 19,
             col=ifelse(seq_along(wss)==input$nClu, "red", "black"),
             xlab="# cluster", ylab="Within sum of square")
      }
    }
  })

  output$plotseg2 <- renderPlotly({
    pieseg(segInput()$segmentation)
    #barplot(table(segInput()$segmentation))
  })

  # output$map <- renderLeaflet({
  #   mapleaf(segInput(), sel = input$xvar)
  # })

  output$renderedReport <- renderUI({
    includeHTML('Documentation.html')
  })
})




# segInput <- reactive({
#   req(input$segType)
#   df <- indInput()
#   if (input$segType=="Dorward"){
#     seg <- segmentation_litt(df, type = "dorward",
#                              thPPP = input$thPPP,
#                              thSale = input$thSale,
#                              thOff = input$thOff)
#   }
#   if (input$segType=="Farm orientation"){
#     seg <- segmentation_litt(df, type = "orientation",
#                              thPPP = input$thPPP,
#                              thSale = input$thSale,
#                              thOff = input$thOff,
#                              thSale0= input$thSale2)
#   }
#   if (input$segType=="User-defined"){
#     req(input$var2)
#     if(input$var2=="none"){
#       seg <- segmentation_user(df, input$var1,
#                                th1=input$thvar1)
#     } else if(input$var3=="none") {
#       seg <- segmentation_user(df, input$var1,
#                                input$var2,
#                                th1=input$thvar1,
#                                th2=input$thvar2)
#     } else {
#       seg <- segmentation_user(df, input$var1,
#                                input$var2, input$var3,
#                                input$thvar1, input$thvar2,
#                                input$thvar3)
#     }
#   }
#   if (input$segType=="Data-driven"){
#     req(input$var2)
#     if(input$var2=="none"){
#       seg <- segmentation_data(df, input$var1,
#                                nclu=input$nClu, type=input$cluType)$seg
#     } else if(input$var3=="none") {
#       seg <- segmentation_data(df, input$var1, input$var2,
#                                nclu=input$nClu, type=input$cluType)$seg
#     } else {
#       seg <- segmentation_data(df, input$var1,
#                               input$var2, input$var3,
#                               input$nClu, input$cluType)$seg
#     }
#   }
#   df$segmentation <- as.factor(seg)
#
#   return(df)
# })

# tabsegInput <- reactive({
#   req(input$segType)
#   ind <- segInput()
#   tab <- tabInput()
#   tab$segmentation <- ind$segmentation
#   return(tab)
# })

# dataset information
# output$info <- renderTable({
#   nhh <- nrow(tabInput())
#   nacoo <- sum(apply(is.na(tabInput()[,c("gps_lon", "gps_lat")]),1,any))
#   nahdds <- sum(apply(is.na(tabInput()[,c("hdds_good_season", "hdds_bad_season")]),1,any))
#
#   info <- data.frame(
#     nhh,
#     round(nacoo/nhh*100),
#     round(nahdds/nhh*100)
#   )
#   names(info) <- c("number\nhouseholds",
#                    "coordinates\n%NA",
#                    #"lcu to ppp\n%NA",
#                    "HDDS\n%NA")
#   return(info)
# }, digits=0)

# dataset information
# output$infocrop <- renderTable({
#   x <- segInput()
#
#   if(!input$segcrop){
#     info <- data.frame(
#       "lcult"= median(x$land_cultivated_ha, na.rm = TRUE),
#       "cropdiv"= median(x$crop_diversity, na.rm = TRUE),
#       "cropincome"=round(median(x$crop_income_ppp, na.rm=TRUE))
#     )
#     row.names(info) <- "median"
#   } else {
#     info <- data.frame(
#       "lcult"= tapply(x$land_cultivated_ha, x$segmentation, median, na.rm = TRUE),
#       "cropdiv"= tapply(x$crop_diversity, x$segmentation, median, na.rm = TRUE),
#       "cropincome"=round(tapply(x$crop_income_ppp, x$segmentation, median, na.rm=TRUE))
#     )
#   }
#
#   names(info) <- c("land cultivated (ha)",
#                    "crop diversity",
#                    "crop income (USD ppp)")
#
#   return(info)
# }, rownames = TRUE)

# output$infolstk <- renderTable({
#   x <- segInput()
#
#   if(!input$seglstk){
#     info <- data.frame(
#       "tlu"= median(x$lstk_tlu, na.rm = TRUE),
#       "lstkdiv"= median(x$lstk_diversity, na.rm = TRUE),
#       "lstkincome"=round(median(x$lstk_income_ppp, na.rm=TRUE))
#     )
#     row.names(info) <- "median"
#   } else {
#     info <- data.frame(
#       "tlu"= tapply(x$lstk_tlu, x$segmentation, median, na.rm = TRUE),
#       "lstkdiv"= tapply(x$lstk_diversity, x$segmentation, median, na.rm = TRUE),
#       "lstkincome"=round(tapply(x$lstk_income_ppp, x$segmentation, median, na.rm=TRUE))
#     )
#   }
#
#   names(info) <- c("livestock herd (tlu)",
#                    "livestock diversity",
#                    "livestock income (USD ppp)")
#
#   return(info)
# }, rownames = TRUE)

# dataset information
# output$infofs <- renderTable({
#   x <- tabsegInput()
#
#   listmonth <- strsplit(x$foodshortagetime_months_which, " ")
#   nmonth <- sapply(listmonth, lengthNA)
#
#   if(!input$segfs){
#     info <- data.frame(
#       "mfis"= median(nmonth),
#       "hfias"= round(sum(x$hfias_status=="food_secure", na.rm=TRUE)/nrow(x)*100),
#       "hdds"= mean(x$hdds_bad_season, na.rm = TRUE)
#     )
#     row.names(info) <- ""
#   } else {
#     pfs <- tapply(x$hfias_status=="food_secure", x$segmentation, sum, na.rm = TRUE)/table(x$segmentation)
#     info <- data.frame(
#       "mfis"= tapply(nmonth, x$segmentation, median, na.rm = TRUE),
#       "hfias"= round(as.numeric(pfs)*100),
#       "hdds"= tapply(x$hdds_bad_season, x$segmentation, mean, na.rm = TRUE)
#     )
#   }
#
#   names(info) <- c("median number of month with food shortage",
#                    "% food secure (HFIAS)",
#                    "mean diet diversity (HDDS)")
#
#   return(info)
# }, rownames = TRUE)

# output$infoecon <- renderTable({
#   x <- segInput()
#
#   if(!input$segecon){
#     info <- data.frame(
#       "incdiv"= median(x$income_diversity, na.rm = TRUE),
#       "ppp"= median(x$tot_income_ppp_person/365, na.rm = TRUE),
#       "psold"=round(median(x$perc_sold_kg, na.rm=TRUE),2)
#     )
#     row.names(info) <- "median"
#   } else {
#     info <- data.frame(
#       "incdiv"= tapply(x$income_diversity, x$segmentation, median, na.rm = TRUE),
#       "ppp"= tapply(x$tot_income_ppp_person/365, x$segmentation, median, na.rm = TRUE),
#       "psold"=round(tapply(x$perc_sold_kg, x$segmentation, median, na.rm=TRUE),2)
#     )
#   }
#
#   names(info) <- c("income diversity",
#                    "value of activities (USD ppp) per person per day",
#                    "% production sold")
#
#   return(info)
# }, rownames = TRUE)

# output$infogen <- renderTable({
#   x <- segInput()
#   info <- data.frame(
#     "inc"= round(sum(is.na(x$gender_income_perc))/nrow(x)*100),
#     "cons"= round(sum(is.na(x$gender_consumed_perc))/nrow(x)*100)
#   )
#   row.names(info) <- "%NA"
#   names(info) <- c("gender information on income",
#                    "gender information on consumption")
#   return(info)
# }, rownames = TRUE)



# dataset information
# output$quant <- renderTable({
#   if (!is.null(input$formid)){
#     sel <- indInput()[,c( "hh_size_mae", "land_cultivated_ha", "lstk_tlu")]
#     #, "tot_income_ppp_person", "hdds_bad_season", "food_availaibility_kcal_mae")]
#     i1 <- apply(sel, 2, quantile, na.rm=TRUE)
#     i2 <- apply(NAto0(sel)==0, 2, sum)/nrow(indInput())*100
#     quan <- rbind(i1, i2)
#     row.names(quan) <- c("min", "Q1", "median", "Q3", "max", "% of 0")
#     return(quan)
#   }
# }, rownames=TRUE)

# output$warnings <- renderText({
#   # basic checks
#   err <- c()
#   x <- tabInput()
#   # crop production
#   loopcrop <- gsub("^crop_name_", "",
#                    names(x)[grep("^crop_name_", names(x))])
#   crop_harvest <- do.call(rbind,x[,paste0("crop_harvest_kg_per_year_", loopcrop)])
#   if(sum(NAto0(crop_harvest))==0){
#     err <- c(err, "No crop harvest (kg) reported")
#   }
#   crop_sold <- do.call(rbind,x[,paste0("crop_sold_kg_per_year_", loopcrop)])
#   if(sum(NAto0(crop_sold))==0){
#     err <- c(err, "No crop sold reported")
#   }
#   crop_cons <- do.call(rbind,x[,paste0("crop_consumed_kg_per_year_", loopcrop)])
#   if(sum(NAto0(crop_cons))==0){
#     err <- c(err, "No crop consumed reported")
#   }
#   #value
#   crop_value <- do.call(rbind,x[,paste0("value_crop_consumed_lcu_", loopcrop)])
#   if(sum(NAto0(crop_value))==0){
#     err <- c(err, "No value for crop consumed reported")
#   }
#   if(sum(NAto0(x$value_livestock_consumed_ppp))==0){
#     err <- c(err, "No value for livestock consumed reported")
#   }
#
#   # lstk production
#   return(err)
# }, sep="\n")
#
# output$textseg <- renderText({
#   if (input$segtype=="dorward"){
#     if (input$segdata==1){
#       txt <- c("<p><b>Step out</b>: income >1PPP/day/person <br>AND off-farm income >50%</p>",
#                "<p><b>Step up</b>: income >1PPP/day/person <br>AND sold farm production >50% AND NOT Step out</p>",
#                "<p><b>Hang in</b>: NOT Step up <br>AND NOT Step out</p>")
#     } else {
#       txt <- c("<p><b>Step out</b>: FA >2000kcal/day/MAE <br>AND off-farm income >50%</p>",
#                "<p><b>Step up</b>: FA >2000kcal/day/MAE <br>AND sold farm production >50%</p>",
#                "<p><b>Hang in</b>: NOT Step up AND NOT Step out</p>")
#     }
#   } else {
#     if (input$segdata==1){
#       txt <- c("<p><b>Off farm</b>(off): off-farm income >50%</p>",
#                "<p><b>Intensifying market</b>(i-mkt): income >1PPP/day/person <br>AND sold farm production >50% AND NOT off</p>",
#                "<p><b>Mixed subsistence+market</b>(sub-mkt):\n sold farm production >20% AND NOT off AND NOT i-mkt</p>",
#                "<p><b>Subsistence</b>(sub): NOT off AND NOT i-mkt AND NOT sub-mkt</p>")
#     } else {
#       txt <- c("<p><b>Off farm</b>(off): off farm income >50%</p>",
#                "<p><b>Intensifying market</b>(i-mkt): FA >2000kcal/day/MAE <br>AND sold farm production >50% AND NOT off</p>",
#                "<p><b>Mixed subsistence+market</b>(sub-mkt):\n sold farm production >20% AND NOT off AND NOT i-mkt</p>",
#                "<p><b>Subsistence</b>(sub):\n NOT off AND NOT i-mkt AND NOT sub-mkt</p>")
#     }
#   }
#   return(txt)
# }, sep="\n")

# output$quantViolin <- renderPlotly({
#   boxquant(indInput())
# })
#

# output$inseg <- renderUI({
#   output = tagList()
#   if(input$segType%in%c("Dorward", "Farm orientation")){
#     output[[1]] <- sliderInput('thPPP', "Threshold Income",
#                 min = 0, max = 5, value = 1, step=0.25)
#     output[[2]] <- sliderInput('thSale', "Threshold % sold",
#                                min = 0, max = 100, value = 50, step=5)
#     output[[3]] <-sliderInput('thOff', "Threshold % off farm",
#                 min = 0, max = 100, value = 50, step=5)
#   }
#   if(input$segType=="Farm orientation"){
#     output[[4]] <-sliderInput('thSale2', "Second threshold % sold",
#                               min = 0, max = 50, value = 20, step=5)
#   }
#   if(input$segType%in%c("Data-driven")){
#     output[[1]] <- selectInput('cluType', "Clustering", c("kmeans", "hclust"))
#     output[[2]] <- sliderInput('nClu', "Number of clusters",
#                                min = 2, max = 10, value = 3, step=1)
#
#   }
#   if(input$segType%in%c("User-defined", "Data-driven")){
#     output[[length(output)+1]] <- selectInput('var1', "Variable 1", varY,
#                                               selected="hdds_bad_season")
#   }
#   return(output)
# })

# output$inthvar1 <- renderUI({
#   req(input$var1)
#   if (input$segType=="User-defined"){
#     x1 <- indInput()[,input$var1]
#     sel1 <- which(x1>min(x1, na.rm=TRUE)&x1<max(x1, na.rm=TRUE))
#     q1 <- round(quantile(x1[sel1],
#                  probs=c(0.1, 0.5, 0.9), na.rm=TRUE),1)
#     sliderInput('thvar1', "Threshold",
#                 min = q1[1], max = q1[3],
#                 value = q1[2], step=diff(range(q1))/10)
#   }
# })

# output$invar2 <- renderUI({
#   req(input$var1)
#   selectInput('var2', "Variable 2",
#               c("none", varY[-which(varY%in%input$var1)]))
# })

# output$inthvar2 <- renderUI({
#   req(input$var2)
#   if (input$var2!="none" & input$segType=="User-defined"){
#     output = tagList()
#     x2 <- indInput()[,input$var2]
#     sel2 <- which(x2>min(x2, na.rm=TRUE)&x2<max(x2, na.rm=TRUE))
#     q2 <- round(quantile(x2[sel2],
#                          probs=c(0.1, 0.5, 0.9), na.rm=TRUE),1)
#     sliderInput('thvar2', "Threshold",
#                 min = q2[1], max = q2[3],
#                 value = q2[2], step=diff(range(q2))/10)
#   }
# })

# output$invar3 <- renderUI({
#   req(input$var2)
#   if (input$var2!="none"){
#     selectInput('var3', "Variable 3",
#                 c("none", varY[-which(varY%in%c(input$var1, input$var2))]))
#   }
# })


# output$inthvar3 <- renderUI({
#   req(input$var3)
#   if (input$var3!="none" & input$segType=="User-defined"){
#     x3 <- indInput()[,input$var3]
#     sel3 <- which(x3>min(x3, na.rm=TRUE)&x3<max(x3, na.rm=TRUE))
#     q3 <- round(quantile(x3[sel3],
#                  probs=c(0.1, 0.5, 0.9), na.rm=TRUE), 1)
#
#   sliderInput('thvar3', "Threshold",
#               min = q3[1], max = q3[3],
#               value = q3[2], step=diff(range(q3))/10)
#   }
# })
