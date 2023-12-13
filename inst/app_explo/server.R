shinyServer(function(input, output, session) {

  # Tab 1 : DATASET --------------------------
  info <- eventReactive(input$load, {
    req(input$file)
    load(input$file$datapath)
    return(hhinfo)
  })

  output$inscale1 <- renderUI({
    req(info())
    hhinfo <- info()
    sc1 <-  c("none", scaleChoices[scaleChoices%in%names(hhinfo)])
    selectInput("scale1", h5("Subsetting 1:"),
                choices = sc1)
  })

  output$insub1 <- renderUI({
    req(input$scale1)
    if (input$scale1 != "none"){
    hhinfo <- info()
    if (is.factor(hhinfo[,input$scale1])){
      listO <- levels(hhinfo[,input$scale1])
    } else {
      listO <- sort(unique(hhinfo[,input$scale1]))
    }
    selectInput('sub1', input$scale1, listO)
    }
  })

  hhS1 <- reactive({
    req(input$scale1)
    hhinfo <- info()
    if (input$scale1 != "none"){
      sel <- hhinfo[,input$scale1]==input$sub1
    } else {
      sel <- rep(TRUE, nrow(hhinfo))
    }
    return(hhinfo[sel,])
  })

  output$inscale2 <- renderUI({
    req(input$scale1)
    if (input$scale1!="none"){
      hhinfo <- info()
      sc2 <-  c("none", scaleChoices[!scaleChoices%in%input$scale1 & scaleChoices%in%names(hhinfo)])
      selectInput('scale2', "Subsetting 2:", sc2)
    }
  })

  output$insub2 <- renderUI({
    req(input$scale2)
    if (input$scale2!="none"){
      tabS <- hhS1()
      if (is.factor(tabS[,input$scale2])){
        listO2 <- levels(tabS[,input$scale2])
      } else {
        listO2 <- sort(unique(tabS[,input$scale2]))
      }
      selectInput('sub2', input$scale2, listO2)
    }
  })

  hhInput <- reactive({
    tabS <- hhS1()
    if (input$scale1 == "none"){
      return(tabS)
    } else {
      if (input$scale2 != "none"){
        sel <- tabS[,input$scale2]%in%input$sub2
      } else {
        sel <- rep(TRUE, nrow(tabS))
      }
      return(tabS[sel,])
    }
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
    req(input$scale1)
    paste("N=", nrow(hhInput()), "households were selected.")
  })

  # Tab 2 : CROP --------------------------------
  output$distha <- renderPlotly({
    if (input$segcrop){
      bar_hist(hhInput()$land_cultivated_ha, seg = segInput()$segmentation,
               interplot=TRUE)
    } else {
      bar_hist(hhInput()$land_cultivated_ha, interplot=TRUE)
    }
  })

  output$popcrop <- renderPlotly({
    if (input$segcrop){
      bar_div(cropInput(), hhInput()$hhid, interplot = TRUE,
              seg = segInput()$segmentation)
    } else {
      bar_div(cropInput(), hhInput()$hhid, interplot = TRUE)
    }
  })

  # Tab 3 : LIVESTOCK --------------------------------
  output$disttlu <- renderPlotly({
    if (input$seglstk){
      bar_hist(hhInput()$livestock_tlu, interplot=TRUE,
               xunit = "tlu", seg = segInput()$segmentation)
    } else {
      bar_hist(hhInput()$livestock_tlu, interplot=TRUE, xunit = "tlu")
    }
  })

  output$poplstk<- renderPlotly({
    if (input$seglstk){
      bar_div(lstkInput(), hhInput()$hhid, interplot = TRUE,
              seg = segInput()$segmentation)
    } else {
      bar_div(lstkInput(), hhInput()$hhid, interplot = TRUE)
    }
  })

  # Tab 4: FOOD SECURITY ----------------------------
  # Months with food shortage
  output$mfi_bar<- renderPlotly({
    if("foodshortage_count"%in% names(hhInput())){
      if (input$segfs){
        bar_box(hhInput()$foodshortage_count, interplot = TRUE,
                seg = segInput()$segmentation)
      } else {
        bar_box(hhInput()$foodshortage_count, interplot = TRUE)
      }
    }
  })


  output$mfi_which<- renderPlotly({
    if("foodshortage_months"%in% names(hhInput())){
      bar_months(hhInput()$foodshortage_months, interplot = TRUE)
    }
  })

  #HDDS
  output$hdds_bar<- renderPlotly({
    if("hdds_score"%in% names(hhInput())){
      if (input$segfs){
        bar_box(hhInput()$hdds_score, interplot = TRUE,
                lab = "Household Diet Diversity Score",
                seg = segInput()$segmentation)
      } else {
        bar_box(hhInput()$hdds_score, interplot = TRUE,
                lab = "Household Diet Diversity Score")
      }
    }
  })

  output$hdds_group<- renderPlotly({
    if(length(grep(paste0("HDDS_"), names(hhInput())))>1){
      if (input$segfs){
        bar_score(hhInput(), interplot = TRUE, rm0 = TRUE,
                  seg = segInput()$segmentation)
      } else {
        bar_score(hhInput(), interplot = TRUE, rm0 = TRUE)
      }
    }
  })

  #FIES
  output$fies_bar<- renderPlotly({
    if("fies_score"%in% names(hhInput())){
      if (input$segfs){
        bar_box(hhInput()$fies_score, interplot = TRUE,
                lab = "Food Security Experience Scale",
                seg = segInput()$segmentation)
      } else {
        bar_box(hhInput()$fies_score, interplot = TRUE,
                lab = "Food Security Experience Scale")
      }
    }
  })

  output$fies_group<- renderPlotly({
    if(length(grep(paste0("FIES_"), names(hhInput())))>1){
      if (input$segfs){
        bar_score(hhInput(), score = "FIES", interplot = TRUE,
                  seg = segInput()$segmentation)
      } else {
        bar_score(hhInput(), score = "FIES", interplot = TRUE)
      }
    }

  })

  # Tab 5 : ECONOMIC --------------------------
  output$divincome<- renderPlotly({
    x <- hhInput()
    income_div <- NAto0(x$off_farm_div)+NAto0(x$farm_income_div)
    if (input$segecon){
      bar_box(income_div, lab = "Number of income sources", interplot = TRUE,
              seg = segInput()$segmentation)
    } else {
      bar_box(income_div, lab = "Number of income sources", interplot = TRUE)
    }
  })

  output$income<- renderPlotly({
    if (input$segecon){
      bar_income(hhInput(), interplot = TRUE,
                 seg = segInput()$segmentation)
    } else {
      bar_income(hhInput(), interplot = TRUE)
    }
  })


  # Tab 6: SEGMENTATION -----------------------------
  output$inseg <- renderUI({
    output = tagList()
    if(input$segType%in%c("Dorward", "Farm orientation")){
      output[[1]] <- sliderInput('thPPP', "Threshold Income",
                                 min = 0, max = 5, value = 1, step=0.25)
      output[[2]] <- sliderInput('thSale', "Threshold % sold",
                                 min = 0, max = 100, value = 50, step=5)
      output[[3]] <-sliderInput('thOff', "Threshold % off farm",
                                min = 0, max = 100, value = 50, step=5)
    }
    if(input$segType=="Farm orientation"){
      output[[4]] <-sliderInput('thSale2', "Second threshold % sold",
                                min = 0, max = 50, value = 20, step=5)
    }
    if(input$segType%in%c("User-defined")){
      output[[length(output)+1]] <- selectInput('var1', "Variable 1",
                                                varY[varY%in%names(hhInput())])
    }
    if(input$segType=="Geography"){
      output[[1]] <- selectInput('varC', "Variable",
                                 varG[varG%in%names(hhInput())])
    }

    return(output)
  })

  output$inthvar1 <- renderUI({
    req(input$var1)
    if (input$segType=="User-defined"){
      x1 <- hhInput()[,input$var1]
      sel1 <- which(x1>min(x1, na.rm=TRUE)&x1<max(x1, na.rm=TRUE))
      q1 <- round(quantile(x1[sel1],
                           probs=c(0.1, 0.5, 0.9), na.rm=TRUE),1)
      sliderInput('thvar1', "Threshold",
                  min = q1[1], max = q1[3],
                  value = q1[2], step=diff(range(q1))/10)
    }
  })

  output$invar2 <- renderUI({
    req(input$var1)
    var2 <- varY[varY%in%names(hhInput())]
    var2 <- var2[-which(var2%in%input$var1)]
    if (input$segType=="User-defined"){
      selectInput('var2', "Variable 2",
                  c("none", var2))
    }
  })

  output$inthvar2 <- renderUI({
    req(input$var2)
    if (input$var2!="none" & input$segType=="User-defined"){
      output = tagList()
      x2 <- hhInput()[,input$var2]
      sel2 <- which(x2>min(x2, na.rm=TRUE)&x2<max(x2, na.rm=TRUE))
      q2 <- round(quantile(x2[sel2],
                           probs=c(0.1, 0.5, 0.9), na.rm=TRUE),1)
      sliderInput('thvar2', "Threshold",
                  min = q2[1], max = q2[3],
                  value = q2[2], step=diff(range(q2))/10)
    }
  })

  output$invar3 <- renderUI({
    req(input$var2)
    var3 <- varY[varY%in%names(hhInput())]
    var3 <- var3[-which(var2%in%c(input$var1, input$var2))]
    if (input$var2!="none" & input$segType=="User-defined"& length(var3)>1){
      selectInput('var3', "Variable 3",
                  c("none", var3))
    }
  })


  output$inthvar3 <- renderUI({
    req(input$var3)
    if (input$var3!="none" & input$segType=="User-defined"){
      x3 <- hhInput()[,input$var3]
      sel3 <- which(x3>min(x3, na.rm=TRUE)&x3<max(x3, na.rm=TRUE))
      q3 <- round(quantile(x3[sel3],
                           probs=c(0.1, 0.5, 0.9), na.rm=TRUE), 1)

      sliderInput('thvar3', "Threshold",
                  min = q3[1], max = q3[3],
                  value = q3[2], step=diff(range(q3))/10)
    }
  })

  segInput <- reactive({
    df <- hhInput()
    if(is.null(input$segType)){
      seg <- df$region
    } else { # req(input$segType)
      if (input$segType=="Dorward"){
        seg <- segmentation_litt(df, type = "dorward",
                                 thPPP = input$thPPP,
                                 thSale = input$thSale,
                                 thOff = input$thOff)
      }
      if (input$segType=="Farm orientation"){
        seg <- segmentation_litt(df, type = "orientation",
                                 thPPP = input$thPPP,
                                 thSale = input$thSale,
                                 thOff = input$thOff,
                                 thSale0= input$thSale2)
      }
      if (input$segType=="User-defined"){
        req(input$var2)
        abv1 <- abbreviate(gsub("_", " ", input$var1),
                           minlength = 8, method="both.sides")
        bk1 <- c(min(df[,input$var1], na.rm = TRUE), input$thvar1, max(df[,input$var1], na.rm = TRUE))
        cat1 <- cut(df[,input$var1], breaks = bk1,
                    include.lowest = TRUE,
                    labels = paste0(abv1, c("-", "+")))

        seg <- cat1
        if(input$var2!="none"){
          abv2 <- abbreviate(gsub("_", " ", input$var2),
                             minlength = 8, method="both.sides")
          bk2 <- c(min(df[,input$var2], na.rm = TRUE), input$thvar2,
                   max(df[,input$var2], na.rm = TRUE))
          cat2 <- cut(df[,input$var2], breaks = bk2,
                      include.lowest = TRUE,
                      labels = paste0(abv2, c("-", "+")))

          seg <- paste(seg, cat2)

          if(input$var3!="none"){
            abv3 <- abbreviate(gsub("_", " ", input$var3),
                               minlength = 8, method="both.sides")
            bk3 <- c(min(df[,input$var3], na.rm = TRUE), input$thvar3, max(df[,input$var3], na.rm = TRUE))
            cat3 <- cut(df[,input$var3], breaks = bk3,
                      include.lowest = TRUE,
                      labels = paste0(abv3, c("-", "+")))
            seg <- paste(seg, cat3)
          }
        }
      }
      if (input$segType=="Geography"){
        req(input$varC)
        seg <- df[,input$varC]
      }
    }
    df$segmentation <- as.factor(seg)
    return(df)
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
      plot_density(df, "income_per_person_per_day_usd", input$thPPP)
      plot_density(df, "farm_sold_perc_kg", input$thSale)
      plot_density(df, "off_farm_perc", input$thOff)
    }
    if (input$segType=="Geography"){
      tmap_ind(df, input$varC, interplot=FALSE)
    }
  })

  output$plotseg2 <- renderPlotly({
    pie_seg(segInput()$segmentation)
  })


  # Tab 7: SPATIAL -----------------------------
  # Scatter plot
  # output$scatter <- renderPlotly({
  #   if (input$zvar=="none"){
  #     return(scatterly(segInput(), input$xvar, input$yvar, z = NULL))
  #   } else {
  #     return(scatterly(segInput(), input$xvar, input$yvar, input$zvar))
  #   }
  # })

  output$mapplot <- renderPlotly({
    lymap_ind(segInput(), var = input$mapvar)
  })

})
