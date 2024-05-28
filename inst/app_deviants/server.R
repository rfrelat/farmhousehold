shinyServer(function(input, output, session) {

  # 1 Dataset -----------------------------------
  info <- eventReactive(input$load, {
    req(input$file)

    if (grepl("rds$", tolower(input$file$datapath))){
      hhdb <- readRDS(input$file$datapath)
      hhinfo <- hhdb$hhinfo
    } else {
      hhinfo <- read.csv(input$file$datapath)
      if ("X" %in% names(hhinfo)[1]){
        row.names(hhinfo) <- hhinfo[,1]
        hhinfo <- hhinfo[,-1]
      }
    }
    # region is sometime called adm1 ...
    if(!"region"%in%names(hhinfo) & "adm1"%in% names(hhinfo)){
      hhinfo$region <- hhinfo$adm1
    }
    #row.names(hhinfo) <- 1:nrow(hhinfo)
    sc1 <- c("none", scaleChoices[scaleChoices%in%names(hhinfo)])
    updateSelectInput(session, "scale1","Subsetting:",
                      choices = sc1)

    return(hhinfo)
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

  hhInput <- reactive({
    req(input$scale1)
    hhinfo <- info()
    if (input$scale1 != "none"){
      sel <- hhinfo[,input$scale1]==input$sub1
    } else {
      sel <- rep(TRUE, nrow(hhinfo))
    }
    return(hhinfo[sel,])
  })

  output$inselmax <- renderUI({
    tabS <- hhInput()
    # need numeric variable
    sel <- unlist(lapply(tabS, is.numeric))
    # and with some variance
    var1 <- names(sel)[sel][apply(tabS[,sel],2,sd, na.rm=TRUE)>0.00001]
    selectInput('selmax', 'To be maximized', var1,
                multiple=TRUE, selectize=TRUE)
  })

  output$inselmin <- renderUI({
    tabS <- hhInput()
    sel <- unlist(lapply(tabS, is.numeric))
    # and with some variance
    var2 <- names(sel)[sel][apply(tabS[,sel],2,sd, na.rm=TRUE)>0.00001]
    # which are npt listed in maximized variable
    var2 <- var2[!var2%in%input$selmax]
    selectInput('selmin', 'To be minimized', var2,
                multiple=TRUE, selectize=TRUE, selected=input$selmin)
  })

  output$nhh <- renderText({
    paste("There are N=", nrow(hhInput()), "households.")
  })

  # 2&3 Variables -----------------------------------
  tabInput <- reactive({
    tabS <- hhInput()[,c(input$selmax, input$selmin)]
    tabS <- tabS[complete.cases(tabS),]
    if (input$log){
      # compute skewness
      sk <- apply(tabS, 2, moments::skewness, na.rm=TRUE)
      if (sum(abs(sk)>5)>0)
        for (i in which(abs(sk)>5))
          tabS[,i] <- log(tabS[,i]+1)
    }
    if (input$out){
      # compute iqr
      m <- apply(tabS, 2, median, na.rm=TRUE)
      iq <- apply(tabS, 2, IQR, na.rm=TRUE)
      maxout <- m+3*iq
      vardif <- t(t(tabS)-maxout)
      outlier <- apply(vardif>0,1,sum)>0
      tabS <- tabS[!outlier,]
    }
    return(tabS)
  })

  output$nvar <- renderUI({
    tabS <- hhInput()[,input$select]
    na <- sum(!complete.cases(tabS))
    txt <- paste(na, "households with incomplete information were discarded.")
    tabS <- tabS[complete.cases(tabS),]
    if(input$log){
      # compute skewness
      sk <- apply(tabS, 2, moments::skewness, na.rm=TRUE)
      if (sum(abs(sk)>5)>0){
        txt <- c(txt, paste("Variables log transformed: ", paste(names(sk)[abs(sk)>5], collapse = ", ")))
      }
    }
    tab2 <- tabInput()
    if(input$out){
      nout <- nrow(tabS)-nrow(tab2)
      if (nout>0){
        txt <- c(txt, paste(nout, "outliers removed"))
      }
    }
    txt <- c(txt, paste("Final dataset: ", nrow(tab2), "households and ", ncol(tab2), "variables"))
    HTML(paste(txt, collapse = '<br/>'))
  })

  # 4 Explore -----------------------------------
  output$box <- renderPlot({
    boxplot(scale(tabInput()), ylab="Scaled variable")
  })

  output$cor <- renderPlot({
    tabS <- tabInput()
    names(tabS) <- gsub("_", "\n",names(tabS))
    corrplot.mixed(cor(tabS, use = "complete.obs", method="spearman"),
                   lower = "ellipse",
                   upper = "number",
                   tl.col = "black",
                   mar = c(1,1,1,1))
  })

  # 5 Positive deviant analysis -----------------------------------
  output$pdplot <- renderPlot({
    tabS <- tabInput()
    maxvar <- names(tabS) %in% input$selmax
    posdevplot(tabS, maxvar, input$met)
  })

  # 6 Visualization ------------------------------
  pdInput <- reactive({
    tabS <- tabInput()
    maxvar <- names(tabS) %in% input$selmax
    pr<-pareto_rank(tabS,maximise = maxvar)
    # Calculate median values per variable
    medvar <- apply(tabS, 2, median)
    # Calculate the difference between household and the median
    vardif <- t(t(tabS)-medvar)
    # Number of variables with 'better' than median
    better <- rowSums(as.matrix(vardif[,maxvar]>=0)) + rowSums(as.matrix(vardif[,!maxvar]>=0))

    if (input$met=="pr1"){
      pd <- pr==1
    } else if (input$met=="prbt12"){
      pd <- pr<=2 & better >=ncol(tabS)-1
    } else{
      pd <- pr==2 & better ==ncol(tabS) | pr==1 & better >=ncol(tabS)-1
    }

    pd <- factor(pd, labels = c("normal", "deviants"))

    return(pd)
  })


  output$class <- renderPlot({
    tabS <- tabInput()
    pca1 <- dudi.pca(tabS, scannf = FALSE, nf = 3)
    s.class(pca1$li, fac = pdInput(), col = c("black", "red"))
    scatterutil.eti(pca1$co[,1]*max(pca1$li)*0.8, pca1$co[,2]*max(pca1$li)*0.8,
                    row.names(pca1$co), clabel=0.8, boxes=TRUE,
                    coul = rep("grey40", ncol(tabS)))
  })

  output$varbox <- renderUI({
    selectInput('varY', "Variable", c(input$selmax, input$selmin))
  })

  output$boxvar <- renderPlot({
    boxplot(tabInput()[,input$varY]~pdInput(),
            ylab=input$varY, xlab="")
  })

})
