shinyServer(function(input, output, session) {

  # 1 Dataset -----------------------------------
  info <- eventReactive(input$load, {
    req(input$file)

    if (grepl("rds$", tolower(input$file$datapath))){
      hhdb <- readRDS(input$file$datapath)
      hhinfo <- hhdb$hhinfo
    } else {
      hhinfo <- read.csv(input$file$datapath)
    }
    # row.names(hhinfo) <- hhinfo$hhid
    if(!"region"%in%names(hhinfo) & "adm1"%in% names(hhinfo)){
      hhinfo$region <- hhinfo$adm1
    }

    row.names(hhinfo) <- 1:nrow(hhinfo)
    sc1 <- c("none", scaleChoices[scaleChoices%in%names(hhinfo)])
    updateSelectInput(session, "scale1","Subsetting:",
                      choices = sc1)

    if (sum(varChoices %in% names(hhinfo))<10){
      vc1 <- names(hhinfo)
      sl1 <- vc1[2:min(c(5, length(vc1)))]
    } else {
      vc1 <- varChoices[varChoices%in%names(hhinfo)]
      sl1 <- varDefault[varDefault%in%names(hhinfo)]
    }
    updateSelectInput(session, "select","Options:",
                      choices = vc1, selected = sl1)
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


  output$nhh <- renderText({
    paste("There are N=", nrow(hhInput()), "households.")
  })

  # 2&3 Variables -----------------------------------
  tabInput <- reactive({
    tabS <- hhInput()[,input$select]
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

  # 5 Multivariate ------------------------------
  output$xaxis <- renderUI({
    scale <- paste0("PC", 1:input$npc)
    selectInput('xpc', "x-axis", scale, selected="PC1")
  })

  output$yaxis <- renderUI({
    scale <- paste0("PC", 1:input$npc)
    selectInput('ypc', "y-axis", scale, selected="PC2")
  })

  output$grp <- renderUI({
    sc1 <- as.character(scaleChoices)
    sc1 <- sc1[sc1 %in% names(hhInput())]
    selectInput('fac', "grouped",
                c("no", "cluster", sc1), selected="no")
  })

  pcaInput <- reactive({
    tabS <- tabInput()
    pca1 <- dudi.pca(tabS, scannf = FALSE, nf = input$npc)
    return(pca1)
  })

  output$corcircle <- renderPlot({
    s.corcircle(pcaInput()$co, clabel = 1,
                xax = as.numeric(gsub("PC","",input$xpc)),
                yax = as.numeric(gsub("PC","",input$ypc)))
    add.scatter.eig(pcaInput()$eig, nf = input$npc, ratio = 0.2,
                    xax = as.numeric(gsub("PC","",input$xpc)),
                    yax = as.numeric(gsub("PC","",input$ypc)))
  })

  output$class <- renderPlot({
    if(input$fac=="no"){
      s.label(pcaInput()$li, pch=16,
              clabel = 0, cpoint=1,
              boxes=FALSE,
              xax = as.numeric(gsub("PC","",input$xpc)),
              yax = as.numeric(gsub("PC","",input$ypc)))
    } else {
      s.class(pcaInput()$li, fac = cluInput()[,input$fac],
              xax = as.numeric(gsub("PC","",input$xpc)),
              yax = as.numeric(gsub("PC","",input$ypc)))
    }
  })

  output$pcinfo <- renderText({
    pca1 <- pcaInput()
    toteig <- sum(pca1$eig)/100

    txt <- paste("Selected PC axis explain", round(sum(pca1$eig[1:input$npc])/toteig), "% of variations (",
          input$xpc, round(pca1$eig[as.numeric(gsub("PC","",input$xpc))]/toteig), "%, ",
          input$ypc, round(pca1$eig[as.numeric(gsub("PC","",input$ypc))]/toteig), "%)")
    return(txt)
  })

  # 6 Cluster ------------------------------

  output$varbox <- renderUI({
    selectInput('varY', "Variable", input$select)
  })

  cluInput <- reactive({
    pca1 <- pcaInput()
    if(input$clust=="Hierarchical"){
      hc <- hclust(dist(pca1$li), method = "ward.D2")
      cluster <- cutree(hc, k=input$nclu)
    } else {
      cluster <- kmeans(pca1$li, centers = input$nclu, nstart = 50)$cluster
    }
    tab1 <- hhInput()
    sc1 <- as.character(scaleChoices)
    sc1 <- sc1[sc1 %in% names(tab1)]
    fac1 <- tab1[match(row.names(pca1$li), row.names(tab1)), sc1]

    out <- data.frame(
      "cluster"=as.factor(cluster),
      fac1
    )
    out <- as.data.frame(lapply(out, as.factor))
    colnames(out) <- c("cluster", sc1)
    return(out)
  })

  output$numclu <- renderPlot({
    pca1 <- pcaInput()
    pcol <- rep("black", 20)
    pcol[input$nclu] <- "red"

    if(input$clust=="Hierarchical"){
      hc <- hclust(dist(pca1$li), method = "ward.D2")
      plot(sort(hc$height, decreasing = TRUE)[1:20],
          type="b", pch=16, col=pcol,
          xlab="Number of clusters", ylab="Height")
    } else {
      wss <- sapply(1:20, function(x) kmeans(pca1$li, x)$tot.withinss)
      plot(1:20, wss,
           type="b", pch=16, col=pcol,
           xlab="Number of clusters", ylab="Within sum of square")
    }
  })

  output$boxvar <- renderPlot({
    boxplot(tabInput()[,input$varY]~cluInput()$cluster,
            ylab=input$varY, xlab="cluster")
  })

  # 7 Download ------------------------
  output$dwldlData <- downloadHandler(
    filename = function() {
      "Household_Data.csv"
    },
    content = function(file) {
      write.csv(hhInput(), file, row.names = TRUE)
    }
  )

  output$dwldlScript <- downloadHandler(
    filename = function() {
      "Typology_Script.R"
    },
    content <- function(file) {
      file.copy("Typology_script.R", file)
    },
    contentType = "application/zip"
  )
})
