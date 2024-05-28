# Define UI for application

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("Farm household typology",

  tabPanel("Dataset",
           fluidRow(h4("1. Select dataset"),
                    column(6,
                           fileInput('file', 'Choose dataset',
                                     accept = c('.csv','.rds', '.RDS')),
                           actionButton("load", "Load data")),
                    column(6,
                           selectInput("scale1", "Subsetting:",
                                       choices = scaleChoices,
                                       selected = "Country"),
                           uiOutput('insub1'))
                    ),
           fluidRow(textOutput("nhh")),

           fluidRow(column(6,
                           h4("2. Select variables"),
                           selectInput('select', 'Options', varChoices,
                                multiple=TRUE, selectize=TRUE, selected = varDefault)),
                    column(6,
                           h4("3. Scale variables"),
                           checkboxInput('log', "Log-transform the right skewed variables", value = TRUE),
                           checkboxInput('out', "Remove outliers", value = FALSE))),
           fluidRow(htmlOutput("nvar")),

           fluidRow(h4("4. Explore data"),
                    tabsetPanel(
                      id = "explo",
                      tabPanel("Boxplot",plotOutput("box")),
                      tabPanel("Correlation",plotOutput("cor")))
           )
  #export dataset
  ),
  tabPanel("Analysis",
           fluidRow(h4("5. Multivariate analysis"),
                    column(2,
                           sliderInput("npc", label = "Number of PC",
                                       min = 2, max = 10, value = 3, step = 1),
                           uiOutput('xaxis'),
                           uiOutput('yaxis'),
                           uiOutput('grp')
                           ),
                    column(5, plotOutput("corcircle")),
                    column(5, plotOutput("class"))),

           fluidRow(textOutput("pcinfo")),

           fluidRow(h4("6. Cluster analysis"),
                    column(2,
                           selectInput('clust', "Clustering",
                                       c("Hierarchical", "K-means")),
                           sliderInput("nclu", label = "Number of clusters",
                                       min = 2, max = 10, value = 3, step = 1),
                           uiOutput('varbox')
                    ),
                    column(5, plotOutput("numclu")),
                    column(5, plotOutput("boxvar"))),

           # export data and script
           fluidRow(
             column(4),
             column(8, downloadButton("dwldlData", "Download Data"),
                    downloadButton("dwldlScript", "Download Script"))
           )

  )

)))
