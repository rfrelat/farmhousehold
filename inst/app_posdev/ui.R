# Define UI for application

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("Positive deviant analysis",

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
                           uiOutput('inselmax'),
                           uiOutput('inselmin')),
                    column(6,
                           h4("3. Scale variables"),
                           checkboxInput('log', "Log-transform the right skewed variables", value = FALSE),
                           checkboxInput('out', "Remove outliers", value = FALSE))),
           fluidRow(htmlOutput("nvar")),

           fluidRow(h4("4. Explore data"),
                    tabsetPanel(
                      id = "explo",
                      tabPanel("Boxplot",plotOutput("box")),
                      tabPanel("Correlation",plotOutput("cor")))
           )
  ),
  tabPanel("Selection",
          fluidRow(h4("5. Pareto ranking"),
                   column(2,
                          selectInput('met', "Criteria",
                                      metChoices, selected="rank 1")
                   ),
                   column(10, plotOutput("pdplot"))
                   ),

          fluidRow(h4("6. Vizualisation"),
                   tabsetPanel(
                     id = "visu",

                     tabPanel("Multivariate",
                              column(2),
                              column(10,plotOutput("class"))),
                     tabPanel("Boxplot",
                              column(2, uiOutput('varbox')),
                              column(10,plotOutput("boxvar")))
                     )
          )
  )
)))
