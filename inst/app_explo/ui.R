# Define UI for application

# Set the input list

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("Household data exploration",

  tabPanel("Dataset",
           fluidRow(column(3,
                     fileInput('file', 'Choose dataset',
                               accept = c('.rda','.Rdata','.rds')),
                     actionButton("load", "Load data"),
                     selectInput("scale1", h5("Subsetting 1:"),
                                choices = scaleChoices,
                                selected = "Country"),
                     uiOutput('insub1'),
                     uiOutput('inscale2'),
                     uiOutput('insub2')),
                    column(9,
                     withSpinner(tmapOutput('tmapdata'), type=4),
                     textOutput("nhh"))
                    )
    ),
    tabPanel("Crop",
             # fluidRow(
             #   column(3, checkboxInput("segcrop", "Segmentation",
             #                        value = FALSE))#,
             #   #column(9, tableOutput('infocrop'))),
             # ),
             fluidRow(
               column(6,
                      h5("Distribution of land cultivated"),
                      withSpinner(plotlyOutput('distha'), type=4)),
               column(6,
                      h5("Crop popularity"),
                      withSpinner(plotlyOutput('popcrop'), type=4)))
    ),
    tabPanel("Livestock",
           # fluidRow(
           #   column(3, checkboxInput("seglstk", "Segmentation",
           #                           value = FALSE))#,
           #   #column(9, tableOutput('infolstk'))
           #   ),
           fluidRow(
             column(6, h5("Distribution of herd size"),
                    withSpinner(plotlyOutput('disttlu'), type=4)),
             column(6, h5("Livestock popularity"),
                    withSpinner(plotlyOutput('poplstk'), type=4)))
    ),
    tabPanel("Food Security",
           # fluidRow(
           #   column(3, checkboxInput("segfs", "Segmentation",
           #                           value = FALSE))#,
           #   #column(9, tableOutput('infofs'))
           #   ),
           fluidRow(
             column(4, h5("Months with food shortage"),
                    withSpinner(plotlyOutput('monthfi'), type=4)),
             # column(4, h5("Household Food Insecurity Access Scale (HFIAS)"),
             #           withSpinner(plotlyOutput('disthfias'), type=4)),
             column(8, h5("Household Dietary Diversity Score (HDDS)"),
                    withSpinner(plotlyOutput('hdds'), type=4)))
    ),
     tabPanel("Economic",
           # fluidRow(
           #   column(3, checkboxInput("segecon", "Segmentation",
           #                           value = FALSE))#,
           #   #column(9, tableOutput('infoecon'))),
           # ),
           fluidRow(
             column(5,h5("Diversity of income sources"),
                    withSpinner(plotlyOutput('divincome'), type=4)),
             column(7, h5("Distribution of value of activities"),
                    withSpinner(plotlyOutput('income'), type=4)))
    ),
    # tabPanel("Segmentation",
    #          fluidRow(column(3,
    #                          selectInput("segType", h5("Segmentation:"),
    #                                      choices = c("Dorward", "Farm orientation", "User-defined", "Data-driven"),
    #                                      selected = "Dorward"),
    #                          uiOutput('inseg'),
    #                          uiOutput('inthvar1'),
    #                          uiOutput('invar2'),
    #                          uiOutput('inthvar2'),
    #                          uiOutput('invar3'),
    #                          uiOutput('inthvar3')),
    #                   column(5, plotOutput("plotseg1")),
    #                   column(4, plotlyOutput("plotseg2"))
    #                   )
    #          ),
    # tabPanel("Exploration",
    #          fluidRow(column(3,
    #                 selectInput("xvar", h5("Explanatory variable (x):"),
    #                             choices = varX,
    #                             selected = "farm_diversity"),
    #                 selectInput("yvar", h5("Response variable (y):"),
    #                             choices = varY,
    #                             selected = "hdds_bad_season"),
    #                 selectInput("zvar", h5("Explanatory variable 2 (z):"),
    #                             choices = varZ,
    #                             selected = NULL)),
    #          column(9,
    #                 withSpinner(plotlyOutput("scatter"), type=4)))
    #          ),
    #
    # tabPanel("Spatial",
    #          fluidRow(column(3,selectInput("mapvar", h5("Variable:"),
    #                                        choices = varX,
    #                                        selected = "hdds_bad_season")),
    #                   column(9,
    #                          withSpinner(plotlyOutput("mapplot"), type=4)))
    # )
    )
  )
)


