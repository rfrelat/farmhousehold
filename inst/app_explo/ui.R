# Define UI for application

# Set the input list

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("Household data exploration",

  tabPanel("Dataset",
           fluidRow(column(3,
                     fileInput('file', 'Choose farmhousehold dataset',
                               accept = c('.rds', '.RDS')),
                     actionButton("load", "Load RDS data"),
                     uiOutput('inscale1'),
                     uiOutput('insub1'),
                     uiOutput('inscale2'),
                     uiOutput('insub2'),
                     uiOutput('inmap')),

                    column(9,
                     withSpinner(tmapOutput('tmapdata'), type=4),
                     h5("Locations are approximated to the nearest 0.01 degree and jittered to safeguard the privacy of households."),
                     textOutput("nhh"))
                    )
    ),
    tabPanel("Crop",
             fluidRow(
               column(6,
                      h5("Distribution of land cultivated"),
                      withSpinner(plotlyOutput('distha'), type=4)),
               column(6,
                      h5("Crop popularity"),
                      withSpinner(plotlyOutput('popcrop'), type=4))),
             fluidRow(
               column(6, checkboxInput("segcrop", "Summary per groups of households",
                                       value = FALSE))
             )
    ),
    tabPanel("Livestock",
           fluidRow(
             column(6, h5("Distribution of herd size"),
                    withSpinner(plotlyOutput('disttlu'), type=4)),
             column(6, h5("Livestock popularity"),
                    withSpinner(plotlyOutput('poplstk'), type=4))),
           fluidRow(
             column(6, checkboxInput("seglstk", "Summary per groups of households",
                                     value = FALSE))
           )
    ),
    tabPanel("Food Security",
             fluidRow(
               tabsetPanel(
                 tabPanel("HDDS",
                          column(5, h5("Household Dietary Diversity Score (HDDS)"),
                                 withSpinner(plotlyOutput('hdds_bar'), type=4)),
                          column(7, h5(" "), withSpinner(plotlyOutput('hdds_group'), type=4))
                 ),
                 tabPanel("Food shortage",
                          column(5, h5("Months with food shortage"),
                                 withSpinner(plotlyOutput('mfi_bar'), type=4)),
                          column(7, h5(" "), withSpinner(plotlyOutput('mfi_which'), type=4)),
                 ),
                 tabPanel("FIES",
                          column(5, h5("Food Insecurity Experience Scale"),
                                 withSpinner(plotlyOutput('fies_bar'), type=4)),
                          column(7, h5(" "), withSpinner(plotlyOutput('fies_group'), type=4))
                 ))),
             fluidRow(
               column(6, checkboxInput("segfs", "Summary per groups of households",
                                       value = FALSE))
             )
    ),
     tabPanel("Economic",
           fluidRow(
             column(5,h5("Diversity of income sources"),
                    withSpinner(plotlyOutput('divincome'), type=4)),
             column(7, h5("Distribution of value of activities"),
                    withSpinner(plotlyOutput('income'), type=4))),
           fluidRow(
             column(6, checkboxInput("segecon", "Summary per groups of households",
                                     value = FALSE))
           )
    ),
    tabPanel("Segmentation",
           fluidRow(column(3,
                           selectInput("segType", h5("Segmentation:"),
                                       choices = c(segChoices),
                                       selected = "User-defined"),
                           uiOutput('inseg'),
                           uiOutput('inthvar1'),
                           uiOutput('invar2'),
                           uiOutput('inthvar2'),
                           uiOutput('invar3'),
                           uiOutput('inthvar3')),
                    column(4, plotOutput("plotseg1")),
                    column(5, plotlyOutput("plotseg2"))
           )
    )
    )
  )
)


