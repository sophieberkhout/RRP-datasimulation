library(shiny)

navbarPage("", id = "navbar",
        tabPanel("Hello",
          sidebarLayout(
            sidebarPanel(
              h3("Research and Report Practical"),
              h4("Data Simulation"),
              fluidRow(column(12, offset = 8, actionButton("next1", "Next")))
            ),
            mainPanel(

            )
          )
        ),
        tabPanel("Start",
          sidebarLayout(
            sidebarPanel(
                 numericInput("ID", "Student Number", 0),
                 sliderInput("N", "Sample Size per Group", 30, 70, 50),
                 numericInput("age", "Mean age", 21, 0),
                 fluidRow(
                   column(6,
                          numericInput("minAge", "Minimumn age", NA, 0)
                   ),
                   column(6,
                          uiOutput("maxAge")
                   )
                 ),
                 uiOutput("females"),
                 uiOutput("males"),
                 selectInput("design", "Design", c("2 independent 3 repeated" = "2x3",
                                                   "3 independent 2 repeated" = "3x2")
                 ),
                 fluidRow(column(12, offset = 8, actionButton("next2", "Next")))
            ),
            mainPanel(

            )
        )
      ),
      tabPanel("Dependent",
               sidebarLayout(
        sidebarPanel(
                 h4("Dependent Variable"),
                 textInput("nameDV", "Variable name"),
                 helpText("Measurement moment"),
                 fluidRow(
                   column(4,
                          textInput("t1DV", "First", "1")
                   ),
                   column(4,
                          textInput("t2DV", "Second", "2")
                   ),
                   column(4,
                          conditionalPanel("input.design == '2x3'",
                                           textInput("t3DV", "Last", "3")
                          )
                   )
                 ),
                 helpText("Group names"),
                 fluidRow(
                   column(4,
                          textInput("g1DV", "Group 1", "Control")
                   ),
                   column(4,
                          textInput("g2DV", "Group 2", "Treatment")
                   ),
                   column(4,
                          conditionalPanel("input.design == '3x2'",
                                           textInput("g3DV", "Group 3", "Placebo")
                          )
                   )
                 ),
                 h4("Expectations"),
                 uiOutput("expecDV2x3"),
                 uiOutput("expecDV3x2"),
                 h4("Restrictions"),
                 fluidRow(
                   column(6,
                          uiOutput("minDV")
                   ),
                   column(6,
                          uiOutput("maxDV")
                   )
                 ),
                 fluidRow(column(12, offset = 8, actionButton("next3", "Next")))
        ),
        mainPanel(
          plotOutput("plotDV", width = "500px", height = "350px")
        )
      )
      ),
        tabPanel("Manipulation",
                 sidebarLayout(
          sidebarPanel(
                 h4("Manipulation Variable"),
                 textInput("nameMV", "Variable name"),
                 helpText("Measurement moment"),
                 fluidRow(
                   column(4,
                          textInput("t1MV", "First", "1")
                   ),
                   column(4,
                          textInput("t2MV", "Second", "2")
                   ),
                   column(4,
                          conditionalPanel("input.design == '2x3'",
                                           textInput("t3MV", "Last", "3")
                          )
                   )
                 ),
                 helpText("Group names"),
                 fluidRow(
                   column(4,
                          textInput("g1MV", "Group 1", "Control")
                   ),
                   column(4,
                          textInput("g2MV", "Group 2", "Treatment")
                   ),
                   column(4,
                          conditionalPanel("input.design == '3x2'",
                                           textInput("g3MV", "Group 3", "Placebo")
                          )
                   )
                 ),
                 h4("Expectations"),
                 uiOutput("expecMV2x3"),
                 uiOutput("expecMV3x2"),
                 h4("Restrictions"),
                 fluidRow(
                   column(6,
                          uiOutput("minMV")
                   ),
                   column(6,
                          uiOutput("maxMV")
                   )
                 ),
                 fluidRow(column(12, offset = 8, actionButton("next4", "Next")))
        ),
        mainPanel(plotOutput("plotMV", width = "500px", height = "350px"),
                  tableOutput("test"),
                  tableOutput("table")
        )
      )
      ),
        tabPanel("Extra",
                 sidebarLayout(
          sidebarPanel(
                 checkboxGroupInput("extra", "Extra Variables",
                                    choices = list("Categorical" = "cat", "Continuous" = "cont")
                 ),
                 conditionalPanel("input.extra.indexOf('cat') > -1",
                                  numericInput("lvl", "Number of categories", 2, min = 2, width = "50%"),
                                  radioButtons("catDif", "Probability the same?",
                                               choices = list("Same" = "same", "Different" = "different")
                                  ),
                                  fluidRow(
                                    uiOutput("pCats"),
                                    uiOutput("pCats1"),
                                    uiOutput("pCats2"),
                                    uiOutput("pCats3")
                                  )
                 ),
                 conditionalPanel("input.extra.indexOf('cont') > -1",
                                  textInput("nameCat", "Variable name"),
                                  fluidRow(
                                    column(6,
                                           numericInput("minCont", "Minimum", NA)
                                    ),
                                    column(6,
                                           uiOutput("maxCont")
                                    )
                                  )

                 ),
                 downloadButton('downloadData', 'Download'),
                 downloadButton('downloadDataSAV', 'Download SAV')
          ),
          mainPanel(

          )
      )
    )
)



