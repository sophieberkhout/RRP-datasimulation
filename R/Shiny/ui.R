library(shiny)
library(shinythemes)

navbarPage("", id = "navbar",
           windowTitle = "Data Simulation - Research and Report Practical",
           theme = shinytheme("simplex"),
        tabPanel("Home",
          sidebarLayout(
            sidebarPanel(
              HTML('<img src="uvalogo.jpg", width = "20%">'),
              h3("Data Simulation"),
              h4("Research and Report Practical"),
              p("With this app you can simulate your own data for the writing course."),
              hr(),
              p(HTML("<strong>IMPORTANT:</strong> Do not forget to fill in your student number!")),
              numericInput("ID", "Student Number", 0),
              hr(),
              fluidRow(column(12, offset = 8, actionButton("next1", "Next")))
            ),
            mainPanel(

            )
          )
        ),
        tabPanel("Descriptives",
          sidebarLayout(
            sidebarPanel(
                 sliderInput("N", "Sample Size per Group", 30, 70, 50),
                 hr(),
                 numericInput("age", "Mean age", 21, 0, width = "50%"),
                 fluidRow(
                   column(6,
                          numericInput("minAge", "Minimumn age", NA, 0)
                   ),
                   column(6,
                          uiOutput("maxAge")
                   )
                 ),
                 hr(),
                 uiOutput("females"),
                 uiOutput("males"),
                 hr(),
                 selectInput("design", "Design", c("2 independent 3 repeated" = "2x3",
                                                   "3 independent 2 repeated" = "3x2")
                 ),
                 hr(),
                 fluidRow(column(12, offset = 8, actionButton("next2", "Next")))
            ),
            mainPanel(

            )
        )
      ),
      tabPanel("Dependent Variable",
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
                 hr(),
                 h4("Expectations"),
                 uiOutput("expecDV2x3"),
                 uiOutput("expecDV3x2"),
                 hr(),
                 h4("Restrictions"),
                 fluidRow(
                   column(6,
                          uiOutput("minDV")
                   ),
                   column(6,
                          uiOutput("maxDV")
                   )
                 ),
                 hr(),
                 fluidRow(column(12, offset = 8, actionButton("next3", "Next")))
        ),
        mainPanel(
          plotOutput("plotDV", width = "500px", height = "350px")
        )
      )
      ),
        tabPanel("Manipulation Check",
                 sidebarLayout(
          sidebarPanel(
                 h4("Manipulation Check"),
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
                 hr(),
                 h4("Expectations"),
                 uiOutput("expecMV2x3"),
                 uiOutput("expecMV3x2"),
                 h4("Restrictions"),
                 hr(),
                 fluidRow(
                   column(6,
                          uiOutput("minMV")
                   ),
                   column(6,
                          uiOutput("maxMV")
                   )
                 ),
                 hr(),
                 fluidRow(column(12, offset = 8, actionButton("next4", "Next")))
        ),
        mainPanel(
          plotOutput("plotMV", width = "500px", height = "350px")
        )
      )
      ),
        tabPanel("Extra Variables",
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
                 hr(),
                 fluidRow(column(12, offset = 8, actionButton("next5", "Next")))
          ),
          mainPanel(

          )
      )
    ),
    tabPanel("Download Data",
      sidebarLayout(
        sidebarPanel(
          downloadButton('downloadData', 'Download .CSV'),
          hr(),
          downloadButton('downloadDataSAV', 'Download .SAV')
        ),
        mainPanel(
          tableOutput("table")
        )
      )
    )
)



