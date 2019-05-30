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
                 numericInput("age", "Mean age", NA, 0, width = "50%"),
                 fluidRow(
                   column(6,
                          uiOutput("minAge")
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
              textOutput("descText")
            )
        )
      ),
      tabPanel("Dependent Variable",
               sidebarLayout(
        sidebarPanel(
                 h4("Dependent Variable"),
                 textInput("nameDV", "Variable name", "DV"),
                 helpText("Measurement moment"),
                 fluidRow(
                   column(4,
                          textInput("t1", "First", "one")
                   ),
                   column(4,
                          textInput("t2", "Second", "two")
                   ),
                   column(4,
                          conditionalPanel("input.design == '2x3'",
                                           textInput("t3", "Last", "three")
                          )
                   )
                 ),
                 helpText("Group names"),
                 fluidRow(
                   column(4,
                          textInput("g1", "Group 1", "control")
                   ),
                   column(4,
                          textInput("g2", "Group 2", "treatment")
                   ),
                   column(4,
                          conditionalPanel("input.design == '3x2'",
                                           textInput("g3", "Group 3", "Placebo")
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
          h4(textOutput("plotTitleDV")),
          plotOutput("plotDV", width = "600px", height = "400px")
        )
      )
      ),
        tabPanel("Manipulation Check",
                 sidebarLayout(
          sidebarPanel(
                 h4("Manipulation Check"),
                 textInput("nameMV", "Variable name", "MC"),
                 hr(),
                 h4(textOutput("dir")),
                 radioButtons("dirCor", "Direction",
                              choiceNames = list("Positive", "Negative"),
                              choiceValues = list(.5, -.5)),
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
          h4(textOutput("plotTitleMV")),
          plotOutput("plotMV", width = "600px", height = "400px")
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
                                  hr(),
                                  h4("Categorical Variable"),
                                  numericInput("lvl", "Number of categories", 2, min = 2, width = "50%"),
                                  hr(),
                                  uiOutput("pCats")
                 ),
                 conditionalPanel("input.extra.indexOf('cont') > -1",
                                  hr(),
                                  h4("Continuous Variable"),
                                  textInput("nameCont", "Variable name", "cont"),
                                  hr(),
                                  numericInput("meanCont", "Mean", NA, width = "50%"),
                                  fluidRow(
                                    column(6,
                                           uiOutput("minCont")
                                    ),
                                    column(6,
                                           uiOutput("maxCont")
                                    )
                                  ),
                                  hr(),
                                  helpText("Expected correlation with dependent variable (can be negative)"),
                                  numericInput("corCont", "Correlation", .5, -1, 1, .1, width = "50%")

                 ),
                 hr(),
                 fluidRow(column(12, offset = 8, actionButton("next5", "Next")))
          ),
          mainPanel(
              textOutput("extraTextCat"),
              textOutput("extraTextCont")
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



