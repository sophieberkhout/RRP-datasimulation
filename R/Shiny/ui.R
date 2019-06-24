library(shiny)
library(shinythemes)
library(shinycssloaders)

# Set global options for loader
options(spinner.color = "grey",
        spinner.size = .3,
        spinner.type = 5)


 navbarPage("", id = "navbar",
            windowTitle = "Data Simulation - Research and Report Practical",
            theme = shinytheme("simplex"),
# First tab
            tabPanel("Home",
              sidebarLayout(
                sidebarPanel(
                  HTML('<img src="uvalogo.jpg", width = "20%">'),
                  h3("Data Simulation"),
                  h4("Research and Report Practical"),
                  p("With this app you can simulate your own data for the writing course."),
                  hr(),
                  p(HTML("<strong>IMPORTANT:</strong> Do not forget to fill in your student number!")),
                  numericInput("ID", "Student Number", 1, 0),
                  hr(),
                  fluidRow(column(12, offset = 8, actionButton("next1", "Next")))
                ),
                mainPanel(
                  textOutput("idText")
                )
              )
            ),
# Second tab
            tabPanel("Descriptives",
              sidebarLayout(
                sidebarPanel(
                  h4("Sample Size"),
                  sliderInput("N", "Sample size per group", 30, 70, 50),
                  hr(),
                  h4("Gender"),
                  uiOutput("females"),
                  uiOutput("males"),
                  hr(),
                  h4("Age"),
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
                  h4("Design"),
                  helpText("What kind of measurements do you use?"),
                  selectInput("design", "Measurements",
                              c("2 independent 3 repeated" = "2x3",
                                "3 independent 2 repeated" = "3x2")
                  ),
                  hr(),
                  fluidRow(
                    column(12, offset = 8, actionButton("next2", "Next")
                    )
                  )
                ),
                mainPanel(
                  textOutput("descText")
                )
              )
            ),
# Third tab
            tabPanel("Dependent Variable",
              sidebarLayout(
                sidebarPanel(
                  h4("Dependent Variable"),
                  textInput("nameDV", "Variable name", "dv"),
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
                                           textInput("g3", "Group 3", "placebo")
                          )
                   )
                  ),
                  hr(),
                  h4("Expectations"),
                  helpText("Fill in the expected means per group and time."),
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
                  textOutput("valMinDV"),
                  textOutput("valMAxDV"),
                  withSpinner(plotOutput("plotDV"))
                )
              )
            ),
# Tab four
            tabPanel("Manipulation Check",
              sidebarLayout(
                sidebarPanel(
                  h4("Manipulation Check"),
                  textInput("nameMC", "Variable name", "mc"),
                  hr(),
                  h4(textOutput("dir")),
                  radioButtons("dirCor", "Direction of the relationship",
                               choiceNames = list("Positive", "Negative"),
                               choiceValues = list(.5, -.5)),
                  hr(),
                  h4("Expectations"),
                  helpText("Fill in the expected means per group and time."),
                  uiOutput("expecMC2x3"),
                  uiOutput("expecMC3x2"),
                  h4("Restrictions"),
                  hr(),
                  fluidRow(
                   column(6,
                          uiOutput("minMC")
                   ),
                   column(6,
                          uiOutput("maxMC")
                   )
                  ),
                  hr(),
                  fluidRow(column(12, offset = 8, actionButton("next4", "Next")))
                ),
                mainPanel(
                  h4(textOutput("plotTitleMC")),
                  withSpinner(plotOutput("plotMC"))
                )
              )
            ),
# Tab five
            tabPanel("Extra Variables",
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput("extra", h4("Optional: Extra Variables"),
                                     choices = list("Categorical" = "cat",
                                                    "Continuous" = "cont")
                  ),
                  conditionalPanel("input.extra.indexOf('cat') > -1",
                                    hr(),
                                    h4("Categorical Variable"),
                                    numericInput("lvl", "Number of categories",
                                                 2, min = 2, width = "50%"),
                                    hr(),
                                    helpText("Fill in the name of each category
                                             and their corresponding probabilities to occur."),
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
                                    helpText("Expected correlation with the
                                             dependent variable (can be negative)."),
                                    numericInput("corCont", "Correlation",
                                                 .5, -1, 1, .1, width = "50%")
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
# Tab six
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
            ),
            tabPanel("Help",
              includeMarkdown("help.Rmd"))
 )



