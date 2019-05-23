library(shiny)
library(shinydashboard)

dashboardPage(skin = "black",
  dashboardHeader(title = "Data Simulation for Research and Report Practical",
                  titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 5,
      tabBox(width = 12, id = "sidePanel",
        tabPanel("Descriptives",
                 numericInput("ID", "Student Number", 0),
                 sliderInput("N", "Sample Size per Group", 30, 70, 50),
                 uiOutput("females"),
                 uiOutput("males"),
                 selectInput("design", "Design", c("2 independent 3 repeated measures" = "2x3",
                                                   "3 independent 2 repeated measures" = "3x2")
                 ),
                 h4("Dependent Variable"),
                 textInput("v1", "Variable name"),
                 fluidRow(
                   column(6,
                    numericInput("v1Min", "Min", -1)
                   ),
                   column(6,
                    numericInput("v1Max", "Max", 1)
                   )
                 )
        ),
        tabPanel("Manipulation",
                 h5("Fill in this form for your manipulation check variable."),
                 textInput("v1", "Variable name"),
                 numericInput("v1Min", "Min", 0),
                 numericInput("v1Max", "Max", 0)
        ),
        tabPanel("Extra",
                 h5("If you want to add an extra continuous and/or categorical variable, fill in this form."),
                 textInput("v1", "Variable name"),
                 numericInput("v1Min", "Min", 0),
                 numericInput("v1Max", "Max", 0)
        )
      ),
      box(
        width = 5,
        actionButton("submit", "Submit")
      )
    ),
    box(title = "Expectations",
      width = 5,
      conditionalPanel("input.design == '2x3'",
        fluidRow(
          column(4,
                 numericInput("v111", "g1t1", 0),
                 numericInput("v121", "g2t1", 0)
          ),
          column(4,
                 numericInput("v112", "g1t2", 0),
                 numericInput("v122", "g2t2", 0)
          ),
          column(4,
                 numericInput("v113", "g1t3", 0),
                 numericInput("v123", "g2t3", 0)
          ))
      ),
      conditionalPanel("input.design == '3x2'",
                       fluidRow(
                         column(6,
                                numericInput("v111t", "g1t1", 0),
                                numericInput("v121t", "g2t1", 0),
                                numericInput("v131t", "g3t1", 0)
                         ),
                         column(6,
                                numericInput("v112t", "g1t2", 0),
                                numericInput("v122t", "g2t2", 0),
                                numericInput("v132t", "g3t2", 0)
                         ))
      )
    ),
    box(width = 5,
        plotOutput("plot"),
        tableOutput("table")
    )
  )
)


