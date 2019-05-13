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
                 textInput("ID", "Student Number"),
                 sliderInput("N", "Sample Size", 60, 200, 130),
                 uiOutput("females"),
                 uiOutput("males"),
                 selectInput("design", "Design", c("2x3", "3x2")
                 )
        ),
        tabPanel("Manipulation",
                 h5("Fill in this form for your manipulation check variable."),
                 textInput("v1", "Variable name"),
                 numericInput("v1Min", "Min", 0),
                 numericInput("v1Max", "Max", 0)
        ),
        tabPanel("Dependent",
                 h5("Fill this in for the dependent variable."),
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
      )
    )
  )
)


