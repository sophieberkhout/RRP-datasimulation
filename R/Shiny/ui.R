library(shiny)
library(shinydashboard)

dashboardPage(skin = "black",
  dashboardHeader(title = "Data Simulation for Research and Report Practical",
                  titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 6,
      tabBox(width = 12, id = "sidePanel",
        tabPanel("Start",
                 numericInput("ID", "Student Number", 0),
                 sliderInput("N", "Sample Size per Group", 30, 70, 50),
                 uiOutput("females"),
                 uiOutput("males"),
                 selectInput("design", "Design", c("2 independent 3 repeated" = "2x3",
                                                   "3 independent 2 repeated" = "3x2")
                 )
        ),
        tabPanel("Dependent",
                 h4("Dependent Variable"),
                 textInput("nameDV", "Variable name"),
                 helpText("Measurement moment"),
                 fluidRow(
                   column(4,
                          textInput("t1DV", "First", "One")
                   ),
                   column(4,
                          textInput("t2DV", "Second", "Two")
                   ),
                   column(4,
                          conditionalPanel("input.design == '2x3'",
                                           textInput("t3DV", "Last", "Three")
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
                 uiOutput("expec2x3"),
                 uiOutput("expec3x2"),
                 h4("Restrictions"),
                 fluidRow(
                   column(6,
                          uiOutput("minDVt")
                   ),
                   column(6,
                          uiOutput("maxDVt")
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
    box(width = 6,
        plotOutput("plot"),
        tableOutput("table"),
        tableOutput("test")
    )
  )
)


