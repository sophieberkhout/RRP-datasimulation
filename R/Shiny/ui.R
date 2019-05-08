library(shiny)
library(shinydashboard)

fluidPage(

  titlePanel("Data Simulation"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Form",
                 textInput("ID", "Student Number"),
                 sliderInput("N", "Sample Size", 60, 200, 130),
                 uiOutput("females"),
                 uiOutput("males"),
                 selectInput("design", "Design",
                             c("2x3",
                               "3x2")
                 )
         ),
         tabPanel("Man",
                h5("Manipulation check"),
                 textInput("v1", "Variable name"),
                 numericInput("v1Min", "Min", 0),
                 numericInput("v1Max", "Max", 0)
         )
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data"),
        tabPanel("Help")
      )
    )
  )
)
