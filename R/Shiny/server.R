source(file = "plotData.R")
source(file = "mySample.R")
source(file = "dataSimulation.R")
source(file = "createSigma.R")

shinyServer(function(input, output,session) {

  output$females <- renderUI({
    numericInput("gender", "Number of females", round(input$N/2, 0), 0, input$N)
  })
  output$males <- renderUI({
    helpText(HTML("<strong>Number of males</strong></br>&emsp;"), input$N - input$gender)
  })

  output$expec2x3 <- renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(input$g1DV),
                     fluidRow(
                       column(4,
                              numericInput("DV11", input$t1DV, 0)
                       ),
                       column(4,
                              numericInput("DV12", input$t2DV, 0)
                       ),
                       column(4,
                              numericInput("DV13", input$t3DV, 0)
                       )
                     ),
                     helpText(input$g2DV),
                     fluidRow(
                       column(4,
                              numericInput("DV21", input$t1DV, 0)
                       ),
                       column(4,
                              numericInput("DV22", input$t2DV, 0)
                       ),
                       column(4,
                              numericInput("DV23", input$t3DV, 0)
                       )
                     )

    )
  })

  output$expec3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1DV),
                     fluidRow(
                       column(6,
                              numericInput("v111", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("v112", input$t2DV, 0)
                       )
                     ),
                     helpText(input$g2DV),
                     fluidRow(
                       column(6,
                              numericInput("v113", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("v121", input$t2DV, 0)
                       )
                     ),
                     helpText(input$g3DV),
                     fluidRow(
                       column(6,
                              numericInput("v122", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("v123", input$t2DV, 0)
                       )
                     )
    )
  })


  meansDV <- reactive({
    if(input$design == "2x3"){
      c(input$DV11, input$DV12, input$DV13, input$DV21, input$DV22, input$DV23)
    } else {
      c(input$v111, input$v112, input$v113, input$v121, input$v122, input$v123)
    }
  })

  output$minDVt <- renderUI({
    numericInput("minDV", "Minimum", NA, max = (min(meansDV()) - 1))
  })
  output$maxDVt <- renderUI({
    numericInput("maxDV", "Maximum", NA, min = (max(meansDV()) + 1))
  })

  simMat <- reactive({
      set.seed(input$ID)
      mySample(N = input$N,
               mu = meansDV(),
               min = input$minDV,
               max = input$maxDV,
               Sigma = createSigma(input$design)
      )
  })

  output$test <- renderTable({
    c(input$minDV, input$maxDV, meansDV())
  })

  columnNames <- reactive({
    paste(input$nameDV, c(input$t1DV, input$t2DV, input$t3DV), sep = ".")
  })


  dat <- reactive({
    dataRRP(simMat(), input$N, input$design, input$gender, names = columnNames())
  })

  output$plot <- renderPlot({
    plotData(dat(),
             input$nameDV,
             c(input$g1DV, input$g2DV, input$g3DV),
             c(input$t1DV, input$t2DV, input$t3DV))
  })

  output$table <- renderTable({
    head(dat())
  })

})

