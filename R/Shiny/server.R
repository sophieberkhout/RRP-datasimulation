source(file = "plotData.R")
source(file = "mySample.R")
source(file = "dataSimulation.R")

shinyServer(function(input, output,session) {

  repeatable(mySample, seed = observe({input$ID}))
  repeatable(sample, seed = observe({input$ID}))
  repeatable(rnorm, seed = observe({input$ID}))


  output$females <- renderUI({
    numericInput("gender", "Number of females", input$N/2, 0, input$N, 5)
  })
  output$males <- renderUI({
    helpText(HTML("<strong>Number of males</strong></br>&emsp;"), input$N - input$gender)
  })

  simMat <- eventReactive(
    input$submit, {
      mySample(input$N,
               c(input$v111, input$v112, input$v113, input$v121, input$v122, input$v123),
               input$v1Min,
               input$v1Max,
               createSigma(input$design)
      )
  })

  dat <- reactive({
    dataRRP(simMat(), input$N, input$design, input$gender)
  })

  output$plot <- renderPlot({
    plotData(dat(), input$v1)
  })

  output$table <- renderTable({
    head(dat())
  })

})

