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

## DV
  output$expecDV2x3 <- renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(input$g1DV),
                     fluidRow(
                       column(4,
                              numericInput("DV2x3.11", input$t1DV, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.12", input$t2DV, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.13", input$t3DV, 0)
                       )
                     ),
                     helpText(input$g2DV),
                     fluidRow(
                       column(4,
                              numericInput("DV2x3.21", input$t1DV, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.22", input$t2DV, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.23", input$t3DV, 0)
                       )
                     )

    )
  })

  output$expecDV3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1DV),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.11", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.12", input$t2DV, 0)
                       )
                     ),
                     helpText(input$g2DV),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.21", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.22", input$t2DV, 0)
                       )
                     ),
                     helpText(input$g3DV),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.31", input$t1DV, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.32", input$t2DV, 0)
                       )
                     )
    )
  })

  meansDV <- reactive({
    if(input$design == "2x3"){
      c(input$DV2x3.11, input$DV2x3.12, input$DV2x3.13, input$DV2x3.21, input$DV2x3.22, input$DV2x3.23)
    } else {
      c(input$DV3x2.11, input$DV3x2.12, input$DV3x2.21, input$DV3x2.22, input$DV3x2.31, input$DV3x2.32)
    }
  })

  output$minDV <- renderUI({
    numericInput("minDV", "Minimum", NA, max = (min(meansDV()) - 1))
  })
  output$maxDV <- renderUI({
    numericInput("maxDV", "Maximum", NA, min = (max(meansDV()) + 1))
  })

## MV
  output$expecMV2x3 <- renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(input$g1MV),
                     fluidRow(
                       column(4,
                              numericInput("MV2x3.11", input$t1MV, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.12", input$t2MV, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.13", input$t3MV, 0)
                       )
                     ),
                     helpText(input$g2MV),
                     fluidRow(
                       column(4,
                              numericInput("MV2x3.21", input$t1MV, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.22", input$t2MV, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.23", input$t3MV, 0)
                       )
                     )

    )
  })

  output$expecMV3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1MV),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.11", input$t1MV, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.12", input$t2MV, 0)
                       )
                     ),
                     helpText(input$g2MV),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.21", input$t1MV, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.22", input$t2MV, 0)
                       )
                     ),
                     helpText(input$g3MV),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.31", input$t1MV, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.32", input$t2MV, 0)
                       )
                     )
    )
  })

  meansMV <- reactive({
    if(input$design == "2x3"){
      c(input$MV2x3.11, input$MV2x3.12, input$MV2x3.13, input$MV2x3.21, input$MV2x3.22, input$MV2x3.23)
    } else {
      c(input$MV3x2.11, input$MV3x2.12, input$MV3x2.21, input$MV3x2.22, input$MV3x2.31, input$MV3x2.32)
    }
  })

  output$minMV <- renderUI({
    numericInput("minMV", "Minimum", NA, max = (min(meansMV()) - 1))
  })
  output$maxMV <- renderUI({
    numericInput("maxMV", "Maximum", NA, min = (max(meansMV()) + 1))
  })

  ## max continuous variable
  output$maxCont <- renderUI({
    numericInput("maxCont", "Maximum", NA, min = (input$minCont + 1))
  })

  ## categorical variable
  output$pCats <- renderUI({
    conditionalPanel("input.catDif == 'same'",
                     column(4,
                            lapply(1:input$lvl, function(i){
                              numericInput(paste0("pCat", i), paste("Category", i), NA, 0, 1, .1)
                            })
                     )
    )
  })

  ## max age
  output$maxAge <- renderUI({
    numericInput("maxAge", "Maximum age", NA, min = (input$minAge + 1))
  })


  output$pCats1 <- renderUI({
    conditionalPanel("input.catDif == 'different'",
                     column(4,
                            helpText(input$g1DV),
                            lapply(1:input$lvl, function(i){
                              numericInput(paste0("pCat1", i), paste("Category", i), NA, 0, 1, .1)
                            })
                     )
    )
  })

  output$pCats2 <- renderUI({
    conditionalPanel("input.catDif == 'different'",
                     column(4,
                            helpText(input$g2DV),
                            lapply(1:input$lvl, function(i){
                              numericInput(paste0("pCat2", i), paste("Category", i), NA, 0, 1, .1)
                            })
                     )
    )
  })

  output$pCats3 <- renderUI({
    conditionalPanel("input.catDif == 'different' & input.design == '3x2'",
                     column(4,
                            helpText(input$g3DV),
                            lapply(1:input$lvl, function(i){
                              numericInput(paste0("pCat3", i), paste("Category", i), NA, 0, 1, .1)
                            })
                     )
    )
  })




  simMatDV <- reactive({
      set.seed(input$ID)
      mySample(N = input$N,
               mu = meansDV(),
               min = input$minDV,
               max = input$maxDV,
               Sigma = createSigma(input$design)
      )
  })

  simMatMV <- reactive({
    set.seed(input$ID + 1)
    mySample(N = input$N,
             mu = meansMV(),
             min = input$minMV,
             max = input$maxMV,
             Sigma = createSigma(input$design)
    )
  })

  columnNamesDV <- reactive({
    paste(input$nameDV, c(input$t1DV, input$t2DV, input$t3DV), sep = ".")
  })

  columnNamesMV <- reactive({
    paste(input$nameMV, c(input$t1MV, input$t2MV, input$t3MV), sep = ".")
  })


  datDV <- reactive({
    set.seed(input$ID)
    dataRRP(dat = simMatDV(),
            N = input$N,
            design = input$design,
            gender = input$gender,
            age = input$age,
            minAge = input$minAge,
            maxAge = input$maxAge,
            names = columnNamesDV())
  })

  datMV <- reactive({
    set.seed(input$ID + 1)
    dataRRP(dat = simMatMV(),
            N = input$N,
            design = input$design,
            gender = input$gender,
            age = input$age,
            minAge = input$minAge,
            maxAge = input$maxAge,
            names = columnNamesMV())
  })

  output$plotDV <- renderPlot({
    plotData(datDV(),
             input$nameDV,
             c(input$g1DV, input$g2DV, input$g3DV),
             c(input$t1DV, input$t2DV, input$t3DV))
  })

  output$plotMV <- renderPlot({
    plotData(datMV(),
             input$nameMV,
             c(input$g1MV, input$g2MV, input$g3MV),
             c(input$t1MV, input$t2MV, input$t3MV))
  })

  observeEvent(input$next1, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Start")
  })

  observeEvent(input$next2, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Dependent")
  })

  observeEvent(input$next3, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Manipulation")
  })

  observeEvent(input$next4, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Extra")
  })

})

