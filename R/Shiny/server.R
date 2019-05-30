source(file = "plotData.R")
source(file = "mySample.R")
source(file = "dataSimulation.R")
source(file = "createSigma.R")
source(file = "sampleCont.R")
library("haven")

shinyServer(function(input, output,session) {

  output$logo <- renderUI({
    tags$img(src = "https://shklinkenberg.github.io/uva_style/images/logo_uva.png", width = "30%")
  })

  output$females <- renderUI({
    numericInput("gender", "Number of females", round(input$N/2, 0), 0, input$N, width = "50%")
  })
  output$males <- renderUI({
    helpText(HTML("<strong>Number of males</strong></br>&emsp;"), input$N - input$gender)
  })

## DV
  output$expecDV2x3 <- renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(input$g1),
                     fluidRow(
                       column(4,
                              numericInput("DV2x3.11", input$t1, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.12", input$t2, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.13", input$t3, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(4,
                              numericInput("DV2x3.21", input$t1, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.22", input$t2, 0)
                       ),
                       column(4,
                              numericInput("DV2x3.23", input$t3, 0)
                       )
                     )

    )
  })

  output$expecDV3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.11", input$t1, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.12", input$t2, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.21", input$t1, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.22", input$t2, 0)
                       )
                     ),
                     helpText(input$g3),
                     fluidRow(
                       column(6,
                              numericInput("DV3x2.31", input$t1, 0)
                       ),
                       column(6,
                              numericInput("DV3x2.32", input$t2, 0)
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
                     helpText(input$g1),
                     fluidRow(
                       column(4,
                              numericInput("MV2x3.11", input$t1, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.12", input$t2, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.13", input$t3, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(4,
                              numericInput("MV2x3.21", input$t1, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.22", input$t2, 0)
                       ),
                       column(4,
                              numericInput("MV2x3.23", input$t3, 0)
                       )
                     )

    )
  })

  output$expecMV3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.11", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.12", input$t2, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.21", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.22", input$t2, 0)
                       )
                     ),
                     helpText(input$g3),
                     fluidRow(
                       column(6,
                              numericInput("MV3x2.31", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MV3x2.32", input$t2, 0)
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
    lapply(1:input$lvl, function(i){
      fluidRow(
        column(6,
               textInput(paste0("nameCat.", i), paste0(i, ".", " ", "Name"), paste0("Cat", i))
        ),
        column(6,
               numericInput(paste0("pCat.", i), "Probability", 1/input$lvl, 0, 1, .1)
        )
      )
    })
  })

  ## max age
  output$maxAge <- renderUI({
    numericInput("maxAge", "Maximum age", NA, min = (input$minAge + 1))
  })

  simMatDV <- reactive({
      set.seed(input$ID)
      req(meansDV())
      mySample(N = input$N,
               mu = meansDV(),
               min = input$minDV,
               max = input$maxDV,
               Sigma = createSigma(input$design)
      )
  })

  simMatMV <- reactive({
    set.seed(input$ID + 1)
    req(meansMV())
    mySample(N = input$N,
             mu = meansMV(),
             min = input$minMV,
             max = input$maxMV,
             Sigma = createSigma(input$design)
    )
  })

  columnNamesDV <- reactive({
    paste(input$nameDV, c(input$t1, input$t2, input$t3), sep = ".")
  })

  columnNamesMV <- reactive({
    paste(input$nameMV, c(input$t1, input$t2, input$t3), sep = ".")
  })


  datDV <- reactive({
    set.seed(input$ID)
    dataRRP(dat = simMatDV(),
            N = input$N,
            design = input$design,
            names = columnNamesDV())
  })

  datMV <- reactive({
    set.seed(input$ID + 1)
    dataRRP(dat = simMatMV(),
            N = input$N,
            design = input$design,
            names = columnNamesMV())
  })

  output$plotDV <- renderPlot({
    plotData(datDV(),
             input$nameDV,
             c(input$g1, input$g2, input$g3),
             c(input$t1, input$t2, input$t3))
  })

  output$plotMV <- renderPlot({
    plotData(datMV(),
             input$nameMV,
             c(input$g1, input$g2, input$g3),
             c(input$t1, input$t2, input$t3))
  })

  observeEvent(input$next1, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Descriptives")
  })

  observeEvent(input$next2, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Dependent Variable")
  })

  observeEvent(input$next3, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Manipulation Check")
  })

  observeEvent(input$next4, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Extra Variables")
  })

  observeEvent(input$next5, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Download Data")
  })

  pCat <- reactive({
    sapply(1:input$lvl, function(i){
      input[[paste0("pCat.", i)]]
    })
  })

  nameCat <- reactive({
    sapply(1:input$lvl, function(i){
      input[[paste0("nameCat.", i)]]
    })
  })

  rawDataDV <- reactive({
    subset(datDV(), select = -Group)
  })

  dat <- reactive({
    set.seed(input$ID)
    df <- cbind(rawDataDV(), datMV())
    df$Gender <- sample(1:2, nrow(df), replace = T, prob = c(input$gender/input$N, (1-input$gender/input$N)))
    df$Age <- round(rnorm(nrow(df), input$age))
    df$Age[df$Age < input$minAge | df$Age > input$maxAge] <- mean(df$Age)
    df$Gender <- ordered(df$Gender, levels = 1:2, labels = c("F", "M"))

    attr(df$Group, "labels") <- unique(df$Group)
    names(attr(df$Group, "labels")) <- c(input$g1, input$g2)

    #df$Group <- ordered(df$Group, levels = 1:2, labels = c(input$g1, input$g2))
    if(!is.null(input$extra) && input$extra == "cat"){
      df$Cat <- sample(1:input$lvl, nrow(df), replace = T, prob = pCat())
      df$Cat <- ordered(df$Cat, levels = 1:input$lvl, labels = nameCat())
    }

    if(!is.null(input$extra) && (input$extra == "cont" || identical(input$extra, c("cat", "cont")))){
      df$Cont <- sampleCont(nrow(df), rowMeans(rawDataDV()), input$corCont, input$meanCont, input$minCont, input$maxCont)
    }

    set.seed(input$ID)
    df <- df[sample(nrow(df)), ]
    return(df)
  })

  output$table <- renderTable({
    dat()
  })

  output$test <- renderTable({
    pCat()
  })

  output$downloadData <- downloadHandler(
    filename = "dataRRP.csv",
    content = function(file) {
      write.csv(dat(), file, row.names = F)
    }
  )

  output$downloadDataSAV <- downloadHandler(
    filename = "dataRRP.sav",
    content = function(file) {
      write_sav(dat(), file)
    }
  )



})


