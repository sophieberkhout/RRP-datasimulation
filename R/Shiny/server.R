source(file = "plotDataRRP.R")
source(file = "rrpSample.R")
source(file = "rrpAsDf.R")
source(file = "rrpSigma.R")
source(file = "sampleContRRP.R")
library(haven)

shinyServer(function(input, output,session) {
### LOGO ###
# Logo on tab one
  output$logo <- renderUI({
    tags$img(src = "https://shklinkenberg.github.io/uva_style/images/logo_uva.png",
             width = "30%")
  })
#------------------------------------------------------------------------------#
### REACTIVE UI ###
# Gender reactive UI tab two
  output$females <- renderUI({
    sliderInput("gender", "Number of females", 0, input$N, input$N/2, step = 1)
  })
  output$males <- renderUI({
    helpText(HTML("<strong>Number of males</strong></br>&emsp;"),
             input$N - input$gender)
  })
# Age reactive UI tab two
  output$maxAge <- renderUI({
    numericInput("maxAge", "Maximum age", NA, min = (input$age + 1))
  })
  output$minAge <- renderUI({
    numericInput("minAge", "Minimumn age", NA, 0, max = (input$age - 1))
  })
# DV reactive UI tab three
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
  output$minDV <- renderUI({
    numericInput("minDV", "Minimum", NA, max = (min(meansDV()) - 1))
  })
  output$maxDV <- renderUI({
    numericInput("maxDV", "Maximum", NA, min = (max(meansDV()) + 1))
  })
# MC reactive UI tab four
  output$expecMC2x3 <- renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(input$g1),
                     fluidRow(
                       column(4,
                              numericInput("MC2x3.11", input$t1, 0)
                       ),
                       column(4,
                              numericInput("MC2x3.12", input$t2, 0)
                       ),
                       column(4,
                              numericInput("MC2x3.13", input$t3, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(4,
                              numericInput("MC2x3.21", input$t1, 0)
                       ),
                       column(4,
                              numericInput("MC2x3.22", input$t2, 0)
                       ),
                       column(4,
                              numericInput("MC2x3.23", input$t3, 0)
                       )
                     )

    )
  })
  output$expecMC3x2 <- renderUI({
    conditionalPanel("input.design == '3x2'",
                     helpText(input$g1),
                     fluidRow(
                       column(6,
                              numericInput("MC3x2.11", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MC3x2.12", input$t2, 0)
                       )
                     ),
                     helpText(input$g2),
                     fluidRow(
                       column(6,
                              numericInput("MC3x2.21", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MC3x2.22", input$t2, 0)
                       )
                     ),
                     helpText(input$g3),
                     fluidRow(
                       column(6,
                              numericInput("MC3x2.31", input$t1, 0)
                       ),
                       column(6,
                              numericInput("MC3x2.32", input$t2, 0)
                       )
                     )
    )
  })
  output$minMC <- renderUI({
    numericInput("minMC", "Minimum", NA, max = (min(meansMC()) - 1))
  })
  output$maxMC <- renderUI({
    numericInput("maxMC", "Maximum", NA, min = (max(meansMC()) + 1))
  })
# Cont reactive UI tab five
  output$dir <- renderText({
    paste("Relationship with", input$nameDV)
  })
  output$maxCont <- renderUI({
    numericInput("maxCont", "Maximum", NA, min = (input$meanCont + 1))
  })
  output$minCont <- renderUI({
    numericInput("minCont", "Minimum", NA, max = (input$meanCont - 1))
  })
# Cat reactive UI tab five
  output$pCats <- renderUI({
    lapply(1:input$lvl, function(i){
      fluidRow(
        column(6,
               textInput(paste0("nameCat.", i), paste0(i, ".", " ", "Name"),
                         paste0("Cat", i))
        ),
        column(6,
               numericInput(paste0("pCat.", i), "Probability",
                            1/input$lvl, 0, 1, .1)
        )
      )
    })
  })
#------------------------------------------------------------------------------#
### StORING VARIABLES ###
# Store expected mean values in a vector
  meansDV <- reactive({
    if(input$design == "2x3"){
      c(input$DV2x3.11, input$DV2x3.12, input$DV2x3.13,
        input$DV2x3.21, input$DV2x3.22, input$DV2x3.23)
    } else {
      c(input$DV3x2.11, input$DV3x2.12, input$DV3x2.21,
        input$DV3x2.22, input$DV3x2.31, input$DV3x2.32)
    }
  })
  meansMC <- reactive({
    if(input$design == "2x3"){
      c(input$MC2x3.11, input$MC2x3.12, input$MC2x3.13,
        input$MC2x3.21, input$MC2x3.22, input$MC2x3.23)
    } else {
      c(input$MC3x2.11, input$MC3x2.12, input$MC3x2.21,
        input$MC3x2.22, input$MC3x2.31, input$MC3x2.32)
    }
  })
# Store measurent moment names in a vector
  columnNamesDV <- reactive({
    paste(input$nameDV, c(input$t1, input$t2, input$t3), sep = ".")
  })
  columnNamesMC <- reactive({
    paste(input$nameMC, c(input$t1, input$t2, input$t3), sep = ".")
  })
# Store cat probabilities in a vector
  pCat <- reactive({
    sapply(1:input$lvl, function(i){
      input[[paste0("pCat.", i)]]
    })
  })
# Store cat names in a vector
  nameCat <- reactive({
    sapply(1:input$lvl, function(i){
      input[[paste0("nameCat.", i)]]
    })
  })
#------------------------------------------------------------------------------#
### DATA SIMULATION AND RESHAPING
# Simulate data (output is matrix)
  simMatDV <- reactive({
      set.seed(input$ID)
      muDV <- meansDV()
      muMC <- meansMC()
      if(is.null(muDV)){
        muDV <- rep(0, 6)
      }
      if(is.null(muMC)){
        muMC <- rep(0, 6)
      }
      mu <- c(muDV, muMC)
      rrpSample(N = input$N,
               mu = mu,
               Sigma = rrpSigma(input$design, .7, as.numeric(input$dirCor))
      )
  })
# Reshape data in data frame with grouping variable
  datDV <- reactive({
    set.seed(input$ID)
    rrpAsDf(dat = simMatDV()[, 1:6],
            N = input$N,
            min = input$minDV,
            max = input$maxDV,
            design = input$design,
            names = columnNamesDV())
  })
  datMC <- reactive({
    set.seed(input$ID)
    rrpAsDf(dat = simMatDV()[, 7:12],
            N = input$N,
            min = input$minMC,
            max = input$maxMC,
            design = input$design,
            names = columnNamesMC())
  })
# Create subset of data without group variable
  rawDataDV <- reactive({
    subset(datDV(), select = -group)
  })
# Create data frame with all variables
  dat <- reactive({
    req(input$gender)
    set.seed(input$ID)
    df <- cbind(rawDataDV(), datMC())
  # Sample gender
    df$gender <- sample(1:2, nrow(df), replace = T,
                        prob = c(input$gender/input$N, (1-input$gender/input$N)))
    df$gender <- ordered(df$gender, levels = 1:2, labels = c("F", "M"))
  # Sample age and replace violations of min and max with mean
    df$age <- round(rnorm(nrow(df), input$age))
    df$age[df$age < input$minAge | df$age > input$maxAge] <- mean(df$age)
    df$group <- ordered(df$group, levels = 1:2, labels = c(input$g1, input$g2))
    # attr(df$group, "labels") <- unique(df$group)
    # names(attr(df$group, "labels")) <- c(input$g1, input$g2)
  # Add categorical variable if supplied
    if(!is.null(input$extra) && input$extra == "cat"){
    # Sample categorical variable
      df$cat <- sample(1:input$lvl, nrow(df), replace = T, prob = pCat())
      df$cat <- ordered(df$cat, levels = 1:input$lvl, labels = nameCat())
    }
  # Add continuous variable if supplied (uses function sampleContRRP)
    if(!is.null(input$extra) && (input$extra == "cont" ||
                                 identical(input$extra, c("cat", "cont")))){
      df$cont <- sampleContRRP(x = rowMeans(rawDataDV()),
                               cor = input$corCont,
                               mean = input$meanCont,
                               min = input$minCont,
                               max = input$maxCont)
      names(df)[names(df) == "cont"] <- input$nameCont
    }
    set.seed(input$ID)
    df <- df[sample(nrow(df)), ]
    return(df)
  })
#------------------------------------------------------------------------------#
### PLOTS ###
# Create title above plot
  output$plotTitleDV <- renderText({
    paste(input$nameDV, "per Group over Time")
  })
  output$plotTitleMC <- renderText({
    paste(input$nameMC, "per Group over Time")
  })
# Plot data
  output$plotDV <- renderPlot({
    req(length(input$minDV) > 0)
    validate(
      if(!is.na(input$minDV)){
        need(all(input$minDV < meansDV()),
             "Your minimum has to be smaller than the means!")
      },
      if(!is.na(input$maxDV)){
        need(all(input$maxDV > meansDV()),
             "Your maximum has to be larger than the means!")
      }
    )
    plotDataRRP(datDV(),
                input$nameDV,
                c(input$g1, input$g2, input$g3),
                c(input$t1, input$t2, input$t3)
    )
  })
  output$plotMC <- renderPlot({
    req(length(input$minMC) > 0)
    validate(
      if(!is.na(input$minMC)){
        need(all(input$minMC < meansMC()),
             "Your minimum has to be smaller than the means!")
      },
      if(!is.na(input$maxMC)){
        need(all(input$maxMC > meansMC()),
             "Your maximum has to be larger than the means!")
      }
    )
    plotDataRRP(datMC(),
                input$nameMC,
                c(input$g1, input$g2, input$g3),
                c(input$t1, input$t2, input$t3)
    )
  })
#------------------------------------------------------------------------------#
### NEXT BUTTONS ###
# Create next buttons
  observeEvent(input$next1, {
    updateTabsetPanel(session = session, inputId = "navbar",
                      selected = "Descriptives")
  })
  observeEvent(input$next2, {
    updateTabsetPanel(session = session, inputId = "navbar",
                      selected = "Dependent Variable")
  })
  observeEvent(input$next3, {
    updateTabsetPanel(session = session, inputId = "navbar",
                      selected = "Manipulation Check")
  })
  observeEvent(input$next4, {
    updateTabsetPanel(session = session, inputId = "navbar",
                      selected = "Extra Variables")
  })
  observeEvent(input$next5, {
    updateTabsetPanel(session = session, inputId = "navbar",
                      selected = "Download Data")
  })
#------------------------------------------------------------------------------#
### VALIDATION ###
# Validation tab one
  output$idText <- renderText({
    validate(
      need(input$ID != 1, "Fill in your student number!")
    )
  })
# Validation tab two
  output$descText <- renderText({
    req(length(input$minAge) > 0)
    validate(
      need(input$age, "Provide the mean age!"),
      if(!is.na(input$minAge)){
        need(input$minAge < input$age,
             "Your minimum has to be smaller than the mean age!")
      },
      if(!is.na(input$maxAge)){
        need(input$maxAge > input$age,
             "Your maximum has to be larger than the mean age!")
      }
    )
  })
# Validation tab five
  output$extraTextCat <- renderText({
    req(input$extra == "cat")
    validate(
      need(!any(is.na(pCat())), "Provide a probability for each category!")
    )
  })
  output$extraTextCont <- renderText({
    req(input$extra == "cont")
    req(length(input$minCont) > 0)
    validate(
      need(input$meanCont,
           paste0("Provide the mean for ", input$nameCont, "!")),
      if(!is.na(input$minCont)){
        need(input$minCont < input$meanCont,
             paste0("Your minimum has to be smaller than the mean ",
                    input$nameCont, "!"))
      },
      if(!is.na(input$maxCont)){
        need(input$maxCont > input$meanCont,
             paste0("Your maximum has to be larger than the mean ",
                    input$nameCont, "!"))
      }
    )
  })
#------------------------------------------------------------------------------#
### TAB SIX ###
# Create table tab six
  output$table <- renderTable({
    validate(
      need(input$gender, "Fill in the descriptives tab!")
    )
    dat()
  })
# Create download buttons tab six
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


