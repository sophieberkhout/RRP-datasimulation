expectations2x3 <- function(inputGroup, inputTime, id)
  renderUI({
    conditionalPanel("input.design == '2x3'",
                     helpText(inputGroup),
                     fluidRow(
                       column(4,
                              numericInput(id[1], inputTime[1], 0)
                       ),
                       column(4,
                              numericInput(id[2], inputTime[2], 0)
                       ),
                       column(4,
                              numericInput(id[3], inputTime[3], 0)
                       )
                     ),
                     helpText(input$g2DV),
                     fluidRow(
                       column(4,
                              numericInput(id[4], inputTime[1], 0)
                       ),
                       column(4,
                              numericInput(id[5], inputTime[2], 0)
                       ),
                       column(4,
                              numericInput(id[6], inputTime[3], 0)
                       )
                     )

    )
  })
