shinyServer(function(input, output,session) {
  output$females <- renderUI({
    numericInput("gender", "Number of females", input$N/2, 0, input$N, 5)
  })
  output$males <- renderUI({
    helpText(HTML("<strong>Number of males</strong></br>&emsp;"), input$N - input$gender)
  })
})

