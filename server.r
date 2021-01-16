library(shiny)

shinyServer(
  function(input, output) {
    # Run whenever reset button is pressed
    observeEvent(input$action, {
      text = isolate(input$text)
      output$value <- renderText({text})
    }
    )
  }
)