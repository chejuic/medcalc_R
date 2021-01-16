library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    # Set up shinyjs
    useShinyjs(),  
    # text input box
    textInput("text", label = h3("心血管事件10年風險估計 (ACC/AHA 2013 ASCVD Risk Calculator)"), value = "Enter text..."),
    # action button
    actionButton("action", label = "開始計算"),
    hr(),
    fluidRow(column(3, wellPanel(verbatimTextOutput("value"))))
  )
)

