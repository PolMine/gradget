library(shiny)
library(polmineR)
library(magrittr)
library(DT)


shinyServer(function(input, output, session) {
  graphServer(input, output, session)
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationX', as.numeric(input$graph_callibration_x))
  })
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationY', as.numeric(input$graph_callibration_y))
  })
  
  
})
