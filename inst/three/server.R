shinyServer(function(input, output, session) {
  
  graphServer(input, output, session)
  kwicServer(input, output, session) # in package polmineR.shiny
  cooccurrencesServer(input, output, session)
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationX', as.numeric(input$graph_callibration_x))
  })
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationY', as.numeric(input$graph_callibration_y))
  })
  
  
})
