shinyServer(function(input, output, session) {
  
  graphServer(input, output, session)
  kwicServer(input, output, session) # in package polmineR.shiny
  cooccurrencesServer(input, output, session)
  readServer(input, output, session)
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationX', as.numeric(input$graph_callibration_x))
  })
  
  observe({
    session$sendCustomMessage(type = 'passCallibrationY', as.numeric(input$graph_callibration_y))
  })
  
  observe({
    session$sendCustomMessage(type = 'setAnaglyphMode', input$graph_anaglyph_mode)
  })
  
  session$onSessionEnded(function() {
    funs <- c(
      "graphServer",
      "graphUiInput",
      "graphUiOutput",
      "rectifySpecialChars",
      "settingsGraphUiInput",
      "cooccurrencesServer",
      "cooccurrencesUiInput",
      "cooccurrencesUiOutput",
      "dispersionServer",
      "dispersionUiInput",
      "dispersionUiOutput",
      "jsFunctionClick",
      "kwicServer",
      "kwicUiInput",
      "kwicUiOutput",
      "partitionGadget",
      "partitionServer",
      "partitionUiInput",
      "partitionUiOutput",
      "readServer",
      "readUiInput",
      "readUiOutput",
      "values"
    )
    rm(list = funs, envir = .GlobalEnv)
  })
})
