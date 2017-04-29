graphUiInput <- function(){
  list(
    go = actionButton("graph_go", label="", icon = icon("play", lib = "glyphicon")),
    br(),
    br(),
    object = selectInput("graph_object", label = "object", choices = getObjects("Cooccurrences", envir = .GlobalEnv)),
    reference = selectInput("graph_reference", label = "reference", choices = c("", getObjects("Cooccurrences", envir = .GlobalEnv))),
    max_rank = sliderInput("graph_max_rank", label = "max. rank", min = 10, max = 1000, value = 200),
    dim = radioButtons("graph_dim", "dimensions", choices = c("2d", "3d"), selected = "3d", inline = TRUE),
    anaglyph = conditionalPanel(
      condition = 'input.graph_dim == "3d"',
      radioButtons("graph_anaglyph_mode", "anaglyph", choices = c("true", "false"), selected = "false", inline = TRUE)
    ),
    community = radioButtons("graph_communities", label = "communities", choices = c("TRUE", "FALSE"), selected = "TRUE", inline = TRUE),
    layout = selectInput("graph_layout", label = "layout", choices = c("kamada.kawai")),
    cutoff = sliderInput("graph_cutoff", label = "min group size", min = 1, max = 100, value = 2),
    # this is a workaround for space hits
    
    time = conditionalPanel(
      condition = "input.cooccurrences_go == -1",
      selectInput("cooccurrences_time", "time", choices = Sys.time())
    )
  )
}




graphUiOutput <- function(){
}




graphServer <- function(input, output, session){
  
  observeEvent(
    input$graph_dim,
    {
      if (input$graph_dim == "2d"){
        
        domElement <- .asDOMElement(cache[["igraph"]])
        js$twoDimGraph(domElement)
        
      }
    }
  )
  
  
  observeEvent(
    input$graph_go,
    {
      
      withProgress(
        message = "Generating graph", value = 1, max = 6, detail = "... getting started",
        expr = {
          
          coocObjectOriginal <- get(input$graph_object, envir = .GlobalEnv)
          coocObject <- copy(coocObjectOriginal)
          
          # denoise
          if (TRUE){
            coocObject$drop <- list(
              c(
                polmineR::punctuation,
                unlist(noise(pAttributes(coocObject$partition, pAttribute = coocObject$pAttribute)))
              )
            )
            names(coocObject$drop) <- coocObject$pAttribute
            coocObject$trim(action = "drop", by.id = FALSE)
          }
          
          setProgress(value = 2, detail = "... apply max rank")
          maxValue <- as.integer(input$graph_max_rank)
          if (input$graph_reference != ""){
            coocObject$featureSelection(reference = get(input$graph_reference, envir = .GlobalEnv), included = TRUE, n = maxValue)
          } else {
            coocObject$dt <- coocObject$dt[1:maxValue]
          }
          
          setProgress(value = 3, detail = "... generate igraph object")
          igraphObject <- coocObject$as.igraph()
          
          if (input$graph_cutoff >= 2){
            setProgress(value = 4, detail = "... removing components")
            comps <- components(igraphObject)
            verticesToDrop <- which(comps[[1]] %in% (1:max(comps[[2]]))[which(comps[[2]] <= input$graph_cutoff)])
            igraphObject <- delete_vertices(igraphObject, verticesToDrop)
          }
          
          setProgress(value = 5, detail = "... community detection")
          igraphObject <- addCommunities(igraphObject, method = "fastgreedy", weights = FALSE)
          
          setProgress(value = 6, detail = "... layout / coordinates")
          igraphObject <- addCoordinates(igraphObject, layout = "kamada.kawai", dim = 3)
          
          values[["igraph"]] <- igraphObject
          
        }
      )
      
      if (input$graph_dim == "2d"){
        
        message("... rescaling")
        values[["igraph"]] <- rescale(igraphObject, -600, 600)
        domElement <- .asDOMElement(values[["igraph"]])
        js$twoDimGraph(domElement)
        
      } else if (input$graph_dim == "3d"){
        
        message("... 3d json")
        values[["igraph"]] <- rescale(values[["igraph"]], -400, 400)
        newJson <- .igraphToJson(values[["igraph"]])
        message("... transferring new values")
        js$reloadData(newJson)
        js$reinitialize()
        js$reanimate()
        
      }
    }
  )
  
  observeEvent(
    input$graph_node_selected,
    {
      updateTextInput(session, "cooccurrences_a", value = input$graph_node_selected)
      updateTextInput(session, "cooccurrences_b", value = "")
    }
    
  )
  
  observeEvent(
    input$graph_edge_selected_a,
    updateTextInput(session, "cooccurrences_a", value = input$graph_edge_selected_a)
  )
  
  observeEvent(
    input$graph_edge_selected_b,
    updateTextInput(session, "cooccurrences_b", value = input$graph_edge_selected_b)
  )
  
  
  observeEvent(
    input$graph_space_pressed,
    {
      # Sys.sleep(0.1) # minimal delay required so that values are transferred
      newTime <- as.character(Sys.time())
      updateSelectInput(
        session, "cooccurrences_time",
        choices = newTime,
        selected = newTime
      )
      updateNavbarPage(session, "polmineR", selected = "cooccurrences")
    }
  )
  
}
