#' Building blocks for shiny apps and widgets.
#' 
#' @param input input
#' @param output output
#' @param session session
#' @importFrom DT renderDataTable dataTableOutput
#' @export graphUiInput
graphUiInput <- function(){
  list(
    go = actionButton("graph_go", label="", icon = icon("play", lib = "glyphicon")),
    br(),
    br(),
    callibration_x = sliderInput("graph_callibration_x", "x callibration", min = 0, max = 1, value = 0.68, step = 0.01),
    callibration_y = sliderInput("graph_callibration_y", "y callibration", min = 0, max = 1, value = 0.4, step = 0.01),
    object = selectInput("graph_object", label = "object", choices = getObjects("cooccurrences", envir = .GlobalEnv)),
    max_rank = sliderInput("graph_max_rank", label = "max. rank", min = 10, max = 1000, value = 200),
    dim = radioButtons("graph_dim", "dimensions", choices = c("2d", "3d"), selected = "3d", inline = TRUE),
    community = radioButtons("graph_communities", label = "communities", choices = c("TRUE", "FALSE"), selected = "TRUE", inline = TRUE),
    layout = selectInput("graph_layout", label = "layout", choices = c("kamada.kawai"))
    
    
  )
}


#' @export graphUiInput
graphUiOutput <- function(){
  
}


#' @export graphServer
graphServer <- function(input, output, session){

  observeEvent(
    input$graph_go,
    if (input$graph_go > 0){
      coocObject <- get(input$graph_object, envir = .GlobalEnv)
      
      message("... trimming object / applying max_rank")
      maxValue <- as.integer(input$graph_max_rank)
      print(maxValue)
      print(nrow(coocObject@stat))
      coocObject@stat <- coocObject@stat[which(coocObject[["rank_ll"]] <= maxValue)]
      print(nrow(coocObject@stat))
      
      message("... as igraph")
      igraphObject <- asIgraph(coocObject)
      
      message("... community detection")
      igraphObject <- enrich(igraphObject, community = list(method = "fastgreedy", weights=FALSE))
      
      message("... layout / coordinates")
      igraphObject <- enrich(igraphObject, layout = "kamada.kawai", dim = 3)
      
      message("... rescaling")
      igraphObject <- three::rescale(igraphObject, -400, 400)
      
      print(length(V(igraphObject)))
      
      message("... three dimensions")
      threeObject <- polmineR.graph::as.three(
        igraphObject, bgColor = "0xcccccc", fontSize = 12, fontColor = "0x000000", nodeSize = 4,
        edgeColor = "0xeeeeee", edgeWidth = 3, fontOffset = c(x = 10, y = 10, z = 10)
        )
      
      message("... creating json")
      newJson <- paste(
        unlist(lapply(
          names(threeObject@json),
          function(name){ paste(name, " = ", threeObject@json[[name]], ";", sep="") })
        ),
        collapse = "\n"
      )
      
      message("... transferring new values")
      js$reloadData(newJson)
      js$reinitialize()
      
      return(NULL)
      
    } else {
      threeObject <- get(input$three, env = .GlobalEnv)
      json <- paste(
        unlist(lapply(
          names(threeObject@json),
          function(name){ paste(name, " = ", threeObject@json[[name]], ";", sep="") })
        ),
        collapse = "\n"
      )
      # cat(json, file = "//Users/blaette/Lab/tmp/three/foo.json")
      json <- paste(scan("/Users/blaette/Lab/tmp/three/foo.json", what  = "character", sep = "\n"), sep = "\n", collapse = "\n")
      print("foo")
      js$reset("a = 'something'")
    }
  )
  
  output$three <- renderUI({
  })
  
  observeEvent(
    input$node,
    {
      updateTextInput(session, "foo", value = input$node)
    }
  )
  
}

