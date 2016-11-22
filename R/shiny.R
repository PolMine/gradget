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
    object = selectInput("graph_object", label = "object", choices = getObjects("cooccurrences", envir = .GlobalEnv)),
    max_rank = sliderInput("graph_max_rank", label = "max. rank", min = 10, max = 1000, value = 200),
    dim = radioButtons("graph_dim", "dimensions", choices = c("2d", "3d"), selected = "3d", inline = TRUE),
    community = radioButtons("graph_communities", label = "communities", choices = c("TRUE", "FALSE"), selected = "TRUE", inline = TRUE),
    layout = selectInput("graph_layout", label = "layout", choices = c("kamada.kawai")),
    
    # this is a workaround for space clicks
    time = conditionalPanel(
      condition = "input.cooccurrences_go == -1",
      selectInput("cooccurrences_time", "time", choices = Sys.time())
    )
  )
}

settingsUiInput <- function(){
  list(
    callibration_x = sliderInput("graph_callibration_x", "x callibration", min = 0, max = 1, value = 0.68, step = 0.01),
    callibration_y = sliderInput("graph_callibration_y", "y callibration", min = -1, max = 0, value = -0.18, step = 0.01)
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
      json <- paste(scan("/Users/blaette/Lab/tmp/three/foo.json", what  = "character", sep = "\n"), sep = "\n", collapse = "\n")
      print("foo")
      js$reset("a = 'something'")
    }
  )
  
  output$three <- renderUI({
  })
  
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


#########################
##                     ##
##   coocccurrences    ##
##                     ##
#########################

cooccurrencesUiInput <- function(){
  list(
    actionButton("cooccurrences_go", "", icon = icon("play", lib = "glyphicon")),
    br(), br(),
    selectInput("cooccurrences_name", "name", choices = getObjects("cooccurrences", envir = .GlobalEnv)),
    textInput("cooccurrences_a", "a", value = ""),
    textInput("cooccurrences_b", "b", value = ""),
    br()
  )
}


#' @rdname shiny_helper_functions
#' @export contextUiOutput
cooccurrencesUiOutput <- function(){
  list(
    DT::dataTableOutput('cooccurrences_table')
  )
}


#' @rdname shiny_helper_functions
#' @export contextServer
cooccurrencesServer <- function(input, output, session){
  
  output$cooccurrences_table <- DT::renderDataTable({
    input$cooccurrences_go
    input$cooccurrences_time
    isolate({
      if (input$cooccurrences_go > 0 && input$cooccurrences_name != ""){
        df <- get(input$cooccurrences_name, envir = .GlobalEnv)@stat
        
        if (input$cooccurrences_a != "" && input$cooccurrences_b == ""){
          df <- df[a_word == input$cooccurrences_a]
        }
        
        if (input$cooccurrences_a != "" && input$cooccurrences_b != ""){
          df <- data.table::rbindlist(list(
            df[a_word == input$cooccurrences_a][b_word == input$cooccurrences_b],
            df[a_word == input$cooccurrences_b][b_word == input$cooccurrences_a]
          ))
        }
        return(df)
      } else {
        df <- data.frame(
          word = ""[0], count_window = ""[0], count_partition = ""[0],
          exp_window = integer(), exp_partition = integer(), ll = integer(),
          rank_ll = integer()
        )
        return(df)
      }
    })
  })
  
  observeEvent(
    input$cooccurrences_table_rows_selected,
    {
      if (length(input$context_table_rows_selected) > 0){
        # ctext <- get("ctext", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        # updateTextInput(
        #   session, "kwic_neighbor",
        #   value = ctext@stat[[input$context_pAttribute[1]]][input$context_table_rows_selected]
        # )
        # if (input$kwic_object == "partition"){
        #   updateSelectInput(session, "kwic_object", selected = "partition")
        #   updateSelectInput(session, "kwic_partition", selected = input$context_partition)
        # } else if (input$kwic_object == "corpus"){
        #   updateSelectInput(session, "kwic_object", selected = "corpus")
        #   updateSelectInput(session, "kwic_corpus", selected = input$context_corpus)
        # }
        # updateTextInput(session, "kwic_query", value = input$context_query)
        # updateSelectInput(session, "kwic_left", selected = input$context_left)
        # updateSelectInput(session, "kwic_right", selected = input$context_right)
        # updateSelectInput(session, "kwic_pAttribute", selected = input$context_pAttribute)
        # updateNavbarPage(session, "polmineR", selected = "kwic")
        # Time <- as.character(Sys.time())
        # updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
}

