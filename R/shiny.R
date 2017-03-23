#' Building blocks for shiny apps and widgets.
#' 
#' @param input input
#' @param output output
#' @param session session
#' @importFrom DT renderDataTable dataTableOutput
#' @import shiny
#' @export graphUiInput
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

#' @export settingsGraphUiInput
settingsGraphUiInput <- function(){
  list(
    callibration_x = sliderInput("graph_callibration_x", "x callibration", min = 0, max = 1, value = 0.68, step = 0.01),
    callibration_y = sliderInput("graph_callibration_y", "y callibration", min = -1, max = 0, value = -0.18, step = 0.01),
    bgColcour = colourpicker::colourInput(inputId = "graph_background", label = "background")
  )
}



#' @export graphUiOutput
graphUiOutput <- function(){
}

.cooccurrencesToIgraph <- function(input, output, session){
  print(input$graph_object) # this is a Cooccurrences object!
  
  coocObject <- get(input$graph_object, envir = .GlobalEnv)$copy()
  print(dim(coocObject))
  
  if (TRUE){
    coocObject$drop <- c(polmineR::punctuation, unlist(noise(pAttributes(coocObject, pAttribute = "word"))))
    coocObject$trim(action = "drop", by.id = FALSE)
  }
  
  message("... trimming object / applying max_rank")
  maxValue <- as.integer(input$graph_max_rank)
  if (input$graph_reference != ""){
    coocObject$featureSelection(reference = get(input$graph_reference, envir = .GlobalEnv), included = TRUE, n = maxValue)
    print(dim(coocObject))
  } else {
    coocObject$dt <- coocObject$dt[1:maxValue]
  }
  
  
  message("... as igraph")
  igraphObject <- coocObject$as.igraph()
  
  if (input$graph_cutoff >= 2){
    message("... removing components")
    comps <- components(igraphObject)
    verticesToDrop <- which(comps[[1]] %in% (1:max(comps[[2]]))[which(comps[[2]] <= input$graph_cutoff)])
    igraphObject <- delete_vertices(igraphObject, verticesToDrop)
  }
  
  message("... community detection")
  igraphObject <- addCommunities(igraphObject, method = "fastgreedy", weights = FALSE)
  
  message("... layout / coordinates")
  igraphObject <- addCoordinates(igraphObject, layout = "kamada.kawai", dim = 3)
  igraphObject
}

.asDOMElement <- function(x){
  x <- rescale(x, -600, 600)
  svg <- SVG$new(x)
  svg$make(width = 800, height = 800)
  y <- svg$xml
  y <- gsub("^.*?(<svg.*?</svg>).*$", "\\1", y)
  
  jsFunctionClick <- paste(scan(
    file = system.file("js", "onclick_functions_2d.js", package = "polmineR.graph"),
    what = character(),
    sep = "\n", quiet = TRUE
  ), collapse = "\n")
  
  y <- gsub("(<svg.*?>)", paste("\\1<script>", jsFunctionClick, "</script>", sep = ""), y)
  y
}

# turn igraph object into the json that is needed by THREE
# x needs to be an igraph object
.igraphToJson <- function(x){

  message("... three dimensions")
  T <- Three$new(x)
  T$type = "anaglyph"
  T$bgColor = "0xcccccc"
  T$fontSize = 12
  T$fontColor = "0x000000"
  T$nodeSize = 4
  T$edgeColor = "0xeeeeee"
  T$edgeWidth = 3
  T$fontOffset = c(x = 10, y = 10, z = 10)
  T$make()

  message("... creating json")
  newJson <- T$as.json()
  newJson
}

#' @export graphServer
#' @export graphServer
graphServer <- function(input, output, session){

  observeEvent(
    input$graph_dim,
    {
      if (input$graph_dim == "2d"){
        
        igraphObject <- get("igraphObject", envir = get(".polmineR_graph_cache", envir = .GlobalEnv))
        domElement <- .asDOMElement(igraphObject)
        js$twoDimGraph(domElement)
        
      }
    }
  )
  
  observeEvent(
    input$graph_go,
    {
      # if (input$graph_go > 0){
        
        message("... generating new object")
        igraphObject <- .cooccurrencesToIgraph(input, output, session)
        
        # assign to cache, so that object will be available for switching 2d/3d mode
        message("... assign to .GlobalEnv")
        assign("igraphObject", igraphObject, envir = get(".polmineR_graph_cache", envir = .GlobalEnv))

        if (input$graph_dim == "2d"){
          
          message("... rescaling")
          igraphObject <- rescale(igraphObject, -600, 600)
          domElement <- .asDOMElement(igraphObject)
          js$twoDimGraph(domElement)
          
        } else if (input$graph_dim == "3d"){
          
          message("... 3d json")
          igraphObject <- rescale(igraphObject, -400, 400)
          newJson <- .igraphToJson(igraphObject)
          message("... transferring new values")
          js$reloadData(newJson)
          js$reinitialize()
          js$reanimate()
        
        }
      # }
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
      Sys.sleep(0.5) # minimal delay required so that values are transferred
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

#' @export cooccurrencesUiInput
cooccurrencesUiInput <- function(){
  list(
    actionButton("cooccurrences_go", "", icon = icon("play", lib = "glyphicon")),
    br(), br(),
    selectInput("cooccurrences_name", "name", choices = getObjects("Cooccurrences", envir = .GlobalEnv)),
    textInput("cooccurrences_a", "a", value = ""),
    textInput("cooccurrences_b", "b", value = ""),
    any = conditionalPanel(
      condition = "input.cooccurrences_go == -1",
      textInput("cooccurrences_any", "any", value = "default")
    ),
    
    br()
  )
}


#' @export cooccurrencesUiOutput
cooccurrencesUiOutput <- function(){
  list(
    DT::dataTableOutput('cooccurrences_table')
  )
}


#' @export cooccurrencesServer
cooccurrencesServer <- function(input, output, session){
  
  output$cooccurrences_table <- DT::renderDataTable({
    input$cooccurrences_go
    input$cooccurrences_time
    isolate({
      if (input$cooccurrences_name != ""){
        df <- get(input$cooccurrences_name, envir = .GlobalEnv)$dt
        
        a <- input$cooccurrences_a
        Encoding(a) <- "unknown"

        if (input$cooccurrences_a != "" && input$cooccurrences_b == ""){
          df <- df[a_word == a]
        }
        
        if (input$cooccurrences_a != "" && input$cooccurrences_b != ""){
          b <- input$cooccurrences_b
          Encoding(b) <- "unknown"

          df <- data.table::rbindlist(list(
            df[a_word == a][b_word == b],
            df[a_word == b][b_word == a]
          ))
        }
        assign("df", df, envir = get(".polmineR_graph_cache", envir = .GlobalEnv))
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
      if (length(input$cooccurrences_table_rows_selected) > 0){
        if (input$cooccurrences_a == ""){
          statTab <- get(input$cooccurrences_name, envir = .GlobalEnv)@stat
        } else {
          statTab <- get("df", envir = get(".polmineR_graph_cache", envir = .GlobalEnv))
        }
        updateSelectInput(session, "kwic_object", selected = "partition")
        updateSelectInput(session, "kwic_partition", selected = get(input$graph_object, envir = .GlobalEnv)@partition)
        updateTextInput(
          session, "kwic_query",
          value = statTab[["a_word"]][input$cooccurrences_table_rows_selected]
          )
        updateTextInput(
          session, "kwic_neighbor",
          value = statTab[["b_word"]][input$cooccurrences_table_rows_selected]
        )
        updateSelectInput(session, "kwic_left", selected = get(input$cooccurrences_name, envir = .GlobalEnv)@left)
        updateSelectInput(session, "kwic_right", selected = get(input$cooccurrences_name, envir = .GlobalEnv)@right)
        updateSelectInput(session, "kwic_pAttribute", selected = get(input$cooccurrences_name, envir = .GlobalEnv)@pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
}

