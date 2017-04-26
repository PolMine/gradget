#' Building blocks for shiny apps and widgets.
#' 
#' @param input input
#' @param output output
#' @param session session
#' @importFrom DT renderDataTable dataTableOutput
#' @import shiny
#' @export graphUiInput
#' @rdname shinyGraphBuildingBlocks
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
#' @rdname shinyGraphBuildingBlocks
settingsGraphUiInput <- function(){
  list(
    callibration_x = sliderInput("graph_callibration_x", "x callibration", min = 0, max = 1, value = 0.68, step = 0.01),
    callibration_y = sliderInput("graph_callibration_y", "y callibration", min = -1, max = 0, value = -0.18, step = 0.01),
    bgColcour = colourpicker::colourInput(inputId = "graph_background", label = "background")
  )
}



#' @export graphUiOutput
#' @rdname shinyGraphBuildingBlocks
graphUiOutput <- function(){
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
#' @rdname shinyGraphBuildingBlocks
#' @importFrom shinyjs js
#' @importFrom igraph components delete_vertices
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


#########################
##                     ##
##   coocccurrences    ##
##                     ##
#########################

#' @export cooccurrencesUiInput
#' @rdname shinyGraphBuildingBlocks
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
#' @rdname shinyGraphBuildingBlocks
cooccurrencesUiOutput <- function(){
  list(
    DT::dataTableOutput('cooccurrences_table')
  )
}


#' @export cooccurrencesServer
#' @rdname shinyGraphBuildingBlocks
cooccurrencesServer <- function(input, output, session){
  
  output$cooccurrences_table <- DT::renderDataTable({
    input$cooccurrences_go
    input$cooccurrences_time
    isolate({
      
      if (input$cooccurrences_name != ""){
        
        message("... getting Cooccurrences object: ", input$cooccurrences_name)
        dt <- get(input$cooccurrences_name, envir = .GlobalEnv)$dt
        
        a <- input$cooccurrences_a
        Encoding(a) <- "unknown" # to avoid data.table errors (?!)

        if (input$cooccurrences_a != "" && input$cooccurrences_b == ""){
          dt <- dt[dt[["a_word"]] == a]
        }
        
        if (input$cooccurrences_a != "" && input$cooccurrences_b != ""){
          b <- input$cooccurrences_b
          Encoding(b) <- "unknown"

          dt1 <- dt[dt[["a_word"]] == a]
          dt1 <- dt1[dt1[["b_word"]] == b]
          dt2 <- dt[dt[["a_word"]] == b]
          dt2 <- dt2[dt2[["b_word"]] == a]
          dt <- data.table::rbindlist(list(dt1, dt2))
        }
        values[["dt"]] <- dt
        
        return(dt)
      } else {
        dt <- data.table(
          word = ""[0], count_window = ""[0], count_partition = ""[0],
          exp_window = integer(), exp_partition = integer(), ll = integer(),
          rank_ll = integer()
        )
        return(dt)
      }
    })
  })
  
  observeEvent(
    input$cooccurrences_table_rows_selected,
    {
      if (length(input$cooccurrences_table_rows_selected) > 0){
        if (input$cooccurrences_a == ""){
          statTab <- get(input$cooccurrences_name, envir = .GlobalEnv)$dt
        } else {
          statTab <- values[["dt"]]
        }
        updateSelectInput(session, "kwic_object", selected = "partition")
        P <- get(input$graph_object, envir = .GlobalEnv)$partition
        values[["partitions"]][[P@name]] <- P
        updateSelectInput(session, "kwic_partition", choices = P@name, selected = P@name)
        updateTextInput(
          session, "kwic_query",
          value = statTab[["a_word"]][input$cooccurrences_table_rows_selected]
          )
        updateTextInput(
          session, "kwic_neighbor",
          value = statTab[["b_word"]][input$cooccurrences_table_rows_selected]
        )
        updateSelectInput(session, "kwic_left", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$window)
        updateSelectInput(session, "kwic_right", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$window)
        updateSelectInput(session, "kwic_pAttribute", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
}

