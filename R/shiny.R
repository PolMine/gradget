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
    reference = selectInput("graph_reference", label = "reference", choices = c("", getObjects("cooccurrences", envir = .GlobalEnv))),
    max_rank = sliderInput("graph_max_rank", label = "max. rank", min = 10, max = 1000, value = 200),
    dim = radioButtons("graph_dim", "dimensions", choices = c("2d", "3d"), selected = "3d", inline = TRUE),
    anaglyph = conditionalPanel(
      condition = 'input.graph_dim == "3d"',
      radioButtons("graph_anaglyph_mode", "anaglyph", choices = c("true", "false"), selected = "true", inline = TRUE)
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
  print(input$graph_object)
  
  coocObject <- get(input$graph_object, envir = .GlobalEnv)
  print(dim(coocObject))
  
  if (TRUE){
    coocObject@stat <- coocObject@stat[!a_word %in% polmineR::punctuation][!b_word %in% polmineR::punctuation]
    coocObject@stat <- coocObject@stat[!a_word %in% tm::stopwords("de")][!b_word %in% tm::stopwords("de")]
  }
  
  message("... trimming object / applying max_rank")
  maxValue <- as.integer(input$graph_max_rank)
  if (input$graph_reference != ""){
    comparison <- polmineR::compare(
      x = coocObject, y = get(input$graph_reference, envir = .GlobalEnv)
    )
    comparison@stat <- comparison@stat[which(comparison[["rank_ll"]] <= maxValue)]
    coocObject <- trim(coocObject, by = comparison)
    print(dim(coocObject))
  } else {
    coocObject@stat <- coocObject@stat[which(coocObject[["rank_ll"]] <= maxValue)]
  }
  
  
  message("... as igraph")
  igraphObject <- asIgraph(coocObject)
  
  if (input$graph_cutoff >= 2){
    message("... removing components")
    comps <- components(igraphObject)
    verticesToDrop <- which(comps[[1]] %in% (1:max(comps[[2]]))[which(comps[[2]] <= input$graph_cutoff)])
    igraphObject <- delete_vertices(igraphObject, verticesToDrop)
  }
  
  message("... community detection")
  igraphObject <- enrich(igraphObject, community = list(method = "fastgreedy", weights=FALSE))
  
  message("... layout / coordinates")
  igraphObject <- enrich(igraphObject, layout = "kamada.kawai", dim = 3)
  igraphObject
}

.asDOMElement <- function(x){
  x <- three::rescale(x, -600, 600)
  y <- XML::saveXML(as.svg(x, width = 800, height = 800)@xml)
  y <- gsub("^.*?(<svg.*?</svg>).*$", "\\1", y)
  y <- gsub("(<svg.*?>)", paste("\\1<script>", jsFunctionClick, "</script>", sep = ""), y)
  y
}

# turn igraph object into the json that is needed by THREE
# x needs to be an igraph object
.igraphToJson <- function(x){

  message("... three dimensions")
  threeObject <- polmineR.graph::as.three(
    x, type = "anaglyph", bgColor = "0xcccccc",
    fontSize = 12, fontColor = "0x000000", nodeSize = 4,
    edgeColor = "0xeeeeee", edgeWidth = 3, fontOffset = c(x = 10, y = 10, z = 10)
  )
  
  message("... creating json")
  newJson <- as(threeObject, "json")
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
      if (input$graph_go > 0){
        
        message("... generating new object")
        igraphObject <- .cooccurrencesToIgraph(input, output, session)
        
        # assign to cache, so that object will be available for switching 2d/3d mode
        message("... assign to .GlobalEnv")
        assign("igraphObject", igraphObject, envir = get(".polmineR_graph_cache", envir = .GlobalEnv))

        if (input$graph_dim == "2d"){
          
          message("... rescaling")
          igraphObject <- three::rescale(igraphObject, -600, 600)
          domElement <- .asDOMElement(igraphObject)
          js$twoDimGraph(domElement)
          
        } else if (input$graph_dim == "3d"){
          
          message("... 3d json")
          igraphObject <- three::rescale(igraphObject, -400, 400)
          newJson <- .igraphToJson(igraphObject)
          message("... transferring new values")
          js$reloadData(newJson)
          js$reinitialize()
        
        }
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
    selectInput("cooccurrences_name", "name", choices = getObjects("cooccurrences", envir = .GlobalEnv)),
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
        df <- get(input$cooccurrences_name, envir = .GlobalEnv)@stat
        
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

