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
        
        dt[, word_id := NULL][, i.word_id := NULL][, size_window := NULL][, rank_ll := NULL][, exp_partition := NULL]
        print(colnames(dt))
        setnames(dt, old = c("a_word", "b_word", "exp_window", "count_a", "count_b"), new = c("a", "b", "exp_ab", "obs_a", "obs_b"))
        setcolorder(dt, neworder = c("a", "b", "obs_a", "obs_b", "count_ab", "exp_ab", "ll"))
        
        # will not be shown
        attr(dt[["a"]], "label") <- "word1"
        attr(dt[["b"]], "label") <- "word1"
        attr(dt[["obs_a"]], "label") <- "word1"
        attr(dt[["obs_b"]], "label") <- "word1"
        attr(dt[["count_ab"]], "label") <- "word1"
        attr(dt[["exp_ab"]], "label") <- "word1"

        dt[, ll := round(ll, 2)][, exp_ab := round(exp_ab, 2)]

        
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
        print(P@name)
        values[["partitions"]][[P@name]] <- P
        updateSelectInput(session, "kwic_partition", choices = P@name, selected = P@name)
        updateTextInput(
          session, "kwic_query",
          value = statTab[["a"]][input$cooccurrences_table_rows_selected]
          )
        updateTextInput(
          session, "kwic_neighbor",
          value = statTab[["b"]][input$cooccurrences_table_rows_selected]
        )
        updateSelectInput(session, "kwic_left", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$window)
        updateSelectInput(session, "kwic_right", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$window)
        updateSelectInput(session, "kwic_pAttribute", selected = get(input$cooccurrences_name, envir = .GlobalEnv)$pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        if (debug) message("... resetting values/kwic_go")
        values[["kwic_go"]] <- as.character(Sys.time()) # will initiate kwic preparation & display
      }
    })
}

