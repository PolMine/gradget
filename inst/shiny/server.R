library(shiny)
library(polmineR)
library(magrittr)
library(DT)

renewKwic <- 0

shinyServer(function(input, output, session) {

  output$svg <- renderUI({
    tmp <- get(input$svg, env=.GlobalEnv)
    html(tmp)
  })
  
  ## kwic 
  
  observe({
    x <- input$kwic_partition
    new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
    new_pAttr <- pAttributes(get(x, ".GlobalEnv")@corpus)
    updateSelectInput(session, "kwic_pAttribute", choices=new_pAttr, selected=NULL)
    updateSelectInput(session, "kwic_meta", choices=new_sAttr, selected=NULL)
  })
  
  output$kwic_table <- DT::renderDataTable({
    kwicit()
    withProgress(
      message="please wait", value=0, max=5, detail="preparing data",
      {
      
      input$kwic_go
      # incProgress(0.5, detail = paste("Doing part", 1))
      
    isolate({
      kwicObject <<- polmineR::kwic(
        .Object=get(input$kwic_partition, '.GlobalEnv'),
        query=input$kwic_query, pAttribute=input$kwic_pAttribute,
        left=input$kwic_left, right=input$kwic_right,
        meta=input$kwic_meta, verbose="shiny",
        neighbor=input$kwic_neighbor
        )
      })
    
    tab <- kwicObject@table
    if (length(input$kwic_meta) == 0){
      retval <- data.frame(no=c(1:nrow(tab)), tab)
    } else if (length(input$kwic_meta) > 0){
      metaRow <- unlist(lapply(
        c(1:nrow(tab)),
        function(i){
          paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
        }
      ))
      retval <- data.frame(meta=metaRow, tab[,(length(input$kwic_meta)+1):ncol(tab)])
    }
      }) # end withProgress
    retval <- DT::datatable(retval, selection="single", rownames=FALSE) %>% 
      DT::formatStyle("node", color="#4863A0", textAlign="center") %>%
      DT::formatStyle("left", textAlign="right")
    if (length(input$kwic_meta) > 0){
      retval <- DT::formatStyle(retval, "meta", fontStyle="italic", textAlign="left", borderRight="1px solid DarkGray")
    }
    retval
  }
  )

  observeEvent(
    input$kwic_table_rows_selected,
    {
      if (length(input$kwic_table_rows_selected) > 0){
        if (input$kwic_read == "TRUE"){
          fulltext <- html(kwicObject, input$kwic_table_rows_selected, type="plpr")
          browse(fulltext)
        } else {
          output$fulltext <- renderText("you do not want to read")
        }
      }
    })
  
  observeEvent(
    input$kwic_query_nodeclick,
    {
      print(input$kwic_query_nodeclick)
      updateTextInput(session, "kwic_query", value=input$kwic_query_nodeclick)
      updateActionButton(session, "kwic_go", label="Go!")
      updateNavbarPage(session, "polmineR.graph", selected = "kwic")
      renewKwic <<- renewKwic + 1 
    }
  )
  
  observe({
    input$kwic_neighbor_edgeclick
    input$kwic_query_edgeclick
    print(input$kwic_neighbor_update)
    updateTextInput(session, "kwic_neighbor", value=input$kwic_neighbor_edgeclick)
    updateTextInput(session, "kwic_query", value=input$kwic_query_edgeclick)
    updateNavbarPage(session, "polmineR.graph", selected = "kwic")
    paste(input$kwic_neighbor_edgeclick, input$kwic_query_edgeclick, sep="|")
  })
  
  
  kwicit <- reactive({
    input$kwic_query_nodeclick
    input$kwic_query_edgeclick
    return("yeah")
  })
  
  observeEvent(
    kwicit(), {print("hi")})

})
