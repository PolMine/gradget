#' Graph Annotation with Shiny.
#' 
#' A shiny gadget for graph annotation with shiny, or granny in short. 
#' 
#' @param graph An \code{igraph} object that is enhanced for visualisation with
#'   the gradget htmlwidget (x, y, and z coordinates, if applicable; additional
#'   edge and node data).
#' @importFrom miniUI miniPage miniTitleBar miniTabstripPanel miniTabPanel gadgetTitleBar miniContentPanel
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @export granny
granny <- function(graph) { 
  
  
  jsCode <- '
    shinyjs.get_annotations = function(){Shiny.onInputChange("annotations", window.annotated);}
  '
  
  ui <- miniPage(
    
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = "get_annotations"),
    
    gadgetTitleBar(title = "Annotation Gadget"),
    miniTabstripPanel(
      miniTabPanel(
        "Graph", icon = icon("area-chart"),
        miniContentPanel( threeOutput("three"), padding = 0)
      ),
      miniTabPanel(
        "Nodes", icon = icon("genderless"),
        miniContentPanel( DT::dataTableOutput('annotations_table') )
      ),
      miniTabPanel(
        "Edges", icon = icon("location-arrow"),
        miniContentPanel( DT::dataTableOutput('annotations_table2') )
      ),
      miniTabPanel(
        "Data", icon = icon("table"),
        miniContentPanel( DT::dataTableOutput('graph_data') )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$three <- renderThree( three(graph, raycaster = TRUE) )
    
    output$graph_data <- DT::renderDataTable(
      DT::datatable(igraph::as_edgelist(graph), rownames = FALSE)
    )
    
    observeEvent(
      input$graph_space_pressed,
      {
        print("space pressed")
        js$get_annotations()
        
        df <- data.frame(x = input$annotations)
        print(df)
        output$annotations_table <- DT::renderDataTable(
          DT::datatable(df, selection = "single", rownames = FALSE)
        )
      }
    )
    observeEvent(
      input$done,
      stopApp(input$annotations)
    )
    
  }
  
  runGadget(ui, server, viewer = browserViewer())
}
