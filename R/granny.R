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
#' @examples
#' G <- merkel2008
#' G <- igraph_add_coordinates(G, layout = "kamada.kawai", dim = 3)
#' G <- igraph_add_communities(G)
#' G <- rescale(G, -250, 250)
#' 
#' am2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' V(G)$kwic <- pblapply(V(G)$name, function(n) as.character(kwic(am2008, query = n, verbose = F)))
#' V(G)$kwic <- sapply(V(G)$kwic, function(x) paste(x, collapse = "<br/>"))
#' V(G)$kwic <- unlist(V(G)$kwic)
#' 
#' edge_matrix <- igraph::as_edgelist(merkel2008)
#' q1 <- sprintf('"%s" []{0,4} "%s"', edge_matrix[,1], edge_matrix[,2])
#' q2 <- sprintf('"%s" []{0,4} "%s"', edge_matrix[,2], edge_matrix[,1])
#' E(G)$kwic <- pblapply(
#'   split(data.frame(q1, q2, stringsAsFactors = F), f = 1L:length(q1)),
#'   function(q) as.character(kwic(am2008, query = unlist(q), cqp = T, verbose = F))
#' )
#' E(G)$kwic <- sapply(E(G)$kwic, function(x) paste(x, collapse = "<br/>"))
#' 
#' granny(G)
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
    
    output$three <- renderThree( three(igraph_as_gradget_data(graph), raycaster = TRUE) )
    
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
