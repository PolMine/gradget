setOldClass("igraph")
setOldClass("json")


#' Cooccurrence (i)graph of Merkel 2008.
#' 
#' @docType data
#' @encoding UTF-8 
#' @format An igraph object
"merkel2008"




#' Graph Annotation with Shiny.
#' 
#' A shiny gadget for graph annotation with shiny, or granny in short. 
#' 
#' @param graph An \code{igraph} object that is enhanced for visualisation with
#'   the gradget htmlwidget (x, y, and z coordinates, if applicable; additional
#'   edge and node data).
#' @importFrom miniUI miniPage miniTitleBar miniTabstripPanel miniTabPanel gadgetTitleBar miniContentPanel
#' @importFrom shiny runGadget observeEvent stopApp browserViewer icon reactiveValues
#' @importFrom pbapply pblapply
#' @importFrom polmineR kwic
#' @examples
#' library(gradget)
#' library(magrittr)
#' library(polmineR)
#' library(pbapply)
#' library(igraph)
#' use("GermaParl")
#' 
#' am2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' 
#' G <- gradget::merkel2008 %>%
#'   igraph_add_coordinates(layout = "kamada.kawai", dim = 3) %>%
#'   igraph_add_communities() %>% 
#'   rescale(-250, 250) %>%
#'   igraph_add_kwic(subcorpus = am2008)
#' 
#' if (interactive()) G <- granny(G)
#' @export granny
granny <- function(graph) { 
  
  values <- reactiveValues()
  values[["graph"]] <- graph
  
  ui <- miniPage(
    
    gadgetTitleBar(title = "Annotation Gadget"),
    miniTabstripPanel(
      miniTabPanel(
        "Graph", icon = icon("area-chart"),
        miniContentPanel( gradgetOutput("gradget"), padding = 0)
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
    
    output$gradget <- renderGradget( gradget(igraph_as_gradget_data(graph), raycaster = TRUE, elementId = NULL) )
    
    output$graph_data <- DT::renderDataTable(
      DT::datatable(igraph::as_edgelist(graph), rownames = FALSE)
    )
    
    observeEvent(
      input$annotation_added,
      {
        action_types <- c("1" = "keep", "2" = "reconsider", "3" = "drop")
        if (input$annotation$type == "vertex"){
          V(values$graph)[[input$annotation$name]]$action <- action_types[[input$annotation$selection]]
          V(values$graph)[[input$annotation$name]]$annotation <- input$annotation$annotation
        } else if (input$annotation$type == "edge"){
          edge_no <- which(attr(E(graph), "vnames") == input$annotation$name)
          E(values$graph)[[edge_no]]$action <- action_types[[input$annotation$selection]]
          E(values$graph)[[edge_no]]$annotation <- input$annotation$annotation
        }
        
        # output$annotations_table <- DT::renderDataTable(DT::datatable(df, selection = "single", rownames = FALSE))
      }
    )
    
    observeEvent(
      input$done,
      stopApp(values$graph)
    )
    
  }
  
  runGadget(ui, server, viewer = browserViewer())
}
