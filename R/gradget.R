setOldClass("igraph")
setOldClass("json")


#' Cooccurrence (i)graph of Merkel 2008.
#' 
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
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @importFrom shiny runGadget observeEvent stopApp browserViewer icon
#' @importFrom pbapply pblapply
#' @importFrom polmineR kwic
#' @examples
#' library(magrittr)
#' library(polmineR)
#' library(pbapply)
#' use("GermaParl")
#' 
#' G <- merkel2008 %>%
#'   igraph_add_coordinates(layout = "kamada.kawai", dim = 3) %>%
#'   igraph_add_communities() %>% 
#'   rescale(-250, 250)
#' 
#' am2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' # G <- igraph_add_kwic(G, subcorpus = am2008)
#' if (interactive()) gradget(G)
#' @export gradget
gradget <- function(graph) { 
  
  
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
    
    output$three <- renderThree( three(igraph_as_gradget_data(graph), raycaster = TRUE, elementId = NULL) )
    
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
