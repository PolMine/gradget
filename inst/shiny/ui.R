library(shiny)
library(polmineR)
library(shinyBS)
library(shinythemes)

svgObjects <- getObjects('svg')
partitionNames <- getObjects('partition')

shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR.graph",
    id = "polmineR.graph",
    
    tabPanel(
      "graph",
      sidebarLayout(
        sidebarPanel(
          selectInput("svg", "svg", choices = svgObjects, selected = svgObjects[1])
          ),
        mainPanel(
            uiOutput('svg')
          )
        )
      ),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel(
          actionButton("kwic_go", "Go!"), br(),br(),
          selectInput("kwic_partition", "partition", choices=partitionNames),
          textInput("kwic_query", "query", value="Suche"),
          textInput("kwic_neighbor", "neighbor", value=""),
          selectInput(
            "kwic_meta", "sAttribute",
            choices=sAttributes(get(partitionNames[1], ".GlobalEnv")@corpus),
            selected=getOption("polmineR.meta"),
            multiple=TRUE
          ),
          selectInput(
            "kwic_pAttribute", "pAttribute",
            choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
          ),
          numericInput("kwic_left", "left", value=getOption("polmineR.left")),
          numericInput("kwic_right", "right", value=getOption("polmineR.right")),
          radioButtons("kwic_read", "read", choices=c("TRUE", "FALSE"), selected="FALSE", inline=T),
          br()
        ),

        mainPanel(DT::dataTableOutput('kwic_table'))
      )
    )
   )
)