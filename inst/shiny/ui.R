library(shiny)
library(polmineR)
library(shinyBS)
library(shinythemes)

svgObjects <- getObjects('svg')
partitionNames <- getObjects('partition')

shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  fluidPage(
    
#    theme = shinyThemeToUse,
#    title = "polmineR.graph",
#    id = "polmineR.graph",
    
#    tabPanel(
#      "graph",
      sidebarLayout(
        sidebarPanel(
          selectInput("svg", "svg", choices = svgObjects, selected = svgObjects[1])
          ),
        mainPanel(
            uiOutput('svg')
          )
        )
#      )
   )
)