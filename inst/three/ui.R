library(shiny)
library(polmineR)
library(shinyjs)
library(shinythemes)

shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

threeObjects <- getObjects('three')

shinyUI(
  
  
  
  navbarPage(
    
    # titlePanel("Hello Shiny!"),
    
    theme = shinyThemeToUse,
    
    title = "polmineR",
    
    tabPanel(
      
      "graph",
      
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/three.min.js"),
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/TrackballControls.js"),
      includeScript("/Users/blaette/Lab/tmp/three/foo.json"),
      includeScript("/Users/blaette/Lab/github/polmineR.graph/inst/three/www/foo.js"),
      
      sidebarLayout(
        sidebarPanel(
          
          graphUiInput(),
          selectInput("three", "three", choices = threeObjects, selected = threeObjects[1]),
          textInput("foo", "foo", value = "")
          
        ),
        mainPanel(
          
                     div(
                       id = "content",
                       uiOutput('three')
                     )
        )
      ),
      tags$script("init()"), # happens on load
      tags$script("animate()")
      
    ),
  
    useShinyjs(debug = TRUE),
    extendShinyjs(script = "/Users/blaette/Lab/github/polmineR.graph/inst/three/www/shinyjs.js")

  )
)