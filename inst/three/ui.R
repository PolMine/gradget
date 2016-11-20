library(shiny)
library(polmineR)
library(shinyjs)
library(shinythemes)

shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

threeObjects <- getObjects('three')

shinyUI(
  
  
  
  fluidPage(
    
    # titlePanel("Hello Shiny!"),
    
#    theme = shinyThemeToUse,
    
#    title = "polmineR",
    
#    tabPanel(
      
#      "graph",
      
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/three.min.js"),
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/TrackballControls.js"),
      includeScript("/Users/blaette/Lab/tmp/three/foo.json"),
      includeScript("/Users/blaette/Lab/github/polmineR.graph/inst/three/www/foo.js"),
      
      fluidRow(
        column(2,
          
          graphUiInput(),
          selectInput("three", "three", choices = threeObjects, selected = threeObjects[1]),
          textInput("foo", "foo", value = "")
          
        ),
        column(10,
          div(
            id = "content",
            uiOutput('three')
          )
        )
      ),
      tags$script("init()"), # happens on load
      tags$script("animate()"),
      
      
      # for making progress
      tags$script('var calibrationX = 5; Shiny.addCustomMessageHandler("passCallibrationX", function(calibrateX) {calibrationX = calibrateX; console.log(calibrationX);});'),
      tags$script('var calibrationY = 5; Shiny.addCustomMessageHandler("passCallibrationY", function(calibrateY) {calibrationY = calibrateY; console.log(calibrationY);});'),
      
      
#    ),
  
    useShinyjs(debug = TRUE),
    extendShinyjs(script = "/Users/blaette/Lab/github/polmineR.graph/inst/three/www/shinyjs.js")

  )
)