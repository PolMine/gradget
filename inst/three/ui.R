shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

threeObjects <- getObjects('three')

shinyUI(
  
  navbarPage(
    
    #titlePanel("Hello Shiny!"),
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    tabPanel(
      
      title = "graph",
      
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/three.min.js"),
      includeScript("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/three/js/TrackballControls.js"),
      includeScript("/Users/blaette/Lab/tmp/three/foo.json"),
      includeScript("/Users/blaette/Lab/github/polmineR.graph/inst/three/www/foo.js"),
      
      sidebarLayout(
        sidebarPanel = sidebarPanel(graphUiInput()),
        mainPanel = mainPanel( div(id = "content", uiOutput('three')) )
      ),
      
      tags$script("init()"), # happens on load
      tags$script("animate()"),
      
      # to pass calibration factors between slider inputs and javascript
      tags$script('var calibrationX = 5; Shiny.addCustomMessageHandler("passCallibrationX", function(calibrateX) {calibrationX = calibrateX; console.log(calibrationX);});'),
      tags$script('var calibrationY = 5; Shiny.addCustomMessageHandler("passCallibrationY", function(calibrateY) {calibrationY = calibrateY; console.log(calibrationY);});')
      
    ),
    
    tabPanel(
      title = "cooccurrences",
      sidebarLayout(
        sidebarPanel(cooccurrencesUiInput()),
        mainPanel(cooccurrencesUiOutput())
      )
    ),
    
    tabPanel(
      title = "kwic",
      sidebarLayout(
        sidebarPanel(kwicUiInput()),
        mainPanel(kwicUiOutput())
      )
    ),
    
    tabPanel(
      title = "fulltext",
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    ),
    
    tabPanel(
      
      title = "settings",
      
      sidebarLayout(
        sidebarPanel(settingsUiInput()),
        mainPanel()
        
      )
    ),
    
    useShinyjs(debug = TRUE),
    extendShinyjs(script = "/Users/blaette/Lab/github/polmineR.graph/inst/three/www/shinyjs.js")
    
  )
)