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
      
      includeScript(system.file("js", "three.min.js", package = "three")),
      includeScript(system.file("js", "TrackballControls.js", package = "three")),
      # includeScript("/Users/blaette/Lab/tmp/three/foo.json"),
      includeScript(system.file("three", "www", "foo.js", package = "polmineR.graph")),
      includeScript(system.file("three", "www", "AnaglyphEffect.js", package = "polmineR.graph")),
      tags$script('var anaglyphMode = "false"; Shiny.addCustomMessageHandler("setAnaglyphMode", function(anaglyphModeNew) {anaglyphMode = anaglyphModeNew; console.log(anaglyphModeNew);});'),
      
      sidebarLayout(
        sidebarPanel = sidebarPanel(graphUiInput()),
        mainPanel = mainPanel( div(id = "content", uiOutput('three')) )
      ),
      
      tags$script("init()"), # happens on load
      tags$script("animate()"),
      
      # to pass calibration factors between slider inputs and javascript
      tags$script('var calibrationX = 5; Shiny.addCustomMessageHandler("passCallibrationX", function(calibrateX) {calibrationX = calibrateX;});'),
      tags$script('var calibrationY = 5; Shiny.addCustomMessageHandler("passCallibrationY", function(calibrateY) {calibrationY = calibrateY;});')
      
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
      "read",
      fluidPage(
        fluidRow(
          column(2),
          column(8, readUiOutput()),
          column(2)
        )
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
    extendShinyjs(script = system.file("three", "www", "shinyjs.js", package = "polmineR.graph"))
    
  )
)