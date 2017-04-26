shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

threeObjects <- getObjects('three')

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    tabPanel(
      
      title = "graph",
      
      includeScript(system.file("js", "three.min.js", package = "three")),
      includeScript(system.file("js", "TrackballControls.js", package = "three")),
      includeScript(system.file("three", "www", "onload.js", package = "polmineR.graph")),
      includeScript(system.file("three", "www", "AnaglyphEffect.js", package = "polmineR.graph")),
      # tags$script('var anaglyphMode = "false"; Shiny.addCustomMessageHandler("setAnaglyphMode", function(anaglyphModeNew) {anaglyphMode = anaglyphModeNew; console.log(anaglyphModeNew);});'),
      tags$script(jsFunctionClick),
      
      sidebarLayout(
        sidebarPanel = sidebarPanel(graphUiInput()),
        mainPanel = mainPanel( div(id = "content") )
      )

    ),
    
    tabPanel(
      title = "cooccurrences",
      sidebarLayout(
        sidebarPanel(polmineR.graph::cooccurrencesUiInput()),
        mainPanel(polmineR.graph::cooccurrencesUiOutput())
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
        sidebarPanel(settingsGraphUiInput()),
        mainPanel()
        
      )
    ),
    
    useShinyjs(debug = TRUE),
    extendShinyjs(script = system.file("three", "www", "shinyjs.js", package = "polmineR.graph"))
    
  )
)