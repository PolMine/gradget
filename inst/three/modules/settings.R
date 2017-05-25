settingsGraphUiInput <- function(){
  list(
    callibration_x = sliderInput("graph_callibration_x", "x callibration", min = 0, max = 1, value = 0.68, step = 0.01),
    callibration_y = sliderInput("graph_callibration_y", "y callibration", min = -1, max = 0, value = -0.18, step = 0.01),
    bgColcour = colourpicker::colourInput(inputId = "graph_background_color", label = "background")
  )
}
