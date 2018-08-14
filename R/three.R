#' Generate and manage 3d graph.
#' 
#' Generate 3d graph with three.js
#' 
#' @param data 
#' @param bgColor Background color, a hex value (defaults to "0x888888").
#' @param knitr A logical value, whether htmlwidget is embedded in
#'   knitr/Rmarkdown document. If TRUE, corners will be somewhat rounded, and a
#'   sizing mechanism will ensure that the chunk option is processed
#'   appropriately (best practice: fig.width = 9.5).
#' @name three
#' @rdname three
#' @export three
#' @importFrom igraph V E get.edge.attribute
#' @examples
#' \dontrun{
#' library(polmineR.graph)
#' library(polmineR)
#' use("GermaParl")
#' 
#' merkel2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' 
#' terms_to_drop <- c(
#'   polmineR::punctuation,
#'   unlist(noise(p_attributes(merkel2008, p_attribute = "word")))
#' )
#' 
#' Merkel <- Cooccurrences$new(
#'   partition = merkel2008, p_attribute = "word", window = 5L,
#'   drop = terms_to_drop
#' )
#' Merkel$count()
#' Merkel$trim(action = "drop", by.id = TRUE)
#' Merkel$maths()
#' 
#' Merkel$dt <- Merkel$dt[1:250]
#' 
#' G <- Merkel$as.igraph()
#' G <- igraph_add_coordinates(G, layout = "kamada.kawai", dim = 3)
#' G <- igraph_add_communities(G)
#' G <- rescale(G, -250, 250)
#' 
#' T <- three(
#'   igraph_as_gradget_data(G),
#'   raycaster = TRUE
#' )
#' T
#' }
three <- function(
  data, bgColor = "0x888888", raycaster = TRUE, anaglyph = FALSE, knitr = FALSE,
  width = NULL, height = NULL
  ){
  
  if (is.null(V(G)$z)) warning("coordinates for threedimensional display are not available")

  x <- list(
    data = data,
    settings = list(bgColor = bgColor, raycaster = raycaster, anaglyph = anaglyph, knitr = knitr)
  )
  
  # create the widget
  wdg <- htmlwidgets::createWidget(
    name = "three", x = x,
    # íf elementId is used we get warning: Ignoring explicitly provided widget ID "three"; Shiny doesn't use them
    # elementId = "three", 
    width = width, height = height,
    sizingPolicy = sizingPolicy(
      padding = 0,
      viewer.padding = 0,
      browser.padding = 0, browser.fill = TRUE,
      knitr.defaultHeight = 800, knitr.defaultWidth = 600
    ),
    package = "polmineR.graph"
    )
  wdg

}


#' @export threeOutput
threeOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "three", width, height, package = "polmineR.graph")
}

#' @export renderThree
renderThree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, threeOutput, env, quoted = TRUE)
}

