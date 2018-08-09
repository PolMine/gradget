#' Generate and manage 3d graph.
#' 
#' Generate 3d graph with three.js
#' 
#' @param type display mode
#' @param bgColor a hex value
#' @param nodeSize size of nods
#' @param edgeColor a hex value
#' @param edgeWidth defaults to 5
#' @param fontColor a hex value
#' @param fontSize defaults to 20
#' @param fontOffset a list
#' 
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
#' G <- addCoordinates(G, layout = "kamada.kawai", dim = 3)
#' G <- addCommunities(G)
#' G <- rescale(G, -250, 250)
#' 
#' T <- three(G, type = "base", raycaster = TRUE)
#' T
#' }
three <- function(
  G = G,
  type = "base",
  bgColor = "0x888888",
  nodeSize = 8,
  edgeColor = "0xeeeeee", edgeWidth = 5,
  fontSize = 16, fontColor = "#FFFFFF", fontOffset = c(x = 10, y = 10, z = 10),
  raycaster = FALSE,
  anaglyph = FALSE,
  width = NULL, height = NULL,
  sizing_policy = htmlwidgets::sizingPolicy(padding = 0)
  ){
  if (is.null(V(G)$z)) warning("coordinates for threedimensional display are not available")
  
  vertex_data <- list(
    x = V(G)$x,
    y = V(G)$y,
    z = V(G)$z,
    color = if (is.null(V(G)$color)) "0xcccccc" else V(G)$color,
    nodeSize = rep(nodeSize, times = length(V(G))),
    count = V(G)$count,
    name = V(G)$name,
    fontColor = rep(fontColor, times = length(V(G))),
    fontSize = fontSize
  )
  
  edgelistId <- get.edgelist(G, names = FALSE)
  edge_data <- list(
    from = list(
      name = get.edgelist(G)[,1],
      x = V(G)[edgelistId[,1]]$x,
      y = V(G)[edgelistId[,1]]$y,
      z = V(G)[edgelistId[,1]]$z
    ),
    to = list(
      name = get.edgelist(G)[,2],
      x = V(G)[edgelistId[,2]]$x,
      y = V(G)[edgelistId[,2]]$y,
      z = V(G)[edgelistId[,2]]$z
    ),
    names = attr(E(G), "vnames"),
    ll = unlist(lapply(
      get.edge.attribute(G, "ll"), function(x) paste(round(x, 2), collapse = "|")
      )),
    count = unlist(lapply(get.edge.attribute(G, "ab_count"), mean)),
    color = edgeColor,
    lwd = edgeWidth
  )
  
  x <- list(
    data = list(
      vertex_data = vertex_data,
      edge_data = edge_data
    ),
    settings = list(
      bgColor = bgColor,
      raycaster = raycaster,
      anaglyph = anaglyph
    )
  )
  
  # create the widget
  wdg <- htmlwidgets::createWidget(
    name = "three", x = x, elementId = "three",
    width = width, height = height,
    sizingPolicy = sizingPolicy(
      padding = 0,
      viewer.padding = 0,
      browser.padding = 0,
      knitr.defaultHeight = 800, knitr.defaultWidth = 600
    ),
    package = "polmineR.graph"
    )
  wdg

}


#' @export
threeOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "three", width, height, package = "polmineR.graph")
}
#' @export
renderThree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, threeOutput, env, quoted = TRUE)
}