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
#' @importFrom igraph V E
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
#' three.settings <- list(
#'   fontSize = 18, fontColor <- "0xffffff",
#'   edgeWidth = 6, edgeColor = "0x666666",
#'   bgColor = "0x000000"
#'   )
#' T <- three(G, type = "base")
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
  width = NULL, height = NULL,
  sizing_policy = htmlwidgets::sizingPolicy(padding = 0)
  ){
  if (is.null(V(G)$z)) warning("coordinates for threedimensional display are not available")
  
  point_data <- list(
    x = V(G)$x,
    y = V(G)$y,
    z = V(G)$z,
    color = if (is.null(V(G)$color)) "0xcccccc" else V(G)$color,
    nodeSize = rep(nodeSize, times = length(V(G)))
  )
  
  edgelistId <- get.edgelist(G, names = FALSE)
  edge_data <- list(
    from = list(
      x = V(G)[edgelistId[,1]]$x,
      y = V(G)[edgelistId[,1]]$y,
      z = V(G)[edgelistId[,1]]$z
    ),
    to = list(
      x = V(G)[edgelistId[,2]]$x,
      y = V(G)[edgelistId[,2]]$y,
      z = V(G)[edgelistId[,2]]$z
    ),
    color = edgeColor,
    lwd = edgeWidth
  )
  
  text_data <- list(
    x = V(G)$x + 8,
    y = V(G)$y,
    z = V(G)$z,
    name = V(G)$name,
    fontColor = rep(fontColor, times = length(V(G))),
    fontSize = fontSize
#    , offset = settings[["fontOffset"]]
  )
  
  # vertexAttributes <- list.vertex.attributes(G)
  # jsonVertexAttributes <- vertexAttributes[which(!vertexAttributes %in% c("name", "x", "y", "z", "color"))]
  # threeObject@json[["vertexData"]] <- toJSON(
  #   lapply(
  #     setNames(jsonVertexAttributes, jsonVertexAttributes),
  #     function(name) get.vertex.attribute(G, name)
  #   ))
  
  
  # edgeAttributes <- list.edge.attributes(.self$igraph)
  # if ("ll" %in% edgeAttributes){
  #   threeObject@json[["edgeData"]] <- toJSON(
  #     list(
  #       a = get.edgelist(G)[,1],
  #       b = get.edgelist(G)[,2],
  #       a2b = sapply(get.edge.attribute(G, "ll"), function(x) x[1]),
  #       b2a = sapply(get.edge.attribute(G, "ll"), function(x) x[2])
  #     ))
  # }
  
  x <- list(
    data = list(
      point_data = point_data,
      edge_data = edge_data,
      text_data = text_data
    ),
    settings = list(
      bgColor = bgColor,
      raycaster = raycaster
    )
  )
  
  # create the widget
  wdg <- htmlwidgets::createWidget(name = "three", x = x, width = width, height = height, sizingPolicy = sizing_policy, package = "polmineR.graph")
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