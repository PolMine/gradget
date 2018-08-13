#' Convert igraph object to gradget data.
#' 
#' @param nodeSize Size of nodes (defaults to 8).
#' @param edgeWidth Width of edges, an integer (defaults to 5).
#' @param edgeColor Color of edges, a hex value (defaults to "0xeeeeee").
#' @param fontSize Size of node text, an integer (defaults to 20).
#' @param fontColor Color of node text, a hex value (defaults to "#FFFFFF").
#' @export igraph_as_gradget_data
igraph_as_gradget_data <- function(
  graph,
  nodeSize = 8, nodeColor = "0xcccccc",
  edgeWidth = 5, edgeColor = "0xeeeeee",
  fontSize = 16, fontColor = "#FFFFFF"
){
  vertex_data <- list(
    x = V(G)$x, y = V(G)$y, z = V(G)$z,
    count = V(G)$count, name = V(G)$name,
    nodeSize = rep(nodeSize, times = length(V(G))), color = if (is.null(V(G)$color)) nodeColor  else V(G)$color,
    fontSize = fontSize, fontColor = rep(fontColor, times = length(V(G)))
  )
  
  edgelistId <- as_edgelist(G, names = FALSE)
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
  
  list(
    vertex_data = vertex_data,
    edge_data = edge_data
  )
}



#' Utility functions to manipulate igraph objects.
#' 
#' @param layout choose from list
#' @param dim no of dimensions
#' @param method method for coordinate calculation
#' @export igraph_add_coordinates
#' @rdname igraph_utils
#' @importFrom igraph layout.fruchterman.reingold layout.kamada.kawai layout.spring layout.lgl layout.graphopt edge.betweenness.community is.directed
#' @importFrom igraph layout_with_graphopt
igraph_add_coordinates = function(
  x,
  layout = c("kamada.kawai", "fruchterman.reingold", "kamada.kawai", "spring", "lgl", "graphopt"),
  dim = 3
  ){
  if (!dim %in% c(2,3)) warning("numer of dimensions is not valid")
  coords <- switch(
    layout,
    fruchterman.reingold = layout.fruchterman.reingold(x, dim = dim),
    kamada.kawai = layout.kamada.kawai(x, dim = dim),
    spring = layout.spring(x, dim = dim),
    lgl = layout.lgl(x, dim = dim),
    graphopt = layout_with_graphopt(x)
    )
  V(x)$x <- coords[,1]
  V(x)$y <- coords[,2]
  if (dim == 3){
    V(x)$z <- coords[,3]
  }
  x
}

#' Normalize coordinates of igraph object.
#' 
#' @param x igraph object
#' @param width new width
#' @param height new height
#' @param margin margin
#' @rdname igraph_utils
#' @export igraph_normalize_coordinates
igraph_normalize_coordinates = function(x, width, height, margin){
  coords <- cbind(V(x)$x, V(x)$y)
  normalize <- function(y) {
    y <- y - min(y)
    y / max(y)
  }
  coords <- apply(coords, 2, normalize)
  V(x)$x <- round(coords[,1] * (width - 2 * margin) + margin, 0)
  V(x)$y <- round(coords[,2] * (height - 2 * margin) + margin, 0)
  x
}


#' @param min min
#' @param max max
#' @rdname igraph_utils
#' @export rescale
rescale <- function(x, min, max){
  
  .rescale <- function(values, min, max){
    values <- values - min(values)
    values <- values / max(values)
    values <- values * (max - min)
    values <- values + min
    values
  }
  
  V(x)$x <- .rescale(V(x)$x, min, max)
  V(x)$y <- .rescale(V(x)$y, min, max)
  V(x)$z <- .rescale(V(x)$z, min, max)
  x
}


#' @param weights use weights for community detection
#' @import RColorBrewer
#' @rdname igraph_utils
#' @export igraph_add_communities
#' @importFrom igraph cluster_fast_greedy fastgreedy.community edge.betweenness.community multilevel.community
#' @importFrom igraph membership V<-
igraph_add_communities <- function(x, method = "fastgreedy", weights = FALSE){
  
  colors <- rep(c(brewer.pal(5, "Set1"), brewer.pal(8, "Dark2")), times = 10)
  
  if (method == "fastgreedy"){
    G <- x
    if (is.directed(G) == TRUE) G <- as.undirected(G, edge.attr.comb = "concat")
    if (weights == TRUE){
      fgComm <- fastgreedy.community(G, weights = E(G)$ll)  
    } else if (weights == FALSE){
      fgComm <- cluster_fast_greedy(G)
    }
    mem <- membership(fgComm)
    V(x)$community <- mem
    colors <- rep(colors, times = ceiling(max(V(x)$community)/length(colors)))
    V(x)$color <- colors[mem + 1]
  } else if (method == "edge.betweenness"){
    if (weights == TRUE){
      ebc <- edge.betweenness.community(
        x,
        directed = is.directed(x),
        weights = E(x)$ll
      )
    }
  } else if (method == "multilevel"){
    com <- multilevel.community(x)
    warning("not implemented properly")
  }
  x
}


#' @param no number of community
#' @rdname igraph_utils
#' @export selectCommunity
selectCommunity <- function(x, no){
  delete.vertices(x, V(x)[which(! V(x)$community %in% no)])
}

#' @importFrom DiagrammeR create_node_df create_edge_df create_graph
#' @importFrom igraph ends as_edgelist get.edgelist
#' @export as.dgr_graph
#' @rdname igraph_utils
as.dgr_graph <- function(x){
  nodes <- create_node_df(
    n = length(V(x)),
    label = V(x)$name,
    fillcolor =  "transparent",
    type = "type_1",
    fontsize = 15,
    penwidth = 0,
    shape = NA,
    fontcolor = V(x)$color,
    tooltip = V(x)$name
  )
  edges <- create_edge_df(
    from = ends(x, E(x), names = FALSE)[,1],
    to = ends(x, E(x), names = FALSE)[,2],
    penwidth = 1.5,
    arrowhead = "dot",
    tooltip = paste("ll:", as.character(round(E(x)$ll, 2)))
  )
  graph <- create_graph(
    nodes_df = nodes,
    edges_df = edges
  )
  graph
}

#' @importFrom plotly plot_ly add_markers add_text layout toRGB
#' @export as.plotly
#' @rdname igraph_utils
as.plotly <- function(x){
  
  nodes <- plot_ly(
    x = ~ V(x)$x, y = ~ V(x)$y, mode = "markers", type = "scatter",
    color = V(x)$color,
    sizes = rep(5, times = length(V(x))),
    text = V(x)$name, hoverinfo = "text"
    )
  
  edges <- apply(
    get.edgelist(x, names = FALSE), 1,
    function(edgepair){
      list(
        type = "line",
        line = list(color = "#030303", width = 0.3),
        x0 = V(x)$x[edgepair[1]],
        y0 = V(x)$y[edgepair[1]],
        x1 = V(x)$x[edgepair[2]],
        y1 = V(x)$y[edgepair[2]],
        hoverinfo = "edge"
      )
    }
  )
  
  t <- list(size = 12, color = toRGB("grey50"))
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  p <- layout(nodes, shapes = edges, xaxis = axis, yaxis = axis)
  p <- add_markers(p)
  p <- add_text(p, textfont = t, textposition = "center")
  p
}

#' @importFrom networkD3 forceNetwork
#' @export as.networkD3
#' @rdname igraph_utils
as.networkD3 <- function(x){
  
  links <- as.data.frame(as_edgelist(x, names = FALSE))
  links[,1] <- links[,1] - 1
  links[,2] <- links[,2] - 1 
  links <- cbind(links, rep(1, times = nrow(links)))
  colnames(links) <- c("source", "target", "value")
  
  nodes <- data.frame(
    name = V(x)$name,
    group = V(x)$community,
    size = 3
  )
  
  forceNetwork(
    Links = links, Nodes = nodes, Source = "source",
    Target = "target", Value = "value", NodeID = "name",
    Group = "group", opacity = 0.75, fontSize = 20, zoom = TRUE,
    opacityNoHover = TRUE
  )
  
}



#' Turn igraph object into SVG.
#' 
#' @param edgeAttributes attributes of edges to maintain
#' @param verticeAttributes attributes of vertices to maintain
#' @param width the width of the svg
#' @param height the height of the svg
#' @param margin margins of the svg
#' @param fontSize font size of the vertex labels
#' @param textOffset where to put text
#' @param edgeAttributes attributes of edges for tooltips
#' @param verticeAttributes attributes of attributes for tooltips
#' @importFrom xml2 read_xml
#' @import svgPanZoom svgPanZoom
#' @importFrom igraph V
#' @examples 
#' library(polmineR.graph)
#' G <- merkel2008
#' G <- igraph_add_coordinates(G, layout = "kamada.kawai", dim = 2)
#' G <- igraph_normalize_coordinates(G, width = 800, height = 800, margin = 50)
#' S <- igraph_as_svg(G)
#' 
#' w <- svgPanZoom::svgPanZoom(S)
#' 
#' w <- appendContent(
#'   w,
#'   includeScript(path = system.file(package = "polmineR.graph", "js", "svg_js_extensions.js"))
#'   )
#'    
#' @export igraph_as_svg
igraph_as_svg <- function(
  graph,
  radius_min = 5, radius_tf = TRUE,
  edgeColor = "black",
  fontSize = 8, textOffset = 3,
  width = 800, height = 800
){
  
  if (is.null(V(graph)$color)) V(graph)$color <- rep("blue", times = length(V(G)))
  if (radius_tf){
    if (!is.null(V(graph)$count)){
      rad <- radius_min + sqrt(sqrt(V(graph)$count / 3.14159))  
    } else {
      rad <- rep(radius_min, times = length(V(graph)))
    }
    
  } else {
    rad <- rep(radius_min, times = length(V(graph)))
  }
  
  if (is.null(V(graph)$community)) V(graph)$community <- rep(0, length(V(graph)))
  
  nodes <- sprintf(
    '<circle r="%s" stroke="%s" fill="%s" cx="%s" cy="%s" nodeId="%s" token="%s" count="%s" freq="%s" community="%s" onmouseover="nodeMouseOver(event)"/>',
    as.character(rad),
    rep("black", times = length(V(G))),
    V(graph)$color,
    V(graph)$x,
    V(graph)$y,
    1L:length(V(G)),
    V(graph)$name,
    "", #as.character(V(self$igraph)[i]$tfAbs),
    "", #as.character(V(self$igraph)[i]$tfRel),
    V(graph)$community
  )

  edgelistId <- get.edgelist(graph, names = FALSE)
  edgelistString <- get.edgelist(graph, names = TRUE)
  
  if (!is.null(E(graph)$ll)){
    llValues <- round(sapply(E(graph)$ll, mean), 2)
  } else {
    llValues <- rep(0, times = length(E(igraph)))
  }
  
  edges <- sprintf(
    '<line style="%s" x1="%s" y1="%s" x2="%s" y2="%s" from="%s" to="%s" llXY="%s" llYX="%s" onmouseover="edgeMouseOver(event, this)"/>',
    sprintf("stroke:%s; stroke-width:1px; fill:none;", edgeColor),
    V(graph)[edgelistId[,1]]$x, #x1
    V(graph)[edgelistId[,1]]$y, #y1
    V(graph)[edgelistId[,2]]$x, #x2
    V(graph)[edgelistId[,2]]$y, #y2
    edgelistId[,1], #y2
    edgelistId[,2], #
    llValues,
    llValues
  )
  
  labels <- sprintf(
    '<text fill="%s" style = "%s" x="%s" y="%s" nodeId="%s" onmouseover="nodeMouseOver(event)">%s</text>',
    "red",
    paste("font-size:", as.character(fontSize), "px;font-family:sans-serif", sep = ""),
    V(graph)$x + textOffset,
    V(graph)$y - textOffset,
    1L:length(V(graph)),
    V(graph)$name
  )
  
  svg_vec <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width ="%s" height="%s">%s%s%s</svg>',
    width, height,
    paste(unlist(edges), collapse = ""),
    paste(unlist(nodes), collapse = ""),
    paste(unlist(labels), collapse = "")
  )
  xml2::read_xml(svg_vec)
}