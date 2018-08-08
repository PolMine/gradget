#' Utility functions to manipulate igraph objects.
#' 
#' @param layout choose from list
#' @param dim no of dimensions
#' @param method method for coordinate calculation
#' @export addCoordinates
#' @rdname igraph_utils
#' @importFrom igraph layout.fruchterman.reingold layout.kamada.kawai layout.spring layout.lgl layout.graphopt edge.betweenness.community is.directed
#' @importFrom igraph layout_with_graphopt
addCoordinates = function(
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
#' @export normalizeCoordinates
normalizeCoordinates = function(x, width, height, margin){
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
#' @export addCommunities
#' @importFrom igraph cluster_fast_greedy fastgreedy.community edge.betweenness.community multilevel.community
#' @importFrom igraph membership V<-
addCommunities <- function(x, method = "fastgreedy", weights = FALSE){
  
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

