#' Utility functions to manipulate igraph objects.
#' 
#' @param x igraph object
#' @param layout choose from list
#' @param dim no of dimensions
#' @export addCoordinates
#' @rdname igraph_utils
#' @importFrom igraph layout.fruchterman.reingold layout.kamada.kawai layout.spring layout.lgl layout.graphopt edge.betweenness.community is.directed
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
    graphopt = layout.graphopt(x, dim = dim)
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

setGeneric("rescale", function(object, ...) standardGeneric("rescale"))


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


#' @param method 
#' @param weights use weights for community detection
#' @import RColorBrewer
#' @rdname igraph_utils
#' @export addCommunities
#' @importFrom igraph cluster_fast_greedy fastgreedy.community edge.betweenness.community multilevel.community
addCommunities <- function(x, method = "fastgreedy", weights = FALSE){
  
  colors <- c(
    brewer.pal(9, "Set1"),
    brewer.pal(8, "Set2"),
    brewer.pal(12, "Set3"),
    brewer.pal(8, "Accent")
  )
  
  if (method=="fastgreedy"){
    if (is.directed(x) == TRUE) x <- as.undirected(x, edge.attr.comb = "concat")
    if (weights == TRUE){
      fgComm <- fastgreedy.community(x, weights = E(x)$ll)  
    } else if (weights == FALSE){
      fgComm <- cluster_fast_greedy(x)
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
        weights=E(x)$ll
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
selectCommunity <- function(x, no){
  delete.vertices(x, V(x)[which(! V(x)$community %in% no)])
}


#' @param order order of neighborhood
#' @param token token at center pf neighborhood
#' @rdname igraph_utils
tokenNeighborhood <- function(x, order, token){
  toKeep <- unlist(neighborhood(x, order = order, nodes = token))
  delete.vertices(x, which(!c(1:length(V(x))) %in% toKeep))
}