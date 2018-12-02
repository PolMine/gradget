#' Convert igraph object to gradget data.
#' 
#' @param graph The \code{igraph} object to transform.
#' @param nodeSize Size of nodes (defaults to 8).
#' @param nodeColor Color of node. 
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
  # add annotatin and action, if the graph has not yet been annotated
  if (is.null(V(graph)$action)) V(graph)$action <- rep(NA, times = length(V(graph)))
  if (is.null(V(graph)$annotation)) V(graph)$annotation <- rep(NA, times = length(V(graph)))
  if (is.null(E(graph)$action)) E(graph)$action <- rep(NA, times = length(E(graph)))
  if (is.null(E(graph)$annotation)) E(graph)$annotation <- rep(NA, times = length(E(graph)))

  vertex_data <- list(
    x = V(graph)$x,
    y = V(graph)$y,
    z = V(graph)$z,
    action = V(graph)$action,
    annotation = V(graph)$annotation,
    count = V(graph)$count, name = V(graph)$name,
    nodeSize = rep(nodeSize, times = length(V(graph))), color = if (is.null(V(graph)$color)) nodeColor  else V(graph)$color,
    fontSize = fontSize, fontColor = rep(fontColor, times = length(V(graph))),
    info = unlist(V(graph)$info)
  )
  
  edgelistId <- as_edgelist(graph, names = FALSE)
  edge_data <- list(
    from = list(
      name = get.edgelist(graph)[,1],
      x = V(graph)[edgelistId[,1]]$x,
      y = V(graph)[edgelistId[,1]]$y,
      z = V(graph)[edgelistId[,1]]$z
    ),
    to = list(
      name = get.edgelist(graph)[,2],
      x = V(graph)[edgelistId[,2]]$x,
      y = V(graph)[edgelistId[,2]]$y,
      z = V(graph)[edgelistId[,2]]$z
    ),
    action = E(graph)$action,
    annotation = E(graph)$annotation,
    names = attr(E(graph), "vnames"),
    ll = unlist(lapply(
      get.edge.attribute(graph, "ll"), function(x) paste(round(x, 2), collapse = "|")
    )),
    count = unlist(lapply(get.edge.attribute(graph, "ab_count"), mean)),
    color = edgeColor,
    lwd = edgeWidth,
    info = unlist(E(graph)$info)
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
#' @param ... Further arguments.
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
#' @importFrom igraph membership V<- as.undirected
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
#' @importFrom igraph delete_vertices
selectCommunity <- function(x, no){
  delete_vertices(x, V(x)[which(! V(x)$community %in% no)])
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
#' @param x An \code{igraph} object.
#' @param width the width of the svg
#' @param height the height of the svg
#' @param fontSize font size of the vertex labels
#' @param textOffset where to put text
#' @param radius_min ...
#' @param radius_tf ...
#' @param edgeColor ...
#' @importFrom xml2 read_xml
#' @import svgPanZoom svgPanZoom
#' @importFrom igraph V
#' @examples 
#' library(gradget)
#' library(magrittr)
#' library(igraph)
#' library(shiny)
#' 
#' G <- merkel2008 %>%
#'   igraph_add_coordinates(layout = "kamada.kawai", dim = 2) %>%
#'   igraph_normalize_coordinates(width = 800, height = 800, margin = 50)

#' S <- igraph_as_svg(G)
#' w <- svgPanZoom::svgPanZoom(S)
#' 
#' w <- htmlwidgets::appendContent(
#'   w,
#'   shiny::includeScript(path = system.file(package = "gradget", "js", "svg_js_extensions.js"))
#'   )
#'    
#' @export igraph_as_svg
igraph_as_svg <- function(
  x,
  radius_min = 5, radius_tf = TRUE,
  edgeColor = "black",
  fontSize = 8, textOffset = 3,
  width = 800, height = 800
){
  
  if (is.null(V(x)$color)) V(x)$color <- rep("blue", times = length(V(x)))
  if (radius_tf){
    if (!is.null(V(x)$count)){
      rad <- radius_min + sqrt(sqrt(V(x)$count / 3.14159))  
    } else {
      rad <- rep(radius_min, times = length(V(x)))
    }
    
  } else {
    rad <- rep(radius_min, times = length(V(x)))
  }
  
  if (is.null(V(x)$community)) V(x)$community <- rep(0, length(V(x)))
  
  nodes <- sprintf(
    '<circle r="%s" stroke="%s" fill="%s" cx="%s" cy="%s" nodeId="%s" token="%s" count="%s" freq="%s" community="%s" onmouseover="nodeMouseOver(event)"/>',
    as.character(rad),
    rep("black", times = length(V(x))),
    V(x)$color,
    V(x)$x,
    V(x)$y,
    1L:length(V(x)),
    V(x)$name,
    "", #as.character(V(self$ix)[i]$tfAbs),
    "", #as.character(V(self$ix)[i]$tfRel),
    V(x)$community
  )

  edgelistId <- get.edgelist(x, names = FALSE)
  edgelistString <- get.edgelist(x, names = TRUE)
  
  if (!is.null(E(x)$ll)){
    llValues <- round(sapply(E(x)$ll, mean), 2)
  } else {
    llValues <- rep(0, times = length(E(x)))
  }
  
  edges <- sprintf(
    '<line style="%s" x1="%s" y1="%s" x2="%s" y2="%s" from="%s" to="%s" llXY="%s" llYX="%s" onmouseover="edgeMouseOver(event, this)"/>',
    sprintf("stroke:%s; stroke-width:1px; fill:none;", edgeColor),
    V(x)[edgelistId[,1]]$x, #x1
    V(x)[edgelistId[,1]]$y, #y1
    V(x)[edgelistId[,2]]$x, #x2
    V(x)[edgelistId[,2]]$y, #y2
    edgelistId[,1], #y2
    edgelistId[,2], #
    llValues,
    llValues
  )
  
  labels <- sprintf(
    '<text fill="%s" style = "%s" x="%s" y="%s" nodeId="%s" onmouseover="nodeMouseOver(event)">%s</text>',
    "red",
    paste("font-size:", as.character(fontSize), "px;font-family:sans-serif", sep = ""),
    V(x)$x + textOffset,
    V(x)$y - textOffset,
    1L:length(V(x)),
    V(x)$name
  )
  
  svg_vec <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width ="%s" height="%s">%s%s%s</svg>',
    width, height,
    paste(unlist(edges), collapse = ""),
    paste(unlist(nodes), collapse = ""),
    paste(unlist(labels), collapse = "")
  )
  retval <- xml2::read_xml(svg_vec)
  class(retval) <- c("svg", class(retval))
  retval
}

#' @export plot.svg
#' @importFrom htmltools tags
#' @importFrom htmlwidgets appendContent JS
#' @rdname igraph_utils
plot.svg <- function(x, ...){
  w <- svgPanZoom::svgPanZoom(x)
  js <- JS(readLines(system.file(package = "gradget", "js", "svg_js_extensions.js")))
  appendContent(w, tags$script(js))
}


#' Add KWIC lineview to igraph object.
#' 
#' @param graph An igraph object.
#' @param subcorpus A partition.
#' @param left Left context.
#' @param right Right context.
#' @param verbose Logical.
#' @param progress Logical.
#' @importFrom igraph E<-
#' @importFrom stats setNames
#' @importFrom polmineR context registry_get_encoding as.corpusEnc highlight
#' @examples
#' am2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' G <- igraph_add_kwic(merkel2008, subcorpus = am2008)
#' @export igraph_add_kwic
igraph_add_kwic <- function(graph, subcorpus, left = 5L, right = 5L, verbose = TRUE, progress = TRUE){
  
  message("... getting context of nodes")
  context_list <- pblapply(
    setNames(V(graph)$name, V(graph)$name),
    function(q) context(subcorpus, query = q, left = left * 2L, right = right * 2L, verbose = FALSE)
  )

  if (progress) message("... getting kwic for nodes")
  .get_kwic_for_nodes <- function(x){
    x@cpos <- subset(x@cpos, x@cpos[["position"]] >= -left)
    x@cpos <- subset(x@cpos, x@cpos[["position"]] <= right)
    vec <- as.character(kwic(x), fmt = '<span style="background-color:yellow">%s</span>')
    el <- paste(vec, collapse = "<br/>")
    unlist(el)
  }
  V(graph)$info <- pblapply(context_list, .get_kwic_for_nodes)
  
  if (progress) message("... creating edge data")
  enc <- registry_get_encoding("GERMAPARL")
  edge_data <- pblapply(
    strsplit(attr(E(graph), "vnames"), "\\|"),
    function(edge_names){
      context_min <- polmineR:::trim(
        context_list[[edge_names[1]]],
        positivelist = as.corpusEnc(edge_names[2], corpusEnc = enc),
        verbose = FALSE
      )
      if (is.null(context_min)){
        return("")
      } else {
        K <- kwic(context_min, verbose = FALSE)
        K <- highlight(K, yellow = edge_names[2])
        y <- as.character(K, fmt = '<b style="background-color:yellow">%s</b>')
        y <- paste(y, collapse = "</br>")
        return(y)
      }
    }
  )
  E(graph)$info <- unlist(edge_data)
  
  graph
}


#' Perform action on igraph object 
#' 
#' @param x An \code{igraph} object that has been annotated.
#' @export igraph_perform_action
igraph_perform_action <- function(x){
  i <- which(V(x)$action == "drop")
  vertices_to_drop <- V(x)[i]
  y <- delete_vertices(x, vertices_to_drop)
  
  i <- which(E(y)$action == "drop")
  edges_to_drop <- V(y)[i]
  delete_vertices(y, edges_to_drop)
  y
}

