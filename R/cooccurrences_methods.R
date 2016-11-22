#' @import methods
#' @import polmineR
#' @importFrom igraph graph.data.frame delete.vertices
#' @include generics.R igraph_methods.R 
NULL


#' Turn cooccurrences object into igraph
#' 
#' @param x an object
#' @param edgeAttributes attributes of edges to maintain
#' @param verticeAttributes attributes of vertices to maintain
#' @param as.undirected logical, whether to turn object into directed graph
#' @exportMethod asIgraph
#' @rdname asIgraph
setMethod("asIgraph", "cooccurrences", function(x, edgeAttributes = "ll", verticeAttributes = NULL, as.undirected = TRUE){
  if (!all(edgeAttributes %in% colnames(x@stat))) warning("edgeAttribute supplied is not available")
  tab <- as.data.frame(x)
  aColsStr <- paste("a_", x@pAttribute, sep = "")
  bColsStr <- paste("b_", x@pAttribute, sep = "")
  tab[["node"]] <- apply(tab, 1, function(x) paste(x[aColsStr], collapse="//"))
  tab[["collocate"]] <- apply(tab, 1, function(x) paste(x[bColsStr], collapse="//"))
#   for (what in c("node", "collocate")){
#      tab[,what] <- iconv(tab[,what], from=x@encoding, to="UTF-8")  
#   }
  g <- graph.data.frame(tab[,c("node","collocate", edgeAttributes)])
  if ("count" %in% verticeAttributes){
    TF <- get(x@partition, ".GlobalEnv")@stat # this will be a data.frame
    TF[, key := apply(TF, 1, function(row) paste(row[x@pAttribute], collapse="//"))]
    setkey(TF, key)
    tfVector <- TF[names(V(g))][["count"]]
    V(g)$count <- tfVector
    V(g)$freq <- round((tfVector/get(x@partition, ".GlobalEnv")@size)*100000, 3)
  }
  if (as.undirected == TRUE) g <- trim(g, as.undirected=TRUE)
  g <- delete.vertices(g, V(g)[name == "\u0084"])
  g <- delete.vertices(g, V(g)[name == "\u0093"])
  attr(g, "partition") <- x@partition
  return(g)
})



#' Make svg for cooccurrences graph
#' 
#' A story to be told
#' 
#' @param object the collocation object
#' @param layout either "kamada.kawai" or "fruchterman.reingold"
#' @param width the width of the svg
#' @param height the height of the svg
#' @param margin margins of the svg
#' @param fontSize font size of the vertex labels
#' @param textOffset where to put text
#' @param edgeAttributes attributes of edges for tooltips
#' @param verticeAttributes attributes of attributes for tooltips
#' @param pandocTab logical, whether to format tables with pandoc
#' @param mc logical, whether to use multicore
#' @param returnXML logical, whether to return XML
#' @param verbose whether to be talkative
#' @param x a svg object
#' @param ... parameters that will be passed
#' @importFrom parallel mcparallel mccollect
#' @importFrom htmltools html_print HTML
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(polmineR.graph)
#' bt17merkel <- partition(
#'   "PLPRTXT", text_lp="17", text_speaker="Angela Merkel", text_type="speech",
#'   pAttribute="word"
#'   )
#' bt17merkelColl <- cooccurrences(bt17merkel, pAttribute="word", mc=TRUE)
#' bt17merkelCollTrimmed <- subset(bt17merkelColl, ll >=150)
#' iMerkel <- asIgraph(bt17merkelCollTrimmed)
#' iMerkelComm <- enrich(iMerkel, community=list(method="fastgreedy", weights=FALSE))
#' merkelSvg <- as.svg(iMerkelComm, width=1000, height=1000)
#' merkelSvg
#' }
#' @rdname as.svg
#' @exportMethod as.svg
setMethod(
  "as.svg", "cooccurrences",
  function(
    object, layout="kamada.kawai", verbose=TRUE, ...
  ){
    if (verbose == TRUE) message("... creating igraph object (step 1)")
    igraphObject <- asIgraph(object, ...)
    if (verbose == TRUE) message("... creating svg object (step 2)")
    svgObject <- as.svg(igraphObject, verbose=verbose, ...)
    svgObject
  })

setMethod("plot", "cooccurrences", function(x, y=NULL, ...){
  toBePlotted <- as.svg(x)
  plot(toBePlotted)
})
