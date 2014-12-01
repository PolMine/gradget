#' @import XML
#' @import methods
#' @import polmineR
#' @import igraph
#' @include generics.R igraph.R 
NULL


#' Turn collocations object into igraph
#' 
#' @param x an object
#' @param edgeAttributes attributes of edges to maintain
#' @param verticeAttributes attributes of vertices to maintain
#' @param as.undirected logical, whether to turn object into directed graph
#' @importClassesFrom polmineR collocations
#' @exportMethod asIgraph
#' @rdname asIgraph
#' @aliases asIgraph asIgraph,collocations-method asIgraph,keynessCollocations-method
setMethod("asIgraph", "collocations", function(x, edgeAttributes="ll", verticeAttributes="tf", as.undirected=TRUE){
  if (!all(edgeAttributes %in% colnames(x@stat))) warning("edgeAttribute supplied is not available")
  tab <- x@stat
  for (what in c("node", "collocate")){
     tab[,what] <- iconv(tab[,what], from=x@encoding, to="UTF-8")  
  }
  g <- graph.data.frame(tab[,c("node","collocate", edgeAttributes)])
  if ("tf" %in% verticeAttributes){
    tf <- get(x@partition, ".GlobalEnv")@tf[[x@pAttribute]] # this will be a data.frame
    tfVector <- setNames(
      tf[,"tf"],
      iconv(rownames(tf), from=x@encoding, to="UTF-8")
    )
    V(g)$tfAbs <- tfVector[V(g)]
    V(g)$tfRel <- round((tfVector[V(g)]/get(x@partition, ".GlobalEnv")@size)*100000, 3)
  }
  if (as.undirected == TRUE) g <- trim(g, as.undirected=TRUE)
  g <- delete.vertices(g, V(g)[name == "\u0084"])
  g <- delete.vertices(g, V(g)[name == "\u0093"])
  attr(g, "partition") <- x@partition
  return(g)
})



#' Make svg for collocations graph
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
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_speaker="Angela Merkel", text_type="speech"), tf="word")
#' bt17merkelColl <- collocations(bt17merkel, pAttribute="word", mc=TRUE)
#' bt17merkelCollTrimmed <- trim(bt17merkelColl, cutoff=list(ll=50))
#' iMerkel <- asIgraph(bt17merkelCollTrimmed)
#' iMerkelComm <- enrich(iMerkel, community=list(method="fastgreedy", weights=FALSE))
#' merkelSvg <- asSvg(iMerkelComm, width=1000, height=1000)
#' }
#' @rdname asSvg
#' @aliases asSvg asSvg,collocations-method plot,collocations-method
#' @exportMethod asSvg
setMethod(
  "asSvg", "collocations",
  function(
    object, layout="kamada.kawai", verbose=TRUE, ...
  ){
    if (verbose == TRUE) message("... creating igraph object (step 1)")
    igraphObject <- asIgraph(object, ...)
    if (verbose == TRUE) message("... creating svg object (step 2)")
    svgObject <- asSvg(igraphObject, verbose=verbose, ...)
    svgObject
  })

setMethod("plot", "collocations", function(x, y=NULL, ...){
  toBePlotted <- asSvg(x)
  plot(toBePlotted)
})
