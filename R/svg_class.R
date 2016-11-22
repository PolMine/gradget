#' @include generics.R
NULL

setClass(
  "svg",
  slots = c(
    xml = "XMLInternalDocument",
    igraph = "igraph",
    width = "numeric",
    height = "numeric",
    margin = "numeric",
    fontSize = "numeric",
    textOffset = "numeric"
    )
  )



#' @rdname as.svg
setMethod("print", "svg", function(x) saveXML(x@xml))

#' @rdname as.svg
setMethod("as.character", "svg", function(x) saveXML(x@xml))


setMethod("html", "svg", function(object){
  docString <- as.character(object)
  docHtml <- htmltools::HTML(docString)
  jsFunction <- "
  <script>
  function edgeClick(x, y){
    console.log(x);
    console.log(y);
    Shiny.onInputChange('kwic_query_edgeclick', x);
    Shiny.onInputChange('kwic_neighbor_edgeclick', y);
  };
  function nodeClick(x){
    console.log(x);
    Shiny.onInputChange('kwic_query_nodeclick', x);
  };
  </script>
  "
  gsub("<svg", paste(jsFunction, "<svg", sep=""), docHtml)
})

#' @importFrom htmltools HTML html_print
#' @rdname as.svg
setMethod("show", "svg", function(object){
  docHtml <- html(object)
  htmltools::html_print(docHtml)
})

#' @exportMethod browse
#' @rdname as.svg
setMethod("browse", "svg", function(object){
  tmpFile <- tempfile(fileext=".html")
  docString <- saveXML(object@xml)
  docHtml <- HTML(docString)
  cat(docHtml, file=tmpFile)
  browseURL(tmpFile)
})


.selectCommunity <- function(object, community){
  xmlDoc <- xmlClone(object@xml)
  root <- xmlRoot(xmlDoc)
  circleNodes <- unlist(lapply(
    community,
    function(com){
      getNodeSet(xmlDoc, path=paste('//circle[@community="', as.character(com),'"]', sep=""))
    }))
  nodeIds <- sapply(circleNodes, function(x) xmlAttrs(x)["nodeId"])
  textNodes <- xpathSApply(xmlDoc, "//text", function(node) {
    if (xmlAttrs(node)["nodeId"] %in% nodeIds) xmlParent(node)
  })
  textNodes[sapply(textNodes, is.null)] <- NULL
  lineNodes <- xpathSApply(
    xmlDoc, "//line",
    function(node){
      from=xmlAttrs(node)["from"]
      to=xmlAttrs(node)["to"]
      if (from %in% nodeIds && to %in% nodeIds) xmlParent(node)
    })
  lineNodes[sapply(lineNodes, is.null)] <- NULL
  svgRoot <- xmlElementsByTagName(xmlDoc, "svg")[[1]]
  foo <- removeChildren(svgRoot, kids=xmlChildren(svgRoot))
  foo <- addChildren(svgRoot, kids=textNodes)
  foo <- addChildren(svgRoot, kids=lineNodes)
  foo <- addChildren(svgRoot, kids=circleNodes)
  object@xml <- xmlDoc
  return(object)
}

setMethod("trim", "svg", function(object, community=NULL, neighbors=NULL){
  doc <- object@xml
  if (!is.null(community)){
    trimmedObject <- .selectCommunity(object, community)
  }
  if (!is.null(neighbors)){
    trimmedIgraph <- trim(asIgraph(object), neighbors=neighbors)
    trimmedObject <- as.svg(
      trimmedIgraph,
      neighbors=neighbors
      )
  }
  return(trimmedObject)
})

#' @exportMethod plot
setMethod("plot", "svg", function(x,y=NULL, ...){
  show(x)
})

#' @rdname asIgraph
setMethod("asIgraph", "svg", function(x, as.undirected=TRUE){
  nodeTab <- do.call(rbind, xpathApply(x@xml, "//circle", xmlAttrs))
  rownames(nodeTab) <- nodeTab[,"token"]
  id2token <- setNames(nodeTab[,"token"], nodeTab[,"nodeId"])
#  count <- setNames(as.integer(nodeTab[,"count"]), nodeTab[,"token"])
#  freq <- setNames(as.integer(nodeTab[,"tfRel"]), nodeTab[,"token"])
#  if ("community" %in% colnames(nodeTab)) community <- setNames(as.integer(nodeTab[,"community"]), nodeTab[,"token"])
#  color <- setNames(nodeTab[,"fill"], nodeTab[,"token"])
  edgesPrep <- xpathSApply(
    x@xml, "//line",
    function(node){
      eAttr <- xmlAttrs(node)
      retval <- list(
        c(id2token[eAttr["from"]], id2token[eAttr["to"]], as.numeric(eAttr["llXY"]))
        )
      if (eAttr["llYX"] != "NA"){
        retval[[2]] <- c(id2token[eAttr["to"]], id2token[eAttr["from"]], eAttr["llYX"])
      }
      retval
    })
  edgesTab <- matrix(unlist(edgesPrep), ncol=3, byrow=TRUE)
  edgesDf <- data.frame(node=edgesTab[,1], collocate=edgesTab[,2], ll=as.numeric(edgesTab[,3]))
  g <- graph.data.frame(edgesDf)
  V(g)$count <- as.integer(nodeTab[vertex.attributes(g)$name, "count"])
  V(g)$freq <- as.numeric(nodeTab[vertex.attributes(g)$name, "freq"])
  V(g)$community <- as.numeric(nodeTab[vertex.attributes(g)$name, "community"])
  V(g)$color <- nodeTab[vertex.attributes(g)$name, "fill"]
  if (as.undirected == TRUE) g <- trim(g, as.undirected=TRUE)
  return(g)
})
