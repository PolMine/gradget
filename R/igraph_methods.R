#' @include generics.R
#' @import RColorBrewer
#' @importFrom igraph layout.fruchterman.reingold layout.kamada.kawai layout.spring layout.lgl layout.graphopt edge.betweenness.community is.directed
#' @importFrom XML addChildren newXMLTextNode newXMLNode xmlTreeParse
NULL



setOldClass("igraph")

.normalizeCoords <- function(igraphObject, width, height, margin){
  coords <- cbind(V(igraphObject)$x, V(igraphObject)$y)
  normalize <- function(x) {
    x <- x - min(x)
    x / max(x)
  }
  coords <- apply(coords, 2, normalize)
  V(igraphObject)$x <- round(coords[,1] * (width - 2*margin) + margin, 0)
  V(igraphObject)$y <- round(coords[,2] * (height - 2*margin) + margin, 0)
  return(igraphObject)
}

.makeCoords <- function(igraphObject, layout, dim){
  if (!dim %in% c(2,3)) warning("numer of dimensions is not valid")
  if (layout=="fruchterman.reingold"){
    coords <- layout.fruchterman.reingold(igraphObject, dim=dim)    
  } else if (layout=="kamada.kawai"){
    coords <- layout.kamada.kawai(igraphObject, dim=dim)
  } else if (layout=="spring"){
    coords <- layout.spring(igraphObject, dim=dim)
  } else if (layout=="lgl"){
    coords <- layout.lgl(igraphObject, dim=dim)
  } else if (layout=="graphopt"){
    coords <- layout.graphopt(igraphObject, dim=dim)
  }
  V(igraphObject)$x <- coords[,1]
  V(igraphObject)$y <- coords[,2]
  if (dim == 3){
    V(igraphObject)$z <- coords[,3]
  }
  return(igraphObject)
}

#' @importFrom pander pandoc.table.return
.circleNodes <- function(igraphObject, radius=list(minSize=3, tf=TRUE), returnXML=TRUE, pandocTab=TRUE){
  if (is.null(V(igraphObject)$color)) V(igraphObject)$color <- "blue"
  if (radius[["tf"]] == TRUE){
    if (!is.null(V(igraphObject)$count)){
      rad <- radius[["minSize"]] + sqrt(sqrt(V(igraphObject)$count/3.14159))  
    } else {
      rad <- rep(radius[["minSize"]], times=length(V(igraphObject)))
    }
    
  } else {
    rad <- rep(radius[["minSize"]], times=length(V(igraphObject)))
  }
  .circleNode <- function(i){
    community <- as.character(
      ifelse(
        is.null(V(igraphObject)$community[i]),
        0,
        V(igraphObject)$community[i])
    )
    if (pandocTab == TRUE) {
      tab <- data.frame(
        c("tf/abs", "tf/rel", "community"),
        c(V(igraphObject)[i]$count, V(igraphObject)[i]$freq, community)
      )
      colnames(tab) <- c("token", V(igraphObject)[i]$name)
      tooltipTab <- pandoc.table.return(tab, justify=c("left", "left"))
      tooltipTab <- gsub("^\n(.*?)\n\n$", "\\1", tooltipTab)
    } else {
      tooltipTab <- paste(
        V(igraphObject)[i]$name, "\n",
        "count: ", as.character(V(igraphObject)[i]$count), "\n",
        "freq: ", as.character(V(igraphObject)[i]$tfRel), "\n",
        "community: ", community,
        sep="")
    }
    circleNode <- newXMLNode(
      "circle",
      attrs=c(
        r=rad[i],
        stroke="black",
        fill=V(igraphObject)[i]$color,
        cx=as.character(V(igraphObject)[i]$x),
        cy=as.character(V(igraphObject)[i]$y),
        nodeId=as.character(i),
        token=V(igraphObject)[i]$name,
        count=as.character(V(igraphObject)[i]$tfAbs),
        freq=as.character(V(igraphObject)[i]$tfRel),
        community=community
      )
    )
    tooltipNode <- newXMLNode(
      "title",
      attrs=c(style="letter-spacing:4px;background-color: yellow;")
    )
    newXMLTextNode(
      tooltipTab,      
      tooltipNode
    )
    addChildren(circleNode, tooltipNode)
    return(circleNode)
  }
  retval <- lapply(1:length(V(igraphObject)), .circleNode)
  if (returnXML == FALSE) retval <- paste(lapply(retval, saveXML), collapse="\n")
  return(retval)
}

.lineNodes <- function(igraphObject, returnXML=TRUE){
  edgelistId <- get.edgelist(igraphObject, names=FALSE)
  edgelistString <- get.edgelist(igraphObject, names=TRUE)
  .lineNode <- function(i){
    linkNode <- newXMLNode(
      "a",
      attrs=c(
        onClick=paste("edgeClick(x='", edgelistString[i,1], "', y='", edgelistString[i,2], "')", sep="")
      ),
#      attrs=c(
#        "xlink:href"=paste(
#          "http://localhost/cgi-bin/R/graph2kwic?",
#          "partition=", attr(igraphObject, "partition"), "__node=", edgelistString[i,1],
#          "__collocate=", edgelistString[i,2], sep="")
#      ),
      suppressNamespaceWarning = TRUE
    )
    llValues <- as.character(round(unlist(E(igraphObject)$ll[i]), 2))
    newLineNode <- newXMLNode(
      "line",
      attrs=c(
        style="stroke:black; stroke-width:1px; fill:none;",
        x1=as.character(V(igraphObject)[edgelistId[i,1]]$x),
        y1=as.character(V(igraphObject)[edgelistId[i,1]]$y),
        x2=as.character(V(igraphObject)[edgelistId[i,2]]$x),
        y2=as.character(V(igraphObject)[edgelistId[i,2]]$y),
        from=as.character(edgelistId[i,1]),
        to=as.character(edgelistId[i,2]),
        llXY=llValues[1],
        llYX=ifelse(is.na(llValues[2]), "NA", llValues[2])
      )
    )
    tooltipNode <- newXMLNode("title")
    labelStat <- paste(llValues, collapse=" / ")
    newXMLTextNode(paste(edgelistString[i,1], " - ", edgelistString[i,2], " (ll: ", labelStat, ")", sep=""), tooltipNode)
    addChildren(newLineNode, tooltipNode)
    addChildren(linkNode,newLineNode)
    linkNode
  }
  retval <- lapply(c(1:nrow(edgelistId)), .lineNode)    
  if (returnXML == FALSE) retval <- paste(lapply(retval, saveXML), collapse="\n")
  return(retval)
}



.textNodes <- function(igraphObject, fontSize=8, textOffset=3, returnXML=TRUE){
  .textNode <- function(i){
    linkNode <- newXMLNode(
      "a",
      # attrs=c("xlink:href"=paste("http://localhost/cgi-bin/R/graph2kwic?", "partition=", attr(igraphObject, "partition"), "__node=", V(igraphObject)[i]$name, sep="")),
      attrs=c("onClick"=paste("nodeClick(x='", V(igraphObject)[i]$name, "')", sep="")),
      suppressNamespaceWarning=TRUE
    )
    textNode <- newXMLNode(
      "text",
      attrs=c(
        fill="red",
        style=paste("font-size:", as.character(fontSize), "px;font-family:sans-serif", sep=""),
        x=V(igraphObject)[i]$x + textOffset,
        y=V(igraphObject)[i]$y - textOffset,
        nodeId=as.character(i)
      )
    )
    newXMLTextNode(V(igraphObject)[i]$name, parent=textNode)
    addChildren(linkNode, textNode)
    linkNode
  }
  retval <- lapply(1:length(V(igraphObject)), .textNode)    
  if (returnXML == FALSE) retval <- paste(lapply(retval, saveXML), collapse="\n")
  return(retval)
}

#' Make svg for collocations graph
#' 
#' The resulting svg graph will have clickable nodes and edges. To use this functionality
#' you need to install FastRWeb and start it with sudo /var/FastRWeb/code/start
#' 
#' @examples
#' \dontrun{
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_speaker="Angela Merkel", text_type="speech"), tf="word")
#' save(bt17merkel, file="/Users/blaette/Lab/polmineR.data/partitionDir/bt17merkel.RData")
#' bt17merkelColl <- collocations(bt17merkel, pAttribute="word", mc=TRUE)
#' bt17merkelCollTrimmed <- trim(bt17merkelColl, cutoff=list(ll=50, collocateWindowFreq=2))
#' iMerkel <- asIgraph(bt17merkelCollTrimmed)
#' iMerkel <- enrich(iMerkel, community=list(method="fastgreedy", weights=FALSE))
#' merkelSvg <- as.svg(iMerkel, width=1000, height=1000)
#' show(merkelSvg)
#' }
#' @rdname as.svg
#' @aliases as.svg as.svg,collocations-method browse,svg-method browse plot,igraph-method plot,svg-method
#' @exportMethod as.svg
setMethod(
  "as.svg", "igraph",
  function(
    object, layout="kamada.kawai", width=400, height=400, margin=50,
    fontSize=8, textOffset=5, edgeAttributes="ll", verticeAttributes="tf",
    pandocTab=TRUE,
    mc=FALSE, returnXML=FALSE, verbose=TRUE
  ){
    if (!is.null(layout)){
      if (verbose==TRUE) message("... calculate coordinates")
      object <- enrich(object, layout=layout)
      object <- trim(object, size=list(width=width, height=height, margin=margin))
    }
    if (mc == FALSE) {
      doc <- xmlTreeParse(
        '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>',
        useInternalNodes=TRUE, asText=TRUE, encoding="UTF-8"
      )
      docRoot <- xmlRoot(doc)
      if (verbose==TRUE) message("... generating edges")
      lineNodes <- .lineNodes(object)
      if (verbose==TRUE) message("... generating nodes")
      circleNodes <- .circleNodes(object, pandocTab=pandocTab)
      if (verbose==TRUE) message("... generating text")
      textNodes <- .textNodes(object, fontSize, textOffset)  
      if (verbose==TRUE) message("... putting together svg")
      foo <- addChildren(docRoot, kids=lineNodes)
      foo <- addChildren(docRoot, kids=circleNodes)  
      foo <- addChildren(docRoot, kids=textNodes)
    } else if (mc == TRUE){
      if (verbose==TRUE) message("... creating XML")
      lineNodes <- mcparallel(.lineNodes(object, returnXML=FALSE))
      circleNodes <- mcparallel(.circleNodes(object, returnXML=FALSE))
      textNodes <- mcparallel(.textNodes(object, fontSize, textOffset, returnXML=FALSE))
      bag <- mccollect(list(lineNodes, circleNodes, textNodes), wait=TRUE)
      names(bag) <- c("lineNodes", "circleNodes", "textNodes")
      rawXML <- sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">%s%s%s</svg>',
       bag$lineNodes, bag$circleNodes, bag$textNodes
      )
      doc <- xmlTreeParse(rawXML, useInternalNodes=TRUE, asText=TRUE, encoding="UTF-8")
      docRoot <- xmlRoot(doc)
    }  
    xmlAttrs(docRoot) <- c(width=as.character(width), height=as.character(height))
    if (returnXML==FALSE){
      retval <- new(
        "svg",
        xml=doc, igraph=object,
        width=width, height=height, margin=margin,
        fontSize=fontSize, textOffset=textOffset
      )
    } else {
      retval <- doc
    }
    return(retval)
  }
)




# setMethod("as.igraph", "keyness", function(x, edgeAttributes="ll"){
#   graph.data.frame(x@stat[,c("term1", "term2", edgeAttributes)])
# })


.communityDetection <- function(igraphObject, method = "fastgreedy", weights = FALSE){
  colors <- c(
    brewer.pal(9, "Set1"),
    brewer.pal(8, "Set2"),
    brewer.pal(12, "Set3"),
    brewer.pal(8, "Accent")
  )
  if (method=="fastgreedy"){
    if (is.directed(igraphObject) == TRUE){
      igraphObject <- trim(igraphObject, as.undirected = TRUE)
    }
    if (weights == TRUE){
      fgComm <- fastgreedy.community(igraphObject, weights=E(igraphObject)$ll)  
    } else if (weights == FALSE){
#      fgComm <- fastgreedy.community(igraphObject)
      fgComm <- cluster_fast_greedy(igraphObject)
    }
#     mem <- community.to.membership(
#       igraphObject,
#       fgComm$merges,
#       steps=which.max(fgComm$modularity)-1
#     )
#    V(igraphObject)$community <- mem$membership + 1
    mem <- membership(fgComm)
    V(igraphObject)$community <- mem
    colors <- rep(colors, times=ceiling(max(V(igraphObject)$community)/length(colors)))
    V(igraphObject)$color <- colors[mem + 1]
  } else if (method == "edge.betweenness"){
    if (weights == TRUE){
      ebc <- edge.betweenness.community(
        igraphObject,
        directed=is.directed(igraphObject),
        weights=E(igraphObject)$ll
      )
    }
  } else if (method == "multilevel"){
    com <- multilevel.community(igraphObject)
  }
  return(igraphObject)
}


setMethod("enrich", "igraph", function(object, layout=NULL, dim=2, community=NULL){
  if (!is.null(layout)){
    object <- .makeCoords(object, layout=layout, dim=dim)
  }
  if (!is.null(community)){
    object <- .communityDetection(object, method=community[["method"]], weights=community[["weights"]])
  }
  return(object)
})

#' @importMethodsFrom polmineR trim
setMethod("trim", "igraph", function(object, size=NULL, communityNo=NULL, neighbors=NULL, as.undirected=FALSE, simplify=FALSE){
  if (!is.null(size)){
    trimmedObject <- .normalizeCoords(object, size[["width"]], size[["height"]], size[["margin"]])
  }
  if (!is.null(communityNo)){
    trimmedObject <- delete.vertices(
      object,
      V(object)[which(! V(object)$community %in% communityNo)]
    )
  }
  if (!is.null(neighbors)){
    toKeep <- unlist(neighborhood(
      object,
      order=neighbors[["order"]],
      nodes=neighbors[["token"]]
    ))
    trimmedObject <- delete.vertices(object, which(!c(1:length(V(object))) %in% toKeep))
  }
  if (as.undirected == TRUE){
    trimmedObject <- as.undirected(object, edge.attr.comb="concat")
  }
  return(trimmedObject)
})

setMethod("plot", "igraph", function(x, y=NULL, ...){
  toBePlotted <- as.svg(x, ...)
  plot(toBePlotted)
})


