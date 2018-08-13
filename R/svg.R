# svg_as_igraph = function(as.undirected = TRUE){
#   nodeTab <- do.call(rbind, xpathApply(self$xml, "//circle", xmlAttrs))
#   rownames(nodeTab) <- nodeTab[,"token"]
#   id2token <- setNames(nodeTab[,"token"], nodeTab[,"nodeId"])
#   #  count <- setNames(as.integer(nodeTab[,"count"]), nodeTab[,"token"])
#   #  freq <- setNames(as.integer(nodeTab[,"tfRel"]), nodeTab[,"token"])
#   #  if ("community" %in% colnames(nodeTab)) community <- setNames(as.integer(nodeTab[,"community"]), nodeTab[,"token"])
#   #  color <- setNames(nodeTab[,"fill"], nodeTab[,"token"])
#   edgesPrep <- xpathSApply(
#     self$xml, "//line",
#     function(node){
#       eAttr <- xmlAttrs(node)
#       retval <- list(
#         c(id2token[eAttr["from"]], id2token[eAttr["to"]], as.numeric(eAttr["llXY"]))
#       )
#       if (eAttr["llYX"] != "NA"){
#         retval[[2]] <- c(id2token[eAttr["to"]], id2token[eAttr["from"]], eAttr["llYX"])
#       }
#       retval
#     })
#   edgesTab <- matrix(unlist(edgesPrep), ncol = 3, byrow = TRUE)
#   edgesDf <- data.frame(node = edgesTab[,1], collocate = edgesTab[,2], ll = as.numeric(edgesTab[,3]))
#   g <- graph.data.frame(edgesDf)
#   V(g)$count <- as.integer(nodeTab[vertex.attributes(g)$name, "count"])
#   V(g)$freq <- as.numeric(nodeTab[vertex.attributes(g)$name, "freq"])
#   V(g)$community <- as.numeric(nodeTab[vertex.attributes(g)$name, "community"])
#   V(g)$color <- nodeTab[vertex.attributes(g)$name, "fill"]
#   if (as.undirected == TRUE) g <- as.undirected(g, edge.attr.comb = "concat")
#   return(g)
# },


# svg_select_community = function(community){
#   xmlDoc <- xmlClone(self$xml)
#   root <- xmlRoot(xmlDoc)
#   circleNodes <- unlist(lapply(
#     community,
#     function(com){
#       getNodeSet(xmlDoc, path=paste('//circle[@community="', as.character(com),'"]', sep=""))
#     }))
#   nodeIds <- sapply(circleNodes, function(x) xmlAttrs(x)["nodeId"])
#   textNodes <- xpathSApply(xmlDoc, "//text", function(node) {
#     if (xmlAttrs(node)["nodeId"] %in% nodeIds) xmlParent(node)
#   })
#   textNodes[sapply(textNodes, is.null)] <- NULL
#   lineNodes <- xpathSApply(
#     xmlDoc, "//line",
#     function(node){
#       from = xmlAttrs(node)["from"]
#       to = xmlAttrs(node)["to"]
#       if (from %in% nodeIds && to %in% nodeIds) xmlParent(node)
#     })
#   lineNodes[sapply(lineNodes, is.null)] <- NULL
#   svgRoot <- xmlElementsByTagName(xmlDoc, "svg")[[1]]
#   foo <- removeChildren(svgRoot, kids=xmlChildren(svgRoot))
#   foo <- addChildren(svgRoot, kids=textNodes)
#   foo <- addChildren(svgRoot, kids=lineNodes)
#   foo <- addChildren(svgRoot, kids=circleNodes)
#   self$xml <- xmlDoc
# },
# 
