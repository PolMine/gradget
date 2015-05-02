setOldClass("igraph")

setGeneric("as.three", function(object, ...) standardGeneric("as.three"))

#' generate 3d graph
#' 
#' generate 3d graph with three.js
#' 
#' @param object the object
#' @param type display mode
#' @param bgColor a hex value
#' @param nodeSize size of nods
#' @param edgeColor a hex value
#' @param edgeWidth defaults to 5
#' @param fontColor a hex value
#' @param fontSize defaults to 20
#' @param fontOffset a list
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(polmineR.graph)
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_speaker="Angela Merkel", text_type="speech"), tf="word")
#' bt17merkelColl <- collocations(bt17merkel, pAttribute="word", mc=TRUE)
#' bt17merkelCollTrimmed <- trim(bt17merkelColl, cutoff=list(ll=50))
#' iMerkel <- asIgraph(bt17merkelCollTrimmed)
#' iMerkelComm <- enrich(iMerkel, community=list(method="fastgreedy", weights=FALSE))
#' iMerkel3d <- enrich(iMerkelComm, layout="kamada.kawai", dim=3)
#' iMerkel3d <- rescale(iMerkel3d, -400, 400)
#' as.three(iMerkel3d, bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' as.three(iMerkel3d, type="raycaster", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' as.three(iMerkel3d, type="anaglyph", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' }
#' @name as.three
#' @aliases as.three,igraph-method
#' @rdname as.three
#' @exportMethod as.three
#' @importFrom rjson toJSON
setMethod(
  "as.three", "igraph",
  function(
    object, type="base", bgColor="0xffffff", nodeSize=5,
    edgeColor="0x000000", edgeWidth=5,
    fontSize=20, fontColor="0x000000", fontOffset=c(x=5, y=5, z=5),
    jsUrlPrefix=NULL
  ){
    if (is.null(V(object)$z)) warning("coordinates for threedimensional display are not available")
    threeObject <- three(type, bgColor, jsUrlPrefix)
    if (is.null(V(object)$color)){
      color <- "0xcccccc"
    } else {
      color <- V(object)$color
    }
    threeObject <- points(
      threeObject,
      coords=data.frame(
        x=V(object)$x, y=V(object)$y, z=V(object)$z
      ),
      size=nodeSize, color=color
    )
    vertexAttributes <- list.vertex.attributes(object)
    jsonVertexAttributes <- vertexAttributes[which(!vertexAttributes %in% c("name", "x", "y", "z", "color"))]
    threeObject@json[["vertexData"]] <- toJSON(
      lapply(
        setNames(jsonVertexAttributes, jsonVertexAttributes),
        function(name) get.vertex.attribute(object, name)
      ))  
    edgelistId <- get.edgelist(object, names=FALSE)
    threeObject <- lines(
      threeObject,
      from=data.frame(x=V(object)[edgelistId[,1]]$x, y=V(object)[edgelistId[,1]]$y, z=V(object)[edgelistId[,1]]$z),
      to=data.frame(x=V(object)[edgelistId[,2]]$x, y=V(object)[edgelistId[,2]]$y, z=V(object)[edgelistId[,2]]$z),
      color=edgeColor, lwd=edgeWidth
    )
    edgeAttributes <- list.edge.attributes(object)
    if ("ll" %in% edgeAttributes){
      threeObject@json[["edgeData"]] <- toJSON(
        list(
          a=get.edgelist(object)[,1],
          b=get.edgelist(object)[,2],
          a2b=sapply(get.edge.attribute(object, "ll"), function(x) x[1]),
          b2a=sapply(get.edge.attribute(object, "ll"), function(x) x[2])
        ))
    }
    threeObject <- text(
      threeObject,
      data.frame(
        x=V(object)$x+8, y=V(object)$y, z=V(object)$z,
        row.names=V(object)$name
      ),
      color=fontColor, fontSize, offset=fontOffset
    )
    #   threeObject <- light(
    #     threeObject,
    #     pointLight=list(
    #       x=floor(max(V(object)$x)) + 100,
    #       y=floor(max(V(object)$y)) + 100,
    #       z=floor(max(V(object)$z)) + 100
    #     )
    #   )
    threeObject
  })




