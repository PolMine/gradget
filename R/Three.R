#' Generate and manage 3d graph.
#' 
#' Generate 3d graph with three.js
#' 
#' @field type display mode
#' @field bgColor a hex value
#' @field nodeSize size of nods
#' @field edgeColor a hex value
#' @field edgeWidth defaults to 5
#' @field fontColor a hex value
#' @field fontSize defaults to 20
#' @field fontOffset a list
#' 
#' @name Three
#' @rdname Three
#' @export Three
#' @importFrom rjson toJSON
#' @importFrom three three points light text
#' @importFrom igraph V E
#' @import three
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(rjson)
#' 
#' merkel2008 <- partition("PLPRBT", speaker_name = "Angela Merkel", speaker_year = "2008", speaker_type = "speech", pAttribute = "word")
#' termsToDrop <- c(polmineR::punctuation, unlist(noise(pAttributes(merkel2008, pAttribute = "word"))))
#' 
#' Merkel <- Cooccurrences$new(partition = merkel2008, pAttribute = "word", window = 5L, drop = termsToDrop)
#' Merkel$count()
#' Merkel$trim(action = "drop", by.id = TRUE)
#' Merkel$maths()
#' 
#' Merkel$dt <- Merkel$dt[1:250]
#' 
#' G <- Merkel$as.igraph()
#' G <- addCoordinates(G, layout = "kamada.kawai", dim = 3)
#' G <- addCommunities(G)
#' G <- rescale(G, -275, 275)
#' 
#' T <- Three$new(G, dir = "/Users/blaette/Lab/tmp/three")
#' T$fontSize <- 18
#' T$edgeWidth <- 6
#' T$edgeColor <- "0x666666"
#' T$bgColor <- "0x000000"
#' T$fontColor <- "0xffffff"
#' T$type <- "anaglyph"
#' T$make()
#' T$browse()
#' }
Three <- setRefClass(
  
  Class = "Three",
  
  fields = list(
    
    igraph = "igraph",
    type = "character",
    bgColor = "character",
    nodeSize = "numeric",
    edgeColor = "character",
    edgeWidth = "numeric",
    fontSize = "numeric",
    fontColor = "character",
    fontOffset = "numeric",
    jsUrlPrefix = "character",
    adjust = "list",
    dir = "character",
    three = "three"
    
  ),
  
  methods = list(
    
    initialize = function(x, dir = character()){
      
      "Create new Three object."
      
      .self$igraph <- x
      .self$type = "base"
      .self$bgColor = "0xcccccc"
      .self$nodeSize = 5
      .self$edgeColor = "0xeeeeee"
      .self$edgeWidth = 5
      .self$fontSize = 12 
      .self$fontColor = "0x000000"
      .self$fontOffset = c(x=10,y=10,z=10)
      .self$jsUrlPrefix = character()
      .self$adjust = list()
      .self$dir = dir
      
    },
    
    rescale = function(){},
    
    store = function(directory = NULL){
      if (length(.self$dir) > 0 && is.null(directory)) directory <-  .self$dir
      message("storing at dir: ", directory)
      three::store(.self$three, directory = directory) 
    },
    
    browse = function(){
      filenames <- T$store()
      utils::browseURL(filenames["tmpFileJs"])
    },
    
    
    make = function(){
      
      "Fill three slot."
      
      if (is.null(V(.self$igraph)$z)) warning("coordinates for threedimensional display are not available")
      threeObject <- three(.self$type, .self$bgColor, NULL, .self$adjust)
      if (is.null(V(.self$igraph)$color)){
        color <- "0xcccccc"
      } else {
        color <- V(.self$igraph)$color
      }
      threeObject <- three::points(
        threeObject,
        coords = data.frame(x = V(.self$igraph)$x, y = V(.self$igraph)$y, z = V(.self$igraph)$z),
        size = .self$nodeSize, color = color
      )
      vertexAttributes <- list.vertex.attributes(.self$igraph)
      jsonVertexAttributes <- vertexAttributes[which(!vertexAttributes %in% c("name", "x", "y", "z", "color"))]
      threeObject@json[["vertexData"]] <- toJSON(
        lapply(
          setNames(jsonVertexAttributes, jsonVertexAttributes),
          function(name) get.vertex.attribute(.self$igraph, name)
        ))  
      edgelistId <- get.edgelist(.self$igraph, names = FALSE)
      threeObject <- lines(
        threeObject,
        from = data.frame(x=V(.self$igraph)[edgelistId[,1]]$x, y=V(.self$igraph)[edgelistId[,1]]$y, z=V(.self$igraph)[edgelistId[,1]]$z),
        to = data.frame(x=V(.self$igraph)[edgelistId[,2]]$x, y=V(.self$igraph)[edgelistId[,2]]$y, z=V(.self$igraph)[edgelistId[,2]]$z),
        color = .self$edgeColor, lwd = .self$edgeWidth
      )
      edgeAttributes <- list.edge.attributes(.self$igraph)
      if ("ll" %in% edgeAttributes){
        threeObject@json[["edgeData"]] <- toJSON(
          list(
            a = get.edgelist(.self$igraph)[,1],
            b = get.edgelist(.self$igraph)[,2],
            a2b = sapply(get.edge.attribute(.self$igraph, "ll"), function(x) x[1]),
            b2a = sapply(get.edge.attribute(.self$igraph, "ll"), function(x) x[2])
          ))
      }
      threeObject <- text(
        threeObject,
        data.frame(
          x = V(.self$igraph)$x+8, y = V(.self$igraph)$y, z = V(.self$igraph)$z,
          row.names=V(.self$igraph)$name
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
      .self$three <- threeObject
    },
    
    as.json = function(){
      
      "Turn into json."
      
      paste(
        unlist(lapply(
          names(.self$three@json),
          function(name){ paste(name, " = ", .self$three@json[[name]], ";", sep="") })
        ),
        collapse = "\n"
      )    
    }
  )
)

