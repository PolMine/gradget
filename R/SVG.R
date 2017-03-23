#' Make svg for collocations graph
#' 
#' The resulting svg graph will have clickable nodes and edges. To use this functionality
#' you need to install FastRWeb and start it with sudo /var/FastRWeb/code/start
#' 
#' @field edgeAttributes attributes of edges to maintain
#' @field verticeAttributes attributes of vertices to maintain
#' @field as.undirected logical, whether to turn object into directed graph
#' @field object the collocation object
#' @field  layout either "kamada.kawai" or "fruchterman.reingold"
#' @field  width the width of the svg
#' @field  height the height of the svg
#' @field  margin margins of the svg
#' @field  fontSize font size of the vertex labels
#' @field  textOffset where to put text
#' @field edgeAttributes attributes of edges for tooltips
#' @field verticeAttributes attributes of attributes for tooltips
#' @field  pandocTab logical, whether to format tables with pandoc
#' @importFrom htmltools HTML html_print
#' @importFrom pander pandoc.table.return
#' @examples
#' \dontrun{
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_speaker="Angela Merkel", text_type="speech"), tf="word")
#' }
#' @rdname as.svg
#' @export SVG
#' @importFrom XML xmlRoot
#' @importFrom igraph V
SVG <- setRefClass(
  
  Class = "SVG",
  
  fields = list(
    
    xml = "character",
    igraph = "igraph",
    width = "numeric",
    height = "numeric",
    margin = "numeric",
    fontSize = "numeric",
    textOffset = "numeric",
    edgeAttributes = "character",
    verticeAttributes = "character",
    layout = "character",
    pandocTab = "logical"
    
  ),
  
  methods = list(
    
    initialize = function(x){
      
      "Create new SVG object."
      
      .self$igraph = x
      .self$width = 400
      .self$height = 400
      .self$margin = 50
      .self$fontSize = 8
      .self$textOffset = 5
      .self$edgeAttributes = "ll"
      .self$verticeAttributes = "tf"
      .self$layout = "kamada.kawai"
      .self$pandocTab = TRUE
      
    },
    
    plot = function(){
      
      "Show SVG in viewer pane of RStudio."
      
      if (length(S$xml) == 0) warning("Field 'xml' is empty, nothing to show.")
      htmltools::html_print(html())
      },
    
    browse = function(){
      
      "Show SVG in browser. The xml/svg file is stored in a temporary file, which is
      returned invisibly."
      
      if (length(S$xml) == 0) warning("Field 'xml' is empty, nothing to show.")
      tmpFile <- tempfile(fileext=".html")
      cat(.self$xml, file = tmpFile)
      browseURL(tmpFile)
      invisible(tmpFile)
    },
    

    selectCommunity = function(community){
      
      "Select a community."
      
      xmlDoc <- xmlClone(.self$xml)
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
          from = xmlAttrs(node)["from"]
          to = xmlAttrs(node)["to"]
          if (from %in% nodeIds && to %in% nodeIds) xmlParent(node)
        })
      lineNodes[sapply(lineNodes, is.null)] <- NULL
      svgRoot <- xmlElementsByTagName(xmlDoc, "svg")[[1]]
      foo <- removeChildren(svgRoot, kids=xmlChildren(svgRoot))
      foo <- addChildren(svgRoot, kids=textNodes)
      foo <- addChildren(svgRoot, kids=lineNodes)
      foo <- addChildren(svgRoot, kids=circleNodes)
      .self$xml <- xmlDoc
    },
    
    as.igraph = function(as.undirected = TRUE){
      
      "Turn SVG into igraph object."
      
      nodeTab <- do.call(rbind, xpathApply(.self$xml, "//circle", xmlAttrs))
      rownames(nodeTab) <- nodeTab[,"token"]
      id2token <- setNames(nodeTab[,"token"], nodeTab[,"nodeId"])
      #  count <- setNames(as.integer(nodeTab[,"count"]), nodeTab[,"token"])
      #  freq <- setNames(as.integer(nodeTab[,"tfRel"]), nodeTab[,"token"])
      #  if ("community" %in% colnames(nodeTab)) community <- setNames(as.integer(nodeTab[,"community"]), nodeTab[,"token"])
      #  color <- setNames(nodeTab[,"fill"], nodeTab[,"token"])
      edgesPrep <- xpathSApply(
        .self$xml, "//line",
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
      edgesTab <- matrix(unlist(edgesPrep), ncol = 3, byrow = TRUE)
      edgesDf <- data.frame(node = edgesTab[,1], collocate = edgesTab[,2], ll = as.numeric(edgesTab[,3]))
      g <- graph.data.frame(edgesDf)
      V(g)$count <- as.integer(nodeTab[vertex.attributes(g)$name, "count"])
      V(g)$freq <- as.numeric(nodeTab[vertex.attributes(g)$name, "freq"])
      V(g)$community <- as.numeric(nodeTab[vertex.attributes(g)$name, "community"])
      V(g)$color <- nodeTab[vertex.attributes(g)$name, "fill"]
      if (as.undirected == TRUE) g <- as.undirected(g, edge.attr.comb = "concat")
      return(g)
    },
    
    
    html = function(){
      
      "Turn into html."
      
      docHtml <- htmltools::HTML(.self$xml)
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
      Shiny.onInputChange('kwic_query', x);
      };
      </script>
      "
      gsub("<svg", paste(jsFunction, "<svg", sep=""), docHtml)
    },
    

    makeNodes = function(radius = list(minSize = 3, tf = TRUE), pandocTab = TRUE){
      
      "Make nodes."
      
      if (is.null(V(.self$igraph)$color)) V(.self$igraph)$color <- "blue"
      if (radius[["tf"]] == TRUE){
        if (!is.null(V(.self$igraph)$count)){
          rad <- radius[["minSize"]] + sqrt(sqrt(V(.self$igraph)$count/3.14159))  
        } else {
          rad <- rep(radius[["minSize"]], times=length(V(.self$igraph)))
        }
        
      } else {
        rad <- rep(radius[["minSize"]], times = length(V(.self$igraph)))
      }
      .circleNode <- function(i){
        community <- as.character(
          if (is.null(V(.self$igraph)$community[i])) 0 else V(.self$igraph)$community[i]
          )
        if (pandocTab == TRUE) {
          tab <- data.frame(
            c("tf/abs", "tf/rel", "community"),
            c(V(.self$igraph)[i]$count, V(.self$igraph)[i]$freq, community)
          )
          colnames(tab) <- c("token", V(.self$igraph)[i]$name)
          tooltipTab <- pandoc.table.return(tab, justify=c("left", "left"))
          tooltipTab <- gsub("^\n(.*?)\n\n$", "\\1", tooltipTab)
        } else {
          tooltipTab <- paste(
            V(.self$igraph)[i]$name, "\n",
            "count: ", as.character(V(.self$igraph)[i]$count), "\n",
            "freq: ", as.character(V(.self$igraph)[i]$tfRel), "\n",
            "community: ", community,
            sep="")
        }
        tooltip <- sprintf(
          '<title style="letter-spacing:4px;background-color: yellow;">%s</title>',
          tooltipTab
        )
        sprintf(
          '<circle r="%s" stroke="%s" fill="%s" cx="%s" cy="%s" nodeId="%s" token="%s" count="%s" freq="%s" community="%s">%s</circle>',
          as.character(rad[i]),
          "black",
          V(.self$igraph)[i]$color,
          as.character(V(.self$igraph)[i]$x),
          as.character(V(.self$igraph)[i]$y),
          as.character(i),
          V(.self$igraph)[i]$name,
          "", #as.character(V(.self$igraph)[i]$tfAbs),
          "", #as.character(V(.self$igraph)[i]$tfRel),
          community,
          tooltip
        )
        
      }
      lapply(1:length(V(.self$igraph)), function(i) .circleNode(i))
    },
    
    makeEdges = function(){
      
      "Make edges."
      
      edgelistId <- get.edgelist(.self$igraph, names = FALSE)
      edgelistString <- get.edgelist(.self$igraph, names = TRUE)
      
      lapply(
        c(1:nrow(edgelistId)),
        function(i){
          
          llValues <- as.character(round(unlist(E(.self$igraph)$ll[i]), 2))
          tooltip <- sprintf(
            "<title>%s/n%s</title>",
            paste(edgelistString[i,1], " - ", edgelistString[i,2], " (ll: ", "XXXX", ")", sep=""),
            paste(llValues, collapse=" / ")
          )
          sprintf(
            '<line style="%s" x1="%s" y1="%s" x2="%s" y2="%s" from="%s" to="%s" llXY="%s" llYX="%s" onclick="%s">%s</line>',
            "stroke:black; stroke-width:1px; fill:none;",
            as.character(V(.self$igraph)[edgelistId[i,1]]$x), #x1
            as.character(V(.self$igraph)[edgelistId[i,1]]$y), #y1
            as.character(V(.self$igraph)[edgelistId[i,2]]$x), #x2
            as.character(V(.self$igraph)[edgelistId[i,2]]$y), #y2
            as.character(edgelistId[i,1]), #y2
            as.character(edgelistId[i,2]), #
            llValues[1],
            ifelse(is.na(llValues[2]), "NA", llValues[2]),
            paste("edgeClick(x='", edgelistString[i,1], "', y='", edgelistString[i,2], "')", sep=""),
            tooltip
          )
        })    
    },
    
    textNodes = function(fontSize = 8, textOffset = 3){
      
      "Make text nodes."
      
      lapply(
        1:length(V(.self$igraph)),
        function(i){
          sprintf(
            '<text fill="%s" style = "%s" x="%s" y="%s" onlick="%s" nodeId="%s">%s</text>',
            "red",
            paste("font-size:", as.character(fontSize), "px;font-family:sans-serif", sep=""),
            as.character(V(.self$igraph)[i]$x + textOffset),
            as.character(V(.self$igraph)[i]$y - textOffset),
            paste("nodeClick('", V(.self$igraph)[i]$name, "')", sep = ""),
            as.character(i),
            V(.self$igraph)[i]$name
          )
        })    
    },
    
    
    make = function(verbose = TRUE){
      
      "Create XML."
      
      if (!is.null(.self$layout)){
        if (verbose) message("... calculate coordinates")
        .self$igraph <- addCoordinates(.self$igraph, layout = .self$layout, dim = 2)
        .self$igraph <- normalizeCoordinates(.self$igraph, width = .self$width, height = .self$height, margin = .self$margin)
      }
      
      if (verbose) message("... generating edges")
      edges <- .self$makeEdges()
      if (verbose) message("... generating nodes")
      nodes <- .self$makeNodes(pandocTab = .self$pandocTab)
      if (verbose) message("... generating text")
      text <- .self$textNodes(.self$fontSize, .self$textOffset)  
      
      doc <- sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width ="%s" height="%s">%s%s%s</svg>',
        as.character(width), as.character(height),
        paste(unlist(edges), collapse = ""),
        paste(unlist(nodes), collapse = ""),
        paste(unlist(text), collapse = "")
      )
      .self$xml <- doc
    }
  )
)