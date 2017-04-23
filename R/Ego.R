#' Analyse ego networks of queries.
#' 
#' Reference class to organise the analysis of ego networks.
#' 
#' @param coi corpus of interest, either a partition or name of a CWB corpus (character vector length 1)
#' @param query query, may be a CQP query
#' @param left number of tokens to the left of the query
#' @param right number of tokens to the right of the query
#' @param pAttribute character vector (length 1 or more) indicating p-attributes
#' @param min.count minimum number of counts (in window)
#' @param min.ll minimum log likelihood value
#' @param max.coocs maximum number of cooccurrences
#' @param keep a named list of character vectors with tokens to be kept
#' @param drop a named list of character vectors with tokens to be dropped
#' @param verbose logical, whether to print status messages
#' 
#' @field query the query
#' @field left number of tokens to the left of the query
#' @field right number of tokens to the right of the query
#' @field partition a partition object
#' @field Corpus a Corpus object
#' @field pAttribute pAttribute(s)
#' @field keep named list (see arguments)
#' @field drop named list (see arguments)
#' @field min.count minimum number of counts (in window)
#' @field min.ll minimum log likelihood value
#' @field max.coocs maximum number of cooccurrences
#' @field igraph igraph object
#' @field fontSize font size 
#' @field layout graph layout
#' @field verbose logical, whether to print status messages
#' 
#' @examples 
#' \dontrun{
#' use("taz.beta")
#' E <- Ego$new(
#'   coi = partition("TAZ", text_date = "2010.*", regex = TRUE),
#'   query = "Islam",
#'   left = 10, right = 10,
#'   drop = list(
#'     word = c('"', '!', '/', '.', ":", "[", "]", "#", "'", ","),
#'     pos = c("ART", "APPR", "PRELS", "PDAT", "$(", "$.", "$,", "APPRAR",
#'              "VAFIN", "APPRART", "KOUI", "KON", "PPOSAT", "KOUS")
#'    )
#' )
#' E$calc()
#' 
#' E$plot(with = "networkD3")
#' 
#' graph <- as.dgr_graph(E$igraph)
#' DiagrammeR::render_graph(graph)
#' 
#' E$plot(with = "plotly")
#' 
#' S <- E$as.SVG()
#' S$width <- 500
#' S$height <- 500
#' S$fontSize <- 12
#' S$make()
#' S$browse()
#' 
#' 
#' }
#' @importFrom igraph simplify
#' @importFrom pbapply pblapply
#' @importFrom igraph graph_from_data_frame as_edgelist
#' @importFrom networkD3 forceNetwork
#' @export Ego
Ego <- setRefClass(
  
  Class = "Ego",
  
  fields = list(
    
    query = "character",
    left = "numeric",
    right = "numeric",
    partition = "partition",
    Corpus = "Corpus",
    pAttribute = "character",
    keep = "list",
    drop = "list",
    min.count = "numeric",
    min.ll = "numeric",
    max.coocs = "numeric",
    igraph = "igraph",
    fontSize = "numeric",
    layout = "character",
    verbose = "logical"
    
  ),
  
  methods = list(
    
    initialize = function(coi, query, left = getOption("polmineR.left"), right = getOption("polmineR.right"),
      pAttribute = getOption("polmineR.pAttribute"),
      min.count = 3, min.ll = 10.83, max.coocs = 10,
      keep = list(), drop = list(word = c('"', '!', '/', '.', '[', ']')),
      verbose = TRUE
      ){
      
      "Create new Ego object."
      
      if ("partition" %in% is(coi)){
        .self$partition <- coi
      } else if (is.character(coi)){
        # .self$Corpus <- Corpus$new(coi)
      }

      .self$query <- query
      .self$left <- left
      .self$right <- right
      .self$pAttribute <- pAttribute
      .self$min.count <- min.count
      .self$min.ll <- min.ll
      .self$max.coocs <- max.coocs
      .self$keep <- keep
      .self$drop <- drop
      .self$verbose <- verbose
      
    },
    
    calc = function(){
      
      "Initiate calculation of ego network."
      
      if (nrow(.self$partition@cpos) > 0){
        coi <- .self$partition
      } else {
        coi <- .self$Corpus
      }
      
      nodeCoocs <- cooccurrences(coi, query = .self$query, pAttribute = .self$pAttribute, verbose = FALSE)
      nodeCoocs@stat <- .self$filter(nodeCoocs@stat)
      if (.self$verbose) message("... no of cooccurrences to get: ", nrow(nodeCoocs))
      
      coocs <- pbapply::pblapply(
        nodeCoocs[["word"]],
        function(token){
          C <- cooccurrences(coi, query = token, cqp = FALSE, pAttribute = .self$pAttribute, verbose = FALSE)
          C@stat <- .self$filter(C@stat)
          C
        }
      )
      
      coocBundle <- new("cooccurrencesBundle", objects = coocs)
      coocBundle <- coocBundle + nodeCoocs
      
      df <- as.data.frame(coocBundle)
      
      if (length(pAttribute) >= 2) df[["b"]] <- gsub("^(.*?)//.*?$", "\\1", df[["b"]])
      
      G <- igraph::graph_from_data_frame(df)
      G <- simplify(
        G, remove.multiple = TRUE, remove.loops = TRUE,
        edge.attr.com = mean
      )
      G <- addCommunities(G, method = "fastgreedy", weights = FALSE)
      G <- addCoordinates(G, layout = "kamada.kawai", dim = 3)
      .self$igraph <- G
    },
    
    filter = function(x){
      for (what in names(.self$keep)) x <- x[which(x[[what]] %in% .self$keep[[what]])]
      for (what in names(.self$drop)) x <- x[!which(x[[what]] %in% .self$drop[[what]])]
      x <- subset(x, count_window >= .self$min.count)
      x <- subset(x, ll >= .self$min.ll)
      if (nrow(x) > .self$max.coocs) x <- x[1:.self$max.coocs]
      x
    },
    
    as.SVG = function(){
      
      "Turn igraph object into SVG class for plotting."
      
      SVG$new(.self$igraph)
    },
    
    plot = function(with = "networkD3"){
      
      "Plot graph using network D3, DiagrammeR or plotly."
      
      switch(
        with,
        "networkD3" = as.networkD3(.self$igraph),
        "DiagrammeR" = as.as.dgr_graph(.self$igraph),
        "plotly" = as.plotly(.self$igraph)
      )
      
    }
  )
)