#' Get cooccurrences.
#' 
#' Reference class to generate and manage cooccurrence statistics.
#' 
#' To reduce the size of the data.table with the cooccurrence statistics, the 
#' parameters \code{keep} and \code{drop} provide tokens with tokens that will be kept and
#' dropped, respectively. The parameters are used by the \code{trim} method.
#' 
#' @field partition partition object
#' @field pAttribute character
#' @field keep list of named character vectors, names are p-attributes
#' @field drop list of named character vectors, names are p-attributes
#' @field method statistical test ("ll")
#' @field window integer
#' @field verbose logical
#' @field contextSizes data.table
#' @field dt data.table
#' 
#' @param keep character vector 
#' @param returnXML logical, whether to return XML
#' @param verbose whether to be talkative
#' @param x a svg object
#' @param ... parameters that will be passed
#' 
#' @importFrom parallel mcparallel mccollect
#' @importFrom htmltools html_print HTML
#' @importFrom data.table data.table melt.data.table
#' @import methods
#' @import polmineR
#' @importFrom igraph graph.data.frame delete.vertices
#' @importFrom igraph as.undirected
#' @examples 
#' \dontrun{
#'
#' standard workflow:
#' ------------------
#' 
#' library(polmineR)
#' library(polmineR.graph)
#' 
#' merkel2008 <- partition(
#'   "PLPRBT", speaker_name = "Angela Merkel", speaker_year = "2008", speaker_type = "speech",
#'   pAttribute = "word", name = "merkel2008"
#'   )
#' termsToDrop <- c(polmineR::punctuation, unlist(noise(pAttributes(merkel2008, pAttribute = "word"))))
#' 
#' Merkel <- Cooccurrences$new(partition = merkel2008, pAttribute = "word", window = 5L, drop = termsToDrop)
#' Merkel$count()
#' Merkel$trim(action = "drop", by.id = TRUE)
#' Merkel$maths()
#' 
#' bt2008 <- partition("PLPRBT", speaker_year = "2008", pAttribute = "word", name = "bt2008")
#' termsToDrop <- c(polmineR::punctuation, unlist(noise(pAttributes(bt2008, pAttribute = "word"))))
#' BT2008 <- Cooccurrences$new(partition = bt2008, pAttribute = "word", window = 5L, drop = termsToDrop)
#' BT2008$count()
#' BT2008$trim(action = "drop", by.id = TRUE)
#' BT2008$maths()
#' 
#' Merkel2 <- copy(Merkel)
#' Merkel2$featureSelection(reference = BT2008, included = TRUE)
#' 
#' G <- Merkel$as.igraph(as.undirected = TRUE)
#' G <- addCommunities(G, method = "fastgreedy", weights = FALSE)
#' G <- addCoordinates(G, layout = "kamada.kawai", dim = 3)
#' G <- rescale(G, -1000, 1000)
#' 
#' Y <- SVG(G)
#' Y$make()
#' Y$plot()
#' Y$width = 750
#' Y$height = 750
#' Y$make()
#' Y$browse()
#' }
#' @import polmineR
#' @import data.table
#' @importFrom slam simple_triplet_matrix
#' @export Cooccurrences
Cooccurrences <- setRefClass(
  
  Class = "Cooccurrences",
  
  fields = list(
    
    corpus = "character",
    partition = "partition",
    pAttribute = "character",
    minimized = "logical",
    keep = "list",
    drop = "list",
    method = "character",
    window = "integer",
    verbose = "logical",
    contextSizes = "data.table",
    dt = "data.table",
    svg = "character"
  
  ),
  
  methods = list(
    
    initialize = function(corpus = NULL, partition = NULL, pAttribute = "word", window = 5L, verbose = TRUE, drop = c(polmineR::punctuation, tm::stopwords("de")), keep = NULL){
      
      "Initialize a Cooccurrences class object."
      
      if (is.null(corpus) && is.null(partition)){
        stop("corpus and partition are NULL, one needs to be provided")
      }
      if(!is.null(corpus)){
        C <- Corpus$new(corpus)
        C$pAttribute <- character()
        .self$partition <- C$as.partition()
        .self$corpus <- corpus
      } else if (!is.null(partition)){
        .self$partition <- partition
        .self$corpus <- partition@corpus
      }
      
      .self$pAttribute <- pAttribute
      .self$window <- as.integer(window)
      .self$verbose <- verbose
      
      # for convencience, keep may be a character vector - turn that into list structure
      if (is.vector(keep)){
        if (length(pAttribute) == 1){
          .self$keep <- list()
          .self$keep[[pAttribute]] <- keep
        } else {
          stop("Param 'keep' is a vector, but length(pAttribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(keep)){
        .self$keep <- list
      }
      
      # the same for drop
      if (is.vector(drop)){
        if (length(pAttribute) == 1){
          .self$drop <- list()
          .self$drop[[pAttribute]] <- drop
        } else {
          stop("Param 'drop' is a vector, but length(pAttribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(drop)){
        .self$drop <- list
      }
    },
    
    show = function(){
      cat("Object of 'cooccurrences'-class\n")
    },
    
    summary = function(){
      message("not yet implemented")
    },
    
    count = function(){
      
      "Count the cooccurrence of terms. The field 'dt' is filled with a data.table with the
      columns 'a_id', 'b_id' and 'count_ab'."
      
      if (length(.self$pAttribute) == 1 && requireNamespace("polmineR.Rcpp", quietly = TRUE)){
        if (.self$verbose) message("... getting window matrix (using Rcpp)")
        windowMatrix <- polmineR.Rcpp::getCbowMatrix(
          .self$partition@corpus, .self$pAttribute, Sys.getenv("CORPUS_REGISTRY"),
          .self$partition@cpos, .self$window
        )
        windowDT <- as.data.table(windowMatrix)
        
        rm(windowMatrix); gc()
        
        setnames(windowDT, old = paste("V", window + 1, sep = ""), new = "a_id")
        if (verbose) message("... melting")
        coocDT <- data.table::melt.data.table(windowDT, id.vars = "a_id", value.name = "b_id")
        
        rm(windowDT); gc()
        
        coocDT[, "variable" := NULL, with = TRUE]
        
        if (verbose) message("... kicking out -1")
        DTmin <- coocDT[b_id != -1]
        .self$contextSizes <- DTmin[, .N, by = "a_id"]
        setnames(.self$contextSizes, old = "N", new = "size_window")
        if (verbose) message("... counting cooccurrences")
        .self$dt <- DTmin[, .N, by = c("a_id", "b_id"), with = TRUE]
        setnames(.self$dt, "N", "count_ab")
        setkeyv(.self$dt, "a_id")
        
      } else {
        
        if (length(.self$pAttribute) == 0) stop("The partition is required to included counts. Enrich the object first!")
        
        pAttr <- sapply(pAttribute, function(x) paste(.self$corpus, x, sep = "."))
        aColsId <- setNames(paste("a", .self$pAttribute, "id", sep="_"), .self$pAttribute)
        bColsId <- setNames(paste("b", .self$pAttribute, "id", sep="_"), .self$pAttribute)
        aColsStr <- setNames(paste("a", .self$pAttribute, sep="_"), .self$pAttribute)
        bColsStr <- setNames(paste("b", .self$pAttribute, sep="_"), .self$pAttribute)
        
        .makeWindows <- function(i, cpos, ...){
          cposMin <- cpos[i,1]
          cposMax <- cpos[i,2]
          if (cposMin != cposMax){
            cposRange <- cposMin:cposMax
            lapply(
              setNames(cposRange, cposRange),
              function(x) {
                cpos <- c((x - window):(x-1), (x + 1):(x + window))
                cpos <- cpos[which(cpos >= cposMin)]
                cpos[which(cpos <= cposMax)]
              })
          }
        }
        bag <- blapply(as.list(c(1:nrow(.self$partition@cpos))), f = .makeWindows, cpos = .self$partition@cpos, mc = mc)
        bCpos <- lapply(
          bag,
          function(x) lapply(names(x), function(y) rep(as.numeric(y), times = length(x[[y]])))
        )
        if (verbose) message("... putting together data.table")
        DT <- data.table(a_cpos = unlist(bag), b_cpos = unlist(bCpos))
        
        if (verbose == TRUE) message("... getting token ids")
        lapply(
          pAttribute, function(x){
            DT[, eval(aColsId[x]) := CQI$cpos2id(.self$partition@corpus, x, DT[["a_cpos"]]), with = TRUE]
            DT[, eval(bColsId[x]) := CQI$cpos2id(.self$partition@corpus, x, DT[["b_cpos"]]), with = TRUE]
          }
        )
        if (verbose == TRUE) message("... counting window size")
        
        contextDT <- DT[, .N, by = c(eval(aColsId)), with = TRUE]
        setnames(contextDT, "N", "size_window")
        .self$contextSizes <- contextDT
        
        # if (verbose == TRUE) message("... applying filter")
        
        if (verbose) message("... counting co-occurrences")
        TF <- DT[, .N, by = c(eval(c(aColsId, bColsId))), with = TRUE]
        setnames(TF, "N", "count_ab")
        
        if (verbose) message("... adding window size")
        setkeyv(contextDT, cols = aColsId)
        setkeyv(TF, cols = aColsId)
        TF <- contextDT[TF]
        
        
        
      }
    },
    
    trim = function(action, by.id){
      
      "Trim the overall list of cooccurrences by dropping terms that are not frequent. Recommended
      to speed up computation of statistical test values."
      
      # turn tokens to keep to id
      
      toMatch <- .self[[action]]
      if (by.id == TRUE){
        toMatch <- lapply(
          setNames(names(toMatch), names(toMatch)),
          function(x) CQI$str2id(.self$corpus, x, toMatch[[x]])
          )
        if (length(.self$pAttribute) > 1){
          colRegex <- paste(names(toMatch), "id$", sep = "_")
        } else {
          colRegex <- "_id$"
        }
      } else {
        colRegex <- paste(names(toMatch), "$", sep = "")
      }
      
      indexList <- lapply(
        names(toMatch),
        function(pAttr){
          lapply(
            grep(colRegex, colnames(.self$dt)),
            function(i) which(.self$dt[[i]] %in% toMatch[[pAttr]])
          )}
      )
      
      rowindex <- unique(unlist(indexList))
      if (action == "keep"){
        .self$dt <- .self$dt[rowindex]
      } else if (action == "drop"){
        if (length(rowindex) > 0){
          .self$dt <- .self$dt[-rowindex]
        }
      }
    },
    
    makeTermCooccurrenceMatrix = function(){
      
      "Returns a simple triplet matrix based on the counts of term cooccurrences. If counts are
      not yet present, that is done first."
      
      if (verbose) message("... creating data.table for reindexing")  
      ID2STR <- data.table(id = unique(.self$dt[["a_id"]]))
      ID2STR[ , str := as.nativeEnc(CQI$id2str(.self$partition@corpus, .self$pAttribute, ID2STR[["id"]]), from = getEncoding(.self$corpus))]
      setkeyv(ID2STR, cols = "id")
      setorderv(ID2STR, cols = "id")
      ID2STR[, "id_new" := 1:nrow(ID2STR), with = TRUE]
      setkeyv(.self$dt, "a_id")
      
      if (verbose) message("... id2str for a")
      coocCount2 <- .self$dt[ID2STR]
      data.table::setnames(coocCount2, old = c("str", "id_new"), new = c("a_token", "a_new_key"))
      setkeyv(coocCount2, "b_id")
      if (verbose) message("... id2str for b")
      coocCount3 <- coocCount2[ID2STR]
      rm(coocCount2)
      setnames(coocCount3, old = c("str", "id_new"), new = c("b_token", "b_new_key"))
      if (verbose) message("... preparing simple_triplet_matrix")
      retval <- slam::simple_triplet_matrix(
        i = coocCount3[["a_new_key"]],
        j = coocCount3[["b_new_key"]],
        v = coocCount3[["count_ab"]],
        dimnames = list(ID2STR[["str"]], ID2STR[["str"]])
      )
      return(retval)
      
    },
    
    maths = function(method = "ll"){
      
      "Based on counts of term cooccurrences, the data.table is enriched and statistical operations
      - the maths - are performed to get the significance of cooccurrences."
      
      if (is.null(.self$dt)){
        .self$count()
        .self$trim(action = "drop", by.id = TRUE)
      }
      
      if (verbose) message("... adding window size")
      
      setkeyv(.self$contextSizes, "a_id")
      setkeyv(.self$dt, "a_id")
      DT <- .self$contextSizes[.self$dt]
      
      if (length(pAttribute) == 1){
        
        DT[, a := as.nativeEnc(CQI$id2str(.self$partition@corpus, .self$partition@pAttribute, a_id), from = getEncoding(.self$corpus))]
        DT[, b := as.nativeEnc(CQI$id2str(.self$partition@corpus, .self$partition@pAttribute, b_id), from = getEncoding(.self$corpus))]
        DT[, "a_id" := NULL][, "b_id" := NULL]
        setkeyv(.self$partition@stat, .self$partition@pAttribute)
        setkeyv(DT, cols = "a")
        DT2 <- .self$partition@stat[DT]
        setnames(DT2, old = c(.self$pAttribute, "count"), new = c("a", "count_a"))
        
        setkeyv(DT2, cols = "b")
        DT3 <- .self$partition@stat[DT2]
        setnames(DT3, old = c(.self$pAttribute, "count"), new = c("b", "count_b"))
        
        setnames(DT3, old = c("a", "b"), new = c(paste("a", .self$pAttribute, sep = "_"), paste("b", .self$pAttribute, sep = "_")))
        .self$dt <- DT3
        coll <- .self$as.S4()
        if ("ll" %in% method) {
          message('... g2-Test')
          coll <- ll(coll)
          setorderv(coll@stat, cols = "ll", order = -1)
          .self$dt <- coll@stat
        }
      } else {
        # if (verbose == TRUE) message("... converting ids to strings")
        # lapply(
        #   c(1:length(pAttribute)),
        #   function(i){
        #     TF[, eval(aColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[aColsId[i]]])), with = TRUE]
        #     TF[, eval(bColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[bColsId[i]]])), with=TRUE]
        #     TF[, eval(aColsId[i]) := NULL]
        #     TF[, eval(bColsId[i]) := NULL]
        #   }
        # )
        # setkeyv(TF, cols = aColsStr)
        # setkeyv(.Object@stat, cols = pAttribute)
        # TF[, "count_a" := .Object@stat[TF][["count"]]]
        # setkeyv(TF, cols=bColsStr)
        # TF[, "count_b" := .Object@stat[TF][["count"]]]
        # setcolorder(TF, c(aColsStr, bColsStr, "count_ab", "count_a", "count_b", "size_window"))
        # if (tcm == FALSE){
        #   coll@stat <- TF
        #   if ("ll" %in% method) {
        #     message('... g2-Test')
        #     coll <- ll(coll)
        #     coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
        #   }
        #   return(coll)
        # } else if (tcm == TRUE){
        #   concatenate <- function(x) paste(x, collapse = "//")
        #   if (length(pAttribute) > 1){
        #     TF[, "strKeyA" := apply(TF[, eval(paste("a", pAttribute, sep = "_")), with = FALSE], 1, concatenate)]
        #     TF[, "strKeyB" := apply(TF[, eval(paste("b", pAttribute, sep = "_")), with = FALSE], 1, concatenate)]
        #   } else {
        #     setnames(TF, old = paste("a", pAttribute, sep = "_"), new = "strKeyA")
        #     setnames(TF, old = paste("b", pAttribute, sep = "_"), new = "strKeyB")
        #   }
        #   uniqueKey <- unique(c(TF[["strKeyA"]], TF[["strKeyB"]]))
        #   keys <- setNames(c(1:length(uniqueKey)), uniqueKey)
        #   i <- unname(keys[TF[["strKeyA"]]])
        #   j <- unname(keys[TF[["strKeyB"]]])
        #   retval <- simple_triplet_matrix(
        #     i = i, j = j, v = TF[["count_ab"]],
        #     dimnames = list(a = names(keys)[1:max(i)], b = names(keys)[1:max(j)])
        #   )
        #   return(retval)
        # }
        
      }
      
    },
    
    getFeatureSelectionStats = function(reference, included = FALSE, method = "ll", verbose = TRUE){
      
      if (identical(.self$pAttribute, reference$pAttribute) == FALSE) {
        warning("BEWARE: cooccurrences objects are not based on the same pAttribute!")
      }
      
      if (verbose) message("... preparing tabs for matching")
      keys <- unlist(lapply(c("a", "b"), function(ab) paste(ab, .self$pAttribute, sep = "_"))) 
      setkeyv(.self$dt, keys)
      setkeyv(reference$dt, keys)
      MATCH <- reference$dt[.self$dt]
      
      # remove columns not needed
      setnames(MATCH, old = c("count_ab", "i.count_ab"), new = c("count_ref", "count_coi"))
      colsToKeep <- c(keys, "count_ref", "count_coi")
      colsToDrop <- colnames(MATCH)[!colnames(MATCH) %in% colsToKeep]
      for (drop in colsToDrop) MATCH[, eval(drop) := NULL, with = TRUE]
      if (included == TRUE) MATCH[, "count_ref" := MATCH[["count_ref"]] - MATCH[["count_coi"]] ]
      
      compObject <- new(
        "features",
        included = FALSE, corpus = .self$corpus, sizeCoi = .self$partition@size,
        sizeRef = if (included) reference$partition@size - .self$partition@size else reference$partition@size,
        pAttribute = .self$pAttribute,
        stat = MATCH
      )
      
      for (how in method){
        if (verbose == TRUE) message("... statistical test: ", how)
        compObject <- do.call(how, args = list(.Object = compObject))
      }
      return(compObject@stat)
    },
    
    
    featureSelection = function(reference, included = FALSE, method = "ll", verbose = TRUE, n = 250){
      DT <- .self$getFeatureSelectionStats(reference = reference, included = included, method = method, verbose = verbose)
      keys <- unlist(lapply(c("a", "b"), function(what) paste(what, .self$pAttribute, sep = "_")))
      rowsToKeep <- c(keys, "rank_ll")
      DT <- DT[, rowsToKeep, with = FALSE]
      DT[, keep := ifelse(rank_ll <= n, TRUE, FALSE)]
      DT[, rank_ll := NULL]
      DT2 <- DT[DT[["keep"]] == TRUE]
      
      setkeyv(.self$dt, keys)
      setkeyv(DT2, keys)
      Y <- .self$dt[DT2]
      .self$dt <- Y
    },
    
    minimize = function(){
      DT <- copy(.self$dt)
      aColsStr <- paste("a_", .self$pAttribute, sep="")
      bColsStr <- paste("b_", .self$pAttribute, sep="")
      KEY <- data.table(
        i = c(1:nrow(DT)),
        aKey = apply(DT, 1, function(x) paste(x[aColsStr], collapse = "//")),
        bKey = apply(DT, 1, function(x) paste(x[bColsStr], collapse = "//"))
      )
      DT[, "order" := KEY[, order(c(.SD[["aKey"]][1], .SD[["bKey"]][1]))[1], by = "i"][["V1"]]]
      setkey(DT, "order")
      aToB <- DT[list(1)]
      setkeyv(aToB, cols = c(aColsStr, bColsStr))
      bToA <- DT[list(2)]
      setnames(bToA, old = c(aColsStr, bColsStr), new = c(bColsStr, aColsStr))
      setkeyv(bToA, cols = c(aColsStr, bColsStr))
      merger <- merge(aToB, bToA, all.x = FALSE, all.y = TRUE)
      FIN <- merger[, c(aColsStr, bColsStr, "ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"), with = FALSE]
      setnames(
        FIN,
        c("ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"),
        c("ab_count", "ab_ll", "ba_ll", "a_count", "b_count")
        )
      setcolorder(FIN, c(aColsStr, bColsStr, "ab_count", "a_count", "b_count", "ab_ll", "ba_ll"))
      setkeyv(FIN, cols = c(aColsStr, bColsStr))
      .self$minimized <- TRUE
      .self$dt <- FIN
    },
    
    as.sparseMatrix = function(x, col){
      uniqueTerms <- unique(c(.self$dt[,"node"], .self$dt[,"cooccurrence"]))
      keyVector <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
      splittedTab <- split(x = .self$dt[,c(col, "cooccurrence")], f = .self$dt[,"node"])
      
      bag <- list()
      i <- unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]]))))) #nodes
      j <- unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"cooccurrence"]]))) # cooccurrences
      x <- unname(unlist(lapply(splittedTab, function(tab) tab[,col]))) # values
      
      retval <- sparseMatrix(
        i = i, j = j, x = x, 
        dims = c(length(uniqueTerms), length(uniqueTerms)),
        dimnames = list(names(keyVector), names(keyVector)),
        giveCsparse = TRUE
      )   
      return(retval)
    },
    
    as.igraph = function(edgeAttributes = "ll", verticeAttributes = NULL, as.undirected = TRUE){
      if (!all(edgeAttributes %in% colnames(.self$dt))){
        warning("edgeAttribute supplied is not available")
      }
      tab <- as.data.frame(.self$dt)
      aColsStr <- paste("a", .self$pAttribute, sep = "_")
      bColsStr <- paste("b", .self$pAttribute, sep = "_")
      if (length(.self$pAttribute) == 1){
        tab[["node"]] <- tab[[aColsStr]]
        tab[["collocate"]] <- tab[[bColsStr]]
      } else {
        tab[["node"]] <- apply(tab, 1, function(x) paste(x[aColsStr], collapse = "//"))
        tab[["collocate"]] <- apply(tab, 1, function(x) paste(x[bColsStr], collapse="//"))
      }
      g <- graph.data.frame(tab[, c("node", "collocate", edgeAttributes)])
      if ("count" %in% verticeAttributes){
        TF <- .self$partition@stat # this will be a data.frame
        if (.self$pAttribute == 1){
          TF[,key := TF[[.self$pAttribute]] ]
        } else{
          TF[, key := apply(TF, 1, function(row) paste(row[.self$pAttribute], collapse = "//"))]
        }
        setkey(TF, key)
        tfVector <- TF[names(V(g))][["count"]]
        V(g)$count <- tfVector
        V(g)$freq <- round((tfVector / .self$partition@size) * 100000, 3)
      }
      if (as.undirected) g <- as.undirected(g, edge.attr.comb = "concat")
      g <- delete.vertices(g, V(g)[name == "\u0084"])
      g <- delete.vertices(g, V(g)[name == "\u0093"])
      return(g)
    },
    
    as.svg = function(object, layout = "kamada.kawai", verbose = TRUE, ...){
      if (verbose == TRUE) message("... creating igraph object (step 1)")
      igraphObject <- .self$as.igraph()
      if (verbose == TRUE) message("... creating svg object (step 2)")
      svgObject <- as.svg(igraphObject, verbose = verbose, ...)
      svgObject
    },
    
    plot = function(...){
      toBePlotted <- as.svg(.self, ...)
      plot(toBePlotted)
    },
    
    as.S4 = function(){
      
      "Turns a Cooccurrence object (reference class) into an S4 class cooccurrences object as defined in the
      polmineR base package."
      
      new(
        "cooccurrences",
        pAttribute = .self$partition@pAttribute, corpus = .self$partition@corpus, encoding = .self$partition@encoding,
        left = .self$window, right = .self$window, partitionSize = .self$partition@size,
        stat = .self$dt, cpos = data.table()
      )
    }
    
  )
)





# if (big == TRUE){
#   if (requireNamespace("bigmemory", quietly = TRUE) && requireNamespace("bigtabulate", quietly = TRUE) ) {
#     if (verbose == TRUE) message("... generating context tables")
#     BIG <- bigmemory::big.matrix(ncol = window * 2 + 1, nrow = .Object@size, ...)
#     ids <- lapply(
#       c(1:nrow(.Object@cpos)),
#       function(i) 
#         CQI$cpos2id(.Object@corpus, pAttribute, c(.Object@cpos[i,1]: .Object@cpos[i,2]))
#     )
#     idPos <- cumsum(lapply(ids, length))
#     .windowPrep <- function(i, ids, window, BIG, ...){
#       idChunk <- ids[[i]]
#       lapply(
#         c(-window:-1, 1:window),
#         function(x){
#           idsToFill <- c(
#             rep(NA, times = min(ifelse( x < 0 , -x, 0), length(idChunk))),
#             idChunk[
#               ifelse(length(idChunk) <= abs(x), 0, ifelse(x < 0, 1, x + 1))
#               :
#                 ifelse(length(idChunk) <= abs(x), 0, ifelse(x < 0, length(idChunk)+x, length(idChunk)))
#               ],
#             rep(NA, times=min(ifelse(x > 0, x, 0), length(idChunk)))
#           )
#           BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), ifelse(x < 0, x + window + 1, x+window)] <- idsToFill
#           BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), window * 2 + 1] <- idChunk
#         })
#     }
#     dummy <- blapply(as.list(c(1:length(ids))), f = .windowPrep, ids = ids, window = window, BIG = BIG, mc = mc)
#     if (verbose == TRUE) message("... counting cooccurrences")
#     rowIndices <- bigtabulate::bigsplit(BIG, ccols = ncol(BIG), breaks=NA, splitcol=NA)
#     .getTables <- function(node, rowIndices, BIG, window, ...){
#       toTabulate <- as.vector(BIG[rowIndices[[node]], c(1:(window * 2))])
#       toTabulate <- toTabulate + 1
#       tabulated <- tabulate(toTabulate)
#       idRawPresent <- which(tabulated != 0)
#       matrix(
#         data=c(
#           rep(as.integer(node), times=length(idRawPresent)),
#           idRawPresent - 1,
#           tabulated[idRawPresent],
#           rep(sum(tabulated[idRawPresent]), times=length(idRawPresent))
#         ),
#         ncol = 4
#       )
#     }
#     tables <- blapply(
#       as.list(names(rowIndices)), f=.getTables,
#       rowIndices = rowIndices, BIG = BIG, window = window,
#       mc = mc
#     )
#     rm(BIG)
#     countMatrices <- do.call(rbind, tables)
#     TF <- data.table(countMatrices)
#     setnames(TF, c(aColsId[1], bColsId[1], "count_ab", "size_window"))
#   } else {
#     stop("MISSING DEPENDENCIES: Packages bigmemory and/or bigtabulate are not installed") 
#   }
#   






#' #' @include cooccurrences_class.R
#' NULL
#' 
#' #' @rdname cooccurrencesBundle-class
#' setMethod("as.TermDocumentMatrix", "cooccurrencesBundle", function(x, col, directed=TRUE, rel=FALSE, mc=getOption("polmineR.mc")){
#'   tabs <- lapply(x@objects, as.data.frame)
#'   if (directed == TRUE){
#'     keys <- unique(unlist(lapply(tabs, rownames)))
#'     keyVector <- setNames(c(1:length(keys)), keys)
#'     i <- unname(unlist(lapply(tabs, function(tab) keyVector[rownames(tab)])))
#'     j <- unlist(lapply(c(1:length(tabs)), function(i) rep(i, nrow(tabs[[i]]))))
#'     v <- unlist(lapply(tabs, function(tab) tab[,col]))
#'   } else if (directed == FALSE){
#'     .uniqueKeys4tab <- function(tab){
#'       tabMatrix <- as.matrix(tab[,c("nodeId", "cooccurrenceId", col)])
#'       tabMatrixPlus <- t(apply(tabMatrix, 1, .minMaxId))
#'       colnames(tabMatrixPlus) <- c(colnames(tabMatrix), c("idMin", "idMax"))
#'       tabDataFrame <- data.frame(
#'         tabMatrixPlus,
#'         characterKey=paste(
#'           CQI$id2str(x@corpus, x@pAttribute, tabMatrixPlus[,"idMin"]), "<->",
#'           CQI$id2str(x@corpus, x@pAttribute, tabMatrixPlus[,"idMax"]), sep=""
#'         ),
#'         stringsAsFactors=FALSE
#'       )
#'       Encoding(tabDataFrame[,"characterKey"]) <- x@encoding
#'       tabDataFrame
#'     }
#'     tabs <- blapply(tabs, f=.uniqueKeys4tab, mc=mc)
#'     keys <- unique(unlist(lapply(tabs, function(tab) tab[, "characterKey"])))
#'     keyVector <- setNames(c(1:length(keys)), keys)
#'     .reduceTab <- function(i, tab, keyVector) {
#'       tab <- data.frame(tabs[[i]], no=i, key=keyVector[tabs[[i]][, "characterKey"]])
#'       tab <- as.matrix(tab[,c("no", col, "key")])
#'       tabSplit <- split(tab, tab[,"key"])
#'       tabSplitReduced <- lapply(tabSplit, function(foo) {
#'         noRow <- length(foo)/3
#'         return(c(foo[1], foo[noRow+1], foo[2*noRow+1]))
#'       })
#'       tabReduced <- do.call(rbind, tabSplitReduced)
#'       colnames(tabReduced) <- c("no", col, "key")
#'       return(tabReduced)
#'     }
#'     tabsReduced <- blapply(as.list(c(1:length(tabs))), f=.reduceTab, tab=tab, keyVector=keyVector)
#'     tab <- do.call(rbind, tabsReduced)
#'     i <- tab[,"key"]
#'     j <- tab[,"no"]
#'     v <- tab[,col]
#'   }
#'   mat <- simple_triplet_matrix(
#'     i=i, j=j, v=v,
#'     ncol=length(tabs),
#'     nrow=length(keyVector),
#'     dimnames=list(
#'       Terms=names(keyVector),
#'       Docs=names(x@objects)
#'     )
#'   ) 
#'   mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=x@encoding, to="UTF-8")
#'   class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
#'   return(mat)
#' })


# I do not really recall the purpose of this method


#' #' @rdname cooccurrencesReshaped
#' setMethod("merge", "cooccurrencesReshaped", function(x,y){
#'   if (all(c(class(x), class(y)) == "cooccurrencesReshaped") == FALSE) warning("x and y need to be cooccurrences objects")
#'   dfRet <- merge(x@stat, y@stat, by.x=0, by.y=0)
#'   dfRet
#' })
#' 
