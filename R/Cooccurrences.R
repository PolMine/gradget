#' Get cooccurrences.
#' 
#' @field partition partition object
#' @field pAttribute character
#' @field drop character vector
#' @field method statistical test ("ll")
#' @field window integer
#' @field verbose logical
#' @field contextSizes data.table
#' @field dt data.table
#' 
#' @importFrom data.table data.table melt.data.table
#' @examples 
#' \dontrun{
#' merkel_partition <- partition("PLPRBT", speaker_name = "Angela Merkel", speaker_lp = "17", speaker_type = "speech", pAttribute = "word")
#' termsToDrop <- c(polmineR::punctuation, unlist(noise(pAttributes(merkel_partition, pAttribute = "word"))))
#' Merkel <- Cooccurrences$new(partition = merkel_partition, pAttribute = "word", window = 5L, drop = termsToDrop)
#' Merkel$getCounts()
#' Merkel$trim(drop = termsToDrop)
#' Merkel$getStats()
#' merkelCooc <- Merkel$as.S4()
#' tcm <- Merkel$makeTermCooccurrenceMatrix()
#' 
#' bt17_partition <- partition("PLPRBT", speaker_lp = "13", pAttribute = "word")
#' termsToDrop <- c(polmineR::punctuation, unlist(noise(pAttributes(bt17_partition, pAttribute = "word"))))
#' BT17 <- Cooccurrences$new(partition = bt17_partition, pAttribute = "word", window = 5L, drop = termsToDrop)
#' BT17$getStats()
#' bt17cooc <- BT17$as.S4()
#'
#' merkel_compared <- compare(Merkel$as.S4(), BT17$as.S4(), included = TRUE)
#' merkel_trimmed <- trim(kohl, by = subset(kohl_compared, rank_ll <= 250))
#' 
#' library(polmineR.graph)
#' library(igraph)
#' K <- asIgraph(kohl_trimmed)
#' K <- enrich(K, community = list(method = "fastgreedy", weights=FALSE))
#' K <- enrich(K, layout = "kamada.kawai", dim = 3)
#' K <- three::rescale(K, -400, 400)
#' t <- as.three(K, bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' t <- as.three(K, type="anaglyph", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' toView <- three:::store(t, directory="/Users/blaette/Lab/tmp/three")
#' shiny::runApp("/Users/blaette/Lab/github/polmineR.graph/inst/three")
#' browseURL(toView["tmpFileJs"])
#' }
#' @import polmineR
#' @import data.table
#' @importFrom slam simple_triplet_matrix
#' @export Cooccurrences
Cooccurrences <- setRefClass(
  
  Class = "Cooccurrences",
  
  fields = list(
    partition = "partition",
    pAttribute = "character",
    drop = "character",
    method = "character",
    window = "integer",
    verbose = "logical",
    contextSizes = "data.table",
    dt = "data.table"
  ),
  
  methods = list(
    initialize = function(partition, pAttribute, window, verbose = TRUE, drop = character()){
      .self$partition <- partition
      .self$pAttribute <- pAttribute
      .self$window <- window
      .self$verbose <- verbose
      .self$drop <- drop
    },
    
    getCounts = function(){
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
    },
    
    trim = function(drop = NULL){
      if (!is.null(drop)){
        .self$drop <- drop
      }
      if (verbose) message("... droping terms (by id)")
      ids_to_drop <- CQI$str2id(
        .self$partition@corpus, .self$partition@pAttribute,
        iconv(.self$drop, from = "UTF-8", to = .self$partition@encoding)
      )
      rows_to_drop <- union(
        which(.self$dt[["a_id"]] %in% ids_to_drop),
        which(.self$dt[["b_id"]] %in% ids_to_drop)
      )
      if (length(rows_to_drop) > 0){
        .self$dt <- .self$dt[-rows_to_drop]
      }
      
    },
    
    makeTermCooccurrenceMatrix = function(){
      
      if (verbose) message("... creating data.table for reindexing")  
      ID2STR <- data.table(id = unique(.self$dt[["a_id"]]))
      ID2STR[ , str := as.utf8(CQI$id2str(.self$partition@corpus, "word", ID2STR[["id"]]))]
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
    
    getStats = function(method = "ll"){
      if (is.null(.self$dt)){
        .self$getCounts()
        .self$trim()
      }
      if (verbose) message("... adding window size")
      
      setkeyv(.self$contextSizes, "a_id")
      DT <- contextSizes[.self$dt]

      DT[, a := as.utf8(CQI$id2str(.self$partition@corpus, .self$partition@pAttribute, a_id))]
      DT[, b := as.utf8(CQI$id2str(.self$partition@corpus, .self$partition@pAttribute, b_id))]
      DT[, "a_id" := NULL][, "b_id" := NULL]
      if (!haskey(.self$partition@stat)) setkeyv(.self$partition@stat, .self$partition@pAttribute)
      setkeyv(DT, cols = "a")
      DT2 <- .self$partition@stat[DT]
      setnames(DT2, old = c("word", "count"), new = c("a", "count_a"))
      
      setkeyv(DT2, cols = "b")
      DT3 <- .self$partition@stat[DT2]
      setnames(DT3, old = c("word", "count"), new = c("b", "count_b"))
      
      setnames(DT3, old = c("a", "b"), new = c("a_word", "b_word"))
      setcolorder(DT3, c("a_word", "b_word", "count_ab", "count_a", "count_b", "size_window"))
      .self$dt <- DT3
      coll <- .self$as.S4()
      if ("ll" %in% method) {
        message('... g2-Test')
        coll <- ll(coll)
        setorderv(coll@stat, cols = "ll", order = -1)
        .self$dt <- coll@stat
      }
    },
    
    as.S4 = function(){
      new(
        "cooccurrences",
        pAttribute = .self$partition@pAttribute, corpus = .self$partition@corpus, encoding = .self$partition@encoding,
        left = .self$window, right = .self$window, partitionSize = .self$partition@size,
        stat = .self$dt
      )
    }
    
  )
)
