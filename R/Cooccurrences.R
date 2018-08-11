#' Get cooccurrences.
#' 
#' Reference class to generate and manage cooccurrence statistics.
#' 
#' To reduce the size of the \code{data.table} with the cooccurrence statistics, the 
#' parameters \code{keep} and \code{drop} provide tokens with tokens that will be kept and
#' dropped, respectively. The parameters are used by the \code{trim} method.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{partition}}{The \code{partition} for which all cooccurrences shall be computed.}
#'   \item{\code{p_attribute}}{character}
#'   \item{\code{keep}}{A list of named character vectors, names are p-attributes.}
#'   \item{\code{drop}}{A list of named character vectors, names are p-attributes.}
#'   \item{\code{method}}{The statistical test to use (such as "ll").}
#'   \item{\code{window}}{An integer value.}
#'   \item{\code{verbose}}{Logical}
#'   \item{\code{contextSizes}}{A \code{data.table}.}
#'   \item{\code{dt}}{A \code{data.table}.}
#' }
#' @section Arguments:
#' \describe{
#'   \item{\code{keep}}{}
#'   \item{\code{returnXML}}{}
#'   \item{\code{verbose}}{}
#'   \item{\code{x}}{}
#'   \item{\code{...}}{}
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$initialize(corpus = NULL, partition = NULL, p_attribute = "word", window = 5L, verbose = TRUE, drop = c(polmineR::punctuation, tm::stopwords("de")), keep = NULL)}}{Initialize.}
#'   \item{\code{$count()}}{Count the cooccurrence of terms. The field 'dt' is filled with a data.table with the columns 'a_id', 'b_id' and 'ab_count'.}
#'   \item{\code{$trim(function(action, by.id)}}{Trim the overall list of cooccurrences by dropping terms that are not frequent. Recommended to speed up computation of statistical test values.}
#'   \item{\code{$makeTermCooccurrenceMatrix()}}{Returns a simple triplet matrix based on the counts of term cooccurrences. If counts are not yet present, that is done first.}
#'   \item{\code{(method = "ll")}}{Based on counts of term cooccurrences, the data.table is enriched and statistical operations - the maths - are performed to get the significance of cooccurrences.}
#'   \item{\code{$getFeatureSelectionStats(reference, included = FALSE, method = "ll", verbose = TRUE)}}{}
#'   \item{\code{$featureSelection(reference, included = FALSE, method = "ll", verbose = TRUE, n = 250)}}{}
#'   \item{\code{$minimize()}}{}
#'   \item{\code{$as.sparseMatrix(x, col)}}{}
#'   \item{\code{$as.igraph(edgeAttributes = "ll", verticeAttributes = NULL, as.undirected = TRUE)}}{}
#'   \item{\code{$as.svg(object, layout = "kamada.kawai", verbose = TRUE, ...)}}{}
#'   \item{\code{$plot(...)}}{}
#' }
#' @importFrom parallel mcparallel mccollect
#' @importFrom htmltools html_print HTML
#' @importFrom data.table data.table melt.data.table
#' @importFrom RcppCWB cl_id2str cl_str2id cl_cpos2id
#' @import methods
#' @import polmineR
#' @importFrom igraph graph.data.frame delete.vertices
#' @importFrom igraph as.undirected
#' @importFrom RcppCWB get_cbow_matrix
#' @examples 
#' \dontrun{
#' library(polmineR)
#' library(polmineR.graph)
#' use("GermaParl")
#' 
#' merkel2008 <- partition(
#'   "GERMAPARL", speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word", name = "merkel2008"
#' )
#' terms_to_drop <- c(
#'   polmineR::punctuation,
#'   unlist(noise(p_attributes(merkel2008, p_attribute = "word")))
#' )
#' 
#' Merkel <- Cooccurrences$new(
#'   partition = merkel2008, p_attribute = "word", window = 5L,
#'   drop = terms_to_drop
#' )
#' Merkel$count()
#' Merkel$trim(action = "drop", by.id = TRUE)
#' Merkel$maths()
#' 
#' bt2008 <- partition(
#'   "GERMAPARL", year = 2008, interjection = FALSE,
#'   p_attribute = "word",
#'   name = "bt2008"
#' )
#' terms_to_drop <- c(
#'   polmineR::punctuation,
#'   unlist(noise(p_attributes(bt2008, p_attribute = "word")))
#' )
#' BT2008 <- Cooccurrences$new(partition = bt2008, p_attribute = "word", window = 5L, drop = terms_to_drop)
#' BT2008$count()
#' BT2008$trim(action = "drop", by.id = TRUE)
#' BT2008$maths()
#' 
#' Merkel2 <- copy(Merkel)
#' Merkel2$featureSelection(reference = BT2008, included = TRUE)
#' 
#' G <- Merkel$as.igraph(as.undirected = TRUE)
#' G <- igraph_add_communities(G, method = "fastgreedy", weights = FALSE)
#' G <- igraph_add_coordinates(G, layout = "kamada.kawai", dim = 3)
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
Cooccurrences <- R6::R6Class(
  
  classname = "Cooccurrences",
  
  public = list(
    
    corpus = NULL, # character 
    partition = NULL, # partition
    p_attribute = NULL, # character
    minimized = NULL, # logical
    keep = NULL, # list
    drop = NULL, # list
    method = NULL, # character
    window = NULL, # integer
    verbose = NULL, # logical
    contextSizes = NULL, # data.table
    dt = NULL, # data.table
    svg = NULL, # character"
  
    
    initialize = function(corpus = NULL, partition = NULL, p_attribute = "word", window = 5L, verbose = TRUE, drop = c(polmineR::punctuation, tm::stopwords("de")), keep = NULL){
      
      if (is.null(corpus) && is.null(partition)){
        stop("corpus and partition are NULL, one needs to be provided")
      }
      if(!is.null(corpus)){
        C <- Corpus$new(corpus)
        C$p_attribute <- character()
        self$partition <- C$as.partition()
        self$corpus <- corpus
      } else if (!is.null(partition)){
        self$partition <- partition
        self$corpus <- partition@corpus
      }
      
      self$p_attribute <- p_attribute
      self$window <- as.integer(window)
      self$verbose <- verbose
      
      # for convencience, keep may be a character vector - turn that into list structure
      if (is.vector(keep)){
        if (length(p_attribute) == 1){
          self$keep <- list()
          self$keep[[p_attribute]] <- keep
        } else {
          stop("Param 'keep' is a vector, but length(p_attribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(keep)){
        self$keep <- list
      }
      
      # the same for drop
      if (is.vector(drop)){
        if (length(p_attribute) == 1){
          self$drop <- list()
          self$drop[[p_attribute]] <- drop
        } else {
          stop("Param 'drop' is a vector, but length(p_attribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(drop)){
        self$drop <- list
      }
    },
    
    show = function(){
      cat("Object of 'cooccurrences'-class\n")
    },
    
    summary = function(){
      message("not yet implemented")
    },
    
    count = function(){
      
      if (length(self$p_attribute) == 1L && requireNamespace("RcppCWB", quietly = TRUE)){
        if (self$verbose) message("... getting window matrix (using Rcpp)")
        windowMatrix <- RcppCWB::get_cbow_matrix(
          corpus = self$partition@corpus, p_attribute = self$p_attribute,
          registry = Sys.getenv("CORPUS_REGISTRY"),
          matrix = self$partition@cpos, window = self$window
        )
        windowDT <- as.data.table(windowMatrix)
        
        rm(windowMatrix); gc()
        
        setnames(windowDT, old = paste("V", self$window + 1, sep = ""), new = "a_id")
        if (self$verbose) message("... melting")
        coocDT <- data.table::melt.data.table(windowDT, id.vars = "a_id", value.name = "b_id")
        
        rm(windowDT); gc()
        
        coocDT[, "variable" := NULL, with = TRUE]
        
        if (self$verbose) message("... kicking out -1")
        DTmin <- coocDT[b_id != -1]
        self$contextSizes <- DTmin[, .N, by = "a_id"]
        setnames(self$contextSizes, old = "N", new = "size_window")
        if (self$verbose) message("... counting cooccurrences")
        self$dt <- DTmin[, .N, by = c("a_id", "b_id"), with = TRUE]
        setnames(self$dt, "N", "ab_count")
        setkeyv(self$dt, "a_id")
        
      } else {
        
        if (length(self$p_attribute) == 0) stop("The partition is required to included counts. Enrich the object first!")
        
        pAttr <- sapply(p_attribute, function(x) paste(self$corpus, x, sep = "."))
        aColsId <- setNames(paste("a", self$p_attribute, "id", sep="_"), self$p_attribute)
        bColsId <- setNames(paste("b", self$p_attribute, "id", sep="_"), self$p_attribute)
        aColsStr <- setNames(paste("a", self$p_attribute, sep="_"), self$p_attribute)
        bColsStr <- setNames(paste("b", self$p_attribute, sep="_"), self$p_attribute)
        
        .makeWindows <- function(i, cpos, ...){
          cposMin <- cpos[i,1]
          cposMax <- cpos[i,2]
          if (cposMin != cposMax){
            cposRange <- cposMin:cposMax
            lapply(
              setNames(cposRange, cposRange),
              function(x) {
                cpos <- c((x - window):(x - 1L), (x + 1L):(x + window))
                cpos <- cpos[which(cpos >= cposMin)]
                cpos[which(cpos <= cposMax)]
              })
          }
        }
        bag <- blapply(as.list(c(1:nrow(self$partition@cpos))), f = .makeWindows, cpos = self$partition@cpos, mc = mc)
        bCpos <- lapply(
          bag,
          function(x) lapply(names(x), function(y) rep(as.numeric(y), times = length(x[[y]])))
        )
        if (self$verbose) message("... putting together data.table")
        DT <- data.table(a_cpos = unlist(bag), b_cpos = unlist(bCpos))
        
        if (self$verbose) message("... getting token ids")
        lapply(
          p_attribute, function(x){
            DT[, eval(aColsId[x]) := cl_cpos2id(corpus = self$partition@corpus, p_attribute = x, cpos = DT[["a_cpos"]]), with = TRUE]
            DT[, eval(bColsId[x]) := cl_cpos2id(corpus = self$partition@corpus, p_attribute = x, cpos = DT[["b_cpos"]]), with = TRUE]
          }
        )
        if (self$verbose) message("... counting window size")
        
        contextDT <- DT[, .N, by = c(eval(aColsId)), with = TRUE]
        setnames(contextDT, "N", "size_window")
        self$contextSizes <- contextDT
        
        if (self$verbose) message("... counting co-occurrences")
        TF <- DT[, .N, by = c(eval(c(aColsId, bColsId))), with = TRUE]
        setnames(TF, "N", "ab_count")
        
        if (self$verbose) message("... adding window size")
        setkeyv(contextDT, cols = aColsId)
        setkeyv(TF, cols = aColsId)
        TF <- contextDT[TF]
      }
    },
    
    trim = function(action, by.id){
      # turn tokens to keep to id
      
      toMatch <- self[[action]]
      if (by.id == TRUE){
        toMatch <- lapply(
          setNames(names(toMatch), names(toMatch)),
          function(x) cl_str2id(corpus = self$corpus, p_attribute = x, str = toMatch[[x]])
          )
        if (length(self$p_attribute) > 1){
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
            grep(colRegex, colnames(self$dt)),
            function(i) which(self$dt[[i]] %in% toMatch[[pAttr]])
          )}
      )
      
      rowindex <- unique(unlist(indexList))
      if (action == "keep"){
        self$dt <- self$dt[rowindex]
      } else if (action == "drop"){
        if (length(rowindex) > 0){
          self$dt <- self$dt[-rowindex]
        }
      }
    },
    
    makeTermCooccurrenceMatrix = function(){
      
      if (self$verbose) message("... creating data.table for reindexing")  
      ID2STR <- data.table(id = unique(self$dt[["a_id"]]))
      ID2STR[ , str := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$p_attribute, id = ID2STR[["id"]]), from = getEncoding(self$corpus))]
      setkeyv(ID2STR, cols = "id")
      setorderv(ID2STR, cols = "id")
      ID2STR[, "id_new" := 1:nrow(ID2STR), with = TRUE]
      setkeyv(self$dt, "a_id")
      
      if (self$verbose) message("... id2str for a")
      coocCount2 <- self$dt[ID2STR]
      data.table::setnames(coocCount2, old = c("str", "id_new"), new = c("a_token", "a_new_key"))
      setkeyv(coocCount2, "b_id")
      if (self$verbose) message("... id2str for b")
      coocCount3 <- coocCount2[ID2STR]
      rm(coocCount2)
      setnames(coocCount3, old = c("str", "id_new"), new = c("b_token", "b_new_key"))
      if (self$verbose) message("... preparing simple_triplet_matrix")
      retval <- slam::simple_triplet_matrix(
        i = coocCount3[["a_new_key"]],
        j = coocCount3[["b_new_key"]],
        v = coocCount3[["ab_count"]],
        dimnames = list(ID2STR[["str"]], ID2STR[["str"]])
      )
      return(retval)
      
    },
    
    maths = function(method = "ll", verbose = self$verbose){
      
      if (is.null(self$dt)){
        self$count()
        self$trim(action = "drop", by.id = TRUE)
      }
      
      if (self$verbose) message("... adding window size")
      
      setkeyv(self$contextSizes, "a_id")
      setkeyv(self$dt, "a_id")
      DT <- self$contextSizes[self$dt]
      
      if (length(self$p_attribute) == 1L){
        
        DT[, "a" := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$partition@p_attribute, id = DT[["a_id"]]), from = registry_get_encoding(self$corpus))]
        DT[, "b" := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$partition@p_attribute, id = DT[["b_id"]]), from = registry_get_encoding(self$corpus))]
        DT[, "a_id" := NULL][, "b_id" := NULL]
        setkeyv(self$partition@stat, self$partition@p_attribute)
        setkeyv(DT, cols = "a")
        DT2 <- self$partition@stat[DT]
        
        rm(DT); gc()
        setnames(DT2, old = c(self$p_attribute, "count", paste(self$p_attribute, "id", sep = "_")), new = c("a", "a_count", paste("a", self$p_attribute, "id", sep = "_")))
        
        setkeyv(DT2, cols = "b")
        
        self$dt <- self$partition@stat[DT2]
        rm(DT2); gc()
        
        setnames(self$dt, old = c(self$p_attribute, "count", paste(self$p_attribute, "id", sep = "_")), new = c("b", "b_count", paste("b", self$p_attribute, "id", sep = "_")))
        setnames(self$dt, old = c("a", "b"), new = c(paste("a", self$p_attribute, sep = "_"), paste("b", self$p_attribute, sep = "_")))
        
      } else {
        # if (verbose == TRUE) message("... converting ids to strings")
        # lapply(
        #   c(1:length(p_attribute)),
        #   function(i){
        #     TF[, eval(aColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, p_attribute[i], TF[[aColsId[i]]])), with = TRUE]
        #     TF[, eval(bColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, p_attribute[i], TF[[bColsId[i]]])), with=TRUE]
        #     TF[, eval(aColsId[i]) := NULL]
        #     TF[, eval(bColsId[i]) := NULL]
        #   }
        # )
        # setkeyv(TF, cols = aColsStr)
        # setkeyv(.Object@stat, cols = p_attribute)
        # TF[, "count_a" := .Object@stat[TF][["count"]]]
        # setkeyv(TF, cols=bColsStr)
        # TF[, "count_b" := .Object@stat[TF][["count"]]]
        # setcolorder(TF, c(aColsStr, bColsStr, "ab_count", "count_a", "count_b", "size_window"))
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
        #   if (length(p_attribute) > 1){
        #     TF[, "strKeyA" := apply(TF[, eval(paste("a", p_attribute, sep = "_")), with = FALSE], 1, concatenate)]
        #     TF[, "strKeyB" := apply(TF[, eval(paste("b", p_attribute, sep = "_")), with = FALSE], 1, concatenate)]
        #   } else {
        #     setnames(TF, old = paste("a", p_attribute, sep = "_"), new = "strKeyA")
        #     setnames(TF, old = paste("b", p_attribute, sep = "_"), new = "strKeyB")
        #   }
        #   uniqueKey <- unique(c(TF[["strKeyA"]], TF[["strKeyB"]]))
        #   keys <- setNames(c(1:length(uniqueKey)), uniqueKey)
        #   i <- unname(keys[TF[["strKeyA"]]])
        #   j <- unname(keys[TF[["strKeyB"]]])
        #   retval <- simple_triplet_matrix(
        #     i = i, j = j, v = TF[["ab_count"]],
        #     dimnames = list(a = names(keys)[1:max(i)], b = names(keys)[1:max(j)])
        #   )
        #   return(retval)
        # }
        
      }
      
      if ("ll" %in% method) {
        
        if (verbose) message('... g2-Test')
        
        exp_total <- self$dt[["b_count"]] / self$partition@size
        count_ref <- self$dt[["b_count"]] - self$dt[["ab_count"]]
        count_ref <- ifelse(count_ref < 0L, 0L, count_ref)
        self$dt[, "exp_coi" := self$dt[["size_window"]] * exp_total]
        self$dt[, "exp_ref" := (self$partition@size - self$dt[["size_window"]]) * exp_total]
        
        A <- self$dt[["ab_count"]] / self$dt[["exp_coi"]]
        B <- count_ref / self$dt[["exp_ref"]]
        C <- ifelse(B > 0, log(B), log(0.0000001))
        D <- count_ref * C
        ll_value <- 2 * (self$dt[["ab_count"]] * log(A) + D)
        
        direction <- ifelse(self$dt[["ab_count"]] < self$dt[["exp_coi"]], -1L, 1L)
        self$dt[, "ll" := ll_value * direction]
        
        setorderv(self$dt, cols = "ll", order = -1)
        self$dt[, "rank_ll" := 1L:nrow(self$dt)]
        
      }
      invisible(self)

    },
    
    getFeatureSelectionStats = function(reference, included = FALSE, method = "ll", verbose = TRUE){
      
      if (!identical(self$p_attribute, reference$p_attribute)) {
        warning("BEWARE: cooccurrences objects are not based on the same p_attribute!")
      }
      
      if (verbose) message("... preparing tabs for matching")
      keys <- unlist(lapply(c("a", "b"), function(ab) paste(ab, self$p_attribute, sep = "_"))) 
      setkeyv(self$dt, keys)
      setkeyv(reference$dt, keys)
      MATCH <- reference$dt[self$dt]
      
      # remove columns not needed
      setnames(MATCH, old = c("ab_count", "i.ab_count"), new = c("count_ref", "count_coi"))
      colsToKeep <- c(keys, "count_ref", "count_coi")
      colsToDrop <- colnames(MATCH)[!colnames(MATCH) %in% colsToKeep]
      for (drop in colsToDrop) MATCH[, eval(drop) := NULL, with = TRUE]
      if (included) MATCH[, "count_ref" := MATCH[["count_ref"]] - MATCH[["count_coi"]] ]
      
      compObject <- new(
        "features",
        included = FALSE, corpus = self$corpus, size_coi = self$partition@size,
        size_ref = if (included) reference$partition@size - self$partition@size else reference$partition@size,
        p_attribute = self$p_attribute,
        stat = MATCH
      )
      
      for (how in method){
        if (verbose == TRUE) message("... statistical test: ", how)
        compObject <- do.call(how, args = list(.Object = compObject))
      }
      return(compObject@stat)
    },
    
    
    featureSelection = function(reference, included = FALSE, method = "ll", verbose = TRUE, n = 250){
      DT <- self$getFeatureSelectionStats(reference = reference, included = included, method = method, verbose = verbose)
      keys <- unlist(lapply(c("a", "b"), function(what) paste(what, self$p_attribute, sep = "_")))
      rowsToKeep <- c(keys, "rank_ll")
      DT <- DT[, rowsToKeep, with = FALSE]
      DT[, keep := ifelse(rank_ll <= n, TRUE, FALSE)]
      DT[, rank_ll := NULL]
      DT2 <- DT[DT[["keep"]] == TRUE]
      
      setkeyv(self$dt, keys)
      setkeyv(DT2, keys)
      Y <- self$dt[DT2]
      self$dt <- Y
    },
    
    minimize = function(){
      DT <- copy(self$dt)
      aColsStr <- paste("a_", self$p_attribute, sep="")
      bColsStr <- paste("b_", self$p_attribute, sep="")
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
      self$minimized <- TRUE
      self$dt <- FIN
    },
    
    as.sparseMatrix = function(x, col){
      uniqueTerms <- unique(c(self$dt[,"node"], self$dt[,"cooccurrence"]))
      keyVector <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
      splittedTab <- split(x = self$dt[,c(col, "cooccurrence")], f = self$dt[,"node"])
      
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
    
    as.igraph = function(edgeAttributes = c("ll", "ab_count", "rank_ll"), verticeAttributes = "count", as.undirected = TRUE){
      if (!all(edgeAttributes %in% colnames(self$dt))){
        warning("edgeAttribute supplied is not available")
      }
      tab <- as.data.frame(self$dt)
      aColsStr <- paste("a", self$p_attribute, sep = "_")
      bColsStr <- paste("b", self$p_attribute, sep = "_")
      if (length(self$p_attribute) == 1L){
        tab[["node"]] <- tab[[aColsStr]]
        tab[["collocate"]] <- tab[[bColsStr]]
      } else {
        tab[["node"]] <- apply(tab, 1, function(x) paste(x[aColsStr], collapse = "//"))
        tab[["collocate"]] <- apply(tab, 1, function(x) paste(x[bColsStr], collapse="//"))
      }
      g <- graph.data.frame(tab[, c("node", "collocate", edgeAttributes)])
      if ("count" %in% verticeAttributes){
        TF <- self$partition@stat # this will be a data.frame
        if (length(self$p_attribute) == 1){
          TF[, "key" := TF[[self$p_attribute]] ]
        } else{
          TF[, "key" := apply(TF, 1, function(row) paste(row[self$p_attribute], collapse = "//"))]
        }
        setkey(TF, key)
        tfVector <- TF[names(V(g))][["count"]]
        V(g)$count <- tfVector
        V(g)$freq <- round((tfVector / self$partition@size) * 100000, 3)
      }
      if (as.undirected) g <- as.undirected(g, edge.attr.comb = "concat")
      g <- delete.vertices(g, V(g)[name == "\u0084"])
      g <- delete.vertices(g, V(g)[name == "\u0093"])
      return(g)
    },
    
    as.svg = function(object, layout = "kamada.kawai", verbose = TRUE, ...){
      if (verbose) message("... creating igraph object (step 1)")
      igraphObject <- self$as.igraph()
      if (verbose) message("... creating svg object (step 2)")
      svgObject <- as.svg(igraphObject, verbose = verbose, ...)
      svgObject
    },
    
    plot = function(...){
      toBePlotted <- as.svg(self, ...)
      plot(toBePlotted)
    }
    
  )
)

