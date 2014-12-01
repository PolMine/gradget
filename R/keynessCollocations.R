#' Turn a keyness collocations object into an igraph-object
#' 
#' @examples
#' \dontrun{
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_speaker="Angela Merkel"))
#' bt17cducsu <- partition("PLPRTXT", list(text_lp="17", text_type="speech", text_party="CDU_CSU"))
#' bt17merkelColl <- collocations(bt17merkel, pAttribute="lemma", progress=TRUE)
#' bt17cducsuColl <- collocations(bt17cducsu, pAttribute="lemma", progress=TRUE)
#' merkel <- keyness(bt17merkelColl, bt17cducsuColl, included=TRUE, mc=FALSE)
#' merkelG <- asIgraph(merkel)
#' }
#' @rdname asIgraph
setMethod("asIgraph", "keynessCollocations", function(x){
  return(NULL)
})