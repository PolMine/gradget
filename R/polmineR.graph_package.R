#' How to get started
#' 
#' Find the cgi script that is shipped with the page 
#' system.file("cgi", "graph2kwic.R", package="polmineR.graph")
#' and copy it to /var/FastRWeb/web.R
#' Add/modify line in /var/FastRWeb/code/rserve.R: 
#' pkgs <- c("XML", "Cairo", "Matrix", "FastRWeb", "polmineR", "pipeR", "markdown")
#' To start FastRWeb: sudo /var/FastRWeb/code/start
#' @keywords package
#' @docType package
#' @rdname polmineR.graph
#' @name polmineR.graph-package
#' @import data.table
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(polmineR.graph)
#' library(three)
#' 
#' bt17merkel <- partition(
#'  "PLPRBT", speaker_lp = "17", speaker_name = "Angela Merkel",
#'  speaker_type = "speech", pAttribute = "word"
#' )
#' bt17merkelColl <- cooccurrences(bt17merkel, pAttribute = "word", mc = TRUE)
#' 
#' bt17 <- partition("PLPRBT", speaker_lp = "17", speaker_type = "speech", pAttribute = "word")
#' bt17cooc <- cooccurrences(bt17, pAttribute = "word", mc = TRUE, progress = TRUE)
#' 
#' C <- compare(bt17, bt17cooc)
#' 
#' bt17merkelCollTrimmed <- subset(bt17merkelColl, rank_ll <= 250)
#' iMerkel <- asIgraph(bt17merkelCollTrimmed)
#' iMerkelComm <- enrich(iMerkel, community=list(method="fastgreedy", weights=FALSE))
#' iMerkel3d <- enrich(iMerkelComm, layout="kamada.kawai", dim=3)
#' iMerkel3d <- three::rescale(iMerkel3d, -400, 400)
#' t <- polmineR.graph::as.three(iMerkel3d, bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' t <- polmineR.graph::as.three(iMerkel3d, type="raycaster", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' t <- as.three(iMerkel3d, type="anaglyph", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' t <- as.three(iMerkel3d, type="disco", bgColor="0xcccccc", fontSize=12, fontColor="0x000000", nodeSize=4, edgeColor="0xeeeeee", edgeWidth=3, fontOffset=c(x=10,y=10,z=10))
#' toView <- three:::store(t, directory="/Users/blaette/Lab/tmp/three")
#' shiny::runApp("/Users/blaette/Lab/github/polmineR.graph/inst/three")
#' browseURL(toView["tmpFileJs"])
#' }
NULL 

setOldClass("igraph")

#' @export polmineR.graph
#' @rdname polmineR.graph
polmineR.graph <- function(){
  shiny::runApp(system.file("three", package = "polmineR.graph"))
}
