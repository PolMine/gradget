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
#' @rdname polmineR.graph-package
#' @name polmineR.graph-package
NULL 