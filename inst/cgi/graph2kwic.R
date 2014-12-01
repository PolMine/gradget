run <- function(...){
  passedArgs <- request[["query.string"]]
  par4call <- lapply(unlist(strsplit(passedArgs[1], "__")), function(x){unlist(strsplit(x, "="))})
  names(par4call) <- lapply(par4call, function(x)x[1])
  par4call <- lapply(par4call, function(x)x[2])
  load(file.path(drillingControls$partitionDir, paste(par4call$partition, ".RData", sep="")))
  css <- paste(scan(file=system.file("css", "markdown7.css", package="polmineR"), what="character", sep="\n"), collapse="\n")
  css <- paste("<style>", css, "</style>")
  foo <- kwic(eval(parse(text=par4call$partition)), par4call$node, collocate=par4call$collocate)
  require(xtable)
  retval <- capture.output(print(xtable(foo@table, align=c(rep("left", times=(ncol(foo@table)-2)), "right", "center", "left")), type="html"))
  retval <- gsub(par4call$node, paste('<font color="RoyalBlue">', par4call$node, "<color>", sep=""), retval)
  if (!is.null(par4call$collocate)) retval <- gsub(par4call$collocate, paste('<font color="OliveDrab">', par4call$collocate, "</font>", sep=""), retval)
  out(css)
  out(retval)
}
