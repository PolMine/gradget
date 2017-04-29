.asDOMElement <- function(x){
  x <- rescale(x, -600, 600)
  svg <- SVG$new(x)
  svg$make(width = 800, height = 800)
  y <- svg$xml
  y <- gsub("^.*?(<svg.*?</svg>).*$", "\\1", y)
  
  jsFunctionClick <- paste(scan(
    file = system.file("js", "onclick_functions_2d.js", package = "polmineR.graph"),
    what = character(),
    sep = "\n", quiet = TRUE
  ), collapse = "\n")
  
  y <- gsub("(<svg.*?>)", paste("\\1<script>", jsFunctionClick, "</script>", sep = ""), y)
  y
}

# turn igraph object into the json that is needed by THREE
# x needs to be an igraph object
.igraphToJson <- function(x){
  
  message("... three dimensions")
  T <- Three$new(x)
  T$type = "anaglyph"
  T$bgColor = "0xcccccc"
  T$fontSize = 12
  T$fontColor = "0x000000"
  T$nodeSize = 4
  T$edgeColor = "0xeeeeee"
  T$edgeWidth = 3
  T$fontOffset = c(x = 10, y = 10, z = 10)
  T$make()
  
  message("... creating json")
  newJson <- T$as.json()
  newJson
}
