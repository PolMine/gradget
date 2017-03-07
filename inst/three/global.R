library(shiny)
library(shinyjs)
library(shinythemes)
library(colourpicker)
library(data.table)

library(igraph)
library(three)

library(polmineR.shiny)
library(polmineR)

library(magrittr)
library(DT)

assign(
  "partitionNames",
  c(getObjects('partition'), getObjects('pressPartition'), getObjects('plprPartition')),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)

assign(".polmineR_graph_cache", new.env())

# startingTime is needed by kwicServer
assign(
  "startingTime", as.character(Sys.time()),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)

jsFunctionClick <- paste(scan(
  file = system.file("js", "onclick_functions_2d.js", package = "polmineR.graph"),
  what = character(),
  sep = "\n", quiet = TRUE
), collapse = "\n")


i <- 0
