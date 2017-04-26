library(polmineR)
library(polmineR.shiny)

library(shiny)
library(shinyjs)
library(shinythemes)
library(colourpicker)

library(data.table)
library(igraph)
library(three)


library(magrittr)
library(DT)


values <- reactiveValues()
values[["startingTime"]] <- as.character(Sys.time()) # needed by kwicServer
values[["partitions"]] <- list()
values[["fulltext"]] <- ""


jsFunctionClick <- paste(scan(
  file = system.file("js", "onclick_functions_2d.js", package = "polmineR.graph"),
  what = character(),
  sep = "\n", quiet = TRUE
), collapse = "\n")


# i <- 0
