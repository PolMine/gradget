library(polmineR)
library(polmineR.graph)

source(system.file(package = "polmineR", "shiny", "modules", "dispersion.R"))
source(system.file(package = "polmineR", "shiny", "modules", "partition.R"))
# source(system.file(package = "polmineR", "shiny", "modules", "kwic.R"))
source("~/Lab/github/polmineR/inst/shiny/modules/kwic.R")
source(system.file(package = "polmineR", "shiny", "modules", "read.R"))
source(system.file(package = "polmineR", "shiny", "modules", "utils.R"))

source("modules/cooccurrences.R")
source("modules/graph.R")
source("modules/utils.R")
source("modules/settings.R")

library(shiny)
library(shinyjs)
library(shinythemes)
library(colourpicker)

library(data.table)
library(igraph)
library(three)

library(magrittr)
library(DT)

debug <- TRUE

values <- reactiveValues()
values[["startingTime"]] <- as.character(Sys.time()) # needed by kwicServer
values[["partitions"]] <- list()
values[["fulltext"]] <- ""


jsFunctionClick <- paste(scan(
  file = system.file("js", "onclick_functions_2d.js", package = "polmineR.graph"),
  what = character(),
  sep = "\n", quiet = TRUE
), collapse = "\n")
