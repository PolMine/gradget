library(shiny)
library(shinyjs)
library(shinythemes)

library(polmineR.shiny)
library(polmineR)

library(magrittr)
library(DT)

assign(
  "partitionNames",
  c(getObjects('partition'), getObjects('pressPartition'), getObjects('plprPartition')),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)

# startingTime is needed by kwicServer
assign(
  "startingTime", as.character(Sys.time()),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)
