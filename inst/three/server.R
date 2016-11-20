library(shiny)
library(polmineR)
library(magrittr)
library(DT)


shinyServer(function(input, output, session) {
  graphServer(input, output, session)
})
