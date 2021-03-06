% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradget.R
\name{gradget}
\alias{gradget}
\alias{gradgetOutput}
\alias{renderGradget}
\title{Generate and manage 3d graph.}
\usage{
gradget(data, bgColor = "0x888888", raycaster = TRUE, anaglyph = FALSE,
  knitr = FALSE, width = NULL, height = NULL, elementId = "gradget")

gradgetOutput(outputId, width = "100\%", height = "400px")

renderGradget(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{data}{Input data.}

\item{bgColor}{Background color, a hex value (defaults to "0x888888").}

\item{raycaster}{...}

\item{anaglyph}{...}

\item{knitr}{A logical value, whether htmlwidget is embedded in
knitr/Rmarkdown document. If TRUE, corners will be somewhat rounded, and a
sizing mechanism will ensure that the chunk option is processed
appropriately (best practice: fig.width = 9.5).}

\item{width}{...}

\item{height}{...}

\item{elementId}{Passed into \code{htmlwidgets::createWidget}, required to be
"gradget" (default) if the widget shall be used directly, optionally NULL, if
the widget shall be included in a shiny app.}

\item{outputId}{output variable to read from}

\item{expr}{An expression that generates an HTML widget}

\item{env}{The environment in which to evaluate expr.}

\item{quoted}{Is \code{expr} a quoted expression (with quote())? This is useful if
you want to save an expression in a variable.}
}
\description{
Generate 3d graph with three.js
}
\examples{
library(gradget)
library(polmineR)
library(magrittr)
use("GermaParl")

merkel2008 <- partition(
  "GERMAPARL",
  speaker = "Angela Merkel", year = 2008, interjection = FALSE,
  p_attribute = "word"
)

terms_to_drop <- p_attributes(merkel2008, p_attribute = "word") \%>\%
  noise(verbose = FALSE) \%>\%
  unlist() \%>\%
  unname() \%>\%
  c(polmineR::punctuation)

G <- Cooccurrences(merkel2008, "word", 5L, 5L, terms_to_drop) \%>\%
  ll() \%>\%
  decode() \%>\%
  subset(rank_ll <= 250) \%>\%
  as_igraph() \%>\%
  igraph_add_coordinates(layout = "kamada.kawai", dim = 3) \%>\%
  igraph_add_communities() \%>\%
  rescale(-250, 250)

G <- igraph_add_kwic(G, subcorpus = merkel2008)
D <- igraph_as_gradget_data(G)
W <- gradget(D, raycaster = TRUE)
}
