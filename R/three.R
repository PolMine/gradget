#' Generate and manage 3d graph.
#' 
#' Generate 3d graph with three.js
#' 
#' @param data Input data.
#' @param bgColor Background color, a hex value (defaults to "0x888888").
#' @param knitr A logical value, whether htmlwidget is embedded in
#'   knitr/Rmarkdown document. If TRUE, corners will be somewhat rounded, and a
#'   sizing mechanism will ensure that the chunk option is processed
#'   appropriately (best practice: fig.width = 9.5).
#' @param elementId Passed into \code{htmlwidgets::createWidget}, required to be
#'   "three" (default) if the widget shall be used directly, optionally NULL, if
#'   the widget shall be included in a shiny app.
#' @param raycaster ...
#' @param anaglyph ... 
#' @param width ...
#' @param height ...
#' @name three
#' @rdname three
#' @export three
#' @importFrom igraph V E get.edge.attribute
#' @importFrom htmlwidgets sizingPolicy
#' @examples
#' library(gradget)
#' library(polmineR)
#' library(magrittr)
#' use("GermaParl")
#' 
#' merkel2008 <- partition(
#'   "GERMAPARL",
#'   speaker = "Angela Merkel", year = 2008, interjection = FALSE,
#'   p_attribute = "word"
#' )
#' 
#' terms_to_drop <- p_attributes(merkel2008, p_attribute = "word") %>%
#'   noise(verbose = FALSE) %>%
#'   unlist() %>%
#'   unname() %>%
#'   c(polmineR::punctuation)
#' 
#' G <- Cooccurrences(merkel2008, "word", 5L, 5L, terms_to_drop) %>%
#'   ll() %>%
#'   decode() %>%
#'   subset(rank_ll <= 250) %>%
#'   as_igraph() %>%
#'   igraph_add_coordinates(layout = "kamada.kawai", dim = 3) %>%
#'   igraph_add_communities() %>%
#'   rescale(-250, 250)
#' 
#' G <- igraph_add_kwic(G, subcorpus = merkel2008)
#' 
#' igraph_as_gradget_data(G) %>% three(raycaster = TRUE) -> Y
three <- function(
  data, bgColor = "0x888888", raycaster = TRUE, anaglyph = FALSE, knitr = FALSE,
  width = NULL, height = NULL, elementId = "three"
  ){
  
  # if (is.null(V(G)$z)) warning("coordinates for threedimensional display are not available")

  x <- list(
    data = data,
    settings = list(bgColor = bgColor, raycaster = raycaster, anaglyph = anaglyph, knitr = knitr, width = width, height = height)
  )
  
  bootbox <- htmltools::htmlDependency(
    name = "bootbox",
    version = "4.4.0",
    src = system.file(package = "gradget", "www", "bootbox", "js"),
    script = "bootbox.min.js"
  )

  deps <- list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap("default"),
    bootbox
  )
  
  # create the widget
  htmlwidgets::createWidget(
    name = "three", x = x,
    # íf elementId is used we get warning: Ignoring explicitly provided widget ID "three"; Shiny doesn't use them
    elementId = elementId, 
    width = width, height = height,
    sizingPolicy = sizingPolicy(
      padding = 0,
      viewer.padding = 0,
      browser.padding = 0, browser.fill = TRUE,
      knitr.defaultHeight = 800, knitr.defaultWidth = 600
    ),
    package = "gradget",
    dependencies = deps
  )
}


#' @export threeOutput
#' @importFrom htmlwidgets shinyWidgetOutput
#' @rdname three
threeOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "three", width, height, package = "gradget")
}


#' @param outputId output variable to read from
#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate expr.
#' @param quoted Is \code{expr} a quoted expression (with quote())? This is useful if
#'   you want to save an expression in a variable.
#' @export renderThree
#' @importFrom htmlwidgets shinyRenderWidget
#' @rdname three
renderThree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, threeOutput, env, quoted = TRUE)
}

