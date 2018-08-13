#' Make svg for collocations graph
#' 
#' The resulting svg graph will have clickable nodes and edges.
#' 
#' @examples
#' \dontrun{
#' 
#' Y <- SVG$new(G)
#' Y$make()
#' Y$as_htmlwidget()
#' 
#' Y$browse()
#' }
#' @rdname two
#' @export two
two <- function(svg){
  jsFunction <- "
      <script>
  function edgeClick(x, y){
  console.log(x);
  console.log(y);
  Shiny.onInputChange('kwic_query_edgeclick', x);
  Shiny.onInputChange('kwic_neighbor_edgeclick', y);
  };
  function nodeClick(x){
  console.log(x);
  Shiny.onInputChange('kwic_query', x);
  };
  </script>
  "
  svgPanZoom(svg)
  
}
