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

i <- 0


coocObject <- get(getObjects("cooccurrences", envir = .GlobalEnv)[1])
coocObject@stat <- coocObject@stat[which(coocObject[["rank_ll"]] <= 250)]
igraphObject <- asIgraph(coocObject)
igraphObject <- enrich(igraphObject, community = list(method = "fastgreedy", weights=FALSE))
igraphObject <- enrich(igraphObject, layout = "kamada.kawai", dim = 3)
igraphObject <- three::rescale(igraphObject, -400, 400)
threeObject <- polmineR.graph::as.three(
  igraphObject, type = "anaglyph", bgColor = "0xcccccc", fontSize = 12, fontColor = "0x000000", nodeSize = 4,
  edgeColor = "0xeeeeee", edgeWidth = 3, fontOffset = c(x = 10, y = 10, z = 10)
  
)
newJson <- as(threeObject, "json")
jsonTmpFile <- tempfile()
cat(newJson, file = jsonTmpFile)


jsFunctionClick <- "
  
  function edgeClick(x, y){
    document.getElementById('cooccurrences_a').value = x;
    Shiny.onInputChange('cooccurrences_a', x);

    document.getElementById('cooccurrences_b').value = y;
    Shiny.onInputChange('cooccurrences_b', y);

    // document.getElementById('cooccurrences_time').value = Date();
    // Shiny.onInputChange('cooccurrences_time', Date());
    
    document.getElementById('cooccurrences_any').value = Date().toString();
    Shiny.onInputChange('cooccurrences_any', Date().toString());
    
    spacehits ++;
    Shiny.onInputChange('graph_space_pressed', spacehits);

};
  
  function nodeClick(x){
    document.getElementById('cooccurrences_a').value = x;
    Shiny.onInputChange('cooccurrences_a', x);

    document.getElementById('cooccurrences_b').value = '';
    Shiny.onInputChange('cooccurrences_b', '');

    spacehits ++;
    Shiny.onInputChange('graph_space_pressed', spacehits);
  };
"
