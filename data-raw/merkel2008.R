# The package includes as sample data an object 'merkel2008'. This is how
# it was generated

library(polmineR.graph)
library(polmineR)
use("GermaParl")

merkel2008 <- partition(
  "GERMAPARL",
  speaker = "Angela Merkel", year = 2008, interjection = FALSE,
  p_attribute = "word"
)

terms_to_drop <- c(
  polmineR::punctuation,
  unlist(noise(p_attributes(merkel2008, p_attribute = "word")))
)

Merkel <- Cooccurrences$new(
  partition = merkel2008,
  p_attribute = "word",
  window = 5L,
  drop = terms_to_drop
)
Merkel$count()
Merkel$trim(action = "drop", by.id = TRUE)
Merkel$maths()

bt2008 <- partition(
  "GERMAPARL",
  year = 2008, interjection = "FALSE",
  p_attribute = "word", name = "bt2008"
)
terms_to_drop <- c(
  polmineR::punctuation,
  unlist(noise(p_attributes(bt2008, p_attribute = "word")))
)
BT2008 <- Cooccurrences$new(
  partition = bt2008,
  p_attribute = "word",
  window = 5L,
  drop = terms_to_drop
)
BT2008$count()
BT2008$trim(action = "drop", by.id = TRUE)
BT2008$maths()

Merkel$featureSelection(reference = BT2008, included = TRUE)

G <- Merkel$as.igraph(as.undirected = TRUE)
G <- igraph_add_communities(G, method = "fastgreedy", weights = FALSE)
G <- igraph_add_coordinates(G, layout = "kamada.kawai", dim = 3)
G <- rescale(G, -1000, 1000)

merkel2008 <- G

save(merkel2008, file = "~/Lab/github/polmineR.graph/data/igraph.RData")


