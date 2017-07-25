
data(MisLinks)
data(MisNodes)
# Plot
n <- forceNetwork(
    Links = MisLinks, Nodes = MisNodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name",
    Group = "group", 
    # Nodesize = "size", 
    opacity = 1
)
saveNetwork(n, file = 'test.html')
forceNetwork(
    Links = MisLinks, Nodes = MisNodes, Source = "source",
    Target = "target", Value = "value", NodeID = "name",
    Nodesize = "size",
    radiusCalculation = "Math.sqrt(d.nodesize)+6",
    Group = "group", opacity = 1, legend = T
)

URL <- paste0(
    "https://cdn.rawgit.com/christophergandrud/networkD3/",
    "master/JSONdata/miserables.json"
)
MisJson <- jsonlite::fromJSON(URL)
forceNetwork(
    Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
    Target = "target", Value = "value", NodeID = "name",
    Group = "group", opacity = 1
)
forceNetwork(
    Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
    Target = "target", Value = "value", NodeID = "name",
    Group = "group", opacity = 1, bounded = T
)
