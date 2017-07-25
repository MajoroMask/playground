library(HiveR)
dataSet <- read.table("./in/lesmis.txt", header = FALSE, sep = "\t")
hive1 <- edge2HPD(edge_df = dataSet)
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
hive4 <- mineHPD(hive3, option = "remove zero edge")
plotHive(
    hive4, method = "abs", bkgnd = "white", 
    axLabs = c("source", "hub", "sink"), axLab.pos = 1
)
gD <- simplify(graph.data.frame(dataSet, directed=FALSE))
gAdj <- get.adjacency(
    gD, type = "upper", edges = FALSE, names = TRUE, sparse = FALSE
) 
hive1 <- adj2HPD(gAdj, type = "2D")
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
hive4 <- mineHPD(hive3, option = "remove zero edge")
plotHive(
    hive4, method = "abs", bkgnd = "white", 
    axLabs = c("source", "hub", "sink"), axLab.pos = 1
)
