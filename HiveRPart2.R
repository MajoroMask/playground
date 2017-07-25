library(igraph)
library(plyr)
library(HiveR)
library(RColorBrewer)
# http://www.vesnam.com/Rblog/viznets3/ ----
dataSet <- read.table("./in/lesmis.txt", header = FALSE, sep = "\t")
gD <- simplify(graph.data.frame(dataSet, directed=FALSE))
# 计算connectivity
degAll <- degree(gD, v = V(gD), mode = "all")
# 计算betweenness
betAll <- betweenness(gD, v = V(gD), directed = FALSE) / 
    (((vcount(gD) - 1) * (vcount(gD) - 2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
node.list <- data.frame(name = V(gD)$name, degree = degAll, betw = betAll.norm)
# 
dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")
F1 <- function(x) {
    data.frame(
        V4 = dsAll[
            which(V(gD)$name == as.character(x$V1)), 
            which(V(gD)$name == as.character(x$V2))
        ]
    )
}
dataSet.ext <- ddply(
    dataSet, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x))
)
rm(degAll, betAll, betAll.norm, F1)
#Determine node/edge color based on the properties

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(node.list$bet)))
nodes_size <- sapply(node.list$bet, function(x) approxVals$y[which(sort(unique(node.list$bet)) == x)])
node.list <- cbind(node.list, size = nodes_size)
rm(approxVals, nodes_size)

# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")
# This function returns a function corresponding to a collor palete of "bias" number of elements
F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"), bias = length(unique(node.list$degree)), space = "rgb", interpolate = "linear")
# Now we'll create a color for each degree
colCodes <- F2(length(unique(node.list$degree)))
# And we will assign a color for each node based on its degree
nodes_col <- sapply(node.list$degree, function(x) colCodes[which(sort(unique(node.list$degree)) == x)])
node.list <- cbind(node.list, color = nodes_col)
rm(F2, colCodes, nodes_col)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(dataSet.ext$V4)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(dataSet.ext$V4)))
edges_col <- sapply(dataSet.ext$V4, function(x) colCodes[which(sort(unique(dataSet.ext$V4)) == x)])
dataSet.ext <- cbind(dataSet.ext, color = edges_col)
rm(F2, colCodes, edges_col)
############################################################################################
# Assign nodes to axes

# Randomly
nodeAxis <- sample(3, nrow(node.list), replace = TRUE )
node.list <- cbind(node.list, axis = nodeAxis)
rm(nodeAxis)

############################################################################################
#Create a hive plot

source("mod.edge2HPD.R")

hive1 <- mod.edge2HPD(edge_df = dataSet.ext[, 1:2], edge.weight = dataSet.ext[, 3], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "degree")], node.axis = node.list[,c("name", "axis")])
#sumHPD(hive1)

hive2 <- mineHPD(hive1, option = "remove zero edge")

plotHive(hive2, method = "abs", bkgnd = "white",  axLab.pos = 1)


########################################
# Based on hierarchical cluestering
d <- dist(dsAll)
hc <- hclust(d)
#plot(hc)
nodeAxis <- cutree(hc, k = 6)
node.list <- cbind(node.list, axisCl = nodeAxis)
rm(nodeAxis)

hive1 <- mod.edge2HPD(edge_df = dataSet.ext[, 1:2], edge.weight = dataSet.ext[, 3], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "degree")], node.axis = node.list[,c("name", "axisCl")])
#sumHPD(hive1)

hive2 <- mineHPD(hive1, option = "remove zero edge")

plotHive(hive2, method = "abs", bkgnd = "white",  axLab.pos = 1)
