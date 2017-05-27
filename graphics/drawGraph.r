##############################################################################
# drawGraph.r
#
# Analyses for master thesis 
# ======================================
#
# Feeds the edgeList from BuildGraph.java into igraph for visualization
###############################################################################

require(igraph)

data = read.table("g1edgeList.txt", header = TRUE, sep = " ")
edgeList = as.matrix(data)
labels = read.table("g1labelList.txt", header = TRUE, sep = " ")

g1 = graph_from_edgelist(edgeList, directed = TRUE)
V(g1)$label = paste(labels[,1])
V(g1)$name = paste(labels[,1])

pdf("g1.pdf", width = 7, height = 6)
plot(g1, vertex.size = 25, edge.arrow.size = .7, vertex.label.cex = 1.8,
     layout = layout.fruchterman.reingold(g1))
dev.off()

data = read.table("g2edgeList.txt", header = TRUE, sep = " ")
edgeList = as.matrix(data)
labels = read.table("g2labelList.txt", header = TRUE, sep = " ")

g2 = graph_from_edgelist(edgeList, directed = TRUE)
V(g2)$label = paste(labels[,1])
V(g2)$name = paste(labels[,1])

pdf("g2.pdf", width = 7, height = 6)
plot(g2, vertex.size = 25, edge.arrow.size = .7, vertex.label.cex = 1.8,
     layout = layout.fruchterman.reingold(g2))
dev.off()

