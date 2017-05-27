##############################################################################
# corPlot.r
#
# Analyses for master thesis
# ======================================
#
# Draw correlation plots between the distances.
###############################################################################

require(ggplot2)
require(gridExtra)
require(cowplot)
require(tikzDevice)

observedNaive = read.table("naiveDataPopulationBubblesRemoved.txt", header = TRUE)[-2:-1,-1]
observedBayes = read.table("bayesData.txt", header = TRUE)[-2:-1,-1]
observedGED = read.table("./GEDresults/gedData.txt", header = TRUE)

# Convert to vector and remove NA's
observedNaive = na.omit(as.vector(t(as.matrix(observedNaive))))
observedBayes = na.omit(as.vector(t(as.matrix(observedBayes))))
observedBayes = observedBayes - min(observedBayes) + 1000
observedGED = na.omit(as.vector(t(as.matrix(observedGED))))
df = data.frame(cbind(observedNaive, observedGED, observedBayes))

# Produce graphics
tikz(file = "../Thesis/figures/corPlot.tex", width = 4, height = 7)
AXIS_SIZE = 10

p1 = ggplot(df, aes(x = observedGED, y = observedNaive)) +
  geom_point() +
  scale_y_continuous(breaks = pretty(df$observedNaive, n = 4)) +
  xlab("$d_{GED}$") + ylab("$d_{BC}$") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) 

p2 = ggplot(df, aes(x = observedBayes, y = observedNaive)) +
  geom_point() +
  xlab("$d_{BF}$") + ylab("$d_{BC}$") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE))  

p3 = ggplot(df, aes(x = observedBayes, y = observedGED)) +
  geom_point() +
  scale_y_continuous(breaks = pretty(df$observedNaive, n = 4)) +
  xlab("$d_{BF}$") + ylab("$d_{GED}$") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE))
        
p = grid.arrange(p1, p2, p3, ncol = 1)
print(p)
dev.off()
