##############################################################################
# BF_inference.r
#
# Analyses for master thesis
# ======================================
#
# Explores some inference about the Bayes factor and the permutation
# distribution based on this measure.
###############################################################################

require(ggplot2)
require(cowplot)
require(wesanderson)
require(tikzDevice)

n = seq(1, 200, by = .01)
bayesFactor1 = log(2) + lgamma(2*n + 3) + 2*lgamma(n + 1) - lgamma(2*n + 1) - 2*lgamma(n + 3)
bayesFactor2 = log(2) + lgamma(2*n + 3) + 4*lgamma(n/2 + 1) - 2*lgamma(n + 1) - 2*lgamma(n + 3)
bayesFactor3 = log(2) + lgamma(2*n + 3) + 6*lgamma(n/3 + 1) - 3*lgamma((2*n)/3 + 1) - 2*lgamma(n + 3)

BF = c(bayesFactor1,bayesFactor2, bayesFactor3)
group = c(rep("A", length(n)),rep("B", length(n)), rep("C", length(n)))
df = data.frame(n = rep(n, 3), BF = BF, group = group)

tikz(file = "../Thesis/figures/BFofN.tex", width = 4, height = 5)

p = ggplot(df, aes(x = n, y = BF)) +
  geom_line(aes(colour = group, group = group)) +
  xlab("$n$") + ylab("$\\log(B_{10})$") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("$(n, 0, 0)\\quad$", "$(\\frac{n}{2}, \\frac{n}{2}, 0)\\quad$",
                       "$(\\frac{n}{3}, \\frac{n}{3}, \\frac{n}{3})\\quad$" ),
                       values = c("indianred", "steelblue", "olivedrab")) +
  theme(legend.position = "top",
        legend.key.size = unit(1, "cm"))

print(p)
dev.off()

# The latest tables
BF_dist = read.table("bayesDataBubblesRemoved.txt", header = TRUE)
BF_distTransformed =  BF_dist -
  min(na.omit(as.vector(t(as.matrix(BF_distTransformed))))) + 1000
