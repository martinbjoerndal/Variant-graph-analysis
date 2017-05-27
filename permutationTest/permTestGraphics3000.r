##############################################################################
# permTestGraphics3000.r
#
# Analyses for master thesis 
# ======================================
#
# Makes graphics for use in the thesis out of the data from the permutation
# test
###############################################################################

require(ggplot2)
require(gridExtra)
require(tikzDevice)
require(cowplot)

d = read.table("./5000Lines/distanceTable.txt", header = TRUE)
obsBC = read.table("distance_BC.txt")$x
obsBF = read.table("distance_BF.txt")$x

q = quantile(d[,1], c(.025, 0.975))
dens = density(d[,1])
dd = with(dens, data.frame(d[,1], x = seq(0, 1, length.out = length(d[,1]))))

pdf("naivePermDist.pdf", width = 8, height = 11.5)
par(mar = c(5, 3, 2, 2) + 0.1)

AXIS_SIZE = 12

p1 = ggplot(d, aes(x = d[,1])) +
  geom_histogram(binwidth = .0035, colour = "black", fill = "white") +
  geom_density(aes(y = .0035 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  geom_vline(xintercept = obsBC[1], col = "steelblue") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = pretty(d[,1], n = 4)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) + 
  ggtitle("GBR-FIN") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,3], c(.025, 0.975))
p3 = ggplot(d, aes(x = d[,3])) +
  geom_histogram(binwidth = .0035, colour = "black", fill = "white") +
  geom_density(aes(y = .0035 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(d[,3]), max(d[,3]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-LWK") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,5], c(.025, 0.975))
p5 = ggplot(d, aes(x = d[,5])) +
  geom_histogram(binwidth = .0035, colour = "black", fill = "white") +
  geom_density(aes(y = .0035 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,5]), max(d[,5]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("FIN-LWK") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,7], c(.025, 0.975))
p7 = ggplot(d, aes(x = d[,7])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,7]), max(d[,7]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("GBR-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,9], c(.025, 0.975))
p9 = ggplot(d, aes(x = d[,9])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,9]), max(d[,9]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("FIN-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,11], c(.025, 0.975))
p11 = ggplot(d, aes(x = d[,11])) +
  geom_histogram(binwidth = .0035, colour = "black", fill = "white") +
  geom_density(aes(y = .0035 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,11]), max(d[,11]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("LWK-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,13], c(.025, 0.975))
p13 = ggplot(d, aes(x = d[,13])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,13]), max(d[,13]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +  
  ggtitle("GBR-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,15], c(.025, 0.975))
p15 = ggplot(d, aes(x = d[,15])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,15]), max(d[,15]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("FIN-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,17], c(.025, 0.975))
p17 = ggplot(d, aes(x = d[,17])) +
  geom_histogram(binwidth = .0045, colour = "black", fill = "white") +
  geom_density(aes(y = .0045 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,17]), max(d[,17]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) + 
  ggtitle("LWK-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,19], c(.025, 0.975))
p19 = ggplot(d, aes(x = d[,19])) +
  geom_histogram(binwidth = .0045, colour = "black", fill = "white") +
  geom_density(aes(y = .0045 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,19]), max(d[,19]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("YRI-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,21], c(.025, 0.975))
p21 = ggplot(d, aes(x = d[,21])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,21]), max(d[,21]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("GBR-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,23], c(.025, 0.975))
p23 = ggplot(d, aes(x = d[,23])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,23]), max(d[,23]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("FIN-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,25], c(.025, 0.975))
p25 = ggplot(d, aes(x = d[,25])) +
  geom_histogram(binwidth = .0045, colour = "black", fill = "white") +
  geom_density(aes(y = .0045 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,25]), max(d[,25]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("LWK-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,27], c(.025, 0.975))
p27 = ggplot(d, aes(x = d[,27])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,27]), max(d[,27]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("YRI-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,29], c(.025, 0.975))
p29 = ggplot(d, aes(x = d[,29])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  geom_vline(xintercept = obsBC[15], col = "steelblue") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,29]), max(d[,29]), length.out = 4), 2)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("CHB-CHS") +
  theme(plot.title = element_text(face = "plain"))

pNaive = grid.arrange(p1, p3, p5, p7, p9, p11, p13, p15, p17, p19, p21, p23,
  p25, p27, p29, ncol = 3)
print(pNaive)
dev.off()

q = quantile(d[,2], c(.025, 0.975))
dens = density(d[,3])
dd = with(dens, data.frame(d[,2], x = seq(0, 1, length.out = length(d[,2]))))

pdf("bayesPermDist.pdf", width = 8, height = 11.5)
par(mar = c(5, 3, 2, 2) + 0.1)

AXIS_SIZE = 9
p2 = ggplot(d, aes(x = d[,2])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  geom_vline(xintercept = obsBF[1], col = "steelblue") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,2]), (max(d[,2]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  ggtitle("GBR-FIN") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,4], c(.025, 0.975))
p4 = ggplot(d, aes(x = d[,4])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,4]), (max(d[,4]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("GBR-LWK") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,6], c(.025, 0.975))
p6 = ggplot(d, aes(x = d[,6])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,6]), (max(d[,6]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("FIN-LWK") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,8], c(.025, 0.975))
p8 = ggplot(d, aes(x = d[,8])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,8]), (max(d[,8]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("GBR-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,10], c(.025, 0.975))
p10 = ggplot(d, aes(x = d[,10])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,10]), (max(d[,10]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("FIN-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,12], c(.025, 0.975))
p12 = ggplot(d, aes(x = d[,12])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,12]), (max(d[,12]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("LWK-YRI") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,14], c(.025, 0.975))
p14 = ggplot(d, aes(x = d[,14])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,14]), (max(d[,14]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("GBR-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,16], c(.025, 0.975))
p16 = ggplot(d, aes(x = d[,16])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,16]), (max(d[,16]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("FIN-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,18], c(.025, 0.975))
p18 = ggplot(d, aes(x = d[,18])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,18]), (max(d[,18]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("LWK-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,20], c(.025, 0.975))
p20 = ggplot(d, aes(x = d[,20])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,20]), (max(d[,20]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("YRI-CHB") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,22], c(.025, 0.975))
p22 = ggplot(d, aes(x = d[,22])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,22]), (max(d[,22]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("GBR-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,24], c(.025, 0.975))
p24 = ggplot(d, aes(x = d[,24])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,24]), (max(d[,24]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("FIN-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,26], c(.025, 0.975))
p26 = ggplot(d, aes(x = d[,26])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,26]), (max(d[,26]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("LWK-CHS") +
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,28], c(.025, 0.975))
p28 = ggplot(d, aes(x = d[,28])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,28]), (max(d[,28]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("YRI-CHS")+
  theme(plot.title = element_text(face = "plain"))

q = quantile(d[,30], c(.025, 0.975))
p30 = ggplot(d, aes(x = d[,30])) +
  geom_histogram(binwidth = 40, colour = "black", fill = "white") +
  geom_density(aes(y = 40 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  geom_vline(xintercept = obsBF[15], col = "steelblue") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,30]), (max(d[,30]) - 100), length.out = 4))) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) +
  ggtitle("CHB-CHS") +
  theme(plot.title = element_text(face = "plain"))

pBayes = grid.arrange(p2, p4, p6, p8, p10, p12, p14, p16, p18, p20, p22, p24, p26,
  p28, p30, ncol = 3)
print(pBayes)
dev.off()
