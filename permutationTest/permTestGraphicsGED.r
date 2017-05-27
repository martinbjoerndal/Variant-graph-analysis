##############################################################################
# permTestGraphics.r
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

d = read.table("./GEDresults/gedPermutations.txt", header = TRUE)
obs = read.table("./GEDresults/gedData.txt")$x
q = quantile(d[,1], c(.025, 0.975))
dens = density(d[,1])
dd = with(dens, data.frame(d[,1], x = seq(0, 1, length.out = length(d[,1]))))

pdf("GEDPermDist.pdf", width = 8, height = 11.5)
par(mar = c(5, 3, 2, 2) + 0.1)

AXIS_SIZE = 11
p1 = ggplot(d, aes(x = d[,1])) +
  geom_histogram(binwidth = .005, colour = "black", fill = "white") +
  geom_density(aes(y = .005 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,1]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[1], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,1]), max(d[,1]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-FIN") +
  theme(plot.title = element_text(face = "plain"))


dd = with(dens, data.frame(d[,2], x = seq(0, 1, length.out = length(d[,2]))))
q = quantile(d[,2], c(.025, 0.975))
p2 = ggplot(d, aes(x = d[,2])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,2]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[2], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,2]), max(d[,2]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-LWK") +
  theme(plot.title = element_text(face = "plain"))

dd = with(dens, data.frame(d[,3], x = seq(0, 1, length.out = length(d[,3]))))
q = quantile(d[,3], c(.025, 0.975))
p3 = ggplot(d, aes(x = d[,3])) +
  geom_histogram(binwidth = .005, colour = "black", fill = "white") +
  geom_density(aes(y = .005 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,3]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[3], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,3]), max(d[,3]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("FIN-LWK") +
  theme(plot.title = element_text(face = "plain"))

dd = with(dens, data.frame(d[,3], x = seq(0, 1, length.out = length(d[,4]))))
q = quantile(d[,4], c(.025, 0.975))
p4 = ggplot(d, aes(x = d[,4])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,4]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[4], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,4]), max(d[,4]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-YRI") +
  theme(plot.title = element_text(face = "plain"))

dd = with(dens, data.frame(d[,3], x = seq(0, 1, length.out = length(d[,5]))))
q = quantile(d[,5], c(.025, 0.975))
p5 = ggplot(d, aes(x = d[,5])) +
  geom_histogram(binwidth = .0045, colour = "black", fill = "white") +
  geom_density(aes(y = .0045 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,5]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[5], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,5]), max(d[,5]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("FIN-YRI") +
  theme(plot.title = element_text(face = "plain"))

dd = with(dens, data.frame(d[,3], x = seq(0, 1, length.out = length(d[,6]))))
q = quantile(d[,6], c(.025, 0.975))
p6 = ggplot(d, aes(x = d[,6])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,6]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[6], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,6]), max(d[,6]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("LWK-YRI") +
  theme(plot.title = element_text(face = "plain"))

dd = with(dens, data.frame(d[,3], x = seq(0, 1, length.out = length(d[,7]))))
q = quantile(d[,7], c(.025, 0.975))
p7 = ggplot(d, aes(x = d[,7])) +
  geom_histogram(binwidth = .0025, colour = "black", fill = "white") +
  geom_density(aes(y = .0025 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,7]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[7], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,7]), max(d[,7]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-CHB") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,8], c(.025, 0.975))
p8 = ggplot(d, aes(x = d[,8])) +
  geom_histogram(binwidth = .006, colour = "black", fill = "white") +
  geom_density(aes(y = .006 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,8]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[8], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,8]), max(d[,8]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("FIN-CHB") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,9], c(.025, 0.975))
p9 = ggplot(d, aes(x = d[,9])) +
  geom_histogram(binwidth = .005, colour = "black", fill = "white") +
  geom_density(aes(y = .005 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,9]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[9], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,9]), max(d[,9]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("LWK-CHB") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,10], c(.025, 0.975))
p10 = ggplot(d, aes(x = d[,10])) +
  geom_histogram(binwidth = .005, colour = "black", fill = "white") +
  geom_density(aes(y = .005 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,10]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[10], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,10]), max(d[,10]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("YRI-CHB") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,11], c(.025, 0.975))
p11 = ggplot(d, aes(x = d[,11])) +
  geom_histogram(binwidth = .0025, colour = "black", fill = "white") +
  geom_density(aes(y = .0025 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,11]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[11], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,11]), max(d[,11]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("GBR-CHS") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,12], c(.025, 0.975))
p12 = ggplot(d, aes(x = d[,12])) +
  geom_histogram(binwidth = .003, colour = "black", fill = "white") +
  geom_density(aes(y = .003 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,12]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[12], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,12]), max(d[,12]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("FIN-CHS") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,13], c(.025, 0.975))
p13 = ggplot(d, aes(x = d[,13])) +
  geom_histogram(binwidth = .004, colour = "black", fill = "white") +
  geom_density(aes(y = .004 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,13]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[13], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,13]), max(d[,13]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("LWK-CHS") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,14], c(.025, 0.975))
p14 = ggplot(d, aes(x = d[,14])) +
  geom_histogram(binwidth = .0035, colour = "black", fill = "white") +
  geom_density(aes(y = .0035 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,14]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  #geom_vline(xintercept = obs[14], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,14]), max(d[,14]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("YRI-CHS") +
  theme(plot.title = element_text(face = "plain"))
  
q = quantile(d[,15], c(.025, 0.975))
p15 = ggplot(d, aes(x = d[,15])) +
  geom_histogram(binwidth = .005, colour = "black", fill = "white") +
  geom_density(aes(y = .005 *..count..), alpha = .2, fill = "#FF6666") +
  geom_ribbon(data = subset(dd, x > q[2] & x < q[1]), aes(ymax = d[,15]), ymin = 0,
              fill = "red", colour = NA) +
  geom_vline(xintercept = q, col = "indianred", linetype = "dashed") +
  geom_vline(xintercept = obs[15], col = "steelblue") +
  scale_x_continuous(breaks = round(seq(min(d[,15]), max(d[,15]), length.out = 4), 3)) +
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  xlab("") + ylab("") +
  ggtitle("CHB-CHS") +
  theme(plot.title = element_text(face = "plain"))
  
pNaive = grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12,
  p13, p14, p15, ncol = 3)
print(pNaive)
dev.off()
