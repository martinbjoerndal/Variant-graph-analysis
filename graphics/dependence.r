##############################################################################
# dependence.r
#
# Analyses for master thesis 
# ======================================
#
# Is the independence assumption in calculating the likelihood valid? Plot of
# the distance per position of the bubble.
###############################################################################

source("readData.r")
source("bayesFactors.r")

require(ggplot2)
require(gridExtra)
require(forecast)
require(cowplot)

# Obtain distances between the individual bubbles and the position of each.
pops = list(brit, fin, luhya, yoruba, hanBei, hanSouth)
d = matrix(ncol = 30, nrow = 5000)

k = 1
for (i in 1:length(pops)) {
  for (j in 1:length(pops)) {
    if(i > j && k < 30) {
      temp = bayesFactors(pops[[i]], pops[[j]])
      d[,k] = temp[[1]]
      d[,k + 1] = temp[[2]]
      k = k + 2
    }
  }
}

# Remove the bubbles that has a (20, 0, 0)-distribution in both populations.
for (i in 1:15) {
  for (j in 1:length(d[,1])) {    
    if (d[j, 2*i - 1] > 60) {
      d[j, 2*i - 1] = NA
    }
  }
}

d = data.frame(d)

# Produce graphics
pdf("distancePerBubble.pdf", width = 8, height = 11.5)
par(mar = c(5, 3, 2, 2) + 0.1)

AXIS_SIZE = 11
SHIFT = 150000
p1 = ggplot(subset(d[,1:2], !is.na(X2)), aes(x = X2, y = X1)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,2]), (max(d[,2] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) + 
  ggtitle("GBR-FIN") +
  theme(plot.title = element_text(face = "plain"))

p2 = ggplot(subset(d[,3:4], !is.na(X3)), aes(x = X4, y = X3)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,4]), (max(d[,4] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("GBR-LWK")

p3 = ggplot(subset(d[,5:6], !is.na(X5)), aes(x = X6, y = X5)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,6]), (max(d[,6] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("FIN-LWK")

p4 = ggplot(subset(d[,7:8], !is.na(X7)), aes(x = X8, y = X7)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,8]), (max(d[,8] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("GBR-YRI")

p5 = ggplot(subset(d[,9:10], !is.na(X9)), aes(x = X10, y = X9)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,10]), (max(d[,10] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("FIN-YRI")

p6 = ggplot(subset(d[,11:12], !is.na(X11)), aes(x = X12, y = X11)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,12]), (max(d[,12] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("LWK-YRI")

p7 = ggplot(subset(d[,13:14], !is.na(X13)), aes(x = X14, y = X13)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,14]), (max(d[,14] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) + 
  ggtitle("GBR-CHB")

p8 = ggplot(subset(d[,15:16], !is.na(X15)), aes(x = X16, y = X15)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,16]), (max(d[,16] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("FIN-CHB")

p9 = ggplot(subset(d[,17:18], !is.na(X17)), aes(x = X18, y = X17)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("")  +
  scale_x_continuous(breaks = round(seq(min(d[,18]), (max(d[,18] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("LWK-CHB")

p10 = ggplot(subset(d[,19:20], !is.na(X19)), aes(x = X20, y = X19)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,20]), (max(d[,20] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("YRI-CHB")

p11 = ggplot(subset(d[,21:22], !is.na(X21)), aes(x = X22, y = X21)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,22]), (max(d[,22] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("GBR-CHS")

p12 = ggplot(subset(d[,23:24], !is.na(X23)), aes(x = X24, y = X23)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,24]), (max(d[,24] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("FIN-CHS")

p13 = ggplot(subset(d[,25:26], !is.na(X25)), aes(x = X26, y = X25)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,26]), (max(d[,26] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("LWK-CHS")

p14 = ggplot(subset(d[,27:28], !is.na(X27)), aes(x = X28, y = X27)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,28]), (max(d[,28] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("YRI-CHS")

p15 = ggplot(subset(d[,29:30], !is.na(X29)), aes(x = X30, y = X29)) +
  geom_point() + geom_smooth(se = FALSE) +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = round(seq(min(d[,30]), (max(d[,30] - SHIFT)), length.out = 3))) +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  ggtitle("CHB-CHS")

pDependence = grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12,
  p13, p14, p15, ncol = 3)
print(pDependence)
dev.off()

# Autocorrelation things
pdf("acf.pdf", height = 11.5, width = 8)
AXIS_SIZE = 12

alpha = .95
acf1 = acf(subset(d[,1:2], !is.na(X1))[,1], lag.max = 100, plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf1$n.used)

acf1_df = with(acf1[2:length(acf1$acf)], data.frame(lag, acf))
p1 = ggplot(data = acf1_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("GBR-FIN")

acf2 = acf(subset(d[,3:4], !is.na(X3))[,1], lag.max = 100, plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf2$n.used)

acf2_df = with(acf2[2:length(acf1$acf)], data.frame(lag, acf))
p2 = ggplot(data = acf2_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("GBR-LWK")

acf3 = acf(subset(d[,5:6], !is.na(X5))[,1], lag.max = 100, plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf3$n.used)

acf3_df = with(acf3[2:length(acf1$acf)], data.frame(lag, acf))
p3 = ggplot(data = acf3_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("FIN-LWK")

acf4 = acf(subset(d[,7:8], !is.na(X7))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf4$n.used)

acf4_df = with(acf4[2:length(acf1$acf)], data.frame(lag, acf))
p4 = ggplot(data = acf4_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("GBR-YRI")

acf5 = acf(subset(d[,9:10], !is.na(X9))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf5$n.used)

acf5_df = with(acf5[2:length(acf1$acf)], data.frame(lag, acf))
p5 = ggplot(data = acf5_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("FIN-YRI")

acf6 = acf(subset(d[,11:12], !is.na(X11))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf6$n.used)

acf6_df = with(acf6[2:length(acf1$acf)], data.frame(lag, acf))
p6 = ggplot(data = acf6_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("LWK-YRI")

acf7 = acf(subset(d[,13:14], !is.na(X13))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf7$n.used)

acf7_df = with(acf7[2:length(acf1$acf)], data.frame(lag, acf))
p7 = ggplot(data = acf7_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("GBR-CHB")

acf8 = acf(subset(d[,15:16], !is.na(X15))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf8$n.used)

acf8_df = with(acf8[2:length(acf1$acf)], data.frame(lag, acf))
p8 = ggplot(data = acf8_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("FIN-CHB")

acf9 = acf(subset(d[,17:18], !is.na(X17))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf9$n.used)

acf9_df = with(acf9[2:length(acf1$acf)], data.frame(lag, acf))
p9 = ggplot(data = acf9_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("LWK-CHB")

acf10 = acf(subset(d[,19:20], !is.na(X19))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf10$n.used)

acf10_df = with(acf10[2:length(acf1$acf)], data.frame(lag, acf))
p10 = ggplot(data = acf10_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("YRI-CHB")

acf11 = acf(subset(d[,21:22], !is.na(X21))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf11$n.used)

acf11_df = with(acf11[2:length(acf1$acf)], data.frame(lag, acf))
p11 = ggplot(data = acf11_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("GBR-CHS")

acf12 = acf(subset(d[,23:24], !is.na(X23))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf12$n.used)

acf12_df = with(acf12[2:length(acf1$acf)], data.frame(lag, acf))
p12 = ggplot(data = acf12_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("FIN-CHS")

acf13 = acf(subset(d[,25:26], !is.na(X25))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf13$n.used)

acf13_df = with(acf13[2:length(acf1$acf)], data.frame(lag, acf))
p13 = ggplot(data = acf13_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +  
  ggtitle("LWK-CHS")

acf14 = acf(subset(d[,27:28], !is.na(X27))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf14$n.used)

acf14_df = with(acf14[2:length(acf1$acf)], data.frame(lag, acf))
p14 = ggplot(data = acf14_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("YRI-CHS")

acf15 = acf(subset(d[,29:30], !is.na(X29))[,1], plot = FALSE)
confLim = c(-1,1)*qnorm((1 + alpha)/2)/sqrt(acf15$n.used)

acf15_df = with(acf15[2:length(acf1$acf)], data.frame(lag, acf))
p15 = ggplot(data = acf15_df, mapping = aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = confLim, col = "blue", lty = "dashed") +
  theme(axis.text.y = element_text(size = AXIS_SIZE)) + 
  theme(axis.text.x = element_text(size = AXIS_SIZE)) +
  theme(plot.title = element_text(face = "plain")) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("CHB-CHS")

pACF = grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12,
  p13, p14, p15, ncol = 3)
print(pACF)
dev.off()
