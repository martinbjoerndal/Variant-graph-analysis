##############################################################################
# ASL.r
#
# Analyses for master thesis
# ======================================
#
# Compute achieved significance level (ASL). Can be used to compare the
# across different permutation sample sizes.
###############################################################################

require(xtable)

d = read.table("./5000Lines/distanceTableBubblesExcluded.txt", header = TRUE)
d = d[1:300,]
dGED = read.table("GEDresults/gedPermutations.txt", header = TRUE)

colnames = c("GBR-FIN","GBR-FIN", "GBR-LWK", "GBR-LWK", "FIN-LWK",  "FIN-LWK",
  "GBR-YRI", "GBR-YRI", "FIN-YRI", "FIN-YRI","LWK-YRI", "LWK-YRI", "GBR-CHB",
  "GBR-CHB", "FIN-CHB", "FIN-CHB", "LWK-CHB", "LWK-CHB", "YRI-CHB",  "YRI-CHB",
  "GBR-CHN", "GBR-CHN","FIN-CHN","FIN-CHN", "LWK-CHN","LWK-CHN", "YRI-CHN",
  "YRI-CHN","CHB-CHN", "CHB-CHN")

# Split table into two: One for each metric
names(d) = colnames
dNaive = matrix(NA, ncol = length(colnames)/2, nrow = length(d[,1]))
dBayes = matrix(NA, ncol = length(colnames)/2, nrow = length(d[,1]))

observedNaive = read.table("naiveData.txt", header = TRUE)[-2:-1,-1]
observedBayes = read.table("bayesData.txt", header = TRUE)[-2:-1,-1]
observedGED = read.table("./GEDresults/gedData.txt", header = TRUE)[-2:-1, -1]

# Convert to vector to have corresponding indexes as the ones from the distance
# table. Convert and remove NA's
observedNaive = na.omit(as.vector(t(as.matrix(observedNaive))))
observedBayes = na.omit(as.vector(t(as.matrix(observedBayes))))
observedGED = na.omit(as.vector(t(as.matrix(observedGED))))

j = 1
k = 1
for (i in 1:length(colnames)) {
  if ((i %% 2) == 1) {
    dNaive[,j] = d[,i]
    j = j + 1
  } else {
    dBayes[,k] = d[,i]
    k = k + 1
  }  
}

calculateASL <- function(i, observed) {
  naiveExtremes = sum(dNaive[,i] > observed[1])
  bayesExtremes =  sum(dBayes[,i] < observed[2])
  gedExtremes = sum(dGED[,i] > observed[3])

  naiveASL = (naiveExtremes + 1) / (length(d[,i] + 1))
  bayesASL = (bayesExtremes + 1) / (length(d[,i] + 1))
  gedASL = (gedExtremes + 1) / (length(dGED[,i]) + 1)
  print(gedExtremes)
  return(list(naiveASL, bayesASL, gedASL))
}

# Calculate the ASL
ASL = matrix(ncol = 3, nrow = length(colnames)/2)
for (i in 1:length(dNaive[1,])) {
  observed = c(observedNaive[i], observedBayes[i], observedGED[i])
  ASL[i,] = unlist(calculateASL(i, observed))
}


# Fill into matrix for report
popNames = c("GBR", "FIN", "LWK", "YRI", "CHB", "CHS")
naiveASL_table = matrix(NA, ncol = length(popNames), nrow = length(popNames))
bayesASL_table = matrix(NA, ncol = length(popNames), nrow = length(popNames))
gedASL_table = matrix(NA, ncol = length(popNames), nrow = length(popNames))

k = 1 
for (i in 1:length(popNames)) {
  for (j in 1:length(popNames)) {
    if (i > j) {
      naiveASL_table[i, j] = ASL[k, 1]
      bayesASL_table[i, j] = ASL[k, 2]
      gedASL_table[i, j] = ASL[k, 3]
      k = k + 1
    }
  }
}

# Clean
data.frame(naiveASL_table)
data.frame(bayesASL_table)
data.frame(gedASL_table)

colnames(naiveASL_table) = popNames
colnames(bayesASL_table) = popNames
colnames(gedASL_table) = popNames
rownames(naiveASL_table) = popNames
rownames(bayesASL_table) = popNames
rownames(gedASL_table) = popNames

# And produce TeX output
naiveASL_tableTeX = xtable(naiveASL_table, digits = 4,
  caption = "ASL for $d_{BC}$", label = "tbl:naiveASL")
bayesASL_tableTeX = xtable(bayesASL_table, digits = 4,
  caption = "ASL for $d_{BF}$", label = "tbl:bayesASL")
gedASL_tableTeX = xtable(gedASL_table, digits = 4,
  caption = "ASL for $d_{GED}$", label = "tbl:gedASL")
