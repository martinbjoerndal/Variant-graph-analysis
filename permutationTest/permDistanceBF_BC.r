##############################################################################
# permDistanceBF_BC.r
#
# Analyses for master thesis
# ======================================
#
# Does permutation tests on the distances metrics from distances.r. Pools
# two populations and does permutations. Runs in parallel.
###############################################################################

require(parallel)
require(doParallel)
require(foreach)

# Find distances
source("readData.r")
source("calculateDist.r")
source("popCleaningForGraph.r")

# Function to perform permutations. Takes two populations, pools them, and 
# draws permuted populations.
pairwisePermutations <- function(pop1, pop2) {
  
  pooled = names(c(pop1, pop2))
  permutation = sample(pooled, length(pooled), replace = FALSE)
  
  permPop1 = subset(data, select = permutation[1:20])
  permPop2 = subset(data, select = permutation[21:40])
  return(list(permPop1, permPop2))
}

# Parallellize
cores = detectCores() - 1
cluster = makeCluster(cores)
registerDoParallel(cores)

K = 3000
popNames = c("GBR", "FIN", "LWK", "YRI", "CHB", "CHS")
pops = list(brit, fin, luhya, yoruba, hanBei, hanSouth)

startTime = proc.time()
# Run loop in parallel: Requires some special syntax
distanceTable = foreach(k = 1:K, .combine = rbind) %dopar% {

  startTime = proc.time()
  distanceList = NULL
  for (i in 1:length(pops)) {
    for (j in 1:length(pops)) {
      if(i > j) {
        
        # Call function that makes permutation. Calculate distance as always.
        permPops = pairwisePermutations(pops[[i]], pops[[j]])
        distances = calculateDist(permPops[[1]], permPops[[2]])
        distanceList = c(distanceList, distances)
      }
    }
  }

  time = proc.time()[3] - startTime[3]
  cat("\nIteration", k, "of", K, "at time:", (time / 60), "min.\n")  
  distanceList
}

stopCluster(cluster)

# Write to file
write.table(distanceTable, file = "./5000Lines/distanceTable.txt")
