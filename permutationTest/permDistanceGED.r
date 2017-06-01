##############################################################################
# permDistanceGED.r
#
# Analyses for master thesis
# ======================================
#
# Does permutation tests on the distances metrics from distances.r. Pools
# two populations and does permutations. GEDEVO runs in parallel, making it
# hard to have the outer loop in parallell....
###############################################################################

source("readData.r")
source("popCleaningForGraph.r")

startTime = proc.time()

# Function to perform permutations. Takes two populations, pools them, and 
# draws permuted populations.
pairwisePermutations <- function(pop1, pop2) {
  
  pooled = names(c(pop1, pop2))
  permutation = sample(pooled, length(pooled), replace = FALSE)

  permPop1 = subset(data, select = permutation[1:20])
  permPop2 = subset(data, select = permutation[21:40])

  return(list(permPop1, permPop2))
}

callToGedevo <- function() {
  cat("\nNow starting GEDEVO...\n")
  system("bash gedevoRun.sh", wait = TRUE)
  
  # Then read the results file and pass back to caller
  con = file("distance.txt")
  open(con)

  # Read only line number 13
  line = scan(con, what = character(), skip = 12, nlines = 1)
  close(con)
  return(as.numeric(line[4]))
}

K = 25
popNames = c("GBR", "FIN", "LWK", "YRI", "CHB", "CHS")
pops = list(brit, fin, luhya, yoruba, hanBei, hanSouth)
gedTable = matrix(NA, nrow = K, ncol = 15)

# This can not so easily go parallel because we need BuildGraph to write files
# in each iteration and the different threads may pick wrong files if they are
# not completely in sync.
for (k in 1:K) {
  gedList = NULL
  timeList = NULL
  for (i in 1:length(pops)) {
    for (j in 1:length(pops)) {
      if(i > j) {

        # Call function that makes permutation. Calculate distance as always.
        permPops = pairwisePermutations(pops[[i]], pops[[j]])	
        
        # Clean the permuted populations and write to file. The files are
        # used by BuildGraph
        cleaned = clean(permPops[[1]], permPops[[2]])
        write.table(cleaned[[1]], file = "pop1Perm.txt")
        write.table(cleaned[[2]], file =  "pop2Perm.txt")

        system("java GedevoGraph pop1Perm.txt pop2Perm.txt")
        ged = callToGedevo()        
        gedList = c(gedList, ged)

        # Do some cleaning.
        #
        # IMPORTANT: Remove connectivity matrices to prevent GEDEVO from using
        # old ones.
        #
        # Also remove the input-file for GEDEVO so that it crashes instead of
        # potentially using the previous file in case GraphBuilder fails.
        system("rm pop1Perm.txt")
        system("rm pop2Perm.txt")
      }
    }
  }

  gedTable[k,] = gedList    
  time = proc.time()[3] - startTime[3]
  cat("\nIteration", k, "of", K, "at time:", (time / 60), "min.\n")
}

# Write to file
write.table(gedTable, file = "GEDresults/gedSubScored_1.txt")
