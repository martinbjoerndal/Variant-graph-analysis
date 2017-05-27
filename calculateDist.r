##############################################################################
# calculateDist.r
#
# Analyses for master thesis 
# ======================================
#
# An extension of naiveDist to include the proportions of the genotypes.
# We still wish to find the distance between two population. 
###############################################################################

calculateDist = function(pop1, pop2) {
  internalStartTime = proc.time()[3]
  samples = cbind(pop1, pop2)

  # Loop through variants. What are the genotypes in the sample? Do they
  # contain  The variant? First, investigate if the sample contain the variant 
  # at all, i.e it is homozyguos for the variant or heterozygous.
  containedPop1 = logical(length(samples[,1]))
  containedPop2 = logical(length(samples[,1]))

  genotypeCountPop1 = matrix(ncol = 3, nrow = length(pop1[,1]))
  genotypeCountPop2 = matrix(ncol = 3, nrow = length(pop1[,1]))

  for (i in 1:length(pop1[,1])) {
    # Genotype proportions. What are the possible genotypes? Lookup in
    # the VCF-file at positions REF and ALT
    ref = chr22Reduced$REF[i]
    alt = chr22Reduced$REF[i]
    zygosityPop1 = numeric(3)
    zygosityPop2 = numeric(3)

    for (j in 1:length(pop1[1,])) {
      # Find  genotypes in the first population
      genotypePop1 = integer(2)
      genotypePop1[1] = strtoi(substr(pop1[i,j], 1, 1))
      genotypePop1[2] = strtoi(substr(pop1[i,j], 3, 3))    

      # Find genotypes in second population
      genotypePop2 = integer(2)
      genotypePop2[1] = strtoi(substr(pop2[i,j], 1, 1))
      genotypePop2[2] = strtoi(substr(pop2[i,j], 3, 3))

      # Is the variant contained in the populations?
      if(genotypePop1[1] != 0 | genotypePop1[2] != 0) {
        containedPop1[i] = TRUE
      }

      if (genotypePop2[1] != 0 | genotypePop2[2] != 0) {
        containedPop2[i] = TRUE
      }

      # What are the genotypes in the populations? The vector "zygosity" 
      # contains the number of individuals that are 1) homozygotes for the 
      # reference, 2) heterozygotes or 3) homozygotes for the alternative
      if (genotypePop1[1] == 0 && genotypePop1[2] == 0) {
        zygosityPop1[1] = zygosityPop1[1] + 1      
      } else if (genotypePop1[1] == 1 && genotypePop1[2] == 1) {
        zygosityPop1[3] = zygosityPop1[3] + 1
      } else {
        zygosityPop1[2] = zygosityPop1[2] + 1
      }

      if (genotypePop2[1] == 0 && genotypePop2[2] == 0) {
        zygosityPop2[1] = zygosityPop2[1] + 1
      } else if (genotypePop2[1] == 1 && genotypePop2[2] == 1) {
        zygosityPop2[3] = zygosityPop2[3] + 1
      } else {
        zygosityPop2[2] = zygosityPop2[2] + 1
      }     
    }

    genotypeCountPop1[i,] = zygosityPop1
    genotypeCountPop2[i,] = zygosityPop2
  }

  proportionsPop1 = genotypeCountPop1 / length(pop1)
  proportionsPop2 = genotypeCountPop2 / length(pop2)

  # If the bubble is contained in both populations, the vector has value "1" 
  # at this index. If it is contained in one of the populations, it has value
  # "0". If it is not contained in any, we will not use it anymore in this 
  # preliminary analysis, so it is denoted by a "-1".
  bubbles = integer(length(containedPop1))
  for (i in (1:length(containedPop1))) {
    if (containedPop1[i] && containedPop2[i]) {
      bubbles[i] = 1
    } else if (!containedPop1[i] && !containedPop2[i]) {
      bubbles[i] = -1
    } else {
      bubbles[i] = 0
    }
  }

  # Naive distance
  # ===========================================================================
  # The distance from graph G1 to graph G2 is estimated by number of bubbles
  # contained in only G1 or G2 divided by the total number of bubbles found in 
  # either.
  separate = sum(bubbles == 0)
  shared = sum(bubbles != 0)
  dist = separate / (shared + separate)

  # Comparison of the genotype proportions. We shuld do a hypothesis test to 
  # see if the proportions are similar in two populations

  # Bayesian approach
  # ===========================================================================
  # Use a Dirichlet prior on the parameters. Under H0, the dirichlet prior has 
  # parameters that are the genotype proportions of the second observations. 
  # Under Ha the parameters gives a rather flat prior. First, we must assign
  # some hyperparameters. 
  alpha = rep(1, 3)
  index = numeric(length(genotypeCountPop2[,1]))
  d = numeric(length(genotypeCountPop2[,1]))

  for (i in 1:length(pop1[,1])) {

    
    lambda = genotypeCountPop2[i,] + genotypeCountPop1[i,] + alpha
    gammaOne = genotypeCountPop1[i,] + alpha
    gammaTwo = genotypeCountPop2[i,] + alpha

    prodAlpha = prod(gamma(alpha))
    sumAlpha = gamma(sum(alpha))

    prodLambda = prod(gamma(lambda))
    sumLambda = gamma(sum(lambda))

    prodGammaOne = prod(gamma(gammaOne))
    sumGammaOne = gamma(sum(gammaOne))

    prodGammaTwo = prod(gamma(gammaTwo))
    sumGammaTwo = gamma(sum(gammaTwo))

    d[i] = ((prodAlpha / sumAlpha) * (prodLambda / sumLambda)) / 
    ((prodGammaOne / sumGammaOne) * (prodGammaTwo / sumGammaTwo))
    index[i] = data[i,2]

    #cat("\nd[i]:", d[i], "genotypeCountPop1[i,]:", genotypeCountPop1[i,],
    #    "genotypeCountPop2[i,]:", genotypeCountPop2[i,])
  }
  
  # Finally, report distance
  #cat("\ndistance between permuted populations")
  #cat("\nNaive dist =", dist, "\tBayes dist =", sum(log(1/d)))

  #internalEndTime = proc.time()[3]
  #cat("\nInternal time:", (internalEndTime - internalStartTime), "sec")
  # Return the Bayes factor of the null hypothesis relative to the alternative
  # hypothesis
  return(c(dist, sum(log(1/d))))
}
