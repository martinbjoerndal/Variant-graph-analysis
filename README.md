# Variant-graph-analysis
Measuring pairwise distances between population graphs built from variant data from the 1000 genomes project. Variants can be coupled with genotype information from individuals using the VCF file format. In this project, VCF data is used to calculate the distance between six different human populations, all downloaded from http://www.internationalgenome.org/data-portal/population.

Three distance metrics are employed. The R-program calculateDist.r computes bubble counter distance and Bayes factor distance. two distance measures for input written by readData.r

GedevoGraph.java creates two objects of class Graph based on VCF input. It outputs a list of edges which may be used by   GEDEVO[1][2] to calculate graph edit distance, or the R-package igraph to draw the graph.

[1] Rashid Ibragimov, Maximilian Malek, Jiong Guo, Jan Baumbach: GEDEVO: An Evolutionary Graph Edit Distance Algorithm for Biological Network Alignment. GCB 2013:68-79

[2] Rashid Ibragimov, Jan Martens, Jiong Guo, and Jan Baumbach. 2013. NABEECO: biological network alignment with bee colony optimization algorithm. In Proceedings of the 15th annual conference companion on Genetic and evolutionary computation (GECCO '13 Companion), Christian Blum (Ed.). ACM, New York, NY, USA, 43-44. DOI=10.1145/2464576.2464600 http://doi.acm.org/10.1145/2464576.2464600
