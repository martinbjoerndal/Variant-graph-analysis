##############################################################################
# readData.r
#
# Analyses for master thesis
# ======================================
#
# Reads the VCF data
###############################################################################

chr22Connection = file("reduced.vcf")
open(chr22Connection)
data = read.table(chr22Connection, nrows = 5000, header = TRUE)
close(chr22Connection)

# Drop columns: Keep only ten individuals (should be chosen at random I 
# suppose). First do brit-fin
chr22Reduced = data[,1:9]

# Make the populations. Currently 20 individuals in each one.
brit = subset(data, select = c(HG00096, HG00097, HG00099, HG00100, HG00101,
                      HG00102, HG00103, HG00104, HG00106, HG00108, HG00109,
                      HG00110, HG00111, HG00112, HG00113, HG00114, HG00116,
                      HG00117, HG00118, HG00119))

fin = subset(data, select = c(HG00171, HG00173, HG00174, HG00176, HG00177,
                     HG00178, HG00179, HG00180, HG00182, HG00183, HG00185,
                     HG00186, HG00187, HG00188, HG00189, HG00190, HG00266,
                     HG00267, HG00268, HG00269))

luhya = subset(data, select = c(NA19020, NA19028, NA19035, NA19036, NA19038,
                       NA19041, NA19044, NA19046, NA19307, NA19308, NA19309,
                       NA19310, NA19311, NA19312, NA19313, NA19315, NA19316,
                       NA19317, NA19318, NA19319))

yoruba = subset(data, select = c(NA18486, NA18486, NA18487, NA18489, NA18498,
                        NA18499, NA18501,  NA18502, NA18504, NA18505, NA18507,
                        NA18508, NA18510, NA18511, NA18516, NA18517, NA18519,
                        NA18520, NA18522, NA18523))
 
hanBei = subset(data, select = c(NA18548, NA18550, NA18555, NA18562, NA18536,
                          NA18543,  NA18613, NA18618, NA18620, NA18740, NA18745,
                          NA18757, NA18567, NA18574, NA18579, NA18593, NA18606,
                          NA18632, NA18637, NA18525))

hanSouth = subset(data, select = c(HG00479, HG00501, HG00513, HG00607, HG00614,
                          HG00556, HG00525, HG00537, HG00619, HG00626, HG00443,
                          HG00448, HG00436, HG00657, HG00671, HG00683, HG00690,
                          HG00708, HG00533, HG00449))
pops = list(brit, fin, luhya, yoruba, hanBei, hanSouth)
cat("Finished reading data.\n")
