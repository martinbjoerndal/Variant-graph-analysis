##############################################################################
# md_scaling.r
#
# Analyses for master thesis
# ======================================
#
# Multidimensional scaling for plotting the populations relative to each other
# in space or plane.
###############################################################################

require(plot3D)
require(scatterplot3d)
require(rgl)
require(Matrix)
require(wesanderson)

d_BC = matrix(c(0,0.313,0.541,0.601,0.290,0.381,
  0.313,0,0.596,0.565,0.353,0.290,
  0.541,0.596,0,0.398,0.530,0.600,
  0.601,0.565,0.398,0,0.578,0.570,
  0.290,0.353,0.530,0.578,0,0.228,
  0.381,0.290,0.600,0.570,0.228,0),
  ncol = 6)

d_GED_list = read.table("GEDresults/gedData.txt", header = TRUE)$x
d_BF_table = read.table("bayesDataBubblesRemoved.txt")[-2:-1, -1]
d_BF_list = na.omit(as.vector(t(as.matrix(d_BF_table))))

K = 6
d_GED = matrix(0, ncol = K, nrow = K)
d_BF = matrix(0, ncol = K, nrow = K)

k = 1
for (i in 1:6) {
  for (j in 1:6) {
    
    if (i > j) {      
      d_GED[i, j] = d_GED_list[k]
      d_BF[i, j] = d_BF_list[k]
      
      k = k + 1
    }
  }
}

d_GED = forceSymmetric(d_GED, uplo = "L")
d_BF = forceSymmetric(d_BF, uplo = "L")

d_BC = as.dist(d_BC)
fit_BC <- cmdscale(d_BC, eig = TRUE, k = 3)

d_BF = as.dist(d_BF)
fit_BF <- cmdscale(d_BF, eig = TRUE, k = 3)

d_GED = as.dist(d_GED)
fit_GED <- cmdscale(d_GED, eig = TRUE, k = 3)

# plot solution 
x_BC <- fit_BC$points[,1]
y_BC <- fit_BC$points[,2]
z_BC <- fit_BC$points[,3]

x_GED <- fit_GED$points[,1]
y_GED <- fit_GED$points[,2]
z_GED <- fit_GED$points[,3]

x_BF <- fit_BF$points[,1]
y_BF <- fit_BF$points[,2]
z_BF <- fit_BF$points[,3]

nullVector = rep(0, 6)

labels = c("GBR", "FIN", "LWK", "YRI", "CHB", "CHS")
continentCol = c("darkgoldenrod", "darkgoldenrod", "maroon", "maroon",
  "darkslategray", "darkslategray")

rgl.open()
rgl.viewpoint(theta = 15, phi = 20)
par3d(windowRect = 50 + c(0, 0, 600, 600))
rgl.bg(color = "white")
plot3d(x_BC, y_BC, z_BC, size = 0, xlab = "", ylab = "", zlab = "", col = continentCol)
points3d(x_BC, y_BC, z_BC, size = 6, col = continentCol)

# Add lines to the floor
for (i in 1:6) {
  rgl.lines(c(x_BC[i], x_BC[i]), c(min(y_BC), y_BC[i]), c(z_BC[i], z_BC[i]),
            color = continentCol[i])
}

rgl.bbox(color = "black")
axes3d()
text3d(x_BC, y_BC + .02, z_BC, labels, cex = 1, col = continentCol)
rgl.postscript("../Thesis/figures/d_BC.pdf", fmt = "pdf")

rgl.open()
rgl.viewpoint(theta = 15, phi = 20)

par3d(windowRect = 50 + c(0, 0, 600, 600))
rgl.bg(color = "white")
plot3d(x_GED, y_GED, z_GED, size = 0, xlab = "", ylab = "", zlab = "", col = continentCol)
points3d(x_GED, y_GED, z_GED, size = 6, col = continentCol)

# Add lines to the floor
for (i in 1:6) {
  rgl.lines(c(x_GED[i], x_GED[i]), c(min(y_GED), y_GED[i]), c(z_GED[i], z_GED[i]),
            color = continentCol[i])
}

rgl.bbox(color = "black")
axes3d()
text3d(x_GED, y_GED + .02, z_GED, labels, cex = 1, col = continentCol)
rgl.postscript("../Thesis/figures/d_GED.pdf", fmt = "pdf")


rgl.open()
rgl.viewpoint(theta = 15, phi = 20)
par3d(windowRect = 50 + c(0, 0, 600, 600))
rgl.bg(color = "white")

plot3d(x_BF, y_BF, z_BF, size = 0, xlab = "", ylab = "", zlab = "", col = continentCol)
points3d(x_BF, y_BF, z_BF, size = 6, col = continentCol)

# Add lines to the floor
for (i in 1:6) {
  rgl.lines(c(x_BF[i], x_BF[i]), c(min(y_BF), y_BF[i]), c(z_BF[i], z_BF[i]),
            color = continentCol[i])
}

rgl.bbox(color = "black")
axes3d()
text3d(x_BF, y_BF + 900, z_BF, labels, cex = 1, col = continentCol)
rgl.postscript("../Thesis/figures/d_BF.pdf", fmt = "pdf")
