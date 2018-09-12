# Gates Dupont #
# Fall 2018    #
# # # # # # # #

source("~/Documents/leslie.R")

a = matrix(c(0, 20, 50, 0.05, 0, 0, 0, 0.1, 0), 3, 3, byrow=T)

# Projected growth rate (lambda)
calc_lam(a)

# Eigenvalues (lambda*N = A*N)
eigen(a)
max(eigen(a)$values) # this is also lambda
eigen(a)$vectors[,1] # this assosciated eigenvector 
                         # gets at stable age proportion

# Calc_w normalizes the assosciate eigenvector 
# (adds them up, divides each by the sum)
# Stable age proprtions
calc_w(a)

# Reproductive value vector
calc_v(a)

# This comes from these same eigen vals
trans_a = t(a)

# Sensitivity
calc_sm(a)

# Just for fun, data viz of this
library(raster)
plot(raster(calc_sm(a)), main="Sensitivity Matrix", frame=F, axes=F)

# Elasticity
calc_em(a)
plot(raster(calc_em(a)), main="Elasticity Matrix", frame=F, axes=F)
