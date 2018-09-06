#----Generate matrix----
a = matrix(c(2,1,3,3),2,2,byrow = T)

#----Get eigenvals and eigenvectors----
vals = eigen(a)

#----Product----
vals$values[1]*vals$vectors[,1]

#----You would get the same thing if you multiplied with the matrix-----
a%*%vals$vectors[,1] # So these are the same!!

#----Actual proj matrix----
a = matrix(c(0,20,50,0.05,0,0,0,0.1,0),3,3,byrow=T)

#----Lam----
calc_lam(a)
eigen(a)$values[1]
