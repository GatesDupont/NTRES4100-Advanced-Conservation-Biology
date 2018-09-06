source("~/Documents/leslie.R")

#----Enter matrix model----
a = matrix(c(0, 4.8, 19.25, 0,
           0.1,   0,     0, 0,
             0, 0.4,     0, 0,
             0,   0,  0.55, 0),
           4,4, byrow=T)

#----Calculate growth rate after equilibirium assuming something----
calc_lam(a) # Lambda = change in population size from one time step to the next
# Population is PROJECTED to grow by 9%/time step

#----Calculate reproducive values----
calc_v(a) # RV

#----Calculate the stable age proportions----
calc_w(a) 

# As long as the matrix is not changing through time, 
# the expectation is the population will equillibrate
