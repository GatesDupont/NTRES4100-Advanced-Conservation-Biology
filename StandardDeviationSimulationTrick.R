# Gates Dupont #
# F18          #
# # # # # # # #

b = 0.9321; sd = 0.1828
# What would be the standard deviation if I douled the mean?

fem = rnorm(10000,b,sd)

hist(fem)
mean(fem) 
sqrt(var(fem)) # This is the known sd

both = fem*2

mean(both)
sqrt(var(both)) # This is the simulated sd if you double the mean
