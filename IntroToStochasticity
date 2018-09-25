# Gates Dupont #
# F18          #
# # # # # # # # 

#----Leslie Library----
#source('~/4100/leslie.R', encoding = 'UTF-8')


#----Random number generation----
set.seed(4100)
test = rnorm(100, 5, sd=0.75)
mean(test)
sqrt(var(test))

#----Arithmetic vs geometric mean----
mean(test)
geomean(test) # This ill always be smaller than arithmetic mean

#----Numerical demonstration that first and last population size get you lambda----
set.seed(4100)
lam = rnorm(1000, 1, 0.25) # vector of lambdas in a time series
pop_size = hist_plot(lam, 15, 1000, 100) # This shows that the 
# Distribution of the outcomes is log-normal

gm = geomean(lam)
100*gm^15

ln_pop = (log(pop_size)-log(100))/15
hist(ln_pop)
med = median(ln_pop)
exp(med)
gm

#----Arithmetic average vs median
pop_sizes = basic_plot(lam, 15, 8, 100)
geomean(lam)
pop_sizes = basic_plot(lam, 15, 500, 100)
# Dealing with log-normal distribution, so the mean is no longer a good measure
# of what the average population is doing.
hist_plot(lam, 15, 500, 100) # Just look at the populations at the last time step
# Artithmetic average is biased high compared to median
