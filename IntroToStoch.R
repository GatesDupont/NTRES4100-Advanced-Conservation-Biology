source("~/4100/leslie.r")

#----What does adding some variation to lambda do?----
mlam = 1.05 # arithmetic mean of lambda
lam = rnorm(1000,mlam,0.1)
hist(lam)
geomean(lam)

stochgr(lam) # This pulls from a number of replicates and just draws 95% CIs manually
# egc says people are moving more towards this instead of mathematical CIs.
# Pat sullivan did this sort of thing in 6700.

lam = rnorm(1000,mlam,0.4)
hist(lam)
summary(lam)
stochgr(lam)
# By increasing the variance, the population goes from increasing to decreasing

lam = rnorm(1000,mlam,0.1)
basic_plot(lam, 15, 5, 50)
lam = rnorm(1000,1,0.2)
basic_plot(lam, 15, 5, 50)
basic_plot(lam, 30, 5, 50)
basic_plot(lam, 60, 5, 50) # Increasing the amount of time


lam = rnorm(1000,1,0.1)
geomean(lam)
stoch.quasi.ext(as.list(lam), 100, 50, 100, 10)

# 1 Lambdas 2 3 4 5 How many populations in your sample? 6 How many samples?


lam = rnorm(1000,1,0.1)
stochgr(lam) # Calculate stochastic growth rate
stoch.quasi.ext(as.list(lam))

lambdaS = c()
variance = c(0.1,0.15,0.2,0.25,0.3)
for(i in variance){
  lam = rnorm(1000,1,0.1)
  lambdaS = c(lambdaS, stochgr(lam)) # Calculate stochastic growth rate
}

lam = rnorm(1000,1,0.1)
hold = stoch.quasi.ext(as.list(lam), 100, 50, 100, 100, 100)
