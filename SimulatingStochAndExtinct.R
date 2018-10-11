# Gates Dupont #
# F18          #
# # # # # # # #
 #----Setting up matricies----
# These matricies just represent two different environments
a = matrix(c(0.15, 3, 0.25, 0),2,2,byrow=T)
b = matrix(c(0.75, 0.1, 1, 0),2,2,byrow=T)
a
b
calc_lam(a); calc_lam(b) # Both less than 1, so declining

 #----Stochastic growth rate----
lam = list(calc_lam(a), calc_lam(b)) # projected asymptotic growth rate
stochgr(lam)
 mat = list(a, b)
mm = (a+b)/2
mm
 start = calc_w(mm) # Still less than one!
start
 start = t(start)
start = round(start*100)
stochgr(mat) # The population is now growing rapidly
 basic_plot(mat, 15, 5, start) # Run this a bunch of times, all increasing.
# So, you cannot simply look at asymptotic properties of individual matrixies in a stochastic world
# With scalars, you could look at geomean, you can't do that here.
# So, the only way to come up with growth rates is to simulate it.

 #----Simulating the population based on the number of bad/good years----
g = matrix(c(0,20,50,0.05,0,0,0,0.1,0),3,3,byrow=T)
b = g*0.8
env = list(g,b)
stochgr(env)
# What if the number of bad years starts to increase?
env = list(g,b,b)
stochgr(env)
# Or you can do this mor elegantly
env = list(g,b)
pv = c(0.2,0.8)# pick a good year 20% of the time and  a bad year 80% of the time
stochgr(env, 100, pv)

 #----Starting population size and extinction probability----
a = matrix(c(0,1.05,0.3,0.7),2,2,byrow=T); calc_lam(a) # Assuming no stochasticity, pop. wont go extinct
sv = calc_w(a)
sv = t(sv)
n10 = round(sv*10)
n25 = round(sv*25)
n50 = round(sv*50)
n1000 = round(sv*1000)
vc = matrix(0,2,2) # Not having any variance
stoch.demog.sim(a, n1000, 50, 500, Next = 5, varF = vc) # Little to no extinction probabilities
stoch.demog.sim(a, n10, 50, 500, Next = 5, varF = vc) # Much higher extinction probabilities here!
