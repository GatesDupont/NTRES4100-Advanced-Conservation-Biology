# Gates Dupont #
# N4100 F18    #
# # # # # # # #

source("~/4100/leslie.R")


#----Stochastic matricies around a mean to deterministic----

# Setting up matricies
a = matrix(c(0,20,50,0.05,0,0,0,0.1,0), 3,3,byrow=T) # Good year
b = matrix(c(0,18,45,0.045,0,0,0,0.09,0), 3,3,byrow=T) # Average year
c = matrix(c(0,14,35,0.035,0,0,0,0.07,0), 3,3,byrow=T) # Bad year
calc_lam(a); calc_lam(b); calc_lam(c)

# Averaging the matricies
avg_lam = (calc_lam(a)+ calc_lam(b)+ calc_lam(c))/3
geoavg_lam = (calc_lam(a)* calc_lam(b)* calc_lam(c))^0.333333

# Calculating stochastic growth
env = list(a,b,c)
stochgr(env) # The stochastic growth rate here is very similar to the geometric mean
stochgr(env, 200) # RUn 200 times instead of 100

# Generating the mean matrix
mm = (a+b+c)/3
calc_lam(mm) # This growth rate is similar to the arithmetic average

# Sensitivity and elasticity
sm_mm = calc_sm(mm) # The big change comes from baby survival
em_mm = calc_em(mm)

# Calculating sotchastic sensitivity
stoch.sens(env, 100) # The number of runs is something worth reporting
# This isn't lower level, though
