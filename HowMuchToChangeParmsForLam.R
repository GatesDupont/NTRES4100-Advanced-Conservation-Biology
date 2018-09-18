source("~/Documents/leslie.R")

# Our standard matrix
a = matrix(c(0,20,50,0.05,0,0,0,0.1,0), 3, 3, byrow=T)

# Lambda
calc_lam(a)

# Sensitivity matrix
calc_sm(a)
# There is no baby-to-baby transiiton, but if there was, what could the impact be?

# Elasticity matrix
calc_em(a) # You can only do elasticity for non-zeros, 
# can't calculate proportional change from zero!
# These add up to one, so prooprtional contribution of each to lambda

library(raster)
plot(raster(calc_em(a)), axes=F, main="Elasticity Matrix")

#
a = matrix(c(0,0.96,1.12,
         0.5,0,0,
         0,0.8,0),3,3, byrow=T)

calc_lam(a) # The population is decreasing... but lets say its a rare species
# that you dont want to go extinct

# So what would I need to do at minimum to stop the population from declining?
# Get lambda equal to 1... "stop the bleeding"
1 - calc_lam(a)

(sm = calc_sm(a))

# Doing the partial derivatives in r 
example.elements = expression(0, s_a*m_2, s_a*m_3,
                              s_j, 0, 0,
                              0, s_a, 0)
example.parms = list(s_a=0.8, s_j=0.5, m_2=1.2, m_3=1.4)
# You just need these two pieces

# Calculatting lower-level elasticities and sensitivities
lower_level(example.elements, example.parms)

# So here, we would want to change adult survival, because that has the highest
# elasticity value. And this is 80% of preliminary conservation plans.


demo = matrix(c(0,20,50,0.5,0,0,0,0.1,0), 3,3,byrow=T)
eigen(demo)

# GET THE NEW S_a VALUES FROM wxMaxima!!!
  # a: Input the matrix
  # cp: charpoly(a, lambda)
  # cp_test: substitue ([parameters with estimates, except what you want to chage, force lambda = 1], [cp])
  # float(solve(cp_test))

# Now put that newestiate, of how much to change Sa, into the original matrix.
a_revised = matrix(c(0,0.84117*1.2,0.84117*1.4,0.5,0,0,0,0.84117,0), 3, 3, byrow=T)
calc_lam(a_revised)
