# Gates Dupont #
# 2018         #
# # # # # # # # 

source('~/4100/leslie.R', encoding = 'UTF-8')

a = matrix(c(0,20,50,
             0.05,0,0,
             0,0.1,0), 3,3,byrow=T)

calc_lam(a)
# Lets say this is something bad (invasive)
# so we want to get lambda to 1

# This tells you the parameter to change
example.elements = expression(0,s1*m2,s2*m3,s0,0,0,0,s1,0)
example.parms = list(s0=0.05,s1=0.1,s2=1,m2=200,m3=50)
lower_level(example.elements, example.parms)

# But let's just say for some reason we want to change m2
# Because its logisitically easy or something

#----wxMaxima----

# 1. Input the parameter matrix
# 2. Generate characteristic polynomial (charpoly(a,lambda))
  # cp: charpoly(a,lambda);
# 3. Substitue in the parameter estimates, except the one you're interested in changing, set lambda to what you want
  # cp_new: substitute([s0=0.05,s1=0.1,s2=1,m3=50,lambda=1],[cp]);  
# 4. Solve for parameter from the substitution
  # float(solve(cp_new));

#----Back in R----

# Remake the matrix with the new value
a_new = matrix(c(0,0.1*150,50,0.05,0,0,0,0.1,0),3,3,byrow=T)

# Check lambda
calc_lam(a_new)
