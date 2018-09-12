source("~/Documents/leslie.R")

#----Question 1----

# Life table values
x = 0:6
l = c(1,0.338,0.257,0.176,0.095,0.041,0)
m = c(0,0.222,1.344,1.536,2.5,2.5,0)

## Part (a)
# Generating survival between time steps (s)
s=c()

for(i in seq(length(x))){
  if(i<length(x)){
    s[i] = l[i+1]/l[i]
  } else {
    s[i] = 0
  }
}

# Compiling life table
lifeTable = data.frame(x,s,l,m)

## Part (c)
# Inputting Leslie Matrix
a = matrix(c(0.075,1.02,1.05,1.35,1.08,0,
           0.338,0,0,0,0,0,
           0,0.760,0,0,0,0,
           0,0,0.685,0,0,0,
           0,0,0,0.540,0,0,
           0,0,0,0,0.432,0), 6,6,byrow=T)

# Calculated critical values
calc_lam(a) # "Asymptotic growth of the populations" (Lambda)
calc_w(a) # Stable age proportions
calc_v(a) # Reporductive Value Vector

#----Question 2----
x = 0:8
l = c(1.0,0.856,0.625,0.375,0.171,0.064,0.018,0.004,0)
m = c(0,0,2,0,2,0,2,0,0)

lifeTable = data.frame(x,l,m,lm = l*m)

## Part a
sum(lifeTable$lm) # = 1.628, population is projected to grow

## Part c
A = matrix(c(0,1.46,0,0.912,0,0.562,0,0,
             0.856,0,0,0,0,0,0,0,
             0,0.73,0,0,0,0,0,0,
             0,0,0.6,0,0,0,0,0,
             0,0,0,0.456,0,0,0,0,
             0,0,0,0,0.374,0,0,0,
             0,0,0,0,0,0.281,0,0,
             0,0,0,0,0,0,0.222,0), 8, 8, byrow=T) # from Populus

# Running 20 samples to explore stable age proportions
set.seed(4100)
{
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) # stable
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) # stable
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) # stable
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1))
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) # stable
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) 
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) 
  age_plot(A, 30, matrix(sample(0:100,8,replace=T),8,1)) 
} # No, not all populations reach stable age distribution, but some of these appeared to!

round(calc_w(A),2)

#----Question 3----

# Part (b)
a = matrix(c(0,0,1.05,
             0.35,0,0,
             0,0.7,0.7), 3,3,byrow = T)
calc_lam(a)

#----Question 4----
a_post = matrix(c(0,10,18,0,
             0.08,0,0,0,
             0,0.15,0,0,
             0,0,1.0,0),4,4,byrow=T)
lam_post = round(calc_lam(a_post),10)


a_pre = matrix(c(0,(0.08*(10/0.15)),1.44,0,
             0.15,0,0,0,
             0,1,0,0,
             0,0,0,0), 4,4, byrow=T)
lam_pre = round(calc_lam(a_pre), 10)

lam_post == lam_pre # check

calc_v(a_post)
calc_v(a_pre)

calc_w(a_post)
calc_w(a_pre)
