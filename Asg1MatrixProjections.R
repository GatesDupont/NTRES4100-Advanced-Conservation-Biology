source("~/Documents/leslie.R")

library(demogR)

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

n = matrix(c(10,0,0,0,0,0,0,0),8,1)

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
