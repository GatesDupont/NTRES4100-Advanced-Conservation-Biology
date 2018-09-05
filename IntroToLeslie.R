# Source the Leslie "library"
source('C:/Users/lib-pac-b30a/Desktop/leslie.R', encoding = 'UTF-8')

# Create the leslie matrix of the population's demography ("projection matrix")
a = matrix(c(0,20,50,0,
           0.05,0,0,0,
           0,0.1,0,0,
           0,0,1,0), 4,4, byrow=T)

# Increase fertility by 5%
#a[1,] = a[1,] * 1.05

# Create a starting column-vector
n0 = matrix(c(10,10,10,10),4,1)

# How big is the population to start?
sum(n0)

# What is the size and structure going to be next year?
n1 = a%*%n0
sum(n1)

# Iterate through times steps for population
age_plot(a, 35, n0) # Projection matrix, number of generations (time-steps), starting vector
age_data = age_plot(a, 35, n0)

# Calculate lambda from this projection
age_data[35,5]/age_data[34,5] # lambda = N(T+1)/N(T)
age_data[34,5]/age_data[33,5]

# Age class ratio at equilirbium
age_data[35,1]/age_data[35,5]

# Raise the matrix to a power
# "constant growth rate of age classes and constant proportion..." I missed this. 3 things, idk.
a35 = mpower(a,35)

# Under assumptions, this is the equilibrium proportions of each age class ("stable age proprtions")
a35[,1]/sum(a35[,1])
a35[,2]/sum(a35[,2])

# Can we get lambda from raising the matrix to a power?
a36 = mpower(a, 36)
a36/a35 # These are all lambda!

# How many babies is each age class worth? Reproductive value!
a35[1,]/a35[1,1]


#----But we don't ever have to do it this way again----
calc_lam(a) # Lambda
calc_w(a) # Stable age proportions
calc_v(a) # Reproductive value


# Do we even need to include an age class with no reproductive value?
a3 = a[1:3,1:3]
calc_lam(a3)
calc_lam(a) # No, we don't, these are the same.
