# Gates Dupont    #
# F18, NTRES 4100 #
# # # # # # # # # #

# Loading the Leslie library
source('~/4100/leslie.R')
library(raster)
library(ggplot2)
library(gridExtra)
library(dplyr)
set.seed(4100)





#----QUESTION 1----

# Generating given matricies
good = matrix(c(0,2,4,0.25,0,0,0,0.6,0),3,3,byrow=T)
poor = matrix(c(0,0.5,0.7,0.5,0,0,0,0.9,0),3,3,byrow=T)

# Given equation
x = -log(calc_lam(poor))/log(calc_lam(good))
calc_lam(poor)*(calc_lam(good)^x)

env = list(good, poor)

pv_test = c(1-(1/x),(1/x)) # Testing the frequencies of each
pv_test = c(x/(x+1),(1-(x/(x+1))))
stochgr(env, 100, pv_test)

pv_gc = c(0.41, 0.59) # Guess and check to get approximate frequencies forlambda = 1
stochgr(env, 100, pv_gc)

pv_comp = data.frame(pv_test, pv_gc)
rownames(pv_comp) = c("good", "poor")
t(pv_comp) # Comparing the friend's vals to correct vals

# So, our friend's equation doesn't work for a structured population
# but perhaps it would work for a scalar? Idk how to test this, though.

stochgr(list(calc_lam(good), calc_lam(poor)), 100, pv_test)
# This suggests that the friend's equation works a little better
# for scalar populations, but not perfect.
# structured: 1.058 vs scalar: 0.9935


#----Regression of proportion good/bad----

pr_good = seq(0,1,by=(1/5))
lambdas = c()
for(i in 1:6){
  lambdas = c(lambdas, stochgr(env, 100, c(pr_good[i], 1-pr_good[i])))
}

lambdas = c(0.8017918, 0.915635, 0.9974242, 1.045597, 1.059181, 1.038224)

lm1 = lm(lambdas~poly(pr_good, 2))
predictions <- predict(lm1,data.frame(pr_good=seq(0,1,0.0002)),interval='prediction')
pred_df = data.frame(fit = predictions[,1], pr_good = seq(0,1,by=(1/length(predictions[,1])))[1:5001])
final_prop = subset(pred_df, fit < 1.00005 & fit > 0.9999)

plot(lambdas~pr_good, pch=20, cex=2, main="Regression to find lambda=1",
     xlab = "Proportion of Good Years", ylab = "Lambda")
lines(pred_df$fit~pred_df$pr_good,col='blue', lwd=0.5)
abline(h=1.0, lty=2)
points(sum(final_prop$pr_good)/2,1, cex=3, pch=7, lwd=3, col="red")
text(0.51,0.99, label = paste(round((sum(final_prop$pr_good)/2),digits=4)), col="Red", cex=1.75)


#----QUESTION 2----

# Matrix 1
s0_1=0.4; sa_1=0.8; m2_1=1.35/0.8; m3_1=1.5/0.8

a1 = matrix(c(0    , sa_1*m2_1  ,sa_1*m3_1 ,
              s0_1 , 0          , 0        ,
              0    , sa_1       , 0        ),
            3,3,byrow=T)


# Matrix 2
s0_2=0.33; sa_2=0.65; m2_2=1.12/0.65; m3_2=1.25/0.65

a2 = matrix(c(0    , sa_2*m2_2 , sa_2*m3_2, 
              s0_2 , 0         , 0        , 
              0    , sa_2      , 0        )
            ,3,3,byrow=T)

#Matrix 3
s0_3=0.38 ; sa_3=0.85; m2_3=1.4/0.85; m3_3=1.6/0.85

a3 = matrix(c(0    , sa_3*m2_3, sa_3*m3_3,
              s0_3 , 0        , 0        ,
              0    , sa_3     , 0        ),
            3,3,byrow = T)

# Mean matrix
xa = (a1+a2+a3)/3 # arithmetic
ga = (a1*a2*a3)^(1/3) # geometric

#----Deterministic values for mean matrix----
calc_lam(ga) # lambda
calc_w(ga) # stable age
calc_v(ga) # repro val

# stochastic growth
env_2 = list(a1,a2,a3)
stochgr(env_2, 100)
abline(v = calc_lam(xa))

#----Generating extinction probabilities----
n0i = matrix(c(350,0,0))
n0ii = matrix(c(0,350,0))
n0iii = matrix(c(0,0,350))

stoch.quasi.ext(mat = env_2, n0 = n0i, Nx = 50, tmax = 40, maxruns = 125)
stoch.quasi.ext(mat = env_2, n0 = n0ii, Nx = 50, tmax = 40, maxruns = 125)
stoch.quasi.ext(mat = env_2, n0 = n0iii, Nx = 50, tmax = 40, maxruns = 125)

#----Find change in lambda----
m2 = ga[1,2]/ga[3,2]
m3 = ga[1,3]/ga[3,2]


params = c(s0=ga[2,1],sa=ga[3,2],m2=ga[1,2]/ga[3,2],m3=ga[1,3]/ga[3,2])
sym_mat = expression(0  , sa*m2, sa*m3 ,
                     s0 , 0    , 0     ,
                     0  , sa   , 0      )
lower_level(sym_mat, params) # sa has the highest elasticity

#----checking this value----
sa_new = 0.8526729763390464

ga_1.015 = matrix(c(ga[1,1], (ga[1,2]/ga[3,2])*sa_new, (ga[1,3]/ga[3,2])*sa_new,
                    ga[2,1], ga[2,2], ga[2,3],
                    ga[3,1], sa_new, ga[3,3]),
                  3,3,byrow=T)
calc_lam(ga_1.015)
prop.increase = sa_new/ga[3,2] # increase of just under 12 percent

#----Increasing other matricies----
sa1_new = a1[3,2]*prop.increase
a1_new = matrix(c(a1[1,1], (a1[1,2]/a1[3,2])*sa1_new, (a1[1,3]/a1[3,2])*sa1_new,
                  a1[2,1], a1[2,2], a1[2,3],
                  a1[3,1], sa1_new, a1[3,3]),
                3,3,byrow=T)

sa2_new = a2[3,2]*prop.increase
a2_new = matrix(c(a2[1,1], (a2[1,2]/a2[3,2])*sa2_new, (a2[1,3]/a2[3,2])*sa2_new,
                  a2[2,1], a2[2,2], a2[2,3],
                  a2[3,1], sa2_new, a2[3,3]),
                3,3,byrow=T)

sa3_new = a3[3,2]*prop.increase
a3_new = matrix(c(a3[1,1], (a3[1,2]/a3[3,2])*sa3_new, (a3[1,3]/a3[3,2])*sa3_new,
                  a3[2,1], a3[2,2], a3[2,3],
                  a3[3,1], sa3_new, a3[3,3]),
                3,3,byrow=T)

#----Calculating new stochgr with comparison to previous----
stochgr(list(a1,a2,a3), 100)
stochgr(list(a1_new,a2_new,a3_new),100)

#----QUESTION 3----



#----wet Years----

# Inputting vital rates
wf1=8.0 ; wf2=12.0 ; wf3=18.0 ; ws=0.400 ; we1=0.100 ; we2=0.020 ; wg=0.0725

# Generating population matrix
wet = matrix(c( wf1*wg     , wf2*wg     , wf3*wg     , 0          , 0          ,
                
                0          , 0          , 0          , ws*we1     , 0          ,
                
                0          , 0          , 0          , 0          , ws*we2     ,
                
                wf1*(1-wg) , wf2*(1-wg) , wf3*(1-wg) , 0          , 0          ,
                
                0          , 0          , 0          , ws*(1-we1) , ws*(1-we2)  ),
             5,5, byrow=T)

calc_lam(wet) # asymptotic growth rate




#----Dry Years----

df1=8.2 ; df2=12.5 ; df3=19.0 ; ds=0.425 ; de1=0.058 ; de2=0.0045 ; dg=0.080

dry = matrix(c( df1*dg     , df2*dg     , df3*dg     , 0          , 0          ,
                
                0          , 0          , 0          , ds*de1     , 0          ,
                
                0          , 0          , 0          , 0          , ds*de2     ,
                
                df1*(1-dg) , df2*(1-dg) , df3*(1-dg) , 0          , 0          ,
                
                0          , 0          , 0          , ds*(1-de1) , ds*(1-de2)  ),
             5,5, byrow=T)

calc_lam(dry) # asymptotic growth rate




#----ltre_ll----

alpha = calc_lam(wet)-calc_lam(dry)

p.wet=list(f1=8.0, f2=12.0, f3=18.0, s=0.400, e1=0.100, e2=0.020 , g=0.0725)
p.dry=list(f1=8.2, f2=12.5, f3=19.0, s=0.425, e1=0.058, e2=0.0045, g=0.080)
parms = list(p.wet, p.dry)

structure = expression(f1*g     , f2*g     , f3*g     , 0        , 0        ,
                       0        , 0        , 0        , s*e1     , 0        ,
                       0        , 0        , 0        , 0        , s*e2     ,
                       f1*(1-g) , f2*(1-g) , f3*(1-g) , 0        , 0        ,
                       0        , 0        , 0        , s*(1-e1) , s*(1-e2)  )
ltre_ll(parms, structure, 1) # r is the matrix to compare to. If rmd, assumes mean mtrx


contribution = c(0.010289910, 0.009880765, 0.001876482, 0.020261072, -0.127117003, -0.043567216, 0.038743643)
ltre_df = data.frame(parameter_names, contribution)
ggplot(ltre_df, aes(x = parameter_names)) + theme_classic() +
  geom_col(aes(y=contribution, fill=contribution)) +
  scale_fill_gradientn(colours = gray.colors(7)) +
  ggtitle("Contribution to alpha of each parameter from the dry population") +
  theme(plot.title = element_text(hjust = 0.2, face="bold")) +
  labs(x = "Parameters") +
  geom_hline(size=2, colour = "black", yintercept = -(calc_lam(wet)-calc_lam(dry)))


#----overall em----

par(mfrow=c(1,2))
# WET Viz of elasticity
wet.em = calc_em(wet)
r.wet.em = raster(wet.em)
plot(r.wet.em, axes=F, main = "Wet Years - Elasticity Matrix", 
     col = colorRampPalette(c("gray", "purple", "blue"))(100)); text(r.wet.em, digits=4, col="white", font=2, cex=1.1)

# DRY Viz of elasticity
dry.em = calc_em(dry)
r.dry.em = raster(dry.em)
plot(r.dry.em, axes=F, main = "Dry Years - Elasticity Matrix", 
     col = colorRampPalette(c("gray", "purple", "blue"))(100)); text(r.dry.em, digits=4, col="white", font=2, cex=1.1)
dev.off()



#----ll em----

# WET Running LL
wet.parms = list(wf1=8.0, wf2=12.0, wf3=18.0, ws=0.400, we1=0.100, we2=0.020, wg=0.0725)
wet.elems = expression(wf1*wg     , wf2*wg     , wf3*wg     , 0          , 0          ,
                       0          , 0          , 0          , ws*we1     , 0          ,
                       0          , 0          , 0          , 0          , ws*we2     ,
                       wf1*(1-wg) , wf2*(1-wg) , wf3*(1-wg) , 0          , 0          ,
                       0          , 0          , 0          , ws*(1-we1) , ws*(1-we2)  )
wetLL = data.frame(lower_level(wet.elems, wet.parms))

# DRY Running LL
dry.parms = list(df1=8.2, df2=12.5, df3=19.0, ds=0.425, de1=0.058, de2=0.0045, dg=0.080)
dry.elems = expression(df1*dg     , df2*dg     , df3*dg     , 0          , 0          ,
                       0          , 0          , 0          , ds*de1     , 0          ,
                       0          , 0          , 0          , 0          , ds*de2     ,
                       df1*(1-dg) , df2*(1-dg) , df3*(1-dg) , 0          , 0          ,
                       0          , 0          , 0          , ds*(1-de1) , ds*(1-de2)  )
dryLL = data.frame(lower_level(dry.elems, dry.parms))

parameter_names = c("F1", "F2", "F3", "S", "e1", "e2", "gamma")

# Making plots
rownames(wetLL) = parameter_names
wet_em_bar = ggplot(wetLL, aes(x = row.names(wetLL))) + 
  geom_col(aes(y=elasticity, fill=elasticity)) +
  scale_fill_gradientn(colours = rev(topo.colors(7))) +
  ggtitle("Wet: Lower-Level Elasticity") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  labs(x = "Parameters")

rownames(dryLL) = parameter_names
dry_em_bar = ggplot(dryLL, aes(x = row.names(dryLL))) + 
  geom_col(aes(y=elasticity, fill=elasticity)) +
  scale_fill_gradientn(colours = rev(topo.colors(7))) +
  ggtitle("Dry: Lower-Level Elasticity") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  labs(x = "Parameters")

grid.arrange(wet_em_bar, dry_em_bar, ncol=1)




#----Decrease F----

# WET Decreasing rate of seed production by 10%
wf1_red=8.0*0.9 ; wf2_red=12.0*0.9 ; wf3_red=18.0*0.9

wet_redF = matrix(c( wf1_red*wg     , wf2_red*wg     , wf3_red*wg      , 0          , 0          ,
                     0              , 0              , 0               , ws*we1     , 0          ,
                     0              , 0              , 0               , 0          , ws*we2     ,
                     wf1_red*(1-wg) , wf2_red*(1-wg) , wf3_red*(1-wg)  , 0          , 0          ,
                     0              , 0              , 0               , ws*(1-we1) , ws*(1-we2)  ),
                  5,5, byrow=T)
calc_lam(wet)
calc_lam(wet_redF) # asymptotic growth rate
calc_lam(wet) - calc_lam(wet_redF) # -0.0689

# DRY Decreasing rate of seed production by 10%
df1_red=8.2*0.9 ; df2_red=12.5*0.9 ; df3_red=19.0*0.9

dry_redF = matrix(c( df1_red*dg     , df2_red*dg     , df3_red*dg     , 0          , 0          ,
                     0              , 0              , 0              , ds*de1     , 0          ,
                     0              , 0              , 0              , 0          , ds*de2     ,
                     df1_red*(1-dg) , df2_red*(1-dg) , df3_red*(1-dg) , 0          , 0          ,
                     0              , 0              , 0              , ds*(1-de1) , ds*(1-de2)  ),
                  5,5, byrow=T)
calc_lam(dry)
calc_lam(dry_redF) # asymptotic growth rate
calc_lam(dry) - calc_lam(dry_redF) # -0.0711

basic_plot(mat=list(wet_redF, dry_redF), 80, 14)


#----Proportion wet:dry----

pr_good_3 = seq(0,1,by=(1/5))
lambdas = c()
for(i in 1:6){
  lambdas = c(lambdas, stochgr(list(wet,dry), 250, c(pr_good_3[i], 1-pr_good_3[i])))
}

mean_lambdas = c(0.9725294, 0.9948764, 1.015122, 1.033453, 1.050089, 1.065072)

lm3 = lm(mean_lambdas~poly(pr_good_3, 2))
predictions3 <- predict(lm3,data.frame(pr_good_3=seq(0,1,0.0002)),interval='prediction')
pred_df_3 = data.frame(fit = predictions3[,1], pr_good_3 = seq(0,1,by=(1/length(predictions3[,1])))[1:5001])
final_prop_3 = subset(pred_df_3, fit < 1.00002 & fit > 0.99997)

plot(mean_lambdas~pr_good_3, pch=20, cex=2, main="Regression to find lambda=1",
     xlab = "Proportion of Good Years", ylab = "Lambda")
lines(pred_df_3$fit~pred_df_3$pr_good_3,col='blue', lwd=0.5)
abline(h=1.0, lty=2)
points(sum(final_prop_3$pr_good_3)/2,1, cex=3, pch=7, lwd=3, col="red")
#text(0.51,0.99, label = paste(round((sum(final_prop$pr_good)/2),digits=4)), col="Red", cex=1.75)

stochgr(list(wet, dry), 100, c(0.24985, 1-0.24985))
stochgr(list(wet, dry), 100, c(0.238, 1-0.238))

# Looking at population projection
basic_plot(mat=list(wet, dry), 2500, 100, prob_vec = c(0.238, 1-0.238))




#----Comparison of population projections-----

par(mfrow=c(2,2))
# No management
basic_plot(mat=list(wet, dry), 100, 100, prob_vec = c(7/11,4/11))

# Seed predator
basic_plot(mat=list(wet_redF, dry_redF), 100, 100, prob_vec = c(7/11,4/11))

# Manage for water
basic_plot(mat=list(wet, dry), 2500, 100, prob_vec = c(0.238, 1-0.238))

# Manage with seed predator and water control
basic_plot(mat=list(wet_redF, dry_redF), 100, 100, prob_vec = c(0.238, 1-0.238))
