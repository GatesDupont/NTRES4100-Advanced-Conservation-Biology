# Gates Dupont    #
# F18, NTRES 4100 #
# # # # # # # # # #

# Loading the Leslie library
source('~/4100/leslie.R')
library(raster)
library(ggplot2)
library(gridExtra)
library(dplyr)

#----QUESTION 1----



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





#----Question 1----

# Generating given matricies
good = matrix(c(0,2,4,0.25,0,0,0,0.6,0),3,3,byrow=T)
poor = matrix(c(0,0.5,0.7,0.5,0,0,0,0.9,0),3,3,byrow=T)

# Given equation
x = -log(calc_lam(poor))/log(calc_lam(good))
calc_lam(poor)*(calc_lam(good)^x)

env = list(good, poor)

pv_test = c(1-(1/x),(1/x)) # Testing the frequencies of each
stochgr(env, 100, pv_test)

pv_gc = c(0.41, 0.59) # Guess and check to get approximate frequencies forlambda = 1
stochgr(env, 500, pv_gc)

pv_comp = data.frame(pv_test, pv_gc)
rownames(pv_comp) = c("good", "poor")
t(pv_comp) # Comparing the friend's vals to correct vals

# So, our friend's equation doesn't work for a structured population
# but perhaps it would work for a scalar? Idk how to test this, though.

stochgr(list(calc_lam(good), calc_lam(poor)), 100, pv_test)
# This suggests that the friend's equation works a little better
# for scalar populations, but not perfect.
