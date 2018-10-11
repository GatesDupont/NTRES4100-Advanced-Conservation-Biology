# Gates Dupont    #
# F18, NTRES 4100 #
# # # # # # # # # #

# Loading the Leslie library
source('~/4100/leslie.R')
library(raster)

#----Question 1----

### WET YEARS ###

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
calc_v(wet) # repro value
calc_w(wet) # stable age dist.

# Viz of sensitivity
wet.sm = calc_sm(wet)
r.wet.sm = raster(wet.sm)
plot(r.wet.sm, axes=F, main = "Wet Years - Sensitivty Matrix",
     col = colorRampPalette(c("gray90", "yellow", "green"))(100)); text(r.wet.sm, digits=4,  font=2, cex=1.1)

# Viz of elasticity
wet.em = calc_em(wet)
r.wet.em = raster(wet.em)
plot(r.wet.em, axes=F, main = "Wet Years - Elasticity Matrix", 
     col = colorRampPalette(c("gray", "purple", "blue"))(100)); text(r.wet.em, digits=4, col="white", font=2, cex=1.1)

# Running LL
wet.parms = list(wf1=8.0, wf2=12.0, wf3=18.0, ws=0.400, we1=0.100, we2=0.020, wg=0.0725)
wet.elems = expression(wf1*wg     , wf2*wg     , wf3*wg     , 0          , 0          ,
                       0          , 0          , 0          , ws*we1     , 0          ,
                       0          , 0          , 0          , 0          , ws*we2     ,
                       wf1*(1-wg) , wf2*(1-wg) , wf3*(1-wg) , 0          , 0          ,
                       0          , 0          , 0          , ws*(1-we1) , ws*(1-we2)  )
lower_level(wet.elems, wet.parms)
wetLL = data.frame(lower_level(wet.elems, wet.parms)); wetLL$vars = rownames(wetLL)
r.sm.wetLL = raster(as.matrix(wetLL[,2]))
r.em.wetLL = raster(as.matrix(wetLL[,3]))


# Dry years
df1=8.2 ; df2=12.5 ; df3=19.0 ; ds=0.425 ; de1=0.058 ; de2=0.0045 ; dg=0.080

dry = matrix(c( df1*dg     , df2*dg     , df3*dg     , 0          , 0          ,
                
                0          , 0          , 0          , ds*de1     , 0          ,
                
                0          , 0          , 0          , 0          , ds*de2     ,
                
                df1*(1-dg) , df2*(1-dg) , df3*(1-dg) , 0          , 0          ,
                
                0          , 0          , 0          , ds*(1-de1) , ds*(1-de2)  ),
             5,5, byrow=T)

calc_lam(dry) # asymptotic growth rate
calc_v(dry) # repro value
calc_w(dry) # stable age dist.

# Viz of sensitivity
dry.sm = calc_sm(dry)
r.dry.sm = raster(dry.sm)
plot(r.dry.sm, axes=F, main = "Dry Years - Sensitivty Matrix",
     col = colorRampPalette(c("gray90", "yellow", "green"))(100)); text(r.dry.sm, digits=4,  font=2, cex=1.1)

# Viz of elasticity
dry.em = calc_em(dry)
r.dry.em = raster(dry.em)
plot(r.dry.em, axes=F, main = "Dry Years - Elasticity Matrix", 
     col = colorRampPalette(c("gray", "purple", "blue"))(100)); text(r.dry.em, digits=4, col="white", font=2, cex=1.1)

# Running LL
dry.parms = list(df1=8.2, df2=12.5, df3=19.0, ds=0.425, de1=0.058, de2=0.0045, dg=0.080)
dry.elems = expression(df1*dg     , df2*dg     , df3*dg     , 0          , 0          ,
                       0          , 0          , 0          , ds*de1     , 0          ,
                       0          , 0          , 0          , 0          , ds*de2     ,
                       df1*(1-dg) , df2*(1-dg) , df3*(1-dg) , 0          , 0          ,
                       0          , 0          , 0          , ds*(1-de1) , ds*(1-de2)  )
lower_level(dry.elems, dry.parms)
dryLL = data.frame(lower_level(dry.elems, dry.parms)); dryLL$vars = rownames(dryLL)
r.sm.dryLL = raster(as.matrix(dryLL[,2]))
r.em.dryLL = raster(as.matrix(dryLL[,3]))


# Comparing the lower-level analyses for the two populations
par(mfrow=c(2,2))
plot(r.sm.wetLL, col = colorRampPalette(c("gray90", "yellow", "green"))(100))
plot(r.em.wetLL, col = colorRampPalette(c("gray", "purple", "blue"))(100))
plot(r.sm.dryLL, col = colorRampPalette(c("gray90", "yellow", "green"))(100))
plot(r.em.dryLL, col = colorRampPalette(c("gray", "purple", "blue"))(100))
dev.off()
