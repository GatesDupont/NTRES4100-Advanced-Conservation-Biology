# Gates Dupont  #
# 4100 Prelim 2 #
# F18           #
# # # # # # # # #

library(RColorBrewer)


#----Setting up populations----
cuspidate = data.frame(
  subpop = 1:43,
  p = c(rep(1,40), 0.49, 0.83, 0.91), # allele frequency
  H0 = c(rep(0,40), 0.17, 0.06, 0.06)
)

drummondii = data.frame(
  subpop = 1:73,
  p = c(rep(1, 66), 0.86, 0.8, 0.7, 0.96, 0.96, 0.73, 0.91),
  H0 = c(rep(0, 66), 0.06, 0.12, 0.2, 0.03, 0.09, 0.15, 0.06)
)

#------------------------------------QUESTION 2a------------------------------------

#----Calculating expected H----
cuspidate$He = 2 * cuspidate$p * (1-cuspidate$p)
drummondii$He = 2 * drummondii$p * (1-drummondii$p)
# Could also do equation 6 on review guide?

#----H stats for each pop----
H0.c = mean(cuspidate$H0) # Observed H
Hs.c = mean(cuspidate$He) # Metapopulation (total) expected H
p.bar.c = mean(cuspidate$p)
Ht.c = 2 * p.bar.c * (1-p.bar.c)
#Ht.c.old = mean(2*(cuspidate$p^2)) # Subpopulation expected H ------------Not sure this is right

H0.d = mean(drummondii$H0) 
Hs.d = mean(drummondii$He)
p.bar.d = mean(drummondii$p)
Ht.d = 2 * p.bar.d * (1-p.bar.d)
#Ht.d.old = mean(2*(drummondii$p^2))

#----cuspidate F stats----
Fis.c = 1 - (H0.c / Hs.c) # Within-subpop H
Fst.c = 1 - (Hs.c / Ht.c) # Metapop H
Fit.c = 1 - (H0.c / Ht.c) # Total / at both levels
Fit.c.check = Fis.c + Fst.c - Fis.c * Fst.c # Math check

#----drummondii F stats----
Fis.d = 1 - (H0.d / Hs.d)
Fst.d = 1 - (Hs.d / Ht.d)
Fit.d = 1 - (H0.d / Ht.d)
Fit.d.check = Fis.d + Fst.d - Fis.d * Fst.d 

#----plotting----
par(mfrow=c(1,3))
barplot(c(Fis.c, Fis.d), names.arg = c("cuspidate", "drummondii"), main="Fis", ylim=c(-1,1), col = brewer.pal(n = 10, name = 'Spectral'))
barplot(c(Fst.c, Fst.d), names.arg = c("cuspidate", "drummondii"), main="Fst", ylim=c(0,1), col =  brewer.pal(n = 10, name = 'Spectral'))
barplot(c(Fit.c, Fit.d), names.arg = c("cuspidate", "drummondii"), main="Fit", ylim=c(-1,1), col =  brewer.pal(n = 10, name = 'Spectral'))
legend("topright",  fill = brewer.pal(n = 10, name = 'Spectral')[1:2], legend = c("Selfing", "Non-selfing"), cex=1.25)
mtext(expression(paste(bold("Population Structure in Selfing and Non-selfing Species of"), bolditalic(" Phlox"))), 
      side = 3, line = -42, outer = TRUE, cex=1.2)
dev.off()

#------------------------------------QUESTION 2b------------------------------------

#----Calculating Nem----
Ne_m.d = 0.25*((1/Fst.d)-1)
Ne_m.d2 = (1-Fst.d)/(4*Fst.d)

#------------------------------------QUESTION 2c------------------------------------

#----Calculating Ne----
Ne.d = Ne_m.d/0.1

#------------------------------------QUESTION 2d------------------------------------

#----Calculating Ne----
Ne.d.2 = Ne_m.d/(0.1*10)
