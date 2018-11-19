# Gates Dupont  #
# 4100 Prelim 2 #
# F18           #
# # # # # # # # #

#------------------------------------QUESTION 2a------------------------------------

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

#----Calculating expected H----
cuspidate$He = 2 * cuspidate$p * (1-cuspidate$p)
drummondii$He = 2 * drummondii$p * (1-drummondii$p)
# Could also do equation 6 on review guide?

#----H stats for each pop----
H0.c = mean(cuspidate$H0) # Observed H
Hs.c = mean(cuspidate$He) # Metapopulation (total) expected H
Ht.c = mean(2*(cuspidate$p^2)) # Subpopulation expected H

H0.d = mean(drummondii$H0) 
Hs.d = mean(drummondii$He)
Ht.d = mean(2*(drummondii$p^2))

#----cuspidate F stats----
Fis.c = 1 - (H0.c / Hs.c) # Within-subpop H
Fst.c = 1 - (Hs.c / Ht.c) # Metapop H
Fit.c = Fis.c + Fst.c - Fis.c * Fst.c # Total / at both levels

#----drummondii F stats----
Fis.d = 1 - (H0.d / Hs.d)
Fst.d = 1 - (Hs.d / Ht.d)
Fit.d = Fis.d + Fst.d - Fis.d * Fst.d 

#------------------------------------QUESTION 2b------------------------------------

#----Calculating Nem----
Ne_m.d = 0.25*((1/Fst.d)-1)

#------------------------------------QUESTION 2c------------------------------------
Ne.d = Ne_m.d/0.1
