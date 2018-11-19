# Gates Dupont  #
# 4100 Prelim 2 #
# F18           #
# # # # # # # # #


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

#----cuspidate F stats----
Fis.c = 1 -  (mean(cuspidate$H0) / mean(cuspidate$He))
Fst.c = 1 - (mean(cuspidate$He) / mean(2*(cuspidate$p^2)))
Fit.c = Fis.c + Fst.c - Fis.c * Fst.c 

#----drummondii F stats----
Fis.d = 1 - (mean(drummondii$H0) / mean(drummondii$He))
Fst.d = 1 - (mean(drummondii$He) / mean(2*(drummondii$p^2)))
Fit.d = Fis.d + Fst.d - Fis.d * Fst.d 

#----Dummy equations----
Fis = 1 - H0/Hs # Within-subpop H
Fst = 1 - Hs/Ht # Metapop H
Fit = Fis + Fst - Fis * Fst # Total / at both levels

H0 # Observed H
Ht # Metapopulation (total) expected H
Hs # Subpopulation expected H
