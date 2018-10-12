# Gates Dupont #
# N4100 F18    #
# # # # # # # #
 #source('~/4100/leslie.R', encoding = 'UTF-8')
library(raster)
 ### Question 1 ####
 #----Arjosh Fish----
  s0=0.15; s1=0.15; s2=0.15; s3=0;
  m2=37; m3=50;
  a = matrix(c(0,s1*m2, s2*m3, s3, s0, 0,0,0,0,s1,0,0,0,0,s2,0), 4,4, byrow=T)
  # Calculations
 calc_lam(a)
 calc_v(a)
 calc_w(a)
 
 a.parms <- list(s0=0.15, s1=0.15, s2=0.15, s3=0, m2=25, m3=50);
 a.elems <- expression(0,s1*m2, s2*m3, s3, s0, 0,0,0,0,s1,0,0,0,0,s2,0);
 #lower_level(a.elems, a.parms)
  N = matrix(c(100,100,10,5),4,1, byrow = T)
 
 age_plot(a, 30, N)
 
 calc_lam(a)
 
 #----ANSWER----
 # Put feeders with 2 year olds, because you don't need to change m2
 # as much as you would need to change m3 in order to reach stability.
 # Also, if you were to change m3, you would have to double it,
 # and for an age class of just 5 individuals, that's likely almost
 # impossible, and even less possible in a stochastic system.
 
 
 
### Question 2 ###
 
#----Sea Urchins----
 s_0=0.2;s_1=0.3;s_2=0.5;s_3=0.6;m_4=65;beta_0=0.1;psi_0=0.4;psi_1=0.6;psi_2=0.2;gamma_1=0.1;gamma_3=0.05;
 a=matrix(c(s_0*(1-psi_0)*(1-beta_0),s_1*(1-psi_1)*gamma_1,0,s_3*(1-gamma_3)*m_4,s_0*psi_0*(1-beta_0),s_1*(1-psi_1)*(1-gamma_1),0,s_3*gamma_3,s_0*(1-psi_0)*beta_0,s_1*psi_1,s_2*(1-psi_2),0,0,0,s_2*psi_2,s_3*(1-gamma_3)),4,4,byrow=T)
 calc_lam(a) # decline of nearly 13% per year
 # Calculatting lower-level elasticities and sensitivities
elements = expression(s_0*(1-psi_0)*(1-beta_0),s_1*(1-psi_1)*gamma_1,0,s_3*(1-gamma_3)*m_4,s_0*psi_0*(1-beta_0),s_1*(1-psi_1)*(1-gamma_1),0,s_3*gamma_3,s_0*(1-psi_0)*beta_0,s_1*psi_1,s_2*(1-psi_2),0,0,0,s_2*psi_2,s_3*(1-gamma_3))
parms = list(s_0=0.2,s_1=0.3,s_2=0.5,s_3=0.6,m_4=65,beta_0=0.1,psi_0=0.4,psi_1=0.6,psi_2=0.2,gamma_1=0.1,gamma_3=0.05)
lower_level(elements, parms)
# Highest sensitivity: s_0, highest elasticity: s_3 
 a.sm = calc_sm(a)
r.a.sm = raster(a.sm)
plot(r.a.sm, axes=F, main = "Sea Urchins - Sensitivty Matrix", 
     col = colorRampPalette(c("gray90", "yellow", "green"))(100)); text(r.a.sm, digits=4,  font=2, cex=1.1)
 
a.em = calc_em(a)
r.a.em = raster(a.em)
plot(r.a.em, axes=F, main = "Sea Urchins - Elasticity Matrix", 
     col = colorRampPalette(c("gray", "purple", "blue"))(100)); text(r.a.em, digits=4, col="white", font=2, cex=1.1)
 #----ANSWER----
# Both gammas have negative sm and em _> increase in gammas = decrease in lambda (shrink)
# beta, though, has positive sm and em -> increase in beta = increase in lambda (skip)
# So warming waters causes increase in gamma for a decrease in lambda
# and a decrease in beta so an extra decrease in lambda. Population will decline even faster.
 ### Question 3 ####
 #----Newts----
  
# far
s0_f=0.5; s1_f=0.6; s2_f=0.7; s3_f=0.8;
m2_f=1.9; m3_f=3.2;
 # near
s0_n=0.3; s1_n=0.4; s2_n=0.7; s3_n=0.78;
m2_n=1.5; m3_n=2.8;
 far <- matrix(c(0,s1_f*m2_f,s2_f*m3_f,s3_f*m3_f,s0_f,0,0,0,0,s1_f,0,0,0,0,s2_f,s3_f),4,4,byrow=T)
near <- matrix(c(0,s1_n*m2_n,s2_n*m3_n,s3_n*m3_n,s0_n,0,0,0,0,s1_n,0,0,0,0,s2_n,s3_n),4,4,byrow=T)
 calc_lam(far)
calc_lam(near)
 p_far=list(s0=0.5, s1=0.6, s2=0.7, s3=0.8, m2=1.9, m3=3.2)
p_near=list(s0=0.3, s1=0.4, s2=0.7, s3=0.78, m2=1.5, m3=2.8)
parms = list(p_far, p_near)
 structure = expression(0,s1*m2,s2*m3,s3*m3,s0,0,0,0,0,s1,0,0,0,0,s2,s3)
ltre_ll(parms, structure,1)
 #----ANSWER----
# Near population isn't growing as quickly as the far population (alpha = -0.3)
# Difference is driven by declines in all lower level parameters except sub-adult survival
# and is primarily due to s0 (egg survival), which is LTRE ll sens = -0.13860402 out of the -0.3.
