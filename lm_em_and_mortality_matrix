# stochastic r data for juv mortality
juvr = c(-0.0282, -0.0415, -0.0551) # These numbers come from running Vortex ST... deterministic?
int = c(-0.02,0.0,0.02)
lm(juvr~int) # Here, the int (-0.6725) is the sensitivity

# Survival Matrix to Mortality Matrix
b=2.16/2;c0=0.3847;c=0.9250;s=0.677;s0=0.382; m=1-s; m0=1-s0;
a.s = matrix(c(c0*b*s0,c*b*s0,s,s),2,2,byrow=T);
a.m =matrix(c(c0*b*(1-m0),c*b*(1-m0),(1-m),(1-m)),2,2,byrow=T);
