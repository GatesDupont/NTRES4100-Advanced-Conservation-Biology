x = c(0,1,2,3,4)
lx = c(1, 0.1, 0.04, 0.022, 0.0)
mx = c(0, 0, 12, 35, 0)
lxmx = lx*mx

R0 = sum(lxmx)

r = 0.08563

lxmxerx = lxmx * exp(-1*r*x)

summation = c()
for(i in 1:5){
  if(i<4){
    summation[i] = sum(lxmxerx[(i+1):4])
  } else {
    summation[i] = 0
  }
}

erxOlx = round(( exp( (r*x) ) )/lx, 2)

RRV = summation*erxOlx

RV = RRV + mx

View(round(cbind(x, lx, mx, lxmx, lxmxerx, summation, erxOlx, RRV, RV), 4))
