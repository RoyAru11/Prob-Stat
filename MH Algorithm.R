target = function(x){
  return(ifelse(x<0,0,exp(-x)))
}
x=rep(0,10000)
x[1]=3 #initialize; arbitrarily set this to 3
for (i in 2:10000) {
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x)
  if(runif(1)<A){
    x[i] = proposed_x #accept move with probability min(1,A)
  } else {
    x[i] = current_x #otherwise "reject" move, and stay were we are
  }
}
plot(x,main = "value of x visited by the MH algorithm")

hist(x,xlim=c(0,10),probability = TRUE, main="Histogram of values of x visited by MH algorithm")
xx = seq(0,10,length=100)
lines(xx,target(xx),col="red")