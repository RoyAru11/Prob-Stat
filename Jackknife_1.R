#JACKKNIFE ESTIMATION OF BIAS AND STANDARD ERROR
install.packages("bootstrap")
library(bootstrap)
str(law)

# Define variables
lsat = law$LSAT
gpa = law$GPA

# Correlation estimate from observed sample
theta.hat = cor(lsat,gpa)
theta.hat

# Set up the Pseudocode for Jackknife
n = nrow(law) #sample size
est.val = numeric(n) #empty vector to store the jackknife estimats
for (i in 1:n) {
  est.val[i] = cor(lsat[-i], gpa[-i]) #correlation estimate for the leave-one-out-of-sample 
}

# Mean of the jackknife estimates
mean.jack = mean(est.val)
mean.jack

cbind(mean.jack, theta.hat) 

# Bias of Jackkife estimate
bias.jack = (n-1)*(mean.jack - theta.hat)
bias.jack

# Jackknife estimate of standard bias
sd.jack = sqrt( ((n-1)/n)*sum((est.val - mean.jack)^2) )
sd.jack                
