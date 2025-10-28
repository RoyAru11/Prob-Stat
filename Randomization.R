library(tidyverse)
library(mosaic)
library(bootstrap)
set.seed(123)

rflip(100, prob = 0.52)
do(10)*nflip(100, prob = 0.52)

dessert_surveys = do(1000)*nflip(100, prob = 0.52)
head(dessert_surveys)

dessert_surveys = dessert_surveys %>%
  mutate(huckleberry_prop = nflip/100)

ggplot(dessert_surveys) + 
  geom_histogram(aes(x=huckleberry_pro), binwidth = 0.01)
dessert_surveys %>%
  summarize(std_err = sd(huckleberry_prop))

# Randomization
male.mandibles <- c(120,107,110,116,114,111,113,117,114,112)
female.mandibles <- c(110,111,107,108,110,105,107,106,111,111)
jackals <- tibble(sex = c(rep("m", 10),rep("f", 10)),
                  mandible.length = c(male.mandibles, female.mandibles))

ggplot(jackals, aes(x = sex, y = mandible.length, color = sex)) +
  geom_jitter(width = 0.1,height = 0,size=2,alpha=0.75)

jackals %>%
  group_by(sex) %>%
  summarize(mean.mandible.length = mean(mandible.length))
mean(male.mandibles) - mean(female.mandibles) 

randomized.two.groups <- function(x,n1,n2){
  # sample w/out replacement
  reordered <- sample(x, length(x)) # see help(sample) for more info
  g1 <- reordered[seq(1,n1)] # take the first n1 items as group 1
  g2 <- reordered[seq(n1+1,n1+n2)] # take the remaining items as group 2
  print(list(g1,g2))

}

randomized.two.groups(jackals$mandible.length,10,10)
randomized.two.groups(jackals$mandible.length,10,10)

mean.diff <- function(x1, x2) {
  mean(x1) - mean(x2)
}
randomization <- function(x1, x2, fxn, nsamples=100){
  stats <- c()
  orig <-c(x1,x2)
  for (i in 1:nsamples) {
    g <- randomized.two.groups(orig, length(x1), length(x2))
    stats <- c(stats,fxn(g[[1]],g[[2]]))
  }
  return (stats)
}

# generate 1000 samples of the mean.diff for randomized data
rsample <- randomization(male.mandibles,
                         female.mandibles,
                         mean.diff, 1000)

# examine the distribution
quickplot(rsample, geom='histogram',
          bins=20,
          main="Histogram of randomized differences\nin male and female means",
          xlab = "mean(males) - mean(females)")  + 
  annotate(geom = "curve", x = 4.8, y = 50, xend = 4.8, yend = 5, curvature=0,
           color='red',arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom = "text", x = 4.8, y = 60,
           label = "Observed\ndifference", hjust = "center")

ngreater <- sum(rsample >= 4.8)
ngreater

ratio.var <- function(x1,x2){
  var(x1)/var(x2)
}
ratio.var(male.mandibles,
          female.mandibles)
vsample <- randomization(male.mandibles, female.mandibles, ratio.var, 1000)
quickplot(vsample, bins=20,
          main = "Histogram of randomized ratio \nof male and female variances",
          xlab = "var(male)/var(females)")
mean(vsample)
sum(vsample >= 2.68)
sum(vsample >= 2.68)/1000.

randomF <- rf(1000,9,9)
ggplot() + geom_density(aes(vsample, color='randomization')) +
  geom_density(aes(randomF, color='theoretical')) + 
  labs(x = "Ratio of Variances") +
  scale_color_manual("Type", values = c("steelblue","firebrick"))

# Jackknife
jknife <- function(x,fxn, ci=0.95){
  theta <- fxn(x)
  n <- length(x)
  partials <- rep(0,n)
  
  for (i in 1:n) {
    partials[i] <- fxn(x[-i])
  }
  pseudos <- (n*theta) - (n-1)*partials
  jack.est <- mean(pseudos)
  jack.se <- sqrt(var(pseudos)/n)
  alpha = 1-ci
  CI <- qt(alpha/2,n-1,lower.tail=FALSE)*jack.se
  jack.ci <- c(jack.est - CI, jack.est + CI)
  list(est=jack.est, se=jack.se, ci=jack.ci, pseudos = pseudos, partials=partials)
}
psample <- rpois(25,4) # 25 observations from poisson with lambda = 4
psample
mean(psample)
var(psample)
jack.means <- jknife(psample, mean)
jack.vars <- jknife(psample, var)

jack.means$ci # 95% bootstreap CI for mean
jack.vars$ci # 95% boostrap CI for vars

# create 500 samples of size 25 drawn from Poisson w/lambda=4
psamples <- matrix(rpois(25*500,4),ncol=25,byrow=T)
dim(psamples)
## [1] 500  25

# create a convenience function
get.ci <- function(x) { return(x$ci) }  #x$ci gives confidence interval

# generate jackknife estimates for mean
j.mean <- apply(psamples, 1, jknife, mean)

# make matrix that holds 95% confidence intervals of mean
mean.ci <- t(sapply(j.mean, get.ci))
mean.ci[1,]
mean.ci[2,]

# checking how often true mean is w/in CI
includes.true.mean <- sum(mean.ci[,1] <=4, mean.ci[,2] >= 4)
includes.true.mean

# now the same for variances
j.var <- apply(psamples, 1, jknife, var)
var.ci <- t(sapply(j.var, get.ci))
includes.true.var <- sum(var.ci[,1] <=4, var.ci[,2] >= 4)
includes.true.var
includes.true.var/500

log.var <- function(x){log(var(x))}
j.log.var <- apply(psamples, 1, jknife, log.var)
log.var.ci <- t(sapply(j.log.var, get.ci))
includes.true.var.transformed <- sum(log.var.ci[,1] <=log(4), log.var.ci[,2] >= log(4))

# an improvement in the performance of the 95% CIs
includes.true.var.transformed/500  

# Bootstrap
b <- bootstrap::bootstrap(psample, 1000, var)
str(b)
quickplot(b$thetastar, geom = "histogram", bins=25)
# standard bootstrap confidence limits
bstar <- b$thetastar
ci.multiplier = abs(qt(0.025, df=24)) #cutoff of t-distn w/24 df
c(mean(bstar) - ci.multiplier*sd(bstar),
  mean(bstar) + ci.multiplier*sd(bstar))

quantile(b$thetastar,c(0.025,0.975))

# BUMPUS HOUSE SPARROW DATA SET 

bumpus <- read_tsv("https://github.com/bio304-class/bio304-course-notes/raw/master/datasets/bumpus-data.txt")

# convert TRUE/FALSE values to 1/0 values, to satisfy geom_smooth
bumpus$survived <- as.integer(bumpus$survived) 

# fit the model
fit.survival <- glm(survived ~ weight.g, family = binomial, 
                    data = bumpus)

# draw the regression plot
ggplot(bumpus, aes(x=weight.g, y=survived)) + 
  geom_jitter(width = 0, height = 0.1) +
  geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE) + 
  labs(x = "Weight (g)", y = "Prob. Survival")
# geom_smooth()` using formula = 'y ~ x'

summary(fit.survival)
fit.survival$coefficients
library(boot)
logistic.reg.coeffs <- function(x, indices){
  fit.model <- glm(survived ~ weight.g, family = binomial, x[indices,])
  reg.b0 <- fit.model$coefficients[[1]] #intercept
  reg.b1 <- fit.model$coefficients[[2]] #regression coefficient
  return(c(reg.b0, reg.b1))
}

#generate 500 bootstraps replicates
set.seed(123)
nreps <- 500

# Run bootstrap
reg.boot <- boot(bumpus, logistic.reg.coeffs, nreps)

# Look at first few bootstrap estimates
head(reg.boot$t)
quickplot(reg.boot$t[,1], bins=25, xlab="Regression Intercept")
quickplot(reg.boot$t[,2], bins = 25, xlab="Regression Coefficient")
boot.ci(reg.boot, index = 1, conf = 0.95)

predicted.y <- function(x, coeffs){
  1.0/(1.0 + exp(-(coeffs[1]+coeffs[2]*x)))
}
range(bumpus$weight.g)
nx <- 200 
x <- seq(22, 32, length.out = nx)

# create empty matrix to hold model predictions for each bootstrap sample
predicted.mtx <- matrix(nrow = nreps, ncol = nx)
for (i in 1:nreps) {
  predicted.mtx[i,] <- predicted.y(x, reg.boot$t[i,])
}
# cast the matrix of prediction as a data frame
predicted.mtx.df <- as_tibble(predicted.mtx)
sample.prediction <- predict(fit.survival,
                             data.frame(weight.g = x),
                             type = "response")
quantile.975 <- function(x){ quantile(x, 0.975)}
quantile.025 <- function(x){ quantile(x, 0.025)}

ggplot() + 
  geom_jitter(aes(x=bumpus$weight.g, y=bumpus$survived), 
              width = 0, height = 0.1) +
  geom_line(aes(x = x, y = sample.prediction), color='red') +
  geom_line(aes(x = x, y = map_dbl(predicted.mtx.df, quantile.975))) +
  geom_line(aes(x = x, y = map_dbl(predicted.mtx.df, quantile.025))) +
  labs(x = "Weight (g)", y = "Prob. Survival",
       title="Bumpus Survival Data\nLogistic Regression and Bootstrap 95% CI") +
  lims(x = c(22,32), y = c(0,1))









