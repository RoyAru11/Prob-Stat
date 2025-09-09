rbinom(n = 1, size = 100, prob = 0.8)
ls()
survivors = rbinom(1000, 100, 0.8)
hist(survivors, col = "skyblue")
hist(rlnorm(1000, 0, 0.1), col = "skyblue")
sample(x=1:10,size=5)
sample(x = c("a", "b", "c"), size = 10, replace = T)
sample(x = c("live", "die"), size = 10, replace = T,
       prob = c(0.8, 0.2))
set.seed(1234)
rnorm(1)
x = rnorm(30, 500, 30)
means = replicate(n = 1000, expr = {
  x_i = sample(x, length(x), replace = T)
mean(x_i)
})
se = function(x) sd(x)/sqrt(length(x))
mean(means); mean(x)
sd(means); se(x)

nt=100
N=NULL
N[1]=1000
for (t in 2:nt) {
  N[t] = (N[t-1]*1.1*rlnorm(1,0,0.1))*(1-0.08)
}
plot(N, type = "l", pch = 15, xlab = "Year", ylab = "Abundance")

pop_sim = function(nt, grow, sd_grow, U, plot = F) {
  N = NULL # empty flexible vector container
  N[1] = 1000
  for (t in 2:nt) {
    N[t] = (N[t-1] * grow * rlnorm(1, 0, sd_grow)) * (1 - U)
  }
  
  if (plot) {
    plot(N, type = "l", pch = 15, xlab = "Year", ylab = "Abundance")
  }
  
  N
}
pop_sim(100, 1.1, 0.1, 0.08, T)
out = replicate(n = 1000, expr = pop_sim(100, 1.1, 0.1, 0.08, F))

N_mean=apply(out, 1, mean)
N_mean[1:10]
N_quants = apply(out, 1, function(x) quantile(x, c(0.1, 0.9)))
plot(N_mean, type ="l", ylim = c(0, 10000))
lines(N_quants[1,], lty = 2)
lines(N_quants[2,], lty = 2)

out10 = ifelse(out[10,] < 1000, "less10", "greater10")
table(out10)
out20 = ifelse(out[20,] < 1100, "less20", "greater20")
table(out20)
table(out10, out20)

mu=500; sig=30
random = rnorm(100, mu, sig)
p = seq(0.01, 0.99, 0.01)
random_q = quantile(random, p)
normal_q = qnorm(p, mu, sig)
plot(normal_q ~ random_q); abline(c(0,1))


q = seq(400, 600, 10)
random_cdf = ecdf(random)
random_p = random_cdf(q)
normal_p = pnorm(q, mu, sig)
plot(normal_p ~ q, type = "l", col = "blue")
points(random_p ~ q, col = "red")

























