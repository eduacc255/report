# 例3.1
ch = function(la) {
  integrate(function(x) {x^(la - 1) * exp(-x)}, 0, Inf)$val
}
plot(lgamma(seq(0.01, 10, le=100)),
     log(apply(as.matrix(seq(0.01, 10, le=100)), 1, ch)),
     xlab = "log(integrate(f))",
     ylab = expression(log(Gamma(lambda)), pch=19, cex=0.6)
)


# 例3.2
cac = rcauchy(10) + 350
lik = function(the) {
  u = dcauchy(cac[1] - the)
  for(i in 2:10)
    u = u*dcauchy(cac[i] - the)
    return(u)
}
integrate(lik, -Inf, Inf)
integrate(lik, 200, 400)

cac = rcauchy(10)
nin = function(a) {integrate(lik, -a, a)$val}
nan = function(a) {area(lik, -a, a)}
x = seq(1, 10^3)
y = log(apply(as.matrix(x), 1, nin))
z = log(apply(as.matrix(x), 1, nan))
plot(x, y, type="l", ylim=range(cbind(y,z)), lwd=2)
lines(x, z, lty=2, col="sienna", lwd=2)
title(main = "タイトルのテスト")