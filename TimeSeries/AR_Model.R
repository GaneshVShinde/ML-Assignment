# AR 1 process
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 0.6*x[t-1] + w[t]

par(mfrow=c(1,2))
plot(x, type="l")
acf(x)

# AR 1 models predition
x.ar <- ar(x, method = "mle")
print(x.ar$order)
print(x.ar$ar)


# Amazon stock prize data ar model
require(quantmod)
getSymbols("AMZN")
AMZN

# plot amazon stock
par(mfrow=c(1,1))
plot(Cl(AMZN))

# plot amazon 
amznrt = diff(log(Cl(AMZN)))
plot(amznrt)


acf(amznrt, na.action=na.omit)

amznrt.ar <- ar(amznrt, na.action=na.omit)
amznrt.ar$order
amznrt.ar$ar

# S&P 
getSymbols("^GSPC")
GSPC

#plot S&P
plot(Cl(GSPC))

gspcrt = diff(log(Cl(GSPC)))
plot(gspcrt)

acf(gspcrt, na.action=na.omit)

gspcrt.ar <- ar(gspcrt, na.action=na.omit)
gspcrt.ar$order