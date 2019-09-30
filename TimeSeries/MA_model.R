births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot(birthstimeseries,type='l')
lines(rollmean(birthstimeseries,5),col="red")
lines(rollmean(birthstimeseries,10),col="blue")
legend(1, 1, legend=c("actual", "ma-5","ma-10"))


## Simulations and Correlogram ######

set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- w[t] + 0.75*w[t-1] 
par(mfrow=c(1,2))
plot(x, type="l")
acf(x)

## prediction##
x.ma <- arima(x, order=c(0, 0, 1))
x.ma

## Simulation of Ma 3 process 
set.seed(3)
x <- w <- rnorm(1000)
for (t in 4:1000) x[t] <- w[t] + 0.6*w[t-1] + 0.4*w[t-2] + 0.3*w[t-3]
par(mfrow=c(1,2))
plot(x, type="l")
acf(x)

## prediction##
x.ma <- arima(x, order=c(0, 0, 3))
x.ma



### AMZN 

require(quantmod)
getSymbols("AMZN")
amznrt = diff(log(Cl(AMZN)))
par(mfrow=c(1,1))

amznrt.ma <- arima(amznrt, order=c(0, 0, 1))
amznrt.ma

acf(amznrt.ma$res[-1])
