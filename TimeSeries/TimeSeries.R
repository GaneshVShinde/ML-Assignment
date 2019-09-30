w=rnorm(200)
par(mfrow=c(1,2))
plot(ts(w))
acf(w)

par(mfrow=c(1,2))
t = seq(0,10,0.1)
sn_t=sin(t)+rnorm(100)
plot(t,sn_t,type='l')
acf(sn_t)
par(mfrow=c(1,2))
w <- rep(1:10, 10)
plot(ts(w))
acf(w)

w = rnorm(500,0,1)
v = filter(w, sides=2, rep(1,3)/5)
par(mfrow=c(1,2))
plot.ts(w)
plot.ts(v)


  births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
  birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
  par(mfrow=c(1,1))
  plot(birthstimeseries)
  acf(birthstimeseries)
  souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
  souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
  
  #decompossing time series 
  library("TTR")
  birthstimeseriescomponents <- decompose(birthstimeseries)
  plot(birthstimeseriescomponents)
  
  #Random Walk simulation 
  x <- w <- rnorm(1000)
  for (t in 2:1000) x[t] <- x[t-1] + w[t]
  plot(x, type="l")
  plot(diff(x))
  
 bd<- diff(birthstimeseries)
 acf(bd)
 
 require('quantmod')
 getSymbols('MSFT', src='yahoo')
 
 
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
