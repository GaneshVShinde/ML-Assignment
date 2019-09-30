set.seed(2)
x <- arima.sim(list(order = c(1,1,1), ar = 0.6, ma=-0.5), n = 1000)
plot(x)

x.arima <- arima(x, order=c(1, 1, 1))

acf(resid(x.arima))

Box.test(resid(x.arima), lag=20, type="Ljung-Box")

library(forecast)

require(quantmod)
getSymbols("AMZN", from="2013-01-01")
amzn = log(Cl(AMZN))


azfinal.aic <- Inf
azfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
     azcurrent.aic <- AIC(arima(amzn, order=c(p, d, q)))
     if (azcurrent.aic < azfinal.aic) {
         azfinal.aic <- azcurrent.aic
         azfinal.order <- c(p, d, q)
         azfinal.arima <- arima(amzn, order=azfinal.order)
       }
}


acf(resid(azfinal.arima), na.action=na.omit)

Box.test(resid(azfinal.arima), lag=29, type="Ljung-Box")

plot(forecast(azfinal.arima, h=25))
