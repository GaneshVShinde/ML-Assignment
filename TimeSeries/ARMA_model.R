set.seed(1)
x <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25), ma=c(0.5, -0.3)))
plot(x)

acf(x)


require(quantmod)
getSymbols("^GSPC")
sp = diff(log(Cl(GSPC)))

spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
     spcurrent.aic <- AIC(arima(sp, order=c(i, 0, j)))
     if (spcurrent.aic < spfinal.aic) {
         spfinal.aic <- spcurrent.aic
         spfinal.order <- c(i, 0, j)
         spfinal.arma <- arima(sp, order=spfinal.order)
       }
}

spfinal.order

acf(resid(spfinal.arma), na.action=na.omit)
