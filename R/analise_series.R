set.seed(10000)
library(patchwork)



yt <- arima.sim(n = 3000, list(ma = c(-0.3, -0.5), order = c(0,2,2)))
ts.plot(yt)
acf(yt)
pacf(yt)

dif_yt <- diff(yt)
ts.plot(dif_yt)
acf(dif_yt)
pacf(dif_yt)

dif2_yt <- diff(dif_yt)
ts.plot(dif2_yt)
acf(dif2_yt)
pacf(dif2_yt)

arima(dif2_yt, order = c(0,0,2), include.mean = F)
arima(dif2_yt, order = c(0,0,3), include.mean = F) # ma3 não signif
arima(dif2_yt, order = c(1,0,2), include.mean = F) # ar1 não signif
arima(dif2_yt, order = c(1,0,1), include.mean = F) # ar1 não signif
arima(dif2_yt, order = c(2,0,1), include.mean = F) # ar1 não signif
arima(dif2_yt, order = c(6,0,1), include.mean = F) # ar1 não signif
ts.plot(arima)

arima(yt, order = c(0,2,2))
arima(yt, order = c(0,2,3)) # ma3 não signif
arima(yt, order = c(1,2,2)) # ar1 não signif
arima(yt, order = c(1,2,1)) # ar1 não signif
arima(yt, order = c(2,2,1)) # ar1 não signif
arima(yt, order = c(6,2,1)) # ar1 não signif



questao4 <- arima.sim(n = 3000, order = c(0,0,1),
                      sazonal = c(0,0,1),
                      )


x_t <- arima.sim(n = 3000, list(ar = c(0.7), ma = c(-0.8, 0.5), order = c(1,2,2)))


















ts.plot(x_t)
acf(x_t)
pacf(x_t)

dif_xt <- diff(x_t)
ts.plot(dif_xt)
acf(dif_xt)
pacf(dif_xt)

dif2_xt <- diff(dif_xt)
ts.plot(dif2_xt)
acf(dif2_xt)
pacf(dif2_xt)

dif4_xt <- diff(dif2_xt,2)
ts.plot(dif4_xt)
acf(dif4_xt)
pacf(dif4_xt)

dif5_xt <- diff(dif4_xt,1)
ts.plot(dif5_xt)
acf(dif5_xt)
pacf(dif5_xt)

modelo1 <- arima(dif5_xt, order= c(1,0,1))
modelo2 <- arima(dif5_xt, order= c(2,0,1))
modelo3 <- arima(dif5_xt, order= c(1,0,2))

modelo1
modelo2
modelo3


modelo4<-arima(dif5_xt, order= c(2,0,2))
AIC(modelo1)
AIC(modelo2)
AIC(modelo3)

acf(modelo4$residuals)
pacf(modelo4$residuals)
