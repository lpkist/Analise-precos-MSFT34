library(yfR)
library(rugarch)
library(tidyverse)
library(fpp2)
library(GAS)

get_data = function(tickers = "MSFT34.SA", first_date = "2013-01-01",
                    last_date = (Sys.Date())){
  data = yf_get(tickers, first_date, last_date)
  data = na.omit(data[,c(2,8,9)])
  colnames(data) = c("date", "adjusted", "ret_adjusted")
  return(data)
}

tendencia_e_saz <- function(dados, y, p, n_saz, h){
  # modelo de regress?o com sazonalidade (pode ter tend?ncia tamb?m)
  dados["y"] <- dados[y]
  dados["t"] <- 1:nrow(dados)
  if(nrow(dados)%%n_saz == 0){
    saz_aux <- rep(1:n_saz, nrow(dados)%/%n_saz)
  }else{
    saz_aux <-c(rep(1:n_saz, nrow(dados)%/%n_saz),1:(nrow(dados)%%n_saz))
  }
  dados["saz"] <- saz_aux
  if(p==0){
    modelo <- "y ~ 1"
  }else{
    modelo <- "y ~ t"
  }
  if(p>1){
    for(i in 1:p){
      if(i ==1){next}
      modelo <- paste0(modelo, "+ I(t^",i,")")
    }
  }
  modelo <- paste0(modelo,"+ I(factor(saz))")
  modelo_adj <- lm(as.formula(modelo), dados)
  ultima_saz <- I(dados[nrow(dados), "saz"])[[1]]
  pred_saz <- rep(ultima_saz,h)+1:h
  pred_saz <- data.frame("saz" = matrix(pred_saz, nrow=5))
  pred_saz <- pred_saz %>% mutate("saz" = saz%%n_saz+1)
  predizer <- rep(nrow(dados),h)
  predizer <- predizer+1:h
  df_predizer <- data.frame(t = predizer, "saz" = pred_saz)
  colnames(df_predizer) <- c("t", "saz")
  return(list(data.frame("y" = predict(modelo_adj,df_predizer)), modelo_adj))
}


dados <- get_data()
ts.plot(dados$adjusted)
acf(dados$adjusted) # tem tend?ncia, mas n?o tem sazonalidade
pacf(dados$adjusted)
acf(dados$adjusted^2)

# ret adjusted ? uma transforma??o da s?rie diferenciada
ts.plot(dados$ret_adjusted)
acf(dados$ret_adjusted) # n?o tem tend?ncia nem sazonalidade
# parece ter autocorrela??o para modelar
pacf(dados$ret_adjusted)  # o primeiro lag est? fora (tem que modelar)
acf(dados$ret_adjusted^2) # vari?ncia n?o ? constante, tem que modelar

# com base nessa an?lise explorat?ria inicial, provavelmente
# um modelo ARMA-GARCH nos retornos deve ser o mais adequado
# entretanto, ser?o ajustados diversos outros modelos vistos em aula


# considerando que os dados s?o coletados 5x por semana, ser?o considerados
# per?odos sazonais de 5, 10 e 20 dias, quando for poss?vel

########## Decomposi??es ########
######### s?rie de retornos
### decomposi??o aditiva
dec_ad5_r <- decompose(ts(dados$ret_adjusted, frequency = 5)) # s?o 5 dias na semana
plot(dec_ad5_r)
acf(na.omit(dec_ad5_r$random)) # n?o se ajustou bem

dec_ad10_r <- decompose(ts(dados$ret_adjusted, frequency = 10))
plot(dec_ad10_r)
acf(na.omit(dec_ad10_r$random)) # n?o se ajustou bem


dec_ad20_r <- decompose(ts(dados$ret_adjusted, frequency = 20)) # s?o aproximadamente 20 dias ?teis na semana
plot(dec_ad20_r)
acf(na.omit(dec_ad5_r$random)) # n?o se ajustou bem
## note que ? n?tido o fato de a decomposi??o aditiva n?o modelar acf

### decomposi??o multiplicativa
dec_m10_r <- decompose(ts(dados$ret_adjusted, frequency = 10), type = "multiplicative")
plot(dec_m10_r)
acf(na.omit(dec_m10_r$random))
pacf(na.omit(dec_m10_r$random))
acf(na.omit(dec_m10_r$random)^2) # parece ter se ajustado bem aos dados

dec_m20_r <- decompose(ts(dados$ret_adjusted, frequency = 20), type = "multiplicative") # s?o aproximadamente 20 dias ?teis na semana
plot(dec_m20_r)
acf(na.omit(dec_m20_r$random)) # n?o se ajustou bem

# a decomposi??o multiplicativa com par?metros iguais a 10 parece ter se ajustado
# bem aos pre?os de retorno


######### s?rie de fechamento
### decomposi??o aditiva
dec_ad5_f <- decompose(ts(dados$adjusted, frequency = 5))
plot(dec_ad5_f)
acf(na.omit(dec_ad5_f$random)) # n?o se ajustou bem

dec_ad10_f <- decompose(ts(dados$adjusted, frequency = 10))
plot(dec_ad10_f)
acf(na.omit(dec_ad10_f$random)) # n?o se ajustou bem


dec_ad20_f <- decompose(ts(dados$adjusted, frequency = 20))
plot(dec_ad20_f)
acf(na.omit(dec_ad5_f$random)) # n?o se ajustou bem
## note que ? n?tido o fato de a decomposi??o aditiva n?o modelar acf

### decomposi??o multiplicativa
dec_m10_f <- decompose(ts(dados$adjusted, frequency = 10), type = "multiplicative")
plot(dec_m10_f)
acf(na.omit(dec_m10_f$random)) # n?o se ajustou bem

dec_m20_f <- decompose(ts(dados$adjusted, frequency = 20), type = "multiplicative")
plot(dec_m20_f)
acf(na.omit(dec_m20_f$random)) # n?o se ajustou bem

### nenhum parece ter se ajustado bem


########## Modelos b?sicos ########
######### s?rie de fechamento
### m?dia
mean_f <- dados$adjusted - mean(dados$adjusted)
acf(mean_f) # n?o se ajustou bem e tem tend?ncia

### naive
naive_f <- dados$adjusted - lag(dados$adjusted,1)
acf(na.omit(naive_f)) # n?o se ajustou bem
pacf(na.omit(naive_f)) # n?o se ajustou bem

### naive sazonal
snaive5_f <- dados$adjusted - lag(dados$adjusted,5)
acf(na.omit(snaive5_f)) # n?o se ajustou bem

snaive10_f <- dados$adjusted - lag(dados$adjusted,10)
acf(na.omit(snaive10_f)) # n?o se ajustou bem

snaive20_f <- dados$adjusted - lag(dados$adjusted,20)
acf(na.omit(snaive20_f)) # n?o se ajustou bem

### drift
y_T <- dados$ret_adjusted[nrow(dados)]
slope <- I((y_T - dados$ret_adjusted[1])/(nrow(dados)-1))[[1]]
dados$idx <- 1:nrow(dados)
drift_f <- data.frame("residuals" =
                       dados$adjusted-
                       (dados$ret_adjusted[1])+slope*(dados$idx-1)) %>%
  na.omit()
acf(drift_f$residuals) # n?o se ajustou bem


######### s?rie de retornos
### m?dia
mean_r <- dados$ret_adjusted - mean(dados$ret_adjusted)
acf(mean_r) # n?o se ajustou bem

### naive
naive_r <- dados$ret_adjusted - lag(dados$ret_adjusted,1)
acf(na.omit(naive_r)) # n?o se ajustou bem

### naive sazonal
snaive5_r <- dados$ret_adjusted - lag(dados$ret_adjusted,5)
acf(na.omit(snaive5_r)) # n?o se ajustou bem

snaive10_r <- dados$ret_adjusted - lag(dados$ret_adjusted,10)
acf(na.omit(snaive10_r)) # n?o se ajustou bem

snaive20_r <- dados$ret_adjusted - lag(dados$ret_adjusted,20)
acf(na.omit(snaive20_r)) # n?o se ajustou bem

### drift
y_T <- dados$ret_adjusted[nrow(dados)]
slope <- I((y_T - dados$ret_adjusted[1])/(nrow(dados)-1))[[1]]
dados$idx <- 1:nrow(dados)
drift_r <- data.frame("residuals" =
                        dados$ret_adjusted-
                        (dados$ret_adjusted[1])+slope*(dados$idx-1)) %>%
  na.omit()
acf(drift_r$residuals) # n?o se ajustou bem
pacf(drift_r$residuals) # n?o se ajustou bem



########## Modelos de regress?o ########
######### s?rie de fechamento
### tend?ncia
rls1_f <- lm(adjusted ~ idx, dados)
acf(rls1_f$residuals) # n?o se ajustou bem e tem tend?ncia

rls2_f <- lm(adjusted ~ idx + I(idx^2), dados)
acf(rls2_f$residuals) # n?o se ajustou bem e tem tend?ncia

### sazonalidade
rls_s5_f <- tendencia_e_saz(dados, "adjusted", 0, 5, 1)
acf(rls_s5_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls_s10_f <- tendencia_e_saz(dados, "adjusted", 0, 10, 1)
acf(rls_s10_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls_s20_f <- tendencia_e_saz(dados, "adjusted", 0, 20, 1)
acf(rls_s20_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

### tend?ncia e sazonalidade
rls1_s5_f <- tendencia_e_saz(dados, "adjusted", 1, 5, 1)
acf(rls1_s5_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls1_s10_f <- tendencia_e_saz(dados, "adjusted", 1, 10, 1)
acf(rls1_s10_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls1_s20_f <- tendencia_e_saz(dados, "adjusted", 1, 20, 1)
acf(rls1_s20_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls2_s5_f <- tendencia_e_saz(dados, "adjusted", 2, 5, 1)
acf(rls2_s5_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls2_s10_f <- tendencia_e_saz(dados, "adjusted", 2, 10, 1)
acf(rls2_s10_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

rls2_s20_f <- tendencia_e_saz(dados, "adjusted", 2, 20, 1)
acf(rls2_s20_f[[2]]$residuals) # n?o se ajustou bem e tem tend?ncia

######### s?rie de retornos
### tend?ncia
rls1_r <- lm(ret_adjusted ~ idx, dados)
acf(rls1_r$residuals) # n?o se ajustou bem

rls2_r <- lm(ret_adjusted ~ idx + I(idx^2), dados)
acf(rls2_r$residuals) # n?o se ajustou bem

### sazonalidade
rls_s5_r <- tendencia_e_saz(dados, "ret_adjusted", 0, 5, 1)
acf(rls_s5_r[[2]]$residuals) # n?o se ajustou bem

rls_s10_r <- tendencia_e_saz(dados, "ret_adjusted", 0, 10, 1)
acf(rls_s10_r[[2]]$residuals) # n?o se ajustou bem

rls_s20_r <- tendencia_e_saz(dados, "ret_adjusted", 0, 20, 1)
acf(rls_s20_r[[2]]$residuals) # n?o se ajustou bem

### tend?ncia e sazonalidade
rls1_s5_r <- tendencia_e_saz(dados, "ret_adjusted", 1, 5, 1)
acf(rls1_s5_r[[2]]$residuals) # n?o se ajustou bem

rls1_s10_r <- tendencia_e_saz(dados, "ret_adjusted", 1, 10, 1)
acf(rls1_s10_r[[2]]$residuals) # n?o se ajustou bem

rls1_s20_r <- tendencia_e_saz(dados, "ret_adjusted", 1, 20, 1)
acf(rls1_s20_r[[2]]$residuals) # n?o se ajustou bem

rls2_s5_r <- tendencia_e_saz(dados, "ret_adjusted", 2, 5, 1)
acf(rls2_s5_r[[2]]$residuals) # n?o se ajustou bem

rls2_s10_r <- tendencia_e_saz(dados, "ret_adjusted", 2, 10, 1)
acf(rls2_s10_r[[2]]$residuals) # n?o se ajustou bem

rls2_s20_r <- tendencia_e_saz(dados, "ret_adjusted", 2, 20, 1)
acf(rls2_s20_r[[2]]$residuals) # n?o se ajustou bem

########## Suaviza??es exponenciais ########
######### s?rie de fechamento
se_f_ANN <- ets(ts(dados$adjusted), model = "ANN")
acf(se_f_ANN$residuals)
pacf(se_f_ANN$residuals) # n?o se ajustou bem

se_f_AAN <- ets(ts(dados$adjusted), model = "AAN")
acf(se_f_AAN$residuals)
pacf(se_f_AAN$residuals) # n?o se ajustou bem

se_f_MAN <- ets(ts(dados$adjusted), model = "MAN")
acf(se_f_MAN$residuals)
pacf(se_f_MAN$residuals)
acf(se_f_MAN$residuals^2) # n?o se ajustou bem

se_f_MMN <- ets(ts(dados$adjusted), model = "MMN")
acf(se_f_MMN$residuals)
pacf(se_f_MMN$residuals)
acf(se_f_MMN$residuals^2) # n?o se ajustou bem

### sazonalidade 5

se5_f_ANA <- ets(ts(dados$adjusted, frequency = 5), model = "ANA")
acf(se5_f_ANA$residuals)
pacf(se5_f_ANA$residuals) # n?o se ajustou bem

se5_f_AAA <- ets(ts(dados$adjusted, frequency = 5), model = "AAA")
acf(se5_f_AAA$residuals)
pacf(se5_f_AAA$residuals) # n?o se ajustou bem

se5_f_MAA <- ets(ts(dados$adjusted, frequency = 5), model = "MAA")
acf(se5_f_MAA$residuals)
pacf(se5_f_MAA$residuals)
acf(se5_f_MAA$residuals^2) # n?o se ajustou bem

se5_f_MAM <- ets(ts(dados$adjusted, frequency = 5), model = "MAM")
acf(se5_f_MAM$residuals)
pacf(se5_f_MAM$residuals) # n?o se ajustou bem

se5_f_MMM <- ets(ts(dados$adjusted, frequency = 5), model = "MMM")
acf(se5_f_MMM$residuals)
pacf(se5_f_MMM$residuals)
acf(se5_f_MMM$residuals^2) # n?o se ajustou bem

### sazonalidade 10

se10_f_ANA <- ets(ts(dados$adjusted, frequency = 10), model = "ANA")
acf(se10_f_ANA$residuals)
pacf(se10_f_ANA$residuals) # n?o se ajustou bem

se10_f_AAA <- ets(ts(dados$adjusted, frequency = 10), model = "AAA")
acf(se10_f_AAA$residuals)
pacf(se10_f_AAA$residuals) # n?o se ajustou bem

se10_f_MAA <- ets(ts(dados$adjusted, frequency = 10), model = "MAA")
acf(se10_f_MAA$residuals)
pacf(se10_f_MAA$residuals)
acf(se10_f_MAA$residuals^2) # n?o se ajustou bem

se10_f_MAM <- ets(ts(dados$adjusted, frequency = 10), model = "MAM")
acf(se10_f_MAM$residuals) # n?o se ajustou bem

se10_f_MMM <- ets(ts(dados$adjusted, frequency = 10), model = "MMM")
acf(se10_f_MMM$residuals)
pacf(se10_f_MMM$residuals)
acf(se10_f_MMM$residuals^2) # n?o se ajustou bem



### sazonalidade 20

se20_f_ANA <- ets(ts(dados$adjusted, frequency = 20), model = "ANA")
acf(se20_f_ANA$residuals)
pacf(se20_f_ANA$residuals) # n?o se ajustou bem

se20_f_AAA <- ets(ts(dados$adjusted, frequency = 20), model = "AAA")
acf(se20_f_AAA$residuals)
pacf(se20_f_AAA$residuals) # n?o se ajustou bem

se20_f_MAA <- ets(ts(dados$adjusted, frequency = 20), model = "MAA")
acf(se20_f_MAA$residuals)
pacf(se20_f_MAA$residuals)
acf(se20_f_MAA$residuals^2) # n?o se ajustou bem

se20_f_MAM <- ets(ts(dados$adjusted, frequency = 20), model = "MAM")
acf(se20_f_MAM$residuals) # n?o se ajustou bem

se20_f_MMM <- ets(ts(dados$adjusted, frequency = 20), model = "MMM")
acf(se20_f_MMM$residuals)
pacf(se20_f_MMM$residuals) # n?o se ajustou bem


######### s?rie de retornos
se_r_ANN <- ets(ts(dados$ret_adjusted), model = "ANN")
acf(se_r_ANN$residuals) # n?o se ajustou bem

se_r_AAN <- ets(ts(dados$ret_adjusted), model = "AAN")
acf(se_r_AAN$residuals) # n?o se ajustou bem

### sazonalidade 5

se5_r_ANA <- ets(ts(dados$ret_adjusted, frequency = 5), model = "ANA")
acf(se5_r_ANA$residuals) # n?o se ajustou bem

se5_r_AAA <- ets(ts(dados$ret_adjusted, frequency = 5), model = "AAA")
acf(se5_r_AAA$residuals) # n?o se ajustou bem

### sazonalidade 10

se10_r_ANA <- ets(ts(dados$ret_adjusted, frequency = 10), model = "ANA")
acf(se10_r_ANA$residuals) # n?o se ajustou bem

se10_r_AAA <- ets(ts(dados$ret_adjusted, frequency = 10), model = "AAA")
acf(se10_r_AAA$residuals) # n?o se ajustou bem

### sazonalidade 20

se20_r_ANA <- ets(ts(dados$ret_adjusted, frequency = 20), model = "ANA")
acf(se20_r_ANA$residuals) # n?o se ajustou bem

se20_r_AAA <- ets(ts(dados$ret_adjusted, frequency = 20), model = "AAA")
acf(se20_r_AAA$residuals) # n?o se ajustou bem

########## MODELOS DA FAM?LIA ARMA ########
######### S?rie de retornos
# como o pre?o de fechamento possui tend?ncia, seria necess?rio diferenci?-la
# uma vez para modelar um arima
# por isso, trabalharemos apenas com a s?rie de retornos

acf(dados$ret_adjusted) # apenas um fora
pacf(dados$ret_adjusted) # apenas um fora

arma01 <- arima(dados$ret_adjusted, order = c(0,0,1))
acf(arma01$residuals)
pacf(arma01$residuals)
acf(arma01$residuals^2) # n?o se ajustou bem

arma10 <- arima(dados$ret_adjusted, order = c(1,0,0))
acf(arma10$residuals)
pacf(arma10$residuals)
acf(arma10$residuals^2) # n?o se ajustou bem

# ambos os modelos captaram as autocorrela??es com o passado, mas n?o s?o
# capazes de captar as autocorrela??es nos quadrados

########## MODELOS DA FAM?LIA ARMA-GARCH ########
######### como omega ? necess?rio no modelo, n?o ser?o tecidos coment?rio sobre ele
######### S?rie de retornos
### distribui??o normal
spec1011 <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                         include.mean = TRUE),
                       variance.model = list(model = 'sGARCH',
                                             garchOrder = c(1,1)),
                       distribution.model = "norm")
garch1011 <-  ugarchfit(spec1011, dados$ret_adjusted, solver = 'hybrid')
garch1011
# sobre os par?metros: todos s?o significativos
# nenhuma acf rejeita H0
# nenhuma acf^2 rejeita H0
plot(garch1011)
# se digitar 9, vai ver que os res?duos n?o s?o normais e ? preciso trocar
# de distribui??o
spec0111 <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                         include.mean = TRUE),
                       variance.model = list(model = 'sGARCH',
                                             garchOrder = c(1,1)),
                       distribution.model = "norm")
garch0111 <-  ugarchfit(spec0111, dados$ret_adjusted, solver = 'hybrid')
garch0111 # sobre os par?metros: todos s?o significativos
# nenhuma acf rejeita H0
# nenhuma acf^2 rejeita H0
plot(garch0111)
# se digitar 9, vai ver que os res?duos n?o s?o normais e ? preciso trocar
# de distribui??o

### distribui??o t
## ar(1)-garch
spec1011t <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                         include.mean = TRUE),
                       variance.model = list(model = 'sGARCH',
                                             garchOrder = c(1,1)),
                       distribution.model = "std")
garch1011t <-  ugarchfit(spec1011t, dados$ret_adjusted, solver = 'hybrid')
garch1011t
# sobre os par?metros: todos s?o significativos
# nenhuma acf rejeita H0
# uma acf^2 rejeita H0
# o modelo parece n?o ter se ajustado bem
plot(garch1011t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que a primeira acf est? fora
# o modelo n?o se ajustou bem


spec1021t <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(2,1)),
                        distribution.model = "std")
garch1021t <-  ugarchfit(spec1021t, dados$ret_adjusted, solver = 'hybrid')
garch1021t
# sobre os par?metros: nenhum alpha ? significativo
# nenhuma acf rejeita H0
# uma acf^2 rejeita H0
# o modelo parece n?o ter se ajustado bem
plot(garch1021t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que a primeira acf est? fora
# o modelo n?o se ajustou bem


spec1012t <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(1,2)),
                        distribution.model = "std")
garch1012t <-  ugarchfit(spec1012t, dados$ret_adjusted, solver = 'hybrid')
garch1012t
# sobre os par?metros: todos os par?metros s?o significativos
# nenhuma acf rejeita H0
# nenhuma acf^2 rejeita H0
# o modelo parece ter se ajustado bem
plot(garch1012t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que a d?cima acf est? um pouco fora, mas n?o ? um problema
# o modelo se ajustou bem

spec1002t <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(0,2)),
                        distribution.model = "std")
garch1002t <-  ugarchfit(spec1002t, dados$ret_adjusted, solver = 'hybrid')
garch1002t
# sobre os par?metros: o beta1 n?o ? significativo
# as acfs n?o est?o boas
# as acfs^2 n?o est?o boas
# o modelo parece n?o ter se ajustado bem
plot(garch1002t)
# se digitar 9, vai ver que os res?duos n?o t?m distribui??o t
# se digitar 10, vai ver que a primeira acf acfs est? fora
# se digitar 11, vai ver que apenas a v?rias acfs^2 est?o fora
# o modelo n?o se ajustou bem



## ma(1)-garch
spec0111t <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(1,1)),
                        distribution.model = "std")
garch0111t <-  ugarchfit(spec0111t, dados$ret_adjusted, solver = 'hybrid')
garch0111t
# sobre os par?metros: todos s?o significativos
# nenhuma acf rejeita H0
# uma acf^2 rejeita H0
# o modelo parece n?o ter se ajustado bem
plot(garch1011t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que a primeira acf est? fora
# o modelo n?o se ajustou bem

spec0121t <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(2,1)),
                        distribution.model = "std")
garch0121t <-  ugarchfit(spec0121t, dados$ret_adjusted, solver = 'hybrid')
garch0121t
# sobre os par?metros: nenhum alpha ? significativo
# nenhuma acf rejeita H0
# uma acf^2 rejeita H0
# o modelo parece n?o ter se ajustado bem
plot(garch0121t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que a primeira acf est? fora
# o modelo n?o se ajustou bem


spec0112t <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(1,2)),
                        distribution.model = "std")
garch0112t <-  ugarchfit(spec0112t, dados$ret_adjusted, solver = 'hybrid')
garch0112t
# sobre os par?metros: nenhum alpha ? significativo
# nenhuma acf rejeita H0
# nenhuma acf^2 rejeita H0
# o modelo parece ter se ajustado bem
plot(garch0112t)
# se digitar 9, vai ver que os res?duos tem aproximadamente distribui??o t
# se digitar 10, vai ver que todas as acfs est?o boas
# se digitar 11, vai ver que apenas a d?cima acf est? fora, mas n?o ? um problema
# o modelo se ajustou bem


spec0102t <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(0,2)),
                        distribution.model = "std")
garch0102t <-  ugarchfit(spec0102t, dados$ret_adjusted, solver = 'hybrid')
garch0102t
# sobre os par?metros: o beta1 n?o ? significativo
# as acfs n?o est?o boas
# as acfs^2 n?o est?o boas
# o modelo parece n?o ter se ajustado bem
plot(garch0102t)
# se digitar 9, vai ver que os res?duos n?o t?m distribui??o t
# se digitar 10, vai ver que a primeira acf acfs est? fora
# se digitar 11, vai ver que apenas a v?rias acfs^2 est?o fora
# o modelo n?o se ajustou bem




##################### CONCLUS?ES ##############################
# a partir dos modelos ajustados, nota-se que apenas tr?s deles se ajustaram
# bem aos dados de retornos: decomposi??o multiplicativa com par?metros iguais
# a 10, # AR(1)-GARCH(1,2) e MA(1)-GARCH(1,2)

# a fim de compar?-los e descobrir qual reproduz melhor os dados, ser?
# realizada uma cross-validation para compar?-los em rela??o ? m?dia da previs?o,
# value at risk e expected shortfall

n <- nrow(dados)
window <- 5*365 # 3 anos
n_cv <- n - window
retornos <- dados$ret_adjusted

media_magarch <- list()
var_magarch <- list() #value at risk
es_magarch <- list() #expected shortfall

media_argarch <- list()
var_argarch <- list() #value at risk
es_argarch <- list() #expected shortfall

media_decm <- list()
var_decm <- list() #value at risk
es_decm <- list() #expected shortfall

alpha <- 0.01
esp_t <- function(x, mu, sigma, shape) {
  x*ddist(distribution = "std",
          y = x,
          mu = mu,
          sigma = sigma,
          skew = 0,
          shape = shape)
}

spec0112t <- ugarchspec(mean.model = list(armaOrder = c(0,1),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(1,2)),
                        distribution.model = "std")

spec1012t <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                          include.mean = TRUE),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(1,2)),
                        distribution.model = "std")

# ? necess?rio prever a tend?ncia para calcular a previs?o da decomposi??o
# multiplicativa
dec_m10_r <- decompose(ts(dados$ret_adjusted, frequency = 10), type = "multiplicative")
# assim, vamos model?-la
# foram ajustados v?rios modelos e chegou-se ao seguinte:
spectrend1211 <- ugarchspec(mean.model = list(armaOrder = c(1,2),
                                              include.mean = TRUE),
                            variance.model = list(model = 'sGARCH',
                                                  garchOrder = c(1,1)),
                            distribution.model = "std")
trend1211 <- ugarchfit(spectrend1211, na.omit(dec_m10_r$trend), solver = 'hybrid')
plot(trend1211)
trend1211
# considerando que a sazonalidade ? de 10 dias, esse modelo se ajustou muito bem


Sys.time()
for (i in 1:n_cv) {
  if(i%%50 == 0) print(Sys.time())
  ret <- retornos[i:(window + i - 1)]
  magarch <- ugarchfit(spec0112t, ret, solver = 'hybrid')
  prev_magarch <- ugarchforecast(magarch, n.ahead = 1)
  media_magarch[[i]] <- prev_magarch@forecast[["seriesFor"]][1]
  var_ma <- qdist(distribution = "std", alpha,
                  mu = prev_magarch@forecast[["seriesFor"]][1],
                  sigma = sigma(prev_magarch)[1],
                  skew = 0, shape = coef(magarch)["shape"])
  var_magarch[[i]] <- var_ma
  es_magarch[[i]] <- integrate(esp_t,
                     -Inf,
                     var_ma,
                     mu = prev_magarch@forecast[["seriesFor"]][1],
                     sigma = sigma(prev_magarch)[1],
                     shape = coef(magarch)["shape"],
                     rel.tol = 1e-7)$value/alpha

  argarch <- ugarchfit(spec1012t, ret, solver = 'hybrid')
  prev_argarch <- ugarchforecast(argarch, n.ahead = 1)
  media_argarch[[i]] <- prev_argarch@forecast[["seriesFor"]][1]
  var_ar <- qdist(distribution = "std", alpha,
                  mu = prev_argarch@forecast[["seriesFor"]][1],
                  sigma = sigma(prev_argarch)[1],
                  skew = 0, shape = coef(argarch)["shape"])
  var_argarch[[i]] <- var_ar
  es_argarch[[i]] <- integrate(esp_t,
                               -Inf,
                               var_ar,
                               mu = prev_argarch@forecast[["seriesFor"]][1],
                               sigma = sigma(prev_argarch)[1],
                               shape = coef(argarch)["shape"],
                               rel.tol = 1e-7)$value/alpha

  decm <- decompose(ts(ret, frequency = 10), type = "multiplicative")
  trend_decm <- ugarchfit(spectrend1211, na.omit(decm$trend), solver = 'hybrid')
  prev_trend <- ugarchforecast(trend_decm, n.ahead = 1)
  prev_decm <- decm$seasonal[window]*prev_trend@forecast[["seriesFor"]][1]
  media_decm[[i]] <- prev_decm
  var_dec <- qdist(distribution = "std", alpha,
                  mu = prev_decm,
                  sigma = decm$seasonal[window]^2*sigma(prev_trend)[1],
                  skew = 0, shape = coef(trend_decm)["shape"])
  var_decm[[i]] <- var_dec
  es_decm[[i]] <- integrate(esp_t,
                            -Inf,
                            var_dec,
                            mu = prev_decm,
                            sigma = decm$seasonal[window]^2*sigma(prev_trend)[1],
                            shape = coef(trend_decm)["shape"],
                            rel.tol = 1e-7)$value/alpha

}
Sys.time()
rets <- retornos[(window + 1):n]
var_magarch <- do.call(rbind,var_magarch)
var_argarch <- do.call(rbind,var_argarch)
var_decm <- do.call(rbind,var_decm)

es_magarch <- do.call(rbind,es_magarch)
es_argarch <- do.call(rbind,es_argarch)
es_decm <- do.call(rbind,es_decm)

media_magarch <- do.call(rbind,media_magarch)
media_argarch <- do.call(rbind,media_argarch)
media_decm <- do.call(rbind,media_decm)



# m?dia
resumo <- data.frame("magarch" = media_magarch, "argarch" = media_argarch,
                    "decm" = media_decm, "retorno" = rets) %>%
  pivot_longer(cols = 1:3, names_to = "modelo", values_to = "predito") %>%
  mutate(erro = abs(retorno - predito)) %>%
  group_by(modelo) %>%
  summarise(MAE = mean(erro), REQM = sqrt(mean(erro^2)))

round(resumo$MAE,6)
round(resumo$REQM,6)

garch0112t <-  ugarchfit(spec0112t, dados$ret_adjusted, solver = 'hybrid')
garch1012t <-  ugarchfit(spec1012t, dados$ret_adjusted, solver = 'hybrid')

infocriteria(garch0112t)
infocriteria(garch1012t)


# Backtests (GARCH)
var_ma <- BacktestVaR(rets, var_magarch, alpha = 0.01, Lags = 4)
var_ma[1:4] # o modelo se ajustou aos dados

var_ar <- BacktestVaR(rets, var_argarch, alpha = 0.01, Lags = 4)
var_ar[1:4] # o modelo se ajustou aos dados

var_dec <- BacktestVaR(rets, var_decm, alpha = 0.01, Lags = 4)
var_dec[1:4] # o modelo n?o se ajustou aos dados



es_ma <- ESTest(alpha = 0.01, rets, es_magarch, var_magarch, conf.level = 0.95,  boot = FALSE)
es_ma # o modelo se ajustou aos dados

es_ar <- ESTest(alpha = 0.01, rets, es_argarch, var_argarch, conf.level = 0.95,  boot = FALSE)
es_ar # o modelo se ajustou aos dados

es_decm <- ESTest(alpha = 0.01, rets, es_decm, var_decm, conf.level = 0.95,  boot = FALSE)
es_decm # o modelo n?o se ajustou aos dados
