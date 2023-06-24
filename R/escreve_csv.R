library(tidyverse)
library(rugarch)
library(yfR)
get_data = function(tickers = "MSFT34.SA", first_date = "2013-01-01",
                    last_date = (Sys.Date())){
  data = yf_get(tickers, first_date, last_date)
  data = na.omit(data[,c(2,8,9)])
  colnames(data) = c("date", "adjusted", "ret_adjusted")
  return(data)
}
dados <- get_data()

spec <- ugarchspec(mean.model = list(armaOrder = c(1,1),
                                    include.mean = FALSE),
                  variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  distribution = "std")
modelo <-  ugarchfit(spec, scale(dados$ret_adjusted, T, T), solver = 'hybrid')

prev <- ugarchforecast(modelo, n.ahead = 5)
prev <- cbind(prev@forecast$seriesFor, prev@forecast$sigmaFor)
rownames(prev) <- NULL
prev <- as.data.frame(prev)
sequencia <- seq(from = max(dados$date)+1, by = 1, length.out = 5)
prev$index <- sequencia
colnames(prev) <- c("previsao","desvio_padrao","data")
modelo <- ugarchfit(spec, dados$ret_adjusted, solver = 'hybrid')@fit
print(prev)

prevs_feitas <- read_csv('dados/previsao.csv')
prevs <- rbind(prevs_feitas,prev)
print(prevs)
write_csv(prevs, "dados/previsao.csv")
