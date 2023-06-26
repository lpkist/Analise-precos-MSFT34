library(tidyverse)
library(rugarch)
library(yfR)
get_day <- function() {
                        data <- as.Date(Sys.time())
                        dia_da_semana <- (as.numeric(format(data, "%u")) %% 7) + 1
                        return(dia_da_semana)
                      }
dia_da_semana <- get_day()

get_data = function(tickers = "MSFT34.SA", first_date = "2013-01-01",
                    last_date = (Sys.Date())){
  data = yf_get(tickers, first_date, last_date)
  data = na.omit(data[,c(2,8,9)])
  colnames(data) = c("date", "adjusted", "ret_adjusted")
  return(data)
}
dados <- get_data()

spec <- ugarchspec(mean.model = list(armaOrder = c(1,0),
                                    include.mean = FALSE),
                  variance.model = list(model = 'sGARCH', garchOrder = c(1,2)),
                  distribution = "std")
modelo <-  ugarchfit(spec, scale(dados$ret_adjusted, T, T), solver = 'hybrid')

prev <- ugarchforecast(modelo, n.ahead = 1)
alpha <- 0.01
esp_t <- function(x, mu, sigma, shape) {
  x*ddist(distribution = "std", 
          y = x,
          mu = mu, 
          sigma = sigma, 
          skew = 0,
          shape = shape)
}
var <- qdist(distribution = "std", alpha,
                  mu = prev@forecast[["seriesFor"]][1],
                  sigma = sigma(prev)[1],
                  skew = 0, shape = coef(modelo)["shape"])
es <- integrate(esp_t, 
                     -Inf, 
                     var, 
                     mu = prev@forecast[["seriesFor"]][1],
                     sigma = sigma(prev)[1],
                     shape = coef(modelo)["shape"])$value/alpha
prev <- cbind(prev@forecast$seriesFor[1], var, es)
rownames(prev) <- NULL
prev <- as.data.frame(prev)
sequencia <- Sys.Date()
prev$index <- sequencia
colnames(prev) <- c("Retorno", "VaR", "SE","Data")

prevs_feitas <- read_csv('dados/previsao.csv')
if(dia_da_semana != 2 && dia_da_semana != 7){
prevs <- rbind(prevs_feitas,prev)
} else{
prevs <- prev
}

write_csv(prevs, "dados/previsao.csv")

