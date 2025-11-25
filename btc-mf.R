##################################################
########### BASE 1: Bitcoin – NASDAQ #############
##################################################

library(quantmod)
library(tidyverse)
library(lubridate)

# Descargar Bitcoin y NASDAQ (fijando tope 2025-10-31)
getSymbols("BTC-USD", src = "yahoo", from = "2015-01-01", to = "2025-10-31")
getSymbols("^IXIC", src = "yahoo", from = "2015-01-01", to = "2025-10-31")

BTC <- `BTC-USD`
NAS <- IXIC

# Convertir ambas a frecuencia mensual, tomando el último valor de cada mes
btc_monthly <- to.monthly(BTC, indexAt = "lastof", OHLC = FALSE)
nas_monthly <- to.monthly(NAS, indexAt = "lastof", OHLC = FALSE)

# Unir ambas series por fecha exacta (último día de mes común)
bitcoin_nasdaq <- merge(btc_monthly, nas_monthly, join = "inner")
View(bitcoin_nasdaq)
# Crear dataframe limpio
bitcoin_nasdaq <- data.frame(
  date = index(bitcoin_nasdaq),
  btc = as.numeric(bitcoin_nasdaq$BTC.USD.Close),
  nasdaq = as.numeric(bitcoin_nasdaq$IXIC.Close)
)

# Verificación inicial
head(bitcoin_nasdaq)
tail(bitcoin_nasdaq)
summary(bitcoin_nasdaq)

# Comprobar NAs
sum(is.na(bitcoin_nasdaq))   # debería dar 0

# Revisar duplicados
any(duplicated(bitcoin_nasdaq$date))   # debería dar FALSE

# Graficar para ver la forma temporal
plot(bitcoin_nasdaq$date, bitcoin_nasdaq$btc, type="l", col="blue", ylab="BTC / NASDAQ")
lines(bitcoin_nasdaq$date, bitcoin_nasdaq$nasdaq, col="red")
legend("topleft", legend=c("BTC","NASDAQ"), col=c("blue","red"), lty=1)


##############################################################
#### BLOQUE ADICIONAL 1: Volatilidad mensual del Bitcoin  ####
##############################################################

# Calcular retorno diario logarítmico
btc_returns <- dailyReturn(BTC$`BTC-USD.Close`, type = "log")

# Volatilidad móvil (30 días)
btc_volatility <- runSD(btc_returns, n = 30) * sqrt(365)  # anualizada aprox.

# Convertir a frecuencia mensual tomando el último valor de cada mes
btc_vol_monthly <- to.monthly(btc_volatility, indexAt = "lastof", OHLC = FALSE)

# Renombrar
colnames(btc_vol_monthly) <- "btc_vol"

# Incorporar al dataset principal (por fecha)
bitcoin_nasdaq <- merge(
  bitcoin_nasdaq,
  data.frame(date = index(btc_vol_monthly),
             btc_vol = as.numeric(btc_vol_monthly$btc_vol)),
  by = "date",
  all.x = TRUE
)

##############################################################
#### BLOQUE ADICIONAL 2: Tasa de interés de la FED (FRED) ####
##############################################################

# Descargar tasa de fondos federales
getSymbols("FEDFUNDS", src = "FRED", from = "2015-01-01", to = "2025-10-31")

# Convertir a frecuencia mensual (último valor de mes)
fed_monthly <- to.monthly(FEDFUNDS, indexAt = "lastof", OHLC = FALSE)
colnames(fed_monthly) <- "fed_rate"

# Incorporar al dataset principal
bitcoin_nasdaq <- merge(
  bitcoin_nasdaq,
  data.frame(date = index(fed_monthly),
             fed_rate = as.numeric(fed_monthly$fed_rate)),
  by = "date",
  all.x = TRUE
)

####################################################
#### BLOQUE FINAL DE VERIFICACIÓN Y EXPORTACIÓN ####
####################################################

# Verificar estructura final
summary(bitcoin_nasdaq)
head(bitcoin_nasdaq)
tail(bitcoin_nasdaq)

# Graficar todas las series juntas (BTC, NASDAQ, FED)
plot(bitcoin_nasdaq$date, bitcoin_nasdaq$btc, type="l", col="blue", ylab="Valor", main="BTC - NASDAQ - FED Funds")
lines(bitcoin_nasdaq$date, bitcoin_nasdaq$nasdaq, col="red")
lines(bitcoin_nasdaq$date, bitcoin_nasdaq$fed_rate*1000, col="green")  # escalado visual
legend("topleft", legend=c("BTC","NASDAQ","FED Funds (x1000)"), col=c("blue","red","green"), lty=1)

# Exportar CSV final limpio
write.csv(bitcoin_nasdaq, "bitcoin_nasdaq_extended.csv", row.names = FALSE)