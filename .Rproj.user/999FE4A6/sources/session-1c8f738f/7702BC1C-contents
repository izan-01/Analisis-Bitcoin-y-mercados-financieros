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


######################################################################
######### BASE 2: PIB vs Energía (múltiples países)###################
######################################################################

# Instalar si hace falta
# install.packages(c("WDI", "dplyr", "tidyr"))

library(WDI)
library(dplyr)
library(tidyr)

##Paso 2. Descargar indicadores del Banco Mundial
# Lista de países (elige uno de los tres siguientes)
paises <- c("ES", "DE", "US")  # España, Alemania, EE.UU.

energia_pib_multi <- WDI(
  country = paises,
  indicator = c("pib_pc" = "NY.GDP.PCAP.KD",
                "energia_pc" = "EG.USE.PCAP.KG.OE"),
  start = 1990, end = 2023
)


##Paso 3. Limpieza y estructura
energia_pib_multi <- energia_pib_multi %>%
  arrange(country, year) %>%
  dplyr::select(country, year, pib_pc, energia_pc) %>%
  na.omit()

# Primeras filas
head(energia_pib_multi)

# Últimas filas
tail(energia_pib_multi)

# Número de observaciones por país
table(energia_pib_multi$country)

# Resumen general
summary(energia_pib_multi)


##Paso 4. Verificación rápida
# Primeras filas
head(energia_pib_multi)

# Últimas filas
tail(energia_pib_multi)

# Número de observaciones por país
table(energia_pib_multi$country)

# Resumen general
summary(energia_pib_multi)

##Paso 5. Gráfico exploratorio
library(ggplot2)

ggplot(energia_pib_multi, aes(x = year)) +
  geom_line(aes(y = pib_pc, color = "PIB per cápita")) +
  geom_line(aes(y = energia_pc, color = "Energía per cápita")) +
  facet_wrap(~country, scales = "free_y") +
  labs(title = "PIB y Consumo energético per cápita (1990–2023)",
       y = "Valor", x = "Año", color = "") +
  theme_minimal()

write.csv(energia_pib_multi, "energia_pib_multi.csv", row.names = FALSE)

#############################################################
## VARIABLES ADICIONALES (RENOVABLES, FÓSILES, POBLACIÓN) ##
#############################################################

# --- VARIABLES ADICIONALES (1990–2023 descargadas, recorte final hasta 2016) ---
library(WDI)
library(dplyr)
library(ggplot2)

# Reutilizamos el vector de países y años
# (asegúrate de tener definido previamente `paises` y `energia_pib_multi`)

# Descargar variables adicionales
extra_vars <- WDI(
  country = paises,
  indicator = c(
    "fossil_share"    = "EG.USE.COMM.FO.ZS",   # Uso de combustibles fósiles (% del total)
    "renewable_share" = "EG.ELC.RNEW.ZS",      # Electricidad renovable (% del total)
    "population"      = "SP.POP.TOTL"          # Población total
  ),
  start = 1990, end = 2023
) %>%
  arrange(country, year)

# Unir con la base original
energia_pib_ext <- energia_pib_multi %>%
  left_join(extra_vars, by = c("country", "year"))

# Eliminar ceros falsos antes del filtrado
energia_pib_ext <- energia_pib_ext %>%
  mutate(
    fossil_share = ifelse(fossil_share == 0, NA, fossil_share),
    renewable_share = ifelse(renewable_share == 0, NA, renewable_share)
  )

# Guardar versión completa (con huecos en años recientes)
write.csv(energia_pib_ext, "energia_pib_multi_ext_completa.csv", row.names = FALSE)


# Filtrar solo hasta 2016 y años con datos completos
energia_pib_final <- energia_pib_ext %>%
  filter(year <= 2016) %>%
  filter(!is.na(fossil_share), !is.na(renewable_share), !is.na(population))

# Revisar estructura final
summary(energia_pib_final)
range(energia_pib_final$year)

# Guardar versión limpia y completa (1990–2016)
write.csv(energia_pib_final, "energia_pib_multi_ext_1990_2016.csv", row.names = FALSE)


# --- GRÁFICO EXPLORATORIO (solo datos completos 1990–2016) ---
ggplot(energia_pib_final, aes(x = year)) +
  geom_line(aes(y = fossil_share, color = "Fósil (%)")) +
  geom_line(aes(y = renewable_share, color = "Renovable (%)")) +
  facet_wrap(~country, scales = "free_y") +
  labs(
    title = "Participación de energías fósiles y renovables (1990–2016)",
    subtitle = "Solo años con información completa para todas las variables",
    y = "Porcentaje del total",
    x = "Año",
    color = ""
  ) +
  theme_minimal()


#################################################################
##### BASE 3: Educación – Desigualdad – Desempleo (2000–2023)####
#################################################################

##Paso 1. Cargar librerías

# Instalar si no las tienes
# install.packages(c("WDI", "dplyr"))

library(WDI)
library(dplyr)

##Paso 2. Descargar los datos
educacion_ineq <- WDI(
  country = c("ES"),
  indicator = c(
    "educacion" = "SE.SEC.CUAT.UP.ZS",
    "gini" = "SI.POV.GINI",
    "desempleo" = "SL.UEM.TOTL.ZS"
  ),
  start = 2000, end = 2023
)

##Paso 3. Limpiar y ordenar
educacion_ineq <- educacion_ineq %>%
  arrange(country, year) %>%
  dplyr::select(country, year, educacion, gini, desempleo) %>%
  na.omit()

##Paso 4. Verificación rápida

head(educacion_ineq)
tail(educacion_ineq)
table(educacion_ineq$country)
summary(educacion_ineq)

##Paso 5. Visualización inicial
library(ggplot2)

ggplot(educacion_ineq, aes(x = year)) +
  geom_line(aes(y = educacion, color = "Educación (%)")) +
  geom_line(aes(y = gini, color = "Gini")) +
  geom_line(aes(y = desempleo, color = "Desempleo (%)")) +
  facet_wrap(~country, scales = "free_y") +
  labs(title = "Educación, Desigualdad y Desempleo (2000–2023)",
       y = "Valor", x = "Año", color = "") +
  theme_minimal()

##########################################
###Paso 6. Estandarización de variables###
##########################################

# Crear una versión estandarizada del data frame
educacion_ineq_std <- educacion_ineq

# Estandarizar las tres variables cuantitativas
educacion_ineq_std[, c("educacion", "gini", "desempleo")] <-
  scale(educacion_ineq[, c("educacion", "gini", "desempleo")])

# Verificar
summary(educacion_ineq_std)

apply(educacion_ineq_std[, c("educacion", "gini", "desempleo")], 2, mean)
apply(educacion_ineq_std[, c("educacion", "gini", "desempleo")], 2, sd)


## Exportar base limpia
write.csv(educacion_ineq, "educacion_ineq.csv", row.names = FALSE)

#####################################################
######## Variables adicionales complementarias ######
#####################################################

# Descargar variables adicionales
extra_vars_ed <- WDI(
  country = c("ES"),
  indicator = c(
    "gasto_educacion" = "SE.XPD.TOTL.GD.ZS",  # Gasto público en educación (% del PIB)
    "pobreza" = "SI.POV.DDAY",                # Población bajo línea de pobreza (%)
    "empleo_fem" = "SL.EMP.TOTL.SP.FE.ZS"     # Empleo femenino (% del total)
  ),
  start = 2000, end = 2023
) %>%
  arrange(country, year)

# Unir con la base principal
educacion_ineq_ext <- educacion_ineq %>%
  left_join(extra_vars_ed, by = c("country", "year"))

# Eliminar filas con valores incompletos en las variables nuevas
educacion_ineq_ext <- educacion_ineq_ext %>%
  filter(!is.na(gasto_educacion), !is.na(pobreza), !is.na(empleo_fem))

# Verificar estructura ampliada
summary(educacion_ineq_ext)

# Guardar versión extendida
write.csv(educacion_ineq_ext, "educacion_ineq_ext.csv", row.names = FALSE)

## Visualización extendida (educación, desigualdad, empleo y pobreza)
ggplot(educacion_ineq_ext, aes(x = year)) +
  geom_line(aes(y = educacion, color = "Educación (%)")) +
  geom_line(aes(y = gini, color = "Gini")) +
  geom_line(aes(y = desempleo, color = "Desempleo (%)")) +
  geom_line(aes(y = gasto_educacion, color = "Gasto en Educación (% PIB)"), linetype = "dashed") +
  geom_line(aes(y = pobreza, color = "Pobreza (% población)"), linetype = "dotted") +
  facet_wrap(~country, scales = "free_y") +
  labs(
    title = "Educación, Desigualdad y Mercado Laboral (2000–2023)",
    y = "Valor (%)", x = "Año", color = ""
  ) +
  theme_minimal()
