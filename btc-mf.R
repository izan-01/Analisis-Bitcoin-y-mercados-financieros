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

##############################################################
####      ANÁLISIS EXPLORATORIO DE DATOS (EDA)            ####
##############################################################

# 1. Librerías para gráficos y tablas (estilo del curso)
library(ggplot2)
library(skimr)
library(stargazer)
library(corrplot) # Para matriz de correlaciones
library(gridExtra) # Para organizar gráficos

# Aseguramos que los datos estén cargados (del bloque anterior)
# bitcoin_nasdaq <- read.csv("bitcoin_nasdaq_extended.csv") # Descomentar si se carga desde cero
# bitcoin_nasdaq$date <- as.Date(bitcoin_nasdaq$date)

# ------------------------------------------------------------
# 2. DESCRIPCIÓN GENERAL DE LA BASE DE DATOS
# ------------------------------------------------------------

# Resumen estadístico robusto (skimr)
# Nos da media, desviación, cuartiles y un pequeño histograma
skim(bitcoin_nasdaq)

# Tabla de estadísticos descriptivos para publicación (stargazer)
stargazer(bitcoin_nasdaq, type = "text", 
          title = "Estadísticos Descriptivos: BTC, NASDAQ, Volatilidad y FED Rate",
          digits = 2,
          median = TRUE)

# Interpretación sugerida:
# - Observar la media y la desviación estándar (sd) para ver la dispersión relativa del BTC vs NASDAQ.
# - Mirar Min y Max para identificar el rango del periodo estudiado.

# ------------------------------------------------------------
# 3. ANÁLISIS GRÁFICO Y EVOLUCIÓN TEMPORAL
# ------------------------------------------------------------

# Gráfico 1: Evolución comparada (Reescalando a base 100 para comparar tendencias)
# Tomamos el primer valor de cada serie como base = 100
base_btc <- bitcoin_nasdaq$btc / bitcoin_nasdaq$btc[1] * 100
base_nas <- bitcoin_nasdaq$nasdaq / bitcoin_nasdaq$nasdaq[1] * 100

df_base100 <- data.frame(date = bitcoin_nasdaq$date, BTC_Idx = base_btc, NAS_Idx = base_nas)

ggplot(df_base100, aes(x = date)) +
  geom_line(aes(y = BTC_Idx, color = "Bitcoin (Base 100)"), size = 1) +
  geom_line(aes(y = NAS_Idx, color = "NASDAQ (Base 100)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Bitcoin (Base 100)" = "darkblue", "NASDAQ (Base 100)" = "red")) +
  labs(title = "Evolución comparada: Bitcoin vs NASDAQ (Base 100 = Inicio Muestra)",
       subtitle = "Permite ver qué activo ha crecido proporcionalmente más",
       y = "Índice (Base 100)", x = "Fecha", color = "Activo") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico 2: Variables Explicativas (Volatilidad y Tasa FED)
p1 <- ggplot(bitcoin_nasdaq, aes(x = date, y = btc_vol)) +
  geom_line(color = "orange") +
  geom_area(fill = "orange", alpha = 0.3) +
  labs(title = "Volatilidad Histórica del Bitcoin (30 días)", y = "Volatilidad", x = "") +
  theme_minimal()

p2 <- ggplot(bitcoin_nasdaq, aes(x = date, y = fed_rate)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Tasa de Fondos Federales (FED)", y = "Tasa (%)", x = "Fecha") +
  theme_minimal()

grid.arrange(p1, p2, nrow = 2)

# ------------------------------------------------------------
# 4. IDENTIFICACIÓN DE RELACIONES (Correlación y Scatter)
# ------------------------------------------------------------

# Matriz de Correlación
cor_matrix <- cor(bitcoin_nasdaq[, c("btc", "nasdaq", "btc_vol", "fed_rate")], use = "complete.obs")
print(cor_matrix)

# Visualización de la matriz de correlación
corrplot(cor_matrix, method = "number", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Matriz de Correlación", mar = c(0,0,1,0))

# Scatter Plot: Bitcoin vs NASDAQ
# Añadimos una línea de tendencia lineal (lm) y suavizada (loess)
ggplot(bitcoin_nasdaq, aes(x = nasdaq, y = btc)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Relación Bitcoin vs NASDAQ",
       subtitle = "¿Existe correlación positiva entre el mercado tecnológico y cripto?",
       x = "Puntos NASDAQ", y = "Precio Bitcoin (USD)") +
  theme_minimal()

# Scatter Plot: Bitcoin vs Tasa FED (Interés)
ggplot(bitcoin_nasdaq, aes(x = fed_rate, y = btc)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Relación Bitcoin vs Tasa de Interés FED",
       subtitle = "Teoría: Tipos altos deberían bajar el precio de activos de riesgo",
       x = "Tasa FED (%)", y = "Precio Bitcoin (USD)") +
  theme_minimal()

##############################################################
####      PRUEBAS ESTADÍSTICAS INICIALES                  ####
##############################################################

# 1. Carga de librerías necesarias para los tests
library(tseries)    # Para kpss.test, jarque.bera.test (opcional)
library(urca)       # Para ur.df (Dickey-Fuller Aumentado)
library(lmtest)     # Para bptest (Breusch-Pagan), dwtest (Durbin-Watson)
library(car)        # Para vif (Multicolinealidad)
library(nortest)    # Opcional, para más tests de normalidad si fuera necesario

# ------------------------------------------------------------
# 2. ESTACIONARIEDAD (ADF y KPSS)
# ------------------------------------------------------------
# Objetivo: Determinar si las series tienen raíz unitaria (son no estacionarias)
# o son estacionarias en media/varianza.

# Función auxiliar para reportar resultados rápidamente (estilo pedagógico)
reportar_estacionariedad <- function(serie, nombre) {
  cat("\n--- Análisis de:", nombre, "---\n")
  
  # Test ADF (H0: Existe raíz unitaria / No estacionaria)
  # Usamos type="trend" porque los precios suelen tener tendencia
  adf <- ur.df(serie, type = "trend", selectlags = "AIC")
  cat("ADF Test (estadístico):", adf@teststat[1], "\n")
  cat("Valores críticos (1%, 5%, 10%):", adf@cval[1,], "\n")
  
  # Test KPSS (H0: La serie es estacionaria)
  kpss <- kpss.test(serie, null = "Trend")
  cat("KPSS Test (p-value):", kpss$p.value, "\n")
  
  if(kpss$p.value < 0.05) {
    cat("-> Resultado: Probablemente NO ESTACIONARIA (rechazamos H0 en KPSS)\n")
  } else {
    cat("-> Resultado: Posiblemente ESTACIONARIA\n")
  }
}

# Aplicamos los tests a las variables principales
# Nota: Trabajamos con las series originales (niveles)
reportar_estacionariedad(bitcoin_nasdaq$btc, "Bitcoin (Precio)")
reportar_estacionariedad(bitcoin_nasdaq$nasdaq, "NASDAQ (Índice)")
reportar_estacionariedad(bitcoin_nasdaq$fed_rate, "Tasa FED")
reportar_estacionariedad(bitcoin_nasdaq$btc_vol, "Volatilidad BTC")

# ------------------------------------------------------------
# 3. ESTIMACIÓN DE MODELO PRELIMINAR (Para diagnóstico de residuos)
# ------------------------------------------------------------
# Para probar Normalidad, Homocedasticidad y Autocorrelación, necesitamos
# un modelo lineal base sobre el cual analizar los residuos.
# Modelo hipotético: btc ~ nasdaq + fed_rate + btc_vol

modelo_preliminar <- lm(btc ~ nasdaq + fed_rate + btc_vol, data = bitcoin_nasdaq)
summary(modelo_preliminar)

# Extraemos los residuos para las pruebas
residuos_pre <- residuals(modelo_preliminar)

# ------------------------------------------------------------
# 4. NORMALIDAD DE LOS RESIDUOS (Shapiro-Wilk)
# ------------------------------------------------------------
# H0: Los residuos siguen una distribución Normal.
# Si p-value < 0.05, rechazamos normalidad.

shapiro_test <- shapiro.test(residuos_pre)
print(shapiro_test)

# Gráfico Q-Q para inspección visual (complemento habitual en tus scripts)
qqnorm(residuos_pre, main = "Q-Q Plot de los Residuos")
qqline(residuos_pre, col = "red", lwd = 2)

# ------------------------------------------------------------
# 5. HOMOCEDASTICIDAD (Breusch-Pagan)
# ------------------------------------------------------------
# H0: La varianza de los errores es constante (Homocedasticidad).
# H1: La varianza depende de las variables explicativas (Heterocedasticidad).
# Si p-value < 0.05, existe heterocedasticidad.

bp_test <- bptest(modelo_preliminar)
print(bp_test)

# ------------------------------------------------------------
# 6. AUTOCORRELACIÓN (Durbin-Watson)
# ------------------------------------------------------------
# H0: No hay autocorrelación de primer orden en los residuos.
# Estadístico DW cercano a 2 indica ausencia de autocorrelación.
# DW < 1.5 o > 2.5 sugiere problemas.

dw_test <- dwtest(modelo_preliminar)
print(dw_test)

# ------------------------------------------------------------
# 7. MULTICOLINEALIDAD (VIF)
# ------------------------------------------------------------
# Factor de Inflación de la Varianza.
# Regla general: VIF > 5 o 10 indica multicolinealidad problemática.
# Esto ocurre si NASDAQ, Tasa FED y Volatilidad están muy correlacionadas entre sí.

vif_valores <- vif(modelo_preliminar)
print(vif_valores)

# Visualización rápida de VIF (barplot)
barplot(vif_valores, main = "VIF Values", horiz = TRUE, col = "steelblue", xlim = c(0, max(vif_valores)+2))
abline(v = 5, col = "red", lty = 2) # Línea de umbral de alerta

# ------------------------------------------------------------
# GRÁFICO Q-Q MEJORADO (ggplot2)
# ------------------------------------------------------------
# Objetivo: Evaluar visualmente si los puntos siguen la línea diagonal teórica.

# 1. Crear un dataframe con los residuos para poder usar ggplot
df_residuos <- data.frame(Residuos_Estandarizados = rstandard(modelo_preliminar))

# 2. Generar el gráfico
ggplot(df_residuos, aes(sample = Residuos_Estandarizados)) +
  stat_qq(color = "steelblue", alpha = 0.6, size = 2) +     # Puntos azules semitransparentes
  stat_qq_line(color = "red", lwd = 1, linetype = "solid") + # Línea de referencia roja sólida
  labs(title = "Gráfico Q-Q de Normalidad de los Residuos",
       subtitle = "Interpretación: Si los puntos se alejan de la línea roja, sospechamos NO normalidad",
       x = "Cuantiles Teóricos (Distribución Normal)",
       y = "Cuantiles Observados (Residuos Estandarizados)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"))

# ------------------------------------------------------------
# GRÁFICO VIF MEJORADO (ggplot2)
# ------------------------------------------------------------
# Objetivo: Visualizar qué variables superan el umbral de multicolinealidad.

# 1. Preparar los datos: Convertir el vector de VIF a un dataframe y ordenar
vif_df <- data.frame(
  Variable = names(vif_valores),
  VIF = as.numeric(vif_valores)
) %>%
  arrange(desc(VIF)) # Ordenar de mayor a menor VIF

# 2. Generar el gráfico de barras
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +    # reorder() ordena el eje Y según el valor VIF
  geom_col(fill = "steelblue", width = 0.7) +                 # Barras azules
  geom_text(aes(label = round(VIF, 2)), hjust = -0.2, size = 4, color = "black") + # Añadir el valor numérico al lado de la barra
  coord_flip() + # Girar el gráfico para que sea horizontal
  scale_y_continuous(limits = c(0, max(vif_df$VIF) * 1.15)) + # Dar un poco de espacio extra a la derecha para los números
  labs(title = "Factor de Inflación de la Varianza (VIF)",
       subtitle = "Valores > 5 indican posible multicolinealidad severa",
       x = "", # Quitamos la etiqueta del eje X (ya son los nombres de variables)
       y = "Valor VIF") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 11, face = "bold", color = "black")) # Nombres de variables más grandes y oscuros


##############################################################
####      MODELIZACIÓN ECONOMÉTRICA Y COINTEGRACIÓN       ####
##############################################################

library(urca)       # Para test de raíz unitaria en residuos (Engle-Granger)
library(nlme)       # Para MCG (GLS) con estructura AR(1)
library(forecast)   # Para ARIMA y auto.arima
library(lmtest)     # Para coeftest

# ------------------------------------------------------------
# 1. TEST DE COINTEGRACIÓN (Engle-Granger)
# ------------------------------------------------------------
# Paso 1: Regresión en niveles (Largo Plazo)
# btc = beta0 + beta1*nasdaq + beta2*fed_rate + u_t
modelo_largo_plazo <- lm(btc ~ nasdaq + fed_rate, data = bitcoin_nasdaq)
summary(modelo_largo_plazo)

# Paso 2: Obtener los residuos
residuos_lp <- residuals(modelo_largo_plazo)

# Paso 3: Test ADF a los residuos (H0: No estacionario / No cointegración)
# Importante: Si los residuos son estacionarios (rechazamos H0), existe cointegración.
test_coint <- ur.df(residuos_lp, type = "none", selectlags = "AIC")
summary(test_coint)

# INTERPRETACIÓN:
# Mira el valor de "test-statistic" frente a los "critical values".
# - Si estadístico < valor crítico (ej. -3.5 < -2.6): RECHAZAMOS H0 -> HAY COINTEGRACIÓN.
# - Si estadístico > valor crítico: NO RECHAZAMOS H0 -> NO HAY COINTEGRACIÓN (Regresión espuria).

# ------------------------------------------------------------
# 2. CAMINO A: MODELO DE CORRECCIÓN DE ERROR (ECM)
# (Ejecutar SOLO si encontraste COINTEGRACIÓN en el paso anterior)
# ------------------------------------------------------------

# Crear diferencias (Deltas) y rezago del residuo
n <- nrow(bitcoin_nasdaq)
dBTC <- diff(bitcoin_nasdaq$btc)
dNASDAQ <- diff(bitcoin_nasdaq$nasdaq)
dFED <- diff(bitcoin_nasdaq$fed_rate)
u_lag <- residuos_lp[1:(n-1)] # Residuo retardado un periodo (t-1)

# Ajustar longitudes (al diferenciar perdemos 1 obs)
df_ecm <- data.frame(dBTC, dNASDAQ, dFED, u_lag)

# Estimación del ECM
# dBTC = alpha + beta1*dNASDAQ + beta2*dFED + lambda*u_lag + error
modelo_ecm <- lm(dBTC ~ dNASDAQ + dFED + u_lag, data = df_ecm)
summary(modelo_ecm)

# Interpretación:
# - El coeficiente de 'u_lag' (lambda) debe ser negativo y significativo.
# - Indica la velocidad de ajuste hacia el equilibrio de largo plazo.

# ------------------------------------------------------------
# 3. CAMINO B: CORRECCIÓN POR AUTOCORRELACIÓN (MCG / GLS)
# (Ejecutar si NO hay cointegración o si los residuos tienen autocorrelación)
# ------------------------------------------------------------
# Usamos 'gls' del paquete nlme como en la fuente

# Creamos índice temporal entero necesario para gls
bitcoin_nasdaq$tiempo <- 1:nrow(bitcoin_nasdaq)

# Modelo GLS con estructura AR(1) para corregir autocorrelación
modelo_gls <- gls(btc ~ nasdaq + fed_rate, 
                  data = bitcoin_nasdaq,
                  correlation = corAR1(form = ~ tiempo),
                  method = "ML")

summary(modelo_gls)

# Comparación con MCO tradicional
AIC(modelo_largo_plazo, modelo_gls)
# Si AIC del GLS es menor, el ajuste AR(1) ha mejorado el modelo.

# ------------------------------------------------------------
# 4. MODELOS ARIMA / ARIMAX (Predicción)
# ------------------------------------------------------------
# ARIMAX: ARIMA de Bitcoin con variables exógenas (NASDAQ y FED)

# Convertir a objetos ts (series temporales) si no lo están
ts_btc <- ts(bitcoin_nasdaq$btc, frequency = 12)
ts_nasdaq <- ts(bitcoin_nasdaq$nasdaq, frequency = 12)
ts_fed <- ts(bitcoin_nasdaq$fed_rate, frequency = 12)

# Matriz de regresores exógenos
xreg_actual <- cbind(nasdaq = ts_nasdaq, fed = ts_fed)

# Auto ARIMA (selecciona automáticamente p,d,q)
modelo_arimax <- auto.arima(ts_btc, xreg = xreg_actual)
summary(modelo_arimax)

# Diagnóstico de residuos del ARIMAX
checkresiduals(modelo_arimax)

# --- PRONÓSTICO (Forecasting) ---
# Para pronosticar BTC a futuro, necesitamos valores futuros de NASDAQ y FED.
# Como ejemplo didáctico, usamos los últimos 12 valores observados como "futuro"
# (En un caso real, deberías proyectar NASDAQ y FED primero o usar escenarios).

xreg_futuro <- tail(xreg_actual, 12) 

# Pronóstico a 12 meses
pronostico <- forecast(modelo_arimax, xreg = xreg_futuro, h = 12)

# Gráfico del pronóstico
autoplot(pronostico) +
  labs(title = "Pronóstico Bitcoin (ARIMAX)", 
       y = "Precio USD", x = "Tiempo") +
  theme_minimal()