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


############################################################
####  BLOQUE: MODELOS ARIMA/ARIMAX Y COINTEGRACIÓN      ####
############################################################

# Librerías necesarias (basadas en Script Granger...)
library(forecast)
library(tseries)
library(urca)
library(ggplot2)
library(lmtest)
library(nlme)      # Para GLS
library(sandwich)  # Para errores robustos

# ----------------------------------------------------------
# 1. PREPARACIÓN DE SERIES TEMPORALES (Objetos ts)
# ----------------------------------------------------------
# Aseguramos frecuencia mensual (12) y fecha de inicio correcta
ts_btc    <- ts(bitcoin_nasdaq$btc,    start = c(2015, 1), frequency = 12)
ts_nasdaq <- ts(bitcoin_nasdaq$nasdaq, start = c(2015, 1), frequency = 12)
ts_fed    <- ts(bitcoin_nasdaq$fed_rate, start = c(2015, 1), frequency = 12)

# ----------------------------------------------------------
# 2. MODELOS ARIMA y ARIMAX (Pronóstico)
# ----------------------------------------------------------
# Objetivo: Modelar la serie y predecir.

# --- A) Modelo ARIMA Univariante (Solo Bitcoin) ---
# stepwise=FALSE y approximation=FALSE mejoran la precisión (según tu script)
modelo_arima <- auto.arima(ts_btc, stepwise = FALSE, approximation = FALSE)
cat("\n--- Resumen ARIMA Base ---\n")
summary(modelo_arima)

# --- B) Modelo ARIMAX (Con NASDAQ y FED como exógenas) ---
xreg_actual <- cbind(NASDAQ = ts_nasdaq, FED = ts_fed)

modelo_arimax <- auto.arima(ts_btc, xreg = xreg_actual, 
                            stepwise = FALSE, approximation = FALSE)
cat("\n--- Resumen ARIMAX ---\n")
summary(modelo_arimax)

# Comparación de modelos (AIC)
cat("AIC ARIMA:", AIC(modelo_arima), "\n")
cat("AIC ARIMAX:", AIC(modelo_arimax), "\n")
# Si AIC ARIMAX < AIC ARIMA -> Las variables externas mejoran el modelo.

# --- C) Diagnóstico de Residuos ---
checkresiduals(modelo_arimax)

# --- D) Pronóstico Dinámico (12 meses) ---
# Para evitar la línea plana, proyectamos primero las exógenas (NASDAQ y FED)
horizonte <- 12
fit_nasdaq_fut <- auto.arima(ts_nasdaq)
fit_fed_fut    <- auto.arima(ts_fed)

# Proyección de regresores futuros
futuro_nasdaq <- forecast(fit_nasdaq_fut, h = horizonte)$mean
futuro_fed    <- forecast(fit_fed_fut, h = horizonte)$mean
xreg_futuro   <- cbind(NASDAQ = futuro_nasdaq, FED = futuro_fed)

# Pronóstico final del Bitcoin
forecast_arimax <- forecast(modelo_arimax, xreg = xreg_futuro, h = horizonte)

# Visualización
autoplot(forecast_arimax) +
  labs(title = "Pronóstico Bitcoin (ARIMAX 12 meses)",
       subtitle = "Con proyecciones dinámicas de NASDAQ y FED",
       y = "Precio USD", x = "Año") +
  theme_minimal()


# ----------------------------------------------------------
# 3. TEST DE COINTEGRACIÓN (Engle-Granger)
# ----------------------------------------------------------
# "Si las variables son no estacionarias, prueba de cointegración"

# Paso 1: Regresión de Largo Plazo (Niveles)
coint_reg <- lm(ts_btc ~ ts_nasdaq + ts_fed)
summary(coint_reg)

# Paso 2: Extraer residuos
resid_coint <- residuals(coint_reg)

# Visualización de residuos (¿Parecen estacionarios?)
# Ajustamos los márgenes: c(abajo, izquierda, arriba, derecha)
# Aumentamos un poco la izquierda (5) y abajo (5) para que quepan las etiquetas
par(mar = c(5, 5, 4, 2))

plot(resid_coint, 
     type = "l", 
     col = "purple", 
     lwd = 2, 
     main = "Residuos de la relación de largo plazo",
     xlab = "Tiempo (en meses)",       # Etiqueta eje X
     ylab = "Residuos",     # Etiqueta eje Y
     las = 1,               # Pone los números del eje Y en horizontal (más legible)
     cex.lab = 1.2,         # Aumenta un poco el tamaño de las etiquetas de los ejes
     cex.axis = 1)          # Tamaño de los números

# Línea de referencia
abline(h = 0, col = "gray", lwd = 2, lty = 2)

# Cuadrícula
grid()

# Restauramos márgenes por defecto (buena práctica)
par(mar = c(5, 4, 4, 2) + 0.1)

# Paso 3: Test ADF a los residuos
# type="none" porque los residuos tienen media cero teórica.
adf_resid <- ur.df(resid_coint, type="none", selectlags="AIC")
summary(adf_resid)

# === GUÍA DE DECISIÓN ===
# MIRA EL VALOR "t-statistic" DEL TEST ADF ANTERIOR:
# - Si t-stat < Valor Crítico: RECHAZAS H0 -> HAY COINTEGRACIÓN.
#   -> Ejecuta el apartado 4.A (ECM).
# - Si t-stat > Valor Crítico: NO RECHAZAS H0 -> NO HAY COINTEGRACIÓN.
#   -> Ejecuta el apartado 4.B (GLS/MCG).


# ----------------------------------------------------------
# ----------------------------------------------------------
# 4.A CASO AFIRMATIVO: MODELO DE CORRECCIÓN DE ERROR (ECM)
# ----------------------------------------------------------

# 1. Definir N para recortar vectores correctamente
n <- length(resid_coint)

# 2. Construir las variables
# dy, dx1, dx2 pierden la primera observación (tienen longitud n-1)
dy  <- diff(ts_btc)
dx1 <- diff(ts_nasdaq)
dx2 <- diff(ts_fed)

# u_lag: Queremos el residuo de t-1. 
# Para alinear con las diferencias (que empiezan en t=2), 
# cogemos los residuos desde el 1 hasta el penúltimo (n-1).
u_lag <- resid_coint[1:(n-1)]

# 3. Crear el dataframe (Ahora sí tendrán la misma longitud)
ecm_df <- data.frame(dy = as.numeric(dy), 
                     dx1 = as.numeric(dx1), 
                     dx2 = as.numeric(dx2), 
                     u_lag = as.numeric(u_lag))

# 4. Estimación del ECM
ecm_fit <- lm(dy ~ dx1 + dx2 + u_lag, data = ecm_df)
summary(ecm_fit)



###############################################################
####    BLOQUE: ENDOGENEIDAD Y VARIABLES INSTRUMENTALES    ####
###############################################################

# 1. CARGA DE LIBRERÍAS (Basado en Script Brent-EURUSD)
library(AER)        # Para la función ivreg (MC2E)
library(lmtest)     # Para tests de diagnóstico
library(sandwich)   # Para errores robustos
library(stargazer)  # Para tablas comparativas
library(dplyr)      # Para crear retardos (lag)

# 2. PREPARACIÓN DE DATOS E INSTRUMENTOS
# -------------------------------------------------------------
# Creamos un rezago de la Tasa FED como instrumento adicional
# Esto nos permite tener 2 instrumentos (FED actual y FED pasada) para probar sobreidentificación.
datos_iv <- bitcoin_nasdaq %>%
  mutate(fed_rate_L1 = dplyr::lag(fed_rate, 1)) %>%
  na.omit() # Eliminamos el NA generado por el lag

# 3. MODELO BASE (MCO / OLS)
# -------------------------------------------------------------
# Modelo ingenuo: Asume que NASDAQ es exógeno.
# btc = beta0 + beta1 * nasdaq + error
modelo_mco <- lm(btc ~ nasdaq, data = datos_iv)

cat("\n--- Resumen Modelo MCO (Sin corregir endogeneidad) ---\n")
summary(modelo_mco)


# 4. ESTIMACIÓN POR VARIABLES INSTRUMENTALES (MC2E / 2SLS)
# -------------------------------------------------------------
# Ecuación: btc ~ nasdaq
# Instrumentos: fed_rate + fed_rate_L1
# Sintaxis ivreg: dependiente ~ endógena | instrumentos
modelo_iv <- ivreg(btc ~ nasdaq | fed_rate + fed_rate_L1, data = datos_iv)

cat("\n--- Resumen Modelo MC2E (Con Instrumentos: Tasa FED) ---\n")
# El argumento diagnostics = TRUE nos da los tests de Hausman, Sargan y Weak Instruments automáticamente
summary(modelo_iv, diagnostics = TRUE)


# 5. DIAGNÓSTICO E INTERPRETACIÓN DE TESTS
# -------------------------------------------------------------
# Extraemos los tests diagnósticos para analizar uno a uno (estilo fuente Brent)

diagnosticos <- summary(modelo_iv, diagnostics = TRUE)$diagnostics

cat("\n--- ANÁLISIS DE DIAGNÓSTICOS ---\n")

# A) TEST DE INSTRUMENTOS DÉBILES (Weak Instruments)
# H0: Los instrumentos son débiles (no correlacionan con la endógena).
# Regla práctica: Si el estadístico F > 10, los instrumentos son fuertes.
f_stat_weak <- diagnosticos["Weak instruments", "statistic"]
cat("1. Test de Instrumentos Débiles (F-statistic):", round(f_stat_weak, 2), "\n")
if(f_stat_weak > 10) {
  cat("   -> Conclusión: Instrumentos FUERTES (F > 10). La Tasa FED explica bien al NASDAQ.\n")
} else {
  cat("   -> Conclusión: Instrumentos DÉBILES (Cuidado con la inferencia).\n")
}

# B) TEST DE ENDOGENEIDAD (Wu-Hausman)
# H0: La variable explicativa (NASDAQ) es exógena (MCO es consistente).
# H1: La variable es endógena (MCO es sesgado, MC2E es necesario).
p_hausman <- diagnosticos["Wu-Hausman", "p-value"]
cat("2. Test de Endogeneidad (Wu-Hausman) p-value:", round(p_hausman, 4), "\n")
if(p_hausman < 0.05) {
  cat("   -> Conclusión: Se RECHAZA exogeneidad. Hay endogeneidad. Usar MC2E es CORRECTO.\n")
} else {
  cat("   -> Conclusión: No se rechaza exogeneidad. MCO sería suficiente.\n")
}

# C) TEST DE SOBREIDENTIFICACIÓN (Sargan)
# Solo funciona si tenemos más instrumentos (2) que variables endógenas (1).
# H0: Los instrumentos son válidos (no correlacionados con el error estructural).
p_sargan <- diagnosticos["Sargan", "p-value"]
cat("3. Test de Validez (Sargan) p-value:", round(p_sargan, 4), "\n")
if(p_sargan > 0.05) {
  cat("   -> Conclusión: No se rechaza H0. Los instrumentos son VÁLIDOS (Exógenos).\n")
} else {
  cat("   -> Conclusión: Se rechaza H0. Los instrumentos podrían ser inválidos (correlacionados con el error).\n")
}


# 6. COMPARACIÓN MCO vs MC2E (Tabla Resumen)
# -------------------------------------------------------------
stargazer(modelo_mco, modelo_iv, type = "text",
          title = "Comparación: MCO vs MC2E (Corrección de Endogeneidad)",
          column.labels = c("MCO (Ingenuo)", "MC2E (IV: FED Rate)"),
          model.names = FALSE)


# 7. DIAGNÓSTICO VISUAL (Residuos MC2E)
# -------------------------------------------------------------
# Verificar si los residuos del modelo corregido se comportan bien
residuos_iv <- residuals(modelo_iv)

par(mar = c(5, 5, 4, 2)) # Ajuste de márgenes para que se vea bien
plot(residuos_iv, type = "l", col = "darkblue", lwd = 1.5,
     main = "Residuos del Modelo MC2E",
     ylab = "Error Estimado", xlab = "Índice Temporal",
     las = 1)
abline(h = 0, col = "red", lty = 2, lwd = 2)
grid()
