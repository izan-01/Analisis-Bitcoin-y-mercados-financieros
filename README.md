# Analisis-Bitcoin-y-mercados-financieros
Repositorio para la elaboración del análisis de Bitcoin y mercados financieros en R, con un enfoque econométrico
Instrucciones del trabajo:
Estructura sugerida del trabajo escrito (15-20 páginas)
1. Introducción
• Contextualización del tema.
• Pregunta de investigación y objetivos.
• Breve revisión de la literatura (mínimo 3 fuentes académicas, en formato APA).
2. Análisis exploratorio
• Descripción general de la base de datos.
• Análisis gráfico y evolución temporal de las variables.
• Identificación de posibles relaciones visuales entre ellas.
3. Pruebas estadísticas iniciales
• Estacionariedad (ADF, KPSS).
• Normalidad, homocedasticidad y autocorrelación (Shapiro–Wilk, Breusch–
Pagan, Durbin–Watson).
• Multicolinealidad (VIF).
4. Modelos econométricos
• Aplicación de ARIMA y/o ARIMAX para modelar series temporales y
pronóstico.
• Si las variables son no estacionarias, prueba de cointegración (Johansen o
Engle–Granger).
o En caso afirmativo, aplicar Modelo de Corrección de Error (ECM).
o En caso negativo, aplicar Mínimos Cuadrados Generalizados (MCG)
con AR(1) para corregir autocorrelación.
5. Endogeneidad y variables instrumentales
• Comprobar endogeneidad (test de Hausman o Wu-Hausman).
• Aplicar Mínimos Cuadrados en Dos Etapas (2SLS) cuando proceda, utilizando
las variables instrumentales disponibles en la base de datos (por ejemplo, tasas de
interés, gasto en educación, inversión energética, etc.).
• Interpretar los resultados de los diagnósticos: instrumentos débiles, validez,
sobreidentificación (Sargan).
6. Interpretación y discusión de resultados
• Interpretación económica de los coeficientes.
• Evaluación del ajuste de los modelos (RMSE, MAPE, R² ajustado).
• Contraste de los hallazgos con la literatura revisada.
7. Conclusiones y recomendaciones
• Síntesis del análisis.
• Reflexión sobre las limitaciones y posibles extensiones del estudio.
8. Referencias bibliográficas
• Citar todas las fuentes teóricas, metodológicas y de datos en formato APA (7ª
edición).
4. Aspectos técnicos y formato
• Formato:
El trabajo final deberá incluir:
1. Portada (título, autores, curso, fecha).
2. Introducción y planteamiento del problema.
3. Revisión de la literatura (mínimo 5 fuentes académicas en formato APA).
4. Marco metodológico y descripción de datos.
5. Resultados y discusión.
6. Conclusiones.
7. Bibliografía (formato APA 7.ª edición).
• Extensión: 15-20 páginas, excluyendo anexos y referencias.
• Formato: letra Times New Roman 12, interlineado 1,5, márgenes estándar.
• 1. Entrega: PDF a través del Campus Virtual antes del 12 de diciembre a las
23:59.
• En los anexos se pueden incluir gráficos, salidas de R, pruebas estadísticas, etc.
• 2. Entrega: Archivo reproducible (.R, .Rmd) con todos los pasos numerados.
• 3. Entrega: Base de datos limpia (.csv) si se elige un país distinto a los tres
principales y de una fuente diferente a la sugerida.
