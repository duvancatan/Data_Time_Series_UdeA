# ===============================
# 0. LIBRERÍAS
# ===============================
library(tidyverse)
library(xts)
library(forecast)
library(tseries)

# ===============================
# 1. CARGA Y LIMPIEZA DE DATOS
# ===============================
peajesconcesion <- read.csv("C:/Users/laura.toro/Downloads/bases por peaje con consecion.csv", sep=";")

# Conversión de fecha
peajesconcesion$fecha <- as.Date(peajesconcesion$fecha)

# Revisión básica
str(peajesconcesion)
head(peajesconcesion)

# ===============================
# 2. FILTRADO POR CONCESIÓN
# ===============================
datos_filtrados1 <- peajesconcesion %>%
  filter(cdoperador == 1, cdpunto_atencion == 1) %>%
  arrange(fecha)
head(datos_filtrados1)
# ===============================
# 3. VISUALIZACIÓN INICIAL
# ===============================
plot(datos_filtrados1$fecha,
     datos_filtrados1$recaudo_total_diario,
     type = "l",
     xlab = "Fecha",
     ylab = "Recaudo total diario",
     main = "Serie de tiempo del recaudo diario – Concesión 1")
# ===============================
# 4. OBJETO XTS (FECHAS REALES)
# ===============================
recaudo_xts1 <- xts(
  datos_filtrados1$recaudo_total_diario,
  order.by = datos_filtrados1$fecha
)
head(recaudo_xts1)
plot(recaudo_xts1,
     main = "Serie diaria de recaudo (xts)",
     ylab = "Recaudo")

recaudo_ts1 <- ts(
  datos_filtrados1$recaudo_total_diario,
  frequency = 7
)

head(recaudo_ts1)
plot(recaudo_ts1,
     main = "Serie diaria de recaudo (xts)",
     ylab = "Recaudo")
# ===============================
# 5. DIAGNÓSTICO
# ===============================
acf(coredata(recaudo_xts1), main = "ACF de la serie")
pacf(coredata(recaudo_xts1), main = "PACF de la serie")

# Prueba Dickey-Fuller
adf.test(coredata(recaudo_xts1))

# ===============================
# 6. MODELOS DE SERIES DE TIEMPO
# ===============================

# ---- ARIMA automático
fit_arima1 <- auto.arima(recaudo_xts1)
summary(fit_arima1)

checkresiduals(fit_arima1)

# ---- SARIMA (si hubiera estacionalidad)
fit_sarima1 <- auto.arima(recaudo_xts1, seasonal = TRUE)
summary(fit_sarima1)

# ---- ETS
fit_ets1 <- ets(recaudo_xts1)
summary(fit_ets1)

# ===============================
# 7. PRONÓSTICOS
# ===============================
h <- 7

forecast_arima1 <- forecast(fit_arima1, h = h)
forecast_sarima1 <- forecast(fit_sarima1, h = h)
forecast_ets1 <- forecast(fit_ets1, h = h)

plot(forecast_arima1,
     main = "Pronóstico ARIMA – Recaudo diario",
     ylab = "Recaudo")

plot(forecast_ets1,
     main = "Pronóstico ETS – Recaudo diario",
     ylab = "Recaudo")

# ===============================
# 8. MEDIDAS DE ERROR (COMPARACIÓN)
# ===============================
accuracy(forecast_arima1)
accuracy(forecast_sarima1)
accuracy(forecast_ets1)

# ===============================
# 9. REGRESIÓN POLINOMIAL (TENDENCIA)
# ===============================

# Índice temporal
datos_filtrados1$t <- seq_len(nrow(datos_filtrados1))

# Polinomio grado 2
modelo_poly2 <- lm(recaudo_total_diario ~ t + I(t^2),
                   data = datos_filtrados1)

summary(modelo_poly2)

# Polinomio grado 3
modelo_poly3 <- lm(recaudo_total_diario ~ t + I(t^2) + I(t^3),
                   data = datos_filtrados1)

summary(modelo_poly3)

# ===============================
# 10. GRÁFICA POLINOMIOS
# ===============================
plot(datos_filtrados1$t,
     datos_filtrados1$recaudo_total_diario,
     type = "l",
     col = "gray",
     main = "Regresión polinomial del recaudo",
     xlab = "Tiempo",
     ylab = "Recaudo")

lines(datos_filtrados1$t,
      fitted(modelo_poly2),
      lwd = 2)

lines(datos_filtrados1$t,
      fitted(modelo_poly3),
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("Datos reales", "Polinomio grado 2", "Polinomio grado 3"),
       lty = c(1,1,2),
       lwd = c(1,2,2),
       col = c("gray","black","black"))

# ===============================
# 11. PRONÓSTICO POLINOMIAL
# ===============================
t_futuro <- (max(datos_filtrados1$t) + 1):(max(datos_filtrados1$t) + h)

pred_poly2 <- predict(modelo_poly2,
                      newdata = data.frame(t = t_futuro),
                      interval = "confidence")

plot(pred_poly2)

###########################################################################
orden_dias <- c("Lunes","Martes","Miércoles",
                "Jueves","Viernes","Sábado","Domingo")

datos_filtrados1$dia_semana <- factor(
  datos_filtrados1$dia_semana,
  levels = orden_dias
)

xreg <- model.matrix(~ dia_semana, data = datos_filtrados1)

head(xreg)

# ==========================================
# 1. ORDENAR Y LIMPIAR DATOS
# ==========================================

# Orden cronológico
datos_filtrados1 <- datos_filtrados1[order(datos_filtrados1$fecha), ]

# Eliminar posibles NA
datos_filtrados1 <- na.omit(datos_filtrados1)


# ==========================================
# 2. CREAR VARIABLE DEPENDIENTE (y)
# ==========================================

y <- ts(
  datos_filtrados1$recaudo_total_diario,
  frequency = 7
)


# ==========================================
# 3. CREAR VARIABLES EXÓGENAS (DUMMIES)
# ==========================================

# Definir orden de días
orden_dias <- c("Lunes","Martes","Miércoles",
                "Jueves","Viernes","Sábado","Domingo")

# Convertir a factor ordenado
datos_filtrados1$dia_semana <- factor(
  datos_filtrados1$dia_semana,
  levels = orden_dias
)

# Crear matriz de regresores eliminando la categoría base (Lunes)
xreg <- model.matrix(~ dia_semana, data = datos_filtrados1)[, -1]


# ==========================================
# 4. VERIFICAR DIMENSIONES
# ==========================================

cat("Longitud de y:", length(y), "\n")
cat("Filas de xreg:", nrow(xreg), "\n")

# Deben ser exactamente iguales


# ==========================================
# 5. AJUSTAR MODELO ARIMA CON EXÓGENAS
# ==========================================

library(forecast)

modelo <- Arima(
  y,
  order = c(5,0,0),
  xreg = xreg,
  include.mean = TRUE
)

summary(modelo)


# ==========================================
# 6. VER MATRIZ DE REGRESORES
# ==========================================

head(xreg)

################################################################################

# ===============================
# 0. LIBRERÍAS
# ===============================
library(tidyverse)
library(xts)
library(forecast)
library(tseries)

# ===============================
# 1. CARGA Y LIMPIEZA DE DATOS
# ===============================
peajesconcesion <- read.csv("C:/Users/laura.toro/Downloads/bases por peaje con consecion.csv", sep=";")

# Conversión de fecha
peajesconcesion$fecha <- as.Date(peajesconcesion$fecha)

# Revisión básica
str(peajesconcesion)
head(peajesconcesion)

# ===============================
# 2. FILTRADO POR CONCESIÓN
# ===============================
datos_filtrados1 <- peajesconcesion %>%
  filter(cdoperador == 1, cdpunto_atencion == 3) %>%
  arrange(fecha)
head(datos_filtrados1)
# ===============================
# 3. VISUALIZACIÓN INICIAL
# ===============================
plot(datos_filtrados1$fecha,
     datos_filtrados1$recaudo_total_diario,
     type = "l",
     xlab = "Fecha",
     ylab = "Recaudo total diario",
     main = "Serie de tiempo del recaudo diario – Concesión 2")
# ===============================
# 4. OBJETO XTS (FECHAS REALES)
# ===============================
recaudo_xts1 <- xts(
  datos_filtrados1$recaudo_total_diario,
  order.by = datos_filtrados1$fecha
)
head(recaudo_xts1)
plot(recaudo_xts1,
     main = "Serie diaria de recaudo (xts)",
     ylab = "Recaudo")

recaudo_ts1 <- ts(
  datos_filtrados1$recaudo_total_diario,
  frequency = 7
)

head(recaudo_ts1)
plot(recaudo_ts1,
     main = "Serie diaria de recaudo (xts)",
     ylab = "Recaudo")
# ===============================
# 5. DIAGNÓSTICO
# ===============================
acf(coredata(recaudo_xts1), main = "ACF de la serie")
pacf(coredata(recaudo_xts1), main = "PACF de la serie")

# Prueba Dickey-Fuller
adf.test(coredata(recaudo_xts1))

# ===============================
# 6. MODELOS DE SERIES DE TIEMPO
# ===============================

# ---- ARIMA automático
fit_arima1 <- auto.arima(recaudo_xts1)
summary(fit_arima1)

checkresiduals(fit_arima1)

# ---- SARIMA (si hubiera estacionalidad)
fit_sarima1 <- auto.arima(recaudo_xts1, seasonal = TRUE)
summary(fit_sarima1)

# ---- ETS
fit_ets1 <- ets(recaudo_xts1)
summary(fit_ets1)

# ===============================
# 7. PRONÓSTICOS
# ===============================
h <- 7

forecast_arima1 <- forecast(fit_arima1, h = h)
forecast_sarima1 <- forecast(fit_sarima1, h = h)
forecast_ets1 <- forecast(fit_ets1, h = h)

plot(forecast_arima1,
     main = "Pronóstico ARIMA – Recaudo diario",
     ylab = "Recaudo")

plot(forecast_ets1,
     main = "Pronóstico ETS – Recaudo diario",
     ylab = "Recaudo")

# ===============================
# 8. MEDIDAS DE ERROR (COMPARACIÓN)
# ===============================
accuracy(forecast_arima1)
accuracy(forecast_sarima1)
accuracy(forecast_ets1)

# ===============================
# 9. REGRESIÓN POLINOMIAL (TENDENCIA)
# ===============================

# Índice temporal
datos_filtrados1$t <- seq_len(nrow(datos_filtrados1))

# Polinomio grado 2
modelo_poly2 <- lm(recaudo_total_diario ~ t + I(t^2),
                   data = datos_filtrados1)

summary(modelo_poly2)

# Polinomio grado 3
modelo_poly3 <- lm(recaudo_total_diario ~ t + I(t^2) + I(t^3),
                   data = datos_filtrados1)

summary(modelo_poly3)

# ===============================
# 10. GRÁFICA POLINOMIOS
# ===============================
plot(datos_filtrados1$t,
     datos_filtrados1$recaudo_total_diario,
     type = "l",
     col = "gray",
     main = "Regresión polinomial del recaudo",
     xlab = "Tiempo",
     ylab = "Recaudo")

lines(datos_filtrados1$t,
      fitted(modelo_poly2),
      lwd = 2)

lines(datos_filtrados1$t,
      fitted(modelo_poly3),
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("Datos reales", "Polinomio grado 2", "Polinomio grado 3"),
       lty = c(1,1,2),
       lwd = c(1,2,2),
       col = c("gray","black","black"))

# ===============================
# 11. PRONÓSTICO POLINOMIAL
# ===============================
t_futuro <- (max(datos_filtrados1$t) + 1):(max(datos_filtrados1$t) + h)

pred_poly2 <- predict(modelo_poly2,
                      newdata = data.frame(t = t_futuro),
                      interval = "confidence")

plot(pred_poly2)