library('forecast')
library('tseries')

#Datos
df <- read.csv("./datos/temperaturas.csv")
df <- df[, -1]

#Preparacion datasets
df_test <- df[df$dia >= as.Date("2021-01-01"), ]
df_train <- df[df$dia < as.Date("2021-01-01"), ]

#Descomposición de la serie
min_ts = ts(na.omit(df_train$min), frequency=365)
decomp = stl(min_ts, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

#Hipotesis estacionalidad
adf.test(min_ts, alternative = "stationary")
Acf(min_ts, main='')
Pacf(min_ts, main='')

#Hipotesis estacionalidad sobre estacionalidad
count_d1 = diff(deseasonal, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Entrenamiento del modelo
t <- Sys.time()
modeloarima <- auto.arima(deseasonal, seasonal=FALSE)
Sys.time() - t
modeloarima

tsdisplay(residuals(modeloarima), lag.max=10, main='(3,1,1) Model Residuals')

#Predicción
prediccion <- forecast(modeloarima, h=365)
plot(prediccion)

result <- prediccion$mean + decomp$time.series[1:365, 1]
df_test$predict <- result[1:304]

#Validacion
sqrt(mean((df_test$min - df_test$predict)^2))
sqrt(mean((df_test$min[1:90] - df_test$predict[1:90])^2))
sqrt(mean((df_test$min[1:30] - df_test$predict[1:30])^2))

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = arima, colour = "max")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")


