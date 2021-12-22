library('forecast')
library('tseries')

arima_pred <- function(df_train, df_test, nfreq, value){
  
  #Descomposición de la serie
  dat_ts = ts(na.omit(df_train[, value]), frequency=365)
  decomp = stl(dat_ts, s.window="periodic")
  deseasonal <- seasadj(decomp)
  
  #Hipotesis de estacionalidad
  print(adf.test(dat_ts, alternative = "stationary"))
  
  #Entrenamiento del modelo
  modeloarima <- auto.arima(deseasonal, seasonal=FALSE)
  
  #Predicción
  prediccion <- forecast(modeloarima, h=nrow(df_test))
  
  #Validación
  result <- prediccion$mean + decomp$time.series[1:nrow(df_test), 1]
  df_test$arima <- result
  
  df_test$value <- df_test[, value]
  
  print(ggplot(df_test, aes(dia)) + 
    geom_line(aes(y = value, colour = "min")) + 
    geom_line(aes(y = arima, colour = "max")) +
    ylab("Temperatura") + xlab("Fecha") +
    theme(legend.position="none"))
  
  val11 <- round(mean(df_test$arima - df_test[, value]), 4)
  val12 <- round(mean(abs(df_test$arima - df_test[, value])), 4)
  val13 <- round(var((df_test$arima - df_test[, value])), 4)
  
  val21 <- round(mean(df_test$arima[1:30] - df_test[, value][1:30]), 4)
  val22 <- round(mean(abs(df_test$arima[1:30] - df_test[, value][1:30])), 4)
  val23 <- round(var((df_test$arima[1:30] - df_test[, value][1:30])), 4)
  
  val31 <- round(mean(df_test$arima[1:90] - df_test[, value][1:90]), 4)
  val32 <- round(mean(abs(df_test$arima[1:90] - df_test[, value][1:90])), 4)
  val33 <- round(var((df_test$arima[1:90] - df_test[, value][1:90])), 4)
  
  cat(paste0("Error medio: \n Total: ", val11, 
             " \n Primer Mes: ", val21,
             " \n Primer Trimestre: ", val31, "\n",
             "Error medio Absoluto: \n Total: ", val12, 
             " \n Primer Mes: ", val22,
             " \n Primer Trimestre: ", val32, "\n",
             "Varianza: \n Total: ", val13, 
             " \n Primer Mes: ", val23,
             " \n Primer Trimestre: ", val33, "\n"))
}

