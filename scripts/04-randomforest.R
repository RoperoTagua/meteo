library(randomForest)
library(caret)

#Preparacion datasets
df <- read.csv("./datos/temperaturas.csv")
df <- df[, -1]

df_rf <- df[15:nrow(df), ]
for(i in c(1:14)){
  df_rf[, paste0("val_", i)] <- df$min[i:(nrow(df_rf)+i-1)]
}

df_test <- df_rf[df_rf$dia >= as.Date("2021-01-01"), ]
df_train <- df_rf[df_rf$dia < as.Date("2021-01-01"), ]

#Entrenamiento del modelo
t <- Sys.time()
rf <- randomForest(min ~ .,  data=df_train[, c(2, 4:ncol(df_train))], 
                   ntree = 2500, mtry = c(1:15))
Sys.time() - t
#Time difference of 4.641956 mins

#Guardar y cargar modelo
saveRDS(rf, "./modelos/rf.rds")
rf <- readRDS("./modelos/rf.rds")

#Importancia de las variables
rf$importance

#Predicción
df_test$predict <- 0
for(i in c(1:304)){
  result <- predict(rf, df_test[i, ])
  df_test$predict[i] <- result
  df_test[i+1, 4:(ncol(df_test)-1)] <- c(df_test[i+1, 4:(ncol(df_test)-2)], result)
}
df_test <- df_test[1:304, ]

#Validacion
sqrt(mean((df_test$min - df_test$predict)^2))
sqrt(mean((df_test$min[1:90] - df_test$predict[1:90])^2))
sqrt(mean((df_test$min[1:30] - df_test$predict[1:30])^2))

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = predict, colour = "predict")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")

