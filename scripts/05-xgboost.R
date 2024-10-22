library("xgboost")
library("tidyverse")
library("caret")

#Preparacion datasets
df <- read.csv("./datos/temperaturas.csv")
df <- df[, -1]

df_rf <- df[15:nrow(df), ]
for(i in c(1:14)){
  df_rf[, paste0("val_", i)] <- df$min[i:(nrow(df_rf)+i-1)]
}

df_test <- df_rf[df_rf$dia >= as.Date("2021-01-01"), ]
df_train <- df_rf[df_rf$dia < as.Date("2021-01-01"), ]


train_x = data.matrix(df_train[1:nrow(df_train), c(4:ncol(df_train))])
train_y = df_train[1:nrow(df_train), 2]

test_x = data.matrix(df_test[, c(4:ncol(df_test))])
test_y = df_test[, 2]

t <- Sys.time()
xgb <-  xgboost(train_x, label = train_y,
    nrounds = 50000, objective = "reg:squarederror",
    early_stopping_rounds = 3, max_depth = 3, eta = .25)   
Sys.time() - t
#Time difference of 3.470281 mins

#Guardar y cargar modelo
saveRDS(xgb, "./modelos/xgb.rds")
xgb <- readRDS("./modelos/xgb.rds")

#Predicción
df_test$predict <- 0
for(i in c(1:304)){
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  result <- predict(xgb, xgb_test)[i]
  df_test$predict[i] <- result
  if(i != 304){
    test_x[i+1, 1:14] <- c(test_x[i, 2:14], result)
  }
}

#Validacion
sqrt(mean((df_test$min - df_test$predict)^2))
sqrt(mean((df_test$min[1:90] - df_test$predict[1:90])^2))
sqrt(mean((df_test$min[1:30] - df_test$predict[1:30])^2))

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = predict, colour = "predict")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")
