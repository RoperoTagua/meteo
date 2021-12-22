library(keras)
library(tensorflow)
library(dplyr)
library(tidyverse)

t <- Sys.time()
install_keras()
Sys.time() - t

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
train_y = data.matrix(df_train[1:nrow(df_train), 2])

test_x = data.matrix(df_test[, c(4:ncol(df_test))])

t <- Sys.time()
model <- keras_model_sequential() 
Sys.time() - t

model %>% 
  layer_dense(units = 256, activation = 'sigmoid', input_shape = c(14)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adam",
  metrics = list("mean_absolute_error")
)

t <- Sys.time()
history <- model %>% fit(
  train_x, train_y, 
  epochs = 3000, batch_size = 128, 
  validation_split = 0.2
)
Sys.time() - t
#Time difference of 19.02275 mins

#Guardar y cargar modelo
saveRDS(xgb, "./modelos/rn.rds")
model <- readRDS("./modelos/rn.rds")

#Predicción
df_test$predict <- 0
for(i in c(1:304)){
  result <- predict(model, test_x)[i]
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















