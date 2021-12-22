library("xgboost")
library("tidyverse")
library("caret")

#Preparacion datasets
df <- read_df()
df_rf <- df[15:nrow(df), ]
for(i in c(1:14)){
  df_rf[, paste0("val_", i)] <- df$min[i:(nrow(df_rf)+i-1)]
}

df_test <- df_rf[df_rf$dia >= as.Date("2021-01-01"), ]
df_train <- df_rf[df_rf$dia < as.Date("2021-01-01"), ]


train_x = data.matrix(df_train[15000:nrow(df_train), c(4:ncol(df_train))])
train_y = df_train[15000:nrow(df_train), 2]

test_x = data.matrix(df_test[, c(4:ncol(df_test))])
test_y = df_test[, 2]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)


xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 500, eval_metric = "error")
pred_y = predict(xgbc, xgb_test)

df_test$predict <- pred_y
df_test$predict <- 0
for(i in c(1:304)){
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  result <- predict(xgbc, xgb_test)[i]
  df_test$predict[i] <- result
  if(i != 304){
    test_x[i+1, 1:14] <- c(test_x[i, 2:14], result)
  }
}

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = predict, colour = "predict")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")
