library(randomForest)
library(caret)

#Preparacion datasets
df <- read_df()
df_rf <- df[15:nrow(df), ]
for(i in c(1:14)){
  df_rf[, paste0("val_", i)] <- df$min[i:(nrow(df_rf)+i-1)]
}

df_test <- df_rf[df_rf$dia >= as.Date("2021-01-01"), ]
df_train <- df_rf[df_rf$dia < as.Date("2021-01-01"), ]

#Modelo
rf <- randomForest(min ~ .,  data=df_train[15000:nrow(df_train), c(2, 4:ncol(df_train))], ntree = 2500, mtry = c(1:15))
rf$importance

df_test$predict <- 0
for(i in c(1:304)){
  result <- predict(rf, df_test[i, ])
  df_test$predict[i] <- result
  df_test[i+1, 4:(ncol(df_test)-1)] <- c(df_test[i+1, 4:(ncol(df_test)-2)], result)
}
df_test <- df_test[1:304, ]

df_test$predict <- predict(rf, df_test)

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = predict, colour = "predict")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")

#Total
mean(df_test$predict - df_test$min)
mean(abs(df_test$predict - df_test$min))
var(abs(df_test$predict - df_test$min))

#Primer mes
mean(df_test$predict[1:30] - df_test$min[1:30])
mean(abs(df_test$predict[1:30] - df_test$min[1:30]))
var(abs(df_test$predict[1:30] - df_test$min[1:30]))

#Primer trimestre
mean(df_test$predict[1:90] - df_test$min[1:90])
mean(abs(df_test$predict[1:90] - df_test$min[1:90]))
var(abs(df_test$predict[1:30] - df_test$min[1:30]))


# train model
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


metric <- "BIC"
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(23)
custom <- train(min~., data=df_train[15000:nrow(df_train), c(2, 4:ncol(df_train))], method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)





