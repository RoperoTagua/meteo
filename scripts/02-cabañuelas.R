library(ggplot2)

#Preparacion datasets
df <- read.csv("./datos/temperaturas.csv")
df <- df[, -1]

df_test <- df[df$dia >= as.Date("2021-01-01"), ]
df_train <- df[df$dia < as.Date("2021-01-01"), ]
df_cabanuelas <- read.xlsx("./datos/cabañuelas.xlsx", 1)[1:304, ]

df_test$predict <- as.numeric(as.character(df_cabanuelas$max))

#Validacion
sqrt(mean((df_test$min - df_test$predict)^2))
sqrt(mean((df_test$min[1:90] - df_test$predict[1:90])^2))
sqrt(mean((df_test$min[1:30] - df_test$predict[1:30])^2))

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = max, colour = "max")) + 
  geom_line(aes(y = cabanuelas, colour = "cab")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")


