library(ggplot2)

#Preparacion datasets
df_test <- df[df$dia >= as.Date("2021-01-01"), ]
df_train <- df[df$dia < as.Date("2021-01-01"), ]
df_cabañuelas <- read.xlsx("/home/developer/Documentos/proyectos/meteo/datos/cabañuelas.xlsx", 1)[1:304, ]

df_test$cabañuelas <- as.numeric(df_cabañuelas$max)

ggplot(df_test, aes(dia)) + 
  geom_line(aes(y = max, colour = "max")) + 
  geom_line(aes(y = cabañuelas, colour = "cab")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")

#ECM
sqrt(mean((df_test$max - df_test$cabañuelas)^2))

#Total
mean(df_test$cabañuelas - df_test$max)
mean(abs(df_test$cabañuelas - df_test$max))
var(abs(df_test$cabañuelas - df_test$max))

#Primer mes
mean(df_test$cabañuelas[1:30] - df_test$max[1:30])
mean(abs(df_test$cabañuelas[1:30] - df_test$max[1:30]))
var(abs(df_test$cabañuelas[1:30] - df_test$max[1:30]))

#Primer trimestre
mean(df_test$cabañuelas[1:90] - df_test$max[1:90])
mean(abs(df_test$cabañuelas[1:90] - df_test$max[1:90]))
var(abs(df_test$cabañuelas[1:30] - df_test$max[1:30]))
