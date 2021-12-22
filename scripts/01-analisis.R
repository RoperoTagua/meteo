library('ggplot2')
library('dplyr')

#Datos
df <- read.csv("./datos/temperaturas.csv")
df <- df[, -1]
df$min <- as.numeric(df$min)
df$max <- as.numeric(df$max)
df$dia = as.Date(df$dia, '%Y-%m-%d')

#Dias
nrow(df)
min(df$dia)
max(df$dia)
summary(df)

#Representacion datos
ggplot(df, aes(dia)) + 
  geom_line(aes(y = min, colour = "min")) + 
  geom_line(aes(y = max, colour = "max")) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none")

#Minimos y maximos
View(df[order(df$min)[1:10], ])
View(df[order(df$min, decreasing = TRUE)[1:10], ])

View(df[order(df$max)[1:10], ])
View(df[order(df$max, decreasing = TRUE)[1:10], ])

#Media anual
df$year <- format(df$dia,"%Y")
dfg_year <- df %>% group_by(year) %>% summarise(min_min = min(min), max_min = max(min),
                                   min_max = min(max), max_max = max(max),
                                   mean_min = mean(min), mean_max = mean(max))

dfg_year <- as.data.frame(dfg_year)
ggplot(dfg_year, aes(year)) + 
  geom_line(aes(y = min_min, colour = "min_min", group = 1)) + 
  geom_line(aes(y = max_min, colour = "max_min", group = 1)) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.1))

ggplot(dfg_year, aes(year)) + 
  geom_line(aes(y = min_max, colour = "min_max", group = 1)) + 
  geom_line(aes(y = max_max, colour = "max_max", group = 1)) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.1))

ggplot(dfg_year, aes(year)) + 
  geom_line(aes(y = mean_min, colour = "mean_min", group = 1)) + 
  geom_line(aes(y = mean_max, colour = "mean_max", group = 1)) +
  ylab("Temperatura") + xlab("Fecha") +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.1))


