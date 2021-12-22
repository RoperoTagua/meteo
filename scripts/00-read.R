library(xlsx)

read_df <- function(){
  datos <- read.xlsx("./datos/temperaturas.xlsx", 3)
  
  df <- data.frame("dia" = 1, "min" = 1, "max" = 1)
  df <- df[-1, ]
  
  for(i in c(17:nrow(datos))){
    for(j in c(1:3)){
      x <- strsplit(as.character(datos[i,j]), " ")[[1]]
      for(k in c(1:length(x))){
        if(grepl("-19", x[k]) | grepl("-20", x[k])){
          print(x[k])
          nr <- nrow(df) + 1
          df[nr, 1] <- x[k]
          df[nr, 3] <- x[k+1]
          df[nr, 2] <- x[k+2]
        }
      }
    }
  }
  
  for(i in c(1:nrow(df))){
    if(grepl("-19", df$max[i]) | grepl("-20", df$max[i])){
      print(df$max[i])
      if(grepl("-19", df$max[i+1]) | grepl("-20", df$max[i+1])){
        df$max[i] <- df$max[i-1]
        df$min[i] <- df$min[i-1]
      }else{
        df$max[i] <- (as.numeric(df$max[i-1]) + as.numeric(df$max[i+1]))/2
        df$min[i] <- (as.numeric(df$min[i-1]) + as.numeric(df$min[i+1]))/2
      }
    }
  }
  
  df$min <- as.numeric(df$min)
  df$max <- as.numeric(df$max)
  df$dia = as.Date(df$dia, '%d-%m-%Y')
  
  return(df)
}


