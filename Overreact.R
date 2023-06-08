# ========================================================== #
#               Does the Stock Market Overreact?             #
# Curso: Econometría Financiera                              #
# Profesora : Yessenia Portal                                #
# ========================================================== #
# Universidad Nacional Mayor de San Marcos                   #
# Facultad de Ciencias Económicas                            #
# Círculo de Estidios Financieros y del Mercado de Capitales #
# Daniel Ayquipa, Gian Franco Huamán, Frank Vidal            #
# ========================================================== #

cat("\f") # Limpia la consola
rm(list = ls()) # Limpia el Environment

# Instalamos los packages
# install.packages("quantmod")
# install.packages("PerformanceAnalytics")
# install.packages("dplyr")
# install.packages("tidyverse") 
# install.packages("timetk")
# install.packages("tidyquant")
# install.packages("broom")
# install.packages("openxlsx")
# install.packages("lubridate")
# install.packages("reshape2")

# Activamos los packages
library(quantmod)  
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(timetk)
library(tidyquant)
library(broom)
library(openxlsx)
library(lubridate)
library(reshape2)

# Seteamos la ruta en la que encontraremos la data
setwd("G:/")

# Importamos la data
# ------------------
# 1. Utilizaremos la función read.xlsx para poder importar la data de excel
# 2. Con la función colnames le daremos nombre a la matriz con la data de los precios
# 3. Luego la función xts nos permitirá convertir el objeto "Stocks_NYSE" en un objeto time series
# 4. Con la función storage.mode podemos convertir la data en un objeto con contenido "double" (numerico)
# Acciones
Stocks_NYSE <- read.xlsx("NYSE.xlsx", "Data")
colnames(Stocks_NYSE) <- Stocks_NYSE[6,]
Stocks_NYSE <- Stocks_NYSE[-c(1:8),]
Stocks_NYSE <- xts(Stocks_NYSE, order.by = as.Date(as.numeric(Stocks_NYSE$INST), origin = "1899-12-30"))[,-1]
storage.mode(Stocks_NYSE) <- "double"
# Indice
Market_NYSE <- read.xlsx("NYSE.xlsx", "Index")
colnames(Market_NYSE) <- Market_NYSE[6,]
Market_NYSE <- Market_NYSE[-c(1:8),]
Market_NYSE <- xts(Market_NYSE, order.by = as.Date(as.numeric(Market_NYSE$INST), origin = "1899-12-30"))[,-1]
storage.mode(Market_NYSE) <- "double"

# Construimos los retornos
# ------------------------
# 1. Con la función "return.calculate" podemos convertir los precios en retornos.
# 2. Con la función "na.omit" vamos a eliminar los elementos del tipo "NA" que resulten de utilizar el 
# comando Return.calculate
# Acciones
Stocks_NYSE_returns <- Stocks_NYSE%>%
                       Return.calculate(., method = "log") %>%
                       na.omit()
  
Stocks_NYSE_returns <- data.frame(index(Stocks_NYSE_returns),Stocks_NYSE_returns) %>% 
                       as_tibble() %>%
                       rename(Date = index.Stocks_NYSE_returns.)  

# Indice
Market_NYSE_returns <- Market_NYSE%>%
                       Return.calculate(., method = "log") %>%
                       na.omit() 

Market_NYSE_returns <- data.frame(index(Market_NYSE_returns),Market_NYSE_returns) %>% 
                       as_tibble() %>%
                       rename(Date = index.Market_NYSE_returns.)  
      

# Limpiamos y unimos la data
# --------------------------
# 1. Vamos a quitar las últimas fechas de la data... con el fin de uniformizar la información, es decir,
# tener el mismo rango de fechas para ambas matrices de data (Acciones del NYSE y el indice de mercado)
rows_delete <- nrow(Market_NYSE_returns) - nrow(Stocks_NYSE_returns)
Market_NYSE_returns <- Market_NYSE_returns[-c(1:rows_delete),]

# 2. Uniremos ambas matrices con el comando "data.frame":
Total_data_returns <- data.frame(Stocks_NYSE_returns,Market_NYSE_returns$SPDR.NYSE.Technology.ETF)

# Creamos una matriz "residuals"
# ------------------------------
# 1. Creamos una matriz donde más adelante llenaremos la información de los residuos de regresionar cada una de las
# acciones frente al indice del mercado (esta matriz tendrá el mismo rango de fechas que el objeto Total_data_returns):
residuals <- data.frame(Total_data_returns$Date)
colnames(residuals) <- c("Date")

# Regresión de acciones vs indice del mercado
# -------------------------------------------
# 1. Con este loop queremos agrugar en una matriz los residuos que obtuvimos luego de hacer una regresión entre cada una
# de las acciones frente al índice de mercado.
for(i in 2:(ncol(Total_data_returns)-1)){
res.regression <- Total_data_returns %>%
                  do(model = lm(as.vector(Total_data_returns[,i]) ~ Total_data_returns$Market_NYSE_returns.SPDR.NYSE.Technology.ETF)) %>%
                  augment(model) %>%  
                  select(.resid) %>%
                  data.frame(.)
residuals <- data.frame(residuals,res.regression$.resid)
} 

# 2. Ok, tenemos todos los residuos agrupados en una gran matriz. Ahora lo que debemos hacer es acumularlos y luego
# rankearlos, si?
# Para hacer esto, hay un paso muy importante... Dado que tenemos que hacer una suma de residuos rolling, tenemos que
# trabajar con un loop. ¿Por qué? Porque esto nos facilitará la suma de las filas de la matriz.
# Empecemos:
# ---------
# Contemos el número de instrumentos que tenemos en la matriz:
n_inst <- ncol(residuals)-1
# Contemos el número de meses que tenemos en la matriz:
n_months <- nrow(residuals)
# Ahora creemos las matrices donde guardaremos los "t-36" residuos acumulados:
cum_resid_1 <- residuals[1:31,]
cum_resid_2 <- cum_resid_1
cum_resid_3 <- cum_resid_2
cum_resid_4 <- cum_resid_3

# Lo que haremos nosotros es sumar los 31 residuos anteriores al tiempo "t" de formación
# del portafolio "winners" y "losers"    
n_reb_1 <- n_months/5
n_reb_2 <- n_reb_1*2
n_reb_3 <- n_reb_1*3
n_reb_4 <- n_reb_1*4

# Armamos nuestras matrices de residuos acumulados 31 días atrás.
# Matrices

for(i in (n_reb_1+1):((n_reb_1*2))){      
  for(j in 2:(n_inst+1)){
    cum_resid_1[(i-n_reb_1),1] <- residuals[(n_reb_1+1),1]
    cum_resid_1[(i-n_reb_1),j] <-  sum(as.vector(residuals[c((i-n_reb_1):(i-1)),j]))
  }
}

# Matriz: cum_resid_2 
for(i in (n_reb_2+1):((n_reb_1*3))){      
  for(j in 2:(n_inst+1)){
    cum_resid_2[(i-n_reb_2),1] <- residuals[(n_reb_2+1),1]
    cum_resid_2[(i-n_reb_2),j] <-  sum(as.vector(residuals[c((i-n_reb_2):(i-1)),j]))
  }
}

# Matriz: cum_resid_3 
for(i in (n_reb_3+1):((n_reb_1*4))){      
  for(j in 2:(n_inst+1)){
    cum_resid_3[(i-n_reb_3),1] <- residuals[(n_reb_3+1),1]
    cum_resid_3[(i-n_reb_3),j] <-  sum(as.vector(residuals[c((i-n_reb_3):(i-1)),j]))
  }
}

# Matriz: cum_resid_4 
for(i in (n_reb_4+1):((n_reb_1*5))){      
  for(j in 2:(n_inst+1)){
    cum_resid_4[(i-n_reb_4),1] <- residuals[(n_reb_4+1),1]
    cum_resid_4[(i-n_reb_4),j] <-  sum(as.vector(residuals[c((i-n_reb_4):(i-1)),j]))
  }
}

# Ya tenemos la suma de los "t-31" residuos (rolling) en una matriz.
# Ahora debemos rankear "cum_resid_1"
rank_cum_resid_1 <- rank(cum_resid_1[1,c(2:n_inst)])%>%
                    t() %>%
                    t() 
rank_cum_resid_1 <- data.frame(rownames(rank_cum_resid_1),rank_cum_resid_1)
winners_port_names_1 <- filter(rank_cum_resid_1, rank_cum_resid_1 >= 1 & rank_cum_resid_1 <= 35)[,1]
losers_port_names_1 <- filter(rank_cum_resid_1, rank_cum_resid_1 >= 36 & rank_cum_resid_1 <= 70)[,1]
# Ahora debemos rankear "cum_resid_2"
rank_cum_resid_2 <- rank(cum_resid_2[1,c(2:n_inst)])%>%
                    t() %>%
                    t() 
rank_cum_resid_2 <- data.frame(rownames(rank_cum_resid_2),rank_cum_resid_2)
winners_port_names_2 <- filter(rank_cum_resid_2, rank_cum_resid_2 >= 1 & rank_cum_resid_2 <= 35)[,1]
losers_port_names_2 <- filter(rank_cum_resid_2, rank_cum_resid_2 >= 36 & rank_cum_resid_2 <= 70)[,1]
# Ahora debemos rankear "cum_resid_3"
rank_cum_resid_3 <- rank(cum_resid_3[1,c(2:n_inst)])%>%
  t() %>%
  t() 
rank_cum_resid_3 <- data.frame(rownames(rank_cum_resid_3),rank_cum_resid_3)
winners_port_names_3 <- filter(rank_cum_resid_3, rank_cum_resid_3 >= 1 & rank_cum_resid_3 <= 35)[,1]
losers_port_names_3 <- filter(rank_cum_resid_3, rank_cum_resid_3 >= 36 & rank_cum_resid_3 <= 70)[,1]
# Ahora debemos rankear "cum_resid_4"
rank_cum_resid_4 <- rank(cum_resid_4[1,c(2:n_inst)])%>%
  t() %>%
  t() 
rank_cum_resid_4 <- data.frame(rownames(rank_cum_resid_4),rank_cum_resid_4)
winners_port_names_4 <- filter(rank_cum_resid_4, rank_cum_resid_4 >= 1 & rank_cum_resid_4 <= 35)[,1]
losers_port_names_4 <- filter(rank_cum_resid_4, rank_cum_resid_4 >= 36 & rank_cum_resid_4 <= 70)[,1]

##################################
# Elección del nuevo portafolio
# 1er portafolio
res_cum_winners_1 <- residuals[32:62,winners_port_names_1]
res_cum_losers_1 <- residuals[32:62,losers_port_names_1]
# 2do portafolio
res_cum_winners_2 <- residuals[63:93,winners_port_names_2]
res_cum_losers_2 <- residuals[63:93,losers_port_names_2]
# 3er portafolio
res_cum_winners_3 <- residuals[94:124,winners_port_names_3]
res_cum_losers_3 <- residuals[94:124,losers_port_names_3]
# 4to portafolio
res_cum_winners_4 <- residuals[125:155,winners_port_names_4]
res_cum_losers_4 <- residuals[125:155,losers_port_names_4]

# 3.
# Seleccionamos el vector con los pesos (equally weighted)
w <- 100/35
weights <- rep(w,35)

# Ponderamos excess return por weights
res_port_winners_1 <- rowSums(res_cum_winners_1[,c(2:ncol(res_cum_winners_1))] * weights) %>%
                      data.frame()  
res_port_losers_1 <- rowSums(res_cum_losers_1[,c(2:ncol(res_cum_losers_1))] * weights) %>%
                      data.frame()
res_port_winners_2 <- rowSums(res_cum_winners_1[,c(2:ncol(res_cum_winners_2))] * weights) %>%
                      data.frame()  
res_port_losers_2 <- rowSums(res_cum_losers_1[,c(2:ncol(res_cum_losers_2))] * weights) %>%
                      data.frame()
res_port_winners_3 <- rowSums(res_cum_winners_1[,c(2:ncol(res_cum_winners_3))] * weights) %>%
                      data.frame()  
res_port_losers_3 <- rowSums(res_cum_losers_1[,c(2:ncol(res_cum_losers_3))] * weights) %>%
                      data.frame()
res_port_winners_4 <- rowSums(res_cum_winners_1[,c(2:ncol(res_cum_winners_4))] * weights) %>%
                      data.frame()  
res_port_losers_4 <- rowSums(res_cum_losers_1[,c(2:ncol(res_cum_losers_4))] * weights) %>%
                      data.frame()

# Crearemos un objeto con que tenga el número de observaciones para juntar todos 
# los objetos anteriores:
total_res_port_winners <- data.frame(residuals$Date[32:155],residuals$res.regression..resid[32:155])
total_res_port_winners[1:31,2] <- res_port_winners_1 
total_res_port_winners[32:62,2] <- res_port_winners_2
total_res_port_winners[63:93,2] <- res_port_winners_2
total_res_port_winners[94:124,2] <- res_port_winners_2

total_res_port_losers <- data.frame(residuals$Date[32:155],residuals$res.regression..resid[32:155])
total_res_port_losers[1:31,2] <- res_port_losers_1 
total_res_port_losers[32:62,2] <- res_port_losers_2
total_res_port_losers[63:93,2] <- res_port_losers_2
total_res_port_losers[94:124,2] <- res_port_losers_2

# Acumulamos los excess return ponderados por weights
# ===================================================
# Winner portfolio
for(i in 2:nrow(total_res_port_winners)){
  total_res_port_winners[i,2] <- total_res_port_winners[i,2] + total_res_port_winners[i-1,2]
}
colnames(total_res_port_winners) <- c("Date","cum_res")

# Loser portfolio
for(i in 2:nrow(total_res_port_losers)){
  total_res_port_losers[i,2] <- total_res_port_losers[i,2] + total_res_port_losers[i-1,2]
}
colnames(total_res_port_losers) <- c("Date","cum_res")

# Agregamos los objetos en una sola matriz
Total_portfolios <- data.frame(total_res_port_losers$Date,total_res_port_losers$cum_res,total_res_port_winners$cum_res)
colnames(Total_portfolios) <- c("Dates","Losers","Winners")

# Ploteamos el resultado: Winner Portfolio vs Loser Portfolio
Portfolios <- melt(data = Total_portfolios,
                   id.vars = "Dates",
                   measure.vars = c("Losers", "Winners"))

ggplot(data = Portfolios,
       aes(x=Dates, y=value, colour=variable)) + 
       geom_line()+
       ggtitle("Loser Portfolio vs Winner Portfolio")