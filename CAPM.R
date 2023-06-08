# ========================================================== #
#                          CAPM                              #
# Curso: Gestión de Riesgos                                  #
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

# Activamos los packages
library(quantmod)  
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(timetk)
library(tidyquant)
library(broom)

# Creamos un objeto con nombres de ETFs que queremos importar de yahoo,
# Esto va a ser útil para importar la data de yahoo finance, más adelante
symbols <- c("SPY","EFA","IJS","EEM","AGG")
# SPY: SPDR S&P 500 ETF
# EFA: iShares MSCI EAFE ETF
# IJS: iShares S&P Small-Cap 600 Value ETF
# EEM: iShares MSCI Emerging Markets ETF
# AGG: iShares Core U.S. Aggregate Bond ETF

# Importamos la data de yahoo finance
# 1. Con la función getSymbols extraemos la data de Yahoo Finance
# definiendo una fecha inicial y final.
# 2. Con la función map(~Ad(get(.))), buscamos extraer la los precios de
# la columna "Adjusted" de todos los instrumentos
# 3. Con la función reduce(merge), queremos juntar todos los precios extraídos
# en una matriz, ordenados por la fecha que corresponde.
# 4. Con la función colnames, cambiamos el nombre de cada columna por los
# nombres que tenemos en nuestro objeto "symbols"
prices <- getSymbols(symbols,
                     src ='yahoo',
                     from = "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = TRUE,
                     warnings = FALSE) %>%
          map(~Ad(get(.))) %>%
          reduce(merge) %>%
          `colnames<-`(symbols)

# Con la función "head" mostramos las 6 primeras observacion es del objeto prices
head(prices)

# Ok, hemos descargado la data de los instrumentos que están en el objeto
# "symbols", pero es diaria... ¿cómo lo convertimos en data mensual?
# 1. Con la función to.montly, indexAt = 'lastof', extraemos el último dato
# de cada mes, OHLC significa "Open", "High", "Low", "Close", ponemos "=FALSE"
# porque el nombre de las columnas del objeto son otros.
# 2. Con la función data.frame(), convertimos el objeto en uno
# data.frame, con la función date=index(.) estamos agregando la columna inicial
# donde están las fechas al final del objeto data.frame
# 3. Con la funcion remove_rownames(), estamos quitando el nombre de las 
# filas.
# 4. Con la función gather(asset, prices, -date):
# Estamos ordenando la matriz, de tal forma que, "-date" indica la primera
# columna, "asset", la segunda columna y "prices", la tercera columna.
# 5. Con la función group_by(asset), convertiremos el objeto en uno "tibble"
# agrupandolo por "asset"
# 6. Con la función mutate(returns = log(prices)-log(lag(prices))), estamos
# agregando una columna más al objeto anterior llamada "returns" que contiene
# los retornos de los precios de cada uno de los instrumentos del objeto "prices"
# 7. Con la función select(-prices) estamos suprimiendo la columna "prices", y
# nos quedamos solo con la fecha, asset y los retornos.
# 8. Con la función spread(asset, returns) estamos separando la columna anterior
# donde aparecían tooooodos los assets juntos, en una columna por cada uno (Cada uno
# de ellos mostrará su respectivo retorno).
# 9. Con la función select, estamos ordenando la matriz de acuerdo al orden
# de los instrumentos en el objeto symbols.
# 10. Con la funcion na.omit(), estamos suprimiento los valores NA.
# En este caso, están en la primera fila porque al nosotros sacar los retornos
# utilizamos la fórmula (Pt/Pt-1)-1... cierto? Pero la primera fila, no tenía una fila
# t-1.... entonces ahí se generó ese "NA".
asset_returns_dplyr_byhand <- prices %>%
                              to.monthly(indexAt = 'lastof',
                                         OHLC = FALSE) %>%
                              data.frame(date=index(.)) %>%
                              remove_rownames() %>%
                              gather(asset, prices, -date) %>%
                              group_by(asset) %>%
                              mutate(returns = log(prices)-log(lag(prices))) %>%
                              select(-prices)%>%
                              spread(asset, returns) %>%
                              select(date, symbols)%>%
                              na.omit()
  
# El objeto "asset_returns_dplyr_byhand" tenía todos los instrumentos por separado en cada columna
# Vamos a generar un objeto "asset_returns_long", de la siguiente manera:
# 1. Con la función gather(asset, returns, -date) estamos reordenando el objeto, de tal forma que,
# "-date" indica la columna inicial, "asset" indica la segunda columna donde estarán todos los instrumentos
# "returns" indica el retorno de cada instrumento de acuerdo a la fecha en que se encuentra.
asset_returns_long <- asset_returns_dplyr_byhand %>%
                      gather(asset, returns, -date) %>%
                      group_by(asset)
# Para qué hicimos esto?... Para generar los retornos de nuestro portafolio

# Ok... pero cuánto peso le colocamos a cada uno de nuestros instrumentos?
# Agregaremos un vector con los "pesos" (En inglés, "weights")... con el peso que cada
# instrumento del objeto "symbols" tendrá dentro del portafolio. 
w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

# Ya tenemos los retornos mensuales por instrumento y los pesos que cada uno tendrá en el portafolio
# ¿Qué les parece si obtenemos los retornos del portafolio?
# Para esto, utilizaremos el package "tidyquant", Ok? En él hay una función llamada
# "tq_portfolio" que nos ayudará a obtener los retornos del portafolio.
# asset_col = Nos indica que del objeto "asset_returns_long" vamos a indicar la columna con los "assets"
# returns_col = Nos indica que del objeto "asset_returns_long" vamos a indicar la columna con los "returns"
# weights = "w" ... es el vector con los pesos que definimos antes, recuerdan?
# col_rename = "returns"... es el nombre que le daremos a los retornos... en este caso "returns" 
# rebalance_on = "months"... con esto indicamos que queremos un rebalanceo mensual, esto significa que,
# se reasignan los pesos mes a mes aleatoriamente entre los instrumentos del portafolio.
portfolio_returns_tq_rebalanced_monthly <-  asset_returns_long %>%
                                            tq_portfolio(assets_col = asset,
                                                         returns_col = returns,
                                                         weights = w,
                                                         col_rename = "returns",
                                                         rebalance_on = "months")
# Listo, tenemos los retornos del portafolio...
# Ahora queremos obtener el "BETA del modelo CAPM"
# ¿Qué nos faltaría?
# Pues... los retornos mensuales del mercado, cierto?
# Entonces, hay que generarlos:

# Importamos la data de yahoo finance
# 1. Con la función getSymbols extraemos la data de Yahoo Finance
# definiendo una fecha inicial y final 
# 2. Con la función map(~Ad(get(.))), buscamos extraer la los precios de
# la columna "Adjusted" del SPY
# 3. Con la función reduce(merge), queremos juntar todos los precios extraídos
# en una matriz, ordenados por la fecha que corresponde.
# 4. Con la función colnames, cambiamos el nombre por "SPY"
# 5. Con la función to.montly, indexAt = 'lastof', extraemos el último dato
# de cada mes, OHLC significa "Open", "High", "Low", "Close", ponemos "=FALSE"
# porque el nombre de las columnas del objeto son otros.
# 6. Con la función Return.calculate(.,method="log") ... del package "PerformanceAnalytics"
# podemos convertir los precios en retornos.
# 7. Con la funcion na.omit(), estamos suprimiento los valores NA.
# En este caso, están en la primera fila porque al nosotros sacar los retornos 
# utilizamos la fórmula log(Pt/Pt-1)... cierto? Pero la primera fila, no tenía una fila
# t-1.... entonces ahí se generó ese "NA".
market_returns_xts <- getSymbols("SPY",
                                 src = 'yahoo',
                                 from = "2012-12-31",
                                 to = "2017-12-31",
                                 auto.assign = TRUE,
                                 warnings = FALSE) %>%
                      map(~Ad(get(.))) %>%  
                      reduce(merge) %>%
                      `colnames<-`("SPY") %>%
                      to.monthly(indexAt = "lastof",
                                 OHLC = FALSE) %>%
                      Return.calculate(.,method = "log") %>%
                      na.omit()

# Convertimos el objeto de "xts" a "tibble" para usar "tidyverse" y le asignaremos el nombre 
# "market_returns_tidy": 
# 1. Con la función tk_tbl, queremos convertir el objeto "xts" en "tibble"
# utilizamos "preserve_index=TRUE" porque quremos mantener la columna que contiene las fechas
# bajo la forma de "nombres de filas". Además, utilizamos "rename_index=date" para darle
# el nombre "date"
# 2. Con la función na.omit(), suprimimos los NAs. 
# 3. Con la función select(date, returns = SPY), estamos ordenando la matriz de tal forma que
# la primera columna es "date" y la segunda "returns"... (Se coloca returns = SPY, para hacer
# un cambio de nombre).
market_returns_tidy <- market_returns_xts %>%
                       tk_tbl(preserve_index = TRUE,
                              rename_index = "date") %>%
                       na.omit() %>%
                       select(date, returns = SPY)
# Listo, tenemos los retornos del mercado.

# Ahora, ¿Qué les parece si creamos un objeto en el que por fecha se muestre
# el retorno mensual de nuestro portafolio y el del mercado.
# Hagámoslo!
# 1. Con la función mutate(market_returns = market_returns_tidy$returns), estamos agregando
# una columna con el nombre "market_returns" en el que se muestran los retornos del mercado
# 2. Con la función head() mostramos los retultados en la consola.
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  head()
# En los primeros 6 datos, podemos ver que no nos ha ido muy bien frente al mercado :c

# Recordemos la fórmula del "Beta del modelo CAPM":
# cOVARIANZA(Retornos del portafolio, Retornos del mercado)/ VARIANZA(Retornos del mercado)
# Calculemoslo!
beta_CAPM <- cov(portfolio_returns_tq_rebalanced_monthly$returns,
             market_returns_tidy$returns)/var(market_returns_tidy$returns)

# ¿Qué les parece si calculamos el beta de cada uno de los instrumentos del portafolio?
# Veamoslos!
# Partiremos con el objeto "asset_returns_long", que tenía tres columnas: dates, assets, returns
# 1. Con la función nest estamos agrupando la info por tipo de asset... en él se tendrá
# la fecha y el retorno.
# 2. Con la función "mutate" vamos a agregar una columna más en donde efetivamente estarán la regresión
# Entre el los retornos del instrumento (individualmente) frente al mercado.
# 3. Con la función unnest(model),  desagrupamos la columna "modelo" de cada instrumento
# que contiene el coeficiente de la regresión, el error estándar, el t-esdístico y el p-value
# aqui estamos incluyendo una función "tidy" que nos permite limpiar la data... esto es,
# resume la información sobre los componentes de un modelo tal como coeficiente de una regresión
# error estandar, t-estadístico y p-value.
# 4. Con la función filter, vamos a quitar los interceptos de la regresión.
# 5. Con la función select(-term), suprimimos la columna "term"
# 6. Usamos mutate.if(is.numeric, funs(round(., 4))), ¿qué queremos decir?
# que si es valor es un número, lo redondee a 4 decimales
beta_assets <-  asset_returns_long %>%
                nest(-asset) %>%
                mutate(model = 
                         map(data, ~ 
                               lm(returns ~ market_returns_tidy$returns,
                                        data = .))) %>%
                unnest(model %>% map(tidy)) %>%
                filter(term != "(Intercept)") %>%
                select(-term) %>%  
                mutate_if(is.numeric, funs(round(., 4)))
# Listo!, Tenemos los betas de cada uno de los instrumentos.

# Veamos cada uno:
beta_assets
beta_SPY <- beta_assets$estimate[1] # beta = 1 ... Hace sentido porque el SPY es el ETF del SP500
beta_EFA <- beta_assets$estimate[2]
beta_IJS <- beta_assets$estimate[3]
beta_EEM <- beta_assets$estimate[4]
beta_AGG <- beta_assets$estimate[5]

# Construmos el beta del portafolio
beta_byhand <- beta_SPY*w[1] + beta_EFA*w[2] + beta_IJS*w[3] + beta_EEM*w[4] + beta_AGG*w[5]
beta_byhand

# De esta forma, comprobamos que el Beta del Modelo CAPM, también lo podemos obtener
# de esta forma
beta_CAPM
beta_byhand # Diferen en el 5 decimal porque el "beta_byhand" fue redondeado.

# Sin embargo, hay una forma de calcular el beta de forma una forma más directa:
beta_dplyr_byhand <-  portfolio_returns_tq_rebalanced_monthly %>%
                      do(model = lm(returns ~ market_returns_tidy$returns,
                                     data =.)) %>%
                      tidy(model) %>%
                      mutate(term = c("alpha","beta")) %>%
                      select(estimate)
beta_dplyr_byhand$estimate[2] # Es el mismo! :)

# Veamos el CAPM, gráficamente
# 1. Utilizamos la función mutate para agregar una columna "market_returns" que contiene
# los retornos del mercado
# 2. Utilizamos el package "ggplot2" para graficar el CAPM
# En el eje "x", estará el retorno del mercado
# En el eje "y", estará el retorno del portafolio
# 3. Utilizamos "geom_point" para hacer un scatterplot con las variables en mención
# 4. Utilizamos "geom_abline" para hacer una línea, cuyo punto inicial es el intercepto
# y tiene una pendiente que es el beta del modelo CAPM
# 5. Utilizamos ylab y xlab para cambiar el nombre de los ejes.
# 6. Utilizamos la función theme, para añadir un fondo blanco y borde gris.
portfolio_returns_tq_rebalanced_monthly%>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  ggplot(aes(x=market_returns, y=returns)) +
  geom_point(color="cornflowerblue") +
  geom_abline(aes(
              intercept = beta_dplyr_byhand$estimate[1],
              slope = beta_dplyr_byhand$estimate[2])) + 
  ylab("Portfolio Returns") +
  xlab("Market Returns") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  
# ========================================================== #
#                   FAMA-FRENCH FACTOR MODEL                 #
# Curso: Gestión de Riesgos                                  #
# Profesora : Yessenia Portal                                #
# ========================================================== #
# Universidad Nacional Mayor de San Marcos                   #
# Facultad de Ciencias Económicas                            #
# Círculo de Estidios Financieros y del Mercado de Capitales #
# Daniel Ayquipa, Gian Franco Huamán, Frank Vidal            #
# ========================================================== #

# La función tempfile devuelve un vector de cadenas de caracteres que se pueden usar como 
# nombres para archivos temporales
temp <- tempfile()

# Vamos a formar el url de descarga de los factores
base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor <- "Global_3_Factors"
format <- "_CSV.zip"
full_url <- paste(base, factor, format, sep ="")

# Una vez que tenemos el link de descarga:
# Utilizamos la función download.file() para obtenerlos
# y los guardaremos en la ruta "temp"
download.file(full_url, temp, quiet=TRUE)

# 1. Utilizamos la función read_csv para poder leer la data descargada
# 2. La función unz nos permite quitar el "zip" de los archivos descargados
# Además, utilizamos skip = 6 para indicar que nos salteamos 6 filas.
# 3. Utilizamos la función rename para cambiar el nombre de la columna X1 a "date"
# 4. Utilizamos la función "mutate_at" porque los números están como "character" y queremos que
# estén como "double", la única columna que es excluída es "dates" y se utiliza la función
# vars para excluirla
# 5. Utilizamos la función mutate(date=...) para manipular la columna date, lo que queremos
# lograr es que el fotmato de la fecha se muestre asi: YYYY-MM-DD
# 6. Utilizamos nuevamente la función mutate(date=...) para manipular la columna date, y cambiar
# la fecha que se muestra en la columna por la fecha del fin de mes.
Global_3_Factors <- read_csv(unz(temp,
                                 "Global_3_Factors.csv"),
                             skip=6) %>%
  rename(date = X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>%
  mutate(date = rollback(date+months(1)))

head(Global_3_Factors,3)  

# Ahora obtendremos los retornos 

# 1. Utilizaremos la función "left_join" para hacer match entre el objeto "Global_3_Factors"
# con el objeto "portfolio_returns_tq_rebalanced_monthly" por "fecha"
# 2. Utilizaremos la función "mutate" para manipular las columnas y agregar una más llamada
# "R_excess" que viene a ser los retornos del portafolio menos la risk free.
# 3. Con la función select, quitaremos las columnas returns, RF y `Mkt-RF`.
ff_portfolio_returns <- portfolio_returns_tq_rebalanced_monthly %>%
                        left_join(Global_3_Factors, by = "date") %>%
                        mutate(MKT_RF = `Mkt-RF`/100,
                               SMB = SMB/100,
                               HML = HML/100,
                               RF = RF/100,
                               R_excess = round(returns - RF, 4)) %>%
                        select(-returns, -RF, -`Mkt-RF`)         

# Lo que sigue es, hacer una regresión entre "R_excess" frente a cada uno de los factores
# con el fin de detectar cuál de ellos tiene un mayor poder de explicación.
# 1. Con la función do, vamos a aplicar la regresión R_excess ~ MKT_RF + SMB + HML
# 2. Con la función tidy, queremos limpiar la información que obtuvimos de la regresión
# Y agregamos dos columnas más "conf.inf=T" y "conf.level=0.95".
# 3. Con la función rename, queremos cambiar el nombre de la columna "estimate" por "beta"
# 4. Con la función mutate_if, queremos redondear los retornos a 3 cifras
# 5. Con la funcion select, estamos quitando las columnas "statistic","std.error"
ff_dplyr_byhand <-  ff_portfolio_returns %>%
                    do(model = lm(R_excess ~ MKT_RF + SMB + HML,
                                  data = .)) %>%
                    tidy(model, conf.int = T, conf.level = 0.95) %>%
                    rename(beta = estimate) %>%
                    mutate_if(is.numeric, funs(round(.,3))) %>%
                    select(-statistic, -std.error)

# Veamoslo Gráficamente
# Partimos del objeto "ff_dplyr_byhand"
# 1. Con la función "mutate_if", queremos redondear los retornos a 3 cifras
# 2. Con la función "filter", queremos quitar el "intercepto"
# 3. Con la función ggplot, vamos a graficar los factores  y su beta
# 4. Utilizaremos geom_point para ubicarlos con una figura geométrica en el gráfico
# 5. Utilizaremos geom_errorbar para agregarle una bandas de confianza 
# 6. utilizaremos labs para modificar el título, subtitulo, texto en ejes y fuente.
# 7. utilizaremos theme para modificar color del fondo, y alinear el texto del título
# subtitulo y fuente.
ff_dplyr_byhand %>%
  mutate_if(is.numeric, funs(round(.,3))) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term,
             y = beta,
             shape = term,
             color = term)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high)) +
  labs(title = "FF 3-Factor coefficients",
       subtitle = "balanced portfolio",
       x = "",
       y = "coefficient",
       caption = "data source: Fama-French website") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

