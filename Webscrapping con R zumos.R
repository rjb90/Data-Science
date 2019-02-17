library('rvest')
library('dplyr')
library(stringr)
library(tidyr)
library(ggplot2)

# "Importo" la web

webzumos <- 'https://www.carrefour.es/supermercado/bebidas/aguas-refrescos-y-zumos/zumos-no-refrigerados/N-1k4id5/c'
zumos_html <- read_html(webzumos)

# Obtengo los datos en función del código fuente de la web
zumos_nodos<- html_nodes(zumos_html, ".product-card-item") 
zumos_titulos <- zumos_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos_nodos %>% html_nodes(".format-price") %>% html_text()

# Creo el df con los datos obtenidos de la web
zumos_df_1 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_1) 

# Limpio los datos para que pueda trabajar con ellos
zumos_df_1 <- zumos_df_1 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                          zumos_prices  = as.character(zumos_prices),
                          zumos_format = as.character(zumos_format),
                          zumos_titulos = gsub('\\n', '', zumos_titulos),
                          zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_1)

# Continuo limpiando los datos, esta vez obtengo los numéricos y creo nueva columna de precios
zumos_df_1 <- zumos_df_1 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_1$precio_litro,start=0, end = 4)))))
str(zumos_df_1)

#Hago los mismos pasos para la página 2
webzumos2 <- 'https://www.carrefour.es/supermercado/c?No=24&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos2_html <- read_html(webzumos2)

zumos2_nodos <- html_nodes(zumos2_html, ".product-card-item")
zumos_titulos <- zumos2_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos2_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos2_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_2 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_2) 

zumos_df_2 <- zumos_df_2 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_2)

zumos_df_2 <- zumos_df_2 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_2$precio_litro,start=0, end = 4)))))
str(zumos_df_2)

# Mismos pasos página 3
webzumos3 <- 'https://www.carrefour.es/supermercado/c?No=48&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos3_html <- read_html(webzumos3)

zumos3_nodos <- html_nodes(zumos3_html, ".product-card-item")
zumos_titulos <- zumos3_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos3_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos3_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_3 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_3) 

zumos_df_3 <- zumos_df_3 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_3)

zumos_df_3 <- zumos_df_3 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_3$precio_litro,start=0, end = 4)))))
str(zumos_df_3)

# Mismos pasos página 4
webzumos4 <- 'https://www.carrefour.es/supermercado/c?No=72&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos4_html <- read_html(webzumos4)

zumos4_nodos <- html_nodes(zumos4_html, ".product-card-item")
zumos_titulos <- zumos4_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos4_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos4_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_4 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_4) 

zumos_df_4 <- zumos_df_4 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_4)

zumos_df_4 <- zumos_df_4 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_4$precio_litro,start=0, end = 4)))))
str(zumos_df_4)

# Mismos pasos página 5
webzumos5 <- 'https://www.carrefour.es/supermercado/c?No=96&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos5_html <- read_html(webzumos5)

zumos5_nodos <- html_nodes(zumos5_html, ".product-card-item")
zumos_titulos <- zumos5_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos5_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos5_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_5 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_5) 

zumos_df_5 <- zumos_df_5 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_5)

zumos_df_5 <- zumos_df_5 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_5$precio_litro,start=0, end = 4)))))
str(zumos_df_5)

# Mismos pasos página 6
webzumos6 <- 'https://www.carrefour.es/supermercado/c?No=120&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos6_html <- read_html(webzumos6)

zumos6_nodos <- html_nodes(zumos6_html, ".product-card-item")
zumos_titulos <- zumos6_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos6_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos6_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_6 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_6) 

zumos_df_6 <- zumos_df_6 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_6)

zumos_df_6 <- zumos_df_6 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_6$precio_litro,start=0, end = 4)))))
str(zumos_df_6)

# Mismos pasos página 7
webzumos7 <- 'https://www.carrefour.es/supermercado/c?No=144&Nr%3DAND%28product.shopCodes%3A004320%2Cproduct.salepointWithActivePrice_004320%3A1%2COR%28product.siteId%3AbasicSite%29%29OR%29&Ntt=zumo+de+naranja&sb=true'
zumos7_html <- read_html(webzumos7)

zumos7_nodos <- html_nodes(zumos7_html, ".product-card-item")
zumos_titulos <- zumos7_nodos %>% html_nodes(".title-product") %>% html_text()
zumos_prices <- zumos7_nodos %>% html_nodes(".price") %>% html_text()
zumos_format <- zumos7_nodos %>%  html_nodes(".format-price") %>% html_text()

zumos_df_7 <- data.frame(cbind(zumos_titulos,zumos_prices, zumos_format))
str(zumos_df_7) 

zumos_df_7 <- zumos_df_7 %>% mutate(zumos_titulos = as.character(zumos_titulos),
                                    zumos_prices  = as.character(zumos_prices),
                                    zumos_format = as.character(zumos_format),
                                    zumos_titulos = gsub('\\n', '', zumos_titulos),
                                    zumos_format = gsub('\\n', '', zumos_format)) %>% 
  separate(zumos_format, into = c("empaquetado","precio_litro"), sep = "\\|")
str(zumos_df_7)

zumos_df_7 <- zumos_df_7 %>% mutate(
  zumos_prices = as.numeric(str_trim(gsub("\\,","\\.",(gsub("\\\200","", zumos_prices))))),
  precio_litro = as.numeric(str_trim(gsub("\\,","\\.",str_sub(zumos_df_7$precio_litro,start=0, end = 4)))))
str(zumos_df_7)

# Una vez creados los df, los agrego a un df con todos los datos
df_zumos <- rbind(zumos_df_1, zumos_df_2, zumos_df_3, zumos_df_4, zumos_df_5, zumos_df_6, zumos_df_7)

# Empiezo a jugar con los datos
ggplot(df_zumos, aes(x=precio_litro))+geom_histogram(bins=10)

df_zumos <- df_zumos %>% mutate(
  marca = ifelse(grepl("Carrefour", zumos_titulos)>0,"PROPIA", "OTROS"))

ggplot(df_zumos, aes(x=precio_litro,fill=marca))+geom_histogram(bins=10, alpha = 0.5)


