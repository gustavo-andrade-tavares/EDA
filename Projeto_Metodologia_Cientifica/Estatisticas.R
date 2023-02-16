# Medidas de posição, dispersão, assimetria, curtose e correlação
library(tidyverse)
library(moments)

dados_paises <- read_rds("Paises_Selecionados.rds")

dados_pib <- read_rds("PIB_per_capita_paises.rds")

#visualização dos pibs
dados_pib %>% 
  ggplot(aes(x = country, y = `2019`))+
  geom_col(aes(fill = country))

tapply(dados_pib$country, dados_pib$`2019`, sort)


#medidas de posição: média, mediana, quantis
#eletricity generation (TWh)
#teste. ps: modificar apenas as colunas, as linhas já correspondem aos países
x_swe <- dados_paises[1:10, 29]
x_ndl <- dados_paises[11:20, 29]
x_aus <- dados_paises[21:30, 29]
x_dnk <- dados_paises[31:40, 29]
x_usa <- dados_paises[41:50, 29]
x_nor <- dados_paises[51:60, 29]
x_irl <- dados_paises[61:70, 29]
x_che <- dados_paises[71:80, 29]
x_rus <- dados_paises[81:90, 29]
x_can <- dados_paises[91:100, 29]
x_bra <- dados_paises[101:110, 29]

mean(x_swe)
median(x_swe)
summary(x_swe)

#renewables energy per capita
x_swe <- dados_paises[1:10, 107]
x_ndl <- dados_paises[11:20, 107]
x_aus <- dados_paises[21:30, 107]
x_dnk <- dados_paises[31:40, 107]
x_usa <- dados_paises[41:50, 107]
x_nor <- dados_paises[51:60, 107]
x_irl <- dados_paises[61:70, 107]
x_che <- dados_paises[71:80, 107]
x_rus <- dados_paises[81:90, 107]
x_can <- dados_paises[91:100, 107]
x_bra <- dados_paises[101:110, 107]

#swe 
summary(x_swe)
round(mean(x_swe), 2)
round(skewness(x_swe), 3)
round(kurtosis(x_swe), 3)
round(sd(x_swe), 2)
hist(x_swe)

#ndl
summary(x_ndl)
round(mean(x_ndl), 2)
round(skewness(x_ndl), 3)
round(kurtosis(x_ndl), 3)
round(sd(x_ndl), 2)
hist(x_ndl)

#aus 
summary(x_aus)
round(mean(x_aus), 2)
round(skewness(x_aus), 3)
round(kurtosis(x_aus), 3)
round(sd(x_aus), 2)
hist(x_aus)

#dnk 
summary(x_dnk)
round(mean(x_dnk), 2)
round(skewness(x_dnk), 3)
round(kurtosis(x_dnk), 3)
round(sd(x_dnk), 2)
hist(x_dnk)

#usa 
summary(x_usa)
round(mean(x_usa), 2)
round(skewness(x_usa), 3)
round(kurtosis(x_usa), 3)
round(sd(x_usa), 2)
hist(x_usa)

#nor 
summary(x_nor)
round(mean(x_nor), 2)
round(skewness(x_nor), 3)
round(kurtosis(x_nor), 3)
round(sd(x_nor), 2)
hist(x_nor)

#irl 
summary(x_irl)
round(mean(x_irl), 2)
round(skewness(x_irl), 3)
round(kurtosis(x_irl), 3)
round(sd(x_irl), 2)
hist(x_irl)

#che 
summary(x_che)
round(mean(x_che), 2)
round(skewness(x_che), 3)
round(kurtosis(x_che), 3)
round(sd(x_che), 2)
hist(x_che)

#rus 
summary(x_rus)
round(mean(x_rus), 2)
round(skewness(x_rus), 3)
round(kurtosis(x_rus), 3)
round(sd(x_rus), 2)
hist(x_rus)

#can 
summary(x_can)
round(mean(x_can), 2)
round(skewness(x_can), 3)
round(kurtosis(x_can), 3)
round(sd(x_can), 2)
hist(x_can)

#bra
summary(x_bra)
round(mean(x_bra), 2)
round(skewness(x_bra), 3)
round(kurtosis(x_bra), 3)
round(sd(x_bra), 2)
hist(x_bra)

par(mfrow = c(2, 6))
hist(x_bra)
hist(x_can)
hist(x_rus)
hist(x_che)
hist(x_irl)
hist(x_nor)
hist(x_usa)
hist(x_dnk)
hist(x_aus)
hist(x_ndl)
hist(x_swe)

#renovável TWh
x_swe <- dados_paises[1:10, 106]
x_ndl <- dados_paises[11:20, 106]
x_aus <- dados_paises[21:30, 106]
x_dnk <- dados_paises[31:40, 106]
x_usa <- dados_paises[41:50, 106]
x_nor <- dados_paises[51:60, 106]
x_irl <- dados_paises[61:70, 106]
x_che <- dados_paises[71:80, 106]
x_rus <- dados_paises[81:90, 106]
x_can <- dados_paises[91:100, 106]
x_bra <- dados_paises[101:110, 106]

round(mean(x_swe), 2)
round(mean(x_ndl), 2)
round(mean(x_aus), 2)
round(mean(x_dnk), 2)
round(mean(x_usa), 2)
round(mean(x_nor), 2)
round(mean(x_irl), 2)
round(mean(x_che), 2)
round(mean(x_rus), 2)
round(mean(x_can), 2)
round(mean(x_bra), 2)