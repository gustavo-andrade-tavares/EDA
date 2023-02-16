# Visualizações e Exploração dos Dados

library(tidyverse)
#library(esquisse)

dados_paises <- read_rds("Paises_Selecionados.rds")

dados_pib <- read_rds("PIB_per_capita_paises.rds")

dados_paises %>% 
  ggplot(aes(x = country, y = renewables_electricity))+
  geom_col(aes(fill = country))


dados_paises %>% 
  ggplot(aes(x = country, y = renewables_consumption))+
  geom_col(aes(fill = country))

dados_paises %>% 
  ggplot(aes(x = year, y = renewables_consumption))+
  geom_line(aes(color = country))


#estatístcas energias não renováveis
dados_paises2 <- read_rds("Paises_Selecionados_2.rds")

x_swe <- dados_paises2[1:10, 128]
x_ndl <- dados_paises2[11:20, 128]
x_aus <- dados_paises2[21:30, 128]
x_dnk <- dados_paises2[31:40, 128]
x_usa <- dados_paises2[41:50, 128]
x_nor <- dados_paises2[51:60, 128]
x_irl <- dados_paises2[61:70, 128]
x_che <- dados_paises2[71:80, 128]
x_rus <- dados_paises2[81:90, 128]
x_can <- dados_paises2[91:100, 128]
x_bra <- dados_paises2[101:110, 128]

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

#teste assimetria de Fisher g1 = m3/m2^(3/2)
m3 <- all.moments(x_ndl, 3, central = TRUE)
m2 <- all.moments(x_ndl, 2, central = TRUE)

#não renovável TWh
x_swe <- dados_paises2[1:10, 125]
x_ndl <- dados_paises2[11:20, 125]
x_aus <- dados_paises2[21:30, 125]
x_dnk <- dados_paises2[31:40, 125]
x_usa <- dados_paises2[41:50, 125]
x_nor <- dados_paises2[51:60, 125]
x_irl <- dados_paises2[61:70, 125]
x_che <- dados_paises2[71:80, 125]
x_rus <- dados_paises2[81:90, 125]
x_can <- dados_paises2[91:100, 125]
x_bra <- dados_paises2[101:110, 125]

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