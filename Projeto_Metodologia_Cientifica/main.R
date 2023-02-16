# Projeto de Metodologia Científica I
# Matrizes energéticas
# Comparação de consumo e disponibilidade de fontes de energias de países selecionados

library(tidyverse)
library(readxl)

# Leitura dos dados, energy data, gdp per capita
dados_energia <- read.csv("owid-energy-data.csv")

dados_PIB <- read_xls("API_NY.GDP.PCAP.CD_DS2_en_excel_v2_2445346.xls", skip = 2)

#Energias renováveis: eólica, solar. biomassa, resíduos, hídrica e geotérmica.
#Energias não-renováveis: petróleo, gás natural e carvão.

# Filtros dos dados

# PIB per capita

df_pibpc <- dados_PIB %>% select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, `2019`)

#Objetivo: descobrir os 10 países

# dados da população de 2019 para anexar no df do pib
df_pop2019 <- dados_energia %>% select(country, year, population)

df_pop2019 <- df_pop2019 %>% 
  filter(
    year == 2019
  ) 

df_pop2019 <- df_pop2019 %>% 
  select(-year) 

colnames(df_pop2019)[2] <- "population_2019"

df_gdppc <- left_join(df_pop2019, df_pibpc, by = c("country" = "Country Name"))
df_GDPpc <- left_join(df_pibpc, df_pop2019, by = c("Country Name" = "country"))


# Países com mais de 1mi de habitantes
df_1miHAB <- df_gdppc %>% 
  filter(
    population_2019 >= 1000000
  )

linhas <- c(2, 47, 103)

df_1miHAB <- df_1miHAB[-linhas,]

df_1miHAB <- as.data.frame.data.frame(df_1miHAB)

df_1miHAB %>% 
  top_n(10, `2019`) %>% 
  ggplot(aes(x = country, y = `2019`))+
  geom_col(aes(fill =  country))

tapply(df_1miHAB$country, df_1miHAB$`2019`, sort)

# países: Sweden, Netherlands, Australia, Denmark, Qatar, Singapore, United States, Norway, Ireland, Switzerland.

# salvando em rds
df_gdppc %>% 
  write_rds("PIB_percapita_filtrado.rds")


# testesss
df_pibpc %>%
  top_n(10, `2019`) %>% 
  ggplot(aes(x = `Country Name`, y = `2019`))+
  geom_col(aes(fill = `Country Name`))

pib_filtrado <- df_pibpc %>% 
  filter(
    `2019` >= 63000
  )

tapply(pib_filtrado$`Country Name`, pib_filtrado$`2019`, sort)


#Sweden, Netherlands, Australia, Denmark, United States, Norway, Ireland, Switzerland. Russia e Canada

#Filtrar dados da energia para 2010 a 2019 e deixar apenas os países que faremos as análises.

df_en_10_19 <- dados_energia %>% 
  filter(
    year >= 2010
  )

#Sweden
df_paises <- df_en_10_19 %>% 
  filter(
    country == "Sweden",
    year < 2020
  )

df_pib_paises <- df_1miHAB %>% 
  filter(
    country == "Sweden"
  )
#Netherlands
df_paises2 <- df_en_10_19 %>% 
  filter(
    country == "Netherlands",
    year < 2020
  )
df_pib_paises2 <- df_1miHAB %>% 
  filter(
    country == "Netherlands"
  )

#Australia
df_paises3 <- df_en_10_19 %>% 
  filter(
    country == "Australia",
    year < 2020
  )
df_pib_paises3 <- df_1miHAB %>% 
  filter(
    country == "Australia"
  )

#Denmark
df_paises4 <- df_en_10_19 %>% 
  filter(
    country == "Denmark",
    year < 2020
  )
df_pib_paises4 <- df_1miHAB %>% 
  filter(
    country == "Denmark"
  )

#United States
df_paises5 <- df_en_10_19 %>% 
  filter(
    country == "United States",
    year < 2020
  )
df_pib_paises5 <- df_1miHAB %>% 
  filter(
    country == "United States"
  )

#Norway
df_paises6 <- df_en_10_19 %>% 
  filter(
    country == "Norway",
    year < 2020
  )
df_pib_paises6 <- df_1miHAB %>% 
  filter(
    country == "Norway"
  )

#Ireland
df_paises7 <- df_en_10_19 %>% 
  filter(
    country == "Ireland",
    year < 2020
  )
df_pib_paises7 <- df_1miHAB %>% 
  filter(
    country == "Ireland"
  )

#Switzerland
df_paises8 <- df_en_10_19 %>% 
  filter(
    country == "Switzerland",
    year < 2020
  )
df_pib_paises8 <- df_1miHAB %>% 
  filter(
    country == "Switzerland"
  )

#Russia
df_paises9 <- df_en_10_19 %>% 
  filter(
    country == "Russia",
    year < 2020
  )
df_pib_paises9 <- df_1miHAB %>% 
  filter(
    country == "Russia"
  )

#Canada
df_paises10 <- df_en_10_19 %>% 
  filter(
    country == "Canada",
    year < 2020
  )
df_pib_paises10 <- df_1miHAB %>% 
  filter(
    country == "Canada"
  )

#Brazil
df_paises11 <- df_en_10_19 %>% 
  filter(
    country == "Brazil",
    year < 2020
  )
df_pib_paises11 <- df_1miHAB %>% 
  filter(
    country == "Brazil"
  )

paises_selecionados <- bind_rows(df_paises, df_paises2, df_paises3, df_paises4, df_paises5, df_paises6, df_paises7, df_paises8, df_paises9, df_paises10, df_paises11)

pib_percapita <- bind_rows(df_pib_paises, df_pib_paises2, df_pib_paises3, df_pib_paises4, df_pib_paises5, df_pib_paises6, df_pib_paises7, df_pib_paises8, df_pib_paises9, df_pib_paises10, df_pib_paises11)

#salvar em rds
paises_selecionados %>% 
  write_rds("Paises_Selecionados.rds")

pib_percapita %>% 
  write_rds("PIB_per_capita_paises.rds")

#todos os países
df_en_10_19 %>% 
  write_rds("Paises_2010a2019.rds")

#todos os paises com pop > 1mi
df_1miHAB %>% 
  write_rds("GDP_pc_filtrado.rds")

teste <- read_rds("GDP_pc_filtrado.rds")

tapply(teste$country, teste$`2019`, sort)
