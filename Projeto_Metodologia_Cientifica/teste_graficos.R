library(tidyverse)
library(plotly)

#dados_graficos <- read_rds("dados_saude_patologias_canceres.rds")

dados_graficos <- read_rds("dados_canceres_grafico.rds")


dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = continent, y = `Mortes por câncer devido ao álcool(%)`))+
  geom_boxplot(aes(fill = continent))

p <- dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = continent, y = `Mortes por câncer devido ao álcool(%)`))+
  geom_boxplot(aes(fill = continent))

fig <- ggplotly(p)

fig

#tabagismo
p <- dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = continent, y = `Tabagismo masculino (%)`))+
  geom_col(aes(fill = continent))

fig <- ggplotly(p)

fig


#tabagismo vs cancer de pulmão homens
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `Tabagismo masculino (%)`, y = `Câncer de pulmão, homens (por 100 mil)`, color = continent))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#tabagismo vs cancer de pulmão mulheres
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `Tabagismo feminino (%)`, y = `Câncer de pulmão, mulheres (por 100 mil)`, color = continent))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#cânceres atribuiveis a infecções
dados_graficos %>% 
  filter(continent != "NA") %>%
  ggplot(aes(x = `Cânceres atribuíveis a infecções (%)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Proporção de cânceres atribuíveis a infecções", y = "Quantidade de países", caption = "Elaborado pelos autores.")

#câncer cervical
dados_graficos %>% 
  filter(continent != "NA") %>%
  ggplot(aes(x = `Câncer cervical (por 100 mil)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Taxas de incidência de câncer cervical", y = "Quantidade de países", caption = "Elaborado pelos autores.")

#sobreviventes de câncer
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `Sobreviventes de câncer (por 100 mil)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Sobreviventes de câncer nos últimos cinco anos", y = "Quantidade de países", caption = "Elaborado pelos autores.")


# câncer de pele
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `Câncer de pele Melanoma (por 100 mil)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Incidência de câncer de pele Melanoma", y = "Quantidade de países", caption = "Elaborado pelos autores.")

#disponibilidade de radioterapia
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = continent, y = `Disponibilidade de radioterapia (por 1000 pacientes)`))+
  geom_boxplot(aes(fill = continent))+
  labs(title = "Disponibilidade de radioterapia", x = "continente", caption = "Elaborado pelos autores.")

#HIV
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `HIV (%)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Prevalência de HIV", y = "Quantidade de países", caption = "Elaborado pelos autores.")

#Organizações de câncer
dados_graficos %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x = `UICC (organizações de câncer)`))+
  geom_histogram(aes(fill = continent))+
  labs(title = "Quantidade de organizações de câncer por país", y = "Quantidade de países", caption = "Elaborado pelos autores.")

