dados <- read.csv('covid_cidades.csv')

df <- data.frame(dados)

library(tidyverse)

names(dados)

# Agrupando os dados por estado

# group_by

data <- dados %>% 
  group_by(state) %>% 
  summarise(confirmed = sum(confirmed),
            deaths = sum(deaths))

# Representação tabular

# Por que nesse caso não é necessário usar table?

tab <- data$deaths
names(tab) <- levels(data$state)
tab

tab.rel <- data$deaths / sum(tab)
names(tab.rel) <- levels(data$state)
tab.rel

teste <- data$confirmed
names(teste) <- levels(data$state)
teste

teste.rel <- data$confirmed / sum(teste)
names(teste.rel) <- levels(data$state)
teste.rel

tabela.rel <- data$confirmed / sum(data$confirmed)
names(tabela.rel) <- levels(data$state)
tabela.rel

# Gráficos padrões do R

barplot(tab, col='lightblue')

barplot(tab.rel, col='lightblue')


# Gráfico de Pareto

# install('qicharts2')
library(qicharts2)

x <- rep(data$state, data$deaths)

paretochart(x,
            title = 'Gráfico de Pareto para mortes')


# Usando o pacote plotly

#install.packages('plotly')
library(plotly)

# Para variáveis qualitativas
# Por que nesse caso não precisamos obter a tabela de frequências?

fig <- plot_ly(data, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes acumuladas nos estados até 29/12/2020')

fig

# Gráfico de barras de mortes

fig <- plot_ly(data,
               x = ~state,
               y = ~deaths,
               name = 'mortes',
               type = 'bar')
fig

# Gráfico de barras de confirmados

fig <- plot_ly(data,
               x = ~state,
               y = ~confirmed,
               name = 'confirmados',
               type = 'bar')
fig

# Refaça os gráficos com estados onde o número de mortes é maior que 9 mil

# filter

dados_mais9milmortes <- data %>%
  filter(
    deaths > 9000
  )

dados_mais9milmortes

fig <- plot_ly(dados_mais9milmortes, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes em estados com mais de 9 mil mortes em 29/12/2020')

fig

# Exercício: Criar as regiões usando o tibble

data = data %>% 
  mutate(regiao = case_when(
    state %in% c("RS","PR","SC") ~ 'sul',
    state %in% c("RJ","SP","MG","ES") ~ 'sudeste',
    state %in% c("GO","MT","MS","DF") ~ 'centro-oeste',
    state %in% c("CE","PI","BA","MA","RN","PB","PE","SE","AL") ~ 'nordeste',
    state %in% c("AM", "AC", "PA", "RO", "RR", "AP", "TO") ~ 'norte'))


# Filtrando dados apenas da região sudeste

dados_sudeste <- data %>%
  filter(
    regiao == 'sudeste'
  )


fig <- plot_ly(dados_sudeste, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes na região sudeste')

fig 

dados_sul <- data %>% 
  filter(
    regiao == 'sul'
  )

graf <- plot_ly(dados_sul, labels = ~state, values = ~deaths, type = 'pie')
graf <- graf %>% layout(title = 'Gráfico de setores para mortes na região sul')

graf


#ordenar
data = data[order(data$regiao),]

dados_sul = dados_sul[order(dados_sul$deaths), ]
