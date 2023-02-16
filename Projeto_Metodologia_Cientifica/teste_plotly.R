library(tidyverse)
library(plotly)

dados_pib <- read_rds("PIB_per_capita_paises.rds")

dados_paises <- read_rds("Paises_Selecionados.rds")

#PIB países selecionados
dados_pib %>% 
  plotly() %>% 
  add_bars(x = ~country, y = ~`2019`) %>% 
  layout(
    title = "PIB per capita dos países selecionados",
    xaxis = "Países",
    yaxis = "PIB per capita (US$)"
  )

plotly(dados_pib) %>% 
  add_bars(x = ~country, y = ~`2019`) %>% 
  layout(
    title = "PIB per capita dos países selecionados",
    xaxis = "Países",
    yaxis = "PIB per capita (US$)"
  )

fig <- plotly(dados_pib, x = ~country, y = ~`2019`, type = "bar")
fig <- fig %>% layout(title = "PIB per capita dos países selecionados",
                      xaxis = "Países",
                      yaxis = "PIB per capita (US$)")

fig


dados_pib %>% 
  ggplot(aes(x = country, y = `2019`))+
  geom_col(aes(fill = country), show.legend = FALSE)

p <- dados_pib %>% 
  ggplot(aes(x = country, y = `2019`))+
  geom_col(aes(fill = country), show.legend = FALSE)

fig <- ggplotly(p)

fig


p <- dados_pib %>% 
  ggplot(aes(x = country, y = `2019`))+
  geom_col(aes(fill = country, text = paste0(
    "country: ", country, "\n" ,
    "PIB per capita: ", round(`2019`, 2), "\n"
  )))
  
  

fig <- ggplotly(p, tooltip = "text")

fig
