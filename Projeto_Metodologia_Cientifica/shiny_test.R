library(shiny)
library(plotly)
library(tidyverse)

dados_pib <- read_rds("PIB_per_capita_paises.rds")

ui <- fluidPage(
  titlePanel("Teste gráfico interativo"),
  plotlyOutput("grafico1")
)

server <- function(input, output, session) {
  output$grafico1 <- renderPlotly({
    p <- dados_pib %>% 
      ggplot(aes(x = country, y = `2019`))+
      geom_col(aes(fill = country, text = paste0(
        "country: ", country, "\n" ,
        "PIB per capita: ", round(`2019`, 2), "\n"
      )))
    
    
    
    fig <- ggplotly(p, tooltip = "text")
    
    fig
  })
}

shinyApp(ui, server)