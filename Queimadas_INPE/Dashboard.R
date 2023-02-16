library(shiny)
library(shinydashboard)
library(leaflet)

df_queimadas <- read.csv("Focos_2021-06-15_2021-06-16.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Queimadas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Focos de queimadas", tabName = "Fq", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Fq",
        h2("Focos de queimadas de 15/06/2021 a 16/06/2021."),
        leafletOutput("mapa_principal", height = 500)
      ),
      
      tabItem(
        tabName = "widgets",
        h2("Widgets tab content"),
        
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        )
        
      )
    )
  )
)

server <- function(input, output) {
  output$mapa_principal <- renderLeaflet({
    
    df_queimadas %>% 
      leaflet() %>% 
      addTiles(attribution = "Dados das queimadas extraídos do INPE.") %>% 
      addCircleMarkers(~longitude, ~latitude, label = ~municipio)
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)