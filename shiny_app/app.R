library(shiny)
library(tidyverse)
library(gridExtra)

source("R/plot_sar_stock.R")

data <- readRDS("data/US_sars_data.Rds")

ui <- fluidPage(
  
  titlePanel("U.S. Marine Mammal Stock Assessment Report data viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Region",
        choices = sort(unique(data$region)),
        selected = "Pacific", #sort(unique(data$region))[1]
      ),
      
      selectInput(
        inputId = "stock",
        label = "Stock",
        choices =  NULL # "Humpback whale (California-Oregon-Washington)" # NULL
      )
    ),
    
    mainPanel(
      plotOutput("sar_plot", height = "600px", width="800px")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$region, {
    
    stock_choices <- data %>% 
      filter(region == input$region) %>% 
      pull(stock) %>% 
      unique() %>% 
      sort()
    
    updateSelectInput(
      session,
      inputId = "stock",
      choices = stock_choices,
      selected = stock_choices[1]
    )
  })
  
  output$sar_plot <- renderPlot({
    
    req(input$stock)
    
    grid::grid.draw(
      plot_sar_stock(
        data = data,
        stock_do = input$stock
      )
    )
  })
}

shinyApp(ui, server)