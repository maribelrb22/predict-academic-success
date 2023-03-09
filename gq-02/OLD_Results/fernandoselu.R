---
---
---
# Load libraries

library(shiny)
library(plotly)
library(dplyr)


data <- read.csv("dataset.csv")
#Conocer máximos y mínimos para los límites de cada slider

min(data$Unemployment.rate)
max(data$Unemployment.rate)

min(data$Inflation.rate)
max(data$Inflation.rate)

min(data$GDP)
max(data$GDP)

#Generamos UI con los sliders

ui <- fluidPage(
  titlePanel("Variación de las carreras elegidas por los estudiantes en función del GDP"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "gdp_slider", label = "Seleccione un rango de GDP", min = -5.0, max = 5, value = c(-5.0, 5)),
      sliderInput(inputId = "unemployment_slider", label = "Seleccione un rango de tasa de desempleo", min = 7, max = 17, value = c(7, 17)),
      sliderInput(inputId = "inflation_slider", label = "Seleccione un rango de tasa de inflación", min = -1, max = 4, value = c(-1, 4))
    ),
    mainPanel(
      plotlyOutput(outputId = "career_plot")
    )
  )
)

# Generamos server
server <- function(input, output) {
  
  output$career_plot <- renderPlotly({
    
    data_filtered <- data %>%
      filter(GDP >= input$gdp_slider[1] & GDP <= input$gdp_slider[2] &
               Unemployment.rate >= input$unemployment_slider[1] & Unemployment.rate <= input$unemployment_slider[2] &
               Inflation.rate >= input$inflation_slider[1] & Inflation.rate <= input$inflation_slider[2]) %>%
      group_by(Course) %>%
      summarise(count = n()) %>%
      mutate(Course = case_when(Course == "1" ~ "Biofuel Production Technologies",
                                Course == "2" ~ "Animation and Multimedia Design",
                                Course == "3" ~ "Social Service (evening attendance)",
                                Course == "4" ~ "Agronomy",
                                Course == "5" ~ "Communication Design",
                                Course == "6" ~ "Veterinary Nursing",
                                Course == "7" ~ "Informatics Engineering",
                                Course == "8" ~ "Equiniculture",
                                Course == "9" ~ "Management",
                                Course == "10" ~ "Social Service",
                                Course == "11" ~ "Tourism",
                                Course == "12" ~ "Nursing",
                                Course == "13" ~ "Oral Hygiene",
                                Course == "14" ~ "Advertising and Marketing Management",
                                Course == "15" ~ "Journalism and Communication",
                                Course == "16" ~ "Basic Education",
                                Course == "17" ~ "Management (evening attendance)",
                                TRUE ~ as.character(Course)))
    
    data_filtered$Course <- as.character(data_filtered$Course) 
    
    plot <- plot_ly(data_filtered, x = ~Course, y = ~count, type = "bar", color = ~count, colors = "Blues") %>%
      layout(title = "Carreras elegidas en función de la situación familiar y del país",
             xaxis = list(title = "Carreras"),
             yaxis = list(title = "Número de estudiantes"),
             coloraxis = list(colorbar = list(title = "Número de estudiantes")))
  })
}

shinyApp(ui, server)
