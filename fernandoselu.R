library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(fmsb)
library(radarchart)
library(RColorBrewer)
data <- read.csv("dataset.csv")

library(plotly)

courses <- unique(data$Course)

nb_cols <- length(courses)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb_cols)

ggplot(data, aes(x = data$Unemployment.rate, y = data$Inflation.rate)) +
  geom_bin2d(bins = 25) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap de Unemployment Rate e Inflation Rate", 
       x = "Unemployment Rate", y = "Inflation Rate")


ggplot(data, aes(x=data$GDP)) + 
  geom_density() + 
  labs(title="Density Plot of GDP", x="GDP", y="Density")

ggplot(data, aes(x=data$Inflation.rate)) + 
  geom_density() + 
  labs(title="Density Plot of Inflation Rate", x="Inflation Rate", y="Density")

ggplot(data, aes(x=data$Unemployment.rate)) + 
  geom_density() + 
  labs(title="Density Plot of Unemployment Rate", x="Unemployement Rate", y="Density")

# Agrupamos por curso y cuartiles de las variables GDP y Unemployment.Rate
data %>% 
  mutate(Quartile.GDP = ntile(GDP, 4), Quartile.Unemployment.rate = ntile(Unemployment.rate, 4)) %>%
  group_by(Course, Quartile.GDP, Quartile.Unemployment.rate) %>%
  summarize(n = n()) %>%
  
  # Creamos el mapa de calor
  ggplot(aes(x = Quartile.GDP, y = Course, fill = n)) +
  geom_tile(aes(width = 0.9, height = 0.9)) +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "white", high = "blue") +
  facet_grid(. ~ Quartile.Unemployment.rate, scales = "free_y") +
  xlab("Quartile GDP") +
  ylab("Course") +
  theme_bw()

data %>%
  mutate(Quartile.GDP = ntile(GDP, 4), Quartile.Unemployment.rate = ntile(Unemployment.rate, 4)) %>%
  group_by(Course, Quartile.GDP, Quartile.Unemployment.rate) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Course, y = n, fill = factor(Quartile.Unemployment.rate))) +
  geom_col(position = "stack") +
  facet_grid(. ~ Quartile.GDP) +
  scale_fill_brewer(palette = "PuBuGn", name = "Quartile Unemployment Rate") +
  labs(title = "Número de alumnos en cada carrera según Quartiles de PIB y Tasa de desempleo",
       x = "Carrera", y = "Número de alumnos")




