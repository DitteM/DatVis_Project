#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tidyverse)

# Dato filter for valg af specific måned. 
## https://stackoverflow.com/questions/71193296/how-to-only-select-month-and-year-in-a-shiny-widget


units <- colnames(df[4:6])
levels <- colnames(df[7:10])
# Define UI for application that draws a histogram
ui <- fluidPage(sidebarLayout(
                            sidebarPanel(
                              h4("Filtre:"),
                              selectInput(inputId = "y_var",
                                          label = "Enhed:",
                                          choices = c("Indeks", "Sidste måned", "Samme måned sidste år"),
                                          selected =  "Sidste måned"),
                              selectInput(inputId = "order_input",
                                          label = "Sortering:",
                                          choices = c("Alfabetisk", "Aftagende", "Stigende"),
                                          selected =  "Alfabetisk",),
                              airDatepickerInput(inputId = "year_month_input",
                                                 label = "Måned:",
                                                 value = max(df$Dato),
                                                 minDate = min(df$Dato),
                                                 maxDate = max(df$Dato),
                                                 view = "months",
                                                 minView = "months",
                                                 dateFormat = "yyyy-MM")
                            ),
                            
                            mainPanel(
                              plotlyOutput("beskrivelse_1", height = 300),
                              plotlyOutput("beskrivelse_2", height = 300),
                              plotlyOutput("beskrivelse_3", height = 300),
                              plotlyOutput("beskrivelse_4", height = 300)
                            )
                          )
)

