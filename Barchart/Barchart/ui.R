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

# Dato filter for valg af specific måned. 
## https://stackoverflow.com/questions/71193296/how-to-only-select-month-and-year-in-a-shiny-widget



# Define UI for application that draws a histogram
ui <- navbarPage("Forbrugerprisindeks",
                 tabPanel("Søjlediagram for ændring af forbrugerprisindeks",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select your desired filteres:"),
                              selectInput("y_var",
                                          label = "Enhed:",
                                          choices = c("Indeks", "Sidste måned", "Samme måned sidste år"),
                                          selected =  "Sidste måned"),
                              selectInput("order_input",
                                          label = "Sortering:",
                                          choices = c("Numerisk", "Aftagende", "Stigende"),
                                          selected =  "Numerisk",),
                              selectInput("niveau_in",
                                          label = "Niveau:",
                                          choices = c("Nievau 1", "Niveau 2", "Niveau 3", "Niveau 4"),
                                          selected =  "Nievau 1"),
                              airDatepickerInput("year_month_input",
                                                 label = "Month of interest:",
                                                 value = max(df$Date),
                                                 minDate = min(df$Date),
                                                 maxDate = max(df$Date),
                                                 view = "months",
                                                 minView = "months", 
                                                 dateFormat = "yyyy-mm")
                              )
                            ),
                            mainPanel(
                              plotOutput("barPlot")
                            )
                            
                          )
                          
                 ),
                 
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 )
)
