#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Dato filter for valg af specific måned. 


# Define UI for application that draws a histogram
ui <- navbarPage("Forbrugerprisindeks",
                 tabPanel("Søjlediagram for ændring af forbrugerprisindeks",
                          sidebarLayout(
                            sidebarPanel(
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
                                          selected =  "Nievau 1",)
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
