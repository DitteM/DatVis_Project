#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("../src/packages.R")

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Forbrugerprisindeks",
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins",
                                          "Number of bins:",
                                          min = 1,
                                          max = 50,
                                          value = 30)
                            ),
                            mainPanel(
                              plotOutput("distPlot")
                            )
                            
                          )
                          
                 ),
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Plot type",
                                           c("Scatter"="p", "Line"="l")
                              )
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 )
)
