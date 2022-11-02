#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$barPlot <- renderPlotly({
    
    # Level of detail
    niv_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
    names(niv_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
    niv_in <- niv_val[[input$niveau_in]]
    
    # Generates data for plot
    data_work <- df[!(is.na(df[[niv_in]])),]
    data_work <- data_work[data_work$Dato == input$year_month_input,]
    
    # Specifies the categories
    x_in <- data_work$beskrivelse
    
    # Specefies the values of the categories
    y_val <- list("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(y_val) <- c("Indeks", "Sidste måned", "Samme måned sidste år")
    y_in <- y_val[[input$y_var]]
    
    
    # Deffining plot based on ordering choice 
    
    if(input$order_input == "Alfabetisk"){
      p <- ggplotly(ggplot(data=data_work, aes(x = x_in, y = .data[[y_in]])) +
        geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        labs(x = "", y = input$y_var) +
        scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue")) +
        coord_flip()
      )
    } else if(input$order_input == "Aftagende"){
      p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, .data[[y_in]]), y = .data[[y_in]])) +
        geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        labs(x = "", y = input$y_var) +
        scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
        coord_flip()
      )
    } else {
      p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, desc(.data[[y_in]])), y = .data[[y_in]])) +
        geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        labs(x = "", y = input$y_var) +
        scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
        coord_flip()
      )
    }
    
    if(input$order_input == "Alfabetisk"){
      plot_ly(df, x = x_in, y = .data[[y_in]]), type = "bar", color = factor(sign(.data[[y_in]])))
      p <- ggplotly(ggplot(data=data_work, aes(x = x_in, y = .data[[y_in]])) +
                      geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
                      labs(x = "", y = input$y_var) +
                      scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue")) +
                      coord_flip()
      )
    } else if(input$order_input == "Aftagende"){
      p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, .data[[y_in]]), y = .data[[y_in]])) +
                      geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
                      labs(x = "", y = input$y_var) +
                      scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
                      coord_flip()
      )
    } else {
      p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, desc(.data[[y_in]])), y = .data[[y_in]])) +
                      geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
                      labs(x = "", y = input$y_var) +
                      scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
                      coord_flip()
      )
    }
    
  
    
    
    
  })
  
})