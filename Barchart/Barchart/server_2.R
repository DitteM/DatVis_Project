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
  
  # Reactive values
  Niv_1 <- reactive()
  Niv_2 <- reactive()
  Niv_3 <- reactive()
  
  
  # When clicking on a level of detail
  observeEvent(event_data("plotly_click", source = "Niv_1"), {
    Niv_1(event_data("plotly_click", source = "Niv_1")$x)
    Niv_2(NULL)
    Niv_3(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "Niv_2"), {
    Niv_2(event_data("plotly_click", source = "Niv_2")$x)
    Niv_3(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "Niv_3"), {
    Niv_3(event_data("plotly_click", source = "Niv_2")$x)
  })
  
  
  output$Niv_1 <- renderPlotly({
    
    # Level of detail
    niv_in <- "Niveau 1"
    
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
      
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~x_in, source = "Niv_1", type = "bar", color = factor(sign(.data[[y_in]]))) %>%
        layout(xaxis = list(titel = ""),
               yaxis = list(title = input$y_var)) 
      
      
      #p <- ggplotly(ggplot(data=data_work, aes(x = x_in, y = .data[[y_in]])) +
       #               geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        #              labs(x = "", y = input$y_var) +
         #             scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue")) +
          #            coord_flip()
      #)
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, .data[[y_in]]), source = "Niv_1", type = "bar", color = factor(sign(.data[[y_in]]))) %>%
        layout(xaxis = list(titel = ""),
               yaxis = list(title = input$y_var)) 
      
      #p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, .data[[y_in]]), y = .data[[y_in]])) +
       #               geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        #              labs(x = "", y = input$y_var) +
         #             scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
          #            coord_flip()
      #)
    } else {
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, desc(.data[[y_in]])), source = "Niv_1", type = "bar", color = factor(sign(.data[[y_in]]))) %>%
        layout(xaxis = list(titel = ""),
               yaxis = list(title = input$y_var)) 
      
      #p <- ggplotly(ggplot(data=data_work, aes(x = reorder(x_in, desc(.data[[y_in]])), y = .data[[y_in]])) +
       #               geom_bar(stat="identity", aes(fill = factor(sign(.data[[y_in]])))) + 
        #              labs(x = "", y = input$y_var) +
         #             scale_fill_manual("Legend:", values = c("-1"= "yellow", "0" = "grey", "1" = "blue"), labels = c("Negativ", "Neutral", "Positiv")) +
          #            coord_flip()
      #)
    }
    
    
    
  })
  
})