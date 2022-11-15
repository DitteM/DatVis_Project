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


##Filtrer data på baggrund af det valgte niveau værdi i forrige plot - altså ligesom de filtrere på category i sub:category plot, så filtrere vi på niveau i mere deltajeret view 
#Dette skal ske i stedet for den data filtrering vi allerede laver hvor vi deffinere vores datasæt til plottet. 

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
        plot_ly(x = ~.data[[y_in]], y = ~x_in, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      

    } else {
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  

    }
    
    
    
  })
  
  output$Niv_2 <- renderPlotly({
    if (is.null(Niv_1())) return(NULL)
    
    # Level of detail
    niv_in <- "Niveau 2"
    
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
        filter("Niveau 1" %in% Niv_1()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~x_in, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
  output$Niv_3 <- renderPlotly({
    if (is.null(Niv_2())) return(NULL)
    
    # Level of detail
    niv_in <- "Niveau 3"
    
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
        filter("Niveau 2" %in% Niv_2()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~x_in, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
  output$Niv_4 <- renderPlotly({
    if (is.null(Niv_3())) return(NULL)
    
    # Level of detail
    niv_in <- "Niveau 4"
    
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
        filter("Niveau 2" %in% Niv_2()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~x_in, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(x_in, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "Niv_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = x_in),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
})