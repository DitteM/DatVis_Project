#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readxl)
library(lazyeval)
library(tidyverse)


##Filtrer data på baggrund af det valgte niveau værdi i forrige plot - altså ligesom de filtrere på category i sub:category plot, så filtrere vi på niveau i mere deltajeret view 
#Dette skal ske i stedet for den data filtrering vi allerede laver hvor vi deffinere vores datasæt til plottet. 

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  
  # Reactive values
  beskrivelse_1 <- reactiveVal()
  beskrivelse_2 <- reactiveVal()
  beskrivelse_3 <- reactiveVal()
  beskrivelse_4 <- reactiveVal()
  
  ##### reactive categories
  
  # when clicking on a category, 
  # 1st category, "category"
  observeEvent(event_data("plotly_click", source = "beskrivelse_1"), {
    beskrivelse_1(event_data("plotly_click", source = "beskrivelse_1")$y)
    beskrivelse_2(NULL)
    beskrivelse_3(NULL)
    beskrivelse_4(NULL)
  })
  
  # 2nd category, "sub_category"
  observeEvent(event_data("plotly_click", source = "beskrivelse_2"), {
    beskrivelse_2(
      event_data("plotly_click", source = "beskrivelse_2")$y
    )
    beskrivelse_3(NULL)
    beskrivelse_4(NULL)
  })
  
  # 3rd category, "subsub_category"
  observeEvent(event_data("plotly_click", source = "beskrivelse_3"), {
    beskrivelse_3(
      event_data("plotly_click", source = "beskrivelse_3")$y
    )
    beskrivelse_4(NULL)
  })
  
  # 4th category, "subsubsub_category"
  observeEvent(event_data("plotly_click", source = "beskrivelse_4"), {
    beskrivelse_4(
      event_data("plotly_click", source = "beskrivelse_4")$y
    )
  })
  

  
  
  output$beskrivelse_1 <- renderPlotly({
    
    # Level of detail
    #niv_in <- "Niveau 1"
    
    # Generates data for plot
    #data_work <- df[!(is.na(df[[niv_in]])),]
    data_work <- df[df$Dato == input$year_month_input,]    
    # Specifies the categories
    #x_in <- data_work$beskrivelse
    
    # Specefies the values of the categories
    y_val <- list("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(y_val) <- c("Indeks", "Sidste måned", "Samme måned sidste år")
    y_in <- y_val[[input$y_var]]
    
    
    
    # Deffining plot based on ordering choice 
    
    if(input$order_input == "Alfabetisk"){
      p_1 <- data_work %>%
        filter(is.na(beskrivelse_2)) %>%
        plot_ly(x = ~.data[[y_in]], y = ~beskrivelse_1, type = "bar", color = ~.data[[y_in]]>0, colors = c("yellow", "blue"), source = "beskrivelse_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_1),
               showlegend = FALSE) 
      p <- p_1 %>%
        filter(~.data[[y_in]]==0) %>%
        add_trace(x = ~.data[[y_in]], mode = 'scatter')
      
      
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>%
        filter(is.na(beskrivelse_2)) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_1, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_1),
               showlegend = FALSE)  
      

    } else {
      p <- data_work %>%
        filter(is.na(beskrivelse_2)) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_1, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_1") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_1),
               showlegend = FALSE)  

    }
    
    
    
  })
  
  output$beskrivelse_2 <- renderPlotly({
    if (is.null(beskrivelse_1())) return(NULL)
    
    
    # Generates data for plot
    #data_work <- df[!(is.na(df[[niv_in]])),]
    data_work <- df[df$Dato == input$year_month_input,]
    
    # Specifies the categories
    #x_in <- data_work$beskrivelse
    
    # Specefies the values of the categories
    y_val <- list("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(y_val) <- c("Indeks", "Sidste måned", "Samme måned sidste år")
    y_in <- y_val[[input$y_var]]
    
    
    
    
    # Deffining plot based on ordering choice 
    
    if(input$order_input == "Alfabetisk"){
      p <- data_work %>% 
        filter(is.na(beskrivelse_3)) %>%
        filter(beskrivelse_1 %in% beskrivelse_1()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~beskrivelse_2, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_2),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>% 
        filter(is.na(beskrivelse_3)) %>%
        filter(beskrivelse_1 %in% beskrivelse_1()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_2, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_2),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>% 
        filter(is.na(beskrivelse_3)) %>%
        filter(beskrivelse_1 %in% beskrivelse_1()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_2, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_2") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_2),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
  output$beskrivelse_3 <- renderPlotly({
    if (is.null(beskrivelse_1())) return(NULL)
    if (is.null(beskrivelse_2())) return(NULL)
    
    # Generates data for plot
    #data_work <- df[!(is.na(df[[niv_in]])),]
    data_work <- df[df$Dato == input$year_month_input,]
    
    # Specifies the categories
    #x_in <- data_work$beskrivelse
    
    # Specefies the values of the categories
    y_val <- list("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(y_val) <- c("Indeks", "Sidste måned", "Samme måned sidste år")
    y_in <- y_val[[input$y_var]]
    
    
    
    
    # Deffining plot based on ordering choice 
    
    if(input$order_input == "Alfabetisk"){
      p <- data_work %>% 
        filter(is.na(beskrivelse_4)) %>%
        filter(beskrivelse_2 %in% beskrivelse_2()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~beskrivelse_3, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_3),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>% 
        filter(is.na(beskrivelse_4)) %>%
        filter(beskrivelse_2 %in% beskrivelse_2()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_3, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_3),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>% 
        filter(is.na(beskrivelse_4)) %>%
        filter(beskrivelse_2 %in% beskrivelse_2()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_3, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_3") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_3),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
  output$beskrivelse_4 <- renderPlotly({
    if (is.null(beskrivelse_1())) return(NULL)
    if (is.null(beskrivelse_2())) return(NULL)
    if (is.null(beskrivelse_3())) return(NULL)
    
    # Generates data for plot
    #data_work <- df[!(is.na(df[[niv_in]])),]
    data_work <- df[df$Dato == input$year_month_input,]
    
    # Specifies the categories
    #x_in <- data_work$beskrivelse
    
    # Specefies the values of the categories
    y_val <- list("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(y_val) <- c("Indeks", "Sidste måned", "Samme måned sidste år")
    y_in <- y_val[[input$y_var]]
    
    
    
    
    # Deffining plot based on ordering choice 
    
    if(input$order_input == "Alfabetisk"){
      p <- data_work %>% 
        filter(beskrivelse_3 %in% beskrivelse_3()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~beskrivelse_4, type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_4),
               showlegend = FALSE)  
      
    } else if(input$order_input == "Aftagende"){
      p <- data_work %>% 
        filter(beskrivelse_3 %in% beskrivelse_3()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_4, .data[[y_in]]), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_4),
               showlegend = FALSE)  
      
      
    } else {
      p <- data_work %>% 
        filter(beskrivelse_3 %in% beskrivelse_3()) %>%
        plot_ly(x = ~.data[[y_in]], y = ~reorder(beskrivelse_4, desc(.data[[y_in]])), type = "bar", color = ~.data[[y_in]] > 0, colors = c("yellow", "blue"), source = "beskrivelse_4") %>%
        layout(xaxis = list(titel = " "),
               yaxis = list(title = beskrivelse_4),
               showlegend = FALSE)  
      
    }
    
    
    
  })
  
})