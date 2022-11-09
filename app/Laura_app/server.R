#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
library(plotly)
library(readxl)
library(lazyeval)
library(tidyverse)
library(gganimate)

# The server is where you load libraries and data files
# and define charts/plots 
df <- read_xlsx("data.xlsx")

# figure out how to incorporate filters into vis


server1 <- function(input, output) {

  output$plot <- renderPlotly({

    # filter on months
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))

    # get unit by filter
    # done like this to ensure dynamic y-axis
    enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(enhed_val) <- c("Indeks",
                          "Ændring ift. måneden før (pct.)",
                          "Ændring ift. samme måned året før (pct.)")
    enhed_in <- enhed_val[[input$enhed]]

    # get level by filter
    niveau_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
    names(niveau_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
    niveau_in <- niveau_val[[input$niveau]]

    # filter out NA values to only have current level values
    df <- df[!(is.na(df[[niveau_in]])),]

    # filter empty descriptions
    df <- df[!(is.na(df$beskrivelse)),]

    plot_ly(df, x=~Dato, y=~get(enhed_in), type="scatter", color=~beskrivelse, mode="lines", hoverinfo='text',
            text= ~paste('</br> År-måned: ', format(Dato,"%Y-%m"),
                         '</br> Varegruppe: ', beskrivelse,
                         '</br> Værdi: ', get(enhed_in))) %>%
            layout(xaxis = list(title = 'Månedligt interval'),
                   yaxis = list(title = input$enhed),
                   legend = list(title=list(text='<b> Varegruppe </b>')))

  })
}

###############################################

server2 <- function(input, output) {

  output$plot <- renderPlotly({

    # filter on months
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))

    # get unit by filter
    # done like this to ensure dynamic y-axis
    enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(enhed_val) <- c("Indeks",
                          "Ændring ift. måneden før (pct.)",
                          "Ændring ift. samme måned året før (pct.)")
    enhed_in <- enhed_val[[input$enhed]]

    # get level by filter
    niveau_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
    names(niveau_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
    niveau_in <- niveau_val[[input$niveau]]

    # filter out NA values to only have current level values
    df <- df[!(is.na(df[[niveau_in]])),]

    # filter empty descriptions
    df <- df[!(is.na(df$beskrivelse)),]

    # filter missing values for unit
    df <- df[!(is.na(df[[enhed_in]])),]

    plot_ly(df,
           x = ~Dato, y = ~beskrivelse,
           z = ~get(enhed_in), type = "heatmap",
           colorbar = list(title = "Værdi"),
           hoverinfo='text', text= ~paste('</br> År-måned: ', format(Dato,"%Y-%m"),
                                          '</br> Varegruppe: ', beskrivelse,
                                          '</br> Værdi: ', get(enhed_in))) %>%
          layout(xaxis = list(title = 'Månedligt interval'),
                 yaxis = list(title = input$enhed))

  })
}

###############################################

server3 <- function(input, output) {

  output$plot <- renderPlotly({

    # filter on months
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))

    # get unit by filter
    # done like this to ensure dynamic y-axis
    enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(enhed_val) <- c("Indeks",
                          "Ændring ift. måneden før (pct.)",
                          "Ændring ift. samme måned året før (pct.)")
    enhed_in <- enhed_val[[input$enhed]]

    # get level by filter
    niveau_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
    names(niveau_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
    niveau_in <- niveau_val[[input$niveau]]

    # filter out NA values to only have current level values
    df <- df[!(is.na(df[[niveau_in]])),]

    # filter empty descriptions
    df <- df[!(is.na(df$beskrivelse)),]

    # filter missing values for unit
    df <- df[!(is.na(df[[enhed_in]])),]


    line <- plot_ly(df, x=~Dato, y=~get(enhed_in), type="scatter", color=~beskrivelse, mode="lines", hoverinfo='text',
            text= ~paste('</br> År-måned: ', format(Dato,"%Y-%m"),
                         '</br> Varegruppe: ', beskrivelse,
                         '</br> Værdi: ', get(enhed_in))) %>%
            layout(xaxis = list(title = 'Månedligt interval'),
                   yaxis = list(title = input$enhed),
                   legend = list(title=list(text='<b> Varegruppe </b>')))

    heat <- plot_ly(df,
            x = ~Dato, y = ~beskrivelse,
            z = ~get(enhed_in), type = "heatmap",
            colorbar = list(title = "Værdi"),
            hoverinfo='text', text= ~paste('</br> År-måned: ', format(Dato,"%Y-%m"),
                                           '</br> Varegruppe: ', beskrivelse,
                                           '</br> Værdi: ', get(enhed_in))) %>%
            layout(xaxis = list(title = 'Månedligt interval'),
                   yaxis = list(title = input$enhed))

    s <- subplot(
      line,
      plotly_empty(),
      heat,
      plotly_empty(),
      nrows = 2, margin = 0, heights = c(0.3, 0.7), widths = c(0.99, 0.01),
      shareX = TRUE, shareY = FALSE, titleX = FALSE, titleY = FALSE
    )
    layout(s, showlegend = FALSE)

    s

  })
}

###############################################

# Animation

server4 <- function(input, output) {

  output$plot <- renderPlotly({

    # filter on months
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))

    # get unit by filter
    # done like this to ensure dynamic y-axis
    enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
    names(enhed_val) <- c("Indeks",
                          "Ændring ift. måneden før (pct.)",
                          "Ændring ift. samme måned året før (pct.)")
    enhed_in <- enhed_val[[input$enhed]]

    # get level by filter
    niveau_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
    names(niveau_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
    niveau_in <- niveau_val[[input$niveau]]

    # filter out NA values to only have current level values
    df <- df[!(is.na(df[[niveau_in]])),]

    # filter empty descriptions
    df <- df[!(is.na(df$beskrivelse)),]

    # filter missing values for unit
    df <- df[!(is.na(df[[enhed_in]])),]

    gg <- ggplot(df, aes(get(enhed_in), beskrivelse, color = beskrivelse)) +
      geom_point(aes(frame = Dato))
    ggplotly(gg)

  })
}
