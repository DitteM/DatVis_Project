#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
library(plotly)
library(readxl)
library(lazyeval)
library(tidyverse)
library(RColorBrewer)

# The server is where you load libraries and data files
# and define charts/plots 
df <- read_xlsx("data.xlsx")

## code here temporarily under test phase
# making sure every category is on same form
df <- df %>%
  mutate(niv_1 = ifelse(niv_1 == "00", "00.", niv_1),
         niv_1 = ifelse(niv_1 == "01", "01.", niv_1),
         niv_4 = ifelse(str_count(niv_4)==8, niv_4, substr(niv_4, star=1, stop = 8)))

# adding trace of levels to the data
df <- df %>%
  mutate(niv_3 = ifelse(is.na(niv_3), substr(niv_4, start=1, stop = 6), niv_3),
         niv_2 = ifelse(is.na(niv_2), substr(niv_3, start=1, stop = 4), niv_2),
         niv_1 = ifelse(is.na(niv_1), substr(niv_2, start=1, stop = 3), niv_1))

##### TEMP - talk to Ditte
# remove . from niv_1 - otherwise, you can choose both 11 and 11. e.g.
df$niv_1 <- substr(df$niv_1,1,2)


############ Drill_down server

server <- function(input, output) {

  # filter by months
  df2 <- reactive({
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))

  })

  # for maintaining the state of drill-down variables
  # reactive values for levels
  niv_1 <- reactiveVal()
  niv_2 <- reactiveVal()
  niv_3 <- reactiveVal()
  niv_4 <- reactiveVal()
  
  # reactive value for date (bridge between heatmaps and bar charts)
  date <- reactiveVal()

  ##### reactive categories

  # when clicking on a cell in the heatmap from the source niv_1
  # meaning the output$niv_1 plotly graph (first plot)
  # 1st category
  observeEvent(event_data("plotly_click", source = "niv_1"), {
    # pull out y-coordinate (goods group) for event on first graph and assign to reactive value
    niv_1(event_data("plotly_click", source = "niv_1")$y)
    niv_2(NULL)
    niv_3(NULL)
    niv_4(NULL)
    
    # pull out x-coordinate (date)
    date(event_data("plotly_click", source = "niv_1")$x)
  })

  # 2nd category
  observeEvent(event_data("plotly_click", source = "niv_2"), {
    niv_2(event_data("plotly_click", source = "niv_2")$y)
    niv_3(NULL)
    niv_4(NULL)
    
    date(event_data("plotly_click", source = "niv_2")$x)
  })

  # 3rd category
  observeEvent(event_data("plotly_click", source = "niv_3"), {
    niv_3(event_data("plotly_click", source = "niv_3")$y)
    niv_4(NULL)
    
    date(event_data("plotly_click", source = "niv_3")$x)
  })

  # 4th category
  observeEvent(event_data("plotly_click", source = "niv_4"), {
    niv_4(event_data("plotly_click", source = "niv_4")$y)
    
    date(event_data("plotly_click", source = "niv_4")$x)
  })

  #######
  # color scale: -50 to 298 (min of either unit to max)
  
  ##### first table
  
  temp <- subset(df, !is.na(niv_1) & is.na(niv_2) & is.na(niv_3) & is.na(niv_4))
  Kode <- unique(temp$kode)
  Beskrivelse <- unique(temp$beskrivelse)
  temp <- cbind(Kode,Beskrivelse)
  
  # mangler nogle kategorier, for some reason
  
  output$tabel_1 <- renderDataTable({
    datatable(
      temp,
      options = list(
        scrollX = TRUE,
        scrollY = "250px"
      )
    )
  })
  
  ##### first plot
  
  output$niv_1 <- renderPlotly({

    p_heat <- df2() %>%
      plot_ly(x=~Dato, y=~niv_1, z = ~get(input$enhed), source = "niv_1", # match with source argument in event_data to retrieve reactive values
              type="heatmap",
              xgap = 0.4, ygap = 0.4,   # set gridlines around cells
              # colorscale=colorscale, # 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units, 0-centered
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      colorbar(title = "Heatmap,\nv\u00E6rdiskala", len=1) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe', autorange="reversed"))   # 00 at the top instead of 15
       
    if (is.null(niv_1())) return(p_heat) # if the user hasn't clicked on a plot yet
    
    p_hor <- df2() %>%
      filter(is.na(niv_2)) %>%
      filter(niv_1 %in% niv_1()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#0D15B6", "#B6AE0D")) %>%
      plot_ly(y = ~get(input$enhed), x = ~Dato, type = "bar", 
              color = ~I(pos),  # stop scaling of colors
              # source = "niv_2",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'V\u00E6rdi'),
             showlegend = F) 
    
    sub <- subset(df2(), as.Date(Dato)==as.Date(date()))
    
    p_vert <- sub %>%
      filter(is.na(niv_2)) %>%
      # filter(as.Date(Dato) %in% date()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#0D15B6", "#B6AE0D")) %>%
      plot_ly(x = ~get(input$enhed), y = ~niv_1, type = "bar", 
              color = ~I(pos),
              # source = "niv_2",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = "V\u00E6rdi"),
             yaxis = list(title = "Varegruppe", autorange="reversed"),   # 00 at the top instead of 15
             showlegend = F) 
    
    
    p <- subplot(p_hor, p_heat, p_vert, shareX = FALSE, shareY = FALSE, titleX=TRUE, titleY=TRUE)
    p %>% layout(annotations = list(
                   list(x = 0.1 , y = 1.07, text = paste0("Alle m\u00E5neder, varegruppe ", niv_1()), font = list(size = 14.5), showarrow = F, xref='paper', yref='paper'),
                   list(x = 0.5 , y = 1.07, text = "Alle varegrupper, alle m\u00E5neder", font = list(size = 14.5), showarrow = F, xref='paper', yref='paper'),
                   list(x = 0.9 , y = 1.07, text = paste0("Alle varegrupper, ", format(as.Date(date()),"%Y-%m")), font = list(size = 14.5), showarrow = F, xref='paper', yref='paper')))
    
  })
  
  ##### second table
  
  temp2 <- subset(df, !is.na(niv_1) & !is.na(niv_2) & is.na(niv_3) & is.na(niv_4))
  Kode2 <- unique(temp2$niv_2)
  Beskrivelse2 <- unique(temp2$beskrivelse[str_count(temp2$kode)==4])
  temp2 <- cbind(Kode2,Beskrivelse2)
  
  output$tabel_2 <- renderDataTable({
    datatable(
      temp2,
      options = list(
        scrollX = TRUE,
        scrollY = "250px"
      )
    )
  })

  
  ##### second plot
  
  output$niv_2 <- renderPlotly({

    if (is.null(niv_1())) return(NULL)

    df2() %>% filter(niv_1 %in% niv_1()) %>%
      plot_ly(x=~Dato, y=~niv_2, z = ~get(input$enhed), source = "niv_2", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe', autorange="reversed"))

  })
  
  ##### third table
  
  temp3 <- subset(df, !is.na(niv_1) & !is.na(niv_2) & !is.na(niv_3) & is.na(niv_4))
  Kode3 <- unique(temp3$niv_3)
  Beskrivelse3 <- unique(temp3$beskrivelse[str_count(temp3$kode)==6])
  temp3 <- cbind(Kode3,Beskrivelse3)
  
  output$tabel_3 <- renderDataTable({
    datatable(
      temp3,
      options = list(
        scrollX = TRUE,
        scrollY = "250px"
      )
    )
  })

  
  ##### third plot
  
  output$niv_3 <- renderPlotly({

    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)

    df2() %>%
      filter(niv_2 %in% niv_2()) %>%
      plot_ly(x=~Dato, y=~niv_3, z = ~get(input$enhed), source = "niv_3", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe', autorange="reversed")) 
  })
  
  ##### fourth table
  
  temp4 <- subset(df, !is.na(niv_1) & !is.na(niv_2) & !is.na(niv_4) & !is.na(niv_4))
  Kode4 <- unique(temp4$niv_4)
  Beskrivelse4 <- unique(temp4$beskrivelse[str_count(temp4$kode)==8])
  temp4 <- cbind(Kode4,Beskrivelse4)
  
  output$tabel_4 <- renderDataTable({
    datatable(
      temp4,
      options = list(
        scrollX = TRUE,
        scrollY = "250px"
      )
    )
  })
  
  ##### fourth plot

  output$niv_4 <- renderPlotly({

    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)
    if (is.null(niv_3())) return(NULL)

    df2() %>%
      filter(niv_3 %in% niv_3()) %>%
      plot_ly(x=~Dato, y=~niv_4, z = ~get(input$enhed), source = "niv_4", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', niv_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe', autorange="reversed")) 
  })

}



##################

# server <- function(input, output) {
# 
#   output$plot <- renderPlotly({
# 
#     # filter on months
#     df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
# 
#     # get unit by filter
#     # done like this to ensure dynamic y-axis
#     enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
#     names(enhed_val) <- c("Indeks",
#                           "Ændring ift. måneden før (pct.)",
#                           "Ændring ift. samme måned året før (pct.)")
#     enhed_in <- enhed_val[[input$enhed]]
# 
#     # get level by filter
#     niveau_val <- list("niv_1", "niv_2", "niv_3", "niv_4")
#     names(niveau_val) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4")
#     niveau_in <- niveau_val[[input$niveau]]
# 
#     # filter out NA values to only have current level values
#     df <- df[!(is.na(df[[niveau_in]])),]
# 
#     # filter empty descriptions
#     df <- df[!(is.na(df$beskrivelse)),]
# 
#     plot_ly(df, x=~Dato, y=~get(enhed_in), type="scatter", color=~beskrivelse, mode="lines", hoverinfo='text',
#             text= ~paste('</br> År-måned: ', format(Dato,"%Y-%m"),
#                          '</br> Varegruppe: ', beskrivelse,
#                          '</br> Værdi: ', get(enhed_in))) %>%
#             layout(xaxis = list(title = 'Månedligt interval'),
#                    yaxis = list(title = input$enhed),
#                    legend = list(title=list(text='<b> Varegruppe </b>')))
# 
#   })
# }
