library(shiny)
library(plotly)
library(readxl)
library(lazyeval)
library(tidyverse)
library(RColorBrewer)


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
  date1 <- reactiveVal()
  date2 <- reactiveVal()
  date3 <- reactiveVal()
  date4 <- reactiveVal()
  
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
    date1(event_data("plotly_click", source = "niv_1")$x)
  })
  
  # 2nd category
  observeEvent(event_data("plotly_click", source = "niv_2"), {
    niv_2(event_data("plotly_click", source = "niv_2")$y)
    niv_3(NULL)
    niv_4(NULL)
    
    date2(event_data("plotly_click", source = "niv_2")$x)
  })
  
  # 3rd category
  observeEvent(event_data("plotly_click", source = "niv_3"), {
    niv_3(event_data("plotly_click", source = "niv_3")$y)
    niv_4(NULL)
    
    date3(event_data("plotly_click", source = "niv_3")$x)
  })
  
  # 4th category
  observeEvent(event_data("plotly_click", source = "niv_4"), {
    niv_4(event_data("plotly_click", source = "niv_4")$y)
    
    date4(event_data("plotly_click", source = "niv_4")$x)
  })
  
  #######
  # color scale: -50 to 298 (min of either unit to max)
  output$niv_1 <- renderPlotly({
    
    p_heat <- df2() %>%
      filter(is.na(niv_2)) %>%
      plot_ly(x=~Dato, y=~niv_1, z = ~get(input$enhed), source = "niv_1", # match with source argument in event_data to retrieve reactive values
              type="heatmap",
              xgap = 0.4, ygap = 0.4,   # set gridlines around cells
              # colorscale=colorscale, # 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units, 0-centered
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1, orientation="h"),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe'))
    
    if (is.null(niv_1())) return(p_heat) 
    
    p_hor <- df2() %>%
      filter(is.na(niv_2)) %>%
      filter(niv_1 %in% niv_1()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(y = ~get(input$enhed), x = ~Dato, type = "bar", 
              color = ~I(pos),
              source = "niv_2",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'V\u00E6rdi'),
             showlegend = F)
    
    sub <- subset(df2(), as.Date(Dato)==as.Date(date1()))
    
    print(sub)
    
    p_vert <- sub %>%
      filter(is.na(niv_2)) %>%
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(x = ~get(input$enhed), y = ~niv_1, type = "bar", 
              color = ~I(pos),
              source = "niv_2",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_1,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = " "),
             yaxis = list(title = "Varegruppe"),
             showlegend = F)
    
    
    fig <- subplot(p_hor, subplot(p_heat, p_vert, shareY = TRUE))
    
  })
  
  output$niv_2 <- renderPlotly({
    
    if (is.null(niv_1())) return(NULL)
    
    df_tjek <- df2() %>%
      filter(niv_1 %in% niv_1()) %>%
      filter(!is.na(niv_2))
    
    if(nrow(df_tjek)==0) return(NULL)
    
    p_heat <- df2() %>%
      filter(!is.na(niv_2)) %>%
      filter(is.na(niv_3)) %>% 
      filter(niv_1 %in% niv_1()) %>%
      plot_ly(x=~Dato, y=~niv_2, z = ~get(input$enhed), source = "niv_2", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_2,
                           '</br> V\u00E6rdi: ',get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe')) 
    
    if (is.null(niv_2())) return(p_heat) 
    
    p_hor <- df2() %>%
      filter(!is.na(niv_2)) %>%
      filter(is.na(niv_3)) %>%
      filter(niv_2 %in% niv_2()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(y = ~get(input$enhed), x = ~Dato, type = "bar", 
              color = ~I(pos),
              source = "niv_3",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_2,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'V\u00E6rdi'),
             showlegend = F)
    
    sub <- subset(df2(), as.Date(Dato)==as.Date(date2()))
    
    
    p_vert <- sub %>%
      filter(!is.na(niv_2)) %>%
      filter(is.na(niv_3)) %>%
      filter(niv_1 %in% niv_1()) %>%
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(x = ~get(input$enhed), y = ~niv_2, type = "bar", 
              color = ~I(pos),
              source = "niv_3",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_2,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = " "),
             yaxis = list(title = "Varegruppe"),
             showlegend = F)
    
    
    fig <- subplot(p_hor, subplot(p_heat, p_vert, shareY = TRUE))
    
  })
  
  output$niv_3 <- renderPlotly({
    
    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)
    
    df_tjek <- df2() %>%
      filter(niv_2 %in% niv_2()) %>%
      filter(!is.na(niv_3)) %>%
      filter(is.na(niv_4))
    
    if(nrow(df_tjek)==0) return(NULL)
    
    p_heat <- df2() %>%
      filter(!is.na(niv_3)) %>%
      filter(is.na(niv_4)) %>% 
      filter(niv_2 %in% niv_2()) %>%
      plot_ly(x=~Dato, y=~niv_3, z = ~get(input$enhed), source = "niv_3", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_3,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe')) 
    
    if (is.null(niv_3())) return(p_heat) 
    
    p_hor <- df2() %>%
      filter(!is.na(niv_3)) %>%
      filter(is.na(niv_4)) %>%
      filter(niv_3 %in% niv_3()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(y = ~get(input$enhed), x = ~Dato, type = "bar", 
              color = ~I(pos),
              source = "niv_4",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_3,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'V\u00E6rdi'),
             showlegend = F)
    
    sub <- subset(df2(), as.Date(Dato)==as.Date(date3()))
    
    
    p_vert <- sub %>%
      filter(!is.na(niv_3)) %>%
      filter(is.na(niv_4)) %>%
      filter(niv_2 %in% niv_2()) %>%
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(x = ~get(input$enhed), y = ~niv_3, type = "bar", 
              color = ~I(pos),
              source = "niv_4",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_3,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = " "),
             yaxis = list(title = "Varegruppe"),
             showlegend = F)
    
    fig <- subplot(p_hor, subplot(p_heat, p_vert, shareY = TRUE))
    
    
  })
  
  output$niv_4 <- renderPlotly({
    
    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)
    if (is.null(niv_3())) return(NULL)
    
    
    df_tjek <- df2() %>%
      filter(niv_3 %in% niv_3()) %>%
      filter(!is.na(niv_4))
    
    if(nrow(df_tjek)==0) return(NULL)
    
    
    p_heat <- df2() %>%
      filter(!is.na(niv_4)) %>%
      filter(niv_3 %in% niv_3()) %>%
      plot_ly(x=~Dato, y=~niv_4, z = ~get(input$enhed), source = "niv_4", type="heatmap",
              xgap = 0.4, ygap = 0.4,
              # colorscale=colorscale, 0-centered
              # zauto=FALSE, zmin=-50, zmax=298,  # fix colors, across all units
              zauto=FALSE, zmin=min(input$enhed,na.rm=T), zmax=max(input$enhed,na.rm=T), # fix colors, per unit
              colorbar = list(title = "V\u00E6rdiskala,\nenhed", len=1),
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_4,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(title = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'Varegruppe')) 
    
    if (is.null(niv_4())) return(p_heat) 
    
    p_hor <- df2() %>%
      filter(!is.na(niv_4)) %>%
      filter(niv_4 %in% niv_4()) %>% 
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(y = ~get(input$enhed), x = ~Dato, type = "bar", 
              color = ~I(pos),
              source = "niv_4",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_4,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = 'M\u00E5nedligt interval'),
             yaxis = list(title = 'V\u00E6rdi'),
             showlegend = F)
    
    sub <- subset(df2(), as.Date(Dato)==as.Date(date4()))
    
    
    p_vert <- sub %>%
      filter(!is.na(niv_4)) %>%
      filter(niv_3 %in% niv_3()) %>%
      mutate(pos = ifelse(get(input$enhed) >= 0, "#00008b", "#e5de00")) %>%
      plot_ly(x = ~get(input$enhed), y = ~niv_4, type = "bar", 
              color = ~I(pos),
              source = "niv_4",
              hoverinfo='text',
              text= ~paste('</br> \u00C5r-m\u00E5ned: ', format(Dato,"%Y-%m"),
                           '</br> Varegruppe: ', beskrivelse_4,
                           '</br> V\u00E6rdi: ', get(input$enhed))) %>%
      layout(xaxis = list(titel = " "),
             yaxis = list(title = "Varegruppe"),
             showlegend = F)
    
    fig <- subplot(p_hor, subplot(p_heat, p_vert, shareY = TRUE))
  })
  
  ###################
  
  
  
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
