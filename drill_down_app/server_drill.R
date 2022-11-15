#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
library(plotly)
library(readxl)
library(lazyeval)
library(tidyverse)

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

############ Drill_down server


server <- function(input, output) {
  
  # define data based on unit and month interval chosen from filter
  # df <- reactive({
  #   # filter on months
  #   #df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
  #   df <- df
  #   # get unit by filter
  #   # done like this to ensure dynamic y-axis
  #   # enhed_val <- c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa")
  #   # names(enhed_val) <- c("Indeks",
  #   #                       "Ændring ift. måneden før (pct.)",
  #   #                       "Ændring ift. samme måned året før (pct.)")
  #   # enhed_in <- enhed_val[[input$enhed]]
  #   
  # })
  
  # for maintaining the state of drill-down variables
  niv_1 <- reactiveVal()
  niv_2 <- reactiveVal()
  niv_3 <- reactiveVal()
  niv_4 <- reactiveVal()
  
  ##### reactive categories
  
  # when clicking on a category, 
  # 1st category, "category"
  observeEvent(event_data("plotly_click", source = "niv_1"), {
    niv_1(event_data("plotly_click", source = "niv_1")$x)
    niv_2(NULL)
    niv_3(NULL)
    niv_4(NULL)
  })
  
  # 2nd category, "sub_category"
  observeEvent(event_data("plotly_click", source = "niv_2"), {
    niv_2(
      event_data("plotly_click", source = "niv_2")$x
    )
    niv_3(NULL)
    niv_4(NULL)
  })
  
  # 3rd category, "subsub_category"
  observeEvent(event_data("plotly_click", source = "niv_3"), {
    niv_3(
      event_data("plotly_click", source = "niv_3")$x
    )
    niv_4(NULL)
  })

  # 4th category, "subsubsub_category"
  observeEvent(event_data("plotly_click", source = "niv_4"), {
    niv_4(
      event_data("plotly_click", source = "niv_4")$x
    )
  })
  
  #######
  output$niv_1 <- renderPlotly({
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
    
    df %>% 
    plot_ly(x = ~niv_1, y = ~get(input$enhed), source = "niv_1")
  })
  
  output$niv_2 <- renderPlotly({
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
    
    if (is.null(niv_1())) return(NULL)
    
    df %>% filter(niv_1 %in% niv_1()) %>% 
      plot_ly(x = ~niv_2, y = ~get(input$enhed), source = "niv_2")
    
  })
  
  output$niv_3 <- renderPlotly({
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
    
    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)

    df %>%
      filter(niv_2 %in% niv_2()) %>%
      plot_ly(x = ~niv_3, y = ~get(input$enhed), source = "niv_3")
  })

  output$niv_4 <- renderPlotly({
    df <- subset(df, as.Date(df$Dato) >= as.Date(input$maaned_interval[1]) & as.Date(df$Dato) <= as.Date(input$maaned_interval[2]))
    
    if (is.null(niv_1())) return(NULL)
    if (is.null(niv_2())) return(NULL)
    if (is.null(niv_3())) return(NULL)

    df %>%
      filter(niv_3 %in% niv_3()) %>%
      plot_ly(x = ~niv_4, y = ~get(input$enhed), source = "niv_4")
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
