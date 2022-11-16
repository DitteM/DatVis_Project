#
# user-interface definition of Shiny web application
#

library(shiny)
library(plotly)
library(readxl)

#######################
## Tested with fluidPage. Incorporate with tabPanel when merging
## Specification:
## Three filters: Enhed, niveau, måned. Måned has to support both choosing a singular month and an interval (only with interval for now)
## Line chart: værdi on y-axis, måned on y-axis, one line per varegruppe (specificed in legend)
#######################

# Problems with data frame - not sourcing properly from file. Has to be loaded manually from source file
#df <- read_xlsx("data.xlsx")


# unit and levels choices -> vars, not values
units <- colnames(df[4:6])
levels <- colnames(df[7:10])

ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        
        selectInput(  # first filter, enhed
          inputId = "enhed",
          label = "Choose a unit.",
          choices = c("vaerdi_i", "vaerdi_ae_m", "vaerdi_ae_aa"),
          selected = "vaerdi_i"
        ),
        sliderInput(  # third filter, måned
          inputId = "maaned_interval",
          label = "Choose an interval of months.",
          min=min(as.Date(df$Dato)),
          max=max(as.Date(df$Dato)),
          value=c(min(as.Date(df$Dato)),max(as.Date(df$Dato))), # from 2020M01-2020M04
          timeFormat="%Y-%m"
        )
      ),
    
    mainPanel(
      plotlyOutput("niv_1", height = 200),
      plotlyOutput("niv_2", height = 200),
      plotlyOutput("niv_3", height = 200),
      plotlyOutput("niv_4", height = 200)
    )
    )
    
)


# selectInput(  # first filter, enhed
#   inputId = "enhed",
#   label = "Choose a unit.",
#   choices = c("Indeks",
#               "Ændring ift. måneden før (pct.)",
#               "Ændring ift. samme måned året før (pct.)"),
#   selected = "Indeks"
# )
