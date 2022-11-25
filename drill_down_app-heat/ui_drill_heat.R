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
df <- read_xlsx("data.xlsx")


#### With sidepanel filters

# ui <- fluidPage(
#   
#   titlePanel("Visualisering af det danske forbrugerprisindeks"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       h4("Filtre"),
# 
#       selectInput(  # first filter, enhed
#         inputId = "enhed",
#         label = "V\u00E6lg en enhed",
#         choices = c("Indeks"="vaerdi_i",
#                     "\u00C6ndring ift. m\u00E5neden f\u00F8r (pct.)"="vaerdi_ae_m",  # encoded directly with UTF-8, \u00C6ndring = Ændring
#                     "\u00C6ndring ift. samme m\u00E5ned \u00E5ret f\u00F8r (pct.)"="vaerdi_ae_aa"),
#         selected = "Indeks"
#       ),
#       sliderInput(  # third filter, måned
#         inputId = "maaned_interval",
#         label = "V\u00E6lg et interval af m\u00E5neder",
#         min=min(as.Date(df$Dato)),
#         max=max(as.Date(df$Dato)),
#         value=c(min(as.Date(df$Dato)),max(as.Date(df$Dato))), # from 2020M01-2022M08
#         timeFormat="%Y-%m"
#       )
#     ),
# 
#     mainPanel(
#       plotlyOutput("niv_1"),
#       plotlyOutput("niv_2"),
#       plotlyOutput("niv_3"),
#       plotlyOutput("niv_4")
#     )
#   )
# 
# )

#### With horizontal filters

ui <- fluidPage(

  titlePanel("Visualisering af det danske forbrugerprisindeks"),
  
  p("Denne interaktive visualisering har til form\u00E5l at visualisere ", 
    a(href = "https://www.statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&subword=tabsel&MainTable=PRIS111&PXSId=230965&tablestyle=&ST=SD&buttons=0", 
              "det danske forbrugerprisindeks.")), 
  p("Data er indhentet fra Danmarks Statistik for m\u00E5nederne januar, 2020, til august, 2022."),
  
  p("Visualiseringen tillader filtrering p\u00E5 b\u00E5de enhed, m\u00E5neder og sortering (for bar chart).\nBem\u00E6rk, at 
    v\u00E6rdiskalaen \u00E6ndrer sig alt efter, hvilken enhed der v\u00E6lges. S\u00E5ledes repr\u00E6senterer farverne ikke de samme v\u00E6rdier 
    p\u00E5 tv\u00E6rs af enheder."),

  br(),
  hr(),

  h4("Filtre"),

  fluidRow(
    column(4,  # 10 = width of column for first filter
           selectInput(  # first filter, enhed
             inputId = "enhed",
             label = "V\u00E6lg en enhed",
             choices = c("Indeks"="vaerdi_i",
                         "\u00C6ndring ift. m\u00E5neden f\u00F8r (pct.)"="vaerdi_ae_m",  # encoded directly with UTF-8, \u00C6ndring = Ændring
                         "\u00C6ndring ift. samme m\u00E5ned \u00E5ret f\u00F8r (pct.)"="vaerdi_ae_aa"),
             selected = "Indeks"
           )),
    column(4,
           sliderInput(  # second filter, måned
             inputId = "maaned_interval",
             label = "V\u00E6lg et interval af m\u00E5neder",
             min=min(as.Date(df$Dato)),
             max=max(as.Date(df$Dato)),
             value=c(min(as.Date(df$Dato)),max(as.Date(df$Dato))), # from 2020M01-2022M08
             timeFormat="%Y-%m"
           ))),

  hr(),  # horizontal, grey line

  plotlyOutput("niv_1"),
  plotlyOutput("niv_2"),
  plotlyOutput("niv_3"),
  plotlyOutput("niv_4"),
  plotlyOutput("bar")

)


# selectInput(  # first filter, enhed
#   inputId = "enhed",
#   label = "Choose a unit.",
#   choices = c("Indeks",
#               "Ændring ift. måneden før (pct.)",
#               "Ændring ift. samme måned året før (pct.)"),
#   selected = "Indeks"
# )
