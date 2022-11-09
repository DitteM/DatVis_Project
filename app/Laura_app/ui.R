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

# unit and levels choices -> vars, not values
units <- colnames(df[4:6])
levels <- colnames(df[7:10])

ui1 <- fluidPage(
  "Line charts of consumer price index",

  titlePanel("Development of consumer price index with line charts"),

  p("This panels seeks to investigate the consumer price index fluctuations in terms of line charts, where the x-axis is the month, the y-axis is the value and the lines represent goods groups."),
  p(a(href = "https://www.statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&subword=tabsel&MainTable=PRIS111&PXSId=230965&tablestyle=&ST=SD&buttons=0", "Link to data.")),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),

      selectInput(  # first filter, enhed
        inputId = "enhed",
        label = "Choose a unit.",
        choices = c("Indeks",
                    "Ændring ift. måneden før (pct.)",
                    "Ændring ift. samme måned året før (pct.)"),
        selected = "Indeks"
      ),
      selectInput(  # second filter, niveau
        inputId = "niveau",
        label = "Choose a level.",
        choices = c("Niveau 1",
                    "Niveau 2",
                    "Niveau 3",
                    "Niveau 4"),
        selected = "Niveau 1"
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
      plotlyOutput(outputId = "plot")
    )
  )
)

###############################################

ui2 <- fluidPage(
  "Heatmap",

  titlePanel("Development of consumer price index with heatmap"),

  p("This panels seeks to investigate the consumer price index fluctuations in terms of a heatmap, where the x-axis is the month, the y-axis is the goods group and the values the unit."),
  p(a(href = "https://www.statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&subword=tabsel&MainTable=PRIS111&PXSId=230965&tablestyle=&ST=SD&buttons=0", "Link to data.")),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),

      selectInput(  # first filter, enhed
        inputId = "enhed",
        label = "Choose a unit.",
        choices = c("Indeks",
                    "Ændring ift. måneden før (pct.)",
                    "Ændring ift. samme måned året før (pct.)"),
        selected = "Indeks"
      ),
      selectInput(  # second filter, niveau
        inputId = "niveau",
        label = "Choose a level.",
        choices = c("Niveau 1",
                    "Niveau 2",
                    "Niveau 3",
                    "Niveau 4"),
        selected = "Niveau 1"
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
      plotlyOutput(outputId = "plot")
    )
  )
)

###############################################

ui3 <- fluidPage(
  "Heatmap",

  titlePanel("Development of consumer price index with heatmap"),

  p("This panels seeks to investigate the consumer price index fluctuations in terms of a heatmap, where the x-axis is the month, the y-axis is the goods group and the values the unit."),
  p(a(href = "https://www.statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&subword=tabsel&MainTable=PRIS111&PXSId=230965&tablestyle=&ST=SD&buttons=0", "Link to data.")),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),

      selectInput(  # first filter, enhed
        inputId = "enhed",
        label = "Choose a unit.",
        choices = c("Indeks",
                    "Ændring ift. måneden før (pct.)",
                    "Ændring ift. samme måned året før (pct.)"),
        selected = "Indeks"
      ),
      selectInput(  # second filter, niveau
        inputId = "niveau",
        label = "Choose a level.",
        choices = c("Niveau 1",
                    "Niveau 2",
                    "Niveau 3",
                    "Niveau 4"),
        selected = "Niveau 1"
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
      plotlyOutput(outputId = "plot")
    )
  )
)

###############################################

# Animation

ui4 <- fluidPage(
  "Heatmap",

  titlePanel("Development of consumer price index with heatmap"),

  p("This panels seeks to investigate the consumer price index fluctuations in terms of a heatmap, where the x-axis is the month, the y-axis is the goods group and the values the unit."),
  p(a(href = "https://www.statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&subword=tabsel&MainTable=PRIS111&PXSId=230965&tablestyle=&ST=SD&buttons=0", "Link to data.")),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),

      selectInput(  # first filter, enhed
        inputId = "enhed",
        label = "Choose a unit.",
        choices = c("Indeks",
                    "Ændring ift. måneden før (pct.)",
                    "Ændring ift. samme måned året før (pct.)"),
        selected = "Indeks"
      ),
      selectInput(  # second filter, niveau
        inputId = "niveau",
        label = "Choose a level.",
        choices = c("Niveau 1",
                    "Niveau 2",
                    "Niveau 3",
                    "Niveau 4"),
        selected = "Niveau 1"
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
      plotlyOutput(outputId = "plot")
    )
  )
)


