#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("../src/packages.R")
source("../src/process_data.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Forbrugerprisindeks",
  tabPanel("Histogram",
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
             ),
             mainPanel(
               plotOutput("distPlot")
             )
             
           )
    
  ),
  tabPanel("Plot",
           sidebarLayout(
             sidebarPanel(
               radioButtons("plotType", "Plot type",
                            c("Scatter"="p", "Line"="l")
               )
             ),
             mainPanel(
               plotOutput("plot")
             )
           )
  ),
  tabPanel("Summary",
           verbatimTextOutput("summary")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- as.numeric(df$`2020M01`[1:100])
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$plot <- renderPlot({
      plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
      summary(cars)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
