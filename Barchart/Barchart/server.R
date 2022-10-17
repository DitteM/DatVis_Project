#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Kig evt på plotly

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$barPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    y_in <- input$y_var
    
    order_val <- input$order_input
    reorder_val <- list(y_in, -y_in, +y_in)
    names(reorder_val) <- c("Numerisk", "Aftagende", "Stigende")
    
    order_in <- reorder_val[order_val]
    
    x_in <- as.numeric(df$`num_category`)
    
    
    p <- ggplot(data=df, aes(x = reorder(x_in, order_in), y = y_in)) + 
      geom_bar(stat="identity") + 
      geom_text(aes(label = y_in), vjust = -1, colour = "black") +
      labs(x = "Kategori", y = "Ændring i forbrugerprisindeks") +
      coord_flip()
    
    ggplotly(p)
    
    
    
  })
  
})