#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GEOquery) 

# set upload size of 150 mega bytes
options(shiny.maxRequestSize=150*1024^2)

# Define server logic
shinyServer(function(input, output) {
  output$carExample <- renderPrint({
        head(mtcars,7)
      })
  input
  output$titanicExample <- renderPrint({
    Titanic
  })
  
  output$text <- renderText({
    paste("Current Text value: ", input$text)
  })
  
  output$slider <- renderText({
    paste("Current Slider value: ", input$slider)
  })
  
  
  output$fileInput <- renderPrint({
    inputFile = getGEO(filename=input$userFile$datapath)
    return(slotNames(inputFile))
  })
  

})
