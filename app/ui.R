#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("Shared Kidney risk calculator"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("message", label= NULL, value = "", width = '100%', placeholder = NULL),
            code('install.packages("shiny")')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("uppercase")
        )
    ),
    sidebarLayout(
      sidebarPanel(
        textInput("message", label= NULL, value = "", width = '100%', placeholder = NULL)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("uppercase")
      )
    )
))
