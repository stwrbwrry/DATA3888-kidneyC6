#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application 
shinyUI(fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
    # Create a Navbar
    navbarPage("Shared Kidney Risk Calculator",
               tabPanel("BioMarkers"),
               tabPanel("ML Methods"),
               tabPanel("About")
    ),
    fluidRow(
      column(2,
             "sidebar"
      ),
      column(10,
             " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur id sodales erat, eu mollis nunc. Quisque magna turpis, sollicitudin eget ultrices non, vestibulum eget leo. Maecenas quis nisl imperdiet, tincidunt nibh a, ultricies nulla. Aenean quis nibh id nunc aliquet elementum. Vivamus malesuada nec felis in feugiat. Nam et odio volutpat, congue lacus ut, maximus felis. Aenean in rhoncus quam. Aliquam commodo nulla at nulla pharetra, a varius lorem faucibus. Suspendisse fringilla est a tellus eleifend, vestibulum interdum leo efficitur. Nam accumsan aliquet purus, id consectetur purus sollicitudin sed. Mauris quis tellus ut magna aliquam varius quis eget orci. Aenean ut quam id tortor egestas efficitur. In hac habitasse platea dictumst. Morbi ac cursus felis, id malesuada lacus. Vestibulum volutpat tristique purus, malesuada consequat ligula elementum vel. Curabitur quis ex vitae turpis placerat fringilla.

Duis consectetur quis risus nec porta. Donec eget justo eget elit tincidunt cursus. Nulla facilis"
      )
    ),
    h1("hey bro!"),
    
    fluidRow(
      column(4),
      column(4, "hello this is some text"),
      column(4)
    )
))
