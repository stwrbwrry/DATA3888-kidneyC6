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
shinyUI(fluidPage(theme = shinytheme("sandstone"),
    # Create a Navbar
    navbarPage("Kidney Risk Calculator",
               tabPanel("DashBoard",
                        sidebarLayout(sidebarPanel(
                          textInput("txt", "Text input:", "text here"),
                          sliderInput("slider", "Slider input:", 1, 100, 30),
                          actionButton("action", "Button"),
                          actionButton("action2", "Button2", class = "btn-primary")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Tab 1",
                                     fluidRow(
                                       column(4, ""),
                                       column(4,  paste("Text value: ", "30", sep=" "), 
                                              br(),  
                                              paste("Current slider value: ", "30", sep=" ")),
                                       column(4, "end")
                                     )
                                     
                                    
                                     ),
                            tabPanel("Tab 2")
                          )
                        )),
                        
                        fluidRow(
                          column(2,
                                 "sidebar"
                          ),
                          column(10,
                                 " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur id sodales erat, eu mollis nunc. Quisque magna turpis, sollicitudin eget ultrices non, vestibulum eget leo. Maecenas quis nisl imperdiet, tincidunt nibh a, ultricies nulla. Aenean quis nibh id nunc aliquet elementum. Vivamus malesuada nec felis in feugiat. Nam et odio volutpat, congue lacus ut, maximus felis. Aenean in rhoncus quam. Aliquam commodo nulla at nulla pharetra, a varius lorem faucibus. Suspendisse fringilla est a tellus eleifend, vestibulum interdum leo efficitur. Nam accumsan aliquet purus, id consectetur purus sollicitudin sed. Mauris quis tellus ut magna aliquam varius quis eget orci. Aenean ut quam id tortor egestas efficitur. In hac habitasse platea dictumst. Morbi ac cursus felis, id malesuada lacus. Vestibulum volutpat tristique purus, malesuada consequat ligula elementum vel. Curabitur quis ex vitae turpis placerat fringilla.

Duis consectetur quis risus nec porta. Donec eget justo eget elit tincidunt cursus. Nulla facilis"
                          )
                        )
                ),
               tabPanel("bioMarkers",
                        h2("Motor cars(DELETE)"),
                        verbatimTextOutput("carExample")
                        ),
               tabPanel("genes",
                        h2("Titanic(DELETE)"),
                        verbatimTextOutput("titanicExample")
                        ),
               tabPanel("Classifiers"),
               tabPanel("about",
                        h2("May be delete this if not needed"))
    ),
    
    h1("Please don't hack"),
    hr(),
    p("By Mukund, Liam, Charlotte, Amy, Khang. Group: KidneyC6")
    
    
))
