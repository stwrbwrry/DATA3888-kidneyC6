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
shinyUI(

    # Create a Navbar
    navbarPage(
      # TODO: loading bar for slow processing
      
      # get me the theme for website
      theme = shinytheme("sandstone"), 
      "Kidney Risk Calculator",
               tabPanel("DashBoard",
                        fluidRow(
                          column(12, 
                                 fileInput("userFile", label = h3("File input")),
                                 verbatimTextOutput("fileInput")
                                 ),
                          column(12, 
                                 fileInput("userFile", label = h3("File input")),
                                 verbatimTextOutput("fileInput")
                          )
                        )
                ),
               tabPanel("CPOP Explained",
                        h2("Motor cars(DELETE)"),
                        verbatimTextOutput("carExample")
                        ),
               tabPanel("Classifiers Explained",
                        h2("Titanic(DELETE)"),
                        verbatimTextOutput("titanicExample")
                        ),
               tabPanel("Datasets explained"),
               h1("Please don't hack"),
               hr(),
               p("By Mukund, Liam, Charlotte, Amy, Khang. Group: KidneyC6"),
      # import in the css style sheet
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
      ),
    )
    
)
