#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(visNetwork)
library(DT)

# options(shiny.autoreload = TRUE)



# Define UI for application 
shinyUI(
  
    # Create a Navbar
    navbarPage(
      
      # TODO: loading bar for slow processing
      "Kidney Risk Calculator",
      # get me the theme for website
      theme = shinytheme("sandstone"), 
      
               tabPanel("DashBoard",
                        fluidPage(
                          fluidRow(
                            column(12, 
                                   tags$h2("Purpose"),
                                   tags$p("Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), useShinyjs()
                            )),
                          
                          fluidRow(
                            column(12, 
                                   fileInput("userFile", label = h3("Upload csv file")),
                                   verbatimTextOutput("fileInput")
                            )),
                          fluidRow(
                            column(4,h3("Pairwise Genes"),

                                   DT::dataTableOutput("pairwiseGenes")
                                   
                            ),
                            column(8,h3("VisNetwork") ,
                                   visNetworkOutput("userFileVisNetwork")
                            )
                          ),fluidRow(
                            column(12, br()),
                            column(2, p("Download your pairwise genes", style="font-weight:bold;")),
                            column(6, downloadButton("downloadData", "Download pairwise genes CSV file"))
                          )
                          ,fluidRow(
                            column(4,h5("Want to see how your dataset compares to public datasets?")
                            ),
                            column(4, actionButton(inputId = "showHide", label = "show / hide"))
                            
                          ), fluidRow(
                            column(4,
                                   hidden(
                                    div(id="v1box", h4("GSE36059 vs uploaded data"),
                                        visNetworkOutput( "v1")
                                        ) 
                                     )
                            ),
                            column(4 ,
                                   hidden(
                                     div(id="v2box",h4("GSE48581 vs uploaded data"),
                                         visNetworkOutput( "v2")
                                         )
                                     )
                            ),
                            column(4 ,
                                   hidden(
                                     div(id="v3box",h4("GSE46474 vs uploaded data"),
                                         visNetworkOutput( "v3")
                                     )
                                   )
                            )
                          ),
                          fluidRow(
                            column(12, tags$h2("Dataset combination section:",style="font-weight:bold;")),
                            column(12, tags$p("The purpose of this section of our dashboard is to combine user uploaded in-house data with existing public datasets. This is to allow the researcher to have more samples to work with during their analysis and model building by leveraging historic research. ")), 
                            column(12, tags$p("Before combining the uploaded in-house data and public data, we apply a normalization procedure. This is done by first taking the log of all expression values, and then taking the pairwise differences of the top overlapping genes between all datasets. The transformed expression boxplot visualizes how the normalization process alleviates batch-differences.")),
                            column(12, tags$p("Another issue we aim to solve is the lack of clinical variables present in historic expression data. By extracting key gender-specific expression biomarkers, we are able to predict gender outcomes "))
                          ),
                          fluidRow(
                            column(12, tags$h4("Boxplot - highlights normalisation",style="font-weight:bold;")),
                            column(12,
                                   plotOutput("boxplot", click="plot_click")
                                   )
                          ),
                          fluidRow(
                            column(12, br()),
                            column(7, selectInput("select", label = tags$p("Choose gender to include in combined dataset",style="font-weight:bold;"), 
                                                  choices = list("Both" = 1, "Male" = 2, "Female" = 3), selected = 1)),
                            column(12, br()),
                            column(2, p("Then click the download link", style="font-weight:bold;")),
                            column(6, downloadButton("dc", "Download combined data CSV file"))
                            )
                           
                        ),# belongs to fluidPage
                        
                ),
               tabPanel("How it works!",
                        h2("Figure 1")
                        ),
      tabPanel("Datasets explained",
               fluidPage(
                 
                 
                 fluidRow(
                   column(12, tags$h4("GSE36059 - Dataset", style="font-weight:bold;")),
                   column(8, tags$p("This dataset uses the gene platform GPL570. It contains biopsy samples from 411 patients, 35 of which underwent T cell-mediated rejection and 36 of which underwent Antibody-mediated rejection. The dataset is from a 2014 study which explored the disappearance of T Cell-mediated rejection despite continued antibody-mediated rejection in late kidney transplant recipients. This dataset utilises a reasonably common gene platform, making it more likely that it will be able to be easily combined with other researcher’s datasets. It also has a large sample size, which will increase the training capacity, and therefore the accuracy of the CPOP model if this dataset is selected.")),
                   column(2, offset=1,
                          actionButton(inputId="blah", label= "GSE36959", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE36059', '_blank')")),
                   column(12, hr())
                 ),
                 fluidRow(
                   column(12, tags$h4("GSE46474 - Dataset", style="font-weight:bold;")),
                   column(8, tags$p("Similarly, this dataset uses the gene platform GPL570. It contains peripheral blood samples from 40 patients, and 20 of these patients had their kidneys rejected. The dataset is from a 2014 study that investigated novel methods for the integration of genomics and proteomics data in kidney transplant rejection research. This dataset utilises a common gene platform, meaning that it is more likely that it will be able to be combined with other datasets. It also contains peripheral blood samples, unlike the other two datasets, overall providing more options, especially to researchers who have also taken this type of sample.")),
                   column(2, offset=1,
                          actionButton(inputId="blah", label= "GSE46474", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE46474', '_blank')")),
                   column(12, hr())
                 ),
                 fluidRow(
                   column(12, tags$h4("GSE48581 - Dataset", style="font-weight:bold;")),
                   column(8, tags$p("This dataset also uses the gene platform GPL570. It contains biopsy samples from 306 patients, 32 of which underwent T cell-mediated rejection and 40 of which underwent Antibody-mediated rejection. The dataset is from a 2013 study which looked at the potential impact of microarray diagnosis of T cell-mediated rejection in kidney transplants. This dataset utilises a common gene platform, making it more likely that researchers will be able to easily combine their own datasets with this dataset. The dataset also has a large sample size, which will increase the accuracy of the CPOP model by increasing its training capacity.")),
                   column(2, offset=1,
                          actionButton(inputId="blah", label= "GSE48581", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE48581', '_blank')"))
                 )
                 
               )
      ),
      footer = tagList(
        hr(),
        p("By Mukund, Liam, Charlotte, Amy, Khang. Group: KidneyC6", style="padding:0em 0em 0em 1em")
      )
      

    ) # belongs to navbarPage
)
