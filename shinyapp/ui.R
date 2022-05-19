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
                          ),
                          fluidRow(
                            column(12, br()),
                            column(2, p("Download your pairwise genes", style="font-weight:bold;")),
                            column(6, downloadLink("downloadData", "Download CSV file"))
                          )
                        ),
                        
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
