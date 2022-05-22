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
#library(shinythemes)
library(shinydashboard)
library(visNetwork)
library(DT)

# options(shiny.autoreload = TRUE)


# Define UI for application 
ui <- dashboardPage(
  
  dashboardHeader(title = "CPOP tool for kidney graft survival", titleWidth = 350, tags$li(class = "dropdown", tags$style(".main-header {max-height: 400px}"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tool", tabName = "0", icon = icon("laptop-code")),
      menuItem("Design", tabName = "1", icon = icon("bezier-curve")),
      menuItem("Data sources", tabName = "2", icon = icon("cube"))
    ),
    tags$footer(
      tags$p("By Mukund, Liam, Charlotte, Amy, Khang. Group: KidneyC6"), style="position:absolute;
bottom:0;
width:100%;
height:60px; /* Height of the footer */
color: white;
padding: 10px;
z-index: 1000;"
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "0",
        fluidRow(
          tabBox(
            title = NULL, width = 12, id = "tabset0",
            tabPanel("Overview",
                     box(width=NULL, title = "Introduction",
                         fluidRow(column(12, tags$p("We have developed a cross-platform omics prediction ", tags$a(href="https://kevinwang09.github.io/CPOP/", "(CPOP)"), "tool that identifies stable pairwise genes as biomarkers to predict kidney allograft outcome."), tags$div(tags$ul(tags$li(tags$b("Feature analysis:"), "allows the user to input their own gene expression data and outputs a list of stable genes common across all four datasets (user dataset and the three in-house datasets."), tags$li(tags$b("Dataset integration:"),  "combines user uploaded in-house data with existing public datasets. This is to allow the researcher to have more samples to work with during their analysis and model building by leveraging historic research. "), tags$li(tags$b("Fairness:"), "provides a fairness evaluation of our model which uses biological sex. The model performances are evaluated and displayed for transparency."))
                         )))),
                     box(width=NULL, title = "File Input",
                         fluidRow(column(12, fileInput("userFile", label = h5("Upload csv file"), accept = ".csv"), verbatimTextOutput("fileInput"))),
                         fluidRow(column(12, tags$p("Structure of user-input data", style="font-weight:bold;"), tags$p("In order to work with our Shiny Dashboard, the uploaded dataset must comply with the following requirements:"), tags$div(tags$ol(tags$li("Data must be in CSV format"), tags$li("Rows must be Affymetrix Probe-IDs"), tags$li("Final row must be a binary outcome row labeled as ‘outcome'"), tags$li("Stable should be denoted with 0"), tags$li("Rejection should be denoted with 1"), tags$li("This row cannot be empty, however if some NAs are present they will be replaced by majority class"), tags$li("Columns must be participants"), tags$li("Data must be strictly numeric."), tags$li("Any NA values will be imputed by taking the mean of all other expression values for that given probe-id."), tags$li("Download an example dataset ", tags$a(href="dirtyuser.csv", target='blank', 'HERE', download = 'sampleData.csv'), " to see an example of a compatible data format.")
))))),
                     box(width=NULL, title = "Methods explained",
                         fluidRow(column(12, tags$div(tags$ul(tags$li(tags$b("Dataset combination: "),  "Before combining the uploaded in-house data and public data, we apply a normalization procedure. This is done by first taking the log of all expression values, and then taking the pairwise differences of the top overlapping genes between all datasets. The transformed expression boxplot visualizes how the normalization process alleviates batch-differences.", tags$p("Another issue we aim to solve is the lack of clinical variables present in historic expression data. By extracting key sex-specific expression biomarkers, we are able to predict biological sex of each observation. This is added to the final output dataframe which researchers can use in their own analysis and model building. We have included a filter so you can only download combined data for a specific biological sex if desired.")), tags$li(tags$b("Biological sex ", tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3904696/", "model: ")), "While part of our motivation to extract biological sex data is to add an element to predictive model building, clinical researchers are becoming increasingly concerned in ensuring that models being built are fair when accounting for sex differences.", tags$p("We build the model by selecting three specific gene expression biomarkers. We used the pairwise differences of the logged expression levels of the following genes in the GSE46474 expression data, and then applied a KNN model to determine biological sex:" ), tags$ul(tags$li("XIST -> higher relative expression levels for females"), tags$li("EIF1AY -> higher relative expression levels for males"), tags$li("RPS4Y1 -> neutral gene that can be used as a baseline"))), br(), tags$li(tags$b("Features selection: "), tags$p("How features are selected:"), tags$ul(tags$li("To normalize the data and reduce batch-effects, we take pairwise differences of genes after the expression data has been logged."), tags$li("Features are selected using a CPOP model (with alpha value of 0), 50 features and 30 iterations. We run a CPOP model taking the input of the in-house data, and each of the public datasets, one at a time."), tags$li("We find the overlapping features from all the models produced, and display this as an interactive network. Clicking on the nodes takes the researcher to a site that gives information on that specific gene. This will help the researcher reconcile the statistical evidence produced by our CPOP model with known biological pathways related to then genes shown.")), br(), tags$p("How to read the VisNetwork:"), tags$ul(tags$li("The VisNetwork displays stable features selected across CPOP models."), tags$li("The features are not the nodes (genes) themselves, rather the pairwise difference of genes. In other words, if two genes are connected by an edge, this means that the ratio between these two genes is proposed to be significant in determining if someone is going to have a stable kidney transplant, or reject the organ. The thickness of the edge determines the size of the coefficient of the feature in the final model."))), tags$li(tags$b("Output dataset (after combination): "), tags$p("The final output dataset will be in the following format and can be downloaded as a CSV."), tags$ul(tags$li("Rows will be the observations from all of the datasets that have been combined."), tags$li("Columns will be pairwise differences of genes (not probe_ids) with the highest variance"), tags$li("An additional column for biological sex will be added for all observations based on our predictive model. This is called ‘biological_sex’."), tags$li("An additional column called ‘source’ which provides the original dataset of which an observation came from."), tags$li("A binary outcome column, where 0 is stable, and 1 is reject will also be added for each observation."))), tags$li(tags$b("Fairness evaluation: "), tags$p("One of the aims of our dashboard is to give clinical researchers a way to combine their in-house data with public renal biopsy datasets. In doing so, this provides researchers with more data they can use to train their own models and analyze features."), tags$p("After combining datasets we have computed some fairness statistics to showcase that an example model built on the combined data will perform fairly on all of the combined datasets, as well as when considering biological sex. The tables below summarize fairness metrics when using a simple KNN model (k=7) on the combined dataset. This is an example of the analysis researchers may want to do when model building on the combined data."))
                                         )))))
            ),
            tabPanel("Feature analysis",
                     box(width=NULL, title = "Features", 
                         fluidRow(
                           column(4,h4("Pairwise Genes"), 
                                  DT::dataTableOutput("pairwiseGenes")),
                           column(8,h4("VisNetwork"),
                                  visNetworkOutput("userFileVisNetwork"))
                         ),
                         fluidRow(
                           column(12, br()),
                           column(6, downloadButton("downloadData", "Download pairwise genes (CSV file)"))
                         )),
                     box(width=NULL, title = "Comparison to in-house datasets", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(7, checkboxGroupInput("checkGroup", label = "Dataset to compare:", choices = list("GSE36059", "GSE48581", "GSE21374"), selected = "GSE36059"))),
                         fluidRow(
                           column(12, uiOutput("plot_list"))
                     ))
            ),
            tabPanel("Dataset integration",
                     box(width=NULL, title = "Dataset combination",
                         fluidRow(
                           column(12, actionButton("datasetComb", "View description"), hidden(div(id="combDesc", htmlOutput("text")))),
                           column(12, br()),
                           column(12, tags$h4("Boxplot - highlights normalisation")),
                           column(12,
                                  plotOutput("boxplot", click="plot_click"))
                         ),
                         fluidRow(
                           column(12, br()),
                           column(12, tags$h4("Biological sex model")),
                           column(12, br()),
                           column(7, selectInput("select", label = tags$p("Choose biological sex to include in combined dataset",style="font-weight:bold;"), choices = list("Both" = 1, "Male" = 2, "Female" = 3), selected = 1)),
                           column(12, br()),
                           column(6, downloadButton("dc", "Download combined data CSV file"))
                         )
                     )
            ),
            tabPanel("Fairness",
                     box(width=NULL, title = "Model Prediction Accuracy",
                         fluidRow(tags$style(".box {box-shadow: 0px 1px 1px rgb(0 0 0 / 10%);}"),
                           column(12, box(width=NULL, title = tags$b("Fairness evaluation: "), collapsible = TRUE, collapsed = TRUE, tags$p("One of the aims of our dashboard is to give clinical researchers a way to combine their in-house data with public renal biopsy datasets. In doing so, this provides researchers with more data they can use to train their own models and analyze features."), tags$p("After combining datasets we have computed some fairness statistics to showcase that an example model built on the combined data will perform fairly on all of the combined datasets, as well as when considering biological sex. The tables below summarize fairness metrics when using a simple KNN model (k=7) on the combined dataset. This is an example of the analysis researchers may want to do when model building on the combined data.")), useShinyjs())
                         ),
                         fluidRow(
                           column(6,h4("Performance Between Datasets"),  
                                  DT::dataTableOutput("source_stats")),
                           column(6,h4("Performance Between Biological Sexes"), DT::dataTableOutput("sex_stats"))
                         )))))
      ),
      tabItem(
        tabName = "1", 
        fluidRow(box(width=12, title="Model Design Diagram", img(src="figure1.png", width = "80%")))
      ),
      tabItem(
        tabName = "2", 
        fluidRow(
          box(width=12, title="Datasets explained", 
              fluidRow(column(12, tags$h4("GSE36059 - Dataset", style="font-weight:bold;")), column(8, tags$p("This dataset is being used for the CPOP model and the data combination section of our Shiny app. This dataset uses the gene platform GPL570. It contains biopsy samples from 411 patients, 35 of which underwent T cell-mediated rejection and 36 of which underwent Antibody-mediated rejection. The dataset is from a 2014 study which explored the disappearance of T Cell-mediated rejection despite continued antibody-mediated rejection in late kidney transplant recipients. This dataset utilises a reasonably common gene platform, making it more likely that it will be able to be easily combined with other researcher’s datasets. It also has a large sample size, which will increase the training capacity, and therefore the accuracy of the CPOP model if this dataset is selected.")), column(2, offset=1, actionButton(inputId="blah", label= "GSE36059", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE36059', '_blank')")), column(12, hr())
              ),
              fluidRow(
                column(12, tags$h4("GSE48581 - Dataset", style="font-weight:bold;")),
                column(8, tags$p("This dataset is being used for the CPOP model and the data combination section of our Shiny app. This dataset also uses the gene platform GPL570. It contains biopsy samples from 306 patients, 32 of which underwent T cell-mediated rejection and 40 of which underwent Antibody-mediated rejection. The dataset is from a 2013 study which looked at the potential impact of microarray diagnosis of T cell-mediated rejection in kidney transplants. This dataset utilises a common gene platform, making it more likely that researchers will be able to easily combine their own datasets with this dataset. The dataset also has a large sample size, which will increase the accuracy of the CPOP model by increasing its training capacity.")),
                column(2, offset=1,
                       actionButton(inputId="blah", label= "GSE48581", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE48581', '_blank')"))
              ),
              fluidRow(column(12, tags$h4("GSE21374 - Dataset", style="font-weight:bold;")), column(8, tags$p("This dataset is being used for the CPOP model and the data combination section of our Shiny app. This dataset uses the gene platform GPL570. It contains biopsy samples from 105 patients, 30 of which had their kidneys rejected. The dataset is from a 2010 study which created a molecular classifier for predicting future graft loss in late kidney transplant biopsies. The dataset utilises a relatively common gene platform, meaning that it is more likely that it will be able to be easily combined with other researcher’s datasets. The dataset also has a reasonably large sample size, which will mean that the CPOP model will be relatively accurate if this dataset is chosen.")), column(2, offset=1, actionButton(inputId="blah", label= "GSE21374", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE21374', '_blank')")), column(12, hr())
              ),
              fluidRow(column(12, tags$h4("GSE46474 - Dataset", style="font-weight:bold;")), column(8, tags$p("This dataset is only being used for the data combination section of our Shiny app. Similarly, this dataset uses the gene platform GPL570. It contains peripheral blood samples from 40 patients, and 20 of these patients had their kidneys rejected. The dataset is from a 2014 study that investigated novel methods for the integration of genomics and proteomics data in kidney transplant rejection research. This dataset utilises a common gene platform, meaning that it is more likely that it will be able to be combined with other datasets. It also contains peripheral blood samples, unlike the other two datasets, overall providing more options, especially to researchers who have also taken this type of sample.")), column(2, offset=1, actionButton(inputId="blah", label= "GSE46474", onclick ="window.open('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE46474', '_blank')")), column(12, hr())
              ))
        )
      )
    )
  )
)