#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

# TODO: remove unneccessary library
library(GEOquery)
library(tidyverse)
library(dplyr)
library(CPOP)
library(matrixStats)
library(visNetwork)
library(furrr)
library(tictoc)
library(shinyjs)
library(shiny)
library(GEOquery) 
library(DT)

# set up multi processing, works on khang's mac mini but remove workers if on server
future::plan(multisession)

# set upload size of 150 mega bytes
options(shiny.maxRequestSize=150*1024^2)



# Load all the required rds files
GSE46474 <- readRDS("data/GSE46474.rds")
GSE36059 <-  readRDS("data/GSE36059.rds")
GSE48581 <-  readRDS("data/GSE48581.rds")
bridge <-   readRDS('data/bridge.rds')

calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

gene_names = function(gse) {
  fData(gse)$`Gene Symbol` = unlist(lapply(strsplit(fData(gse)$`Gene Symbol`, " /// ", 1), `[`, 1))
  
  idx = which(!duplicated(fData(gse)$`Gene Symbol`) & !is.na(fData(gse)$`Gene Symbol`))
  gse = gse[idx,] # get only rows that have a gene symbol that is unique, which enables the rownames
  rownames(gse) = fData(gse)$`Gene Symbol`
  
  return(gse)
}

set.seed(3888)
generateCPOPmodel <- function(user,userOutcomes, inhouse){
  return(cpop_model(user,
                    inhouse[[1]], # inhouse[1] is the expression matrix only
                    userOutcomes,
                    inhouse[[2]], #inhouse[2] is the inhouse Outcomes
                    w = NULL,
                    n_features = 30,
                    n_iter = 30,
                    alpha = 0,
                    family = "binomial",
                    s = "lambda.min",
                    cpop2_break = TRUE,
                    cpop2_type = "sign",
                    cpop2_mag = 1,
                    cpop1_method = "normal",
                    intercept = FALSE))
}

GSE46474 = gene_names(GSE46474)
GSE36059 = gene_names(GSE36059)
GSE48581 = gene_names(GSE48581)

p_GSE46474 = pData(GSE46474) 
p_GSE48581 = pData(GSE48581) 
p_GSE36059 = pData(GSE36059) 


# creating the outcome columns to put inside CPOP
p_GSE36059$diagnosis = ifelse(p_GSE36059$characteristics_ch1 == "diagnosis: non-rejecting", 0, 1)
p_GSE48581$diagnosis = ifelse(p_GSE48581$characteristics_ch1.1 == "diagnosis (tcmr, abmr, mixed, non-rejecting, nephrectomy): non-rejecting", 0, 1)
p_GSE46474$diagnosis = ifelse(p_GSE46474$characteristics_ch1.5 == "procedure status: post-transplant non-rejection (NR)", 0, 1)

# Main server logic
shinyServer(function(input, output) {

  
 ## Put everything onto the shiny app screen UI
  output$fileInput <- renderText({
    
    if(!is.null(input$userFile)){
      tic("userFileUpload")
      userFile <- data.frame(read_csv(input$userFile$datapath, name_repair = "minimal"))
      
      
      # changes probe Ids into gene names
      transformProbeIds = function(df){
        temp <- rownames(df)
        
        rowIndexesToKeep <- c()
        newRowNames <- c()
        for(i in 1:length(temp)){
          curIndex <- which(bridge$id == temp[i])
          if( length(curIndex) == 1 ){
            rowIndexesToKeep <-  c(rowIndexesToKeep, i)
            newRowNames <- c(newRowNames, bridge[curIndex, "geneName"])
          }
        }
        df <- df[rowIndexesToKeep,]
        rownames(df) <- newRowNames
        
        return(df)
      }
      
      
      
      # user inputs their file upload
      withProgress(message = 'Cleaning uploaded file', value = 0, {
      n <- 3  
        
      incProgress(1/n, detail = paste("Just started"))
      rownames(userFile) <-  tolower(userFile[[1]])
      userFile <-  userFile[,-1]
      diagnosisIndex <- which(rownames(userFile) == 'diagnosis')
      
      # separate for individual cleaning
      userExpression <- userFile[-diagnosisIndex,]
      userBinaryOutcomes <-  as.factor(unname(unlist(userFile[diagnosisIndex, ])))
      
      # transform probe ids to gene names, will make userExpression smaller
      userExpression <- transformProbeIds(userExpression)
      
      # I calculate the variance so I can get the userExpression to 2000 rows
      myVar = rowVars(as.matrix(userExpression), na.rm=TRUE)
      myVar = as.data.frame(myVar)
      
      userExpression = cbind(userExpression, variance = myVar)
      userExpression = slice_max(userExpression, order_by = myVar, n = 2000)
      userExpression = subset(userExpression, select = -c(myVar))
      
      # get the row names (probe ids) before transpose
      row_names_UE= rownames(userExpression)
      
      userExpression <- as.data.frame(t(userExpression))
      
      incProgress(1/n, detail = paste("Cleaning user expression"))
      # actually clean the userExpression matrix
      userExpression <- userExpression %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
      
      incProgress(1/n, detail = paste("Cleaning binary outcomes"))
      # actually clean the NAs in userBinary Outcomes
      f <- calc_mode(userBinaryOutcomes)
      majorityClass <- as.numeric(levels(f))[f]
      userBinaryOutcomes[is.na(userBinaryOutcomes)] <- majorityClass
      
      
      
      # row.names(userExpression) <- newR
      # feature1 <-generateCPOPmodel(userExpression,userBinaryOutcomes,z1,y1)
      })
      
      
      
      # TODO: save RDS object  of the exp_GSE expression dataframes
      ## keeping only the 2000 most variable genes in my data frame 
      exp_GSE36059 = (exprs(GSE36059))
      
      Variance = rowVars(as.matrix(exp_GSE36059)) #
      Variance = as.data.frame(Variance)
      
      exp_GSE36059 = as.data.frame(exp_GSE36059)
      
      exp_GSE36059 = cbind(exp_GSE36059, variance = Variance)
      exp_GSE36059 = slice_max(exp_GSE36059, order_by = Variance, n = 2000) # For each column, get the 2000 most variable genes, so you get 2000 rows
      exp_GSE36059 = subset(exp_GSE36059, select = -c(Variance))
      
      row_names_exp_GSE36059 = rownames(exp_GSE36059) # used for finding common probe ids
      
      
      ## keeping only the 2000 most variable genes in my data frame 
      exp_GSE46474 = (exprs(GSE46474))
      
      Variance = rowVars(as.matrix(exp_GSE46474))
      Variance = as.data.frame(Variance)
      exp_GSE46474 = as.data.frame(exp_GSE46474)
      
      
      exp_GSE46474 = cbind(exp_GSE46474, variance = Variance)
      exp_GSE46474 = slice_max(exp_GSE46474, order_by = Variance, n = 2000)
      exp_GSE46474 = subset(exp_GSE46474, select = -c(Variance))
      row_names_exp_GSE46474 = rownames(exp_GSE46474)
      
      
      
      ## keeping only the 2000 most variable genes in my data frame 
      exp_GSE48581 = (exprs(GSE48581))
      Variance = rowVars(as.matrix(exp_GSE48581))
      Variance = as.data.frame(Variance)
      exp_GSE48581 = as.data.frame(exp_GSE48581)
      exp_GSE48581 = cbind(exp_GSE48581, variance = Variance)
      exp_GSE48581 = slice_max(exp_GSE48581, order_by = Variance, n = 2000)
      exp_GSE48581 = subset(exp_GSE48581, select = -c(Variance))
      row_names_exp_GSE48581 = rownames(exp_GSE48581)
      
      ## now find the most common probe ids names between 3 datasets
      intersection = intersect(row_names_exp_GSE36059, row_names_exp_GSE46474)
      intersection = intersect(intersection, row_names_exp_GSE48581)
      intersection = intersect(intersection, row_names_UE)
      
      # create dataframes that only contains rows common in all dataframes
      exp_GSE36059 = as.data.frame(t(as.matrix(exp_GSE36059)))
      exp_GSE36059 = subset(exp_GSE36059, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      exp_GSE46474 = as.data.frame(t(as.matrix(exp_GSE46474)))
      exp_GSE46474 = subset(exp_GSE46474, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      exp_GSE48581 = as.data.frame(t(as.matrix(exp_GSE48581)))
      exp_GSE48581 = subset(exp_GSE48581, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      # no need to transpose as you already have done so above for user expression
      userExpression <-  subset(userExpression, select= c(intersection))
      
      # CPOP stuff continued
      
      
      z1 = exp_GSE36059 %>% as.matrix() # used for box plots and CPOP
      z2 = exp_GSE48581 %>% as.matrix()
      z3 = exp_GSE46474 %>% as.matrix()
      userExpression <- userExpression %>% as.matrix()
      
      y1 = as.factor(p_GSE36059$diagnosis)
      y2 = as.factor(p_GSE48581$diagnosis)
      y3 = as.factor(p_GSE46474$diagnosis)
      
    withProgress(message = 'Executing CPOP', value = 0, {
      incProgress(0.5, detail = paste("Starting CPOP in parallel"))
      cpopOutputs <- future_map(list(list(z1,y1),list(z2,y2), list(z3,y3)), generateCPOPmodel, user= userExpression, userOutcomes=userBinaryOutcomes )
    })
      
      # withProgress(message = 'Generating CPOP', value = 0, {
      #   # Number of times we'll go through the loop
      #   n <- 4
      #   incProgress(1/n, detail = paste("Starting CPOP 1/3"))
      #   feature1 <- generateCPOPmodel(userExpression,userBinaryOutcomes,z1,y1)
      #   incProgress(1/n, detail = paste("Finished CPOP 1/3"))
      #   feature2 <- generateCPOPmodel(userExpression,userBinaryOutcomes,z2,y2)
      #   incProgress(1/n, detail = paste("Finished CPOP 2/3"))
      #   feature3 <- generateCPOPmodel(userExpression,userBinaryOutcomes,z3,y3)
      #   incProgress(1/n, detail = paste("Finished CPOP 3/3"))
      #   
      #   
      # })
      
      
      
      f1 <-  cpopOutputs[[1]]$coef_tbl$coef_name[-1] 
      f2 <- cpopOutputs[[2]]$coef_tbl$coef_name[-1]
      f3 <- cpopOutputs[[3]]$coef_tbl$coef_name[-1]
      
      rI <- intersect(f1,f2)
      stableFeatures <- intersect(rI,f3)
      # generateCPOPmodel(z1,y1,z2,y2)
      
      # after finding the stable features, get rows relevant in each dataframe and create your own data frame.
      # we will do a column bind
      tb1 <-  cpopOutputs[[1]]$coef_tbl[cpopOutputs[[1]]$coef_tbl$coef_name %in% stableFeatures,]
      tb2 <-  cpopOutputs[[2]]$coef_tbl[cpopOutputs[[2]]$coef_tbl$coef_name %in% stableFeatures,]
      tb3 <-  cpopOutputs[[3]]$coef_tbl[cpopOutputs[[3]]$coef_tbl$coef_name %in% stableFeatures,]
      
      
      results <- tb1 %>% inner_join(tb2, by ="coef_name" ) %>% inner_join(tb3, by ="coef_name" )
      results$average <- rowMeans(results[,-1])
      # resultTable$average
      # -0.12109045	-0.07927375	AGL--ANKRD22
      # -0.069795029	-0.025225051 AGL--ANKRD22
      # -0.07525255	-0.023783883
      # length(stableFeatures)
      
      coef = results$coef_name
      
      # saveRDS(stableFeatures, file = "../../shinyapp/data/stableFeatures.rds")
      
      results = results %>% mutate(color = case_when( average > 0 ~ "blue", average < 0 ~ "red" ))
      names = data.frame(feature1 = rep("",length(coef)),
                         feature2 = rep("",length(coef)),
                         coef_size = abs(results$average))
      #names
      names$feature1 = sub("--.*", "", coef) #getting the first node from the coef-name vector of the results df
      names$feature2 = sub(".*--", "", coef) #getting the second node from the coef-name vector of the results df
      names$color = results$color
      # network = plot_lratio_network(coef, type = "visNetwork")
      #network
      names_uniq = unique(c(names$feature1, names$feature2))
      # names_uniq
      numbers = names
      #numbers
      for (a in 1:nrow(numbers)) {
        for (i in 1:length(names_uniq)) {
          if (numbers$feature2[a] == names_uniq[i]) {
            numbers$feature2[a] = i
          }
        }
      }
      for (a in 1:nrow(numbers)) {
        for (i in 1:length(names_uniq)) {
          if (numbers$feature1[a] == names_uniq[i]) {
            numbers$feature1[a] = i
          }
        }
      }
      #numbers
      clr = c()
      for (a in 1:length(names_uniq)) {
        for (i in 1:nrow(names)) {
          if (names_uniq[a] == names[i,1] | names_uniq[a] == names[i,2]) {
            clr[a] = names$color[i]
          }
        }
      }
      #clr
      
      cleanNames <- sub("--.*","",names_uniq)
      
      edges = data.frame(from = numbers$feature1, to = numbers$feature2, value = names$coef_size)
      nodes = data.frame(id = c(1:length(names_uniq)), 
                         label = names_uniq, 
                         color = clr,
                         title = paste0('<a href = "https://www.genecards.org/cgi-bin/carddisp.pl?gene=',cleanNames,'">',cleanNames,'</a>'))
      #nodes
      
      output$userFileVisNetwork <- renderVisNetwork({
        
        visNetwork(nodes, edges, height = "500px", width = "100%")
       
      })
      
      output$pairwiseGenes <- DT::renderDataTable({
          data.frame(`Pairwise genes`=stableFeatures)
        })
      
      toc(log=TRUE)
      
      return("Success!")
    }
    
  })
  
  observeEvent(input$showHide, {
      shinyjs::toggle("v1box")
      shinyjs::toggle("v2box")
      shinyjs::toggle("v3box")
  })
  
  output$v1 <- renderVisNetwork({
    readRDS("data/vis1.rds")
  })
  output$v2 <- renderVisNetwork({
    readRDS("data/vis1.rds")
  })
  output$v3 <- renderVisNetwork({
    readRDS("data/vis1.rds")
  })
  
  
  # Rubbish below for the other tab pages. Can delete or refactor.
  output$carExample <- renderPrint({
    head(mtcars,7)
  })
  
  
  # data <- NULL
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste("stablePairwiseGenes-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write_csv(data.frame(`Pairwise genes`=stableFeatures), file)
      },
      contentType = "text/csv"
  )
  
  output$boxplot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)

})
