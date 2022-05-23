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
library(class)
library(viridis)

# set up multi processing, works on khang's mac mini but remove workers if on server
future::plan(multisession)

# set upload size of 150 mega bytes
options(shiny.maxRequestSize=150*1024^2)



# Load all the required rds files
GSE46474 <- readRDS("data/GSE46474.rds")
GSE36059 <-  readRDS("data/GSE36059.rds")
GSE48581 <-  readRDS("data/GSE48581.rds")
GSE21374 = readRDS("data/GSE21374.rds")
GSE129166 = readRDS("data/GSE129166.rds")
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


generateCPOPmodel <- function(user,userOutcomes, inhouse){
  set.seed(3888)
  return(cpop_model(user,
                    inhouse[[1]], # inhouse[1] is the expression matrix only
                    userOutcomes,
                    inhouse[[2]], #inhouse[2] is the inhouse Outcomes
                    w = NULL,
                    n_features = 30,
                    n_iter = 30,
                    alpha = 1,
                    family = "binomial",
                    s = "lambda.min",
                    cpop2_break = TRUE,
                    cpop2_type = "sign",
                    cpop2_mag = 1,
                    cpop1_method = "normal",
                    intercept = FALSE))
}

makeVisnetwork <- function(cpopDF, colorCode){
  cpopDF$average <- rowMeans(cpopDF[,-1])
  coef <- cpopDF$coef_name
  names = data.frame(feature1 = rep("",length(coef)),
                     feature2 = rep("",length(coef)),
                     coef_size = abs(cpopDF$average))
  
  names$feature1 = sub("--.*", "", coef) #getting the first node from the coef-name vector of the cpopDF df
  names$feature2 = sub(".*--", "", coef) #getting the second node from the coef-name vector of the cpopDF df
  
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
                     color = colorCode,
                     title = paste0('<a target="_blank" href = "https://www.genecards.org/cgi-bin/carddisp.pl?gene=',cleanNames,'">',cleanNames,'</a>'))
  #nodes
  
  return(visNetwork(nodes, edges, height = "500px", width = "100%"))
}


pairwise = function(exp_GSE, transform_type) {
  z = exp_GSE
  if (transform_type == "Arc") {
    z = z / max(z)
    z = asin(sqrt(z))
    z = pairwise_col_diff(z) %>% as.matrix()
  }
  else if (transform_type == "Log") {
    # z = log(z +1)
    # removing weird AGL name added.
    z <- pairwise_col_diff(log(z +1))
    colnames(z) <- sub(".*\\.","",colnames(z))
    z = z %>% as.matrix()
  }
  else if (transform_type == "Pair"){
    z = z %>% as.matrix()
    z_pairwise = pairwise_col_diff(z) %>% as.matrix()
    
  }
  z = data.frame(z)
  
  return(z)
}


# returns gender vector
calculate_gender = function(dataframe) {
  colnames(dataframe) = tolower(colnames(dataframe))
  # outcome = NULL
  #   if ("outcome" %in% colnames(dataframe)) {
  #     outcome = dataframe$outcome
  #     dataframe = select(dataframe, -one_of("outcome"))
  # }
  if ("xist" %in% colnames(dataframe) && "eif1ay" %in% colnames(dataframe) && "rps4y1" %in% colnames(dataframe)) {
    # known_genes =  c()
    
    outcome_df = dataframe %>% select("xist", "eif1ay","rps4y1")
    outcome_df = pairwise(outcome_df, transform_type = "Log") %>% as.matrix()
    outcome_df = data.frame(outcome_df)
    
    # Can replace this with an already preprocessed dataframe
    
    p_GSE46474 = pData(GSE46474)
    p_GSE46474$outcome = ifelse(p_GSE46474$characteristics_ch1.1 == "Sex: M", 0, 1) # Male is 0 and female is 1
    exprs_GSE46474 = data.frame(t(exprs(GSE46474)))
    colnames(exprs_GSE46474) = tolower(colnames(exprs_GSE46474))
    exprs_GSE46474 = exprs_GSE46474 %>% select("xist", "eif1ay","rps4y1")
    exprs_GSE46474 = pairwise(exprs_GSE46474, transform_type="Log") %>% as.matrix()
    exprs_GSE46474 = data.frame(exprs_GSE46474)
    
    # print(dim(exprs_GSE46474))
    # print(dim(outcome_df))
    
    model = knn(exprs_GSE46474, outcome_df, p_GSE46474$outcome, k = 3)
    
    # dataframe$gender = model
    # dataframe$outcome = outcome
    
    return(model)
    
  }
  return(NULL)
}

GSE46474 = gene_names(GSE46474)
GSE21374 = gene_names(GSE21374)
GSE36059 = gene_names(GSE36059)
GSE48581 = gene_names(GSE48581)

p_GSE21374 = pData(GSE21374) 
p_GSE48581 = pData(GSE48581) 
p_GSE36059 = pData(GSE36059) 


# creating the outcome columns to put inside CPOP
p_GSE36059$diagnosis = ifelse(p_GSE36059$characteristics_ch1 == "diagnosis: non-rejecting", 0, 1)
p_GSE48581$diagnosis = ifelse(p_GSE48581$characteristics_ch1.1 == "diagnosis (tcmr, abmr, mixed, non-rejecting, nephrectomy): non-rejecting", 0, 1)
p_GSE21374$diagnosis = ifelse(p_GSE21374$characteristics_ch1.3 == "rejection/non rejection: nonrej", 0, 1)

# Main server logic
shinyServer(function(input, output) {
  
  combinedDataset <- NULL
  stableFeatures <- NULL
  
  ## Put everything onto the shiny app screen UI
  output$fileInput <- renderText({
    
    if(!is.null(input$userFile)){
      
      file = input$userFile
      
      ext = tools::file_ext(file$datapath)
      print(ext)
      shiny::validate(need(ext[1] == "csv", "Please upload a csv file"))
      
      
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
        
        shiny::validate(need(try(('outcome' %in% rownames(userFile)) == TRUE), "An outcome column has not been provided"))
        
        userFile <-  userFile[,-1]
        diagnosisIndex <- which(rownames(userFile) == 'outcome')
        
        # separate for individual cleaning
        userExpression <- userFile[-diagnosisIndex,]
        userBinaryOutcomes <-  as.factor(unname(unlist(userFile[diagnosisIndex, ])))
        
        # transform probe ids to gene names, will make userExpression smaller
        userExpression <- transformProbeIds(userExpression)
        
        # I calculate the variance so I can get the userExpression to 2000 rows
        myVar = rowVars(as.matrix(userExpression), na.rm=TRUE)
        myVar = as.data.frame(myVar)
        
        userExpression = cbind(userExpression, variance = myVar)
        userExpression = slice_max(userExpression, order_by = myVar, n = 100)
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
        
        # TODO: Do predictions for outcomes if no outcome column provided
        
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
      exp_GSE36059 = slice_max(exp_GSE36059, order_by = Variance, n = 100) # For each column, get the 300 most variable genes, so you get 2000 rows
      exp_GSE36059 = subset(exp_GSE36059, select = -c(Variance))
      
      row_names_exp_GSE36059 = rownames(exp_GSE36059) # used for finding common probe ids
      
      
      ## keeping only the 2000 most variable genes in my data frame 
      exp_GSE21374 = (exprs(GSE21374))
      
      Variance = rowVars(as.matrix(exp_GSE21374))
      Variance = as.data.frame(Variance)
      exp_GSE21374 = as.data.frame(exp_GSE21374)
      
      
      exp_GSE21374 = cbind(exp_GSE21374, variance = Variance)
      exp_GSE21374 = slice_max(exp_GSE21374, order_by = Variance, n = 100)
      exp_GSE21374 = subset(exp_GSE21374, select = -c(Variance))
      row_names_exp_GSE21374 = rownames(exp_GSE21374)
      
      
      
      ## keeping only the 2000 most variable genes in my data frame 
      exp_GSE48581 = (exprs(GSE48581))
      Variance = rowVars(as.matrix(exp_GSE48581))
      Variance = as.data.frame(Variance)
      exp_GSE48581 = as.data.frame(exp_GSE48581)
      exp_GSE48581 = cbind(exp_GSE48581, variance = Variance)
      exp_GSE48581 = slice_max(exp_GSE48581, order_by = Variance, n = 100)
      exp_GSE48581 = subset(exp_GSE48581, select = -c(Variance))
      row_names_exp_GSE48581 = rownames(exp_GSE48581)
      
      ## now find the most common probe ids names between 3 datasets
      intersection = intersect(row_names_exp_GSE36059, row_names_exp_GSE21374)
      intersection = intersect(intersection, row_names_exp_GSE48581)
      intersection = intersect(intersection, row_names_UE)
      
      # create dataframes that only contains rows common in all dataframes
      exp_GSE36059 = as.data.frame(t(as.matrix(exp_GSE36059)))
      exp_GSE36059 = subset(exp_GSE36059, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      exp_GSE21374 = as.data.frame(t(as.matrix(exp_GSE21374)))
      exp_GSE21374 = subset(exp_GSE21374, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      exp_GSE48581 = as.data.frame(t(as.matrix(exp_GSE48581)))
      exp_GSE48581 = subset(exp_GSE48581, select = c(intersection)) # select rows that contain the probe ids in intersection
      
      # no need to transpose as you already have done so above for user expression
      userExpression <-  subset(userExpression, select= c(intersection))
      
      # CPOP stuff continued
      
      
      z1 = exp_GSE36059 %>% as.matrix() # used for box plots and CPOP
      z2 = exp_GSE48581 %>% as.matrix()
      z3 = exp_GSE21374 %>% as.matrix()
      userExpression <- userExpression %>% as.matrix()
      
      y1 = as.factor(p_GSE36059$diagnosis)
      y2 = as.factor(p_GSE48581$diagnosis)
      y3 = as.factor(p_GSE21374$diagnosis)
      
      withProgress(message = 'Executing CPOP', value = 0, {
        incProgress(0.5, detail = paste("Starting CPOP in parallel"))
        cpopOutputs <- future_map(list(list(z1,y1),list(z2,y2), list(z3,y3)), generateCPOPmodel, user= userExpression, userOutcomes=userBinaryOutcomes)
      })
      
      cpop_coef = merge(merge(cpopOutputs[[1]]$coef_tbl, cpopOutputs[[2]]$coef_tbl, all = TRUE), cpopOutputs[[3]]$coef_tbl, all = TRUE)
      
      cpop_coef$avg = (abs(cpop_coef$coef1) + abs(cpop_coef$coef2)) / 2
      cpop_coef = head(cpop_coef %>% arrange(desc(avg)), 20)
      
      top_features = cpop_coef$coef_name[-1]
      
      # Gets intersect. Legacy feature in case something breaks
      #f1 <-  cpopOutputs[[1]]$coef_tbl$coef_name[-1] 
      #f2 <- cpopOutputs[[2]]$coef_tbl$coef_name[-1]
      #f3 <- cpopOutputs[[3]]$coef_tbl$coef_name[-1]
      #rI <- intersect(f1,f2)
      #stableFeatures <- intersect(rI,f3)
      # generateCPOPmodel(z1,y1,z2,y2)
      
      # after finding the stable features, get rows relevant in each dataframe and create your own data frame.
      # we will do a column bind
      #tb1 <-  cpopOutputs[[1]]$coef_tbl[cpopOutputs[[1]]$coef_tbl$coef_name %in% top_features,]
      #tb2 <-  cpopOutputs[[2]]$coef_tbl[cpopOutputs[[2]]$coef_tbl$coef_name %in% top_features,]
      #tb3 <-  cpopOutputs[[3]]$coef_tbl[cpopOutputs[[3]]$coef_tbl$coef_name %in% top_features,]
      
      
      results <- cpop_coef %>% select(coef_name, coef1, coef2)
      results = subset(results, !duplicated(subset(results, select = c(coef_name))))
      
      cpopOutputs[[1]]$coef_tbl = cpopOutputs[[1]]$coef_tbl %>% filter(coef_name != "(Intercept)")
      cpopOutputs[[2]]$coef_tbl = cpopOutputs[[2]]$coef_tbl %>% filter(coef_name != "(Intercept)")
      cpopOutputs[[3]]$coef_tbl = cpopOutputs[[3]]$coef_tbl %>% filter(coef_name != "(Intercept)")
      
      
      output$userFileVisNetwork <- renderVisNetwork({
        makeVisnetwork(results, "lightblue") 
      })
      
      output[["plota"]] <- renderVisNetwork({makeVisnetwork(cpopOutputs[[1]]$coef_tbl, "purple")})
      
      output[["plotb"]] <- renderVisNetwork({makeVisnetwork(cpopOutputs[[2]]$coef_tbl, "grey")})
      
      output[["plotc"]] <- renderVisNetwork({makeVisnetwork(cpopOutputs[[3]]$coef_tbl, "brown")})
      
      
      output[["plot_list"]] <- renderUI({
        plotList <- list()
        
        if("GSE36059" %in% input$checkGroup){
          plotList <- c(plotList, list(visNetworkOutput("plota")))
        }
        if("GSE48581" %in% input$checkGroup){
          plotList <- c(plotList, list(visNetworkOutput("plotb")))
        }
        if("GSE21374" %in% input$checkGroup){
          plotList <- c(plotList, list(visNetworkOutput("plotc")))
        }
        
        do.call(splitLayout, plotList)
      })
      
      
      
      
    
      output$pairwiseGenes <- DT::renderDataTable({
        data.frame(`Top Pairwise Genes` = cpop_coef$coef_name, `Average Coefficient Weight` = round(cpop_coef$avg, 3))
      })
      
      withProgress(message = 'Finished CPOP. Now combining data', value = 0, { 
        # Add gender and source column to each of the 4 data frames
        ue <- pairwise(userExpression, "Log")
        temp_GSE36059 <- pairwise(exp_GSE36059, "Log")
        temp_GSE48581 <- pairwise(exp_GSE48581, "Log")
        temp_GSE21374 <- pairwise(exp_GSE21374, "Log")
        
        as.data.frame(t(userExpression))
        
        boxplotInput <- bind_rows(temp_GSE36059,temp_GSE48581, temp_GSE21374, ue)
        #boxplotInput <- bind_rows(exp_GSE36059,exp_GSE48581, exp_GSE21374, as.data.frame(userExpression))
        
        ue$outcome <- userBinaryOutcomes
        ue$biological_sex <- calculate_gender(as.data.frame(userExpression))
        ue$source <- c(rep("userUploadedData", length(rownames(userExpression))))
        print(ue$biological_sex)
        
        
        temp_GSE36059$outcome <- y1
        temp_GSE36059$biological_sex <- calculate_gender(data.frame(t(exprs(GSE36059))))
        temp_GSE36059$source <- c(rep("GSE36059", length(rownames(temp_GSE36059))))
        
        
        
        temp_GSE48581$outcome <- y2
        temp_GSE48581$biological_sex <- calculate_gender(data.frame(t(exprs(GSE48581))))
        temp_GSE48581$source <- c(rep("GSE48581", length(rownames(temp_GSE48581))))
        
        
        temp_GSE21374$outcome <- y3
        temp_GSE21374$biological_sex <- calculate_gender(data.frame(t(exprs(GSE21374))))
        temp_GSE21374$source <- c(rep("GSE21374", length(rownames(temp_GSE21374))))
        
        incProgress(0.5, detail = paste("Combining all the data"))
        
        combinedDataset <-  bind_rows(temp_GSE36059,temp_GSE48581, temp_GSE21374,ue )
        colnames(combinedDataset) <- sub("\\.\\.","-", colnames(combinedDataset))
        
        compareCombinedData = cbind(boxplot_tbl(boxplotInput, index =1), source= combinedDataset$source)
      })
      
      
      
      
      withProgress(message = 'Data Combined. Now Calculating Fairness', value = 0, {
        cvK = 5
        X1 = combinedDataset %>% select(-c(outcome, biological_sex, source))
        y1 = combinedDataset$outcome
        
        cvSets = cvTools::cvFolds(nrow(X1), cvK)
        incProgress(0.25, detail = paste("Generating Predictions"))
        predicted_outcome = c()
        predicted_index = c()
        for (i in 1:cvK) {
          test_id = cvSets$subsets[cvSets$which == i]
          X_test = X1[test_id, ]
          X_train = X1[-test_id, ]
          y_test = y1[test_id]
          y_train = y1[-test_id]
          
          fit <- knn(X_train, X_test, y_train, k = 10)
          predicted_outcome = append(predicted_outcome, fit)
          predicted_index = append(predicted_index, test_id)
        }
        incProgress(0.5, detail = paste("Generating Statistics"))
        pred_df = data.frame(predicted_index, predicted_outcome) %>% arrange(predicted_index) #Puts predictions in order again
        combinedDataset$prediction = pred_df$predicted_outcome
        combinedDataset$correct = ifelse(combinedDataset$prediction == combinedDataset$outcome, 1, 0)
        
        gse21374 = combinedDataset %>% filter(source == "GSE21374")
        gse36059 = combinedDataset %>% filter(source == "GSE36059")
        gse48581 = combinedDataset %>% filter(source == "GSE48581")
        user = combinedDataset %>% filter(source == "userUploadedData")
        
        data_acc = c(count(gse21374$correct == 1)/nrow(gse21374), count(gse36059$correct == 1)/nrow(gse36059), count(gse48581$correct == 1)/nrow(gse48581), count(user$correct == 1)/nrow(user))
        data_pos = c(count(gse21374$prediction == 1)/nrow(gse21374), count(gse36059$prediction == 1)/nrow(gse36059), count(gse48581$prediction == 1)/nrow(gse48581), count(user$prediction == 1)/nrow(user))
        data_obsv = c(nrow(gse21374), nrow(gse36059), nrow(gse48581), nrow(user))
        
        
        male = combinedDataset %>% filter(biological_sex == 0)
        female = combinedDataset %>% filter(biological_sex == 1)
        
        gen_acc = c(count(male$correct == 1)/nrow(male), count(female$correct == 1)/nrow(female))
        gen_pos = c(count(male$prediction == 1)/nrow(male), count(female$prediction == 1)/nrow(female))
        gen_obsv = c(nrow(male), nrow(female))
        
        
      })
      output$source_stats <- DT::renderDataTable({
        data.frame(`Dataset` = c("GSE21374", "GSE36059", "GSE48581", "User Input"), Observations = data_obsv, Accuracy = round(data_acc, 2), `Positive Prediction Rate` = round(data_pos, 2))
      })
      output$sex_stats <- DT::renderDataTable({
        data.frame(`Biological Sex` = c("Male", "Female"), Observations = gen_obsv, Accuracy = round(gen_acc, 2), `Positive Prediction Rate` = round(gen_pos, 2))
      })
      
      
      output$boxplot <- renderPlot({
        ggplot(data = compareCombinedData, aes(x = object, y = means)) +
          geom_point(aes(color = source), size = 0.1) +
          geom_errorbar(aes(ymin = q1,
                            ymax = q3,
                            color = source), size = 0.15,  alpha = 0.55) +
          
          theme(axis.ticks = element_blank()) +
          theme(axis.text.x = element_blank()) +
          xlab("Samples") +  ylab("Expression Level (Log)")+
          theme(axis.title.y=element_blank()) +
          labs(title = "Log Transformation + Pairwise Differences") +
          theme(plot.title = element_text(size=10)) + scale_color_viridis(discrete = TRUE, option = "inferno", alpha = 1, begin = 0, end = 0.8)
      })
      
      
      output$dc <- downloadHandler(filename = function() {
        if(input$select == 1){
          paste("maleAndFemaleCombinedDataset-", Sys.Date(), ".csv", sep="")
        }
        else if (input$select == 2){
          paste("maleDataset-", Sys.Date(), ".csv", sep="")
        }
        else if (input$select == 3){
          paste("femaleDataset-", Sys.Date(), ".csv", sep="")
        }
        
      },
      content = function(file) {
        if(input$select == 1){
          write_csv(combinedDataset, file)
        }
        else if (input$select == 2){
          write_csv(combinedDataset[which(combinedDataset$biological_sex == 0),], file)
        }
        else if (input$select == 3){
          write_csv(combinedDataset[which(combinedDataset$biological_sex == 1),], file)
        }
      },
      contentType = "text/csv"
      )
      
      
      toc(log=TRUE)
      
      return("Success!")
    }
    
  })
  
  observeEvent(input$showHide, {
    shinyjs::toggle("v1box")
    shinyjs::toggle("v2box")
    shinyjs::toggle("v3box")
  })
  
  
  
  # Rubbish below for the other tab pages. Can delete or refactor.
  output$carExample <- renderPrint({
    head(mtcars,7)
  })
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("stablePairwiseGenes-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(data.frame(`Pairwise genes`=stableFeatures), file)
    },
    contentType = "text/csv"
  )
  
  
  
})





