## Market Selection for Media Testing
## Verison 1.0
# Shiny app that brings a data file in, performs PCA and then performs K means custering based on the set of 
# variables that explain most of the data, and allows the user plot/download the clustered data. There is an 
# option to weight the variables based on how important they are. This requires the clients business data and 
# the users business logic to pick the best business variables in order to get the best market selecions for media testing.
## Needed Lib's
library(shiny)
library(ggplot2)
library(DT)
library(cluster)
library(plotly)
library(scatterplot3d)
library(ggfortify)
library(reshape)
library(plyr)
library(dplyr)

## Initiate the severer
shinyServer(
  function(input, output, session) {
    
    ## Read in the data
    dataset <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {read.csv(infile$datapath)}
    }) # end of dataset reactive function

    ## We need to scale the data 
    scale_it <- reactive({
      # Bring in the data
      data <- dataset()
      # Need to scale the data before weighting it
      # The scale() function will only take numceric values, so we need to subset off the markets.
      markets <- data[1]
      data_scaled <- scale(data[ , -1])
      # Bind the data back together
      data <- cbind(markets, data_scaled)
      
      return(data)
    }) # end of scale_it reactive function
    
    ## Get the weights 
    weight_it <- reactive({
      # Bring in the raw data
      data <- scale_it()
      # Grab the weights from the silders
      weight_1 <- input$weight_var1
      weight_2 <- input$weight_var2
      weight_3 <- input$weight_var3
      weight_4 <- input$weight_var4
      weight_5 <- input$weight_var5
      weight_6 <- input$weight_var6
      weight_7 <- input$weight_var7
      weight_8 <- input$weight_var8
      weight_9 <- input$weight_var9
      weight_10 <- input$weight_var10
      # Weight the variables
      if (ncol(data) == 3) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        
        return(data)
      } 
      else if (ncol(data) == 4) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        
        return(data)
      } 
      else if (ncol(data) == 5) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        
        return(data)
      }
      else if (ncol(data) == 6) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        
        return(data)
      }
      else if (ncol(data) == 7) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        data[ ,7] <- data[ ,7] * sqrt(weight_6)
        
        return(data)
      }
      else if (ncol(data) == 8) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        data[ ,7] <- data[ ,7] * sqrt(weight_6)
        data[ ,8] <- data[ ,8] * sqrt(weight_7)
        
        return(data)
      }
      else if (ncol(data) == 9) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        data[ ,7] <- data[ ,7] * sqrt(weight_6)
        data[ ,8] <- data[ ,8] * sqrt(weight_7)
        data[ ,9] <- data[ ,9] * sqrt(weight_8)
        
        return(data)
      }
      else if (ncol(data) == 10) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        data[ ,7] <- data[ ,7] * sqrt(weight_6)
        data[ ,8] <- data[ ,8] * sqrt(weight_7)
        data[ ,9] <- data[ ,9] * sqrt(weight_8)
        data[ ,10] <- data[ ,10] * sqrt(weight_9)
        
        return(data)
      }
      else if (ncol(data) == 11) {
        data[ ,2] <- data[ ,2] * sqrt(weight_1)
        data[ ,3] <- data[ ,3] * sqrt(weight_2)
        data[ ,4] <- data[ ,4] * sqrt(weight_3)
        data[ ,5] <- data[ ,5] * sqrt(weight_4)
        data[ ,6] <- data[ ,6] * sqrt(weight_5)
        data[ ,7] <- data[ ,7] * sqrt(weight_6)
        data[ ,8] <- data[ ,8] * sqrt(weight_7)
        data[ ,9] <- data[ ,9] * sqrt(weight_8)
        data[ ,10] <- data[ ,10] * sqrt(weight_9)
        data[ ,11] <- data[ ,11] * sqrt(weight_10)
        
        return(data)
      }
    }) # end of weight_it reactive function

    ## PCA Analysis
    compute_pca <- reactive({
      # Bring in the data
      data <- weight_it()
      # Drop the markets column
      data <- data[ ,-1]
      ## PCA
      pca <- prcomp(data, scale. = FALSE, center = FALSE)
      
      return(pca)
    }) # end of compute_pca reactive function
    
    ## Create dataframe for plotting the PCA
    pca_to_df <- reactive({
      # Bring in the PCA object
      pca <- compute_pca()
      # Grab summary
      pca_summary <- summary(pca)
      # Grab the importance meteric
      pca_importance <- pca_summary$importance
      # Create a df out of the importance meteric
      pca_importance <- data.frame(pca_importance)
      # Need to put pca int dataframe
      pca_df <- data.frame(pca_importance)
      
      return(pca_df)
    }) # end of pca_to_df reactive function
    
    ## PCA results dat frame for bar chart
    pca_results <- reactive({
      # Bring in data
      data <- weight_it()
      # Bring in pca object
      pca <- compute_pca()
      # Drop the markets column
      data <- data[ ,-1]
      # Set the seed
      set.seed(111)
      # Call K means 
      Kclust <- kmeans(data, input$clusters)
      # fortify() gets pca into usable format
      pca.fortify <- fortify(pca)
      # Add group (short for color) column using k
      pca.dat <- cbind(pca.fortify, group=Kclust$cluster)
      
      return(pca.dat)
    }) # end of pca_results reactive function
    
    ## Combine the results of PCA with the raw data
    combine_pca_with_raw <- reactive({
      # Bring in the raw data 
      data <- weight_it()
      # Bring in the PCA results 
      pca_resluts <- pca_results()      
      # Need to combine the raw data with the pca groupings 
      pca_resluts_final <- cbind(data, pca_resluts)
      # Drop the duplicated columns
      pca_resluts_final <- pca_resluts_final[, !duplicated(colnames(pca_resluts_final))]
      # Drop the PCA columns 
      pca_resluts_final <- pca_resluts_final[, !grepl("^PC", names(pca_resluts_final))]
      
      return(pca_resluts_final)
    }) # end of combine_pca_with_raw reactive function  
    
    ## Let's let the user know if it is ok to just use the first two PCs
    output$pca_result_explanation <- renderUI({
      if (input$ClusterChoice == 'More Than Three Columns') {
        # Bring in the pca percent explained varaince
        pca <- pca_to_df()
        # Need to melt the data into a strcture thta enables plotting
        pca_melted <- melt(pca[2, ])
        # Rename the columns
        colnames(pca_melted) <- c('PCs', 'Percent')
        # Multiply the percent columns by 100 to get the percentage form 
        pca_melted$Percent <- pca_melted$Percent * 100
        # Subset the df to just be the percent column
        percents <- pca_melted$Percent
        # Grab the individual percents
        percents_1 <- percents[1]
        percents_2 <- percents[2]
        percents_3 <- percents[3]
        percents_4 <- percents[4]
        percents_5 <- percents[5]
        percents_6 <- percents[6]
        percents_7 <- percents[7]
        percents_8 <- percents[8]
        percents_9 <- percents[9]
        percents_10 <- percents[10]
        # Grab the % expplained variance (the pca cutoff)
        pca_cutoff <- input$pca_cutoff
        # Tell the user which PCs they are ok using
        if (sum(percents_1, percents_2) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                      <strong>You may use just PC1 and PC2</strong>
                      </h3> 
                      <h4>
                      <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first two columns. Therefore, 
                      it is safe to say that you may discard any other columns.</strong> 
                      </h4>"))
        } else if (sum(percents_1, percents_2, percents_3) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                        <strong>You may use just PC1, PC2, and PC3</strong>
                        </h3> 
                        <h4>
                        <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first three columns. Therefore, 
                        it is safe to say that you may discard any other columns.</strong> 
                        </h4>"))
        } else if (sum(percents_1, percents_2, percents_3, percents_4) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                        <strong>You may use just PC1, PC2, PC3, and PC4</strong>
                        </h3> 
                        <h4>
                        <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first four columns. Therefore, 
                        it is safe to say that you may discard any other columns.</strong> 
                        </h4>"))
        } else if (sum(percents_1, percents_2, percents_3, percents_4, percents_5) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                        <strong>You may use just PC1, PC2, PC3, PC4, and PC5</strong>
                        </h3> 
                        <h4>
                        <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first five columns. Therefore, 
                        it is safe to say that you may discard any other columns.</strong> 
                        </h4>"))
        } else if (sum(percents_1, percents_2, percents_3, percents_4, percents_5, percents_6) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                        <strong>You may use just PC1, PC2, PC3, PC4, PC5, and PC6</strong>
                        </h3> 
                        <h4>
                        <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first six columns. Therefore, 
                        it is safe to say that you may discard any other columns.</strong> 
                        </h4>"))
        } else if (sum(percents_1, percents_2, percents_3, percents_4, percents_5, percents_6, percents_7) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                          <strong>You may use just PC1, PC2, PC3, PC4, PC5, PC6, and PC7</strong>
                          </h3> 
                          <h4>
                          <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first seven columns. Therefore, 
                          it is safe to say that you may discard any other columns.</strong> 
                          </h4>"))
        } else if (sum(percents_1, percents_2, percents_3, percents_4, percents_5, percents_6, percents_7, percents_8) > pca_cutoff) {
            return(HTML("<h3 style='color:#008000'>
                            <strong>You may use just PC1, PC2, PC3, PC4, PC5, PC6, PC7, and PC8</strong>
                        </h3> 
                        <h4>
                        <strong>You retain more than", pca_cutoff, "% of the information in your dataset with only using the first eight columns. Therefore, 
                        it is safe to say that you may discard any other columns.</strong> 
                        </h4>"))
        }
      }
      else{NULL}
    }) # end pca_result_explanation render to UI
    
    ## Plot the Variance for each PC
    output$pca_bar_plot <- renderPlotly({
      if (input$ClusterChoice == 'More Than Three Columns') { 
        # Bring in the pca dataframe
        pca_df <- pca_to_df()
        # Need to melt the data into a strcture thta enables plotting
        pca_bar <- melt(pca_df[2, ])
        # Rename the columns
        colnames(pca_bar) <- c('PCs', 'Percent')
        # Multiply the percent columns by 100 to get the percentage form 
        pca_bar$Percent <- pca_bar$Percent * 100
        # Lets plot it 
        pca_bar_plot <- ggplot(data = pca_bar, 
                                 aes(x = PCs, 
                                     y = Percent, 
                                     fill = PCs)) +
                               geom_bar(colour="black", 
                                        stat="identity") +
                               labs(x ='Principle Components', 
                                    y = 'Percent Variance Explained (%)')
        # Use Plotly for interactivity
        plotly_plot_pca_bar <- ggplotly(pca_bar_plot, tooltip = c("x", "y"))
        
        return(plotly_plot_pca_bar)
      }
      else{NULL}
    }) # end pca_bar_plot
    
    ## Render the PCA data to a table 
    output$pca_table <- DT::renderDataTable({
      if (input$ClusterChoice == 'More Than Three Columns') {
        # Bring in the pca results
        pca_results <- combine_pca_with_raw()
        # Render data table
        datatable(pca_results,
                  # Get rid of row indexes
                  rownames = FALSE,
                  # Enable downloading options
                  extensions = 'Buttons',
                  options = list(
                    dom = "Blfrtip",
                    buttons = 
                      list("copy", list(
                        extend = "collection",
                        buttons = c("csv", "excel", "pdf"),
                        text = "Download")),
                    lengthMenu = list(c(10, 20, -1),
                                      c(10, 20, "All")),
                    pageLength = 10))
      }
      else {NULL}
    }) # end DT pca_table
  } # end function call    
) # end shinyServer
