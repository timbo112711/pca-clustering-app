## ML Clustering Web App
## Verison 1.3
## Shiny app that brings a data file in, performs K means custering, and allows the user plot/download the 
## clustered data. This app can also perform PCA to help reduce the amount of variables in a given dataset. 
## There is an option to weight the variables based on how important they are. This requires the clients business 
## data and the users business logic to pick the best business variables in order to get the best market tiers.
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
    
    # Variable #1
    output$varselect1 <- renderUI({
      selectInput("var1", label = "First Column:",
                  choices = names(dataset()), selected = names(dataset())[1])  
    }) # end of varselect1 render UI
    # Variable #2
    output$varselect2 <- renderUI({
      selectInput("var2", label = "Second Column:",
                  choices = names(dataset()), selected = names(dataset())[2])  
    }) # end of varselect2 render UI

    output$varselect3 <- renderUI({
      selectInput("var3", label = "Third Column:",
                  choices = names(dataset()), selected = names(dataset())[3])  
    }) # end of varselect3 render UI
    # Variable #4 - The Bubble
    output$bubble_varselect <- renderUI({
      selectInput("bubble_var", label = "Column for Bubbles:",
                  choices = names(dataset()), selected = names(dataset())[4])  
    }) # end of bubble_varselect render UI
    
    ## Read in the data
    dataset <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {read.csv(infile$datapath)}
    }) # end of dataset reactive function
    
    ## Get the weights 
    weight_it <- reactive({
      # Bring in the raw data
      data <- dataset()
      # Grab the weights from the sliders
      weight_1 <- input$weight_var1
      weight_2 <- input$weight_var2
      weight_3 <- input$weight_var3
      # Weight the variables
      data[[input$var1]] <- data[[input$var1]] * weight_1
      data[[input$var2]] <- data[[input$var2]] * weight_2
      data[[input$var3]] <- data[[input$var3]] * weight_3
      
      return(data)
    }) # end of weight_it reactive function
    
    ## Compute the K means algo
    compute_kmeans <- reactive({
      # Choose between simple K means or a more complex K means with third variable
      if (input$ClusterChoice == 'Two Column Tiering') {
        # Bring in the raw data
        data <- subset(weight_it(), select = c(input$var1, input$var2))
        # Change some columns names
        colnames(data) <- c('x', 'y')
        data <- na.omit(data)
        # Cluster
        Kclust <- kmeans(data, input$clusters, iter.max=15, nstart = 5)
        # Save results to a list
        kmean.result <- list(kmean.result = data.frame(data, cluster = as.factor(Kclust$cluster)))
        
        return(kmean.result)
      }
      # K means with third variable 
      else { (input$ClusterChoice == 'Three Column Tiering') 
        # Bring in the raw data
        three_var_data <- subset(weight_it(), select = c(input$var1, input$var2, input$var3))
        three_var_data <- na.omit(three_var_data)
        # Cluster
        Kclust <- kmeans(three_var_data, input$clusters)
        kmean.result <- list(kmean.result = data.frame(three_var_data, cluster = as.factor(Kclust$cluster)))
        
        return(kmean.result)
      }
    }) # end of compute_kmeans reactive function
    
    ## Create a dataframe of the K means & K means with third variable results
    kmeans_results <- reactive({
      # For only two variables 
      if (input$ClusterChoice == 'Two Column Tiering') {
        # Call the method that computes the k means
        data <- compute_kmeans()
        # Save the results into a df 
        results <- data$kmean.result
        results_df <- data.frame(results)
        colnames(results_df) <- c(input$var1, input$var2, 'Grouping')
        
        return(results_df)
      }
      # For more than two variables
      else { (input$ClusterChoice == 'Three Column Tiering')
        # Call the method that computes the k means with third variable
        three_var_data <- compute_kmeans()
        # Save the results into a df 
        three_var_results <- three_var_data$kmean.result
        three_var_results_df <- data.frame(three_var_results)
        colnames(three_var_results_df) <- c(input$var1, 
                                            input$var2, 
                                            input$var3,
                                            'Grouping')
        
        return(three_var_results_df)
      }
    }) # end of kmeans_results reactive function
    
    ## Results of each K means & K means with third variable
    master_results <- reactive({
      # Call the method that stores the raw input data
      data <- weight_it()
      # For only two variables
      if(input$ClusterChoice == 'Two Column Tiering') {
        # Call the method that stores the K means results
        kmean_results <- kmeans_results()
        # Merge the K means results with the raw input data
        results <- merge(kmean_results, data)
        results <- results[!duplicated(results), ]
        
        return(results)
      }
      # For more than two variables
      else { (input$ClusterChoice == 'Three Column Tiering')
        # Call the method that stores the K means with third variable results
        kmean_three_var_results <- kmeans_results()
        # Merge the K means with third variable results with the raw input data
        three_var_resutls <- merge(kmean_three_var_results, data)
        three_var_resutls <- three_var_resutls[!duplicated(three_var_resutls), ]
        
        return(three_var_resutls)
      }
    }) # end of master_results reactive function

    ## PCA Analysis
    compute_pca <- reactive({
      # Bring in the data
      data <- dataset()
      # Drop the markets column
      data <- data[ ,-1]
      ## PCA
      pca <- prcomp(data, scale. = T, center = T)
      
      return(pca)
    }) # end of compute_pca reactive function
    
    ## Create dataframe for plotting the PCA
    pca_to_df <- reactive({
      # Bring in the PCA object
      pca <- compute_pca()
      # Grab summary
      pca_summary <- summary(pca)
      # Grab the importance metric
      pca_importance <- pca_summary$importance
      # Create a df out of the importance metric
      pca_importance <- data.frame(pca_importance)
      # Need to put pca int dataframe
      pca_df <- data.frame(pca_importance)
      
      return(pca_df)
    }) # end of pca_to_df reactive function
    
    # TODO: update this to auto update based on the explained variance 
    ## PCA results data frame for bar chart
    pca_results <- reactive({
      # Bring in data
      data <- dataset()
      # Bring in pca object
      pca <- compute_pca()
      # Drop the markets column
      data <- data[ ,-1]
      # Call K means 
      Kclust <- kmeans(data, input$clusters, iter.max=15, nstart = 5)
      # fortify() gets pca into usable format
      pca.fortify <- fortify(pca)
      # Add group (short for color) column using k
      pca.dat <- cbind(pca.fortify, group=Kclust$cluster)
      
      return(pca.dat)
    }) # end of pca_results reactive function
    
    ## Update the clustering based on the PCs explain variance 
    update_pca_clustering <- reactive({
      # Bring in the pca percent explained variance
      pca <- pca_to_df()
      # Bring in the first PCA results
      pca_original_results <- pca_results()
      # Need to melt the data into a structure that enables plotting
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
      # Grab the % explained variance (the pca cutoff)
      pca_cutoff <- input$pca_cutoff
      # Tell the user which PCs they are OK using
      if (sum(percents_1, percents_2) > pca_cutoff) {
        # Subset by the most just PC1 and PC2
        cluster_pc1_pc2 <- subset(pca_original_results, select = c("PC1", "PC2"))
        # Call K means on just PC1 and PC2
        Kclust <- kmeans(cluster_pc1_pc2, input$clusters)
        kmean.result <- list(kmean.result = data.frame(three_var_data, cluster = as.factor(Kclust$cluster)))
        # Save the results into a df 
        pc1_pc2_clusters <- kmean.result
        pc1_pc2_clusters_df <- data.frame(pc1_pc2_clusters)
        
        return(kmean.result)
      }
      return(pca.dat)
    }) # end of update_pca_clustering reactive function 
    
    ## Combine the results of PCA with the raw data
    combine_pca_with_raw <- reactive({
      # Bring in the raw data 
      data <- dataset()
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
    
    ## Let's let the user know if it is OK to just use the first two PCs
    output$pca_result_explanation <- renderUI({
      if (input$ClusterChoice == 'More Than Three Columns') {
        # Bring in the pca percent explained variance
        pca <- pca_to_df()
        # Need to melt the data into a structure that enables plotting
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
        # Grab the % explained variance (the pca cutoff)
        pca_cutoff <- input$pca_cutoff
        # Tell the user which PCs they are OK using
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
        # Need to melt the data into a structure that enables plotting
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
    
    ## Plot the K means results
    output$kmeans_plot <- renderPlotly({
      # For only two variables
      if (input$ClusterChoice == 'Two Column Tiering') {
        # Call the K means results method
        results <- master_results()
        # Change the x & y variables so we can call it in the tool-tip
        x_axis <- results[[input$var1]]
        y_axis <- results[[input$var2]]
        Bubble <- results[[input$bubble_var]]
        # Plot
        plot <- ggplot(data = results, 
                         aes(x = x_axis, 
                             y = y_axis, 
                             color = Grouping,
                             size = Bubble,
                             name = Markets)) +
                             geom_point() + 
                           ggtitle("Grouping Results") +
                           geom_vline(xintercept = 0) + 
                           geom_hline(yintercept = 0) +
                           labs(x = input$var1, 
                                y = input$var2)
        # Use Plotly for interactivity
        plotly_plot <- ggplotly(plot)
        
        return(plotly_plot)
      } 
      else{NULL}
    }) # end kmeans_plot
    
    ## 3D Plot
    output$plot_3D <- renderPlot({
      # For more than two variables
      if (input$ClusterChoice == 'Three Column Tiering') {
        # Call the K means and third variable 
        three_var_resutls <- master_results()
        # Change x, y, & z variables so we can call it in the tool-tip
        x_axis <- three_var_resutls[[input$var1]]
        y_axis <- three_var_resutls[[input$var2]]
        z_axis <- three_var_resutls[[input$var3]]
        # Get the colors for the clusters 
        three_var_resutls$culstercolor[three_var_resutls$Grouping==1] <- "blue"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==2] <- "red"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==3] <- "darkgreen"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==4] <- "orange"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==5] <- "black"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==6] <- "purple"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==7] <- "yellow"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==8] <- "green"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==9] <- "orangered"
        three_var_resutls$culstercolor[three_var_resutls$Grouping==10] <- "lightgreen"
        # Plot parameters
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        # Plot it dude
        three_var_plot <- scatterplot3d(x_axis, 
                                        y_axis, 
                                        z_axis,
                                        color = three_var_resutls$culstercolor,
                                        pch = 16,
                                        grid=TRUE, 
                                        box=FALSE,
                                        main = 'Groupings',
                                        xlab = input$var1, 
                                        ylab = input$var2, 
                                        zlab = input$var3)
        legend("right", 
               legend = levels(three_var_resutls$Grouping),
               col =  c("blue", "red", "darkgreen", "orange", "black", 
                        "purple", "yellow", "green", "orangered", "lightgreen"), 
               pch = 16,
               title="Groupings")
        
        return(three_var_plot)
      }
      else{NULL}
    }) # end plot_3D
    
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
    
    ## Render the K means and K means with third variable results to a data table
    output$cluster_table <- DT::renderDataTable({
      # For only two variables
      if (input$ClusterChoice == 'Two Column Tiering') {
        # Call results method
        results <- master_results()
        # Render data table
        datatable(results,
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
      else if (input$ClusterChoice == 'Three Column Tiering') {
        # Call results method
        results_three_var <- master_results()
        # Render data table
        datatable(results_three_var,
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
    }) # end DT cluster_table
  } # end function call    
) # end shinyServer

