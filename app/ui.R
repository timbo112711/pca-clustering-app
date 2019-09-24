## ML Clustering Web App
## Verison 1.3
## Shiny app that brings a data file in, performs K means custering, and allows the user plot/download the 
## clustered data. This app can also perform PCA to help reduce the amount of variables in a given dataset. 
## There is an option to weight the variables based on how important they are.
## Needed Lib's
library(shiny)
library(plotly)

## Start the UI renderer
shinyUI(
  ## Create the page that has a sidebar
  pageWithSidebar(
    ## The TITLE!
    headerPanel("Grouping Data Together"),
    ## Create the side bar panel (this is where the up-loader and drop downs live)
    sidebarPanel(
      # The file input/uploader
      fileInput('datafile', 
                'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      # Variable 1
      uiOutput("varselect1"),
      # Variable 1 weighting 
      sliderInput("weight_var1", 
                  "Weight For First Column:",
                  min = 0, 
                  max = 10,
                  value = 1, 
                  step = 0.1, 
                  pre = "Weight of "),
      # Variable 2
      uiOutput("varselect2"),
      # Variable 2 weighting
      sliderInput("weight_var2", 
                  "Weight For Second Column:",
                  min = 0, 
                  max = 10,
                  value = 1, 
                  step = 0.1,
                  pre = "Weight of "),
      # Variable 3
      uiOutput("varselect3"),
      # Variable 3 weighting
      sliderInput("weight_var3", 
                  "Weight For Third Column:",
                  min = 0,
                  max = 10,
                  value = 1, 
                  step = 0.1, 
                  pre = "Weight of "),
      # Selection of number of clusters
      sliderInput("clusters", 
                  "Number of Groups:",
                  min = 1, 
                  max = 25,
                  value = 5, 
                  step = 1, 
                  post = " Groups"),
      # Instructions for using this app
      h2("Instructions"),
      p("Please follow these instructions to create your groupings."),
      p("1. Upload a data file (.csv) that will be used for making your groups.
        The first row in the .csv file should be the column headers of the data. 
        The first column in the .csv file should be where your data starts.
        THERE SHOULD BE NO 'BUFFERS' AROUND YOUR DATA FILE, i.e. empty rows and columns."),
      p("2. Pick the columns you want to create the groupings off of via the drop-downs"),
      p("3. Indicate the weight of each column via the sliders. 1 being the most important and 0 being the least important "),
      p("4. Indicate the desired number of groups via the slider."),
    ## The main panel where all the shit happens 
    mainPanel(
      # Make it so errors don't show in app, only in logs
      tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
      ## Tabs
      textOutput("text"),
      # Create the tabs (total of 3 tabs)
      tabsetPanel(id = 'ClusterChoice',
                  tabPanel("About", value = "About This Tool Tab",
                           h2("Sample Use Cases"),
                           p("- Your client wants to group markets into similar groups based on the clients business data."),
                           p("- Your client wants to group the offices or stores into similar groups."),
                           p("- Your client wants to create 'Tiers' of markets in order to gauge their business performance."),
                           h2("Want To Know More About Grouping (Clustering)?"),
                           p("This method uses what is called the K-means clustering algorithm. This is a versatile algorithm that can be 
                             used for any type of grouping. When used, it will find groups which have not been explicitly labeled in the 
                             data, i.e. 'Business Tiers'. This can be used to confirm business assumptions about what types of groups exist 
                             or to identify unknown groups in complex data sets.")),
                  tabPanel("Market Prioritization Results: Two Columns", value = 'Two Column Tiering',
                           # The plot instructions 
                           h2("Visualizing Your Grouped Data"),
                           p("Your groupings are displayed below. They are separated by colors. Each grouping has their own color."),
                           p("You can download the plot as a .png file by hovering over the plot and selecting the 
                             camera icon in the upper right hand side of the plot."),
                           # The Bubble variable 
                           h4("Create Bubbled Dots"),
                           uiOutput("bubble_varselect"),
                           plotlyOutput('kmeans_plot', 
                                        height = 700),
                           # The grouping data table instructions
                           h2("The Grouping Data"),
                           p("Instructions for downloading data"),
                           p("1. To download the data use the drop-down below, labeled 'Show entries', to show 'All' entries."),
                           p("2. Click the 'Download' button and then select the type of file (PDF, excel, csv)."),
                           p("3. (Optional) You can copy the data that is shown to your clipboard is paste it into an excel/csv document.")),
                  tabPanel("Market Prioritization Results: Three Columns", value = 'Three Column Tiering',
                           # The plot instructions 
                           h2("Visualizing Your Grouped Data"),
                           p("Your groupings are displayed below. They are separated by colors. Each grouping has their own color."),
                           p("You can download the plot as a .png file by hovering over the plot and selecting the 
                             camera icon in the upper right hand side of the plot."),
                           # 3D plot
                           plotOutput('plot_3D', 
                                      height = 700),
                           ## The grouping data table instructions
                           h2("The Grouping Data"),
                           p("Instructions for downloading data"),
                           p("1. To download the data use the drop-down below, labeled 'Show entries', to show 'All' entries."),
                           p("2. Click the 'Download' button and then select the type of file (PDF, excel, csv)."),
                           p("3. (Optional) You can copy the data that is shown to your clipboard is paste it into an excel/csv document.")),
                  tabPanel("Market Selection For Media Testing Results", value = 'More Than Three Columns',
                           h2("The Principle Components"),
                           uiOutput("pca_result_explanation"),
                           # Selection for the PCA cutoff (% explained varianace)
                           sliderInput("pca_cutoff", 
                                       "PCA Cutoff (%):",
                                       min = 50, 
                                       max = 100,
                                       value = 80, 
                                       step = 5,
                                       post = " % Explained"),
                           # PCA bar plot
                           plotlyOutput('pca_bar_plot', 
                                        height = 300),
                           h2("Your Grouped Data"),
                           # PCA datatable
                           DT::dataTableOutput("pca_table"))),
      # JS DataTable function that links the UI to the Server
      DT::dataTableOutput("cluster_table")
    ) # end of mainPanel
  ) # end of pageWithSidebar
) # end of shinyUI 
