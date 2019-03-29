## Market Selection for Media Testing
## Verison 1.0
# Shiny app that brings a data file in, performs PCA and then performs K means custering based on the set of 
# variables that explain most of the data, and allows the user plot/download the clustered data. There is an 
# option to weight the variables based on how important they are. This requires the clients business data and 
# the users business logic to pick the best business variables in order to get the best market selecions for media testing.
## Needed Lib's
library(shiny)
library(plotly)

## Start the UI renderer
shinyUI(
  ## Create the page that has a sidebar
  pageWithSidebar(
    ## The TITLE!
    headerPanel("Market Selection For Media Testing"),
    ## Create the side bar panel (this is where the up-loader and drop downs live)
    sidebarPanel(
      # The file input/uploader
      fileInput('datafile', 
                'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      # Selection of number of clusters
      sliderInput("clusters", 
                  "Number of Groups:",
                  min = 1, 
                  max = 10,
                  value = 5, 
                  step = 1, 
                  post = " Groups"),
      # Variable 1 weighting 
      sliderInput("weight_var1", 
                  "Weight For First Numeric Column:",
                  min = 0, 
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 2 weighting
      sliderInput("weight_var2", 
                  "Weight For Second Numeric Column:",
                  min = 0, 
                  max = 1,
                  value = 1, 
                  step = 0.05,
                  pre = "Weight of "),

      # Variable 3 weighting
      sliderInput("weight_var3", 
                  "Weight For Third Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 4 weighting
      sliderInput("weight_var4", 
                  "Weight For Fourth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 5 weighting
      sliderInput("weight_var5", 
                  "Weight For Fifth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 6 weighting
      sliderInput("weight_var6", 
                  "Weight For Sixth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 7 weighting
      sliderInput("weight_var7", 
                  "Weight For Seventh Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 8 weighting
      sliderInput("weight_var8", 
                  "Weight For Eighth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 9 weighting
      sliderInput("weight_var9", 
                  "Weight For Ninth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of "),
      # Variable 10 weighting
      sliderInput("weight_var10", 
                  "Weight For Tenth Numeric Column:",
                  min = 0,
                  max = 1,
                  value = 1, 
                  step = 0.05, 
                  pre = "Weight of ")),
    ## The main panel where all the shit happens 
    mainPanel(
      # Make it so errors don't show in app, only in logs
      tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
      ## Tabs
      textOutput("text"),
      # Create the tabs
      tabsetPanel(id = 'ClusterChoice',
                  tabPanel("Instructions", value = "Instructions", 
                           h2("Instructions For Using This Tool"),
                           p("Please follow these instructions to create your groupings."),
                           p("1. Upload a data file (.csv) that will be used for making your groups.
                             The first row in the .csv file should be the column headers of the data. 
                             The first column in the .csv file should be where your data starts.
                             THERE SHOULD BE NO 'BUFFERS' AROUND YOUR DATA FILE, i.e. empty rows and columns."),
                           p("2. Indicate the weight of each column via the sliders. 1 being the most important and 0 being the least important "),
                           p("3. Indicate the desired number of groups via the slider.")),
                  tabPanel("Market Selection For Media Testing Results", value = 'More Than Three Columns',
                           h2("Principle Components Analysis"),
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
