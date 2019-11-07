library(cluster)
library(shiny)
library(tidyr)
library(dplyr)
library(plyr)
library(plotly)
library(readxl)
library(factoextra)
library(StatMatch)
library(reshape2)
library(ggplot2)
library(data.table)
library(googledrive)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- navbarPage("College Scorecard",
                 tabPanel("Map",
                          # Application title
                          titlePanel("Map of Schools with the best fit"),
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarPanel(
                            sliderInput("size", h3("School size: "),
                                                         #increasing max by 1, so countries with max values show on the map 
                                                         #min is 0, so no need for changing it
                                                         min = min(data2016.slider$UGDS), max = max(data2016.slider$UGDS)+1, value = c(3000, 10000)),
                                             #checkboxes for choosing control
                                             checkboxGroupInput("control", h3("Choose the type of the school: "), 
                                                         choices = list("Public", 
                                                                        "Private nonprofit",
                                                                        "Private for-profit"),
                                                         selected = "Public"),
                                             #checkboxes for choosing region
                                             checkboxGroupInput("region", h3("Region: "), 
                                                                choices = list("US Service Schools", 
                                                                               "New England",
                                                                               "Mid East",
                                                                               "Great Lakes",
                                                                               "Plains",
                                                                               "Southeast",
                                                                               "Southwest",
                                                                               "Rocky Mountains" ,
                                                                               "Far West",
                                                                               "Outlying Areas"),
                                                                selected = "Mid East"),
                                             #checkboxes for choosing location
                                             checkboxGroupInput("locale", h3("Location: "), 
                                                                choices = list("City", 
                                                                               "Suburb",
                                                                               "Town",
                                                                               "Rural"),
                                                                selected = "City"),
                                             #slider for the annual cost of the school
                                             sliderInput("cost", h3("Annual cost: "),
                                                         #decreasing/increasing min and max by 1, so the schools with min and max values show on the map as well
                                                         min = min(data2016.slider$COSTT4_A)-1, max = max(data2016.slider$COSTT4_A)+1, value = c(20000)),
                                             #choosing by which test to filter
                                             radioButtons("sat_act",
                                                          h3("Choose the standardized test to filter by:"),
                                                          choices = list ("SAT" = 1, 
                                                                          "ACT" = 2)),
                                             #input if SAT is chosen
                                             conditionalPanel("input.sat_act === '1'",
                                                              numericInput("satread", h3("SAT Critical Reading Midpoint: "), 
                                                                           value = 500, min = 1, max = 800),
                                                              numericInput("satmath", h3("SAT Math Midpoint: "), 
                                                                           value = 500, min = 1, max = 800),
                                                              numericInput("satwrit", h3("SAT Writing Midpoint: "),
                                                                           value = 500, min = 1, max = 800)),
                                             #input if ACT is chosen
                                             conditionalPanel("input.sat_act === '2'",
                                                              numericInput("acteng", h3("ACT English Midpoint: "), 
                                                                           value = 30, min = 1, max = 36),
                                                              numericInput("actmath", h3("ACT Math Midpoint: "), 
                                                                           value = 30, min = 1, max = 36),
                                                              numericInput("actwrit", h3("ACT Writing Midpoint: "), 
                                                                           value = 30, min = 1, max = 36)),
                            radioButtons("womenonly",
                                         h3("Filter women-only schools:"),
                                         choices = list ("No" = 0, 
                                                         "Yes" = 1),
                                         selected = 0),
                            radioButtons("menonly",
                                         h3("Filter men-only schools:"),
                                         choices = list ("No" = 0, 
                                                         "Yes" = 1),
                                         selected = 0),
                            radioButtons("firstgen",
                                         h3("Filter schools with 30% of first-generation students:"),
                                         choices = list ("No" = 0, 
                                                         "Yes" = 1),
                                         selected = 0)
                            ),
                        
                 # Show a plot of the generated distribution
                 mainPanel(
                   #print out chosen values for each filtering variable
                   verbatimTextOutput("o.size"),
                   verbatimTextOutput("o.control"),
                   verbatimTextOutput("o.region"),
                   verbatimTextOutput("o.locale"),
                   verbatimTextOutput("o.cost"),
                   #if SAT chosen, show filtering values
                   conditionalPanel("input.sat_act === '1'",
                                    verbatimTextOutput("o.satread"),
                                    verbatimTextOutput("o.satmath"),
                                    verbatimTextOutput("o.satwrit")),
                   #if ACT chosen, show filtering values
                   conditionalPanel("input.sat_act === '2'",
                                    verbatimTextOutput("o.acteng"),
                                    verbatimTextOutput("o.actmath"),
                                    verbatimTextOutput("o.actwrit")),
                   conditionalPanel("input.womenonly === '1'",
                                    verbatimTextOutput("o.womenonly")),
                   conditionalPanel("input.menonly === '1'",
                                    verbatimTextOutput("o.menonly")),
                   conditionalPanel("input.firstgen === '1'",
                                    verbatimTextOutput("o.firstgen")),
                   #show the map
                   plotlyOutput("map") 
                 )),
                 tabPanel("Clustering",
                          pageWithSidebar(
                            headerPanel('College clustering'),
                            sidebarPanel(
                              numericInput('clusters', h3('Center count'), 3,
                                           min = 1, max = 9),
                              
                              checkboxInput('pca', 'Use the first two principle components', value = TRUE),
                              conditionalPanel(
                                condition = "input.pca == false",
                                selectInput('ycol', h3('Y Variable'), select.vars,
                                            selected=select.vars[[2]]),
                                selectInput('xcol', h3('X Variable'), select.vars)
                              ),
                              checkboxGroupInput("clustering_control", h3("Type of schools: "),
                                                 choices = list(
                                                   "Public" = 1,
                                                   "Private nonprofit" = 2,
                                                   "Private for-profit" = 3
                                                 ),
                                                 selected = "Public"
                              ),
                              checkboxGroupInput("clustering_region", h3("Region"),
                                                 choices = list(
                                                   "US Service Schools" = 0, 
                                                   "New England" = 1,
                                                   "Mid East" = 2, 
                                                   "Great Lakes" = 3,
                                                   "Plains" = 4, 
                                                   "Southeast" = 5,
                                                   "Southwest" = 6,
                                                   "Rocky Mountains" = 7,
                                                   "Far West" = 8,
                                                   "Outlying Areas" = 9
                                                 ),
                                                 selected = "New England"
                              ),
                              checkboxGroupInput("clustering_locale", h3("Location"),
                                                 choices = list(
                                                   "City: Large" = 11, "City: Midsize" = 12, "City: Small" = 13, 
                                                   "Suburb: Large" = 21, "Suburb: Midsize" = 22, "Suburb: Small" = 23, 
                                                   "Town: Fringe" = 31, "Town: Distant" = 32, "Town: Remote" = 33, 
                                                   "Rural: Fringe" = 41, "Rural: Distant" = 42, "Rural: Remote" = 43
                                                 ),
                                                 selected = "Town: Remote"
                              ),
                              checkboxGroupInput("clustering_relaffil", h3("Religious Affiliation"),
                                                 choices = list(
                                                   "None" = '-1',
                                                   "American Evangelical Lutheran Church" = '22',
                                                   "African Methodist Episcopal Zion Church" = '24',
                                                   "Assemblies of God Church" = '27',
                                                   "Brethren Church" = '28',
                                                   "Roman Catholic" = '30',
                                                   "Wisconsin Evangelical Lutheran Synod" = '33',
                                                   "Christ and Missionary Alliance Church" = '34',
                                                   "Christian Reformed Church" = '35',
                                                   "Evangelical Covenant Church of America" = '37',
                                                   "Evangelical Free Church of America" = 38,
                                                   "Evangelical Lutheran Church" = 39,
                                                   "Free Will Baptist Church" = 41,
                                                   "Interdenominational" = 42,
                                                   "Mennonite Brethren Church" = 43,
                                                   "Moravian Church" = 44,
                                                   "Pentecostal Holiness Church" = 47,
                                                   "Christian Churches and Churches of Christ" = 48,
                                                   "Reformed Church in America" = 49,
                                                   "African Methodist Episcopal" = 51,
                                                   "American Baptist" = 52,
                                                   "Baptist" = 54,
                                                   "Church of God" = 57,
                                                   "Church of Brethren" = 58,
                                                   "Church of the Nazarene" = 59,
                                                   "Christian Church (Disciples of Christ)" = 61,
                                                   "Free Methodist" = 64,
                                                   "Friends" = 65,
                                                   "Presbyterian Church (USA)" = 66,
                                                   "Lutheran Church in America" = 67,
                                                   "Lutheran Church - Missouri Synod" = 68,
                                                   "Mennonite Church" = 69,
                                                   "United Methodist" = 71,
                                                   "Protestant Episcopal" = 73,
                                                   "Churches of Christ" = 74,
                                                   "Southern Baptist" = 75,
                                                   "United Church of Christ" = 76,
                                                   "Multiple Protestant Denomination" = 78,
                                                   "Other Protestant" = 79,
                                                   "Jewish" = 80,
                                                   "Reformed Presbyterian Church" = 81,
                                                   "United Brethren Church" = 84,
                                                   "Missionary Church Inc" = 87,
                                                   "Undenominational" = 88,
                                                   "Wesleyan" = 89,
                                                   "Latter Day Saints (Mormon Church)" = 94,
                                                   "Seventh Day Adventists" = 95,
                                                   "The Presbyterian Church in America" = 97,
                                                   "Original Free Will Baptist" = 100,
                                                   "Evangelical Christian" = 102,
                                                   "Presbyterian" = 103,
                                                   "General Baptist" = 105,
                                                   "Plymouth Brethren" = 107
                                                 ),
                                                 selected = "None"
                              )
                            ),
                            mainPanel(
                              conditionalPanel(
                                condition = "input.pca == true",
                                h3("PCA Analysis:"),
                                tableOutput('dimensions')
                              ),
                              h3("Centers:"),
                              tableOutput("centroids"),
                              h3("Clustering Plot:"),
                              plotOutput('pamPlot')
                            )
                          )
                 ),
                 tabPanel("Prediction",
                          titlePanel("Predicting the best school"),
                          sidebarPanel(
                            sliderInput("size_pred", h3("Select school size: "),
                                        #increasing max by 1, so countries with max values values show on the map
                                        #min is already 0, so no reason for decreasing it
                                        min = min(data_pred$UGDS), max = max(data_pred$UGDS)+1, value =10000 ),
                            #checkboxes for choosing control
                            selectInput("control_pred", h3("Choose the type of schools: "), 
                                               choices = list("Public", 
                                                              "Private nonprofit",
                                                              "Private for-profit")),
                            #checkboxes for choosing region
                            selectInput("region_pred", h3("Select region: "), 
                                               choices = list("US Service Schools" = 0, 
                                                              "New England" = 1,
                                                              "Mid East" = 2,
                                                              "Great Lakes" = 3,
                                                              "Plains" = 4,
                                                              "Southeast" = 5,
                                                              "Southwest" = 6,
                                                              "Rocky Mountains" = 7,
                                                              "Far West" = 8,
                                                              "Outlying Areas" = 9),
                                        selected = "Plains"),
                            #checkboxes for choosing location
                            selectInput("locale_pred", h3("Select location: "), 
                                               choices = list("City" = 11, 
                                                              "Suburb" = 21,
                                                              "Town" = 31,
                                                              "Rural" = 41),
                                               selected = "City"),
                            #slider for the annual cost of the school
                            sliderInput("cost_pred", h3("Select annual cost: "),
                                        #decreasing/increasing min and max by 1, so schools with min and max values show on the map as well 
                                        min = min(data2016.slider$COSTT4_A)-1, max = max(data2016.slider$COSTT4_A)+1, value = 20000),
                            #choosing by which test to filter
                            radioButtons("sat_act_pred",
                                         h3("Choose the standardized test to predict by:"),
                                         choices = list ("SAT" = 1, 
                                                         "ACT" = 2)),
                            #input if SAT is chosen
                            conditionalPanel("input.sat_act_pred === '1'",
                                             numericInput("satread_pred", h3("SAT Critical Reading Midpoint: "), 
                                                          value = 500,min = 1, max = 800),
                                             numericInput("satmath_pred", h3("SAT Math Midpoint: "), 
                                                          value = 500,min = 1, max = 800)),
                            #input if ACT is chosen
                            conditionalPanel("input.sat_act_pred === '2'",
                                             numericInput("acteng_pred", h3("ACT English Midpoint: "), 
                                                          value = 30, min = 1, max = 36),
                                             numericInput("actmath_pred", h3("ACT Math Midpoint: "), 
                                                          value = 30, min = 1, max = 36)),
                            radioButtons("womenonly_pred",
                                         h3("Predict for women-only schools:"),
                                         choices = list ("Yes" = 1, 
                                                         "No" = 0),
                                         selected = 0),
                            radioButtons("menonly_pred",
                                         h3("Predict for men-only schools:"),
                                         choices = list ("Yes" = 1, 
                                                         "No" = 0),
                                         selected = 0),
                            radioButtons("firstgen_pred",
                                         h3("Predict for schools with first-generation students:"),
                                         choices = list ("Yes" = 1, 
                                                         "No" = 0),
                                         selected = 0)
                            ),
                          mainPanel(
                            #print out chosen values for each filtering variable
                            verbatimTextOutput("o.size_pred"),
                            verbatimTextOutput("o.control_pred"),
                            verbatimTextOutput("o.region_pred"),
                            verbatimTextOutput("o.locale_pred"),
                            verbatimTextOutput("o.cost_pred"),
                            #if SAT chosen, show filtering values
                            conditionalPanel("input.sat_act_pred === '1'",
                                             verbatimTextOutput("o.satread_pred"),
                                             verbatimTextOutput("o.satmath_pred")),
                            #if ACT chosen, show filtering values
                            conditionalPanel("input.sat_act_pred === '2'",
                                             verbatimTextOutput("o.acteng_pred"),
                                             verbatimTextOutput("o.actmath_pred")),
                            conditionalPanel("input.womenonly_pred === '1'",
                                            verbatimTextOutput("o.womenonly_pred")),
                            conditionalPanel("input.menonly_pred === '1'",
                                             verbatimTextOutput("o.menonly_pred")),
                            conditionalPanel("input.firstgen_pred === '1'",
                                             verbatimTextOutput("o.firstgen_pred")),
                                            
                            #show the college matches
                            h3("Top 10 College Matches"),
                            tableOutput("college_matches")
                          )),
                 tabPanel("Historical comparison",
                          titlePanel("College Attributes Through Time: 2009-2016"),
                          mainPanel({
                            plotOutput("lineplot")
                            
                          }),
                          
                          sidebarPanel(
                            #Let user select range of years for comparison
                            sliderInput(inputId = "range", label = h3("Years"), min = 2009, 
                                        max = 2016, value = c(2009, 2016)),
                            
                            #Let user select variable to see change over time
                            selectInput(inputId = "var", 
                                        label = "Variable", 
                                        choices = c ("Admissions Rate" = 'ADM_RATE',
                                                     "Size" = 'UGDS', 
                                                     "Median Verbal SAT Score"='SATVRMID', 
                                                     "Median Math SAT Score" = 'SATMTMID', 
                                                     "Median Writing SAT Score"='SATWRMID', 
                                                     "Median English ACT Score"='ACTENMID', 
                                                     "Median Math ACT Score" = 'ACTMTMID', 
                                                     "Median Writing ACT Score"='ACTWRMID', 
                                                     "Average Annual Cost of Attendance"= 'COSTT4_A', 
                                                     "Average Faculty Salary" = "AVGFACSAL", 
                                                     "Percent Full-Time Faculty" = "PFTFAC", 
                                                     "Percent First-Generation Students" = 'PAR_ED_PCT_1STGEN')),
                            
                            
                            #autocomplete/select multiple schools
                            selectizeInput(inputId = "schools", label = "College(s)", collegenames,
                                           selected = NULL, multiple = TRUE))
                 ),
                 tabPanel("Authors",
                          p("Shuyi Qi, Milica Cvrkota and Madeleine Vessely at Grinnell College.")
                 )
                          
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ################################# Map #################################################
  
  #map output starts here
  #print out chosen variables for filtering the map
  output$o.size <- renderText( {
    paste("You have selected schooles with sizes between ", input$size[1], " and ", input$size[2], ".") 
  })
  output$o.control <- renderText( {
    paste("You have selected schooles with ", input$control, " type.") 
  })
  output$o.region <- renderText( {
    paste("You have selected schools in ", input$region, " region.") 
  })
  output$o.locale <- renderText( {
    paste("You have selected schools in ", input$locale, " location.") 
  })
  output$o.cost <- renderText( {
    paste("You have selected schooles with cost between $", input$cost, ".") 
  })
  output$o.satread <- renderText( {
    paste("SAT Reading Midpoint: ", input$satread,".") 
  })
  output$o.satmath <- renderText( {
    paste("SAT Math Midpoint: ", input$satmath,".") 
  })
  output$o.satwrit <- renderText( {
    paste("SAT Writing Midpoint: ", input$satwrit,".") 
  })
  output$o.acteng <- renderText( {
    paste("ACT English Midpoint: ", input$acteng,".") 
  })
  output$o.actmath <- renderText( {
    paste("ACT Math Midpoint: ", input$actmath,".") 
  })
  output$o.actwrit <- renderText( {
    paste("ACT Writing Midpoint: ", input$actwrit,".") 
  })
  output$o.womenonly <- renderText( {
    paste("You have chosen women-only schools.") 
  })
  output$o.menonly <- renderText( {
    paste("You have chosen men-only schools.") 
  })
  output$o.firstgen <- renderText( {
    paste("You have chosen schools with at least 30% of first-generation students.") 
  })
  
  #print out chosen variables for the prediction
  output$o.size_pred <- renderText( {
    paste("You have selected schooles with sizes between ", input$size_pred, ".") 
  })
  output$o.control_pred <- renderText( {
    paste("You have selected schooles with ", input$control_pred, " type.") 
  })
  
  #we need locale to remain an integer so the numbers close to each other denote regions that are geographically
  # close to each other
  output$o.region_pred <- renderText( {
   if(input$region_pred == 0) {
     paste("You have selected schools in US Service Schools region.")
  }else if(input$region_pred == 1) {
    paste("You have selected schools in New England region.")
  }else if(input$region_pred == 2) {
    paste("You have selected schools in Mid East region.")
  }else if(input$region_pred == 3) {
    paste("You have selected schools in Great Lakes region.")
  }else if(input$region_pred == 4) {
    paste("You have selected schools in Plains region.")
  }else if(input$region_pred == 5) {
    paste("You have selected schools in Southeast region.")
  }else if(input$region_pred == 6) {
    paste("You have selected schools in Sothwest region.")
  }else if(input$region_pred == 7) {
    paste("You have selected schools in Rocky Mountains region.")
  }else if(input$region_pred == 8) {
    paste("You have selected schools in Far West region.")
  }else if(input$region_pred == 9) {
    paste("You have selected schools in Outlying Areas region.")
  }
  })
  
 #we need locale to remain an integer so the numbers close to each other denote similar type of location
  output$o.locale_pred <- renderText( {
  if(input$locale_pred == 11) {
    paste("You have selected schools in City location.") 
  }else if(input$locale_pred == 21) {
    paste("You have selected schools in Suburb location.") 
  }else if(input$locale_pred == 31) {
    paste("You have selected schools in Town location.")
  }else if(input$locale_pred == 41) {
    paste("You have selected schools in Rural location.")
  }
  })
  
  output$o.cost_pred <- renderText( {
    paste("You have selected schooles with cost between $", input$cost_pred, ".") 
  })
  output$o.satread_pred <- renderText( {
    paste("SAT Reading Midpoint: ", input$satread_pred,".") 
  })
  output$o.satmath_pred <- renderText( {
    paste("SAT Math Midpoint:", input$satmath_pred,".") 
  })
  output$o.acteng_pred <- renderText( {
    paste("ACT English Midpoint: ", input$acteng_pred,".") 
  })
  output$o.actmath_pred <- renderText( {
    paste("ACT Math Midpoint: ", input$actmath_pred,".") 
  })
  output$o.womenonly_pred <- renderText( {
    paste("You have chosen women-only schools.") 
  })
  output$o.menonly_pred <- renderText( {
    paste("You have chosen men-only schools.") 
  })
  output$o.firstgen_pred <- renderText( {
    paste("You have chosen schools with at least 30% of first-generation students.") 
  })
  
  #filtering schools for the map
  #filter based on all checkboxes for locale
  data2016.filtered1 <- reactive ({
    filter(data2016_copy, data2016_copy$LOCALE %in% input$locale)  
  })
  #filter based on all checkboxes for region
  data2016.filtered2 <- reactive ({
    filter(data2016.filtered1(), data2016.filtered1()$REGION %in% input$region)  
  })
  #filter based on all checkboxes for control
  data2016.filtered3 <- reactive ({
    filter(data2016.filtered2(), data2016.filtered2()$CONTROL %in% input$control)  
  })
  
  #if women-only schools are selected, filter based on it
  data2016.filtered4 <-reactive ({
    using.data <- data2016.filtered3()
    if (input$womenonly == 1 && input$menonly == 0) {
      using.data <- filter(using.data, WOMENONLY == 1)
    }else if (input$womenonly == 0 && input$menonly == 1) {
      using.data <- filter(using.data, MENONLY == 1)
    }else if (input$womenonly == 1 && input$menonly == 1) {
      using.data <- filter(using.data, WOMENONLY == 1 | MENONLY == 1)
    }
    using.data
  })
  
  # if first-gen is selected, filter schools with more than 30% first-gen students
  data2016.filtered5 <-reactive ({
    using.data <- data2016.filtered4()
    if (input$firstgen == 1) {
      using.data <- filter(using.data, First_gen == 1)
    }
    using.data
  })

  
  data2016.filtered <- reactive({
    using.data <- data2016.filtered5()
    #is SAT is selected, further filter the data based on the SAT input scored
    if (input$sat_act == 1) {
      using.data <- filter(using.data, UGDS >= input$size[1] & UGDS <= input$size[2] &
                             COSTT4_A <= input$cost & SATMTMID <= input$satmath &
                             SATVRMID <= input$satread & SATWRMID <= input$satwrit)
    }
    #is ACT is selected, further filter the data based on the ACT input scored
    else if (input$sat_act == 2) {
      using.data <- data2016.filtered5()
      using.data <- filter(using.data,UGDS >= input$size[1] & UGDS <= input$size[2] &
                             COSTT4_A <= input$cost & ACTENMID <= input$acteng & 
                             ACTMTMID <= input$actmath & ACTWRMID <= input$actwrit)
    }
    using.data
  })
  
  
  
  
  #printing out the map
  output$map <- renderPlotly({
    #setting up colors, projection, and shapes for maps
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )  
    
    p <- plot_geo(data2016.filtered(), lat = ~LATITUDE, lon = ~LONGITUDE) %>%
      add_markers(
        #show the text for hovering
        text = ~paste(INSTNM,"<br />", "City: ", CITY, "<br />", "Region: ", REGION, "<br />", "Locale: ", LOCALE, 
                      "<br />","Control: ", CONTROL,"<br />", "Acceptance Rate: ", ADM_RATE * 100, "%", "<br />",
                      "School size: ", UGDS, "<br />", "Annual cost: $", COSTT4_A),
        #color the datapoints by admissions rate
        color = ~ADM_RATE, symbol = I("square"), hoverinfo = "text"
      ) %>%
      colorbar(title = "Admissions Rate") %>%
      layout(
        title = 'Schools with best fit<br />(Hover for the school information)', geo = g
      )
  })
  
  ################################# Clustering #################################################
  
  # Filter schools for clustering
  selectedData <- reactive({
    using.data <- cluster_data
    
    # Filter data based on user selection
    if (length(input$clustering_relaffil) != 0) {
      using.data <- using.data %>% subset(RELAFFIL %in% input$clustering_relaffil)
    }
    if (length(input$clustering_locale) != 0) {
      using.data <- using.data %>% subset(LOCALE %in% input$clustering_locale) 
    }
    if (length(input$clustering_region) != 0) {
      using.data <- using.data %>% subset(REGION %in% input$clustering_region) 
    }
    if (length(input$clustering_control) != 0) {
      using.data <- using.data %>% subset(CONTROL %in% input$clustering_control)
    }
    
    # If the filtered data does not have as many rows as the number of clusters, throw an informative error
    if (nrow(using.data) < input$clusters) {
      stop("Not enough data for the options you selected, select more or use fewer clusters!")
    }
    
    # Select just the numeric variables for clustering
    select(using.data, 6:12)
  })
  
  # Run PAM
  pm <- reactive({
    pam(scale(selectedData()), k = input$clusters)
  })
  
  # Show a clustering plot
  output$pamPlot <- renderPlot({
    # Match user selected variables to actual variable names
    xvar <- switch (input$xcol,
                    "Admission Rate" = "ADM_RATE",
                    "SAT Verbal Median" = "SATVRMID",
                    "SAT Maths Median" = "SATMTMID",
                    "ACT English Median" = "ACTENMID",
                    "ACT Maths Median" = "ACTMTMID",
                    "Enrollment of undergraduates" = "UGDS", 
                    "Cost of Attendance" = "COSTT4_A"
    )
    yvar <- switch (input$ycol,
                    "Admission Rate" = "ADM_RATE",
                    "SAT Verbal Median" = "SATVRMID",
                    "SAT Maths Median" = "SATMTMID",
                    "ACT English Median" = "ACTENMID",
                    "ACT Maths Median" = "ACTMTMID",
                    "Enrollment of undergraduates" = "UGDS", 
                    "Cost of Attendance" = "COSTT4_A"
    )
    
    if (input$pca) { # If the user chooses to use the first two principle components for clustering
      fviz_cluster(pm(), data = scale(selectedData())) + labs(title = "PAM Clustering")
    }else { # If the user wants to pick two variables of their choice for clustering
      fviz_cluster(pm(), data = scale(selectedData()), choose.vars = c(xvar, yvar)) + labs(title = "PAM Clustering")
    }
  }, height = 800, width = 1100)
  
  output$centroids <- renderTable({
    using.data <- selectedData()
    subset(cluster_data, rownames(cluster_data) %in% rownames(pm()$medoids)) # Filter data for the colleges that are cluster centers
  })
  
  output$dimensions <- renderTable({
    P <- prcomp(selectedData(), scale = TRUE)
    cbind(data.frame(Variable = rownames(P$rotation)), data.frame(P$rotation, row.names=NULL)) # Render PCA Analysis to main panel
  })
  
  ################################# Prediction #################################################
  
  # Filter data for prediction
  predictData <- reactive ({
    using.data <- data_pred
    
    # Add the user selected options as a new row into the dataframe
    if (input$sat_act_pred == 1) { # If the user selected SAT scores
      using.data <- select(using.data, INSTNM, CONTROL, REGION, LOCALE, SATVRMID, SATMTMID, UGDS, COSTT4_A, MENONLY, WOMENONLY, First_gen)
      user.df <- data.frame(INSTNM = "User", CONTROL = input$control_pred, 
                            REGION = input$region_pred, LOCALE = input$locale_pred, 
                            SATVRMID = input$satread_pred, SATMTMID = input$satmath_pred,
                            UGDS = input$size_pred[2], COSTT4_A = input$cost_pred[2],
                            MENONLY = input$menonly_pred, WOMENONLY = input$womenonly_pred,
                            First_gen = input$firstgen_pred)
      using.data <- rbind(using.data, user.df)
    }else { # If the user selected ACT scores
      using.data <- select(using.data, INSTNM, CONTROL, REGION, LOCALE, ACTENMID, ACTMTMID, UGDS, COSTT4_A,MENONLY, WOMENONLY, First_gen)
      user.df <- data.frame(INSTNM = "User", CONTROL = input$control_pred, 
                            REGION = input$region_pred, LOCALE = input$locale_pred, 
                            ACTENMID = input$acteng_pred, ACTMTMID = input$actmath_pred, 
                            UGDS = input$size_pred[2], COSTT4_A = input$cost_pred[2],
                            MENONLY = input$menonly_pred, WOMENONLY = input$womenonly_pred,
                            First_gen = input$firstgen_pred)
      using.data <- rbind(using.data, user.df)
    }
    
    # Return the filtered data
    using.data
  })
  
  getTopChoices <- reactive ({
    using.data <- predictData()
    
    D <- gower.dist(using.data) # Create a gower distance matrix 
    dissimilarity <- melt(as.matrix(D), varnames = c("row", "col")) # Turn the distance matrix into a dataframe
    dissimilarity <- dissimilarity[dissimilarity$row > dissimilarity$col,] # Only use the non-replicating pairs of dissimilarities
    
    user.index <- nrow(using.data) # Get the row number for user input
    distances <- subset(dissimilarity, row == user.index) # Get dissimilarity values for user input only
    distances <- distances[order(distances$value),] # Sort by dissimilarity value in ascending order
    
    distances[1:10,] # Get the top 10 choices
  })
  
  tableData <- reactive ({
    using.data <- predictData()[getTopChoices()$col,] # Search the dataset for the top 10 match colleges
    data.frame(Ranking = 1:10, Institution = using.data$INSTNM, "Percentage Of Fitness" = (1 -getTopChoices()$value) * 100) #0 distance is our new row, 1 is the furthest school, so 1 - distance presents the percentage of fitness 
  })
  
  output$college_matches <- renderTable(
    tableData() # Render the top 10 college choices to main panel
  )
  
  ################################# Historical Data #################################################
  output$lineplot <- renderPlot({
    # Get y variable from input
    if(input$var == "ADM_RATE") {
      yvarname <- "Admissions Rate"
    }else if(input$var == "UGDS") {
      yvarname <- "Size"
    }else if(input$var == "SATVRMID") {
      yvarname <- "Median Verbal SAT Score"
    }else if(input$var == "SATMTMID") {
      yvarname <- "Median Math SAT Score"
    }else if(input$var == "SATWRMID") {
      yvarname <- "Median Writing SAT Score"
    }else if(input$var == "ACTENMID") {
      yvarname <- "Median English ACT Score"
    }else if(input$var == "ACTMTMID") {
      yvarname <- "Median Math ACT Score"
    }else if(input$var == "ACTWRMID") {
      yvarname <- "Median Writing ACT Score"
    }else if(input$var == "COSTT4_A") {
      yvarname <- "Average Annual Cost of Attendance"
    }else if(input$var == "AVGFACSAL") {
      yvarname <- "Average Faculty Salary"
    }else if(input$var == "PFTFAC") {
      yvarname <- "Percent Full-Time Faculty"
    }else if(input$var == "PAR_ED_PCT_1STGEN") {
      yvarname <- "Percent First Generation Students"
    }
    
    # Filter years based on slider input
    yearsnames <- filter(data_final, INSTNM %in% input$schools & YEAR >= input$range[1] & YEAR <= input$range[2])
    # Filter dataset with user specified column
    yearsnames$yvar <- yearsnames[,colnames(yearsnames) == input$var]
    # Render plot
    ggplot(yearsnames, aes(x = YEAR , y = yvar, color = INSTNM)) + geom_point() + geom_line() + xlab("Year") + ylab(yvarname) + labs(color = "Institution Name")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
