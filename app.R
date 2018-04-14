#List of required packages: COMMENT and RUN after first time execution
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("treemap")
# install_github("timelyportfolio/d3treeR")
# install.packages("Rcpp")
# install.packages(ggplot2)
# install.packages("data.table")
# install.packages("igraph")
# install.packages("stringi")
# install.packages("viridisLite")
# install.packages("geofacet")
# install.packages("DT")
# install.packages("stringr")


# Loading the required libraries (post installation)
require(devtools)
require(dplyr)
require(shiny)
require(shinydashboard)
require(igraph)
require(stringi)
require(viridisLite)
require(treemap)
require(d3treeR)
require(geofacet)
require(leaflet)
require(plyr)
require(geojsonio)
require(rgdal)
require(ggplot2)
require(plotly)
require(DT)
require(stringr)
options(scipen=999)

# Importing all datasets
cleaned_transactions = read.csv("Datasets/Cleaned Transactions.csv")
cleaned_transactions_buildings = read.csv("Datasets/Cleaned Transactions - Buildings.csv")
predictor_variables = subset(read.csv("Datasets/Regression Predictors.csv"), select = -c(X))
singapore_regions <- geojsonio::geojson_read("Datasets/sg-regions.geojson", what = "sp")
singapore_districts <- geojsonio::geojson_read("Datasets/sg-districts.geojson", what = "sp")


#Convert numerical values to nominal where necessary
cleaned_transactions$Month <- as.factor(cleaned_transactions$Month)
cleaned_transactions$Time.Quarter <- as.factor(cleaned_transactions$Time.Quarter)
cleaned_transactions$Postal.District <- as.factor(cleaned_transactions$Postal.District)
cleaned_transactions$Tenure_Bin <- as.factor(cleaned_transactions$Tenure_Bin)
cleaned_transactions$Security <- as.factor(cleaned_transactions$Security)
cleaned_transactions$Carpark <- as.factor(cleaned_transactions$Carpark)
cleaned_transactions$Entertainment <- as.factor(cleaned_transactions$Entertainment)
cleaned_transactions$Chill.Out <- as.factor(cleaned_transactions$Chill.Out)
cleaned_transactions$Food.Dining <- as.factor(cleaned_transactions$Food.Dining)
cleaned_transactions$Family.Friendly <- as.factor(cleaned_transactions$Family.Friendly)
cleaned_transactions$Fitness <- as.factor(cleaned_transactions$Fitness)
cleaned_transactions$Function.Room <- as.factor(cleaned_transactions$Function.Room)
cleaned_transactions$Year <- as.factor(cleaned_transactions$Year)

cleaned_transactions_buildings$Month <- as.factor(cleaned_transactions_buildings$Month)
cleaned_transactions_buildings$Time.Quarter <- as.factor(cleaned_transactions_buildings$Time.Quarter)
cleaned_transactions_buildings$Postal.District <- as.factor(cleaned_transactions_buildings$Postal.District)
cleaned_transactions_buildings$Tenure_Bin <- as.factor(cleaned_transactions_buildings$Tenure_Bin)
cleaned_transactions_buildings$Security <- as.factor(cleaned_transactions_buildings$Security)
cleaned_transactions_buildings$Carpark <- as.factor(cleaned_transactions_buildings$Carpark)
cleaned_transactions_buildings$Entertainment <- as.factor(cleaned_transactions_buildings$Entertainment)
cleaned_transactions_buildings$Chill.Out <- as.factor(cleaned_transactions_buildings$Chill.Out)
cleaned_transactions_buildings$Food.Dining <- as.factor(cleaned_transactions_buildings$Food.Dining)
cleaned_transactions_buildings$Family.Friendly <- as.factor(cleaned_transactions_buildings$Family.Friendly)
cleaned_transactions_buildings$Fitness <- as.factor(cleaned_transactions_buildings$Fitness)
cleaned_transactions_buildings$Function.Room <- as.factor(cleaned_transactions_buildings$Function.Room)
cleaned_transactions_buildings$Year <- as.factor(cleaned_transactions_buildings$Year)


# Building the Regression Model
fit_model = lm(TransactionPricePerUnit ~ ., data=predictor_variables)
fit_df = as.data.frame(predict(fit_model, level=0.05,interval = "prediction"))

# Geofacet Data Preparation
geofacet.grid <- data.frame(
  row = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
  col = c(3, 4, 2, 4, 5, 7, 6, 5, 4, 3, 3, 4, 5, 6, 7, 2, 1, 2, 5, 4, 3, 6, 6, 3, 4, 5, 4, 3),
  code = c("25", "27", "24", "26", "28", "17", "18", "19", "20", "23", "11", "12", "13", "14", "16", "21", "22", "05", "08", "09", "10", "15", "01", "03", "06", "07", "02", "04"),
  name = c("Kranji", "Yishun", "Lim Chu Kang", "Upper Thomson", "Seletar", "Loyang", "Tampines", "Serangoon Garden", "Bishan", "Hillview", "Watten Estate", "Balestier", "Macpherson", "Geylang", "Bedok", "Upper Bukit Timah", "Jurong", "Pasir Panjang", "Little India", "Orchard", "Ardmore", "Katong", "Raffles Place", "Queenstown", "High Street", "Middle Road", "Anson", "Telok Blangah"),
  stringsAsFactors = FALSE
)

geofacet_data = cleaned_transactions[,c("Postal.District","Year","Time.Quarter","TransactionPricePerUnit")]
trim.trailing <- function (x) sub("\\s+$", "", x)
geofacet_data[,'Postal.District']<-trim.trailing(geofacet_data$Postal.District)
geofacet_data = aggregate(.~Postal.District+Year+Time.Quarter, geofacet_data, mean)
geofacet_data$Year <- as.numeric(geofacet_data$Year)
geofacet_data$Time.Quarter <- as.numeric(geofacet_data$Time.Quarter)
geofacet_data$DateRep = as.numeric(as.factor(geofacet_data$Year*100+geofacet_data$Time.Quarter))
colnames(geofacet_data)[1] <- "name"


#--------------------------------------------Application Code-------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Applied 'R'eal Estate",
    titleWidth = 800
  ),
  dashboardSidebar(
    sidebarMenu(width = 150,
                menuItem("Market Overview", icon = icon("home"), tabName = "tab_overview"),
                menuItem("Property Price Analysis", icon = icon("codepen"), tabName = "tab_target"),
                menuItem("Descriptive Analysis", icon = icon("cogs"), tabName = "tab_descriptive"),
                menuItem("Building Summary Statistics", icon = icon("line-chart"), tabName = "tab_summarystats"),
                menuItem("Price Estimator", icon = icon("table"), tabName = "tab_inferential")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        # Tab 1: Market Overview
        tabItem("tab_overview",
                mainPanel(
                  "Browse and filter on any area of Singapore to analyze Property transactions between 2013-2015.", 
                  width = 12, 
                  style = "font-size:16px;text-align:center;font-weight:bold;"
                ),
                fluidRow(
                  style="padding-top:40px;",
                  column(
                    width = 10,
                    leafletOutput("op_Leaflet", height = "300px")  
                  ),
                  column(
                    width = 2,
                    radioButtons("SegmentType", "Segment by:",
                                 choices = c("Planning Region", "Postal District"),
                                 selected = "Planning Region", inline = FALSE),
                    radioButtons("MeasureType", "View by:",
                                 choices = c("Transactions", "Average Price"),
                                 selected = "Transactions", inline = FALSE),
                    actionButton("leafletReset", "Reset Selection"),
                    textOutput("leafletSelection")
                  )
                  
                ),
                fluidRow(
                  style="padding-top:25px;text-align:center;",
                  box(
                    width=6,
                    solidHeader = FALSE,
                    title='Click on an area to analyze Type of Sale',
                    plotlyOutput("typeOfSalePie", height = "220px")
                  ),
                  box(
                    width=6,
                    solidHeader = TRUE,
                    title='Click on an area to analyze Property Type',
                    plotlyOutput("propertyTypePie", height = "220px")
                  )
                )
        ),
        # Tab 2: Property Price Analysis
        tabItem("tab_target",
                fluidRow(
                  style="text-align:center;",
                  box (
                    width=12,
                    solidHeader = TRUE,
                    height=500,
                    title = "Change in Average Transaction Price per Property Unit (per quarter 2013 - 2015)",
                    plotOutput("op_GeofacetPlot")
                  )
                )
        ),
        # Tab 3: Descriptive Analysis
        tabItem("tab_descriptive",
                mainPanel(
                  "Explore the variables related to each Property unit for Univariate and Bivariate Analysis", 
                  width = 12, 
                  style = "font-size:16px;text-align:center;font-weight:bold;"
                ),
                fluidRow(
                  column(
                    width = 12,
                    height = 40,
                    selectInput("ddlVariables", label = "Select an Input Variable for Analysis", choices = 
                                  sort(colnames(subset(cleaned_transactions, select = -c(X, Project.Name, Latitude, Longitude))))
                    )
                  )
                ),
                fluidRow(
                  box(
                    width =6,
                    solidHeader = FALSE,
                    title = 'Univariate Analysis',
                    plotlyOutput("op_Univariate", height = "300px"),
                    sliderInput("binrange", "Choose the Bins", min = 1, max = 30, value = 5)
                  ),
                  box(
                    width =6,
                    solidHeader = FALSE,
                    title = 'Bivariate Analysis',
                    plotOutput("op_Bivariate")
                  )
                )
        ),
        # Tab 4: Building Summary Statistics
        tabItem("tab_summarystats",
                fluidRow(
                  width=12,
                  mainPanel("Explore the treemap below to see building-level transaction summaries from 2013 - 2015", 
                            d3tree3Output("op_Treemap"), 
                            width=12,
                            style="font-size:16px;text-align:center;font-weight:bold;")
                ),
                fluidRow(
                  width=12,
                  solidHeader = TRUE,
                  DT::dataTableOutput("op_summaryTable")
                )
        ),
        # Tab 5: Price Estimator
        tabItem("tab_inferential",
                mainPanel(
                  "Price Estimator: Fill in the details of your dream house to estimate its Sales Price",
                  width = 12,
                  style = "font-size:16px;text-align:center;font-weight:bold;padding-bottom:15px;"
                ),
                fluidRow(
                  width = 12,
                  style="font-size:11px;",
                  column(
                    width=6,
                    fluidRow(
                      style="margin-left:5px;",
                      width = 12,
                      column(
                        width = 4,
                        numericInput("ip_FormAge", "Building Age:",min=0, max=50, value=25, width="75%")
                      ),
                      column(
                        width = 4,
                        numericInput("ip_FormArea", "Area (sqm):", min=0, max=350, value=175, step=10, width="75%")
                      ),
                      column(
                        width = 4,
                        selectInput("ip_FormTenureBin",label = "Tenure Bin:",
                                    choices = c("99 years","> 99 years"),multiple = FALSE, width="75%")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        title="Demographic Preferences:",
                        fluidRow(
                          column(
                            width=4,
                            sliderInput("ip_FormSeniorPopulation", "Senior Citizen Population:", min = 0, max = 20, post="%",value = 0)
                          ),
                          column(
                            width=4,
                            sliderInput("ip_FormTenants", "Occupied Tenants:", min = 0, max = 100, post="%",value = 0)
                          ),
                          column(
                            width=4,
                            sliderInput("ip_FormUniversity", "University Qualifications:", min = 0, max = 100, post="%",value = 0)
                          )
                        ),
                        fluidRow(
                          column(
                            width=6,
                            sliderInput("ip_FormCondoRes", "Condiminium Residents:", min = 0, max = 100,  post="%",value = 0)
                          ),
                          column(
                            width=6,
                            sliderInput("ip_FormIncome", "Monthly Income (>$8000):", min = 0, max = 100,  post="%",value = 0)
                          )
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        title="Geographic Preferences (Distance from):",
                        fluidRow(
                          column(
                            width=3,
                            numericInput("ip_FormURA", "URA Growth Area:", 5, min=0, max=10)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormHawker", "Hawker Center:", 3, min=0, max=6)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormMrt", "MRT Station:", 2, min=0, max=4)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormPark", "Park:", 1, min=0, max=3)
                          )
                        ),
                        fluidRow(
                          column(
                            width=3,
                            numericInput("ip_FormSchool", "Primary School:", 4, min=0, max=8)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormMall", "Shopping Mall:", 2, min=0, max=4)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormBusStop", "Bus Stop:", 1, min=0, max=3)
                          ),
                          column(
                            width=3,
                            numericInput("ip_FormBusInterchange", "Bus Interchange:", 2, min=0, max=5)
                          )
                        ),
                        box(
                          width=12,
                          fluidRow(
                            width=12,
                            column(
                              width=7,
                              checkboxGroupInput("ip_FormFacilities", "Building Facilities:",
                                                 c("Car Park" = "Carpark","Break Room" = "Breakroom"), inline = TRUE)
                            ),
                            column(
                              style="text-align:right;",
                              width=5,
                              actionButton("submit", "Submit",
                                           style="background-color:#065B3F;color:White;margin-top:5px;")   
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width=6,
                    fluidRow(
                      htmlOutput("op_RegressionPrice")  
                    ),
                    fluidRow(
                      style="padding-top:10px;",
                      width=12,
                      solidHeader = TRUE,
                      DT::dataTableOutput("op_RegressionTable")
                    )
                  )
                )
        )
   )
 )
)




server <- function( input, output, session ){

  # Server Code for Tab 1: Leaflet Heatmap and filter selection on Pie charts
  
  # Leaflet Plot generation based on input filter selection
  output$op_Leaflet <- renderLeaflet({
    count_transaction = sp::merge(singapore_regions, count(cleaned_transactions, "Planning.Region"), by="Planning.Region")
    price_transaction = sp::merge(singapore_regions, ddply(cleaned_transactions, ~Planning.Region, summarise,  price=mean(TransactionPricePerUnit)),
                                  by="Planning.Region")
    count_postal_district = count(cleaned_transactions, "Postal.District")
    count_postal_district$Postal.District <- str_trim(count_postal_district$Postal.District)
    count_postal_district_price = ddply(cleaned_transactions, ~Postal.District, summarise, price=mean(TransactionPricePerUnit))
    count_postal_district_price$Postal.District <- str_trim(count_postal_district_price$Postal.District)
    count_transaction_district = sp::merge(singapore_districts, count_postal_district, by="Postal.District")
    price_transaction_district = sp::merge(singapore_districts, count_postal_district_price, by="Postal.District")
    pal = pal <- colorNumeric("Reds", NULL)#colorQuantile("Reds", NULL, n = nrow(count(cleaned_transactions, "Planning.Region")))
    
    segmentType = input$SegmentType
    measureType = input$MeasureType
    
    if(measureType == "Transactions") {
      if(segmentType == "Planning Region") {
        leaflet(count_transaction) %>%
          setView(lng = 103.82092416286469, lat = 1.3552063964811834, zoom = 10) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          addPolygons(
            stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal(as.numeric(freq)),
            label = ~paste0(Planning.Region, ": ", freq),
            highlight = highlightOptions(weight = 1, fillColor = "white")
          ) %>%
          addLegend("topright", pal = pal, opacity = 1, title = "No of Transactions",
                    values = count_transaction$freq)
      }
      else {
        leaflet(count_transaction_district) %>%
          setView(lng = 103.82092416286469, lat = 1.3552063964811834, zoom = 11) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          addPolygons(
            stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal(as.numeric(freq)),
            label = ~paste0(Postal.District, ": ", freq),
            highlight = highlightOptions(weight = 1, fillColor = "white")
          ) %>%
          addLegend("topright", pal = pal, opacity = 1, title = "No of Transactions",
                    values = count_transaction_district$freq)
      }
    }
    else { 
      if(segmentType == "Planning Region") {
        leaflet(price_transaction) %>%
          setView(lng = 103.82092416286469, lat = 1.3552063964811834, zoom = 10) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          addPolygons(
            stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal(as.numeric(price)),
            label = ~paste0(Planning.Region, ": $", price),
            highlight = highlightOptions(weight = 1, fillColor = "white")
          ) %>%
          addLegend("topright", pal = pal, opacity = 1, title = "Mean Price",
                    values = price_transaction$price)
      }
      else {
        leaflet(price_transaction_district) %>%
          setView(lng = 103.82092416286469, lat = 1.3552063964811834, zoom = 11) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          addPolygons(
            stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal(as.numeric(price)),
            label = ~paste0(Postal.District, ": $", round(as.numeric(price,2))),
            highlight = highlightOptions(weight = 1, fillColor = "white")
          ) %>%
          addLegend("topright", pal = pal, opacity = 1, title = "Mean Price",
                    values = price_transaction_district$price)
      }
    }
  })
  
  # Observing the Reset button click to reset the Pie charts
  observeEvent(input$leafletReset, {
    output$typeOfSalePie <- renderPlotly({
      slices <- count(cleaned_transactions, "Type.of.Sale")
      p <- plot_ly(slices, labels = ~Type.of.Sale, values = ~freq, type = 'pie')
      p$elementId <- NULL
      p
    })
    
    output$propertyTypePie <- renderPlotly({
      slices <- count(cleaned_transactions, "Property.Type")
      p <- plot_ly(slices, labels = ~Property.Type, values = ~freq, type = 'pie')
      p$elementId <- NULL
      p
    })
    
    output$leafletSelection <- renderText({ 
      "Selected: \n\n All"
    })
  })
  
  # Observing the Reset click event on the Leaflet Plot to filter the Pie charts
  observeEvent(input$op_Leaflet_shape_click, {
    click <- input$op_Leaflet_shape_click
    
    if(is.null(click))
      return()   
    
    #pulls lat and lon from shiny click event
    lat <- click$lat
    lon <- click$lng
    
    #puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(lon, lat))
    
    segmentType = input$SegmentType
    
    sp::coordinates(coords) <- ~lon+lat
    if(segmentType == "Planning Region") {
      sp::proj4string(coords) <- sp::proj4string(singapore_regions)
      matchingTable <- sp::over(coords, singapore_regions)
      selected <- as.character(matchingTable$Planning.Region)
      filtered_transactions <- cleaned_transactions[cleaned_transactions$Planning.Region == selected, ]
    }
    else {
      sp::proj4string(coords) <- sp::proj4string(singapore_districts)
      matchingTable <- sp::over(coords, singapore_districts)
      selected <- as.character(matchingTable$Postal.District)
      trimed_transactions = cleaned_transactions
      trimed_transactions$Postal.District <- str_trim(trimed_transactions$Postal.District)
      filtered_transactions <- trimed_transactions[trimed_transactions$Postal.District == selected, ]
    }
    
    # Rendering pie chart plots based on selection of leaflet polygon
    output$typeOfSalePie <- renderPlotly({
      slices <- count(filtered_transactions, "Type.of.Sale")
      p <- plot_ly(slices, labels = ~Type.of.Sale, values = ~freq, type = 'pie') 
      p$elementId <- NULL
      p
    })
    
    output$propertyTypePie <- renderPlotly({
      slices <- count(filtered_transactions, "Property.Type")
      p <- plot_ly(slices, labels = ~Property.Type, values = ~freq, type = 'pie')
      p$elementId <- NULL
      p
    })
    
    output$leafletSelection <- renderText({ 
      paste("Selected: \n\n", selected)
    })
  })
  
  # Rendering default Pie Charts
  output$typeOfSalePie <- renderPlotly({
    slices <- count(cleaned_transactions, "Type.of.Sale")
    p <- plot_ly(slices, labels = ~Type.of.Sale, values = ~freq, type = 'pie')
    p$elementId <- NULL
    p
  })
  
  output$propertyTypePie <- renderPlotly({
    slices <- count(cleaned_transactions, "Property.Type")
    p <- plot_ly(slices, labels = ~Property.Type, values = ~freq, type = 'pie')
    p$elementId <- NULL
    p
  })
  
  output$leafletSelection <- renderText({ 
    "Selected: \n\n All"
  })
  
  
  
  ### Server Code for Tab 2 - Geofacet Plot for Target Variable
  
  # Rendering the Geofacet plot based on transaction price per unit
  output$op_GeofacetPlot <- renderPlot({
    ggplot(geofacet_data, aes(DateRep, TransactionPricePerUnit)) +
      scale_y_continuous(name="Average Transaction Price Per Unit", labels = scales::comma) +
      geom_line() +
      facet_geo(~ name, grid = geofacet.grid, label = "name") +
      labs(x = "Time (Quarter-Year)") +
      theme(strip.text.x = element_text(size = 8, colour="white"), axis.text.x=element_blank(), axis.ticks.x=element_blank(), strip.background = element_rect(fill="forestgreen"))
  }, height = 550)
  
  
  
  ### Server Code for Tab 3 - Univariate, Bivariate and Multivariate Plots
  
  # Conditional display of Histogram based on the data type of the selected variable
  output$op_Univariate <- renderPlotly({
    if(class(cleaned_transactions[,input$ddlVariables]) == "factor")
    {	
      graph <- ggplot(cleaned_transactions, aes(cleaned_transactions[,input$ddlVariables])) + 
        geom_histogram(col="black", 
                       stat = "count",
                       bins = input$binrange, 
                       alpha = .2,
                       aes(fill=..count..)) + 
        labs(title = input$ddlVariables, x = "") +
        theme(panel.background = element_blank(),
              axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
      
    }
    else 
    {
      graph <- ggplot(cleaned_transactions, aes(cleaned_transactions[,input$ddlVariables])) + 
        geom_histogram(col="black",
                       stat = "bin",
                       bins = input$binrange, 
                       alpha = .2,
                       aes(fill=..count..)) + 
        labs(title = input$ddlVariables, x = "") +
        theme(panel.background = element_blank())
      
      
    }
    ggplotly(graph, tooltip=c("y"))
  })
  
  # Conditional display of Scatter/Boxplot based on the data type of the selected variable
  output$op_Bivariate <- renderPlot({
    if(class(cleaned_transactions[,input$ddlVariables]) == "factor")
    {
      
      ggplot(cleaned_transactions, aes( y = cleaned_transactions$TransactionPricePerUnit,
                                        x = cleaned_transactions[,input$ddlVariables]  )) + 
        geom_boxplot(fill = "white", colour = "#3366FF",
                     outlier.colour = "black", outlier.shape = 1)+
        labs(title = input$ddlVariables, x = "",y = "Transacted Price") +
        theme(panel.background = element_blank(),
              axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    }
    else
    {
      ggplot(cleaned_transactions, aes( y = cleaned_transactions$TransactionPricePerUnit,
                                        x = cleaned_transactions[,input$ddlVariables]  )) + 
        geom_point(size = 0.5,  color = "blue" ) +
        geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95, color = "darkred")+
        theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))+
        labs(title = input$ddlVariables, x = "", y="Transacted Price") 
    }
  })


  
  ### Server Code for Tab 4 - Treemap filter for Data Table
  
  # Rendering the geographical treemap from the building level dataset
  output$op_Treemap <- renderD3tree3({
    rendered.Treemap <- treemap(
      cleaned_transactions_buildings,
      index=c("Planning.Region","Postal.District","Project.Name"),
      vSize="TransactionPricePerUnit",
      vColor="TransactionPricePerUnit",
      type="value",
      palette="Greens",
      fun.aggregate = "mean",
      position.legend = "none"
    )
    d3tree3(rendered.Treemap,rootname = "Singapore")
  })
  
  # Creating the data table to display summary statistics
  transaction_building_data <- cleaned_transactions
  trans_price <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$TransactionPricePerUnit)), check.names = FALSE, stringsAsFactors = FALSE)))
  row.names(trans_price) <- "TransactionPricePerUnit"
  area_sqm <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$Area.Sqm.Unit)), check.names = FALSE, stringsAsFactors = FALSE)))
  row.names(area_sqm) <- "Area.Sqm.Unit"
  number_transactions <- as.data.frame(t(data.frame(unclass(summary(count(transaction_building_data, c('Project.Name'))$freq)), check.names = FALSE, stringsAsFactors = FALSE)))
  row.names(number_transactions) <- "NumberOfTransactions"
  summary_table <- rbind(trans_price,area_sqm,number_transactions)
  is.num <- sapply(summary_table, is.numeric)
  summary_table[is.num] <- lapply(summary_table[is.num], round, 2)
  
  # Rendering the default data table
  output$op_summaryTable = DT::renderDataTable({
    summary_table
  }, options = list(pageLength=5,ordering=F,paging=F))
  
  # Refreshing the summary data table based on click event captured from the Treemap at any level - Region, Planning Area or Building level
  treemap.Click <- reactiveValues(msg = "")
  observeEvent(input$op_Treemap_click, {
    planning_region_lookup = list(unique(cleaned_transactions_buildings$Planning.Region))
    postal_districts_lookup = list("Kranji", "Yishun", "Lim Chu Kang", "Upper Thomson", "Seletar", "Loyang", 
                                   "Tampines", "Serangoon Garden", "Bishan", "Hillview", "Watten Estate", "Balestier", 
                                   "Macpherson", "Geylang", "Bedok", "Upper Bukit Timah", "Jurong", "Pasir Panjang", 
                                   "Little India", "Orchard", "Ardmore", "Katong", "Raffles Place", "Queenstown", 
                                   "High Street", "Middle Road", "Anson", "Telok Blangah")
    
    treemap.Click$msg <- as.character(input$op_Treemap_click$name)
    transaction_building_data = cleaned_transactions_buildings
    
    # Level 1 - Singapore
    if(str_trim(treemap.Click$msg) == "Singapore") {
      transaction_building_data <- cleaned_transactions
      trans_price <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$TransactionPricePerUnit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(trans_price) <- "TransactionPricePerUnit"
      area_sqm <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$Area.Sqm.Unit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(area_sqm) <- "Area.Sqm.Unit"
      number_transactions <- as.data.frame(t(data.frame(unclass(summary(count(transaction_building_data, c('Project.Name'))$freq)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(number_transactions) <- "NumberOfTransactions"
      summary_table <- rbind(trans_price,area_sqm,number_transactions)
      is.num <- sapply(summary_table, is.numeric)
      summary_table[is.num] <- lapply(summary_table[is.num], round, 2)
      transaction_building_data <- summary_table
    }
    # Level 2 - Selection of a planning region
    else if(treemap.Click$msg %in% unique(cleaned_transactions_buildings$Planning.Region)){
      transaction_building_data <- cleaned_transactions[cleaned_transactions$Planning.Region == treemap.Click$msg,]
      trans_price <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$TransactionPricePerUnit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(trans_price) <- "TransactionPricePerUnit"
      area_sqm <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$Area.Sqm.Unit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(area_sqm) <- "Area.Sqm.Unit"
      number_transactions <- as.data.frame(t(data.frame(unclass(summary(count(transaction_building_data, c('Project.Name'))$freq)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(number_transactions) <- "NumberOfTransactions"
      summary_table <- rbind(trans_price,area_sqm,number_transactions)
      is.num <- sapply(summary_table, is.numeric)
      summary_table[is.num] <- lapply(summary_table[is.num], round, 2)
      transaction_building_data <- summary_table
    }
    # Level 3 - Selection of a Planning Area (postal district)
    else if(str_trim(treemap.Click$msg) %in% postal_districts_lookup){
      transaction_building_data <- cleaned_transactions[cleaned_transactions$Postal.District == treemap.Click$msg,]
      trans_price <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$TransactionPricePerUnit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(trans_price) <- "TransactionPricePerUnit"
      area_sqm <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$Area.Sqm.Unit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(area_sqm) <- "Area.Sqm.Unit"
      number_transactions <- as.data.frame(t(data.frame(unclass(summary(count(transaction_building_data, c('Project.Name'))$freq)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(number_transactions) <- "NumberOfTransactions"
      summary_table <- rbind(trans_price,area_sqm,number_transactions)
      is.num <- sapply(summary_table, is.numeric)
      summary_table[is.num] <- lapply(summary_table[is.num], round, 2)
      transaction_building_data <- summary_table
    }
    # Level 4 - Selection of a building name
    else {
      transaction_building_data <- cleaned_transactions[cleaned_transactions$Project.Name == treemap.Click$msg,]
      trans_price <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$TransactionPricePerUnit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(trans_price) <- "TransactionPricePerUnit"
      area_sqm <- as.data.frame(t(data.frame(unclass(summary(transaction_building_data$Area.Sqm.Unit)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(area_sqm) <- "Area.Sqm.Unit"
      number_transactions <- as.data.frame(t(data.frame(unclass(summary(count(transaction_building_data, c('Project.Name'))$freq)), check.names = FALSE, stringsAsFactors = FALSE)))
      row.names(number_transactions) <- "NumberOfTransactions"
      summary_table <- rbind(trans_price,area_sqm,number_transactions)
      is.num <- sapply(summary_table, is.numeric)
      summary_table[is.num] <- lapply(summary_table[is.num], round, 2)
      transaction_building_data <- summary_table
    }
    # Rendering the Summary statistics data table
    output$op_summaryTable = DT::renderDataTable({
      transaction_building_data
    }, options = list(pageLength=5,ordering=F,paging=F))
  })
  
  
  ### Server Code for Tab 5: user Interface for Regression
  
  # Running the lm model on click of submit button to predict the transaction price per unit based on input variables
  predicted.price <- eventReactive(input$submit,
                                   {   
                                     # Code to retrieve binary value of car park and break room from checkboxes in the UI
                                     carpark = 0
                                     breakroom = 0
                                     tryCatch({
                                       if (str_trim(input$ip_FormFacilities) == "Carpark Breakroom")
                                       {
                                         carpark = 1
                                         breakroom = 1
                                       }
                                       if (str_trim(input$ip_FormFacilities) == "Breakroom")
                                       {
                                         carpark = 0
                                         breakroom = 1
                                       }
                                       if (str_trim(input$ip_FormFacilities) == "Carpark")
                                       {
                                         carpark = 1
                                         breakroom = 0
                                       }
                                     }, error = function(e) {
                                       carpark = 0
                                       breakroom = 0
                                     })
                                     
                                     # Code to retrieve the selected tenure bin value from the dropdown list
                                     tenure.bin = 1
                                     if(str_trim(input$ip_FormTenureBin) == "99 years")
                                     {
                                       tenure.bin = 1
                                     }
                                     if(str_trim(input$ip_FormTenureBin) == "> 99 years")
                                     {
                                       tenure.bin = 2
                                     }
                                     
                                     # Create input vector for regression
                                     input_vector = t(data.frame(c(Year=2018, Carpark=carpark,	Chill.Out=breakroom,	Age=as.numeric(input$ip_FormAge),
                                                                   Tenure_Bin = tenure.bin, Age.65.and.above.of.total.residents=as.numeric(input$ip_FormSeniorPopulation)/100,
                                                                   Resident.in.Condominiums.and.Other.Apartments.of.total.residents = as.numeric(input$ip_FormCondoRes)/100,
                                                                   Monthly.Household.Income.8000.and.above.of.Total.Households = as.numeric(input$ip_FormIncome)/100,
                                                                   Tenants.of.total.households = as.numeric(input$ip_FormTenants)/100,
                                                                   Uni.Qualification.of.total = as.numeric(input$ip_FormUniversity)/100,
                                                                   URA.growth.area.km = as.numeric(input$ip_FormURA),	hawker.centre.and.market.km = as.numeric(input$ip_FormHawker),
                                                                   MRT.station.km = as.numeric(input$ip_FormMrt),	park.km = as.numeric(input$ip_FormPark),
                                                                   top.primary.school.km = as.numeric(input$ip_FormSchool),	shopping.mall.km = as.numeric(input$ip_FormMall),
                                                                   bus.stop.km = as.numeric(input$ip_FormBusStop),	bus.interchange.km = as.numeric(input$ip_FormBusInterchange),
                                                                   Area.Sqm.Unit = as.numeric(input$ip_FormArea))))
                                     rownames(input_vector) = NULL
                                     
                                     # Return the predicted price
                                     round(as.numeric(predict.lm(fit_model,as.data.frame(input_vector), level = 0.95)),2)
                                   }
  )
  
  # Displaying the predicted price with text formatting
  output$op_RegressionPrice <- renderUI({ 
    HTML(paste0("<p style='font-size:16px;'> Based on your inputs, we estimate the price of your dream house to be:</p>","
                <span style='font-size:16px;color:green;margin-left:30%'>$ ",
                formatC(as.numeric(predicted.price()), format="f", digits=2, big.mark=","),
                "</span><br/><br/><br/><div style='font-size:16px;'>Here are some buildings that you may be interested in:</div>"))
  })
  
  # Displaying the data table of property listings based on 95% confidence level range on the predicted price
  output$op_RegressionTable <- DT::renderDataTable({
    building_output = cleaned_transactions_buildings
    building_output['prediction'] = fit_df$fit
    building_output['lower.limit'] = fit_df$lwr
    building_output['upper.limit'] = fit_df$upr
    
    building_output = unique(building_output[as.numeric(predicted.price()) > building_output$lower.limit &
                                               as.numeric(predicted.price()) < building_output$upper.limit,
                                             c("Project.Name", "Postal.District", "Planning.Region")])
    colnames(building_output) = c("Building", "Area","Planning Region")
    building_output
  }, rownames = FALSE)
}

shinyApp( ui, server )