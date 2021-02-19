library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tidyr)
library(tools)

setwd(dir = "C:/Users/shiri/Documents/Homework_1/Economic_indicators/hw1_sverma")
# Read the dataset
world.bank.df <- read.csv("world_bank_data.csv", check.names = FALSE, na.strings = "..")
colnames(world.bank.df)[1] <- "IndicatorName"

# Using pivot_longer and pivot_longer to get dataset in the desired format
world.bank.df <- world.bank.df %>%
    pivot_longer(
        cols = starts_with("20"),
        names_to = "Year",
        values_to = "value",
        values_drop_na = FALSE
    )

world.bank.df <- world.bank.df %>%
    pivot_wider(
        names_from = IndicatorName,
        values_from = "value"
    ) 

# Makes column names syntactically correct
names(world.bank.df) <- make.names(names(world.bank.df), unique = TRUE)

world.bank.df$Year <- as.numeric(as.character(world.bank.df$Year))

# Define UI for application that draws three plots
ui <- fluidPage(

    # Application title
    titlePanel("World Development Indicators"),

    # Sidebar layout with input and output definitions 
    sidebarLayout(
        
        # Inputs: Select variables to plot
        sidebarPanel(
            
            h4(strong("Plot 1 & 2 functionalities")),
            br(),
            
            # Select indicators for y axis of Plot 1 and 2
            selectInput(inputId = "y1",
                        label = "Y-axis:",
                        choices = colnames(world.bank.df)[5:50],
                        selected = "Population.growth..annual..."),
            
            # Select years for x-axis of Plot 1 and 2
            sliderInput(inputId = "x1", 
                  label = "X-axis:", 
                  min = min(world.bank.df$Year), 
                  max = max(world.bank.df$Year),
                  step = 1, 
                  value = c(max(world.bank.df$Year)-10,max(world.bank.df$Year))),
            
            # Horizontal line for visual separation
            hr(),
            
            # Select continents for plots 2 and 3 and data table
            checkboxGroupInput(inputId = "continent",
                               label = "Select continent(s)*:",
                               choices = unique(world.bank.df$ContinentName),
                               selected = "North America"),
            h6("* this filter is applicable everywhere except plot 1"),
            
            # Horizontal line for visual separation
            hr(),
            h4(strong("Plot 3 functionalities")),
            br(),
            
            # Select the country on focus for Plot 3
            uiOutput("selectCountry"),
            
            # Select an indicator for y axis of Plot 3
            selectInput(inputId = "y3",
                        label = "Y-axis:",
                        choices = colnames(world.bank.df)[5:50],
                        selected = "Energy.use..kg.of.oil.equivalent.per.capita."),
            
            # Select an indicator for x axis of Plot 3
            selectInput(inputId = "x3",
                        label = "X-axis:",
                        choices = colnames(world.bank.df)[5:50],
                        selected = "CO2.emissions..metric.tons.per.capita."),
            hr(),
            # Show data table
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            hr(),
            
            h5(strong("Download data table with all columns")),
            # Download datatable
            downloadButton(outputId = "download_dt",
                           label = "Download data")
            
        ),

        # Output
        mainPanel(
            # Show barplot
            plotOutput(outputId = "barplot"),
            br(),
            hr(),
            # show lineplot
            plotOutput(outputId = "lineplot"),
            br(),
            hr(),
            # show scatterplot
            plotOutput(outputId = "scatterplot"),
            br(),
            hr(),
            
            # Show data table
            DT::dataTableOutput(outputId = "table")
        )
    )
)

# Define server logic required to draw plots and tables
server <- function(input, output) {
    
    # Make a subset of aggregated values based on the idicator selected for Plot 1
    indicator_subset <- reactive({
        req(input$y1)
        world.bank.df %>% 
            group_by(ContinentName, Year) %>%
            summarise(Mean = mean(get(input$y1), na.rm = TRUE))
    })
    
    # Create a barplot making comparisons within continents on the selected indicator
    output$barplot <- renderPlot({
        ggplot(data = indicator_subset(), aes_string(x = "Year",
                                                     y = "Mean",
                                                     fill = "ContinentName")) +
            geom_bar(stat = "identity", width = 0.5, position = "dodge") +
            scale_x_continuous(breaks = seq(input$x1[1],input$x1[2],1),
                               limits = c(input$x1[1],input$x1[2])) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Year", y = paste("Indicator: ", input$y1), fill = "Continents",
                 title = paste0("Plot 1: Performance of continents on aggregate level from ",
                                input$x1[1], " to ", input$x1[2])) +
            theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))
    })
    
    # Create a subset of data filtering for selected continents
    continents_subset <- reactive({
        req(input$continent)
        filter(world.bank.df, ContinentName %in% input$continent)
    })
    
    # Create a lineplot between some development indicator and years
    output$lineplot <- renderPlot({
        ggplot(data = continents_subset(), aes_string(x = "Year", 
                                               y = input$y1,
                                               color = "CountryName")) +
            geom_line() +
            scale_x_continuous(breaks = seq(input$x1[1],input$x1[2],1), 
                               limits = c(input$x1[1],input$x1[2])) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Year", y = paste("Indicator: ", input$y1), color = "Countries", 
            title = paste0("Plot 2: Trend in countries indicators from ", 
                           input$x1[1], " to ", input$x1[2])) +
            theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))
    })
    
    # Crete an output UI which can show filtered country names in dropdown
    output$selectCountry <- renderUI(
        selectInput(inputId = "country",
                 label = "Select a country:",
                 choices = unique(continents_subset()$CountryName),
                 selected = "United States")
    )
    
    # Create a subset of data filtering for selected continents
    country_subset <- reactive({
        req(input$country)
        filter(world.bank.df, CountryName %in% input$country)
    })
    
    # Create a scatterplot between 2 development indicators for a selected country
    output$scatterplot <- renderPlot({
        ggplot(data = country_subset(), aes_string(x = input$x3, y = input$y3, label = "Year")) +
            geom_point(size = 5,color = "orange") +
            geom_text(aes(label = Year), hjust = 0, vjust = -1) +
            labs(x = input$x3, y = input$y3, 
                 title = paste0("Plot 3: Comparison between two indicators between 2000 and 2015 for ", input$country)) +
            theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))
    })
    
    # Print data table if checked
    output$table <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = continents_subset()[,1:10], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
    
    # Download the data in data table
    output$download_dt <- downloadHandler(
        filename = function(){"hw1_shirishv.csv"},
        content = function(fname){
            write.csv(continents_subset(), fname)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
