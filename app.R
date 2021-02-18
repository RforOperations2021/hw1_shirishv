#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tidyr)
library(tools)

# Define UI for application that draws three plots
ui <- fluidPage(

    # Application title
    titlePanel("World Development Indicators"),

    # Sidebar layout with input and output definitions 
    sidebarLayout(
        
        # Inputs: Select variables to plot
        sidebarPanel(
            
            # Select indicators for y axis
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = colnames(world.bank.df)[5:50],
                        selected = "Population.growth..annual..."),
            
            # Select years for x-axis
            sliderInput(inputId = "x", 
                  label = "X-axis:", 
                  min = 2000, 
                  max = 2015,
                  step = 1, 
                  value = c(2000,2015)),
            
            # Show data table
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            # Horizontal line for visual separation
            hr(),
            
            # Select which types of movies to plot ------------------------
            checkboxGroupInput(inputId = "continent",
                               label = "Select continent(s):",
                               choices = unique(world.bank.df$ContinentName),
                               selected = "North America")
        ),

        # Output
        mainPanel(
            # Show lineplot
            plotOutput(outputId = "lineplot", hover = hoverOpts(id = "plot_hover", delay = 100)),
            verbatimTextOutput("info"),
            br(),
            
            # Show data table
            DT::dataTableOutput(outputId = "table")
        )
    )
)

# Define server logic required to draw plots and tables
server <- function(input, output) {
    
    # setwd(dir = "C:/Users/shiri/Documents/Homework_1/Economic_indicators")
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
    
    # Create a subset of data filtering for selected continents
    continents_subset <- reactive({
        req(input$continent)
        filter(world.bank.df, ContinentName %in% input$continent)
    })
   
    output$lineplot <- renderPlot({
        ggplot(data = continents_subset(), aes_string(x = "Year", 
                                               y = input$y,
                                               color = "CountryName")) +
            geom_line() +
            scale_x_continuous(breaks = seq(input$x[1],input$x[2],1), limits = c(input$x[1],input$x[2])) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Year", y = paste("Indicator: ", input$y), 
            title = paste(paste(paste("Trend in countries indicators from ", input$x[1]), " to "), input$x[2])) +
            theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))
    })
    
    output$info <- renderText({
        paste0("Year = ", input$plot_hover$x, "\nIndicator value = ", input$plot_hover$y)
    })
    
    
    # Print data table if checked
    output$table <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = continents_subset()[,1:10], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
