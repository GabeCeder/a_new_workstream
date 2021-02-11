# Welcome to my code for this Shiny App

# Loading the necessary libraries

library(shiny)
library(tidyverse)
library(readr)
library(janitor)
library(ggthemes)
library(viridis)  
library(gganimate)
library(shinythemes)
library(shinyWidgets)
library(tidycensus)
library(plotly)
library(bslib)

# Census API Key

Sys.getenv("CENSUS_API_KEY")

options(scipen=999)

# Images for use later
#                        https://phil.cdc.gov//PHIL_Images/23354/23354_lores.jpg
# https://techcrunch.com/wp-content/uploads/2020/03/GettyImages-1209679043.jpg


 # bs_theme(version = 4, bootswatch = "minty") %>%
 #     bs_theme_preview()

# Define UI for the application

ui <- fluidPage(
    
    # Set background image to an image of COVID-19  
    
    setBackgroundImage(src = "https://heller.brandeis.edu/lurie/images/stock-images/coronavirus.jpg"),
    
    theme = bs_theme(version = 4, bootswatch = "minty"),
        #shinytheme("superhero"),
    
    # Add title
    
    h1(strong("Mapping COVID-19 in the United States", 
              style = "color: white"), align = "center"),
    
    
    # Create the navigation bar, while making the title blank
    
    
    
    
    navbarPage("", 
               
               
               # Create first tab
               
               tabPanel("Maps",
                        
                        fluidRow(column(width = 4, 
                                        selectInput(inputId = "select_view",
                                                    label = "",
                                                    choices = c("Cases" = "cases",
                                                                "Deaths" = "deaths",
                                                                "Hospitalizations",
                                                                "Vaccines Administered"),
                                                    multiple = FALSE,
                                                    selected = "Cases")
                                 ),
                                 column(width = 4, 
                                        selectInput(inputId = "select_time",
                                                    label = "",
                                                    choices = c("Current Daily Level",
                                                                "Change Compared to 7 Days Ago",
                                                                "Cumulative All-Time Total"),
                                                    multiple = FALSE,
                                                    selected = "Current ")
                                 ),
                                 column(width = 4, 
                                        selectInput(inputId = "select_time",
                                                    label = "",
                                                    choices = c("Per 100K People",
                                                                "Raw Total"),
                                                    multiple = FALSE,
                                                    selected = "Per 100K People")
                                 )
                        ),
                        fluidRow(column(width = 12, 
                                        wellPanel("Testing")))
               )
    ),
                                 
                                 
                                 
                                 
                                 
               #                   
               #              
               #              fluidRow(column(width = 4, wellPanel("Bottom row, column 1, width 4")),
               #                       column(width = 8, wellPanel("Bottom row, column 2, width 8"))))
               #              
               #              column(4,
               #                     
               #                     wellPanel(
               #                         wellPanel(h1(strong("Explore the Dataset"), align = "center"),
               #                                   h3("County Case Totals on May 14th", align = "center")
               #                         ),
               #                         # Create a selectInput for the user to select which states to view 
               #                         
               #                         selectInput(inputId = "select_state",
               #                                     label = "Select which states to observe",
               #                                     choices = c("Alabama",
               #                                                 #                                                          "Alaska",
               #                                                 "Arizona",
               #                                                 "Arkansas",
               #                                                 "California",
               #                                                 "Colorado",
               #                                                 "Connecticut",
               #                                                 "Delaware",
               #                                                 "Florida",
               #                                                 "Georgia",
               #                                                 #                                                           "Hawaii",
               #                                                 "Idaho",
               #                                                 "Illinois",
               #                                                 "Indiana",
               #                                                 "Iowa",
               #                                                 "Kansas",
               #                                                 "Kentucky",
               #                                                 "Louisiana",
               #                                                 "Maine",
               #                                                 "Maryland",
               #                                                 "Massachusetts",
               #                                                 "Michigan",
               #                                                 "Minnesota",
               #                                                 "Mississippi",
               #                                                 "Missouri",
               #                                                 "Montana",
               #                                                 "Nebraska",
               #                                                 "Nevada",
               #                                                 "New Hampshire",
               #                                                 "New Jersey",
               #                                                 "New Mexico",
               #                                                 "New York",
               #                                                 "North Carolina",
               #                                                 "North Dakota",
               #                                                 "Ohio",
               #                                                 "Oklahoma",
               #                                                 "Oregon",
               #                                                 "Pennsylvania",
               #                                                 "Rhode Island",
               #                                                 "South Carolina",
               #                                                 "South Dakota",
               #                                                 "Tennessee",
               #                                                 "Texas",
               #                                                 "Utah",
               #                                                 "Vermont",
               #                                                 "Virginia",
               #                                                 "Washington",
               #                                                 "West Virginia",
               #                                                 "Wisconsin",
               #                                                 "Wyoming"),
               #                                     multiple = TRUE,
               #                                     selected = "Louisiana"),
               #                         
               #                         checkboxInput(inputId = "select_view",
               #                                       label = "View Cases per Capita",
               #                                       value = TRUE)
               #                     )
               #              ),
               #              
               #              column(7, 
               #                     
               #                     wellPanel(h2(strong("Total Cases in Each County "), align = "center"),
               #                               
               #                               # Output the plot comparing three types of wins to
               #                               # the finish place of a contestant
               #                               
               #                               plotlyOutput("state_cases"),
               #                               br()
               #                     )
               #              )
               #          )
               # )
               # ),
    
    
    
    # Add name 
    
    h3(strong("Compiled by Gabe Cederberg", style = "color:white"), align = "center"),
    
    
    
    
    
    
    
    
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
