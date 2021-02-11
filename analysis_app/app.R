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

# Set date

end_date <- "2021-02-11"

# Census API Key

Sys.getenv("CENSUS_API_KEY")

# Turn off scientific notation

options(scipen=999)

# Load data

map_data <- read_rds("data_files/map_data2021-02-11.rds")

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
    h2(paste("Last Updated ", end_date, sep = ""),
              style = "color: white", align = "center"),
    br(),
    
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
                                                    choices = c("Current Daily Level (7-Day Avg)",
                                                                "% Change Compared to 7 Days Ago",
                                                                "Cumulative All-Time Total"),
                                                    multiple = FALSE,
                                                    selected = "Current ")
                                 ),
                                 column(width = 4, 
                                        selectInput(inputId = "select_cut",
                                                    label = "",
                                                    choices = c("Per 100K People",
                                                                "Raw Total"),
                                                    multiple = FALSE,
                                                    selected = "Per 100K People")
                                 )
                        ),
                        fluidRow(column(width = 12, 
                                        wellPanel(plotlyOutput("us_map"))))
               )
    ),
    
    # Add name 
    
    h3(strong("Compiled by Gabe Cederberg", style = "color:white"), align = "center")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$us_map <- renderPlotly ({
        
        # Require that an input is put in place
        
        req(input$select_view, 
            input$select_time,
            input$select_cut)
        
        if (input$select_view == "cases") {
            map1 <- map_data %>% ggplot(mapping = aes(fill = cases_7day_avg,
                                                      geometry = state_geometry,
                                                      text = paste(state_name, "<br>",
                                                                   "Cases:", cases_7day_avg, "<br>"))) +
                geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = map_data, fill = NA, color = "white") +
                scale_fill_viridis_c(option = "inferno", 
                                     direction = -1) +
                theme_void()
                
            
         #   map1
            ggplotly(map1, tooltip = "text")
        
        }
        
        # 
        # x <- county_data
        # 
        # if (input$select_state != "All") {
        #     x <- filter(x, state %in% c(input$select_state))
        # }
        # 
        # if (input$select_view == FALSE) {
        #     a <- x %>% ggplot(mapping = aes(fill = cases, geometry = geometry,
        #                                     text = paste("County:", county, "<br>",
        #                                                  "State:", state, "<br>",
        #                                                  "Cases:", cases, "<br>"))) +
        #         geom_sf(data = x) +
        #         scale_fill_viridis_c(option = "plasma") +
        #         labs(caption = "Sources: The New York Times and the American Community Survey 2014-2018",
        #              fill = "Total Cases") +
        #         theme_void()
        #     
        #     ggplotly(a, tooltip = "text")
        #     
        # }
        # 
        # else {
        #     b <- x %>% ggplot(mapping = aes(fill = cases_per_thousand, geometry = geometry,
        #                                     text = paste("County:", county, "<br>",
        #                                                  "State:", state, "<br>",
        #                                                  "Cases per Thousand:", round(cases_per_thousand, 2), "<br>"))) +
        #         geom_sf(data = x) +
        #         scale_fill_viridis_c(option = "plasma") +
        #         labs(caption = "Sources: The New York Times and the American Community Survey 2014-2018",
        #              fill = "Cases Per 1,000") +
        #         #     theme(fill.position = element_blank()) +
        #         theme_void()  
        #     
        #     ggplotly(b, tooltip = "text")
        # }
        # 
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
