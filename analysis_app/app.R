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

end_date <- "Feb. 12, 2021"
map_data <- read_rds("data_files/map_data2021-02-12.rds")

# Census API Key

Sys.getenv("CENSUS_API_KEY")

# Turn off scientific notation

options(scipen=999)

# Load data

geo <- read_rds("data_files/geo_data.rds")
theme2 <- theme(plot.title = element_blank(),
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.key.width = unit(0.5, "cm"),
                legend.direction = "horizontal",
                legend.text = element_text(color = "white", size = 12),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA))

style2 <- scale_fill_gradientn(name = "", colors = c("#dde6fb", "#0b2358"))


# Images for use later
#                        https://phil.cdc.gov//PHIL_Images/23354/23354_lores.jpg
# https://techcrunch.com/wp-content/uploads/2020/03/GettyImages-1209679043.jpg


 # bs_theme(version = 4, bootswatch = "minty") %>%
 #     bs_theme_preview()

# Define UI for the application

ui <- fluidPage(
    
    # Set background image to an image of COVID-19  
    
    setBackgroundImage(src = 
                           #"https://drive.google.com/file/d/1HFhzmUurOMS0t5iaym9EFEHZ7KgDXdVu/view"),
                      #         plotOutput("background"))),
                           "coronavirus3.jpg"),
                 #      https://heller.brandeis.edu/lurie/images/stock-images/coronavirus.jpg
    
    theme = bs_theme(version = 4, bootswatch = "minty"),
        #shinytheme("superhero"),
    
    # Add title
    
    h1(strong("Mapping COVID-19 in the United States", 
              style = "color: white"), align = "center"),
    h4(paste("Data Last Updated ", end_date, sep = ""),
              style = "color: white", align = "center"),
    br(),
    
    # Create the navigation bar, while making the title blank
    
    navbarPage("", position = "static-top",
               
               # Create first tab
               
               tabPanel("Maps",
                        
                    fluidRow(
                            
                        
                        column(width = 3,
                               
                               br(),
                               
                               selectInput(inputId = "select_view",
                                           label = "",
                                           choices = c("Cases" = "cases",
                                                       "Deaths" = "deaths",
                                                       "Hospitalizations" = "hosp",
                                                       "Vaccines Administered" = "vax"),
                                           multiple = FALSE,
                                           selected = "Cases"),
                               
                               br(),
                               br(),
                               br(),
                               
                               selectInput(inputId = "select_time",
                                           label = "",
                                           choices = c("Current Daily Level (7-Day Avg)" = "today",
                                                       "% Change Compared to 7 Days Ago" = "WoW",
                                                       "Cumulative All-Time Total" = "cumulative"),
                                           multiple = FALSE,
                                           selected = "Current "),
                               
                               br(),
                               br(),
                               br(),
                               
                               selectInput(inputId = "select_cut",
                                           label = "",
                                           choices = c("Per 100K People" = "pc",
                                                       "Raw Total" = "raw"),
                                           multiple = FALSE,
                                           selected = "Per 100K People")
                               ),
                        
                   #     column(width = 2),
                        
                        
                        column(width = 9, 
                               plotlyOutput("us_map", width = 1000, height = 600)
                        )
                               
                               )
               )
               
               ),

                        
                        
                        
               #          fluidRow(column(width = 4, 
               #                          selectInput(inputId = "select_view",
               #                                      label = "",
               #                                      choices = c("Cases" = "cases",
               #                                                  "Deaths" = "deaths",
               #                                                  "Hospitalizations" = "hosp",
               #                                                  "Vaccines Administered" = "vax"),
               #                                      multiple = FALSE,
               #                                      selected = "Cases")
               #                   ),
               #                   column(width = 4, 
               #                          selectInput(inputId = "select_time",
               #                                      label = "",
               #                                      choices = c("Current Daily Level (7-Day Avg)" = "today",
               #                                                  "% Change Compared to 7 Days Ago" = "WoW",
               #                                                  "Cumulative All-Time Total" = "cumulative"),
               #                                      multiple = FALSE,
               #                                      selected = "Current ")
               #                   ),
               #                   column(width = 4, 
               #                          selectInput(inputId = "select_cut",
               #                                      label = "",
               #                                      choices = c("Per 100K People" = "pc",
               #                                                  "Raw Total" = "raw"),
               #                                      multiple = FALSE,
               #                                      selected = "Per 100K People")
               #                   )
               #          ),
               #          fluidRow(column(width = 12, 
               #                          wellPanel(plotlyOutput("us_map"))))
               # )

    # Add name 
    
    h4("Compiled by Gabe Cederberg", style = "color:white", align = "right")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$us_map <- renderPlotly ({
        
        # Require that an input is put in place
        
        req(input$select_view, 
            input$select_time,
            input$select_cut)
        
        if (input$select_cut == "pc") {
            gem <- map_data %>% 
                filter(slice1 == input$select_view & 
                           slice2 == input$select_time)
            
            mapping <- geo %>% right_join(gem, by = "state")
            
            map1 <- ggplot(data = mapping, mapping = aes(fill = per_100K_number,
                                     geometry = mapping$state_geometry,
                                     text = paste(state, "<br>",
                                                  "Value:", viz_per_100K_number, "<br>"))) +
                geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = mapping, fill = NA, color = "white") +
                scale_fill_viridis_c(option = "inferno", 
                                     direction = -1) +
                theme_void() +
                theme2 +
                style2
            
            ggplotly(map1, tooltip = "text") %>% 
                layout(legend = list(
                    orientation = "h"
                ))
            
        }
        
         else {
             gem <- map_data %>% 
                 filter(slice1 == input$select_view & 
                            slice2 == input$select_time)
             
             mapping <- geo %>% right_join(gem, by = "state")
             
             map2 <- ggplot(data = mapping, mapping = aes(fill = number,
                                                          geometry = mapping$state_geometry,
                                                          text = paste(state, "<br>",
                                                                       "Value:", viz_number, "<br>"))) +
                 geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = mapping, fill = NA, color = "white") +
                 scale_fill_viridis_c(option = "inferno", 
                                      direction = -1) +
                 theme_void() +
                 theme2 +
                 style2
             
             ggplotly(map2, tooltip = "text")
             
             
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
    
    output$background <- renderImage({
        list(src = "coronavirus.jpg")
    }, deleteFile = FALSE) 

}

# Run the application 
shinyApp(ui = ui, server = server)
