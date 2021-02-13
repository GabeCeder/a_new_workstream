# Welcome to my code for this Shiny App

# Loading the necessary libraries

library(shiny)
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
library(tidyverse)
library(ggplot2)

# Set date

end_date <- "Feb. 12, 2021"
map_data <- read_rds("data_files/map_data2021-02-12.rds")
county_map_data <- read_rds("data_files/county_map_data2021-02-12.rds")

# Census API Key

Sys.getenv("CENSUS_API_KEY")

# Turn off scientific notation

options(scipen=999)

# Load data

geo <- read_rds("data_files/geo_data.rds")
county_geo <- read_rds("data_files/county_geo_data.rds")
test <- read_rds("data_files/test.rds")


theme2 <- theme(plot.title = element_blank(),
                legend.title = element_blank(),
                legend.position = "right",
                legend.key.width = unit(0.5, "cm"),
            #    legend.direction = "horizontal",
                legend.text = element_text(color = "white", size = 12, face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA))

style2 <- scale_fill_gradientn(name = "", colors = c("#dde6fb", "#0b2358"))

style3 <- scale_fill_gradientn(name = "", trans = "log", colors = c("#dde6fb", "#0b2358"))


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
               
               tabPanel("State Level",
                        
                    fluidRow(
                            
                        
                        column(width = 3,
                               
                               br(),
                               
                               wellPanel(h4("Select Visualization Options Below:", 
                                            style = "color:#0b2358", 
                                            align = "left"), 
                                         style = "background-color:white;"),
                               
                               br(),
                               br(),
                               
                               selectInput(inputId = "select_view",
                                           label = "",
                                           choices = c("Cases" = "cases",
                                                       "Deaths" = "deaths",
                                                       "Vaccines Administered" = "vax"),
                                           multiple = FALSE,
                                           selected = "Cases"),
                               
                               br(),
                               br(),

                               selectInput(inputId = "select_time",
                                           label = "",
                                           choices = c("Current Daily Level (7-Day Avg)" = "today",
                                                       "% Change Compared to 7 Days Ago" = "WoW",
                                                       "Cumulative All-Time Total" = "cumulative"),
                                           multiple = FALSE,
                                           selected = "Current Daily Level (7-Day Avg)"),
                               
                               br(),
                               br(),

                               selectInput(inputId = "select_cut",
                                           label = "",
                                           choices = c("Per 100K People" = "pc",
                                                       "Raw Total" = "raw"),
                                           multiple = FALSE,
                                           selected = "Per 100K People")
                               ),
                        
                        column(width = 9, 
                               plotlyOutput("state_map", width = 1000, height = 600)
                        )
                               
                               )
               ),
               
               tabPanel("County Level",
                        
                        fluidRow(
                            
                            
                            column(width = 3,
                                   
                                   br(),
                                   
                                   wellPanel(h4("Select Visualization Options Below:", 
                                                style = "color:#0b2358", 
                                                align = "left"), 
                                             style = "background-color:white;"),
                                   
                                   br(),
                                   br(),
                                   
                                   selectInput(inputId = "select_view2",
                                               label = "",
                                               choices = c("Cases" = "cases",
                                                           "Deaths" = "deaths"),
                                               multiple = FALSE,
                                               selected = "Cases"),
                                   
                                   br(),
                                   br(),
                                   
                                   selectInput(inputId = "select_time2",
                                               label = "",
                                               choices = c("Current Daily Level (7-Day Avg)" = "today",
                                                           "% Change Compared to 7 Days Ago" = "WoW",
                                                           "Cumulative All-Time Total" = "cumulative"),
                                               multiple = FALSE,
                                               selected = "Current Daily Level (7-Day Avg)"),
                                   
                                   br(),
                                   br(),
                                   
                                   selectInput(inputId = "select_cut2",
                                               label = "",
                                               choices = c("Per 100K People" = "pc",
                                                           "Raw Total" = "raw"),
                                               multiple = FALSE,
                                               selected = "Per 100K People")
                            ),
                            
                            column(width = 9, 
                                   plotOutput("county_map", width = 1000, height = 600)
                            )
                            
                        )
                        
               ),
               
               tabPanel("Hospitalizations",
                        
                        fluidRow(
                            
                            column(width = 10)
                        )
               ),
               
               tabPanel("About the Data",
                        
                        fluidRow(
                            
                            column(width = 10)
                        )
               ),
               
               tabPanel("Contact",
                        
                        fluidRow(
                            
                            column(width = 10)
                        )
               )
               
               ),

    # Add name 
    
    h4("Compiled by Gabe Cederberg", style = "color:white", align = "right")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$state_map <- renderPlotly ({

        # Require that an input is put in place

        req(input$select_view,
            input$select_time,
            input$select_cut)

        if (input$select_cut == "pc") {
            gem <- map_data %>%
                filter(slice1 == input$select_view &
                           slice2 == input$select_time)

            mapping <- geo %>% right_join(gem, by = "state")

            map1 <- ggplot(data = mapping, aes(fill = per_100K_number,
                                     geometry = state_geometry,
                                     text = paste(state, "<br>",
                                                  "Value:", viz_per_100K_number, "<br>"))) +
                geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = mapping, fill = NA, color = "white") +
                theme_void() +
                theme2 +
                style2

            ggplotly(map1, tooltip = "text")

        }

         else {
             gem <- map_data %>%
                 filter(slice1 == input$select_view &
                            slice2 == input$select_time)

             mapping <- geo %>% right_join(gem, by = "state")

             map2 <- ggplot(data = mapping, aes(fill = number,
                                                          geometry = state_geometry,
                                                          text = paste(state, "<br>",
                                                                       "Value:", viz_number, "<br>"))) +
                 geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = mapping, fill = NA, color = "white") +
                 theme_void() +
                 theme2 +
                 style2

             ggplotly(map2, tooltip = "text")

         }

    })
    
    
    output$county_map <- renderPlot ({
        
        # Require that an input is put in place
        
        req(input$select_view2,
            input$select_time2,
            input$select_cut2)

        if (input$select_cut2 == "pc") {
            gem <- county_map_data %>%
                filter(slice1 == input$select_view2 &
                           slice2 == input$select_time2)

            mapping <- county_geo %>% right_join(gem, by = c("state", "county"))
            
            map3 <- ggplot() +
               geom_sf(data = mapping, aes(fill = per_100K_number,
                                                         geometry = geometry), color = alpha("white", 1 / 2), size = 0.1) +
               geom_sf(data = geo, aes(geometry = state_geometry), fill = NA, color = "white") +
               theme_void() +
               theme2 +
               style2
        
            map3
            
         }

         else {
             gem <- county_map_data %>%
                 filter(slice1 == input$select_view2 &
                            slice2 == input$select_time2)
             
             mapping <- county_geo %>% right_join(gem, by = c("state", "county"))
             
             map3 <- ggplot() +
                 geom_sf(data = mapping, aes(fill = number,
                                             geometry = geometry), color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = geo, aes(geometry = state_geometry), fill = NA, color = "white") +
                 theme_void() +
                 theme2 +
                 style2
             
             map3
         }
        
        
        #     gem <- county_map_data %>% 
        #         filter(slice1 == input$select_view & 
        #                    slice2 == input$select_time)
        #     
        #     mapping <- county_geo %>% right_join(gem, by = c("state", "county"))
        #     
        #     map2 <- ggplot(data = mapping, mapping = aes(fill = number,
        #                                                  geometry = geometry,
        #                                                  text = paste(county, "<br>",
        #                                                               state, "<br>",
        #                                                               "Value:", viz_number, "<br>"))) +
        #         geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
        #         geom_sf(data = mapping, fill = NA, color = "white") +
        #         theme_void() +
        #         theme2 +
        #         style2
        #     
        #     ggplotly(map2, tooltip = "text")
        #     
        # }
        
    }, bg="transparent") 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
