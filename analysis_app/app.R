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

# Set date

end_date <- "Feb. 12, 2021"

# Load data

map_data <- read_rds("data_files/map_data2021-02-13.rds")
county_map_data <- read_rds("data_files/county_map_data2021-02-13.rds")

chart_data <- read_rds("data_files/case_chart_data2021-02-12.rds")
vax_chart_data <- read_rds("data_files/vax_chart_data2021-02-12.rds")

awesome <- read_rds("data_files/awesome2021-02-12.rds")


geo <- read_rds("data_files/geo_data.rds")
county_geo <- read_rds("data_files/county_geo_data.rds")

# Census API Key

Sys.getenv("CENSUS_API_KEY")

# Turn off scientific notation

options(scipen = 999)

theme2 <- theme(plot.title = element_blank(),
                legend.title = element_blank(),
                legend.position = "right",
                legend.key.width = unit(0.8, "cm"),
                legend.key.height = unit(1.5, "cm"),
            #    legend.direction = "horizontal",
                legend.text = element_text(color = "white", size = 12, face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA))

theme3 <- theme(plot.title = element_text(color = "white", face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                                  colour = "gray30"),
                axis.title.y = element_text(color = "white", size = 12),
                axis.title.y.right = element_text(color = "white", size = 12),
                axis.text.y = element_text(color = "white"),
                axis.text.y.right = element_text(color = "white"),
                axis.text.x = element_text(color = "white", angle = 45),
                axis.ticks.x = element_line(color = "white"),
                axis.text = element_text(size = 10),
                plot.caption = element_text(color = "white"))

style2 <- scale_fill_gradientn(name = "", colors = c("#dde6fb", "#0b2358"))


# Images for use later
#                        https://phil.cdc.gov//PHIL_Images/23354/23354_lores.jpg
# https://techcrunch.com/wp-content/uploads/2020/03/GettyImages-1209679043.jpg


  # bs_theme(version = 4, bootswatch = "minty") %>%
  #     bs_theme_preview()

# Define UI for the application

ui <- fluidPage(
    
    # Set background image to an image of COVID-19  
    
    setBackgroundImage(src = "coronavirus3.jpg"),
                 #      https://heller.brandeis.edu/lurie/images/stock-images/coronavirus.jpg
    
    theme = bs_theme(version = 4, bootswatch = "minty"
                 #    ,bg = "#fff", fg = "#0b2358"
                     ),
    
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
                               
                               wellPanel(h5("Select Viewing Options:", 
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
                               
                               ),
                    
                    fluidRow(
                        column(width = 3),
                        column(width = 9,
                               plotlyOutput("bottom_chart", width = 1000, height = 400)
                               )
                    )
               ),
               
               tabPanel("County Level",
                        
                        fluidRow(
                            
                            
                            column(width = 3,
                                   
                                   br(),
                                   
                                   wellPanel(h5("Select Viewing Options:", 
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
                            
                        ),
                        
                        fluidRow(
                            column(width = 3),
                            column(width = 9,
                                   plotlyOutput("bottom_chart2", width = 1000, height = 400)
                            )
                        )
                        
               ),
               
               tabPanel("Hospitalizations",
                        
                        fluidRow(
                            
                            column(width = 3,
                                   selectInput(inputId = "select_view3",
                                              label = "",
                                              choices = c("State Level" = "state",
                                                          "County Level" = "county"),
                                              multiple = FALSE,
                                              selected = "State Level")),
                            column(width = 9,
                                   plotOutput("hosp", width = 1000, height = 600)
                                   )
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
    
    h5("Compiled by Gabe Cederberg", style = "color:white", align = "right")
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

            mapping <- county_geo %>% right_join(gem, by = "fips")
            
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
             
             mapping <- county_geo %>% right_join(gem, by = "fips")
             
             map3 <- ggplot() +
                 geom_sf(data = mapping, aes(fill = number,
                                             geometry = geometry), color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = geo, aes(geometry = state_geometry), fill = NA, color = "white") +
                 theme_void() +
                 theme2 +
                 style2
             
             map3
         }
        
    }, bg="transparent") 
    
    
    output$bottom_chart <- renderPlotly ({
        
        req(input$select_view)
        
        if (input$select_view == "vax") {
            
            avg_daily_stat <- vax_chart_data %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == "vax") %>% 
                summarize(millions = today_seven_day_avg_doses_adm / 1000000) %>% 
                pull(millions) %>% 
                round(2)
            
            chart <- vaccines2 %>% filter(slice1 == "vax") %>% 
                ggplot() +
                geom_line(aes(x = date, 
                              y = today_seven_day_avg_doses_adm / 1000000),
                          color = "white") +
                geom_col(aes(x = date,
                             y = new_doses / 1000000,
                             text = paste("Date: ", date, "<br>",
                                          round(new_doses / 1000000, 2), "M vaccinations reported by CDC", sep = "")),
                         fill = "white",
                         color = NA,
                         alpha = 0.6) +
                    # scale_x_date(date_labels = "%B", 
                    #            date_breaks = "months", 
                    #            name = "") +
                labs(x = "", y = "Daily Vaccine Doses Administered (millions) \n ",
                     title = paste("In the past week, the United States averaged ", avg_daily_stat, " million vaccinations per day.", sep = "")) +
                theme3
            
            ggplotly(chart, tooltip = "text")
        }
        
        else {
            
            if (input$select_view == "cases") {
                
                avg_daily_stat <- aa %>% 
                    slice_max(order_by = date) %>% 
                    filter(slice1 == input$select_view) %>% 
                    pull(avg_number) %>% 
                    round(0)
                
                avg_daily_stat <- comma(avg_daily_stat)
                
                chart <- aa %>% filter(slice1 == "cases") %>% 
                    ggplot() +
                    geom_line(aes(x = date, 
                                  y = avg_number / 1000), 
                              color = "white") +
                    geom_col(aes(x = date, 
                                 y = daily_number / 1000,
                                 text = paste("Date: ", date, "<br>",
                                              round(daily_number), " cases reported", sep = "")), 
                             color = NA, 
                             fill = "white", 
                             alpha = 0.4) +
                    scale_x_date(date_labels = "%B", 
                                 date_breaks = "months", 
                                 name = "") +
                    labs(y = "7-Day Average Daily Cases (thousands) \n   ",
                         title = paste("In the past week, the United States averaged ", 
                                       avg_daily_stat, " cases per day.", sep = "")) +
                    theme3
                
                ggplotly(chart, tooltip = "text")          
         }
            
            else {
            
                avg_daily_stat <- aa %>% 
                    slice_max(order_by = date) %>% 
                    filter(slice1 == input$select_view) %>% 
                    pull(avg_number) %>% 
                    round(0)
                
                avg_daily_stat <- comma(avg_daily_stat)
                
                chart <- aa %>% filter(slice1 == "deaths") %>% 
                    ggplot() +
                    geom_line(aes(x = date, y = avg_number / 1000), color = "white") +
                    geom_col(aes(x = date, y = daily_number / 1000), color = NA, fill = "white", alpha = 0.4) +
                    scale_x_date(date_labels = "%B", 
                                 date_breaks = "months", 
                                 name = "") +
                    labs(y = "7-Day Average Daily Deaths (thousands) \n ",
                         title = paste("In the past week, the United States averaged ", 
                                       avg_daily_stat, " deaths per day.", sep = "")) +
                    theme3
                    
                ggplotly(chart, tooltip = "text")
                
            }
        }
        
    })
    
    output$bottom_chart2 <- renderPlotly ({
        
        req(input$select_view2)
        
        if (input$select_view2 == "cases") {
            
            avg_daily_stat <- aa %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == input$select_view2) %>% 
                pull(avg_number) %>% 
                round(0)
            
            avg_daily_stat <- comma(avg_daily_stat)
            
            chart <- aa %>% filter(slice1 == "cases") %>% 
                ggplot() +
                geom_line(aes(x = date, 
                              y = avg_number / 1000), 
                          color = "white") +
                geom_col(aes(x = date, 
                             y = daily_number / 1000,
                             text = paste("Date: ", date, "<br>",
                                          round(daily_number), " cases reported", sep = "")), 
                         color = NA, 
                         fill = "white", 
                         alpha = 0.4) +
                scale_x_date(date_labels = "%B", 
                             date_breaks = "months", 
                             name = "") +
                labs(y = "7-Day Average Daily Cases (thousands) \n   ",
                     title = paste("In the past week, the United States averaged ", 
                                   avg_daily_stat, " cases per day.", sep = "")) +
                theme3
            
            ggplotly(chart, tooltip = "text")          
        }
        
        else {
            
            avg_daily_stat <- aa %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == input$select_view2) %>% 
                pull(avg_number) %>% 
                round(0)
            
            avg_daily_stat <- comma(avg_daily_stat)
            
            chart <- aa %>% filter(slice1 == "deaths") %>% 
                ggplot() +
                geom_line(aes(x = date, y = avg_number / 1000), color = "white") +
                geom_col(aes(x = date, y = daily_number / 1000), color = NA, fill = "white", alpha = 0.4) +
                scale_x_date(date_labels = "%B", 
                             date_breaks = "months", 
                             name = "") +
                labs(y = "7-Day Average Daily Deaths (thousands) \n ",
                     title = paste("In the past week, the United States averaged ", 
                                   avg_daily_stat, " deaths per day.", sep = "")) +
                theme3
            
            ggplotly(chart, tooltip = "text")
            
        }
        
    })
    
    
    output$hosp <- renderPlot ({
        
        if(input$select_view3 == "state") {
            
            # county_geo %>% 
            #     ggplot(aes(fill = pop, geometry = geometry)) +
            #     geom_sf()
            # 
            
            awesome <- awesome

            hhh <- county_geo %>%
                right_join(awesome, by = c("state", "county"))

            a <- hhh %>%
                ggplot() +
                geom_sf(color = alpha("gray", 1 / 2), size = 0.1) +
                geom_sf(aes(fill = county_avg), color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = geo, fill = NA, color = "white") +
                labs(caption = "Occupancy levels represent average of all reporting hospitals in the county") +
                    theme_void() +
                    theme2 +
                    style2
                
                    
                # scale_fill_viridis_c(name = "Occupied ICU Bed Capacity
                #              ",
                #                      limits = c(0,100), breaks = c(0, 25, 50, 75, 100),
                #                      labels=c("0%", "25%","50%", "75%", "100%"),
                #                      option = "inferno",
                #                      direction = -1) +
                # theme(plot.title = element_text(size = 13),
                #       plot.subtitle = element_text(size = 9),
                #       legend.title = element_blank(),
                #       legend.text = element_text(size = 10),
                #       legend.position = "right",
                #       legend.key.width = unit(0.5, "cm"))

            a
            
        }
        
        else {
            county_geo %>% 
                ggplot(aes(fill = pop, geometry = geometry)) +
                geom_sf() +
                theme_void() +
                theme2 +
                style2
        }
        
        
    }, bg="transparent")
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
