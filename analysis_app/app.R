# Welcome to my code for this Shiny App

# Loading the necessary packages

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidycensus)
library(bslib)
library(tidyverse)
library(scales)

# Set date

end_date <- "Mar. 7, 2021"

# Load data

map_data <- read_rds("data_files/map_data2021-03-07.rds")
county_map_data <- read_rds("data_files/county_map_data2021-03-07.rds")

chart_data <- read_rds("data_files/case_chart_data2021-03-07.rds")
vax_chart_data <- read_rds("data_files/vax_chart_data2021-03-07.rds")

awesome <- read_rds("data_files/awesome2021-03-07.rds")
cool <- read_rds("data_files/cool2021-03-07.rds")

hosp_figure <- read_rds("data_files/ctp2021-03-07.rds")

geo <- read_rds("data_files/geo_data.rds")
county_geo <- read_rds("data_files/county_geo_data.rds")

# Census API Key

Sys.getenv("CENSUS_API_KEY")

# Turn off scientific notation

options(scipen = 999)

# Set themes and styles

theme2 <- theme(plot.title = element_blank(),
                legend.title = element_blank(),
                legend.position = "right",
                legend.key.width = unit(0.8, "cm"),
                legend.key.height = unit(1.5, "cm"),
                legend.text = element_text(color = "white", size = 12, face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                plot.caption = element_text(color = "white", size = 10))

theme3 <- theme(plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                                  colour = "gray30"),
                axis.title.y = element_text(color = "white", size = 12, face = "bold"),
                axis.title.y.right = element_text(color = "white", size = 12),
                axis.text.y = element_text(color = "white"),
                axis.text.y.right = element_text(color = "white"),
                axis.text.x = element_text(color = "white", angle = 45),
                axis.ticks.x = element_line(color = "white"),
                axis.text = element_text(size = 10))

style2 <- scale_fill_gradientn(name = "", colors = c("#dde6fb", "#0b2358"))

# Define UI for the application

ui <- fluidPage(
  
    #Set up favicon and window title  
  
    titlePanel(
    windowTitle = "Mapping COVID-19 in the U.S.",
    title = tags$head(tags$link(rel="icon", 
                                href="icons/favicon2.ico", 
                                type="image/x-icon")
    )),
  
    # Set background image to an image of COVID-19  
    
    setBackgroundImage(src = "coronavirus8.jpg"),
                 #      https://heller.brandeis.edu/lurie/images/stock-images/coronavirus.jpg
    
    theme = bs_theme(version = 4, bootswatch = "minty"),

    # Add title
    
    h2(strong("Mapping COVID-19 in the United States", 
              style = "color: white"), align = "center"),
    br(),
    
    # Create the navigation bar, while making the title blank
    
    navbarPage("", position = "static-top",collapsible = FALSE, fluid = TRUE, 
               
               # Create first tab
               
               tabPanel(strong("State-Level Maps"),
                        
                    fluidRow(
                            
                        
                        column(width = 3,
                               
                               br(),

                               wellPanel(h5(strong("What do you want to see?"), 
                                            style = "color:#0b2358", 
                                            align = "left"), 
                                         style = "background-color:white;"),
                               br(),
                               
                               img(src = "https://tristatesound.com/wp-content/uploads/2014/12/867-arrow-pointing-down-inside-a-circle-outline-icon.png",
                            #       https://icon-library.com/images/white-down-arrow-icon/white-down-arrow-icon-11.jpg", 
                                   height = 100, width = 100, align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                               
                               selectInput(inputId = "select_view",
                                           label = "",
                                           choices = c("Cases" = "cases",
                                                       "Deaths" = "deaths",
                                                       "Vaccines Administered" = "vax"),
                                           multiple = FALSE,
                                           selected = "vax"),

                               selectInput(inputId = "select_time",
                                           label = "",
                                           choices = c("Current Daily Level (7-Day Avg)" = "today",
                                                       "% Change Compared to 7 Days Ago" = "WoW",
                                                       "Cumulative All-Time Total" = "cumulative"),
                                           multiple = FALSE,
                                           selected = "cumulative"),

                               selectInput(inputId = "select_cut",
                                           label = "",
                                           choices = c("Per 100K People" = "pc",
                                                       "Raw Total" = "raw"),
                                           multiple = FALSE,
                                           selected = "pc")
                               ),
                        
                        column(width = 1),
                        
                        column(width = 8, 
                               plotOutput("state_map", width = "100%"),
                               
                               plotOutput("bottom_chart", width = "100%")
                              )
                               
                          )
            
               ),
               
               tabPanel(strong(" County-Level Maps "),
                        
                        fluidRow(
                            
                            column(width = 3,
                                   
                                   br(),
                                   
                                   wellPanel(h5(strong("What do you want to see?"), 
                                                style = "color:#0b2358", 
                                                align = "left"), 
                                             style = "background-color:white;"),
                                   
                                   br(),
                                   
                                   img(src = "https://tristatesound.com/wp-content/uploads/2014/12/867-arrow-pointing-down-inside-a-circle-outline-icon.png",
                                       height = 100, width = 100, align = "center", style="display: block; margin-left: auto; margin-right: auto;"),

                                   selectInput(inputId = "select_view2",
                                               label = "",
                                               choices = c("Cases" = "cases",
                                                           "Deaths" = "deaths"),
                                               multiple = FALSE,
                                               selected = "cases"),
                                   
                                   selectInput(inputId = "select_time2",
                                               label = "",
                                               choices = c("Current Daily Level (7-Day Avg)" = "today",
                                                           "% Change Compared to 7 Days Ago" = "WoW",
                                                           "Cumulative All-Time Total" = "cumulative"),
                                               multiple = FALSE,
                                               selected = "cumulative"),
                                   
                                   selectInput(inputId = "select_cut2",
                                               label = "",
                                               choices = c("Per 100K People" = "pc",
                                                           "Raw Total" = "raw"),
                                               multiple = FALSE,
                                               selected = "pc")
                            ),
                            
                            column(width = 1),
                            
                            column(width = 8, 
                                   plotOutput("county_map", width = "100%"),
                                   
                                   plotOutput("bottom_chart2", width = "100%")
                            )
                            
                        )
                        
               ),
               
               tabPanel(strong(" Hospitalizations "),
                        
                        fluidRow(
                            
                            column(width = 3,
                                   
                                   br(),
                                   
                                   wellPanel(h5(strong("What do you want to see?"), 
                                                style = "color:#0b2358", 
                                                align = "left"), 
                                             style = "background-color:white;"),
                                   
                                   br(),
                                   
                                   img(src = "https://tristatesound.com/wp-content/uploads/2014/12/867-arrow-pointing-down-inside-a-circle-outline-icon.png",
                                       height = 100, width = 100, align = "center", style="display: block; margin-left: auto; margin-right: auto;"),

                                   selectInput(inputId = "select_view3",
                                              label = "",
                                              choices = c("State Level (% ICU beds occupied)" = "state",
                                                          "County Level (% IP beds occupied)" = "county"),
                                              multiple = FALSE,
                                              selected = "state"),
                                   
                                   selectInput(inputId = "select_time5",
                                               label = "",
                                               choices = c("Current Level" = "current"),
                                               multiple = FALSE,
                                               selected = "current")
                                   
                                   ),
                            
                            column(width = 1),
                            
                            column(width = 8,
                                   plotOutput("hosp", width = "100%"),
                                   
                                   # wellPanel(
                                   #   
                                   #   h5(strong("Note: The COVID Tracking Project ended its data reporting on March 7th, so I'll be switching over to HHS data shortly."), 
                                   #      align = "center", style = "color:#d9d9d9")
                                   # ),
                                   
                                   plotOutput("hosp_chart", width = "100%")
                                  )
                            )
                       
               ),
               
               tabPanel(strong(" About the Data "),
                        
                        fluidRow(
                          
                            column(1),
                            
                            column(3,

                                       wellPanel(
                                           h2(strong("These are the data sources used in this analysis:"), align = "center", 
                                              style = "color:#0b2358"), 
                                           style = "background-color:white;")
                                       
                            ),
                            
                            column(4,
                                   
                                   wellPanel(
                                       img(src = "http://sites.bu.edu/reinhartlab/files/2019/04/new-york-times-logo-large-e1439227085840.jpg", width = "100%"),
                                  #           "https://18zu3o13q8pa3oob523tuov2-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/the-new-york-times-logo-900x330-1.png"
                                       h4(strong("Daily COVID-19 Case and Death Totals from The New York Times"), style = "color:#d9d9d9"),
                                       h5("These datasets can be found ",
                                          a(href = "https://github.com/nytimes/covid-19-data", "here", 
                                            .noWS = "outside"), .noWS = c("after-begin", "before-end"), 
                                          ".", style = "color:#d9d9d9")
                                   ),   
                                   
                                   wellPanel(
                                       img(src = "https://psabank.com/wp-content/uploads/2019/06/HHS-Seal-1600x900.png", width = "100%"),
                                       h4(strong("State-Level ICU Bed Occupancy and Hospital-Level Inpatient Hospital Bed Occupancy from the U.S. Department of Health and Human Services"), style = "color:#d9d9d9"),
                                       h5("These datasets can be found ",
                                          a(href = "https://beta.healthdata.gov/dataset/Estimated-ICU-Beds-Occupied-by-State-Timeseries/7ctx-gtb7",
                                            #https://healthdata.gov/dataset/covid-19-estimated-patient-impact-and-hospital-capacity-state", 
                                            "here", 
                                            .noWS = "outside"), 
                                          " and ",
                                          a(href = "https://beta.healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u",
                                          # https://healthdata.gov/dataset/covid-19-reported-patient-impact-and-hospital-capacity-facility", 
                                          "here", 
                                            .noWS = "outside"), .noWS = c("after-begin", "before-end"),
                                          ", respectively.", style = "color:#d9d9d9")
                                   ),
                                   
                                   wellPanel(
                                       img(src = "https://covid19communicationnetwork.org/wp-content/uploads/2020/09/Screen-Shot-2020-09-23-at-5.08.27-PM.png", width = "100%"),
                                       h4(strong("Time Series of COVID-19 Hospitalizations from The COVID Tracking Project"), style = "color:#d9d9d9"),
                                       h5("This data can be found ",
                                          a(href = "https://covidtracking.com/data/download", "here", 
                                            .noWS = "outside"), .noWS = c("after-begin", "before-end"), 
                                          ".", style = "color:#d9d9d9")
                                   )
                                   
                            ),
                            column(4,
                                   
                                   wellPanel(
                                       img(src = "https://www.workingbuildings.com/images/projects/cdc_banner.png", width = "100%"),
                                       h4(strong("State-Level Vaccination Data from the U.S. Centers for Disease Control and Prevention"), style = "color:#d9d9d9"),
                                       h5("The original data can be found ",
                                          a(href = "https://covid.cdc.gov/covid-data-tracker/#vaccinations", "here", 
                                            .noWS = "outside"), ", and my analysis pulls the CDC data from Youyang Gu's fantastic time series dataset on GitHub ",
                                          a(href = "https://github.com/youyanggu/covid19-cdc-vaccination-data", "here", 
                                            .noWS = "outside"), .noWS = c("after-begin", "before-end"), ".", style = "color:#d9d9d9")
                                   ),
                                   
                                   wellPanel(
                                       img(src = "https://www.vocecon.com/wp-content/uploads/american-community-survey.jpg", width = "100%"),
                                       h4(strong("State Population and State Geometry Data from the U.S. Census American Community Survey 2014-2018"), style = "color:#d9d9d9"),
                                       h5("More information can be found ",
                                          a(href = "https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf", "here", 
                                            .noWS = "outside"), .noWS = c("after-begin", "before-end"), 
                                          ".", style = "color:#d9d9d9"),
                                   ),
                                   
                                   wellPanel(
                                       h4(strong("Background image from the Heller School for Social Policy and Management"), style = "color:#d9d9d9"),
                                       h5("Image can be found ",
                                                 a(href = "https://heller.brandeis.edu/lurie/news/covid-19.html", "here", 
                                                   .noWS = "outside"), .noWS = c("after-begin", "before-end"), 
                                                 ".", style = "color:#d9d9d9")
                                   )
                            )
                            
                        )
               ),
               
               tabPanel(strong(" Contact "),
                        
                        fluidRow(
                            
                            column(1),
                            
                            column(3,
                                   
                                   wellPanel(
                                       img(src = "https://media-exp1.licdn.com/dms/image/C4E03AQF5_J0BskIQTg/profile-displayphoto-shrink_400_400/0/1604699131052?e=1618444800&v=beta&t=WNC3v4fd6FTpEmoG8Q9FsqncA0qq4vKb46SeU0DSCGo", width = "100%"), 
                                           style = "background-color:white;"  
                                   )
                            ),
                            column(6,
                                   
                                   wellPanel(
                                       
                                       h4(strong("Hi, I'm Gabe Cederberg. I created this website to help visualize ongoing changes in the pandemic as the new Administration ramped up its data reporting operations."), align = "center", style = "color:#d9d9d9"),
                                       
                                       br(),
                                       
                                       h4("As of March 7th, I stoppped updating the data on this website. Current pandemic data can be found at the CDC COVID Data Tracker ", 
                                                 a(href = "https://covid.cdc.gov/covid-data-tracker/#datatracker-home", "here",
                                                   .noWS = "outside"), .noWS = c("after-begin", "before-end"), 
                                                 ".", align = "center", style = "color:#d9d9d9"),
                                       
                                       br(),
                                       
                                       h5(strong("I'm currently on a gap year, and I'm a rising senior at Harvard studying 
                                   Government with a secondary in Economics."), align = "center", style = "color:#d9d9d9"),
                                          
                                       br(),
                                       
                                       wellPanel(
                                       h5("Feel free to reach out to me at gabriel.cederberg[at]gmail.com", align = "center", style = "color:#0b2358"),
                                       style = "background-color:#d9d9d9;"),
                                       
                                       br(),

                                       wellPanel(
                                           h4("Code and image credits can be accessed from this GitHub repository:", align = "center", style = "color:#d9d9d9"),
                                           h5(strong(a(href = "https://github.com/GabeCeder/a_new_workstream", 
                                                       "github.com/GabeCeder/a_new_workstream", 
                                                       .noWS = "outside"), .noWS = c("after-begin", "before-end")), align = "center", style = "color:#d9d9d9")),
                                       
                                       br()
                                       
                                   )
                            )
                        )
                    )
               ),
               
    
    h5(paste("Data Last Updated ", end_date, sep = ""),
       style = "color: white", align = "center"),
    
    # Add name 
    
    h6("Compiled by Gabe Cederberg", style = "color:white", align = "center")
)

# Set up server logic
server <- function(input, output) {

    output$state_map <- renderPlot ({

        req(input$select_view,
            input$select_time,
            input$select_cut)

        if (input$select_cut == "pc") {
            gem <- map_data %>%
                filter(slice1 == input$select_view &
                           slice2 == input$select_time)

            mapping <- geo %>% right_join(gem, by = "state")

            ggplot(data = mapping, aes(fill = per_100K_number,
                                     geometry = state_geometry)) +
                geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = mapping, fill = NA, color = "white") +
                theme_void() +
                theme2 +
              scale_fill_gradientn(name = paste(mapping$label1, "\n", mapping$label2, "\n", "Per 100K", sep = " "), 
                                   colors = c("#dde6fb", "#0b2358")) +
              theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
        }

         else {
             gem <- map_data %>%
                 filter(slice1 == input$select_view &
                            slice2 == input$select_time)

             mapping <- geo %>% right_join(gem, by = "state")

             ggplot(data = mapping, aes(fill = number, 
                                        geometry = state_geometry)) +
                 geom_sf(color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = mapping, fill = NA, color = "white") +
                 theme_void() +
                 theme2 +
               scale_fill_gradientn(name = paste(mapping$label1, "\n", mapping$label2, "\n", "Raw Total", sep = " "), 
                                    colors = c("#dde6fb", "#0b2358")) +
               theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
             }

    }, bg="transparent")
    
  
    output$county_map <- renderPlot ({
        
        req(input$select_view2,
            input$select_time2,
            input$select_cut2)

        if (input$select_cut2 == "pc") {
            gem <- county_map_data %>%
                filter(slice1 == input$select_view2 &
                           slice2 == input$select_time2)

            mapping <- county_geo %>% right_join(gem, by = "fips")
            
            ggplot() +
               geom_sf(data = mapping, aes(fill = per_100K_number,
                                                         geometry = geometry), 
                       color = alpha("white", 1 / 2), size = 0.1) +
               geom_sf(data = geo, aes(geometry = state_geometry), fill = NA, color = "white") +
               theme_void() +
               theme2 +
              scale_fill_gradientn(name = paste(mapping$label1, "\n", mapping$label2, "\n", "per 100K", sep = " "), 
                                   colors = c("#dde6fb", "#0b2358")) +
              theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
            }

         else {
             gem <- county_map_data %>%
                 filter(slice1 == input$select_view2 &
                            slice2 == input$select_time2)
             
             mapping <- county_geo %>% right_join(gem, by = "fips")
             
             ggplot() +
                 geom_sf(data = mapping, aes(fill = number,
                                             geometry = geometry), 
                         color = alpha("white", 1 / 2), size = 0.1) +
                 geom_sf(data = geo, aes(geometry = state_geometry), fill = NA, color = "white") +
                 theme_void() +
                 theme2 +
               scale_fill_gradientn(name = paste(mapping$label1, "\n", mapping$label2, "\n", "per 100K", sep = " "), 
                                    colors = c("#dde6fb", "#0b2358")) +
               theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
             }
        
    }, bg="transparent") 
    
    
    output$bottom_chart <- renderPlot ({

        req(input$select_view)
        
        if (input$select_view == "vax") {
            
            avg_daily_stat <- vax_chart_data %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == "vax") %>% 
                summarize(millions = today_seven_day_avg_doses_adm / 1000000) %>% 
                pull(millions) %>% 
                round(2)
            
            vax_chart_data %>% filter(slice1 == "vax") %>% 
                ggplot() +
                geom_line(aes(x = date, 
                              y = today_seven_day_avg_doses_adm / 1000000),
                          color = "white") +
                geom_col(aes(x = date,
                             y = new_doses / 1000000),
                         fill = "white",
                         color = NA,
                         alpha = 0.6) +
                    # scale_x_date(date_labels = "%B", 
                    #            date_breaks = "months", 
                    #            name = "") +
                labs(x = "", y = "Vaccine Doses Administered (millions) \n ",
                     title = paste("In the past week, the United States \n averaged ", avg_daily_stat, " million vaccinations per day.", sep = "")) +
                theme3
        }
        
        else {
            
            if (input$select_view == "cases") {
                
                avg_daily_stat <- chart_data %>% 
                    slice_max(order_by = date) %>% 
                    filter(slice1 == input$select_view) %>% 
                    pull(avg_number) %>% 
                    round(0)
                
                avg_daily_stat <- comma(avg_daily_stat)
                
                chart_data %>% filter(slice1 == "cases") %>% 
                    ggplot() +
                    geom_line(aes(x = date, 
                                  y = avg_number / 1000), 
                              color = "white") +
                    geom_col(aes(x = date, 
                                 y = daily_number / 1000), 
                             color = NA, 
                             fill = "white", 
                             alpha = 0.4) +
                    scale_x_date(date_labels = "%B", 
                                 date_breaks = "months", 
                                 name = "") +
                    labs(y = "Daily Cases (thousands) \n   ",
                         title = paste("In the past week, the United States \n averaged ", 
                                       avg_daily_stat, " cases per day.", sep = "")) +
                    theme3
         }
            
            else {
            
                avg_daily_stat <- chart_data %>% 
                    slice_max(order_by = date) %>% 
                    filter(slice1 == input$select_view) %>% 
                    pull(avg_number) %>% 
                    round(0)
                
                avg_daily_stat <- comma(avg_daily_stat)
                
                chart_data %>% filter(slice1 == "deaths") %>% 
                    ggplot() +
                    geom_line(aes(x = date, y = avg_number / 1000), color = "white") +
                    geom_col(aes(x = date, y = daily_number / 1000), 
                             color = NA, fill = "white", alpha = 0.4) +
                    scale_x_date(date_labels = "%B", 
                                 date_breaks = "months", 
                                 name = "") +
                    labs(y = "Daily Deaths (thousands) \n ",
                         title = paste("In the past week, the United States \n averaged ", 
                                       avg_daily_stat, " deaths per day.", sep = "")) +
                    theme3
            }
        }
        
    }, bg="transparent")
    
    
    output$bottom_chart2 <- renderPlot ({
        
        req(input$select_view2)
        
        if (input$select_view2 == "cases") {
            
            avg_daily_stat <- chart_data %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == input$select_view2) %>% 
                pull(avg_number) %>% 
                round(0)
            
            avg_daily_stat <- comma(avg_daily_stat)
            
            chart_data %>% filter(slice1 == "cases") %>% 
                ggplot() +
                geom_line(aes(x = date, 
                              y = avg_number / 1000), 
                          color = "white") +
                geom_col(aes(x = date, 
                             y = daily_number / 1000), 
                         color = NA, 
                         fill = "white", 
                         alpha = 0.4) +
                scale_x_date(date_labels = "%B", 
                             date_breaks = "months", 
                             name = "") +
                labs(y = "Daily Cases (thousands) \n   ",
                     title = paste("In the past week, the United States \n averaged ", 
                                   avg_daily_stat, " cases per day.", sep = "")) +
                theme3
        }
        
        else {
            
            avg_daily_stat <- chart_data %>% 
                slice_max(order_by = date) %>% 
                filter(slice1 == input$select_view2) %>% 
                pull(avg_number) %>% 
                round(0)
            
            avg_daily_stat <- comma(avg_daily_stat)
            
            chart_data %>% filter(slice1 == "deaths") %>% 
                ggplot() +
                geom_line(aes(x = date, y = avg_number / 1000), color = "white") +
                geom_col(aes(x = date, y = daily_number / 1000), 
                         color = NA, fill = "white", alpha = 0.4) +
                scale_x_date(date_labels = "%B", 
                             date_breaks = "months", 
                             name = "") +
                labs(y = "Daily Deaths (thousands) \n ",
                     title = paste("In the past week, the United States \n averaged ", 
                                   avg_daily_stat, " deaths per day.", sep = "")) +
                theme3
        }
        
    }, bg="transparent")
    
    
    output$hosp <- renderPlot ({
        
        if(input$select_view3 == "state") {
            iii <- geo %>% 
                right_join(cool, by = "state")
            
            iii %>% 
                ggplot() +
                geom_sf(aes(fill = pct_ICU_bed_occupied), color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = geo, fill = NA, color = "white") +
                theme_void() +
                labs(title = "") +
                theme2 +
              scale_fill_gradientn(name = "% of ICU Beds \nOccupied", 
                                   colors = c("#dde6fb", "#0b2358")) +
              theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
            }
        
        else {

            hhh <- county_geo %>%
                right_join(awesome, by = c("state", "county"))
            
            hhh %>%
                ggplot() +
                geom_sf(data = county_geo, fill = "dark grey", size = 0.1) +
                
                geom_sf(aes(fill = county_avg), color = alpha("white", 1 / 2), size = 0.1) +
                geom_sf(data = geo, fill = NA, color = "white") +
                labs(caption = "Shading indicates average of all reporting hospitals in the county") +
                theme_void() +
                theme2 +
              scale_fill_gradientn(name = "% of IP Beds \nOccupied", 
                                   colors = c("#dde6fb", "#0b2358")) +
              theme(legend.title = element_text(color = "white", size = 10, face = "bold"))
            }
        
        
    }, bg="transparent")
    
    
    output$hosp_chart <- renderPlot({
        
        hosp_stat <- hosp_figure %>% 
            slice_max(order_by = date) %>% 
            pull(hosp) %>% 
            round(1)
        
        hosp_stat <- comma(hosp_stat)
        
        hosp_figure %>% 
            ggplot() +
            geom_line(aes(x = date, y = hosp / 1000), color = "white") +
            geom_col(aes(x = date, y = hosp / 1000), 
                     color = NA, fill = "white", alpha = 0.4) +
            scale_x_date(date_labels = "%B", 
                         date_breaks = "months", 
                         name = "") +
            labs(y = "COVID-19 Hospitalizations (thousands)\n ",
                 title = paste("On ", end_date, ", ", hosp_stat, " people were hospitalized \n with COVID-19 in the United States.", sep = "")) +
            theme3
    }, bg="transparent")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
