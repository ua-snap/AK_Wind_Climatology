# app for exploring the gappiness of wind data from the various
#   ASOS stations in AK
# 

#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(fread)

# Work directory
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

asos_daily_path <- file.path(datadir, "AK_ASOS_daily_allsites_wind_19700101_to_20190528.csv")
asos_daily <- fread(asos_daily_path)

# init prop success column with 0.5 daily threshold
asos_daily <- asos_daily %>% mutate(prop_succ = if_else(obs >= 0.5, 1, 0))


#-- User Interface ------------------------------------------------------------

ui <- shinyUI(
  fluidPage(
    titlePanel("BDA Discrete Probability Examples"),
    navbarPage("Temp 1",
               tabPanel("Data Presence",
                        h3("Temp"),
                        sidebarPanel(
                          sliderInput("daily_limit",
                                      "Proportion of Successful Hourly Records", 
                                      min = 0.05, max = 1, value = 0.5),
                          sliderInput("total_limit", 
                                      "Proportion of Successful Days",
                                      min = 0.05, max = 1, value = 0.5),
                          dateRangeInput("date_range", 
                                         "Date Range", 
                                         start = "1970-01-01", 
                                         end = "2019-01-01")
                        ),
                        mainPanel(
                          plotOutput("obsPlot")
                        )
               )
    )
  )
)

#-- Server --------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  # Reactive expression to create data frame of all input values 
  asos_temp <- reactive({
    start_date <-input$date_range[1]
    end_date <- input$date_range[2]
    asos_daily %>% filter(date >= input$date_range[1] &
                            date <= input$date_range[2])
  })
  
  limits <- reactive({
    daily_limit <- input$daily_limit
    tot_limit <- input$total_limit
    c(daily_limit, tot_limit)
  })
  
  # 
  output$obsPlot <- renderPlot({
    #asos_temp <- asos_daily %>% filter(date >= observations()[1] &
    #                                     date <= observations()[2])
    
    #asos_temp <- asos_daily %>% filter(date >= "1970-01-01" &
    #                                     date <= "2019-05-28")
    
    asos_temp <- asos_temp() %>% 
      mutate(prop_succ = if_else(obs >= limits()[1], 1, 0))
    #mutate(prop_succ = if_else(obs >= 0.5, 1, 0))
    
    # column of 1's
    asos_temp$one <- 1
    asos_temp <- asos_temp %>% group_by(station) %>%
      summarise(prop_succ = sum(prop_succ)/sum(one)) %>% 
      mutate(tot_succ = if_else(prop_succ >= limits()[2], 1, 0))
    #mutate(tot_succ = if_else(prop_succ >= 0.5, 1, 0))
    
    asos_temp_hist <- asos_temp[asos_temp$tot_succ == 1, ]
    
    hist(asos_temp_hist$prop_succ)
  })
})

shinyApp(ui, server)