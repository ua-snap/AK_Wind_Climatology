# 
# 

#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(openair)
library(RColorBrewer)

# Work directory
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

# changepoints
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)

stids <- cpts_df$stid[cpts_df$cpts == 1]
#-- User Interface ------------------------------------------------------------

ui <- shinyUI(
  fluidPage(
    titlePanel("AK ASOS Wind Speeds Histogram"),
    sidebarPanel(
      selectInput("stid", "Station ID", choices = stids,
                  selected = stids[6]),
      dateRangeInput("date_range", "Dates", 
                     start = "1980-01-01", end = "2015-01-01"),
      sliderInput("bins", "NUmber of Bins", min = 5, max = 30, value = 20)
    ),
    mainPanel(
      plotOutput("wsPlot")
    )
  )
)

#-- Server --------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  
  # Reactive expression to create data frame of all input values 
  asos_temp <- reactive({
    asos_path <- file.path(asos_select_adj_dir, paste0(input$stid, "_qmap.Rds"))
    asos <- readRDS(asos_path)
    
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    asos$sped_adj <- round(asos$sped_adj, digits = 1)
    asos %>% filter(date >= start_date &
                      date <= end_date & 
                      sped < 100)
    
  })
  
  # 
  output$wsPlot <- renderPlot({
    barfill <- "gold1"
    barlines <- "goldenrod2"
    
    p7 <- ggplot(asos_temp(), aes(x = sped_adj)) +
      geom_histogram(aes(y = ..count..), binwidth = 50/input$bins,
                     colour = barlines, fill = barfill) +
      scale_x_continuous(name = "Wind Speeds (mph)",
                         breaks = seq(0, 50, 5),
                         limits=c(0, 50)) +
      scale_y_continuous(name = "Count") +
      ggtitle(input$stid)
    p7
  })
})

shinyApp(ui, server)