# Climatology app
# Various summaries of the wind data
#   over the period 1980-2015
# 

#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(openair)
library(RColorBrewer)
library(sf)
library(USAboundaries)

# Work directory
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

# changepoints
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)
# select stations meta
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

monthly_path <- file.path(datadir, "monthly_speeds.Rds")
asos_monthly <- readRDS(monthly_path)
asos_monthly$month <- factor(month.abb[asos_monthly$mm],
                             levels = month.abb)
# AK 
alaska <- us_states(states = "AK", resolution = "high")
ak <- st_transform(alaska, 26935)
#-- User Interface ------------------------------------------------------------

ui <- shinyUI(
  fluidPage(
    titlePanel("AK ASOS Wind Climatology"),
    navbarPage(" ",
      tabPanel("Monthly", 
        fluidRow(
          sidebarPanel(
            selectInput("stid", "Station ID", choices = stids,
                        selected = stids[21])
          ),
          mainPanel(
            # avg wind speeds plot
            plotOutput("wsBarPlot")
          )
        ),
        fluidRow(
          mainPanel(plotOutput("AK_map"))
        )
      )
    )
  )
)

#-- Server --------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  
  # Reactive expression to create data frame of all input values 
  asos_temp <- reactive({
    
    asos_monthly %>% filter(stid == input$stid)

  })
  
  site_coords <- reactive({
    select_stations %>% 
      filter(stid == input$stid) %>% 
      select(lon, lat) %>%
      as.data.frame()
  })
  
  # 
  output$wsBarPlot <- renderPlot({
    barfill <- "gold1"
    barlines <- "goldenrod2"
    
    p7 <- ggplot(asos_temp(), aes(x = month, y = avg_sped)) +
      geom_bar(stat = "identity", colour = barlines, fill = barfill) +
      ggtitle(input$stid) + xlab("Month") + ylab("Average Speed (mph)") +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))

    p7
  })
  
  output$AK_map <- renderPlot({
    
    site_coords_sf <- st_as_sf(site_coords(), coords = c("lon", "lat"), crs = 4326)
    sites_coords_sf <- st_transform(site_coords_sf, 26935)
    
    ggplot(data = ak) + geom_sf(fill = "cornsilk") +
      geom_sf(data = site_coords_sf, shape = 21, size = 4, fill = "cyan")
  })
})

shinyApp(ui, server)