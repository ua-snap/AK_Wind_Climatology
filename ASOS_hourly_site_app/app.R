# 
# 

#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)

# Work directory
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

# changepoints
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)

stids <- cpts_df$stid
#-- User Interface ------------------------------------------------------------

ui <- shinyUI(
  fluidPage(
    titlePanel("Selected AK ASOS Hourly Stations"),
    navbarPage("Temp 1",
      tabPanel("Speeds", h3("Temp"),
        fluidRow(
          selectInput("stid", "Station ID", choices = stids,
                      selected = stids[1]),
          dateRangeInput("date_range", "Dates", 
                         start = "1980-01-01", end = "2015-01-01")
        ),
        fluidRow(
          plotOutput("tsPlot")
        ),
        fluidRow(
          plotOutput("CDF_plots")
        )
      )
    )
  )
)

#-- Server --------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  
  # Reactive expression to create data frame of all input values 
  asos_temp <- reactive({
    asos_path <- file.path(asos_select_adj_dir, paste0(input$stid, ".Rds"))
    asos <- readRDS(asos_path)
    
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    asos %>% filter(date >= start_date &
                            date <= end_date & 
                      sped < 150)
    
  })
  
  # adjusted station flag
  adjusted <- reactive({
    cpts_df[cpts_df$stid == input$stid, ]
  })
  # 
  output$tsPlot <- renderPlot({
    p <- ggplot(asos_temp(), aes(date, sped, group = 1)) + 
      geom_line() +
      xlab("") + ylab("Speed (mph)") + xlim(input$date_range[1], 
                                            input$date_range[2]) + 
      #scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
      ggtitle(input$stid) + 
      geom_vline(xintercept = ymd("1980-01-01"), col = "gray50", lty = 3, size = 1.25) + 
      geom_vline(xintercept = ymd("2015-01-01"), col = "gray50", lty = 3, size = 1.25)
    
    p
  })
  
  output$CDF_plots <- renderPlot({
    if (adjusted()[1, 10] == 0) {
      
    } else if (adjusted()[1, 10] == 1) {
      set1 <- asos_temp %>% filter(date < adjusted()[1, 2])
      set2 <- asos_temp %>% filter(date >= adjusted()[1, 2])
      
      p1 <- ggplot(set1, aes(sped)) + stat_ecdf(geom = "step")
      p2 <- ggplot(set2, aes(sped)) + stat_ecdf(geom = "step")
      grid.arrange(p1, p2, ncol = 2)

    }
  })
})

shinyApp(ui, server)