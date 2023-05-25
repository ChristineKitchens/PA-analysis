#' R Code for Analyzing Particulate Absorption OceanView Data
#' Contains functions for reading raw text files from OceanView
#' and puts out graphs of data
#' 
#' Christine Kitchens
#' University of Michigan
#' Cooperative Institute for Great Lakes Research
#' chknight@umich.edu
#' October 2022


# 0 - Install Libraries ---------------------------------------------------

# install.packages("data.table")
# install.packages("janitor")
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("reshape")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("tidyverse")


# 1 - Load Libraries ------------------------------------------------------

library("data.table")
library("janitor")
library("leaflet")
library("plotly")
library("reshape")
library("shiny")
library("shinythemes")
library("tidyverse")


# 2 - Set Working Directory -----------------------------------------------

# Generate file path to working directory based on OS in use. OS.type will
# return "unix" if on Mac and "windows" if on PC.
if(.Platform$OS.type == "unix") {
  file_path20 <- file.path("/volumes",
                           "GoogleDrive",
                           "Shared drives",
                           "CSMI 2020 L Michigan Productivity and Optics",
                           "CSMI L Michigan Optics Experiments",
                           "R",
                           "app",
                           fsep = "/")
  file_path22 <- file.path("/volumes",
                           "GoogleDrive",
                           "Shared drives",
                           "CSMI 2022 Primary Production and Optics",
                           "R",
                           "app",
                           fsep = "/")
} else {
  file_path20 <- file.path("G:",
                           "Shared drives",
                           "CSMI 2020 L Michigan Productivity and Optics",
                           "CSMI L Michigan Optics Experiments",
                           "R",
                           "app",
                           fsep = "/")
  file_path22 <- file.path("G:",
                           "Shared drives",
                           "CSMI 2022 Primary Production and Optics",
                           "R",
                           "app",
                           fsep = "/")
}

setwd(file_path20)

# 2 - Import Datasets -----------------------------------------------------

full_df <- read.csv(file = "data/full_dataset.csv")
avg_df <- read.csv(file = "data/averaged_dataset.csv")
#' Find list of lat/long coordinates for sites


# 3 - Update Column Types -------------------------------------------------

#' Add factor levels to fraction column
fraction_list <- c("blank",
                   "<1.2",
                   "<2",
                   "<5",
                   "<10",
                   "<20",
                   "<53",
                   "Whole")

full_df$fraction <- factor(full_df$fraction, levels = fraction_list, ordered = TRUE)
avg_df$fraction <- factor(avg_df$fraction, levels = fraction_list, ordered = TRUE)

#' Update relevant columns to datetime objects
full_df$sample.date <- as.Date(full_df$sample.date, format = "%Y-%m-%d")
avg_df$sample.date <- as.Date(avg_df$sample.date, format = "%Y-%m-%d")
full_df$read.time <- lubridate::ymd_hms(full_df$read.time)

# 4 - Shiny App -----------------------------------------------------------


ui <- fluidPage(
  titlePanel("Particulate Absorbance Data"),
  theme = shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station",
                  label = "Station",
                  choices = unique(avg_df$station),
                  selected = "M45"),
      selectInput("date",
                  label = "Date",
                  choices = unique(avg_df$sample.date)),
      leafletOutput("location")
      ),
    mainPanel(
      plotly::plotlyOutput("plotTransmission"),
      plotly::plotlyOutput("plotReflectance")
      )
    )
  )

server <- function(input, output, session) {
  
  #' Filter data set based on criteria and draw new plot based on filtered data set
  plot_trends <- function(method){
    avg_df %>% 
      filter(station == input$station,
             sample.date == input$date,
             spec.method ==  method) %>% 
      ggplot(aes(x = wavelength, y = mean_value, color = fraction)) +
      geom_line() +
      ylim(-10, 110) +
      ylab(paste("% ", method)) +
      xlab("Wavelength (nm)")}
  
  output$plotTransmission <- plotly::renderPlotly({
    plot_trends("transmission")
    })
  
  output$plotReflectance <- plotly::renderPlotly({
    plot_trends("reflectance")
    })
  
  #' Create map displaying location of site
  output$location <- renderLeaflet({
    
  })
}

shinyApp(ui, server)