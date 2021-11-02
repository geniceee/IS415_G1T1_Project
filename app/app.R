#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)
library(stplanr)
library(tmap)
library(reshape2)
library(sp)
library(caret)

## 4.1 Import Geospatial data
mpsz_sf <- st_read(dsn = "data/geospatial", layer="MP14_SUBZONE_WEB_PL")

## 4.4.1 Assign CRS and check again
mpsz_sf <- st_set_crs(mpsz_sf, 3414)
bus_sf <- st_read(dsn="data/geospatial", layer="BusStop")

### 4.2.1 Remove NA values
bus_sf <- bus_sf[!rowSums(is.na(bus_sf))!=0,]

### 4.3.1 Remove duplicated values
bus_sf <- bus_sf[!duplicated(bus_sf$BUS_STOP_N),]

## 4.4.1 Assign CRS and check again
mpsz_sf <- st_set_crs(mpsz_sf, 3414)
bus_sf <- st_set_crs(bus_sf, 3414)

## 4.5.1 Handle the invalid geometries
mpsz_sf <- st_make_valid(mpsz_sf)

## 4.7.3 Remove bus stops from bus stop data
omit_bus <- list(46211, 46219, 46239, 46609, 47701)
bus_sf <- bus_sf[!bus_sf$BUS_STOP_N %in% omit_bus, ]

od_pop_sz <- read_csv("data/aspatial/od_pop_sz.csv")
print(od_pop_sz)



### Configuration ###
ui <- fluidPage(
    # Choropleth Mapping - Reactive
    titlePanel("Choropleth Mapping - Reactive"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "colour",
                        label = "Colour of Points",
                        choices = c("blue" = "blue",
                                    "green" = "green",
                                    "red" = "red"),
                        selected = "blue"),
            
            sliderInput(inputId = "alpha",
                        label = "Transparency of Points",
                        min = 0.3,
                        max = 1,
                        value = c(0.4)),
            sliderInput(inputId = "size",
                        label = "Size of Points",
                        min = 0.01,
                        max = 0.1,
                        value = c(0.03))
        ),
        mainPanel(
            tmapOutput("pt_bus_stop"),
            # DT::dataTableOutput(outputId = "aTable")
        )
        
        
    )
)



server <- function(input, output){
    output$pt_bus_stop <- renderTmap({
        tm_shape(mpsz_sf) +
            tm_polygons() +
            tm_shape(bus_sf) +
            tm_dots(alpha = input$alpha,
                    col = input$colour,
                    size = input$size)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
