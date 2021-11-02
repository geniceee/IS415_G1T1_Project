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


# ## 4.1 Import Geospatial data
# mpsz_sf <- st_read(dsn = "data/geospatial", layer="MP14_SUBZONE_WEB_PL")
# bus_sf <- st_read(dsn="data/geospatial", layer="BusStop")
# 
# ## 4.2 Check for NA values
# mpsz_sf[rowSums(is.na(mpsz_sf))!=0,]
# bus_sf[rowSums(is.na(bus_sf))!=0,]
# 
# ### 4.2.1 Remove NA values
# bus_sf <- bus_sf[!rowSums(is.na(bus_sf))!=0,]
# 
# ### 4.2.2 Check for NA values
# mpsz_sf[rowSums(is.na(mpsz_sf))!=0,]
# bus_sf[rowSums(is.na(bus_sf))!=0,]







### Configuration ###



# Run the application 
shinyApp(ui = ui, server = server)
