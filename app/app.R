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
library(shinythemes)
library(shinyjs)

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


## Read in OD pop data
od_pop_sz <- read_csv("data/aspatial/od_pop_sz.csv")
print(od_pop_sz)


## Read in OD all data
od_sz <- read_csv("data/aspatial/od_sz_data.csv")
print(od_sz)

### 7.5.3 Check for NA values
od_sz[rowSums(is.na(od_sz))!=0,]

### 7.5.4 Set intra-zonal distances to smaller value
od_sz$dist <- ifelse(od_sz$dist == 0,5,od_sz$dist)
glimpse(od_sz)

### 7.5.5 Remove intra-zonal flows
df_inter <- od_sz[od_sz$ORIGIN_SZ_C != od_sz$DEST_SZ_C, ]

## Unconstrained
uncosim <- glm(TRIPS ~ log(O_TOTAL_POP) + log(D_MONTHLY_INCOME) + log(dist),
               na.action = na.exclude, family = poisson(link = "log"), 
               data = df_inter)
df_inter$fitted <- round(fitted(uncosim),0)


### Configuration ###
ui <- fluidPage(theme = shinytheme("simplex"),
                useShinyjs(),
                # Navigation Bar
                navbarPage(
                    title = div(img(src = '../logo.JPG', style = "margin-top: 0px; padding-right:6px;padding-bottom:20px", height = 55)),
                    windowTitle = "FloSG",
                    collapsible = TRUE,
    # EDA
    tabPanel("EDA", fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                
                # Spatial Point Map of Bus Stops
                conditionalPanel(
                    'input.EDA_var === "Spatial Points"',
                    selectInput(inputId = "colour_pmap",
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
                ), # 1st conditionalPanel
                
                # Choropleth
                conditionalPanel(
                    'input.EDA_var === "Choropleth Maps',
                    selectInput(inputId = "classification",
                                label = "Classification method",
                                choices = c("fixed" = "fixed",
                                               "sd" = "sd",
                                               "equal" = "equal",
                                               "pretty" = "pretty",
                                               "quantile" = "quantile",
                                               "kmeans" = "kmeans",
                                               "hclust" = "hclust",
                                               "bclust" = "bclust",
                                               "fisher" = "fisher",
                                               "jenks" = "jenks"),
                                selected = "pretty"), 
                    selectInput(inputId = "colour_cmap",
                                label = "Colour scheme",
                                choices = c("blues" = "Blues",
                                               "reds" = "Reds",
                                               "greens" = "Greens",
                                               "Yellow-Orange-Red" = "YlOrRd",
                                               "Yellow-Orange-Brown" = "YlOrBr",
                                               "Yellow-Green" = "YlGn",
                                               "Orange-Red" = "OrRd"),
                                selected = "YlOrRd"),
                    selectInput(inputId = "o_d",
                                label = "Origin/Destination:",
                                choices = c("Origin" = "ORIGIN_SZ_C",
                                               "Destination" = "DEST_SZ_C"),
                                selected = "ORIGIN_SZ_C"),
                    
                    selectInput(inputId = "dt",
                                label = "Day Type:",
                                choices = c("Weekday" = "WEEKDAY",
                                               "Weekend" = "WEEKENDS/HOLIDAY"),
                                selected = "WEEKDAY"),
                    
                    selectInput(inputId = "hr_mth",
                                label = "Hour/Month Interval:",
                                choices = c("Month" = "YEAR_MONTH",
                                             "Hour" = "TIME_PER_HOUR"),
                                selected = "YEAR_MONTH")
                    
                ) # 2nd conditionalPanel
                
            ), #sidebarPanel
          
            
            mainPanel(
                tabsetPanel(
                    id="EDA_var",
                    tabPanel("Spatial Points", tmapOutput("pt_bus_stop")),
                    # DT::dataTableOutput(outputId = "aTable") 
                    tabPanel("Choropleth Maps", plotOutput(outputId =  "cmap"))
                )

            )
            
            
        ) #1st sidebarLayout
    ), # 1st tabPanel
    
    
    # Spatial Interaction Model
    tabPanel("SIM", fluid = TRUE,
        sidebarLayout(
            sidebarPanel(

                # Unconstrained
                conditionalPanel(
                    'input.SIM_var === "Unconstrained"',
                    selectInput(inputId = "colour_unmap",
                                label = "Colour of Points",
                                choices = c("blue" = "blue",
                                            "green" = "green",
                                            "red" = "red"),
                                selected = "blue")
                ) # 1st conditionalPanel

            ), # 2nd sidebarPanel

            mainPanel(
                tabsetPanel(
                    id="SIM_var",
                    tabPanel("Unconstrained", plotOutput(outputId = "un_map"))
                    # DT::dataTableOutput(outputId = "aTable")
                    )
    
                )

            ) # 2nd sidebarLayout
        ) # 2nd tabPanel

    )#navbarPage
    
)#fluidpage




server <- function(input, output){
    # Spatial Points Map
    output$pt_bus_stop <- renderTmap({
        tm_shape(mpsz_sf) +
            tm_polygons() +
            tm_shape(bus_sf) +
            tm_dots(alpha = input$alpha,
                    col = input$colour_pmap,
                    size = input$size)
    })
    
    # Choropleth
    output$cmap <- renderPlot({
        
        sub_df <- od_pop_sz %>% filter(DAY_TYPE==input$dt)%>% 
                                group_by(!!!syms(input$o_d), !!!syms(input$hr_mth))  %>%
                                summarise(TOTAL_TRIPS = sum(TRIPS))  %>%
                                ungroup()  %>%
                                select(input$o_d, input$hr_mth, TOTAL_TRIPS)  %>%
                                pivot_wider(names_from=input$hr_mth,  values_from=TOTAL_TRIPS)
        
        # Replace NA values with 0
        sub_df[is.na(sub_df)] = 0

        # Get unique values of grp_by value
        unique_grp <- sort(unique(od_pop_sz[[input$hr_mth]]))

        # if categorical variable is numeric, change to character only after sorting
        if (input$hr_mth=="TIME_PER_HOUR"){
            unique_grp <- as.character(sort(as.numeric(unique_grp)))
        }

        # Perform left join
        mpsz_od_grp <- left_join(mpsz_sf, sub_df, by = c('SUBZONE_C' = input$o_d))

        # Create list to store choropleth maps
        cmap_list <- vector(mode = "list", length = length(unique_grp))
        
        for (i in 1:length(unique_grp)) {
            cmap <- tm_shape(mpsz_od_grp) +
                tm_fill(col = unique_grp[[i]],
                        palette = input$colour_cmap,
                        style = input$classification,
                        n = 5) +
                tm_borders(lwd = 0.05,
                           alpha = 0.5) +

                tm_layout(panel.show = TRUE,
                          panel.labels = unique_grp[[i]],
                          panel.label.color = 'black',
                          panel.label.size = 0.7,
                          inner.margins = 0,
                          legend.text.size = 0.4,
                          frame=T)
            cmap_list[[i]] <- cmap
        }
        do.call(tmap_arrange, cmap_list)
    })
    
    # SIM
    output$un_map <- renderPlot({
        ggplot(data=df_inter, 
               aes(y = `TRIPS`, 
                   x = `fitted`))+
            geom_point(color=input$colour_unmap)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)