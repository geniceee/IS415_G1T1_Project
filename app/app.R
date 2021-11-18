#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
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
# library(cowplot)

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


## Read in OD pop data (EDA)
od_pop_sz <- read_csv("data/aspatial/od_pop_sz.csv")


## To subset peak hours
od_pop_sz$PEAK_HR_CAT <- ifelse(od_pop_sz$TIME_PER_HOUR == 7|
                                    od_pop_sz$TIME_PER_HOUR == 8|
                                    od_pop_sz$TIME_PER_HOUR == 9,
                                    "0700-0900", 
                            ifelse(od_pop_sz$TIME_PER_HOUR == 10|
                                   od_pop_sz$TIME_PER_HOUR == 11|
                                   od_pop_sz$TIME_PER_HOUR == 12,
                                    "1000-1200", 
                           ifelse(od_pop_sz$TIME_PER_HOUR == 17|
                                  od_pop_sz$TIME_PER_HOUR == 18|
                                  od_pop_sz$TIME_PER_HOUR == 19,
                                  "1700-1900",      
                                  "Not peak hour")))
print(od_pop_sz)

## Read in OD all data (Models)
od_sz <- read_csv("data/aspatial/od_sz_data.csv")
print(od_sz)

### 7.5.3 Check for NA values
od_sz[rowSums(is.na(od_sz))!=0,]

### 7.5.4 Set intra-zonal distances to smaller value
od_sz$dist <- ifelse(od_sz$dist == 0,5,od_sz$dist)

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
                    
                    # About 
                    tabPanel("About", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(h3(strong("FloSG"), style = "color: #ff4d4d"),
                                              br(),
                                              
                                              
                                              
                                              h4("A Group Project by:"),
                                                tags$ul(
                                                    tags$li(a(href="https://www.linkedin.com/in/genice-goh/", "Genice Goh"), style = "font-size: 16px;"),
                                                    tags$li(a(href="https://www.linkedin.com/in/nor-aisyah/", "Nor Aisyah"), style = "font-size: 16px;"),
                                                    tags$li(a(href="https://www.linkedin.com/in/wei-ling-wong-a84130131/", "Wong Wei Ling"), style = "font-size: 16px;")
                                                ),
                                              br(),
                                              
                                              h5("Guided by Professor Kam Tin Seong for IS415 Geospatial Analytics and Applications"),
                                              br(),
                                              
                                             # img(src="www/smu.png", height = 300),
                                             imageOutput("smu"), width = 3
                                 ),
                                 
                                 mainPanel(h3("Project Motivation"),
                                           fluidRow(
                                               column(7, 
                                                      h4("There is presently no analytics application that looks at public transportation commuter patterns across time and space.
                                                         We hope that creating this application can bring benefits to multiple stakeholders:
                                                         by allowing commuters to better plan their journeys, workplaces to effectively implement flexible working hours,
                                                         and the relevant governmental organisations to implement measures to address peak hour crowds.", style = "margin-top:5px;"),
                                                      br(),
                                                      h4("Since the pandemic, even though there are people who are less willing to take the public transport, there is still a
                                                         crowd in ridership at certain times. An example would be when Mass Rapid Transport (MRT) services broke down in October 2020,
                                                         this resulted in bus stops being packed with commuters looking for alternatives to get home. This is a cause for concern for
                                                         commuters who are conscious of COVID-19 risks as it is tough to maintain a safe distance from everyone around them."),
                                                      br(),
                                                      h4("The government has implemented measures to address peak hour crowds, including increasing the frequency of buses and trains
                                                         during these periods. While this does solve the issue, we wonder about the sustainability of increasing the frequency of
                                                         public transport - particularly buses, when public transportation ridership is going to increase over the years.")),
                                               column(5,
                                                   imageOutput("sg_map", width = "800%", height = "400px"),
                                                   tags$a(href="https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.mappr.co%2Fcounties%2Fregions-of-singapore%2F&psig=AOvVaw0rGtOx1ubkZxNSK9jB9YM6&ust=1636952115136000&source=images&cd=vfe&ved=0CAsQjRxqFwoTCPCrvP2Hl_QCFQAAAAAdAAAAABAN", "Source: Mappr.co")
                                               )
                                              ), # fluid row
                                           
                                           h3("Project Objectives"),
                                           h4("In our project, we will build an application to investigate commuter patterns for bus services across time and space and how they contribute to peak hour crowds."),
                                           h4("Namely, we wish to cover the following areas:"),
                                           tags$ul(
                                               h4(tags$li("Identify the areas that have the most people travelling to and from (across time)")),
                                               h4(tags$li("Visualise modelling results of potential results")),
                                               
                                               tags$ol(
                                                   h4(tags$li("Unconstrained Spatial Interaction Model")),
                                                   h4(tags$li("Origin Constrained Spatial Interaction Model")),
                                                   h4(tags$li("Destination Constrained Spatial Interaction Model")),
                                                   h4(tags$li("Doubly Constrained Spatial Interaction Model"))
                                               )
                                           ),
                                           h4("We will then analyse and interpret the output, and then conclude the project with our findings."),
                                           br(),
                                           
                                           h3("Applications"),
                                           tags$ol(
                                               h4(tags$li("Exploratory Data Analysis (EDA): to visualize the distribution of bus stops in Singapore using Spatial Points Map as well as
                                                       Origin and Destination of commuter flow using Choropleth and Desire Lines Maps.")),
                                               h4(tags$li("Spatial Interaction Models: to determine how well the models are able to fit the empirical (origin, destination) data."))
                                           ),
                                           br(),
                                           
                                           h3("R Blogdown Page"),
                                           h4("Do check out our R blogdown page for the full report here", a(href="https://flosg-is415.netlify.app/about.html", "here."))
                                           
                                           
                                           
                                ) # About mainPanel
                             ), # About sidebarlayout
                    ), # About tabPanel
                    
                    # EDA
                    tabPanel("EDA", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(fluid = TRUE, width = 3,
                                     
                                     # Spatial Point Map of Bus Stops
                                     conditionalPanel(
                                         'input.EDA_var === "Spatial Points"',
                                         selectInput(inputId = "colour_pmap",
                                                     label = "Colour of Points",
                                                     choices = c("Blue" = "blue",
                                                                 "Green" = "green",
                                                                 "Red" = "red"),
                                                     selected = "blue"),
                                         
                                         sliderInput(inputId = "alpha",
                                                     label = "Transparency of Points",
                                                     min = 0.3,
                                                     max = 1,
                                                     value = c(0.4)),
                                         checkboxInput(inputId = "showData",
                                                       label = "Show data table",
                                                       value = TRUE)
                                         
                                     ), # 1st conditionalPanel
                                     
                                     # Choropleth
                                     conditionalPanel(
                                         'input.EDA_var === "Choropleth Maps"',
                                         selectInput(inputId = "colour_cmap",
                                                     label = "Colour scheme",
                                                     choices = c("Blues" = "Blues",
                                                                 "Reds" = "Reds",
                                                                 "Greens" = "Greens",
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
                                                                 "Peak Hour" = "PEAK_HR_CAT",
                                                                 "Both" = "BOTH"),
                                                     selected = "YEAR_MONTH")
                                         
                                     ), # 2nd conditionalPanel
                                     
                                     # Desire lines
                                     conditionalPanel(
                                         'input.EDA_var === "Desire lines Maps"',
                                         selectInput(inputId = "dt_d",
                                                     label = "Day Type:",
                                                     choices = c("Weekday" = "WEEKDAY",
                                                                 "Weekend" = "WEEKENDS/HOLIDAY"),
                                                     selected = "WEEKDAY"),
                                         
                                         selectInput(inputId = "hr_mth_d",
                                                     label = "Peak Hour/Month Interval:",
                                                     choices = c("Month" = "YEAR_MONTH",
                                                                 "Peak Hour" = "PEAK_HR_CAT",
                                                                 "Both" = "BOTH"),
                                                     selected = "YEAR_MONTH"),
                                         
                                         sliderInput(inputId = "pct",
                                                     label = "Top Percentage:",
                                                     min = 0,
                                                     max = 2,
                                                     value = 0.5,
                                                     step = 0.5),


                                     ) # 3rd conditionalPanel
                                     
                                 ), #sidebarPanel
                                 
                                 
                                 mainPanel(
                                     tabsetPanel(
                                         id="EDA_var",
                                         tabPanel("Spatial Points", tmapOutput("pt_bus_stop"),
                                                  DT::dataTableOutput(outputId = "pt_table")),
                                         # DT::dataTableOutput(outputId = "pt_table"),
                                         tabPanel("Choropleth Maps", plotOutput(outputId =  "cmap", width="100%")),
                                         tabPanel("Desire lines Maps", plotOutput(outputId =  "dmap", width="100%"))
                                     )
                                     
                                 )
                                 
                                 
                             ) # EDA sidebarLayout
                    ), # EDA tabPanel
                    
                    
                    # Spatial Interaction Model
                    tabPanel("SIM", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(fluid = TRUE, width = 3,
                                     
                                     # SIM
                                     conditionalPanel(
                                         'input.SIM_var === "SIM Scatterplot"',
                                         h4("Please select the parameters before proceeding to the other tabs"),
                                         br(),
                                         
                                         selectInput(inputId = "colour_sim",
                                                     label = "Colour of Points",
                                                     choices = c("Blue" = "blue",
                                                                 "Green" = "green",
                                                                 "Red" = "red"),
                                                     selected = "blue"),
                                         selectInput(inputId = "sim_model",
                                                     label = "Type of Model",
                                                     choices = c("Unconstrained" = "Unconstrained",
                                                                 "Origin" = "Origin",
                                                                 "Destination" = "Destination",
                                                                 "Doubly" = "Doubly"),
                                                     selected = "Unconstrained"),
                                         
                                         # Selections for Unconstrained
                                         conditionalPanel(
                                             condition = "input.sim_model =='Unconstrained'",
                                             radioButtons("un_o_var", label = h5("Origin"),
                                                          choices = list("Population" = "O_TOTAL_POP", "Income" = "O_MONTHLY_INCOME"), 
                                                          selected = "O_TOTAL_POP"),
                                             radioButtons("un_d_var", label = h5("Destination"),
                                                          choices = list("Population" = "D_TOTAL_POP", "Income" = "D_MONTHLY_INCOME"), 
                                                          selected = "D_MONTHLY_INCOME")
                                         ),
                                         # Selections for Origin
                                         conditionalPanel(
                                             condition = "input.sim_model =='Origin'",
                                             selectInput(inputId = "o_var",
                                                         label = "Population/Income:",
                                                         choices = c("Population" = "D_TOTAL_POP",
                                                                     "Income" = "D_MONTHLY_INCOME"),
                                                         selected = "D_TOTAL_POP")
                                         ),
                                         # Selections for Destination
                                         conditionalPanel(
                                             condition = "input.sim_model =='Destination'",
                                             selectInput(inputId = "d_var",
                                                         label = "Population/Income:",
                                                         choices = c("Population" = "O_TOTAL_POP",
                                                                     "Income" = "O_MONTHLY_INCOME"),
                                                         selected = "D_TOTAL_POP")
                                         )

                                     ), # 1st conditionalPanel
                                
                                     
                                 ), # 2nd sidebarPanel
                                 
                                 mainPanel(
                                     tabsetPanel(
                                         id="SIM_var",
                                         tabPanel("SIM Scatterplot", plotOutput(outputId = "sim")),
                                          tabPanel(
                                              "SIM Summary", verbatimTextOutput(outputId = "sim_summary"),
                                          ),
                                          tabPanel(
                                              "Original Matrix", tableOutput(outputId = "original_matrix")
                                          ),
                                          tabPanel(
                                              "SIM Matrix", tableOutput(outputId = "sim_matrix")
                                          ),
                                          tabPanel(
                                              "SIM Results", verbatimTextOutput(outputId = "sim_results")
                                          )     

                                         # DT::dataTableOutput(outputId = "aTable")
                                     )
                                     
                                 )
                                 
                             ) # SIM sidebarLayout
                    ) # SIM tabPanel
                    
                ) #navbarPage
                
) #fluidpage




server <- function(input, output, session){
    
    # Spatial Points Map
    output$pt_bus_stop <- renderTmap({
        # tm_shape(mpsz_sf) +
            # tm_polygons() +
                tm_shape(bus_sf) + # , bbox=st_bbox(bus_sf)
                tm_dots(alpha = input$alpha,
                    col = input$colour_pmap) +
            tm_view(set.zoom.limits = c(11,13))+
        tm_basemap("OpenStreetMap")
    })
    
    # Spatial Points data table
    output$pt_table <- DT::renderDataTable({
        if(input$showData) {
            DT::datatable(data = bus_sf,
                          options = list(pageLength  = 4),
                          rownames = FALSE)
        }
    })
    
    # # Choropleth
    # output$cmap <- renderPlot({
    # 
    #     sub_df <- od_pop_sz %>% filter(DAY_TYPE==input$dt)%>%
    #         group_by(!!!syms(input$o_d), !!!syms(input$hr_mth))  %>%
    #         summarise(TOTAL_TRIPS = sum(TRIPS))  %>%
    #         ungroup()  %>%
    #         select(input$o_d, input$hr_mth, TOTAL_TRIPS)  %>%
    #         pivot_wider(names_from=input$hr_mth,  values_from=TOTAL_TRIPS)
    # 
    #     # Replace NA values with 0
    #     sub_df[is.na(sub_df)] = 0
    # 
    #     # Get unique values of grp_by value
    #     unique_grp <- sort(unique(od_pop_sz[[input$hr_mth]]))
    # 
    #     # if categorical variable is numeric, change to character only after sorting
    #     if (input$hr_mth=="TIME_PER_HOUR"){
    #         unique_grp <- as.character(sort(as.numeric(unique_grp)))
    #     }
    # 
    #     # Perform left join
    #     mpsz_od_grp <- left_join(mpsz_sf, sub_df, by = c('SUBZONE_C' = input$o_d))
    # 
    #     # Create list to store choropleth maps
    #     cmap_list <- vector(mode = "list", length = length(unique_grp))
    # 
    #     for (i in 1:length(unique_grp)) {
    #         cmap <- tm_shape(mpsz_od_grp) +
    #             tm_fill(col = unique_grp[[i]],
    #                     palette = input$colour_cmap,
    #                     style = input$classification,
    #                     n = 5) +
    #             tm_borders(lwd = 0.4,
    #                        alpha = 0.5) +
    # 
    #             tm_layout(panel.show = TRUE,
    #                       panel.labels = unique_grp[[i]],
    #                       panel.label.color = 'black',
    #                       panel.label.size = 1.5,
    #                       inner.margins = 0,
    #                       legend.text.size = 1,
    #                       legend.position = c("left", "bottom"),
    #                       frame=T) +
    #             tm_scale_bar(text.size = 1,
    #                          position="right")  +
    #             tm_view(set.zoom.limits = c(11,13))
    # 
    #         cmap_list[[i]] <- cmap
    #     }
    #     ncol_list_c <- c(cmap_list, ncol=3)
    #     do.call(tmap_arrange, ncol_list_c)
    # }, height=650, width=1100)
    
    
    
    # Choropleth
    
    get.var <- function(vname,df) {
        v <- df[vname] %>% 
            st_set_geometry(NULL)
        v <- unname(v[[1]])
        return(v)
    }
    
    output$cmap <- renderPlot({
        
        if (input$hr_mth=="YEAR_MONTH") {

            
            sub_df <- od_pop_sz %>% filter(DAY_TYPE==input$dt) %>% 
                group_by(!!!syms(input$o_d), !!!syms(input$hr_mth)) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup() %>%
                select(input$o_d, input$hr_mth, TRIPS) %>%
                pivot_wider(names_from = input$hr_mth, values_from=TRIPS)
        }
        
        if (input$hr_mth=="PEAK_HR_CAT") {
            
            df <- od_pop_sz %>% filter(PEAK_HR_CAT !="Not peak hour")
            # print(df)

            sub_df <- df %>% filter(DAY_TYPE==input$dt) %>%
                group_by(!!!syms(input$o_d), PEAK_HR_CAT) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup() %>%
                select(input$o_d, PEAK_HR_CAT, TRIPS) %>%
                pivot_wider(names_from=PEAK_HR_CAT, values_from=TRIPS)

        }
        
        if (input$hr_mth=="BOTH") {
            df <- od_pop_sz %>% filter(PEAK_HR_CAT !="Not peak hour")
            
            sub_df <- df %>% filter(DAY_TYPE==input$dt) %>%
                group_by(!!!syms(input$o_d), YEAR_MONTH, PEAK_HR_CAT) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup() %>%
                select(input$o_d, YEAR_MONTH, PEAK_HR_CAT, TRIPS) %>%
                pivot_wider(names_from=c(YEAR_MONTH, PEAK_HR_CAT),
                            values_from=(TRIPS))
        }
        
        # Replace NA values with 0
        sub_df[is.na(sub_df)] = 0
        
        # Get unique values of grp_by value
        if (input$hr_mth=="YEAR_MONTH"){
            unique_grp <- sort(unique(od_pop_sz[[input$hr_mth]]))
        }
        
        if (input$hr_mth=="PEAK_HR_CAT"){
            unique_grp <- sort(unique(df[[input$hr_mth]]))
        }
        
        if (input$hr_mth=="BOTH"){
            df$concat <- paste(df$YEAR_MONTH, df$PEAK_HR_CAT, sep="_")
            
            unique_grp <- sort(unique(df$concat))
        }
        
        # Perform left join
        mpsz_od_grp <- left_join(mpsz_sf, sub_df, by = c('SUBZONE_C' = input$o_d))
        
        # Replace NA values with 0
        mpsz_od_grp[is.na(mpsz_od_grp)] = 0
        
        # # Remove NA values
        # mpsz_od_grp <- mpsz_od_grp[!rowSums(is.na(mpsz_od_grp))!=0,]
        # 
        # # Check for NA values again
        # mpsz_od_grp[rowSums(is.na(mpsz_od_grp))!=0,]

        
        # Creating the percentmap function
        percentmap <- function(vnam, df, legtitle=NA){
            percent <- c(0,.01, .1,.5,.9,.99,1)
            var <- get.var(vnam,df)
            bperc <- quantile (var, percent)
            tm_shape (mpsz_od_grp) +
                tm_polygons() 
                tm_shape(df)+
                tm_fill(vnam, title=legtitle, breaks=bperc, palette=input$colour_cmap,
                        labels = c("< 1%", "1%-10%", "10%-50%", "50%-90%",
                                   "90%-99%", ">99%"))+
                tm_borders(lwd=0.1, alpha = 0.3) +
                tm_layout(legend.show = TRUE)
            
        }
        
        # Create list to store choropleth maps
        cmap_list_percentile <- vector(mode = "list", length = length(unique_grp))
        for (i in 1:length(unique_grp)) {
            
            cmap <- percentmap(unique_grp[[i]], mpsz_od_grp)
            cmap_list_percentile[[i]] <- cmap
        }

            ncol_list_c <- c(cmap_list_percentile, ncol=3)
            do.call(tmap_arrange, ncol_list_c)

    }, height=650, width=1100)

 

                                    
                                    
    # Desire lines
    output$dmap <- renderPlot({

        if (input$hr_mth_d=="YEAR_MONTH") {
            sub_df <- od_pop_sz %>%
                filter(DAY_TYPE==input$dt_d) %>%
                group_by(ORIGIN_SZ_C, DEST_SZ_C, !!!syms(input$hr_mth_d)) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup()
        }
        
        if (input$hr_mth_d=="PEAK_HR_CAT") {
            sub_df <- od_pop_sz %>%
                filter(DAY_TYPE==input$dt_d, PEAK_HR_CAT!="Not peak hour") %>%
                group_by(ORIGIN_SZ_C, DEST_SZ_C, !!!syms(input$hr_mth_d)) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup()
        }
        
        if (input$hr_mth_d=="BOTH") {
            sub_df <- od_pop_sz %>%
                filter(DAY_TYPE==input$dt_d, PEAK_HR_CAT!="Not peak hour") %>%
                group_by(ORIGIN_SZ_C, DEST_SZ_C, YEAR_MONTH, PEAK_HR_CAT) %>%
                summarise(TRIPS = sum(TRIPS)) %>%
                ungroup()
        }
        
        
        #Replace NA values with 0
        sub_df[is.na(sub_df)] = 0
        
        # Remove Intra zonal flows
        df_inter <- sub_df %>%
            filter(ORIGIN_SZ_C != DEST_SZ_C)
        
        # print(df_inter)
        
        # Sort by descending order
        df_inter <- df_inter %>% arrange(desc(TRIPS))

        # Get top rows to show based on input parameter percentage
        top_n <- nrow(df_inter) * input$pct /100

        df_inter <- head(df_inter, top_n)

        #Get min of trips
        min_trip <- min(df_inter$TRIPS)

        # Get max of trips
        max_trip <- max(df_inter$TRIPS)

        # Get the range then divide by 5 to get the legend interval
        leg_int <- (max_trip-min_trip) / 5


        # Get unique values of grp_by value
        if (input$hr_mth_d=="YEAR_MONTH" |  input$hr_mth_d=="PEAK_HR_CAT"){ #
            unique_grp <- sort(unique(df_inter[[input$hr_mth_d]]))
        }

        if (input$hr_mth_d=="BOTH"){
            df_inter$concat <- paste(df_inter$YEAR_MONTH, df_inter$PEAK_HR_CAT)

            unique_grp <- sort(unique(df_inter$concat))
        }


        # Take required columns from mpsz
        mpsz_sf_req = mpsz_sf[, c('SUBZONE_C')]

        dlines_list <- vector(mode = "list", length = length(unique_grp))


        for (i in 1:length(unique_grp)) {

            # Filter according to the respective year_month/ peak hour
            if (input$hr_mth_d == "YEAR_MONTH") {
                # print(unique_grp)
                df_filter <- df_inter %>% filter(YEAR_MONTH == unique_grp[[i]])
            }

            if (input$hr_mth_d == "PEAK_HR_CAT") {
                df_filter <- df_inter %>% filter(PEAK_HR_CAT == unique_grp[[i]])
            }

            if (input$hr_mth_d == "BOTH") {
                df_filter <- df_inter %>% filter(concat == unique_grp[[i]])
            }


            # od2line function
            network_inter <- od2line(flow = df_filter, zones = mpsz_sf_req)

            # dmap <- tm_shape(mpsz_sf) +
            #     tm_borders("grey25", alpha=.3) +
            #     tm_shape(desire_lines_top) +
            #     tm_lines(palette="Paired", col="TRIPS", lwd = wd_width) +
            #     tm_layout(panel.show = TRUE,
            #               panel.labels = unique_grp[[i]],
            #               panel.label.color = 'black',
            #               panel.label.size = 1.5,
            #               inner.margins = 0,
            #               legend.text.size = 0.8,
            #               frame=T) +
            #     tm_scale_bar(text.size = 1)

            dmap <- tm_shape(mpsz_sf) +
                tm_borders("grey25", alpha=.3) +
                tm_shape(network_inter) +
                tm_lines(palette="plasma",
                         col="TRIPS",
                         breaks = c(0, leg_int, leg_int*2, leg_int*3, leg_int*4, leg_int*5, leg_int*6),
                         lwd=2
                ) +
                tm_layout(panel.show = TRUE,
                          panel.labels = unique_grp[[i]],
                          panel.label.color = 'black',
                          panel.label.size = 0.7,
                          inner.margins = 0,
                          legend.text.size = 2,
                          frame=T) +
                tm_scale_bar(text.size = 1)


            dlines_list[[i]] <- dmap
        }
        ncol_list <- c(dlines_list, ncol=3)
        do.call(tmap_arrange, ncol_list)

    }, height=650, width=1100)
    
    


    # SIM - Unconstrained
    output$sim <- renderPlot({
        
        sim <- 0
        
        if (input$sim_model=="Unconstrained"){
            sim <- glm(TRIPS ~ log(df_inter[[input$un_o_var]]) + log(df_inter[[input$un_d_var]]) + log(dist), na.action = na.exclude, family = poisson(link = "log"), data = df_inter)
        }  else if (input$sim_model=="Origin"){
            sim <- glm(TRIPS ~ df_inter[["ORIGIN_SZ_C"]] + log(df_inter[[input$o_var]]) + log(dist)-1, na.action = na.exclude, family = poisson(link = "log"), data = df_inter)
        }  else if (input$sim_model=="Destination"){
            sim <- glm(TRIPS ~ df_inter[["DEST_SZ_C"]] + log(df_inter[[input$d_var]]) + log(dist)-1, na.action = na.exclude, family = poisson(link = "log"), data = df_inter)
        } else if (input$sim_model=="Doubly"){
            sim <- glm(TRIPS ~ df_inter[["ORIGIN_SZ_C"]] + df_inter[["DEST_SZ_C"]] + log(dist), na.action = na.exclude, family = poisson(link = "log"), data = df_inter)
        }
        
        output$sim_summary <- renderPrint({
            summary(sim)
        })
        
        df_inter$simFitted <- round(fitted(sim),0)

        # Original
        df_matrix <- dcast(df_inter, ORIGIN_SZ_C ~ DEST_SZ_C, sum, value.var = "TRIPS", margins=c("ORIGIN_SZ_C", "DEST_SZ_C"))
        
        output$original_matrix <- renderTable({
            head(df_matrix, 10)
        })
        
        # SIM
        df_matrix_sim <- dcast(df_inter, ORIGIN_SZ_C ~ DEST_SZ_C, sum, value.var = "simFitted", margins=c("ORIGIN_SZ_C", "DEST_SZ_C"))
        
        output$sim_matrix <- renderTable({
            head(df_matrix_sim, 10)
        })

        print(ggplot(data=df_inter,
                     aes(y = TRIPS,
                         x = `simFitted`))+
                  geom_point(color=input$colour_sim, fill="light blue") +
                  xlim(0,300000) +
                  ylim(0,400000)
                  )

        
        output$sim_results <- renderPrint({
            postResample(df_inter$TRIPS, df_inter$simFitted)
        })
        

    })
    
    
    
    
    # About SMU logo
    output$smu <- renderImage({
        width <-  session$clientData$output_news_map_width
        height <-  session$clientData$output_news_map_height
        
        list(
            src = "www/smu.png",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "SMU Logo"
        )
    }, deleteFile = FALSE)
    
    
    # About Singapore Map
    output$sg_map <- renderImage({
        # width <- session$clientData$output_news_map_width
        # height <- session$clientData$output_news_map_height
        
        list(
            src = "www/sg_map.jpeg",
            contentType = "image/jpeg",
            width = 500,
            height = 400,
            alt = "Singapore Map"
        )
    }, deleteFile = FALSE)
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)