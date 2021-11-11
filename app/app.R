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
                    
                    # About 
                    tabPanel("About", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(h3(strong("FloSG"), style = "color: #ff4d4d"),
                                              br(),
                                              
                                              # img(src="../smu.JPG", height = 300),
                                              # imageOutput("www/smu.png", width = 3),
                                              
                                              h4("A Group Project by:"),
                                                tags$ul(
                                                    tags$li(a(href="https://www.linkedin.com/in/genice-goh/", "Genice Goh"), style = "font-size: 16px;"),
                                                    tags$li(a(href="https://www.linkedin.com/in/nor-aisyah/", "Nor Aisyah"), style = "font-size: 16px;"),
                                                    tags$li(a(href="https://www.linkedin.com/in/wei-ling-wong-a84130131/", "Wong Wei Ling"), style = "font-size: 16px;")
                                                ),
                                              br(),
                                              
                                              h5("Guided by Professor Kam Tin Seong for IS415 Geospatial Analytics and Applications")
                                              
                                              ), 
                                 
                                 mainPanel(h3("Project Motivation"),
                                           fluidRow(
                                               column(10, 
                                                      h4("There is presently no analytics application that looks at public transportation commuter patterns across time and space.
                                                         We hope that creating this application can bring benefits to multiple stakeholders:
                                                         by allowing commuters to better plan their journeys, workplaces to effectively implement flexible working hours,
                                                         and the relevant governmental organisations to implement measures to address peak hour crowds."),
                                                      br(),
                                                      h4("Since the pandemic, even though there are people who are less willing to take the public transport, there is still a
                                                         crowd in ridership at certain times. An example would be when Mass Rapid Transport (MRT) services broke down in October 2020,
                                                         this resulted in bus stops being packed with commuters looking for alternatives to get home. This is a cause for concern for
                                                         commuters who are conscious of COVID-19 risks as it is tough to maintain a safe distance from everyone around them."),
                                                      br(),
                                                      h4("The government has implemented measures to address peak hour crowds, including increasing the frequency of buses and trains
                                                         during these periods. While this does solve the issue, we wonder about the sustainability of increasing the frequency of
                                                         public transport - particularly buses, when public transportation ridership is going to increase over the years."))
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
                                           
                                           h3("Applications"),
                                           tags$ol(
                                               h4(tags$li("Exploratory Data Analysis (EDA): to visualize the distribution of bus stops in Singapore using Spatial Points Map as well as
                                                       Origin and Destination of commuter flow using Choropleth and Desire Lines Maps.")),
                                               h4(tags$li("Spatial Interaction Models: to determine how well the models are able to fit the empirical (origin, destination) data."))
                                           ),
                                           
                                           h3("R Blogdown Page"),
                                           h4("Do check out our R blogdown page for the full report here", a(href="https://flosg-is415.netlify.app/about.html", "here."))
                                           
                                           
                                           
                                ) # About mainPanel
                             ), # About sidebarlayout
                    ), # About tabPanel
                    
                    # EDA
                    tabPanel("EDA", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(
                                     
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
                                         sliderInput(inputId = "size",
                                                     label = "Size of Points",
                                                     min = 0.01,
                                                     max = 0.1,
                                                     value = c(0.03))
                                     ), # 1st conditionalPanel
                                     
                                     # Choropleth
                                     conditionalPanel(
                                         'input.EDA_var === "Choropleth Maps"',
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
                                                                 "Hour" = "TIME_PER_HOUR"),
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
                                                     label = "Hour/Month Interval:",
                                                     choices = c("Month" = "YEAR_MONTH",
                                                                 "Hour" = "TIME_PER_HOUR"),
                                                     selected = "YEAR_MONTH"),
                                         
                                         conditionalPanel(
                                             condition = "input.hr_mth_d =='YEAR_MONTH'",
                                             radioButtons("mth_var", label = h3("Month"),
                                                          choices = list("June" = "2021-06", "July" = "2021-07", "August" = "2021-08"), 
                                                          selected = "2021-06")
                                         ),



                                         sliderInput(inputId = "maxthres",
                                                     label = "Maximum Threshold:",
                                                     min = 10000,
                                                     max = 100000,
                                                     value = 50000,
                                                     step = 10000),


                                     ) # 3rd conditionalPanel
                                     
                                 ), #sidebarPanel
                                 
                                 
                                 mainPanel(
                                     tabsetPanel(
                                         id="EDA_var",
                                         tabPanel("Spatial Points", tmapOutput("pt_bus_stop")),
                                         # DT::dataTableOutput(outputId = "aTable") 
                                         tabPanel("Choropleth Maps", plotOutput(outputId =  "cmap")),
                                         tabPanel("Desire lines Maps", plotOutput(outputId =  "dmap"))
                                     )
                                     
                                 )
                                 
                                 
                             ) # EDA sidebarLayout
                    ), # EDA tabPanel
                    
                    
                    # Spatial Interaction Model
                    tabPanel("SIM", fluid = TRUE,
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     # SIM
                                     conditionalPanel(
                                         'input.SIM_var === "SIM"',
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
                                             radioButtons("un_o_var", label = h3("Origin"),
                                                          choices = list("Population" = "O_TOTAL_POP", "Income" = "O_MONTHLY_INCOME"), 
                                                          selected = "O_TOTAL_POP"),
                                             radioButtons("un_d_var", label = h3("Destination"),
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

                                     ) # 1st conditionalPanel
                                     
                                 ), # 2nd sidebarPanel
                                 
                                 mainPanel(
                                     tabsetPanel(
                                         id="SIM_var",
                                         tabPanel("SIM", plotOutput(outputId = "sim"),
                                                  tabsetPanel(
                                                      tabPanel(
                                                          "SIM Summary", verbatimTextOutput(outputId = "sim_summary")
                                                      ),
                                                      tabPanel(
                                                          "Original Matrix", tableOutput(outputId = "original_matrix")
                                                      ),
                                                      tabPanel(
                                                          "SIM Matrix", tableOutput(outputId = "sim_matrix")
                                                      )
                                                  ),
                                                  "SIM Results", verbatimTextOutput(outputId = "sim_results")
                                            )
                                         

                                         # DT::dataTableOutput(outputId = "aTable")
                                     )
                                     
                                 )
                                 
                             ) # SIM sidebarLayout
                    ) # SIM tabPanel
                    
                ) #navbarPage
                
) #fluidpage




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
                  geom_point(color=input$colour_sim, fill="light blue"))

        
        output$sim_results <- renderPrint({
            postResample(df_inter$TRIPS, df_inter$simFitted)
        })
        

    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)