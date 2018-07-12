library(shiny)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(sf)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(stringr)
library(scales)

## Loading data

path = "./data/" # Jacob's path

sites <- read_sf(paste0(path, "sites_dat.shp")) %>%
  st_transform(4326)

blocks <- read_sf(paste0(path, "cntys.shp")) %>%
  rename_all(tolower) %>%
  mutate(state = setNames(state.abb, state.name)[state])

m1_polys <- read_sf(paste0(path, "m1_polys.shp")) %>%
  st_transform(4326)

m2_polys <- read_sf(paste0(path, "m2_polys.shp")) %>%
  st_transform(4326)

m3_polys <- read_sf(paste0(path, "m3_polys.shp")) %>%
  st_transform(4326) %>%
  rename(flood_rank = fld_rnk,
         location = locatin,
         e_totpop = e_totpp,
         perc_itrsct = prc_trs,
         wgtd_pop = wgtd_pp)


## Prepping data

sites_df <- data.frame(sites)
sites_df <- as_data_frame(sites_df)
sites_df$state <- as.character(sites_df$state)
sites_df$method <- as.character(sites_df$method)
sites_df$SoVI_div <- round(sites_df$value/sites_df$sm__ttp,3)
#sites_df$index_calc <- str_sub(sites_df$metric,0,3)

#### putting max values in separate column for comparison #####

sites_df$theme <- paste0(str_sub(sites_df$metric,-1), "_", 
                         sites_df$site_id, "_", 
                         sites_df$method, "_", 
                         sites_df$buffer)

pop_weighted_metrics <- list("pop_wgtd_rpl_t1", "pop_wgtd_rpl_t2", "pop_wgtd_rpl_t3",
                             "pop_wgtd_rpl_t4", "pop_wgtd_rpl_themes")
max_metrics <- list("max_rpl_t1", "max_rpl_t2", "max_rpl_t3", "max_rpl_t4", "max_rpl_themes")

df1 <- sites_df %>% filter(metric %in% pop_weighted_metrics)
df2 <- sites_df %>% filter(metric %in% max_metrics) %>%
  select("max_value" = value,
         "theme" = theme)
df <- inner_join(df1, df2, by = "theme") 
df$buffer[is.na(df$buffer)] <- 0
df$std_pop <- rescale(df$value, to = c(0, 1))

method_aa <- df %>%
  filter(method == "aa")

method_uhc <- df %>%
  filter(method == "uhc")

method_bi <- df %>%
  filter(method == "bi")

####                                                     #####

instructions <- "testing"
data_overview <- "test two"




## Studying data

#df <- data.frame(m1_polys)
#df <- as_data_frame(df)
#test <- df %>% filter(metric == "max_rpl_t2")
#min(test$value)

#unique(m3_polys$metric)
#pop_weighted_metrics <- list("pop_wgtd_rpl_t1", "pop_wgtd_rpl_t2", "pop_wgtd_rpl_t3",
#"pop_wgtd_rpl_t4", "pop_wgtd_rpl_themes")
#max_m3 <- max(m2_polys$value, na.rm = TRUE)
#min_m3 <- m2_polys %>% filter(metric %in% pop_weighted_metrics)
#min_M3 <- min(min_m3$value, na.rm = TRUE)

#unique(sites_df$method)



## Setting up environment

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

pal <- colorBin(palette = "OrRd",
                domain = c(0, 14000))



## App coding start

ui <- fluidPage(
  
  titlePanel("Comparing Spatial Methodologies in Risk-Impact Analyses"),
  
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       helpText("Determine which Superfund sites are the most dangerous if flooded for the surrounding populations"),
                       selectInput("states", 
                                   "Choose which States to focus on",
                                   choices = list("Alabama" = "AL",
                                                  "Connecticut" = "CT",
                                                  "Delaware" = "DE",
                                                  "Florida" = "FL",
                                                  "Georgia" = "GA",
                                                  "Louisiana" = "LA",
                                                  "Massachusettes" = "MA",
                                                  "Maine" = "ME",
                                                  "Maryland" = "MD",
                                                  "Missouri" = "MS",
                                                  "North Carolina" = "NC",
                                                  "New Jersey" = "NJ",
                                                  "New York" = "NY",
                                                  "Pennsylvania" = "PA",
                                                  "South Carolina" = "SC",
                                                  "Texas" = "TX",
                                                  "Virginia" = "VA"
                                                  ),
                                   multiple = TRUE,
                                   selected = list("FL", "LA")
                                   ),
                       radioButtons("vulnerability_index", 
                                          "Social Vulnerability Index (SoVI)",
                                          choices = list("Socioeconomic Status" = "pop_wgtd_rpl_t1",
                                                         "Household Composition & Disability" = "pop_wgtd_rpl_t2",
                                                         "Minority Status & Language" = "pop_wgtd_rpl_t3",
                                                         "Housing and Transportation" = "pop_wgtd_rpl_t4",
                                                         "Composite" = "pop_wgtd_rpl_themes"),
                                          selected = "pop_wgtd_rpl_themes"
                                    ),
                       checkboxGroupInput("flood_risk", 
                                          "Flood Risk level (5 = most extreme)",
                                          choices = list("1" = 1,
                                                         "2" = 2,
                                                         "3" = 3,
                                                         "4" = 4,
                                                         "5" = 5),
                                          selected = 1
                                          ),
                       radioButtons("method", 
                                          "Methodology",
                                          choices = list("Unit-Hazard Coincidence" = "uhc",
                                                         "Buffered Intersection" = "bi",
                                                         "Areal Apportionment" = "aa"
                                                         ),
                                          selected = "uhc"
                                          ),
                       radioButtons("buffer",
                                    "Buffer (km)",
                                    choices = list("1" = 1000,
                                                   "2" = 2000,
                                                   "3" = 3000,
                                                   "4" = 4000,
                                                   "5" = 5000),
                                    selected = 1000
                                    ),
                       width = 3
                        ),
      conditionalPanel(condition = "input.tabselected==2",
                       helpText("Visualize differences in the datasets."),
                       selectInput("states_plot", 
                                   "Choose which States to focus on",
                                   choices = list("Alabama" = "AL",
                                                  "Connecticut" = "CT",
                                                  "Delaware" = "DE",
                                                  "Florida" = "FL",
                                                  "Georgia" = "GA",
                                                  "Louisiana" = "LA",
                                                  "Massachusettes" = "MA",
                                                  "Maine" = "ME",
                                                  "Maryland" = "MD",
                                                  "Missouri" = "MS",
                                                  "North Carolina" = "NC",
                                                  "New Jersey" = "NJ",
                                                  "New York" = "NY",
                                                  "Pennsylvania" = "PA",
                                                  "South Carolina" = "SC",
                                                  "Texas" = "TX",
                                                  "Virginia" = "VA"
                                                  ),
                                   multiple = TRUE,
                                   selected = list("FL", "GA")
                                   ),
                       checkboxGroupInput("vulnerability_index_plot", 
                                    "Social Vulnerability Index (SoVI)",
                                    choices = list("Socioeconomic Status" = "pop_wgtd_rpl_t1",
                                                   "Household Composition & Disability" = "pop_wgtd_rpl_t2",
                                                   "Minority Status & Language" = "pop_wgtd_rpl_t3",
                                                   "Housing and Transportation" = "pop_wgtd_rpl_t4",
                                                   "Composite" = "pop_wgtd_rpl_themes"),
                                    selected = "pop_wgtd_rpl_themes"
                                    ),
                       checkboxGroupInput("flood_risk_plot", 
                                          "Flood Risk level (5 = most extreme)",
                                          choices = list("1" = 1,
                                                         "2" = 2,
                                                         "3" = 3,
                                                         "4" = 4,
                                                         "5" = 5),
                                          selected = 1
                       ),
                       checkboxGroupInput("method_plot", 
                                    "Methodology",
                                    choices = list("Unit-Hazard Coincidence" = "uhc",
                                                   "Buffered Intersection" = "bi",
                                                   "Areal Apportionment" = "aa"
                                    ),
                                    selected = "uhc"
                       ),
                       checkboxGroupInput("buffer_plot",
                                    "Buffer (km)",
                                    choices = list("1" = 1000,
                                                   "2" = 2000,
                                                   "3" = 3000,
                                                   "4" = 4000,
                                                   "5" = 5000),
                                    selected = 1000
                       ),
                       width = 3
                       )
      
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Explore", value=1,
                           leafletOutput("superfundmap"),
                           dataTableOutput('table')),
                  tabPanel("Compare", value=1,
                           h3("Social Vulnerabilities associated with each Superfund Site 
                              based on Methodology"),
                           radioButtons("SoVI_calculation",
                                              "SoVI Calculation",
                                              choices = list("Maximum Score" = "Max",
                                                             "Population Weighted" = "Weighted",
                                                             "Population Normalized" = "Normalized"),
                                              selected = "Weighted"
                                              ),
                           plotOutput("bar_plot"),
                           plotOutput("legend")
                           
                           #dataTableOutput("table_uhc"),
                           #dataTableOutput("table_aa"),
                           #dataTableOutput("table_bi")
                           ),
                  tabPanel("Instructions", value=2, 
                           h3("How to use this app"),
                           p(instructions)), 
                  tabPanel("Data/Pre-Processing", value=2,
                          h3("Data Collection and Pre-processing"),
                          p(data_overview)),
                  id = "tabselected"
                  )
      )
  )
  )

server <- function(input, output) {

   selectedData <- reactive({
     
     if(input$method == "uhc") {
       
       sites_df %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                method %in% input$method,
                metric %in% input$vulnerability_index) %>%
         mutate(buffer = "NA") %>%
         dplyr::select("Name" = name, 
                       "State" = state, 
                       "Flood Risk" = fld_rnk,
                       "Population" = sm__ttp,
                       "Population Weighted SoVI Score" = value,
                       "Raw SoVI" = SoVI_div)
   
     } else if(input$method == "bi") {
         
         sites_df %>%
           filter(state %in% input$states,
                  fld_rnk %in% input$flood_risk,
                  method %in% input$method,
                  buffer %in% input$buffer,
                  metric %in% input$vulnerability_index) %>%
           dplyr::select("Name" = name, 
                         "State" = state, 
                         "Flood Risk" = fld_rnk,
                         "Population" = sm__ttp,
                         "Population Weighted SoVI Score" = value,
                         "Raw SoVI" = SoVI_div)
       
       } else {
         
         sites_df %>%
           filter(state %in% input$states,
                  fld_rnk %in% input$flood_risk,
                  method %in% input$method,
                  buffer %in% input$buffer,
                  metric %in% input$vulnerability_index) %>%
           dplyr::select("Name" = name, 
                         "State" = state, 
                         "Flood Risk" = fld_rnk,
                         "Population" = sm__ttp,
                         "Population Weighted SoVI Score" = value,
                         "Population Normalized SoVI" = SoVI_div)
         }
     })
   
   selectedData_map <- reactive({
     
     if(input$method == "uhc") {
       
       sites %>%
         st_as_sf() %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                method %in% input$method,
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id, name, buffer, fld_rnk, sm__ttp, value)
       
     } else {
       
       sites %>%
         st_as_sf() %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                method %in% input$method,
                buffer %in% input$buffer,
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id, name, buffer, fld_rnk, sm__ttp, value)
       
     }
       
   })
   
  
   selectedData_comparePlots_aa <- reactive({
     
       if(input$SoVI_calculation == "Normalized") {
         df %>%
           filter(method == "aa") %>%
           filter(state %in% input$states,
                  fld_rnk %in% input$flood_risk,
                  buffer %in% list(input$buffer, 0),
                  metric %in% input$vulnerability_index) %>% 
           dplyr::select(site_id,
                         method,
                         "Site" = name, 
                         "population_total" = sm__ttp,
                         value,
                         std_pop,
                         max_value,
                         SoVI_div,
                         "plotting" =  SoVI_div)
         
       } else if(input$SoVI_calculation == "Weighted") {
         
         df %>%
           filter(method == "aa") %>%
           filter(state %in% input$states,
                  fld_rnk %in% input$flood_risk,
                  buffer %in% list(input$buffer, 0),
                  metric %in% input$vulnerability_index) %>% 
           dplyr::select(site_id,
                         method,
                         "Site" = name, 
                         "population_total" = sm__ttp,
                         value,
                         std_pop,
                         max_value,
                         SoVI_div,
                         "plotting" =  std_pop)
         
       } else if(input$SoVI_calculation == "Max") {
         
         df %>%
           filter(method == "aa") %>%
           filter(state %in% input$states,
                  fld_rnk %in% input$flood_risk,
                  buffer %in% list(input$buffer, 0),
                  metric %in% input$vulnerability_index) %>% 
           dplyr::select(site_id,
                         method,
                         "Site" = name, 
                         "population_total" = sm__ttp,
                         value,
                         std_pop,
                         max_value,
                         SoVI_div,
                         "plotting" = max_value)
       }
    
   })
   
   selectedData_comparePlots_bi <- reactive({
     
     if(input$SoVI_calculation == "Normalized") {
       df %>%
         filter(method == "bi") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" =  SoVI_div)
       
     } else if(input$SoVI_calculation == "Weighted") {
       
       df %>%
         filter(method == "bi") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" =  std_pop)
       
     }  else if(input$SoVI_calculation == "Max") {
       
       df %>%
         filter(method == "bi") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" = max_value)
       
     }
     
   })
   
   
   selectedData_comparePlots_uhc <- reactive({
     
     if(input$SoVI_calculation == "Normalized") {
       df %>%
         filter(method == "uhc") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" =  SoVI_div)
       
     } else if(input$SoVI_calculation == "Weighted") {
       
       df %>%
         filter(method == "uhc") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" =  std_pop)
       
     } else if(input$SoVI_calculation == "Max") {
       
       df %>%
         filter(method == "uhc") %>%
         filter(state %in% input$states,
                fld_rnk %in% input$flood_risk,
                buffer %in% list(input$buffer, 0),
                metric %in% input$vulnerability_index) %>% 
         dplyr::select(site_id,
                       method,
                       "Site" = name, 
                       "population_total" = sm__ttp,
                       value,
                       std_pop,
                       max_value,
                       SoVI_div,
                       "plotting" = max_value)
       
     }
   })
   
   
   
   selectedBlocks <- reactive({
     
     blocks %>%
       filter(st_abbr %in% input$states) %>%
       select(state, location) %>%
       st_transform(4326)
     
   })
   
   selectedPolys <- reactive({
     
     if(input$method == "uhc") {
       
       m1_polys %>%
         filter(st_abbr %in% input$states,
                flood_rank %in% input$flood_risk,
                metric %in% input$vulnerability_index) %>%
         select(state, location, value, e_totpop)
       
       } else if(input$method == "bi") {
       
         m2_polys %>%
           filter(st_abbr %in% input$states,
                  buffer %in% input$buffer,
                  flood_rank %in% input$flood_risk,
                  metric %in% input$vulnerability_index) %>%
           select(state, location, value, e_totpop)
         
         } else {
         
           m3_polys %>%
             filter(st_abbr %in% input$states,
                    buffer %in% input$buffer,
                    flood_rank %in% input$flood_risk,
                    metric %in% input$vulnerability_index) %>%
             dplyr::select(state, location, value, 
                           e_totpop, wgtd_pop, perc_itrsct)
           
           }
     })
     
   output$superfundmap <- renderLeaflet({
     
     leaflet() %>% 
       addProviderTiles(providers$OpenStreetMap.Mapnik,
                        options = providerTileOptions(noWrap = TRUE)
                        ) %>%
       setView(lat = 28.681389, lng = -82.460000, zoom = 5)
     
   })
   
   # Restructured the webmap to handle case in which no data is selected.
   
   observe({
     
     blocks <- selectedBlocks()
     polys <- selectedPolys()
     sites <- selectedData_map()
     #pal <- colorBin(palette = "RdYlBu",  # moved this to begining to have colors based on absolute values
                     #domain = polys$value)
     
     if(input$method == "aa") {
       
       polys_popup <- paste0("<b>Census block: </b>", polys$location, "<br>",
                             "<b>Census block population: </b>", polys$e_totpop, "<br>",
                             "<b>At risk population: </b>", polys$wgtd_pop, "<br>",
                             "<b>Census block Pop. Wgtd. SoVI: </b>", polys$value, "<br>",
                             "<b>Pop. Normalized SoVI: </b>", signif(polys$value / polys$e_totpop, 3), "<br>")
       
     } else {
       
       polys_popup <- paste0("<b>Census block: </b>", polys$location, "<br>",
                             "<b>Census block population: </b>", polys$e_totpop, "<br>",
                             "<b>Census block Pop. Wgtd. SoVI: </b>", polys$value, "<br>",
                             "<b>Raw SoVI: </b>", signif(polys$value / polys$e_totpop, 3), "<br>")
       
     }
     
     if(nrow(selectedData_map()) == 0) {
         leafletProxy("superfundmap") %>%
           clearShapes()
       }
       else {
         
         leafletProxy("superfundmap") %>%
           clearShapes() %>%
           clearMarkers() %>%
           addPolygons(data = blocks,
                       color = "white",
                       fillColor = "grey",
                       weight = 1,
                       fillOpacity = 0.3) %>%
           addPolygons(data = polys,
                       color = "white",
                       fillColor = ~pal(polys$value),
                       popup = polys_popup,
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0.65) %>%
           addMarkers(data = sites,
                      label = sites$name,
                      popup = paste0("<b>Name: </b>", sites$name, "<br>",
                                     "<b>Site ID: </b>", sites$site_id, "<br>",
                                     "<b>At risk population: </b>", sites$sm__ttp, "<br>",
                                     "<b>Pop. Wgtd. SoVI Score: </b>", sites$value, "<br>",
                                     "<b>Raw SoVI: </b>", signif(polys$value / polys$e_totpop, 3), "<br>"
                                     ))
       }
   })

   output$table <- renderDataTable(selectedData(), options = list(pageLength = 5))
   
   
   
   #output$soviVsPop <- renderPlot({
     
    # dat <- selectedData()
     
     #ggplot(dat) +
    #   geom_point(aes(x = Population, y = `Population Weighted SoVI Score`))
   #})
  
   #output$table_uhc <- renderDataTable(selectedData_comparePlots_uhc())
   #output$table_aa <- renderDataTable(selectedData_comparePlots_aa())  
   #output$table_bi <- renderDataTable(selectedData_comparePlots_bi())
   
   output$bar_plot <- renderPlot({
     
     method_aa <- selectedData_comparePlots_aa()
     method_bi <- selectedData_comparePlots_bi()
     method_uhc <- selectedData_comparePlots_uhc()
  
     
     ggplot() +
       # add a dummy variable for scaling purposes
       geom_point(aes(x = 1, y = method_aa$Site), ##name == Site
                  size = 0, col = "white") +

       # horizontal grid lines  ####  name == Site
       geom_hline(yintercept = 1:length(unique(method_aa$Site)), 
                  col = "grey80") +
       
       # add point for one of the things
       geom_point(aes(x = method_uhc$plotting, y = method_uhc$Site),
                  size = 12.5, col = "white") +
       geom_point(aes(x = method_uhc$plotting, y = method_uhc$Site),
                  size = 12, col = "#083D77") +
       geom_text(aes(x = method_uhc$plotting, y = method_uhc$Site,
                     label = round(method_uhc$plotting, 3)),
                 size = 3,
                 col = "white") +
      
       geom_point(aes(x = method_bi$plotting, y = method_bi$Site),
                  size = 12.5, col = "white") +
       geom_point(aes(x = method_bi$plotting, y = method_bi$Site),
                  size = 12, col = "#DA4167") +
       geom_text(aes(x = method_bi$plotting, y = method_bi$Site,
                     label = round(method_bi$plotting, 3)),
                 size = 3,
                 col = "white") +
       
       geom_point(aes(x = method_aa$plotting, y = method_aa$Site),
                  size = 12.5, col = "white") +
       geom_point(aes(x = method_aa$plotting, y = method_aa$Site),
                  size = 12, col = "#F4D35E") +
       geom_text(aes(x = method_aa$plotting, y = method_aa$Site,
                     label = round(method_aa$plotting, 3)),
                 size = 3,
                 col = "black") +
       
       
       theme_classic(base_size = 12) +
       scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
       scale_y_discrete(expand = c(0.2, 0)) +
       theme(axis.ticks = element_blank()) +
       scale_color_manual(name = "Method",
                          labels = c("Unit-Hazard Coincidence",
                                     "Buffered Intersection",
                                     "Areal Apportionment"),
                          values = c("#0072BB", "#FF4C3B", "#FFD034")) +
       xlab("Social Vulnerability Index") +
       ylab(" ")

     
     
   })
   
 
   output$legend <- renderPlot({
     ggplot() +
       
       theme_nothing() +
       # add a dummy variable for scaling purposes
       geom_point(aes(x = 8, y = 1), ##name == Site
                  size = 0, col = "white") +
       
       # add point for one of the things
       geom_point(aes(x = 1, y = 1),
                  size = 12, col = "#083D77") +
       geom_text(aes(x = 1.8, y = 1,
                     label = "Unit-Hazard Coincidence"),
                 col = "black") +
 
       geom_point(aes(x = 3, y = 1),
                  size = 12, col = "#DA4167") +
       geom_text(aes(x = 3.8, y = 1,
                     label = "Buffered Intersection"),
                 col = "black") +
       
       geom_point(aes(x = 5, y = 1),
                  size = 12, col = "#F4D35E") +
       geom_text(aes(x = 5.8, y = 1,
                     label = "Areal Apportionment"),
                     col = "black")
   },      
   height = 100)
   
   
    
    

}

shinyApp(ui = ui, server = server)



