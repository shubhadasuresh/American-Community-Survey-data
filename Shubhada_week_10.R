library(shiny)
library(tidyverse)
library(tidycensus)
library(choroplethr)
library(leaflet)
library(choroplethrMaps)
library(sf)
library(ggplot2)



census_api_key("ad912fbb6d24b375fb848a1dce355bbd304706a3", install = TRUE, overwrite = TRUE)
data(state)


ui <- fluidPage(
  leafletOutput("map", height="600px"),
  absolutePanel(fixed = TRUE,top = 60, left = 20, right = "auto",
                draggable = TRUE, radioButtons("featureID", "Category",  
                                               choices = c("Median Household Income", "Median Gross Rent", "Ratio of Both"), selected = "Ratio of Both"),
                selectInput("stateID", "State:",
                            state.abb, selected = "NY", multiple = FALSE), style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;")
  , titlePanel("American Community Survey")
  
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    features_vec <- switch(input$featureID,
                           `Median Household Income` = "B19013_001",
                           `Median Gross Rent` = "B25064_001",
                           `Ratio of Both` = "B25074_001")
    state_df <- get_acs(geography = "tract", 
                        variables = features_vec, 
                        state = input$stateID , 
                        geometry = TRUE)
    
    pal <- colorQuantile("Blues", domain = state_df$estimate, n = 9,na.color = "#808080")
    state_df %>% st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.95,
                  color = ~ pal(estimate)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ estimate,
                title = "Range of Estimates",
                opacity = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


