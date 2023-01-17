library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggmap)
library(maps)
library(mapproj)
library(factoextra)
library(plotly)
library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(leaflet)
library(tidyverse)
library(forcats)

# original data set
unicorn_companies <- read.csv("data/Unicorn_Companies.csv")

#dataset cleaned for first function - with investor 1, investor 2, investor 3
unicorn_companies_clean <- read.csv("data/Unicorn_Clean.csv")

#dataset with countries cleaned
unicorn_countries_clustering_cleaned <- read.csv('data/unicorn_ready_for_clustering_and_map.csv')

industry_values <- unique(unicorn_companies_clean$Industry)

industry_choices <- unlist(industry_values)

industry_choices2 <- c(industry_choices, "All Industries")

industry_investor_frequencies <- read.csv("data/industry_investor_frequencies.csv")

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Tab 1 - Industry and Investors", value = "tab1",
           titlePanel("Which are the most likely investors for a new startup?"),
           sidebarPanel(
             selectInput(inputId = "industry", 
                         label = h4(strong("Select a Industry Type:")), 
                         choices = industry_choices,
                         selected = NULL),
             sliderInput(inputId = "range",
                         label = "Number of Investors",
                         min = 1,
                         max = 20,
                         value = 10
           )),
          
           mainPanel(plotlyOutput("industry_investors_plot"))),
  
  
  tabPanel("Tab 2 - Valuation and Total Raised", value = "tab2",
           titlePanel("Is Valuation correlated with total raised?"),
           # not interactive
           sidebarPanel(
             selectInput(inputId = "industry2", 
                         label = h4(strong("Select a Industry Type:")), 
                         choices = industry_choices2,
                         selected = "All Industries"),
             selectInput(inputId = "showC", 
                         label = h4(strong("Show Unicorn name:")), 
                         choices = c("No","Yes"),
                         selected = "All Industries"),
             sliderInput(inputId = "range_clusters",
                         label = "Number of clusters",
                         min = 2,
                         max = 6,
                         value = 2)
           ),
            
           #interactive 
           mainPanel(plotlyOutput("clustering_plot"))
           #plotlyOutput("clustering_plot")
           ),
  
  tabPanel("Tab 3 - Map World Valuation", value = "tab3",titlePanel("Is there any geographical pattern regarding investment?"),
           leafletOutput("map_plot")
  )
)
server <- function(input, output) {
  # Read in the unicorn companies dataset
  industry_select <- reactive({input$industry})
  industry_select2 <-  reactive({input$industry2})
  showC <- reactive({input$showC})

  output$industry_investors_plot <- renderPlotly({
    filtered_data <- unicorn_companies_clean %>% filter(Industry == industry_select())
    selectedX <- industry_select()
    investors_data <- filtered_data %>%
      gather("Investor", "name", Investor.1:Investor.4) %>%
      filter(name != "") %>%
      group_by(name) %>%
      summarise(n=n()) %>%
      arrange(desc(n)) %>%
      slice_head(n = max(input$range))

    investors_data$name <- factor(investors_data$name, levels = investors_data$name[order(investors_data$n)])
    fig <- plot_ly(investors_data, x = reorder(investors_data$name, -investors_data$n), 
                   y = investors_data$n, type = 'bar', 
                   color = investors_data$name,
                   colors = RColorBrewer::brewer.pal(n = nrow(investors_data),
                                                     name = 'Set1'))
    fig <- fig %>% layout(title = paste("Top Investors for",selectedX))
    fig
    
    
    
    
  })
  
  output$clustering_plot <- renderPlotly({
    if(industry_select2() != "All Industries") {
      unicorn_countries_clustering_cleaned <- read.csv('data/unicorn_ready_for_clustering_and_map.csv')
      unicorn_countries_clustering_cleaned <- unicorn_countries_clustering_cleaned %>% filter(Industry == industry_select2())
      valuation_total_raised <- unicorn_countries_clustering_cleaned[, c("Valuation...B.", "Total.Raised")]
      
      rownames(valuation_total_raised) <- unicorn_countries_clustering_cleaned$Company
      valuation_total_raised <- valuation_total_raised %>% drop_na()
      
      # Perform k-means clustering
      kmeans_fancy <- kmeans(valuation_total_raised, max(input$range_clusters) , nstart = 100)
      
      # Add cluster column to the original dataframe
      unicorn_countries_clustering_cleaned$cluster <- kmeans_fancy$cluster
      # Create the ggplot2 object
      
      if (showC() == "Yes"){
        plot <- fviz_cluster(kmeans_fancy, data = valuation_total_raised, 
                             geom = c("point", "text"),
                             ellipse.type = "convex") + xlim(0,20) + ylim(0,20)
      }else {
        plot <- fviz_cluster(kmeans_fancy, data = valuation_total_raised, 
                             geom = c("point"),
                             ellipse.type = "convex") + xlim(0,20) + ylim(0,20)
      }
      # Convert the ggplot2 object to an interactive plotly object
      plotly_plot <- plotly_build(plot, unicorn_countries_clustering_cleaned$Company)
      
      
      # Show the interactive plotly object
      plotly_plot
    }else{
      unicorn_countries_clustering_cleaned <- read.csv('data/unicorn_ready_for_clustering_and_map.csv')
      valuation_total_raised <- unicorn_countries_clustering_cleaned[, c("Valuation...B.", "Total.Raised")]
      
      rownames(valuation_total_raised) <- unicorn_countries_clustering_cleaned$Company
      valuation_total_raised <- valuation_total_raised %>% drop_na()
      
      # Perform k-means clustering
      kmeans_fancy <- kmeans(valuation_total_raised, max(input$range_clusters) , nstart = 100)
      
      # Add cluster column to the original dataframe
      unicorn_countries_clustering_cleaned$cluster <- kmeans_fancy$cluster
      # Create the ggplot2 object
      
      if (showC() == "Yes"){
        plot <- fviz_cluster(kmeans_fancy, data = valuation_total_raised, 
                             geom = c("point", "text"),
                             ellipse.type = "convex") + xlim(0,20) + ylim(0,20)
      }else {
        plot <- fviz_cluster(kmeans_fancy, data = valuation_total_raised, 
                             geom = c("point"),
                             ellipse.type = "convex") + xlim(0,20) + ylim(0,20)
      }
      # Convert the ggplot2 object to an interactive plotly object
      plotly_plot <- plotly_build(plot, unicorn_countries_clustering_cleaned$Company)
      
      
      # Show the interactive plotly object
      plotly_plot
    }
    
    
   
  })

  output$map_plot <- renderLeaflet({
    
    industry_investors_data <- unicorn_countries_clustering_cleaned %>% 
      group_by(Country) %>% 
      summarize(Valuation = sum(Valuation...B.))
    world_map_data <- map_data("world")
    # Merge the map data with your data and fill in missing values
    world_map_valuation <- world_map_data %>% 
      right_join(industry_investors_data, by = c("region" = "Country")) %>%
      mutate(Valuation = coalesce(Valuation, 0.0))
   # valuation_total_raised <- valuation_total_raised %>% filter(Valuation...B. > 0 & Total.Raised > 0)
    
    #data_subset1 <- subset(world_map_valuation, Valuation > 0.00001)
    data_subset <- world_map_valuation[!duplicated(world_map_valuation[ , c("region")]), ]
    capital_coordinates_dataset <- read.csv('data/country-capitals_clean.csv')[, c("CountryName", "CapitalName","CapitalLatitude","CapitalLongitude")]
  
    capital_coordinates_dataset <- capital_coordinates_dataset %>% 
        right_join(data_subset, by = c("CountryName" = "region"))

    col_scale <- colorNumeric(palette = "blue", domain = c(min(capital_coordinates_dataset$Valuation), max(capital_coordinates_dataset$Valuation)))
    leaflet(capital_coordinates_dataset) %>% addTiles() %>% 
      addCircleMarkers(lat = as.numeric(capital_coordinates_dataset$CapitalLatitude), 
                       lng = as.numeric(capital_coordinates_dataset$CapitalLongitude), 
                       color = col_scale(capital_coordinates_dataset$Valuation),
                       popup = paste("Country:",capital_coordinates_dataset$CountryName, "<br>", "Valuation (Billion $):",capital_coordinates_dataset$Valuation))
    
  })

  
 
}

shinyApp(ui = ui, server = server)



