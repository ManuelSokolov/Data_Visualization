library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
#install.packages("libzip")
#install.packages("png")
#install.packages("RgoogleMaps")
#install.packages("ggmap")
#install.packages("rnaturalearth")
#install.packages('maps')
#install.packages('heatmaply')
#install.packages('factoextra')
#install.packages('shinyHeatmaply')
#install.packages('shinyHeatmaply', dependencies = TRUE)
library(ggmap)
library('rnaturalearth')
library(maps)
library(mapproj)
library(factoextra)
library(shinyHeatmaply)
library(heatmaply)
#update.packages()
library(plotly)
# Install if needed by removing the #
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("FactoMineR")
#install.packages("factoextra")
# Load Libraries
library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(leaflet)
library(tidyverse)
library(forcats)



#world_map <- ne_countries(scale = "medium", returnclass = "sf")
# original data set
unicorn_companies <- read.csv("data/Unicorn_Companies.csv")

#dataset cleaned for first function - with investor 1, investor 2, investor 3
unicorn_companies_clean <- read.csv("data/Unicorn_Clean.csv")

#dataset with countries cleanes
unicorn_countries_clustering_cleaned <- read.csv('data/unicorn_ready_for_clustering_and_map.csv')

industry_values <- unique(unicorn_companies_clean$Industry)

industry_choices <- unlist(industry_values)

# numero de investidores por industria
industry_investor_frequencies <- read.csv("data/industry_investor_frequencies.csv")

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Tab 1 - Industry and Investors", value = "tab1",
           titlePanel("Are companies in certain industries more likely to attract certain investors?"),
           fluidRow(
             selectInput(inputId = "industry", 
                         label = h4(strong("Select a Industry Type:")), 
                         choices = industry_choices,
                         selected = NULL),
             sliderInput(inputId = "range",
                         label = "Number of Investors",
                         min = 1,
                         max = 20,
                         value = 10
           ),
           mainPanel(plotlyOutput("industry_investors_plot")))),
  
  
  tabPanel("Tab 2 - Valuation and Total Raised", value = "tab2",
           titlePanel("Is Valuation correlated with total raised?"),
           # not interactive
           plotOutput("clustering_plot", click = "plot_click"),
           sliderInput(inputId = "range_clusters",
                       label = "Number of clusters",
                       min = 2,
                       max = 10,
                       value = 5
           #interactive 
           
           #plotlyOutput("clustering_plot")
           )),
  
  tabPanel("Tab 3 - Map World Valuation", value = "tab3",titlePanel("Is there any geographical pattern regarding investment?"),
           leafletOutput("map_plot")
  ),
  
  tabPanel("Tab 4", value = "tab4",
           titlePanel("What makes a company more valuated?")
)
)
server <- function(input, output) {
  # Read in the unicorn companies dataset
  industry_select <- reactive({input$industry})
  #print(industry_select)
  
  #spending_range <- reactive({
   # Out <- filter(spending, Age >= input$sliderAge[1], Age <= input$sliderAge[2])
 # })
  
  #spending_habits <- reactive({
  #  Out <- filter(spending_range(), Habits %in% input$checkSpending)
 # })
  
  #spending_final <- reactive({
  #  if (!input$mean || (input$mean %% 2) == 0) return(spending_habits())
  #  meanscore(spending_habits(), responses)
  #})
  
  output$industry_investors_plot <- renderPlotly({
    industry_investors_data <- unicorn_companies_clean %>%
      select(Industry, City) %>% 
      filter(Industry == industry_select())
    filtered_data <- unicorn_companies_clean %>% filter(Industry == industry_select())
    investors_data <- filtered_data %>%
      gather("Investor", "name", Investor.1:Investor.4) %>%
      filter(name != "") %>%
      group_by(name) %>%
      summarise(n=n()) %>%
      arrange(desc(n)) %>%
      slice_head(n = max(input$range))
    #ggplot(data = investors_data, aes(x= reorder(name, n), y=n, fill = name)) +
    #  geom_bar(stat = "identity") +
    #  ggtitle("Top Investors for selected Industry") +
    #  xlab("Investor") +
    #  ylab("Frequency") +
    #  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #})
    #ggplotly(ggplot(data = investors_data, aes(x= reorder(name, n), y=n, fill = name)) +
    #          geom_bar(stat = "identity", aes(text = name), show.legend = F) +
    #           ggtitle("Top Investors for selected Industry") +
    #           xlab("Investor") +
    #           ylab("Frequency") +
    #           theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #)
    
    
    investors_data$name <- factor(investors_data$name, levels = investors_data$name[order(investors_data$n)])
    fig <- plot_ly(investors_data, x = reorder(investors_data$name, -investors_data$n), 
                   y = investors_data$n, type = 'bar', 
                   color = investors_data$name,
                   colors = RColorBrewer::brewer.pal(n = nrow(investors_data),
                                                     name = 'Set1'))
    fig <- fig %>% layout(title = "Top Investors for selected Industry")
    fig
    
    
    
    
  })
  
  output$clustering_plot <- renderPlot({
    valuation_total_raised <- unicorn_countries_clustering_cleaned[, c("Valuation...B.", "Total.Raised")]
    valuation_total_raised <- valuation_total_raised %>% drop_na()
    
    # Perform k-means clustering
    kmeans_fancy <- kmeans(scale(valuation_total_raised), max(input$range_clusters) , nstart = 100)
    
    # Add cluster column to the original dataframe
    unicorn_countries_clustering_cleaned$cluster <- kmeans_fancy$cluster
    
    # plot the clusters
    fviz_cluster(kmeans_fancy, data = scale(valuation_total_raised), geom = c("point"),ellipse.type = "euclid")
  })

  output$map_plot <- renderLeaflet({
    industry_investors_data <- unicorn_countries_clustering_cleaned %>% 
      group_by(Country) %>% 
      summarize(Valuation = mean(Valuation...B.))
    world_map_data <- map_data("world")
    #print(sort(unique(ggplot2::map_data("world")$region)))
    
    # Merge the map data with your data and fill in missing values
    world_map_valuation <- world_map_data %>% 
      right_join(industry_investors_data, by = c("region" = "Country")) %>%
      mutate(Valuation = coalesce(Valuation, 0.0))
    #data_subset1 <- subset(world_map_valuation, Valuation > 0.00001)
    
    data_subset <- world_map_valuation[!duplicated(world_map_valuation[ , c("region")]), ]
    print(typeof(data_subset))
    #print(data_subset1)
    print(data_subset)
    leaflet(data_subset) %>% addTiles() %>% 
      addCircleMarkers(lat = data_subset$lat, lng = data_subset$long, 
                       popup = paste("Country:",data_subset$region, "<br>", "Valuation:",data_subset$Valuation))
 
  })
  
  
  
 #output$steamGraph <- renderPlot({
    # Read in the data
  #  industry_investor_frequencies <- read.csv("data/industry_investor_frequencies.csv", row.names = 1)
    
    # Compute the Euclidean distance matrix between investors
   # distance_matrix <- dist(industry_investor_frequencies, method = "euclidean")
   
    # Perform hierarchical clustering on the distance matrix
    #cluster_result <- hclust(distance_matrix)
    #print(cluster_result)
    # Plot the dendrogram of the clustering result
    #plot(cluster_result)
    #heatmap(distance_matrix)
    # heatmap(cluster_result)#un})
  
 
}

shinyApp(ui = ui, server = server)



