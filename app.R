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


#world_map <- ne_countries(scale = "medium", returnclass = "sf")
# original data set
unicorn_companies <- read.csv("data/Unicorn_Companies.csv")

#dataset cleaned for first function - with investor 1, investor 2, investor 3
unicorn_companies_clean <- read.csv("data/Unicorn_Clean.csv")

#dataset with countries cleanes
unicorn_countries_cleaned <- read.csv('data/teste.csv')

#dataset cleaned for clustering (in future)
unicorn_companies_for_clustering <- read.csv("data/Unicorn_Companies_for_cluster.csv")

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
                         min = 0,
                         max = 30,
                         value = c(0, 30))
           ),
           plotOutput("industry_investors_plot", click = "plot_click")),
  
  
  tabPanel("Tab 2", value = "tab2 - Valuation and Total Raised ",
           titlePanel("Is Valuation correlated with total raised?"),
           plotOutput("clustering_plot", click = "plot_click"),
           #plotlyOutput("heatmap")
           ),
  
  tabPanel("Tab 3", value = "tab3",titlePanel("Is there any geographical pattern regarding investment?"),
           plotOutput("map_plot", click = "plot_click"),
  ),
  
  tabPanel("Tab 4", value = "tab4",
           titlePanel("What makes a company more valuated?"))
)

server <- function(input, output) {
  # Read in the unicorn companies dataset
  industry_select <- reactive({input$industry})
  #print(industry_select)
  nrInvestors <- reactive (max(as.numeric(input$range)))
  
  output$industry_investors_plot <- renderPlot({
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
      max_nrInvestors <- as.numeric(nrInvestors) %>%
      slice_head(n = max_nrInvestors)
    
    ggplot(data = investors_data, aes(x= reorder(name, n), y=n, fill = name)) +
      geom_bar(stat = "identity") +
      ggtitle("Top Investors for selected Industry") +
      xlab("Investor") +
      ylab("Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  output$map_plot <- renderPlot({
    # Get the average valuation for each country
    industry_investors_data <- unicorn_countries_cleaned %>% 
      group_by(Country) %>% 
      summarize(Valuation = mean(Valuation...B.))
    world_map_data <- map_data("world")
    #print(sort(unique(ggplot2::map_data("world")$region)))
    
    # Merge the map data with your data and fill in missing values
    world_map_valuation <- world_map_data %>% 
      left_join(industry_investors_data, by = c("region" = "Country")) %>%
      complete(region, Valuation)
    
     # Plot the map
    ggplot(data=world_map_valuation) + 
      geom_polygon(aes(x=long, y=lat, group=group, fill=Valuation)) +
      scale_fill_gradient(low = "white", high = "darkblue") +
      ggtitle("Map of the world by country valuation") +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_void()
  })
  
  
  
 # output$clustering_plot <- renderPlot({
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
   
    
    
   # heatmap(cluster_result)
    
  #un})
  
  output$clustering_plot <- renderPlot({
    print(colnames(unicorn_companies_for_clustering))
    valuation_total_raised <- unicorn_companies_for_clustering[, c("Valuation...B.", "Founded.Year")]
    valuation_total_raised$Valuation...B. <- as.numeric(sub("$","",valuation_total_raised$Valuation...B.))
    valuation_total_raised$Founded.Year <- as.numeric(gsub("[^0-9]", "", valuation_total_raised$Founded.Year))
  
    valuation_total_raised <- na.omit(valuation_total_raised)
    col_names <- names(valuation_total_raised)
    names(valuation_total_raised) <- col_names
    print(sum(is.na(valuation_total_raised)))

  
    # Set seed
    set.seed(1234)
    #heatmaply(industry_investor_frequencies)
    # Cluster Analysis - kmeans
    kmeans_basic <- kmeans(valuation_total_raised, centers = 5)
    kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
    kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, valuation_total_raised)
    # head of df
    head(kmeans_basic_df)
    
    # Fancy kmeans
    kmeans_fancy <- kmeans(scale(valuation_total_raised), 5, nstart = 100)
    # plot the clusters
    fviz_cluster(kmeans_fancy, data = scale(valuation_total_raised), geom = c("point"),ellipse.type = "euclid")
  })
  
}

shinyApp(ui = ui, server = server)



