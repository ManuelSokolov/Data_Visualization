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
library(ggmap)
library('rnaturalearth')
library(maps)
library(mapproj)

#world_map <- ne_countries(scale = "medium", returnclass = "sf")

unicorn_companies <- read.csv("data/Unicorn_Clean.csv")
industry_values <- unique(unicorn_companies$Industry)
print(colnames(unicorn_companies))
industry_choices <- unlist(industry_values)
print(industry_choices) 

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Tab 1 - Industry and Investors", value = "tab1",
           titlePanel("Are companies in certain industries more likely to attract certain investors?"),
           fluidRow(
             selectInput(inputId = "industry", 
                         label = h4(strong("Select a Industry Type:")), 
                         choices = industry_choices,
                         selected = NULL)
           ),
           plotOutput("industry_investors_plot", click = "plot_click")),
  tabPanel("Tab 2", value = "tab2",titlePanel("Is there any geographical pattern regarding investment?"),
           plotOutput("industry_investors_plot2", click = "plot_click"),
           ),
  tabPanel("Tab 3", value = "tab3",
           titlePanel("What makes a company more valuated?"))
)

server <- function(input, output) {
  # Read in the unicorn companies dataset
  #print(unique(unicorn_companies$Industry))
#  print(names(unicorn_companies))
  #print(input$industry)
  industry_select <- reactive({input$industry})
  print(industry_select)
  #output$industry_investors_plot <- renderPlot({
  #  industry_investors_data <- unicorn_companies %>% 
  #    select(Industry, Company)
  #  industry_investors_data <- reshape2::melt(industry_investors_data, id.vars = "Company")
  #  ggplot(data = industry_investors_data, aes(x=Company, y=value, fill = variable)) + 
  #    geom_tile() + 
  #    scale_fill_manual(values = c("#999999", "#E69F00")) +
  #    theme_minimal() +
  #    ggtitle("Industry and Country correlation plot")
  #})
  
  output$industry_investors_plot <- renderPlot({
    industry_investors_data <- unicorn_companies %>%
      select(Industry, City)
    filtered_data <- unicorn_companies %>% filter(Industry == industry_select())
    investors_data <- filtered_data %>%
      gather("Investor", "name", Investor.1:Investor.4) %>%
      filter(name != "") %>%
      group_by(name) %>%
      summarise(n=n()) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) 
    ggplot(data = investors_data, aes(x= reorder(name, n), y=n, fill = name)) +
      geom_bar(stat = "identity") +
      ggtitle("Top Investors for selected Industry") +
      xlab("Investor") +
      ylab("Frequency")
  })
  
  
  
  output$industry_investors_plot2 <- renderPlot({
    # Get the average valuation for each country
    industry_investors_data <- unicorn_companies %>% 
      group_by(Country) %>% 
      summarize(Valuation = mean(Valuation...B.))
    #maping names from "Country" column to match with map_data
    industry_investors_data$Country <- mapvalues(industry_investors_data$Country, from = c("United States"), to = c("United States of America"))
    # Get the map data as a data frame
    world_map_data <- map_data("world")
    
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
  
  
  
}

shinyApp(ui = ui, server = server)



