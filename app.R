library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
#install.packages("libzip")
#install.packages("png")
#install.packages("RgoogleMaps")
#install.packages("ggmap")
install.packages("rnaturalearth")
#library("ggmap")
#library(rnaturalearth)

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
      gather("Investor", "name", Investor.1:Investor.4)# %>%
    investors_data <- investors_data %>% filter(name != "") %>%
      group_by(name) %>% 
      summarise(n=n()) %>%
      arrange(desc(n))
    write.csv(investors_data,"data/cenas.csv",row.names = FALSE)
   # print(type(investors_data))
  
   # print(investors_data)
    ggplot(data = investors_data, aes(x=name, y=n, fill = name)) + 
      geom_bar(stat = "identity") +
      ggtitle("Top Investors for selected Industry") +
      xlab("Investor") +
      ylab("Correlation")
    
  })
  output$industry_investors_plot2 <- renderPlot({
    industry_investors_data <- unicorn_companies %>% select(Country, Valuation...B)
    industry_investors_data <- group_by(industry_investors_data,Country) %>% summarise(Valuation = mean(Valuation...B))
    industry_investors_data$Valuation <- log10(industry_investors_data$Valuation)
    ggplot(data = industry_investors_data, aes(map_id = Country,fill=Valuation)) + 
      geom_sf(data = world_map) +
      scale_fill_gradientn(colours = rev(terrain.colors(5))) +
      theme_void() +
      ggtitle("Valuation by country")
  })
  
}

shinyApp(ui = ui, server = server)



