library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
install.packages("libzip")
install.packages("png")
install.packages("RgoogleMaps")
install.packages("ggmap")
install.packages("rnaturalearth")
install.packages('maps')
library("ggmap")
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
    # Get the average valuation for each country
    industry_investors_data <- unicorn_companies %>% 
      group_by(Country) %>% 
      summarize(Valuation = mean(Valuation...B))
    #merging the data 
    world_map_valuation <- merge(map('world',plot=F,fill=T), industry_investors_data, by.x = "region", by.y = "Country")
    # Plot the map
    ggplot()+geom_polygon(data=world_map_valuation,aes(x=long,y=lat,group=group,fill=Valuation))+
      scale_fill_gradient(low = "white", high = "darkblue")+
      ggtitle("Map of the world by country valuation") +
      xlab("Longitude") +
      ylab("Latitude")+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            legend.title=element_blank(),
            legend.text=element_text(size=10),
            legend.position="right")
  })
  
  
  
}

shinyApp(ui = ui, server = server)



