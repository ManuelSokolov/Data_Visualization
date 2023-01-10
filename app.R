library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
ui <- navbarPage(
  "My Shiny App",
  tabPanel("Tab 1 - Industry and Investors", value = "tab1",
           titlePanel("Are companies in certain industries more likely to attract certain investors?"),
           plotOutput("industry_investors_plot", click = "plot_click")),
  tabPanel("Tab 2", value = "tab2",titlePanel("Is there any geographical pattern regarding investment?"),
           plotOutput("industry_investors_plot2", click = "plot_click"),
           ),
  tabPanel("Tab 3", value = "tab3",
           titlePanel("What makes a company more valuated?"))
)

server <- function(input, output) {
  # Read in the unicorn companies dataset
  unicorn_companies <- read.csv("data/unicorn_companies.csv")
  print(unique(unicorn_companies$Industry))
  print(names(unicorn_companies))
  
  output$industry_investors_plot <- renderPlot({
    industry_investors_data <- unicorn_companies %>% 
      select(Industry, Company)
    industry_investors_data <- reshape2::melt(industry_investors_data, id.vars = "Company")
    ggplot(data = industry_investors_data, aes(x=Company, y=value, fill = variable)) + 
      geom_tile() + 
      scale_fill_manual(values = c("#999999", "#E69F00")) +
      theme_minimal() +
      ggtitle("Industry and Country correlation plot")
  })
  output$industry_investors_plot2 <- renderPlot({
    industry_investors_data <- unicorn_companies %>% 
      select(Industry, City)
    industry_investors_data <- reshape2::melt(industry_investors_data, id.vars = "City")
    ggplot(data = industry_investors_data, aes(x=City, y=value, fill = variable)) + 
      geom_tile() + 
      scale_fill_manual(values = c("#999999", "#E69F00")) +
      theme_minimal() +
      ggtitle("Industry and City correlation plot")
  })
  
}

shinyApp(ui = ui, server = server)



