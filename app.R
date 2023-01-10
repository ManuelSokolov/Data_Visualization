library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)


unicorn_companies <- read.csv("data/Unicorn_Clean.csv")
industry_values <- unique(unicorn_companies$Industry)
industry_choices <- map(industry_values, ~list(name = .x, value = .x))

#print(industry_values)
#industry_choices <- unlist(industry_values)
#print(industry_choices)

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Tab 1 - Industry and Investors", value = "tab1",
           titlePanel("Are companies in certain industries more likely to attract certain investors?"),
           fluidRow(
             column(3, style = "background-color: #F4F6F6; height: 580px; width: 230px",
                    selectInput("var",
                      inputId = "industry",
                      label = "Select a Industry Type:", 
                      choices = industry_choices,
                      selected = NULL,
                      multiple = FALSE
                      )
             )),
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
  print()
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
    filtered_data <- unicorn_companies %>% filter(Industry == input$var)
    investors_data <- filtered_data %>% 
      gather("Investor", "name", Investor.1:Investor.4) %>%
      group_by(name) %>%
      summarise(n=n()) %>%
      top_n(10)
    print(investors_data)
    ggplot(data = investors_data, aes(x=name, y=n, fill = name)) + 
      geom_bar(stat = "identity") +
      ggtitle("Top Investors for") +
      xlab("Investor") +
      ylab("Correlation")
    
  })
  
  output$industry_investors_plot2 <- renderPlot({
    industry_investors_data <- unicorn_companies %>% 
      select(Industry, City)
    industry_investors_data <- reshape2::melt(industry_investors_data, id.vars = "City")
    ggplot(data = industry_investors_data, aes(x=City, y=value, filal = variable)) + 
      geom_tile() + 
      scale_fill_manual(values = c("#999999", "#E69F00")) +
      theme_minimal() +
      ggtitle("Industry and City correlation plot")
  })
  
}

shinyApp(ui = ui, server = server)



