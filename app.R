

library(shiny)
library(shinythemes)
library(tidyverse)
library(streamgraph)
library(ggplot2)

#source("meanscore.R")

# Load data
responses <- read.csv("data/Unicorn_Companies.csv")

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Unicorn Companies",
    
    # QUESTION 1
    tabPanel("Question 1",
           
    ),
    
    # QUESTION 2
    tabPanel("Question 2",
             fluidRow(titlePanel("Spending Habits according to the age"),
                      br(),
                      h4("On this page you will be able to study how the spending habits of individuals vary throughout their ages. "),
                      p("+ Choose the age range you want to study. By default, it will take the whole range."),
                      p("+ Choose the spending habits statements you want to view. By default, all of them will be represented."),
                      p("+ Click on Mean Score button to see the mean score in each age and statement or to go back to the original view."),
                      p("+ Use your mouse to see the streams details or the select menu to highlight one of them.")
                      
             ),

    ),
    
    # QUESTION 3
    tabPanel("Question 3", 
             fluidRow(titlePanel("Hobbies/Phobias vs Demographic Features"),
                      br(),
                      h4("On this page you will be able to study how individuals differ on their opinions about some hobbies or phobias depending on their demographic features."),
                      p("+ Choose between Phobias or Hobbies."),
                      p("+ Select the Phobias or Hobbies you want to study."),
                      p("+ You can choose the demographic feature of the individuals you want to study")
             )
  

    ),
))



# Define server logic
server <- function(input, output, session) {
  
  # QUESTION 1
  # age_encode <- reactive({
  #   Out <- responses
  #   if (!input$showage || (input$showage %% 2) == 0){
  #     size <-NULL
  #   }
  #   else{
  #     size <-responses$Age
  #     Out <- responses
  #   }
  # })
  
  # QUESTION 2
  # spending_range <- reactive({
  #   Out <- filter(spending, Age >= input$sliderAge[1], Age <= input$sliderAge[2])
  # })
  # 
  # spending_habits <- reactive({
  #   Out <- filter(spending_range(), Habits %in% input$checkSpending)
  # })
  # 
  # spending_final <- reactive({
  #   if (!input$mean || (input$mean %% 2) == 0) return(spending_habits())
  #   meanscore(spending_habits(), responses)
  # })
  # 
  
  
  # QUESTION 3
  # output$selectY <- renderUI({
  #   if(input$radio == "Phobias"){
  #     Out <- selectizeInput(
  #       "selectizeY",
  #       h4(strong("Select the Phobias you want to study:")),
  #       choices = (list("Flying" = "Flying", "Thunder/Lightning" = "Storm", "Darkness" = "Darkness", "Heights" = "Heights", "Spiders" = "Spiders", "Snakes" = "Snakes", "Rats" = "Rats", "Ageing" = "Ageing")),
  #       selected = NULL,
  #       multiple = TRUE)
  #   } else{
  #     Out <- selectizeInput(
  #       "selectizeY",
  #       h4(strong("Select the Hobbies you want to study:")),
  #       choices = (list("History" = "History", "Psychology" = "Psychology", "Politics" = "Politics", "Mathematics" = "Mathematics", "Physics" = "Physics", "Internet" = "Internet", "PC Software, Hardware" = "PC", "Biology" = "Biology", "Chemistry" = "Chemistry", "Geography" = "Geography", "Medicine" = "Medicine", "Law" = "Law", "Cars" = "Cars", "Religion" = "Religion", "Dancing" = "Dancing", "Writing" = "Writing", "Active sports" = "Active sports", "Gardening" = "Gardening", "Celebrity lifestyle" = "Celebrities", "Shopping" = "Shopping", "Theatre" = "Theatre", "Pets" = "Pets")),
  #       selected = NULL,
  #       multiple = TRUE)
  #   }
  # })
  
  # data1 <- reactive({
  #   if(input$selectizeX == "Gender" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_Gender_female.csv", sep = ",", head = T)
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_LeftRightHanded_left.csv", sep = ",", head = T)
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_OnlyChild_no.csv", sep = ",", head = T)
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_VillageTown_city.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_HouseBlock_flats.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Gender" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_Gender_female.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_LeftRightHanded_left.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_OnlyChild_no.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_VillageTown_city.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_HouseBlock_flats.csv", sep = ",", head = T)
  #     
  #   }
  # })
  # 
  # graph1_txt <- reactive({
  #   if(input$selectizeX == "Gender" && input$radio == "Phobias"){
  #     Out <- "Female"
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Phobias"){
  #     Out <- "Left-Handed"
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Phobias"){
  #     Out <- "Not-Only-Child"
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Phobias"){
  #     Out <- "From-City"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Phobias"){
  #     Out <- "Flat-living"
  #     
  #   }
  #   else if(input$selectizeX == "Gender" && input$radio == "Hobbies"){
  #     Out <- "Female"
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Hobbies"){
  #     Out <- "Left-Handed"
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Hobbies"){
  #     Out <- "Not-Only-Child"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Hobbies"){
  #     Out <- "From-City"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Hobbies"){
  #     Out <- "Flat-Living"
  #     
  #   }
  # })
  # 
  # data2 <- reactive({
  #   if(input$selectizeX == "Gender" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_Gender_male.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_LeftRightHanded_right.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_OnlyChild_yes.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_VillageTown_village.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Phobias"){
  #     Out <- read.csv(file = "data/Phobias/responses_HouseBlock_house.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Gender" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_Gender_male.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_LeftRightHanded_right.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_OnlyChild_yes.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_VillageTown_village.csv", sep = ",", head = T)
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Hobbies"){
  #     Out <- read.csv(file = "data/Hobbies/responses_HouseBlock_house.csv", sep = ",", head = T)
  #     
  #   }
  # })
  
  # graph2_txt <- reactive({
  #   if(input$selectizeX == "Gender" && input$radio == "Phobias"){
  #     Out <- "Male"
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Phobias"){
  #     Out <- "Right-Handed"
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Phobias"){
  #     Out <- "Only-Child"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Phobias"){
  #     Out <- "From-Village"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Phobias"){
  #     Out <- "House-Living"
  #     
  #   }
  #   else if(input$selectizeX == "Gender" && input$radio == "Hobbies"){
  #     Out <- "Male"
  #     
  #   }
  #   else if(input$selectizeX == "Left/Right handed" && input$radio == "Hobbies"){
  #     Out <- "Right-Handed"
  #     
  #   }
  #   else if(input$selectizeX == "Only child" && input$radio == "Hobbies"){
  #     Out <- "Only-Child"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood place" && input$radio == "Hobbies"){
  #     Out <- "From-Village"
  #     
  #   }
  #   else if(input$selectizeX == "Childhood house style" && input$radio == "Hobbies"){
  #     Out <- "House-Living"
  #     
  #   }
  # })
  # 
  # #Filter by Hobbies/Phobias features
  # data1_filtered <- reactive({
  #   if(input$radio == "Phobias"){
  #     Out <- subset(data1(), phobias %in% unlist(input$selectizeY))
  #   }
  #   else if(input$radio == "Hobbies"){
  #     Out <- subset(data1(), hobbies %in% unlist(input$selectizeY))
  #   }
  # })
  # 
  # data2_filtered <- reactive({
  #   if(input$radio == "Phobias"){
  #     Out <- subset(data2(), phobias %in% unlist(input$selectizeY))
  #   }
  #   else if(input$radio == "Hobbies"){
  #     Out <- subset(data2(), hobbies %in% unlist(input$selectizeY))
  #   }
  # })
  
  
  
  #### Outputs
  # QUESTION 1
  # output$plot <- renderPlot(
  #   if(length(input$music) != 0 && length(input$movies) != 0){
  #     if(input$music == "Dance"){
  #       x = responses$Dance
  #       xlabel <- "Dance, Disco and Funk music (scores)"
  #       
  #     }
  #     else if(input$music == "Folk"){
  #       x = responses$Folk
  #       xlabel <- "Folk music (scores)"
  #       
  #     }
  #     else if(input$music == "Country"){
  #       x = responses$Country
  #       xlabel <- "Country music (scores)"
  #       
  #     }
  #     else if(input$music == "Classical"){
  #       x = responses$Classical
  #       xlabel <- "Classical music (scores)"
  #       
  #     }
  #     else if(input$music == "Musical"){
  #       x = responses$Musical
  #       xlabel <- "Musicals' music (scores)"
  #       
  #     }
  #     else if(input$music == "Pop"){
  #       x = responses$Pop
  #       xlabel <- "Pop music (scores)"
  #       
  #     }
  #     else if(input$music == "Rock"){
  #       x = responses$Rock
  #       xlabel <- "Rock music (scores)"
  #       
  #     }
  #     else if(input$music == "MetalHardrock"){
  #       x = responses$MetalHardrock
  #       xlabel <- "Metal and Hard Rock music (scores)"
  #       
  #     }
  #     else if(input$music == "Punk"){
  #       x = responses$Punk
  #       xlabel <- "Punk music (scores)"
  #       
  #     }
  #     else if(input$music == "HiphopRap"){
  #       x = responses$HiphopRap
  #       xlabel <- "Hip hop and Rap music (scores)"
  #       
  #     }
  #     else if(input$music == "ReggaeSka"){
  #       x = responses$ReggaeSka
  #       xlabel <- "Reggae and Ska music (scores)"
  #       
  #     }
  #     else if(input$music == "SwingJazz"){
  #       x = responses$SwingJazz
  #       xlabel <- "Swing and Jazz music (scores)"
  #       
  #     }
  #     else if(input$music == "Rocknroll"){
  #       x = responses$Rocknroll
  #       xlabel <- "Rock n roll music (scores)"
  #       
  #     }
  #     else if(input$music == "Alternative"){
  #       x = responses$Alternative
  #       xlabel <- "Alternative music (scores)"
  #       
  #     }
  #     else if(input$music == "Latino"){
  #       x = responses$Latino
  #       xlabel <- "Latin music (scores)"
  #       
  #     }
  #     else if(input$music == "TechnoTrance"){
  #       x = responses$TechnoTrance
  #       xlabel <- "Techno and Trance music (scores)"
  #       
  #     }
  #     else if(input$music == "Opera"){
  #       x = responses$Opera
  #       xlabel <- "Opera music (scores)"
  #     }
  #     
  #     if(input$movies == "Horror"){
  #       y = responses$Horror
  #       ylabel <- "Horror movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Thriller"){
  #       y = responses$Thriller
  #       ylabel <- "Thriller movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Comedy"){
  #       y = responses$Comedy
  #       ylabel <- "Comedy movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Romantic"){
  #       y = responses$Romantic
  #       ylabel <- "Romantic movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Scifi"){
  #       y = responses$Scifi
  #       ylabel <- "Sci-fi movies (scores)"
  #       
  #     }
  #     else if(input$movies == "War"){
  #       y = responses$War
  #       ylabel <- "War movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Fantasy"){
  #       y = responses$Fantasy
  #       ylabel <- "Fantasy movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Animated"){
  #       y = responses$Animated
  #       ylabel <- "Animated movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Documentary"){
  #       y = responses$Documentary
  #       ylabel <- "Documentary movies (scores)"
  #       
  #     }
  #     else if(input$movies == "Western"){
  #       y = responses$Western
  #       ylabel <- "Western movies (scores)"
  #     }
  #     else if(input$movies == "Action"){
  #       y = responses$Action
  #       ylabel <- "Action movies (scores)"
  #     }
  #     
  #     # Replace blank spaces with NA to show them
  #     responses[responses == ""] <- NA  
  #     
  #     if(input$demographic == "Gender"){
  #       color <- responses$Gender
  #       scale <- scale_color_brewer(palette="Set1", name = "Color legend", na.value="grey")
  #       
  #     }
  #     else if(input$demographic == "Childhood place"){
  #       color <-responses[, c(149)]
  #       scale <- scale_color_brewer(palette="Set1", name = "Color legend", na.value="grey")
  #       
  #     }
  #     else if(input$demographic == "Childhood house style"){
  #       color <-responses[, c(150)]
  #       scale <- scale_color_brewer(palette="Set1", name = "Color legend", na.value="grey")
  #       
  #     }
  #     else if(input$demographic == "Listen"){
  #       color <-as.factor(responses$Music)
  #       levels(color) <- c("Nothing at all", "No", "Neutral", "Yes", "A lot")
  #       scale <- scale_color_manual(values=c("red3", "DarkOrange", "tan", "SpringGreen2", "green4"), name = "Color legend", na.value="black")
  #       
  #     }
  #     else if(input$demographic == "Watch"){
  #       color <- as.factor(responses$Movies)
  #       levels(color) <- c("Nothing at all", "No", "Neutral", "Yes", "A lot")
  #       scale <- scale_color_manual(values=c("red3", "DarkOrange", "tan", "SpringGreen2", "green4"), name = "Color legend", na.value="black")
  #       
  #     }
  #     else if(input$demographic == "None"){
  #       color <-NULL
  #       scale <- scale_color_brewer(palette="Set1", name = "Color legend", na.value="grey")
  #     }
  #     
  #     if(!input$showage || (input$showage %% 2) == 0){
  #       size <-NULL
  #     }else if ((input$showage %% 2) != 0){
  #       size <-responses$Age
  #     }
  #     
  #     age_encode()%>%
  #       ggplot(aes(x, y, size=size, color=color)) + geom_point(alpha = 0.7, position = "jitter", na.rm = TRUE) +
  #       xlab(xlabel) +
  #       ylab(ylabel) +
  #       scale_size(range = c(.1, 6), name="Age (years)") +
  #       scale +
  #       theme_minimal() +
  #       theme(legend.key.width = unit(2, "cm"), axis.title=element_text(size = 14), legend.text = element_text(size= 13),
  #             legend.title = element_text(size = 14),legend.direction = "vertical", legend.box = "vertical") +
  #       guides(colour = guide_legend(override.aes = list(size = 4)))
  #   }
  # )
  # 
  # # QUESTION 2
  # output$streamgraph <- renderStreamgraph({
  #   streamgraph(spending_final(), key="Habits", value="Scores", date="Age", scale="continuous", interactive="TRUE") %>%
  #     sg_legend(show=TRUE, label="Statement: ") %>%
  #     sg_fill_brewer("Set1")
  # }) 
  # 
  # # QUESTION 3
  # output$graph1 <- renderPlot(
  #   if(input$radio == "Phobias" && length(input$selectizeY) != 0){
  #     data1_filtered() %>%
  #       ggplot(aes(x = phobias, 
  #                  y = decimals,
  #                  fill = grades)) +
  #       geom_col() +
  #       geom_text(aes(label = percentage),
  #                 position = position_stack(vjust = 0.5),
  #                 color = "black",
  #                 fontface = "bold") +
  #       coord_flip() +
  #       scale_x_discrete() +
  #       scale_fill_manual(breaks = c("nada" = "No Fear", "poco" = "Low Fear", "Ni mucho ni poco" = "Average", "mucho" = "Very Fearful", "muchisimo" = "Extreme Fear"),
  #                         values = c(
  #                           "nada" = "green4",
  #                           "poco" = "SpringGreen2",
  #                           "Ni mucho ni poco" = "beige",
  #                           "mucho" = "DarkOrange",
  #                           "muchisimo" = "red3"
  #                         )) +
  #       labs(
  #         x = NULL,
  #         fill = NULL) +
  #       theme_minimal() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             panel.grid = element_blank(),
  #             axis.text = element_text(size = 13),
  #             legend.position = "top")
  #     
  #   }
  #   else if(input$radio == "Hobbies" && length(input$selectizeY) != 0){
  #     data1_filtered() %>%
  #       ggplot(aes(x = hobbies, 
  #                  y = decimals,
  #                  fill = grades)) +
  #       geom_col() +
  #       geom_text(aes(label = percentage),
  #                 position = position_stack(vjust = 0.5),
  #                 color = "black",
  #                 fontface = "bold") +
  #       coord_flip() +
  #       scale_x_discrete() +
  #       scale_fill_manual(breaks = c("nada" = "Worst", "poco" = "Poor", "Ni mucho ni poco" = "Average", "mucho" = "Good", "muchisimo" = "Excellent"),
  #                         values = c(
  #                           "muchisimo" = "green4",
  #                           "mucho" = "SpringGreen2",
  #                           "Ni mucho ni poco" = "beige",
  #                           "poco" = "DarkOrange",
  #                           "nada" = "red3"
  #                         )) +
  #       labs(
  #         x = NULL,
  #         fill = NULL) +
  #       theme_minimal() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             panel.grid = element_blank(),
  #             axis.text = element_text(size = 13),
  #             legend.position = "top")
  #   }
  # )
  # 
  # output$graph2 <- renderPlot(
  #   if(input$radio == "Phobias" && length(input$selectizeY) != 0){
  #     data2_filtered() %>%
  #       ggplot(aes(x = phobias, 
  #                  y = decimals,
  #                  fill = grades)) +
  #       geom_col() +
  #       geom_text(aes(label = percentage),
  #                 position = position_stack(vjust = 0.5),
  #                 color = "black",
  #                 fontface = "bold") +
  #       coord_flip() +
  #       scale_x_discrete() +
  #       scale_fill_manual(breaks = c("nada" = "No Fear", "poco" = "Low Fear", "Ni mucho ni poco" = "Average", "mucho" = "Very Fearful", "muchisimo" = "Extreme Fear"),
  #                         values = c(
  #                           "nada" = "green4",
  #                           "poco" = "SpringGreen2",
  #                           "Ni mucho ni poco" = "beige",
  #                           "mucho" = "DarkOrange",
  #                           "muchisimo" = "red3"
  #                         )) +
  #       labs(
  #         x = NULL,
  #         fill = NULL) +
  #       theme_minimal() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             panel.grid = element_blank(),
  #             axis.text = element_text(size = 13),
  #             legend.position = "top")
  #   }
  #   else if(input$radio == "Hobbies" && length(input$selectizeY) != 0){
  #     data2_filtered() %>%
  #       ggplot(aes(x = hobbies, 
  #                  y = decimals,
  #                  fill = grades)) +
  #       geom_col() +
  #       geom_text(aes(label = percentage),
  #                 position = position_stack(vjust = 0.5),
  #                 color = "black",
  #                 fontface = "bold") +
  #       coord_flip() +
  #       scale_x_discrete() +
  #       scale_fill_manual(breaks = c("nada" = "Worst", "poco" = "Poor", "Ni mucho ni poco" = "Average", "mucho" = "Good", "muchisimo" = "Excellent"),
  #                         values = c(
  #                           "muchisimo" = "green4",
  #                           "mucho" = "SpringGreen2",
  #                           "Ni mucho ni poco" = "beige",
  #                           "poco" = "DarkOrange",
  #                           "nada" = "red3"
  #                         )) +
  #       labs(
  #         x = NULL,
  #         fill = NULL) +
  #       theme_minimal() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             panel.grid = element_blank(),
  #             axis.text = element_text(size = 13),
  #             legend.position = "top")
  #     
  #   }
  # )
  # 
  # 
  # output$graph1_text <- renderText({paste0(graph1_txt(), " individuals' opinion about the selected ", input$radio, sep = "")})
  # output$graph2_text <- renderText({paste0(graph2_txt(), " individuals' opinion about the selected ", input$radio, seo = "")})
  # 
  # output$image_graph1 <- renderImage({
  #   if(input$radio == "Phobias"){
  #     filename <- normalizePath(file.path("legend_images/legend_phobias.png"))
  #     list(src = filename)
  #   }
  #   else if(input$radio == "Hobbies"){
  #     filename <- normalizePath(file.path("legend_images/legend_hobbies.png"))
  #     list(src = filename)
  #   }
  # }, deleteFile = FALSE)
  # 
  # output$image_graph2 <- renderImage({
  #   if(input$radio == "Phobias"){
  #     filename <- normalizePath(file.path("legend_images/legend_phobias.png"))
  #     list(src = filename)
  #   }
  #   else if(input$radio == "Hobbies"){
  #     filename <- normalizePath(file.path("legend_images/legend_hobbies.png"))
  #     list(src = filename)
  #   }
  # }, deleteFile = FALSE)
  # 
  # 
}

# Run the app ----
shinyApp(ui = ui, server = server)
