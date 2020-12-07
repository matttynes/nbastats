# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#save to r markdown
#write.csv(Your DataFrame,"Path where you'd like to export the DataFrame\\File Name.csv")
#    http://shiny.rstudio.com/
#
#fininding the gem players
#stan_glm(formula = "money"~DRAYMOND + PTS + POSITION,data = data,
#refresh = 0)
#intercept co + DRAYMOND co x DRAYMOND + PTS co x PTS + POSITION co x PG

##summary(stats$PLUS_MINUS)remo
library(tidyverse)
#library(rstanarm)

library(shiny)
library(readxl)
stats <- read_excel("nba_draymond.xlsx")


stats2<-as.data.frame(stats)
stats2$season <- as.character(stats2$season)
stats2$possessions <- NULL
stats2$DRAYMOND<- as.numeric(stats2$DRAYMOND)

best <- stats2[order(-stats2$DRAYMOND),]

data_19 <- best %>% 
    filter(season == "2019") %>% 
    slice(1:100)

data_18 <- best %>% 
    filter(season == "2018") %>% 
    slice(1:100)

data_17 <- best %>% 
    filter(season == "2017") %>% 
    slice(1:100)

data_16 <- best %>% 
    filter(season == "2016") %>% 
    slice(1:100)

data_15 <- best %>% 
    filter(season == "2015") %>% 
    slice(1:100)

data_14 <- best %>% 
    filter(season == "2014") %>% 
    slice(1:100)



# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Team Statistics",
             fluidPage(
                 titlePanel("National Basketball Association"), 
                 p("Looking at NBA Teams and Individual Players"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "Year",
                             label = "Select a Season",
                             choices =  c("2014", "2015", "2016", "2017", "2018", "2019")
                         )),
                     mainPanel(plotOutput("plot1")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this webiste aims to give a Moneyball approoach to evaluating professinal basketball player's talent. The combination of advanced general statistis are used to graph our own version of win shares per player, or the expected outcome an indivual player has on winning."),
             h3("Dataset1"),
             p(a("Data1", href = "https://chrome.google.com/webstore/detail/nba-data-retriever/cibebblabkdibhnidfnipfnjkfbcmeha?hl=en"),
               "This first data set gives us defensive statistics for each player in the 2019-2020 NBA Playoffs."),
             h3("About Me"),
             p("My name is Matt Tynes and I study Government and African American Studies. 
             You can reach me at mtynes@college.harvard.edu.")))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        if (input$Year == "2019") {
            season_19 <- data_19 %>%
                
                #Here, the filter command takes the user's input from the sliderInput
                #and then sorts the data accordingly - doing the same process for all
                #the other variables
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
            print(season_19)
        }
        
        else if (input$Year == "2018") {
            season_18 <- data_18 %>%
                
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(season_18)
        }
        
        else if (input$Year == "2017") {
            season_17 <- data_17 %>%
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(season_17)
        }
        
        else if (input$Year == "2016") {
            season_16 <- data_16 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(season_16)
        }
        
        else if (input$Year == "2015") {
            season_15 <- data_15 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(season_15)
        }
        else if (input$Year == "2014") {
            season_14 <- data_14 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(season_14)
        }
        
        
        
        
    })}



# Run the application 
shinyApp(ui = ui, server = server)

