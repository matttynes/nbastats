#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#save to r markdown
#write.csv(Your DataFrame,"Path where you'd like to export the DataFrame\\File Name.csv")
#    http://shiny.rstudio.com/
#
#fininding the gem players
#win share position ws, per, +-
##summary(stats$PLUS_MINUS)remo


library(tidyverse)
library(shiny)
library(readxl)
library(rstanarm)

stats <- read_excel("stats.xlsx",  skip = 1)


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Team Statistics",
             fluidPage(
                 titlePanel("Average Points Per Game"), 
                 p("NBA Team's and Player Averages"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "team",
                             label = "Select a Team",
                             choices =  c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavs", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards")
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
             p("Hello, this webiste aims to give a Moneyball approach to evaluating professinal basketball player's talent, where player effeciently is strictly based on statistics. The combination of advanced general statisics are used to graph our own version of win shares per player, or the expected outcome an indivual player has on winning."),
             h3("Dataset1"),
             p(a("Data1", href = "https://chrome.google.com/webstore/detail/nba-data-retriever/cibebblabkdibhnidfnipfnjkfbcmeha?hl=en"),
               "This first data set gives us defensive statistics for each player in the 2019-2020 NBA Playoffs."),
             h3("About Me"),
             p("My name is Matt Tynes and I study Government and African American Studies. 
             You can reach me at mtynes@college.harvard.edu.")))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        if (input$team == "Atlanta Hawks") {
            atl <- stats %>%
                
                #Here, the filter command takes the user's input from the sliderInput
                #and then sorts the data accordingly - doing the same process for all
                #the other variables
                
                filter(TEAM_ABBREVIATION == "ATL") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(atl)
        }
        
        else if (input$team == "Boston Celtics") {
            bos <- stats %>%
                
        
                
                filter(TEAM_ABBREVIATION == "BOS") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            print(bos)
            
        }
      else if (input$team == "Brooklyn Nets") {
        bkn <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "BKN") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(bkn)
        
      }
      else if (input$team == "Charlotte Hornets") {
        cha <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "CHA") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(cha)
        
      }
      else if (input$team == "Chicago Bulls") {
        chi <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "CHI") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(chi)
        
      }
      else if (input$team == "Cleveland Cavaliers") {
        cle <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "CLE") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(cle)
        
      }
      else if (input$team == "Dallas Mavs") {
        dal <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "DAL") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(dal)
        
      }
      else if (input$team == "Denver Nuggets") {
        den <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "DEN") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(den)
        
      }
      else if (input$team == "Detroit Pistons") {
        det <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "DET") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(det)
        
      }
      else if (input$team == "Golden State Warriors") {
        gsw <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "GSW") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(gsw)
        
      }
      else if (input$team == "Houston Rockets") {
        hou <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "HOU") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(hou)
        
      }
      else if (input$team == "Indiana Pacers") {
        ind <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "IND") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(ind)
        
      }
      else if (input$team == "Los Angeles Clippers") {
        lac <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "LAC") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(lac)
        
      }
      else if (input$team == "Los Angeles Lakers") {
        lal <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "LAL") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(lal)
        
      }
      else if (input$team == "Memphis Grizzlies") {
        mem <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "MEM") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(mem)
        
      }
      else if (input$team == "Miami Heat") {
        mia <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "MIA") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(mia)
        
      }
      else if (input$team == "Milwaukee Bucks") {
        mil <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "MIL") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(mil)
        
      }
      else if (input$team == "Minnesota Timberwolves") {
        min <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "MIN") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(min)
        
      }
      else if (input$team == "New Orleans Pelicans") {
        nop <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "NOP") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(nop)
        
      }
      else if (input$team == "New York Knicks") {
        nyk <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "NYK") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(nyk)
        
      }
      else if (input$team == "Oklahoma City Thunder") {
        okc <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "OKC") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(okc)
        
      }
      else if (input$team == "Orlando Magic") {
        orl <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "ORL") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(orl)
        
      }
      else if (input$team == "Philadelphia 76ers") {
        phi <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "PHI") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(phi)
        
      }
      else if (input$team == "Phoenix Suns") {
        phx <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "PHX") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(phx)
        
      }
      else if (input$team == "Portland Trail Blazers") {
        por <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "POR") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(por)
        
      }
      else if (input$team == "Sacramento Kings") {
        sqc <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "SAC") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(sqc)
        
      }
      else if (input$team == "San Antonio Spurs") {
        sas <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "SAS") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(sas)
        
      }
      else if (input$team == "Toronto Raptors") {
        tor <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "TOR") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Win Percantage") +
          xlab("Average Points Per Game") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(tor)
        
      }
      else if (input$team == "Utah Jazz") {
        uta <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "UTA") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(uta)
        
      }
      else if (input$team == "Washington Wizards") {
        was <- stats %>%
          
          
          
          filter(TEAM_ABBREVIATION == "WAS") %>%
          ggplot(aes(x = PLAYER_NAME, y = PTS)) +
          geom_bar(stat="identity") +
          ylab("Average Points Per Game") +
          xlab("Player Name") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(was)
        
      }
    })}



# Run the application 
shinyApp(ui = ui, server = server)

