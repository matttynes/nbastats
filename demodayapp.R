# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#save to r markdown
#write.csv(Your DataFrame,"Path where you'd like to export the DataFrame\\File Name.csv")
#    http://shiny.rstudio.com/


library(tidyverse)
library(rstanarm)
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
    slice(1:50)

data_18 <- best %>% 
    filter(season == "2018") %>% 
    slice(1:50)

data_17 <- best %>% 
    filter(season == "2017") %>% 
    slice(1:50)

data_16 <- best %>% 
    filter(season == "2016") %>% 
    slice(1:50)

data_15 <- best %>% 
    filter(season == "2015") %>% 
    slice(1:75)

data_14 <- best %>% 
    filter(season == "2014") %>% 
    slice(1:50)



combined2 <- read_csv("Salaries.csv")
stats <- read_excel("stats.xlsx", skip = 1)

salaries_model <- combined2


# Define UI for application that draws a histogram

ui <- navbarPage(
    "NBA Moneyball",
    tabPanel("Offensive Statistics",
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
    tabPanel("Defensive Statistics",
             fluidPage(
                 titlePanel("National Basketball Association DRAYMOND Ratings"), 
                 p("The graph below displays the top 50 defensive players from the chosen NBA season (according to DRAYMOND)."), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "Year",
                             label = "Select a Season",
                             choices =  c("2014", "2015", "2016", "2017", "2018", "2019")
                         )),
                     mainPanel(plotOutput("plot2")))
             )),
    tabPanel("Salary Data",
             dataTableOutput('table'),
             titlePanel("Table of all NBA player's from  the 2017-2018 regular season"),
             p("This table displays each player rom the 2017-2018 season, along with their most important offensive averages, DRAYMOND average, and their actual and estimated contracts for that year."),
             h3("Who is worth it?"),
             p("This table produces the estimated contract for the 2017-2018 based on production. The combination of points scored and overall DRAYMOND rating gives each player their 'true' value as a player.")),
    tabPanel("Discussion",
             titlePanel("Which NBA players are Over or Underpaid?"),
             p("By find creating an equation with the intercept coefficient of both the offensive and defensive variables, then multiplying them by each indivual player's season averages, I was able to calculate what their projected salary should be based on their efficiency. Although baseline statistics like DRAYMOND and scoring are ideal to determine production, I believe that this data is limited. The effect of a player's effort and attitude can change lockeroom dynamics, and aid or harm how much a team wins. Factors like how well teammates 'gel' or get along, can make the unseen difference in the outcome of winning games. For this reason, many All-Star players, on winning teams, are said to be 'overpayed' by the data. I found that this is also because having a good DRAYMOND socre, or playing good defense, was found to be greatly underpaid compared to players that score more points. While offense pays, the combination of the two on differents teams lead to increased winning percentages. For example, the two leading scorers on each team in the NBA Finals this past season were Jimmy Butler of the Miami Heat and Lebron James of the Los Angeles Lakers. Both of these players constanlty step up to score clutch points at crutial moments in games, are great defenders during the playoffs, and are perfect examples of great leaders getting the best of out their teammates to win. Despite those facts, both of these plauyers are supposedly extremely overpaid by our data. This can be contributed to by the lack of pay for good defenders, as found in the stan_glm we made to find the median pay for DRAYMOND and point avergages, alomg with the unwritten traits that the best players dispplay without showing up on the stat sheet."),
             h3(""),
             p()),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this webiste aims to give a Moneyball approach to evaluating professinal basketball player's talent, where player effeciently is strictly based on statistics. The combination of advanced general statisics are used to graph our own version of win shares per player, or the expected outcome an indivual player has on winning. Due to my lifelong interest in pursuing a career in professional sports, I wanted to create an app that accuartley evaulted professional basketball player's talent. The goal of this project was to find what statistics were most important to win basketball games, and use those statistics to determine if players were being overpayer or underpayed. Winning basketball games comes down to two things: scoring more points than the opponent, and stopping the opponent through defense. For this reason I focused on points scored and an a defensive rating that combines different defensive statistics."),
             h3("Dataset 1"),
             p(a("Data1", href = "https://chrome.google.com/webstore/detail/nba-data-retriever/cibebblabkdibhnidfnipfnjkfbcmeha?hl=en"),
               "This first data set gives all of the statistical averages for every player on NBA teams during the 2019-2020 regular season."),
             p("The first category I focused on were each team and player's average points per game."),
             h3("Dataset 2"),
             p("The second catergory I focused on was defense. Because defense consists of factor's that do not appear on a box score, in game statistics, the best way to evaluate how player's defend is through D.R.A.Y.M.O.N.D. DRAYMOND, named after defensive player of the year, Draymond Green, stands for Defensive Rating Accounting for Yielding Minimal Openness by Nearest Defender, a better way to evaluate NBA defense. It is a is a plus-minus statistic measured per 100 possessions, where a score of 0 represents average defense. DRAYMOND data is limited due to outliers that skew the data. Players that played well in a minimal amount of possesions, exceed DRAYMOND averages in my top 50 players of each year. Yet, among players who ' have played at least 10,000 possessions over the past six seasons (the NBA’s opponents’ shooting data goes back to 2013-14), the top defender according to DRAYMOND is … Draymond Green, who has provided the Warriors with +3.2 points per 100 possessions of defensive value based on his scoring defense alone, not counting all of the other ways (e.g., steals) that he produces defensive value.' Draymond's company at the top of the list consists of other star players who are said to be 'overpayed', by the estimation's standards."),
             h3("About Me"),
             p("My name is Matt Tynes and I study Government and African American Studies. Upon graduation I plan on working for a professional sports team, and this movtivated me to find the 'gem' player's that are worth much more than team's pay them. 
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
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(atl)
        }
        
        else if (input$team == "Boston Celtics") {
            bos <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "BOS") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(bos)
            
        }
        else if (input$team == "Brooklyn Nets") {
            bkn <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "BKN") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(bkn)
            
        }
        else if (input$team == "Charlotte Hornets") {
            cha <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "CHA") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(cha)
            
        }
        else if (input$team == "Chicago Bulls") {
            chi <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "CHI") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(chi)
            
        }
        else if (input$team == "Cleveland Cavaliers") {
            cle <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "CLE") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(cle)
            
        }
        else if (input$team == "Dallas Mavs") {
            dal <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "DAL") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(dal)
            
        }
        else if (input$team == "Denver Nuggets") {
            den <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "DEN") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(den)
            
        }
        else if (input$team == "Detroit Pistons") {
            det <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "DET") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(det)
            
        }
        else if (input$team == "Golden State Warriors") {
            gsw <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "GSW") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(gsw)
            
        }
        else if (input$team == "Houston Rockets") {
            hou <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "HOU") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(hou)
            
        }
        else if (input$team == "Indiana Pacers") {
            ind <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "IND") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(ind)
            
        }
        else if (input$team == "Los Angeles Clippers") {
            lac <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "LAC") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(lac)
            
        }
        else if (input$team == "Los Angeles Lakers") {
            lal <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "LAL") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(lal)
            
        }
        else if (input$team == "Memphis Grizzlies") {
            mem <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "MEM") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(mem)
            
        }
        else if (input$team == "Miami Heat") {
            mia <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "MIA") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(mia)
            
        }
        else if (input$team == "Milwaukee Bucks") {
            mil <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "MIL") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(mil)
            
        }
        else if (input$team == "Minnesota Timberwolves") {
            min <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "MIN") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(min)
            
        }
        else if (input$team == "New Orleans Pelicans") {
            nop <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "NOP") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(nop)
            
        }
        else if (input$team == "New York Knicks") {
            nyk <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "NYK") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(nyk)
            
        }
        else if (input$team == "Oklahoma City Thunder") {
            okc <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "OKC") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(okc)
            
        }
        else if (input$team == "Orlando Magic") {
            orl <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "ORL") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(orl)
            
        }
        else if (input$team == "Philadelphia 76ers") {
            phi <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "PHI") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(phi)
            
        }
        else if (input$team == "Phoenix Suns") {
            phx <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "PHX") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(phx)
            
        }
        else if (input$team == "Portland Trail Blazers") {
            por <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "POR") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(por)
            
        }
        else if (input$team == "Sacramento Kings") {
            sqc <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "SAC") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(sqc)
            
        }
        else if (input$team == "San Antonio Spurs") {
            sas <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "SAS") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(sas)
            
        }
        else if (input$team == "Toronto Raptors") {
            tor <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "TOR") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Win Percantage") +
                xlab("Average Points Per Game") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(tor)
            
        }
        else if (input$team == "Utah Jazz") {
            uta <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "UTA") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(uta)
            
        }
        else if (input$team == "Washington Wizards") {
            was <- stats %>%
                
                
                
                filter(TEAM_ABBREVIATION == "WAS") %>%
                ggplot(aes(x = PLAYER_NAME, y = PTS)) +
                geom_bar(stat="identity") +
                ylab("Average Points Per Game") +
                xlab("Player Name") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(was)
            
        }
        
    })
    output$plot2 <- renderPlot({
        if (input$Year == "2019") {
            season_19 <- data_19 %>%
                
                #Here, the filter command takes the user's input from the sliderInput
                #and then sorts the data accordingly - doing the same process for all
                #the other variables
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(season_19)
        }
        
        else if (input$Year == "2018") {
            season_18 <- data_18 %>%
                
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(season_18)
        }
        
        else if (input$Year == "2017") {
            season_17 <- data_17 %>%
                
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(season_17)
        }
        
        else if (input$Year == "2016") {
            season_16 <- data_16 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(season_16)
        }
        
        else if (input$Year == "2015") {
            season_15 <- data_15 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle =50, vjust = 1, hjust=1))
            print(season_15)
        }
        else if (input$Year == "2014") {
            season_14 <- data_14 %>%
                
                ggplot(aes(x = player, y = DRAYMOND)) +
                geom_point() +
                ylab("Defensive Rating") +
                xlab("") +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))
            print(season_14)
        }
        
        
        output$table <- renderDataTable(combined2)
        
    })
    salaries_model <- combined2 %>% 
        filter(player, differenceinsalary , estimatedsalary, salary) 
  
    
    output$table <- renderDataTable(salaries_model)   
    print(salaries_model)
}



# Run the application 
shinyApp(ui = ui, server = server)

