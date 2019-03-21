setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

library(dplyr)
library(bbplot)
library(ggplot2)

#Create a function to create a dataframe from the nba.stats
get_zone_data <- function(URL) {
  library(rjson)
  #Import data from JSON
  shotJSON <- fromJSON(file = URL, method="C")
  
  #Unlist shooting data and save it into a dataframe
  shotDF <- data.frame(matrix(unlist(shotJSON$resultSets[[3]]), ncol=23, byrow = TRUE))
  rm(shotJSON)
  
  #There are three columns that appear in the downloaded data which doesn't appear in the site, so I don't
  #have a way of knowing what they are. Therefore I will simply remove them
  shotDF <- shotDF[,1:20]
  
  #Give meaningful names to the columns of the dataframe
  colnames(shotDF) <- c("TEAM_ID", "TEAM", 
                        "FGM_restricted_area", "FGA_restricted_area", "FG_PCT_restricted_area",
                        "FGM_paint_nonra", "FGA_paint_nonra", "FG_PCT_paint_nonra",
                        "FGM_mid_range", "FGA_mid_range", "FG_PCT_mid_range",
                        "FGM_left_corner_3pt", "FGA_left_corner_3pt", "FG_PCT_left_corner_3pt",
                        "FGM_right_corner_3pt", "FGA_right_corner_3pt", "FG_PCT_right_corner_3pt",
                        "FGM_above_the_break_3pt", "FGA_above_the_break_3pt", "FG_PCT_above_the_break_3pt")
  
  #Drop useless columns
  shotDF[,"TEAM_ID"] <- NULL
  
  #Transform some variables from factor into numeric
  library(magrittr)
  cols = c("FGM_restricted_area", "FGA_restricted_area", "FG_PCT_restricted_area",
           "FGM_paint_nonra", "FGA_paint_nonra", "FG_PCT_paint_nonra",
           "FGM_mid_range", "FGA_mid_range", "FG_PCT_mid_range",
           "FGM_left_corner_3pt", "FGA_left_corner_3pt", "FG_PCT_left_corner_3pt",
           "FGM_right_corner_3pt", "FGA_right_corner_3pt", "FG_PCT_right_corner_3pt",
           "FGM_above_the_break_3pt", "FGA_above_the_break_3pt", "FG_PCT_above_the_break_3pt")
  shotDF[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
  rm(cols)
  
  #Calculate some columns that might help the analysis: total 3-pointers, corner 3-pointers and paint shots. 
  shotDF$FGM_paint <- shotDF$FGM_restricted_area + shotDF$FGM_paint_nonra
  shotDF$FGA_paint <- shotDF$FGA_restricted_area + shotDF$FGA_paint_nonra
  shotDF$FG_PCT_paint <- shotDF$FGM_paint/shotDF$FGA_paint
  
  shotDF$FGM_corner_3pt <- shotDF$FGM_left_corner_3pt + shotDF$FGM_right_corner_3pt
  shotDF$FGA_corner_3pt <- shotDF$FGA_left_corner_3pt + shotDF$FGA_right_corner_3pt
  shotDF$FG_PCT_corner_3pt <- shotDF$FGM_corner_3pt/shotDF$FGA_corner_3pt
  
  shotDF$FGM_3pt <- shotDF$FGM_left_corner_3pt + shotDF$FGM_right_corner_3pt + shotDF$FGM_above_the_break_3pt
  shotDF$FGA_3pt <- shotDF$FGA_left_corner_3pt + shotDF$FGA_right_corner_3pt + shotDF$FGA_above_the_break_3pt
  shotDF$FG_PCT_3pt <- shotDF$FGM_3pt/shotDF$FGA_3pt
  
  #Return the created data set
  return(shotDF)
}


#Let's iterate to collect all the URLs with the team shooting data for different zones. The first season with
#available data is 1996-1997. So I'll collect all of them
urls = vector()
for (i in 2018:1996) {
  if (i >= 2010) {
    url <- paste("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=20",i-2000,"-",i-1999,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=", sep="")
    urls <- c(urls, url)
    } else if (i == 2009) {
    url <- "https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2009-10&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
    urls <- c(urls, url)
    } else if (i >= 2000 & i<2009) {
    url <- paste("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=20",0,i-2000,"-",0,i-1999,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=", sep="")
    urls <- c(urls, url)
    } else if (i == 1999) {
    url <- paste("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=1999-00&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=", sep="")
    urls <- c(urls, url)
    } else {
    url <- paste("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=19",i-1900,"-",i-1899,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=", sep="")
    urls <- c(urls, url)
    }
}

#Let's use a for loop in order to automate the process of creating a dataframe that contains every year
    
year <- 2019
df_total <- data.frame()
for(url in urls)  {
  df <- get_zone_data(url)
  df$Year <- year
  df_total <- rbind(df_total, df)
  rm(df)
  year <- year - 1
}         


#Create a new dataframe that aggregates the data by year, to see the averages for the whole league
league_averages <- df_total %>%
  group_by(Year) %>%
  #Take the yearly league average of the columns we want to conserve
  summarize_at(c("FGM_restricted_area", "FGA_restricted_area",
                 "FGM_paint_nonra", "FGA_paint_nonra", 
                 "FGM_mid_range", "FGA_mid_range",
                 "FGM_left_corner_3pt", "FGA_left_corner_3pt", 
                 "FGM_right_corner_3pt", "FGA_right_corner_3pt", 
                 "FGM_above_the_break_3pt", "FGA_above_the_break_3pt",
                 "FGM_paint", "FGA_paint",
                 "FGM_corner_3pt", "FGA_corner_3pt",
                 "FGM_3pt", "FGA_3pt"), 
               mean, 
               na.rm = T)

write.csv(league_averages, file = "../Data/league_shooting_yearly_averages.csv", row.names = F)


#PLOT THE EVOLUTION OF ATTEMPTS OF EVERY TYPE OF SHOT DURING THE YEARS
ggplot(data = league_averages) +
  geom_line(aes( x = Year, y = FGA_paint, col = " Inside the Paint     "), size = 2 ) +
  geom_line(aes( x = Year, y = FGA_3pt, col = " 3-Point Shot    "), size = 2 ) +
  geom_line(aes( x = Year, y = FGA_mid_range, col = " Mid Range Shot    "), size = 2 ) +
  labs( title= "The death of the mid-range shot",
        subtitle = "Evolution of shots per zone since the 1996-97 NBA season \n",
        caption = "Source: NBA Stats",
        y="Shots per game\n", 
        x = "Year")+
  scale_color_manual(values = c(" Inside the Paint     " = "firebrick",
                                " 3-Point Shot    " = 'midnightblue',
                                " Mid Range Shot    " = "chartreuse4")) +
  bbc_style()  +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) +
  scale_x_continuous(limits = c(1998, 2019),
                     breaks = seq(2000, 2019, by = 5),
                     expand = c(0.025, 0))

