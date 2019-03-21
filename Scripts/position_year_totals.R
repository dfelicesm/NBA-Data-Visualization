setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

library(dplyr)
library(ggplot2)
library(bbplot)

#Load player data
Seasons_Stats <- read.csv("../Data/Seasons_Stats.csv")



#CREATION OF NEW DATA FRAME position_totals
#Use dplyr to group by team and year, and select only the sum of the columns we MIGHT be interested in:
#Field Goals (made and attempted), 3-point shots (made and attempted), 2-point shots (made and attempted),
#Free Throws (made and attempted), Offensive Rebounds, Defensive rebounds, Assists, Steals, Blocks, 
#Turnovers and Total Points

yearly_position_totals <- Seasons_Stats %>%
  group_by(Year, Pos ) %>%
  summarise(FG = sum(FG), FGA = sum(FGA), X3P = sum(X3P), X3PA = sum(X3PA), X2P = sum(X2P), 
            X2PA = sum(X2PA), FT = sum(FT), FTA = sum(FTA), ORB = sum(ORB), DRB = sum(DRB), 
            AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV),PTS = sum(PTS))

centers <- yearly_position_totals[yearly_position_totals$Pos == "C",]

#3PTS ATTEMPTED BY CENTERS
ggplot(data = centers[centers$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X3PA),  col = "midnightblue", size = 2) + 
  labs( title= "How are NBA centers adapting to the new\nsituation?",
        subtitle = "Evolution of 3-point attempts made by centers",
        y="Attempted 3-Point Shots\n", 
        x = "Year") + 
  bbc_style() +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) +
  scale_x_continuous(limits = c(1980, 2017),
                     breaks = seq(1985, 2017, by = 10),
                     expand=c(0,0))

#2PTS ATTEMPTED BY CENTERS
ggplot(data = centers[centers$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X2PA),  col = "midnightblue") + 
  geom_smooth(method = "lm", aes( x = Year, y = X2PA), col = "blue")  + 
  labs( title= "How are NBA centers adapting to the new situation?",
        subtitle = "Evolution of 2-point attempts made by centers",
        y="Attempted 2-Point Shots", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16))

#OFFENSIVE REBOUNDS CAPTURED BY CENTERS
ggplot(data = centers[centers$Year >= 1980,]) +
  geom_line(aes( x = Year, y = ORB), col = "midnightblue") + 
  geom_smooth(method = "lm", aes( x = Year, y = ORB), col = "blue")  + 
  labs( title= "How are NBA centers adapting to the new situation?",
        subtitle = "Evolution of offensive rebounds captured by centers",
        y="Offensive Rebounds", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16)
        )

#2PTS ATTEMPTED VS MADE BY CENTERS
ggplot(data = centers[centers$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X2PA, col = "2-Point Attempts")) + 
  geom_line(aes( x = Year, y = X2P, col = "2-Point Made")) +
  geom_smooth(method = "lm", aes( x = Year, y = X2PA), col = "red")  + 
  geom_smooth(method = "lm", aes( x = Year, y = X2P), col = "blue") + 
  labs( title= "How are NBA centers adapting to the new situation?",
        subtitle = "2-point attempts vs 2-point shot made by the centers in the league",
        y="2-Point Shots", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16))   +
  scale_color_manual(values = c("2-Point Made" = 'midnightblue', "2-Point Attempts" = 'firebrick4'))

#FT ATTEMPTED VS MADE BY CENTERS
ggplot(data = centers[centers$Year >= 1980,]) +
  geom_line(aes( x = Year, y = FTA, col = "2-Point Attempts")) + 
  geom_line(aes( x = Year, y = FT, col = "2-Point Made")) +
  geom_smooth(method = "lm", aes( x = Year, y = FTA), col = "red")  + 
  geom_smooth(method = "lm", aes( x = Year, y = FT), col = "blue") + 
  labs( title= "How are NBA centers adapting to the new situation?",
        subtitle = "2-point attempts vs 2-point shot made by the centers in the league",
        y="2-Point Shots", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16))   +
  scale_color_manual(values = c("2-Point Made" = 'midnightblue', "2-Point Attempts" = 'firebrick4'))

  
