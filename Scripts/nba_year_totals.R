setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(bbplot)

#Load player data
Seasons_Stats <- read.csv("../Data/Seasons_Stats.csv")


#CREATION OF NEW DATA FRAME season_totals
#Use dplyr to group by team and year, and select only the sum of the columns we MIGHT be interested in:
#Field Goals (made and attempted), 3-point shots (made and attempted), 2-point shots (made and attempted),
#Free Throws (made and attempted), Offensive Rebounds, Defensive rebounds, Assists, Steals, Blocks, 
#Turnovers and Total Points

season_totals <- Seasons_Stats %>%
  group_by(Year) %>%
  summarise(FG = sum(FG), FGA = sum(FGA), X3P = sum(X3P), X3PA = sum(X3PA), X2P = sum(X2P), 
            X2PA = sum(X2PA), FT = sum(FT), FTA = sum(FTA), ORB = sum(ORB), DRB = sum(DRB), 
            AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TOV),PTS = sum(PTS))




#PLOT THE EVOLUTION OF 3PTA AND POINTS
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X3PA), col = "blue") +
  geom_line(aes( x = Year, y = PTS), col = "red") + 
  geom_smooth(method = "lm", aes( x = Year, y = X3PA))  + 
  geom_smooth(method = "lm", aes( x = Year, y = PTS))

#PLOT THE EVOLUTION OF POINTS COMING FROM 3PT-SHOTS AND 2PT-SHOTS
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X3P*3, col = "3PT-Shots")) +
  geom_line(aes( x = Year, y = X2P*2, col = "2PT-Shots")) + 
  geom_smooth(method = "lm", aes( x = Year, y = X3P*3), col = "blue")  + 
  geom_smooth(method = "lm", aes( x = Year, y = X2P*2), col = "red") +
  labs( title= "Total points coming from 3pt-shots vs 2pt-shots",
        subtitle = "Since their introduction, 3-pointers represent a bigger and bigger share of the total points achieved in the NBA",
        y="Percentage of total points", 
        x = "Year",
        color = "Type of shot")+
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16)) +
  scale_color_manual(values = c("3PT-Shots" = 'midnightblue', "2PT-Shots" = 'firebrick4'))
            

#PERCENTAGE OF TOTAL POINTS COMING FROM 3-POINTERS VS 2-POINTERS VS FREE THROWS
season_totals$X3P_points <- season_totals$X3P*3
season_totals$X2P_points <- season_totals$X2P*2

points <-melt(season_totals[season_totals$Year >= 1980, ], id.vars = "Year", measure.vars = c("X3P_points","X2P_points", "FT"))

ggplot(points, aes(x=Year,y=value,group=variable,fill=variable)) + geom_area(position="fill", alpha = 0.75)+
  labs(title="The increasing importance of the 3-point shot",
       subtitle = "Contribution of every type of shot to the total points, 1980-2017\n",
       caption = "Source: Basketball-Reference",
       x="Year",
       y="% of total points\n") +
  scale_y_continuous(labels = percent_format()) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = 1980, size = 1, colour="#333333") + 
  bbc_style() +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) +
  scale_x_continuous(limits = c(1980, 2017),
                     breaks = seq(1985, 2017, by = 10),
                     expand = c(0,0)) + 
  scale_fill_discrete(labels = c(" 3-Points Shots   ", " 2-Points Shots   ", " Free Throws   ") )

#FIELD GOALS MADE VS FIELD GOALS ATTEMPTED
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = FGA, col = "FG Attempts")) + 
  geom_line(aes( x = Year, y = FG, col = "FG")) +
  geom_smooth(method = "lm", aes( x = Year, y = FGA), col = "red")  + 
  geom_smooth(method = "lm", aes( x = Year, y = FG), col = "blue")  + 
  labs( title= "Evolution of field goal attempts", 
        subtitle = "Not as clear as when looking at 3-points attempted vs made, but it also appears that total attempts are increasing more than succesful shots. \nThis might hint that, in general, teams are shooting earlier and having more possesions per game.",
        y="FG", 
        x = "Year")+
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16)) +
  scale_color_manual(values = c("FG" = 'midnightblue', "FG Attempts" = 'firebrick4'))

#3PTS MADE VS 3PTS ATTEMPTED
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = X3PA, col = "3-Point Attempts"), size = 2) + 
  geom_line(aes( x = Year, y = X3P, col = "3-Point Made"), size = 2) +
  geom_smooth(method = "lm", aes( x = Year, y = X3PA), col = "red")  + 
  geom_smooth(method = "lm", aes( x = Year, y = X3P), col = "blue")  + 
  labs( title= "Evolution of 3-Point Shots",
        subtitle = "The number of 3-Pointers made increases, albeit a much slower pace than the 3-point attempts",
        y="3-Point Shots", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16))   +
  scale_color_manual(values = c("3-Point Made" = 'midnightblue', "3-Point Attempts" = 'firebrick4')) + 
  bbc_style() +
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

#EVOLUTION OF POINTS SINCE THE 80S
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = PTS), col = "midnightblue", size = 2) + 
  geom_smooth(method = "lm", aes( x = Year, y = PTS), col = "blue", size = 0.5)  + 
  labs( title= "Steady increase in total points scored\nby season",
        subtitle = "Evolution of total points, 1980-2017\n", 
        y="Points\n", 
        x = "Year",
        caption = "\nSource: Basketball-Reference") +
  bbc_style() +
  theme(axis.title = element_text(size = 18),
        axis.text.x=element_text(size = 14),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) + 
  geom_hline(yintercept = 0.7, size = 1, colour="#333333") +
  scale_x_continuous(limits = c(1980, 2017),
                     breaks = seq(1981, 2017, by = 10),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = comma) +
  geom_label(aes(x = 2006, y = 100000, label = "NBA\nLockout"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 12) +
  geom_curve(aes(x = 2006, y = 100000, xend = 1999.5, yend = 132000), 
             colour = "#555555", 
             size=0.5, 
             curvature = -0.2,
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 2010, y = 110000, xend = 2012, yend = 190000), 
             colour = "#555555", 
             size=0.5, 
             curvature = 0.2,
             arrow = arrow(length = unit(0.03, "npc")))

#EVOLUTION OF FGS PERCENTAGE SINCE THE 80S
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = FG/FGA), col = "midnightblue", size = 2) + 
  geom_smooth(method = "lm", aes( x = Year, y = FG/FGA), col = "blue", size = 0.5)  + 
  labs( title= "Steady increase in total points scored\nby season",
        subtitle = "Evolution of total points, 1980-2017\n", 
        y="Points\n", 
        x = "Year",
        caption = "\nSource: Basketball-Reference") +
  bbc_style() +
  theme(axis.title = element_text(size = 18),
        axis.text.x=element_text(size = 14),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) + 
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(limits = c(1980, 2017),
                     breaks = seq(1981, 2017, by = 10),
                     expand = c(0, 0))
  

#EVOLUTION OF FREE THROWS SINCE THE 80S
ggplot(data = season_totals[season_totals$Year >= 1980,]) +
  geom_line(aes( x = Year, y = FTA, col = "FT Attempts")) + 
  geom_line(aes( x = Year, y = FT, col = "FT Made")) +
  geom_smooth(method = "lm", aes( x = Year, y = FTA), col = "red")  + 
  geom_smooth(method = "lm", aes( x = Year, y = FT), col = "blue")  + 
  labs( title= "Evolution of total Free Throws",
        subtitle = "Trend increasing steadily since 1980, but there is significant year to year variation", 
        y="FT", 
        x = "Year") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 28),
        plot.subtitle = element_text(size = 16))  +
  scale_color_manual(values = c("FT Made" = 'midnightblue', "FT Attempts" = 'firebrick4'))




