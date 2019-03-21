#####Champions analysis
setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

#create champions database
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(bbplot)
library(scales)


library(magrittr)
library(gridExtra)
library(readr)



NBA_Champios <- read_excel("~/Documents/GitHub/houston-rockets-shooting-data/Data/NBA Champios.xlsx")
totals <- read.csv("../Data/yearly_team_totals.csv")

#Create champions table
champions<-left_join(NBA_Champios, totals, by = c("Year" = "Year", "Tm" = "Tm"))

#Calculate total points from 2 and 3 pointers
champions$X3P_points <- champions$X3P*3
champions$X2P_points <- champions$X2P*2

points <-melt(champions, id.vars = "Year", measure.vars = c("X3P_points","X2P_points", "FT"))

ggplot(points, aes(x=Year,y=value,group=variable,fill=variable)) + geom_area(position="fill", alpha = 0.75)+
  labs(title="What it takes to be a champion?",
       subtitle = "Contribution of every type of shot to the total points, 1991-2018\n",
       caption = "Source: NBA-Stats",
       x="Year",
       y="% of total points\n") +
  scale_y_continuous(labels = percent_format()) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = 1991, size = 1, colour="#333333") + 
  bbc_style() +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) +
  scale_x_continuous(limits = c(1991, 2017),
                     breaks = seq(1991, 2017, by = 5),
                     expand = c(0,0)) + 
  scale_fill_discrete(labels = c(" 3-Points Shots   ", " 2-Points Shots   ", " Free Throws   ") )
