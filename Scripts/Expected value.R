setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

library(dplyr)
library(reshape2)
library(ggplot2)
library(bbplot)


league_averages <- read.csv("../Data/league_shooting_yearly_averages.csv")

#calculate expected utility for 2pts mid range and 3pts
league_averages<-league_averages %>% mutate( utility_restricted_area = (2 * (FGM_restricted_area/FGA_restricted_area)))
league_averages<-league_averages %>% mutate( utility_paint_nonra = (2 * (FGM_paint_nonra/FGA_paint_nonra)))
league_averages<-league_averages %>% mutate( utility_mid_range = (2 * (FGM_mid_range/FGA_mid_range)))
league_averages<-league_averages %>% mutate( utility_corner_3pt = (3 * (FGM_corner_3pt/FGA_corner_3pt)))
league_averages<-league_averages %>% mutate( utility_above_the_break_3pt = (3 * (FGM_above_the_break_3pt/FGA_above_the_break_3pt)))


#create boxplot for type EV by shot

#To create such boxplot, a new data frame should be created first
EV <- as.data.frame(melt(league_averages,id.vars='Year', measure.vars=c('utility_restricted_area','utility_paint_nonra','utility_mid_range','utility_corner_3pt','utility_above_the_break_3pt')))




ggplot(EV) +
  geom_boxplot(aes(x=Year, y=value, fill=variable))+
  bbc_style() +
  labs(title="Pick your shots wisely",
       subtitle = "Expected value of a shot by area, 1996-2019",
       caption = "Source: NBA Stats",
       x = "Area",
       y = "Expected Value\n")+
  theme(axis.title = element_text(size = 18),
        axis.text.x=element_text(size = 14),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "top",
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic")) + 
  geom_hline(yintercept = 0.7, size = 1, colour="#333333") +
  scale_fill_manual(labels = c(" Restricted Area    ", 
                               " Paint (Non RA)    ", 
                               " Mid Range    ", 
                               " Corner 3-Point    ", 
                               " Above the break 3-Point    "), 
                    values = c("utility_corner_3pt" = "aquamarine2",
                               "utility_above_the_break_3pt" = "magenta",
                               "utility_paint_nonra" = 'firebrick4',
                               "utility_restricted_area" = 'midnightblue',
                               "utility_mid_range" = "chartreuse4"))


#######Bar chart for mid range vs 3-Points
#Calculate expected value for all 3-Point shots
league_averages<-league_averages %>% mutate( utility_3pt = (3 * (FGM_3pt/FGA_3pt)))

#Create plotting database
EV1 <- as.data.frame(melt(league_averages,id.vars='Year', measure.vars=c('utility_mid_range','utility_3pt')))

#Calculate mean utility for every type of shot during the period 1996-2019
EV1 <- EV1 %>% 
  group_by(variable) %>% 
  summarise(value = mean(value))   

#Plot
ggplot(data=EV1, aes(x=variable, y=value)) +
  geom_bar(stat="identity", aes(fill = variable))+ 
  bbc_style() +
  labs(title="Why you shouldn't throw from mid-range?",
       subtitle = "Expected value from a mid-range shot vs 3-Point shot, 1996-2019",
       x = "",
       y = "Expected value",
       caption = "Source: NBA Stats") +
  scale_x_discrete(labels = c("Mid-Range", "3-Point")) + 
  scale_fill_manual(labels = c("Mid-Range", 
                               "3-Point"),
                    values = c("utility_mid_range" = "chartreuse4",
                               "utility_3pt" = "midnightblue"
                               )) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_label(aes(x = variable, y = value, label = round(value, 2)),
             hjust = 0.5, 
             vjust = 1, 
             colour = "white", 
             fill = NA, 
             label.size = NA, 
             family="Helvetica", 
             size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 40),
        plot.subtitle = element_text(size = 30),
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        plot.caption = element_text(size = 18, hjust = 0.95, vjust = 0, colour = "#333333", face = "italic"))

