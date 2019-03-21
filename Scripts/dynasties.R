#Set working directory in the Scripts folder
setwd("Documents/GitHub/houston-rockets-shooting-data/Scripts/")

#LOAD THE PACKAGES REQUIRED FOR THE PARTICULAR SCRIPT
library(reshape2)
library(ggplot2)
library(scales)
library(bbplot)


#Load the dataset that was created in the script "nba_teams_totals"
totals <- read.csv("../Data/yearly_team_totals.csv")

#Add columns for the number of points obtained by different shots
totals$X3P_points <- totals$X3P*3
totals$X2P_points <- totals$X2P*2

#Subset for Bulls/Golden State/Lakers
totals_bulls<-subset(totals,totals$Tm %in% "CHI")
totals_warriors<-subset(totals,totals$Tm %in% "GSW")
totals_lakers<-subset(totals,totals$Tm %in% "LAL")

#######Change the database to build the dynasties stacked graph
#Change for graph Bulls
totals_bulls<-melt(totals_bulls, id.vars = "Year",measure.vars = c("X3P_points","X2P_points", "FT"))
totals_bulls<-subset(totals_bulls,totals_bulls$Year >= 1991 & totals_bulls$Year < 1995)

#Change for graph Warriors
totals_warriors<-melt(totals_warriors, id.vars = "Year",measure.vars = c("X3P_points","X2P_points", "FT"))
totals_warriors<-subset(totals_warriors,totals_warriors$Year >= 2015)

#Change for graph Lakers
totals_lakers<-melt(totals_lakers, id.vars = "Year",measure.vars = c("X3P_points","X2P_points", "FT"))
totals_lakers<-subset(totals_lakers,totals_lakers$Year >= 2000 & totals_lakers$Year < 2005)


#graph relative contribution of 3pts vs 2pts vs FT
plot_stacked <- function(data, years, team_name){
  #Calculates a geom_area plot for the selected team during the selected time period. Data is a dataframe
  #years is a vector of two years (beginning and end years) and team_name is a string
  
  ggplot(data, aes(x=Year,y=value,group=variable,fill=variable)) + geom_area(position="fill", alpha = 0.75)+
    scale_y_continuous(labels = percent_format())+
    bbc_style() +
    labs(title = paste(team_name, ", ", years[1], "-", years[2], sep = "")) +
    geom_hline(yintercept = 0, size = 1, colour="#333333") +
    theme(plot.title = element_text(face = "plain", hjust = 0, size = 16),
          legend.position = "none",
          axis.title = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          plot.margin=unit(c(3,1,1,1.5),"cm")) +
    scale_x_continuous(limits = years,
                       breaks = seq(years[1], years[2], by = 1),
                       expand = c(0,0))
}

#Use the user made function to create a plot for the three dynasties we're interested
g_bulls<- plot_stacked(totals_bulls, c(1991, 1994), "Chicago Bulls")
g_lakers <- plot_stacked(totals_lakers, c(2000, 2004), "L.A. Lakers")
g_warriors <- plot_stacked(totals_warriors, c(2015, 2017), "GS Warriors")

#Take basic multiplot function from R-Cookbook and modify it to add a title and labels to our axis
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL, title=list(), titlepos = c(0.425, 0.94),
                      fontsize = 12, fontfamily = "Helvetica", labs=list(), labpos=list(c(0.5,0.03), c(0.03,0.5))) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if(!length(labs) == 0){
      grid.text(labs[1], x=labpos[[1]][1], y=labpos[[1]][2], gp=gpar(fontsize=16))
      grid.text(labs[2], x=labpos[[2]][1], y=labpos[[2]][2], rot=90, gp=gpar(fontsize=16))
    }
    
    if(!length(title) == 0){
      grid.text(title, x=titlepos[1], y=titlepos[2], gp=gpar(fontsize=40, fontface = "bold"))
    }
  }
}

#Print all the dynasties' plots in the same plot. To use function multiplot, go to scripts and run
#the necesary code to create the user-defined "multiplot()" function
multiplot(g_bulls, 
          g_lakers, 
          g_warriors, 
          cols = 3, 
          labs = list("Year", "% of total points"),
          title = "Dynasty Wars: how do they shoot?")








