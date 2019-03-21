#Create a function get_data to extract information from a URL and create a dataframe of shooting
#data
get_data <- function(URL) {
  library(rjson)

  #Get URL with data from NBA.com
  shotURL <- paste(URL)
  
  #Import data from JSON
  shotJSON <- fromJSON(file = shotURL, method="C")
  
  #Unlist shooting data and save it into a dataframe
  shotDF <- data.frame(matrix(unlist(shotJSON$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
  
  #Give meaningful names to the columns of the dataframe
  colnames(shotDF) <- shotJSON$resultSets[[1]][[2]]
  
  #Convert x and y coordinates into numeric variables
  shotDF$LOC_X <- as.numeric(as.character(shotDF$LOC_X))
  shotDF$LOC_Y <- as.numeric(as.character(shotDF$LOC_Y))
  shotDF$SHOT_DISTANCE <- as.numeric(as.character(shotDF$SHOT_DISTANCE))
  
  return(shotDF)
}


#Repeat the process but modify to extract data for the league average
get_data_league <- function(URL) {
  library(rjson)
  
  #Get URL with data from NBA.com
  shotURL <- paste(URL)
  
  #Import data from JSON
  shotJSON <- fromJSON(file = shotURL, method="C")
  
  #Unlist shooting data and save it into a dataframe
  shotDF <- data.frame(matrix(unlist(shotJSON$resultSets[[2]][[3]]), ncol=7, byrow = TRUE))
  
  #Give meaningful names to the columns of the dataframe
  colnames(shotDF) <- shotJSON$resultSets[[2]][[2]]
  
  return(shotDF)
}




