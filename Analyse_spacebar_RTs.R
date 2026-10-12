library(readr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(stringr)


analyseWCST_times <- function(filepath){
  
  rawData <- read_csv(filepath)
  
  ## Consent & exclusion criteria
  participant <- unique(na.omit(rawData$participant)) # participant code (uncomment below for the first 26 participants )
  #participant <- unique(na.omit(rawData$PROLIFIC_PID)) # For the first 26 participants 
  group <- unique(na.omit(rawData$group))
 
  if (group == 1)
  {feedback_rt <- unique(na.omit(rawData$press_spacebar.rt))}
  else
  {feedback_rt <- unique(na.omit(rawData$press_spacebar_2.rt))}
  
  allData_participant_rt <- cbind(participant, group, feedback_rt) # combine everything 
  
  return (allData_participant_rt) # return a data frame
}

##  Analyse all files in a folder using the function above 

folder <- "data_new/" # 1st 26 participants are in "data/"

list = list.files(path = folder ,full.names=TRUE,recursive=TRUE) # list all files in the folder
all_names = basename(list) # Get names of all files from their corresponding paths

df <- data.frame(matrix(ncol = 4, nrow = 0)) # Make empty data frame

nSubs <- length(all_names)

for (i in 1:nSubs){
  filepath <- paste0(folder, all_names[i])
  allData_participant <- analyseWCST_times(filepath)
  df <- rbind(df, allData_participant) # Append data to the existing data frame
  
}

# Write everything to file 
fileName <- ("ALL_results_feedback_2_rts.csv")
write.csv(df, fileName)