library(readr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(stringr)


# Function to run the data wrangling on individual files 

analyseWCST_surveys <- function(filepath){

rawData <- read_csv(filepath)

## Consent & exclusion criteria
participant <- unique(na.omit(rawData$participant)) # participant code (uncomment below for the first 26 participants )
#participant <- unique(na.omit(rawData$PROLIFIC_PID)) # For the first 26 participants 
group <- unique(na.omit(rawData$group))
consent<- unique(na.omit(rawData$`Participant_Info_Consent.block_1/Consent`))
illness <- unique(na.omit(rawData$`Exclusion_Criteria_1.block_1/Mental_health_conditions`))

## Demographics
sex<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Sex`))
education <- unique(na.omit(rawData$`survey_demographics_IU.block_1/Education`))
ethnicity<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Ethnicity`))
politics<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Politics`))
age <- unique(na.omit(rawData$`survey_demographics_IU.block_1/Age`))

## IU scale

IU_scale <- na.omit(rawData %>% select(contains("IU_scale.q")))

for ( col in 1:ncol(IU_scale)){
  colnames(IU_scale)[col] <-  sub("survey_demographics_IU.block_1/", "", colnames(IU_scale)[col])
} # rename columns to be more concise 

## WCST
prop_correct_WCST <-  mean(na.omit(rawData$Score))  # The mean will work as it's all 1s and 0s! 
mean_RT_WCST <- mean(na.omit(rawData$RT))
median_RT_WCST <- median(na.omit(rawData$RT))

## ETMS scale

ETMS_scale <- na.omit(rawData %>% select(contains("Epistemic")))

for ( col in 1:ncol(ETMS_scale)){
  colnames(ETMS_scale)[col] <-  sub("ETMS_survey_end.block_1/Epistemic Trust Mistrust.", "", colnames(ETMS_scale)[col])
} # rename columns to be more concise 


# Fix coding error in original survey - only for first 26 participants
#ETMS_scale <- ETMS_scale %>% rename_at('Row 17', ~'ETMS_17')
#ETMS_scale <- ETMS_scale %>% rename_at('Row 18', ~'ETMS_18')

## Manipulation check 

manipulation_check_scale <- na.omit(rawData %>% select(contains("Feedback check")))

for ( col in 1:ncol(manipulation_check_scale)){
  colnames(manipulation_check_scale)[col] <-  sub("ETMS_survey_end.block_1/Feedback check.", "", colnames(manipulation_check_scale)[col])
} 

# rename columns to be more concise 

## Qualitative responses 

study_purpose <- na.omit(rawData$`ETMS_survey_end.block_1/StudyPurpose`)
evaluated <- na.omit(rawData$`ETMS_survey_end.block_1/WereYouEvaluated`)
interpretation <- na.omit(rawData$`ETMS_survey_end.block_1/Feedback_interpretation`)
suspicious <- na.omit(rawData$`ETMS_survey_end.block_1/Suspicious`)
disclosure <- na.omit(rawData$`ETMS_survey_end.block_1/Disclosure`)

disclosure <- str_remove_all(disclosure, "Item ") # Fix slight survey coding error that did not assign a value to this variable

data_use <- na.omit(rawData$`ETMS_survey_end.block_1/Data use`)

singleVars <- list(participant = participant,  consent = consent, illness = illness,
                   group = group, age = age, sex = sex, education = education, 
                   ethnicity = ethnicity, politics = politics, 
                   prop_correct_WCST = prop_correct_WCST, mean_RT_WCST = mean_RT_WCST, 
                   median_RT_WCST = median_RT_WCST,
                   study_purpose = study_purpose, evaluated = evaluated, 
                   interpretation = interpretation, suspicious = suspicious, 
                   disclosure = disclosure, data_use = data_use)

singleVars <- data.frame(t(sapply(singleVars,c))) # turn it into a data frame 

surveyVars <- cbind (IU_scale, ETMS_scale, manipulation_check_scale) # combine survey variables

allData_participant <- cbind(singleVars, surveyVars) # combine everything 

return (allData_participant) # return a data frame
}


##  Analyse all files in a folder using the function above 

folder <- "data_new/" # 1st 26 participants are in "data/"

list = list.files(path = folder ,full.names=TRUE,recursive=TRUE) # list all files in the folder
all_names = basename(list) # Get names of all files from their corresponding paths

#df <- data.frame(matrix(ncol = 51, nrow = 0)) # Make empty data frame

nSubs <- length(all_names)

for (i in 1:nSubs){
  filepath <- paste0(folder, all_names[i])
  allData_participant <- analyseWCST_surveys(filepath)

  if (length(allData_participant)==51) # Check that all the columns are there
{df <- rbind(df, allData_participant)} # Append data to the existing data frame
  else  # otherwise write results individually (inelegant hack)
  {indRes <- paste0('individual_results/', allData_participant$participant, '_analysed.csv')
    write.csv(allData_participant, indRes)} # Save individual files to be manually added

}

# Write everything to file 
fileName <- ("ALL_results_new.csv")
write.csv(df, fileName)