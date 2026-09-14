library(readr)
library(dplyr)
library(tidyselect)
library(tidyverse)

analyseWCST_surveys <- function(filepath){

rawData <- read_csv(filepath)

## Consent & exclusion criteria
participant <- unique(na.omit(rawData$participant)) # update this if the new participant info link works & we want to include Qualtrics participants
group <- unique(na.omit(rawData$group))
consent<- unique(na.omit(rawData$`Participant_Info_Consent.block_1/Consent`))
illness <- unique(na.omit(rawData$`Exclusion_Criteria_1.block_1/Mental_health_conditions`))

## Demographics
sex<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Sex`))
education <- unique(na.omit(rawData$`survey_demographics_IU.block_1/Education`))
ethnicity<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Ethnicity`))
politics<- unique(na.omit(rawData$`survey_demographics_IU.block_1/Politics`))
age <- unique(na.omit(rawData$`survey_demographics_IU.block_1/Age`))


## IU scale - still need to work out how to append this, or should we make a separate df for each scale for reliability and just score them in here? 

IU_scale <- na.omit(rawData %>% select(contains("IU_scale.q")))

for ( col in 1:ncol(IU_scale)){
  colnames(IU_scale)[col] <-  sub("survey_demographics_IU.block_1/", "", colnames(IU_scale)[col])
} # rename columns to be more concise 

## WCST
prop_correct_WCST <-  mean(na.omit(rawData$Score))  # The mean will work as it's all 1s and 0s! 
mean_RT_WCST <- mean(na.omit(rawData$RT))
median_RT_WCST <- median(na.omit(rawData$RT))
## Scenarios  - I don't think we really need the response here since it doesn't matter

## ETMS scale - this also produces a df rather than a list

ETMS_scale <- na.omit(rawData %>% select(contains("Epistemic")))

for ( col in 1:ncol(ETMS_scale)){
  colnames(ETMS_scale)[col] <-  sub("ETMS_survey_end.block_1/Epistemic Trust Mistrust.", "", colnames(ETMS_scale)[col])
} # rename columns to be more concise 

# ETMS_scale <- ETMS_scale %>% rename_at('Row 17', ~'ETMS_17')
# ETMS_scale <- ETMS_scale %>% rename_at('Row 18', ~'ETMS_18') # Fix coding error in original survey - only for first 26 participants
=======
# ETMS_scale <- ETMS_scale %>% rename_at('Row 18', ~'ETMS_18') # Fix coding error in original survey
>>>>>>> 251e61f10522cc56d2365d7d0701499d361df88e

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

data_use <- na.omit(rawData$`ETMS_survey_end.block_1/Data use`)


singleVars <- list(participant = participant,  consent = consent, illness = illness,
                   group = group, age = age, sex = sex, education = education, 
                   ethnicity = ethnicity, politics = politics, 
                   prop_correct_WCST = prop_correct_WCST, mean_RT_WCST = mean_RT_WCST, 
                   median_RT_WCST = median_RT_WCST,
                   study_purpose = study_purpose, evaluated = evaluated, 
                   interpretation = interpretation, suspicious = suspicious, 
                   disclosure = disclosure)

singleVars <- data.frame(t(sapply(singleVars,c))) # turn it into a data frame 

surveyVars <- cbind (IU_scale, ETMS_scale, manipulation_check_scale)

allData_participant <- cbind(singleVars, surveyVars)


return (allData_participant)
}


##  Here is the code to do the analysis for all the files 
folder <- "data_new/"

list = list.files(path = folder ,full.names=TRUE,recursive=TRUE) # list all files in the folder
all_names = basename(list) # Get names of all files from their corresponding paths

df <- data.frame(matrix(ncol = 50, nrow = 0)) # Make empty data frame

nSubs <- length(all_names)

for (i in 1:nSubs){
  filepath <- paste0(folder, all_names[i])
  allData_participant <- analyseWCST_surveys(filepath)

  if (length(allData_participant)==50) # Check that all the columns are there
{df <- rbind(df, allData_participant)} # Append data to the existing data frame
  else 
  {indRes <- paste0(folder, 'individual_results/', allData_participant$participant, '_analysed.csv')
    write.csv(allData_participant, indRes)} # Save individual files to be manually added

}


fileName <- ("ALL_results_new.csv")
write.csv(df, fileName)