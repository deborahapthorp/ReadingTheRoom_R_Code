library(readr)

prolific_Data <- read_csv('Prolific_demographics_all.csv')
pavlovia_Data <- read_csv('ALL_results_combined.csv')

harmonised_Data <- merge(prolific_Data, pavlovia_Data, by.x = "Participant id", by.y = "participant", all = TRUE)

fileName <- ("harmonised_results_prolific_pavlovia.csv")
write.csv(harmonised_Data, fileName)