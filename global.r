library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(readxl)
library(shinyBS)
library(DT)
library(shinymanager)
library(openxlsx)
library(zoo)
library(shinycssloaders)

# Create a list of local authority names for use in the UI
LA_names <- c("Aberdeen City", 
              "Aberdeenshire",
              "Angus", 
              "Argyll and Bute" ,     
              "Clackmannanshire",
              "Dumfries and Galloway", 
              "Dundee City",  
              "East Ayrshire",      
              "East Dunbartonshire", 
              "East Lothian", 
              "East Renfrewshire",
              "City of Edinburgh",     
              "Eilean Siar", 
              "Falkirk", 
              "Fife", 
              "Glasgow",       
              "Highland", 
              "Inverclyde",
              "Midlothian",
              "Moray",             
              "North Ayrshire",
              "North Lanarkshire",
              "Orkney Islands",
              "Perth and Kinross",   
              "Renfrewshire", 
              "Scottish Borders",
              "Shetland Islands",
              "South Ayrshire",    
              "South Lanarkshire",
              "Stirling",
              "West Dunbartonshire", 
              "West Lothian")

LA <- c(1:32)
LA_names_dta <- data.frame(LA_names, LA)

# Create a variable for storing the current financial year and quarter
crnt_date <- as.yearqtr(Sys.Date(), format = "%Y-%m-%d") -1/4
crnt_fin_yr <- gsub("\\ ", "-", crnt_date, perl = TRUE)
crnt_fin_yr <- gsub("-Q[0-9]","", crnt_fin_yr) %>% as.numeric(.) 
crnt_fin_yr2 <-  crnt_fin_yr + 1
crnt_fin_yr2 <- substr(crnt_fin_yr2, 3, 4) 
fin_yr <- paste(crnt_fin_yr, crnt_fin_yr2, sep = "/")
rm(crnt_fin_yr2)
crnt_qtr <- gsub("[0-9]*\\ Q", "Quarter ", crnt_date, perl = TRUE)

# Create a variable for storing the previous financial year
prev_fin_yr <- crnt_fin_yr - 1
crnt_fin_yr <- substr(crnt_fin_yr,3,4) 
prev_fin_yr <- paste(prev_fin_yr, crnt_fin_yr, sep = "/")

# Unchanged data ---------------------------------------------------------

# This data is used in the data download page as this is to be presented in
# a wide un-pivoted format. In the server it is filtered to the selected council
# and is formatted. It keeps the additional questions for applicable councils.
fresh_dta <- read_csv("survey_data.csv", col_types = "c") %>% 
  select(-"Tracking Link")
fresh_dta[is.na(fresh_dta$`Local Authority Name`), "Local Authority Name"] <- "-"
fresh_dta$`Ended date` <- as.Date(fresh_dta$`Ended date`, format = "%d/%m/%Y")

# Need to code these as characters because when all responses are in the first column, 
# this is numeric and the other column is character, meaning when the code combines them later it won't work
fresh_dta$`Q. Please select a local authority` <- as.character(fresh_dta$`Q. Please select a local authority`)
fresh_dta$`Q. Please select the local authority that your response relates to` <- as.character(fresh_dta$`Q. Please select the local authority that your response relates to`)

# Pivoted Data --------------------------------------------------------------

# This is the data used in most of the computation for the server e.g. graphs
# and calculating KPO4

pivot_dta <- read_csv("survey_data.csv", col_types = "c") %>% 
  select(-"Tracking Link") %>%
  select(!contains(c("Please explain your answer", 
                     "other comments", 
                     "Please use the comments box")))

pivot_dta$`Ended date` <- as.Date(pivot_dta$`Ended date`, format = "%d/%m/%Y")

# Need to code these as characters because when all responses are in the first column, 
# this is numeric and the other column is character, meaning when the code combines them later it won't work
pivot_dta$`Q. Please select a local authority` <- as.character(pivot_dta$`Q. Please select a local authority`)
pivot_dta$`Q. Please select the local authority that your response relates to` <- as.character(pivot_dta$`Q. Please select the local authority that your response relates to`)

# Where the question has not been answered this needs to be changed to NA 
# These are currently "-" but need to be recoded to distinguish where the 
# question is skipped vs when it is not applicable to the council

# For West Lothian questions replace blank values with NA
pivot_dta[c(29:38)] <- replace(
  pivot_dta[c(29:38)], 
  (pivot_dta$`Q. Please select a local authority` == 32 | pivot_dta$`Q. Please select the local authority that your response relates to` == 32) & 
    pivot_dta[c(29:38)] == "-",
  NA)

# For North Lanarkshire questions replace blank values with NA
pivot_dta[c(39:41)] <- replace(
  pivot_dta[c(39:41)], 
  (pivot_dta$`Q. Please select a local authority` == 22 | pivot_dta$`Q. Please select the local authority that your response relates to` == 22) &
    pivot_dta[c(39:41)] == "-",
  NA)

# For Orkney Islands questions replace blank values with NA
pivot_dta[c(42:56)] <- replace(
  pivot_dta[c(42:56)], 
  (pivot_dta$`Q. Please select a local authority` == 23 | pivot_dta$`Q. Please select the local authority that your response relates to` == 23) &
    pivot_dta[c(42:56)] == "-",
  NA)

# For City of Edinburgh questions replace blank values with NA
pivot_dta[c(57:63)] <- replace(
  pivot_dta[c(57:63)], 
  (pivot_dta$`Q. Please select a local authority` == 12 | pivot_dta$`Q. Please select the local authority that your response relates to` == 12) &
    pivot_dta[c(57:63)] == "-",
  NA)

# For Angus questions replace blank values with NA
pivot_dta[64] <- replace(
  pivot_dta[64], 
  (pivot_dta$`Q. Please select a local authority` == 3 | pivot_dta$`Q. Please select the local authority that your response relates to` == 3) &
    pivot_dta[64] == "-",
  NA)

# For standard council questions replace blank values with NA
pivot_dta[c(22:28)] <- replace(
  pivot_dta[c(22:28)], 
  (pivot_dta$`Q. Please select a local authority` %in% c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ,11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31) | 
     pivot_dta$`Q. Please select the local authority that your response relates to` %in% c(
       1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31)) &
    pivot_dta[c(22:28)] == "-",
  NA)


# The question set is duplicated across columns to account for skip logic
# Rename the columns containing the same questions so they match
colnames(pivot_dta)[c(29, 42, 57)] <- colnames(pivot_dta)[22]
colnames(pivot_dta)[c(30, 43, 58)] <- colnames(pivot_dta)[23]
# The column name of the question ("Quality of the information provided") is the same for the additional question council
# Therefore the column name is repeated and when it is read in it adds a number to the end
# Need to remove the number of change the names of each of these columns
colnames(pivot_dta)[24] <- gsub("\\...[1-9]*$", 
                                "", 
                                colnames(pivot_dta)[24],
                                perl = TRUE)
colnames(pivot_dta)[25] <- gsub("\\...[1-9]*$", 
                                "", 
                                colnames(pivot_dta)[25],
                                perl = TRUE)

colnames(pivot_dta)[c(31,48,59)] <- colnames(pivot_dta)[24]
colnames(pivot_dta)[c(35,54,60)] <- colnames(pivot_dta)[25]
colnames(pivot_dta)[c(36,49,61)] <- colnames(pivot_dta)[26]
colnames(pivot_dta)[c(37,55,62)] <- colnames(pivot_dta)[27]
colnames(pivot_dta)[c(38,56,63)] <- colnames(pivot_dta)[28]

# Remove columns containing additional questions
pivot_dta <- pivot_dta[-c(32, 33, 34, 39, 40, 41, 44, 45, 46, 47, 50, 51, 52, 53, 64)]

# Make sure all data columns are the same data type
pivot_dta[, 22:28] <- sapply(pivot_dta[, 22:28], as.character)

# Pivot indicator data 
pivot_dta <- pivot_dta %>% 
  pivot_longer(cols = 22:28, 
               names_to = "Indicator", 
               values_to = "value")

# As the columns were duplicated there are "-" blank values where the question would be skipped
# Need to remove these to get the complete data set
pivot_dta <- pivot_dta %>% 
  filter(is.na(pivot_dta$value) | value %in% c("1","2","3","4","5"))

# Add in columns with Quarter Info and Financial Year info
pivot_dta$`Tracking Link` <- as.yearqtr(pivot_dta$`Ended date`, 
                                        format = "%Y-%m-%d") 
pivot_dta$`Financial Year` <- pivot_dta$`Tracking Link` -1/4
pivot_dta$`Financial Year` <- gsub("\\ ", 
                                   "-", 
                                   pivot_dta$`Financial Year`, 
                                   perl = TRUE)
pivot_dta$`Financial Year` <- pivot_dta %>% 
  select(contains("Financial Year")) %>% 
  apply(2, function(x) gsub("-Q[0-9]", "", x)) %>% 
  as.numeric(.) %>%
  data.frame() %>% 
  mutate(nxt = .+ 1) %>% 
  mutate(nxt = substr(nxt, 3, 4)) %>% 
  mutate(fy = paste(., nxt, sep = "/")) %>%
  pull(fy)

pivot_dta$`Tracking Link` <- pivot_dta$`Tracking Link`- 1/4
pivot_dta$`Tracking Link` <- gsub("[0-9]*\\ Q", 
                                  "Quarter ", 
                                  pivot_dta$`Tracking Link`, 
                                  perl = TRUE)

# Remove redundant columns and reorder
pivot_dta <- pivot_dta[-c(1:10)]
pivot_dta <- pivot_dta[, c(14, 15, 1, 10, 11, 2:9, 12:13)]

# pivot to combine both LA columns, rename, then remove duplicates
pivot_dta <- pivot_dta %>% 
  pivot_longer(cols = 4:5, names_to = "extra", values_to ="LA") %>%
  filter(LA != "-") %>% 
  select(-extra)
pivot_dta <- pivot_dta[, c(1:3, 14, 4:13)]

# Code local authority name for councils completing survey without login
pivot_dta <- merge(pivot_dta, LA_names_dta)
pivot_dta$`Local Authority Name` <- pivot_dta$LA_names
pivot_dta <- pivot_dta %>% select(-LA_names)

# Remove question numbers from Indicator column and tidy up questions
pivot_dta$Indicator <- gsub("Q[\\.1-9]+\\s", 
                            "", 
                            pivot_dta$Indicator,
                            perl = TRUE)
pivot_dta$Indicator <- gsub(" by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards", 
                            "", 
                            pivot_dta$Indicator)
pivot_dta$Indicator <- gsub(" with \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards from beginning to end", 
                            "", 
                            pivot_dta$Indicator)
pivot_dta[pivot_dta$Indicator == "How would you rate the standard of communication provided service following your initial contact or once your application had been submitted?", "Indicator"] <- "How would you rate the standard of communication provided?"
pivot_dta[pivot_dta$Indicator == "Thinking of your engagement, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?", "Indicator"] <- "Thinking of your engagement, how satisfied were you with the time taken to complete the process?"
pivot_dta[pivot_dta$Indicator == "Time taken to respond to any queries or issues raised", "Indicator"] <- "Responsiveness to any queries or issues raised"

# NA values are coded as 5 by SmartSurvey - replace with NA
pivot_dta$value <- replace(pivot_dta$value, pivot_dta$value == "5", NA)

# Tidy up other column names for selecting data later
pivot_dta <- pivot_dta %>% 
  rename("Q1.4. Other respondent" = "Q1.4. Other (please specify):", 
         "Q2.4. Other reason" = "Q2.4. Other (please specify):") 

# Recode "other" respondents and reasons so it doesn't show text value
pivot_dta$`Q1.4. Other respondent`[pivot_dta$`Q1.4. Other respondent` != "0"] <- "1"
pivot_dta$`Q2.4. Other reason`[pivot_dta$`Q2.4. Other reason` != "0"] <- "1"

# Download Table Data ----------------------------------------------------

# This data set needs to be unpivoted but without the additional questions 
# and the questions named the same

unpivot_data_global <- read_csv("survey_data.csv", col_types = "c") %>% 
  select(-"Tracking Link")
unpivot_data_global[is.na(unpivot_data_global$`Local Authority Name`), "Local Authority Name"] <- "-"
unpivot_data_global$`Ended date` <- as.Date(unpivot_data_global$`Ended date`, 
                                            format = "%d/%m/%Y")

# Need to code these as characters because when all responses are in the first column, 
# this is numeric and the other column is character, meaning when the code combines them later it won't work
unpivot_data_global$`Q. Please select a local authority` <- as.character(unpivot_data_global$`Q. Please select a local authority`)
unpivot_data_global$`Q. Please select the local authority that your response relates to` <- as.character(unpivot_data_global$`Q. Please select the local authority that your response relates to`)

# Add in columns with Quarter Info and Financial Year info
unpivot_data_global$`Tracking Link` <- as.yearqtr(unpivot_data_global$`Ended date`,
                                                  format = "%Y-%m-%d") 
unpivot_data_global$`Financial Year` <- unpivot_data_global$`Tracking Link` - 1/4
unpivot_data_global$`Financial Year` <- gsub("\\ ",
                                             "-", 
                                             unpivot_data_global$`Financial Year`, 
                                             perl = TRUE)

unpivot_data_global$`Financial Year` <- unpivot_data_global %>% 
  select(contains("Financial Year")) %>% 
  apply(2, function(x) gsub("-Q[0-9]", "", x)) %>% 
  as.numeric(.) %>%
  data.frame() %>% 
  mutate(nxt = .+ 1) %>% 
  mutate(nxt = substr(nxt, 3, 4)) %>% 
  mutate(fy = paste(., nxt, sep = "/")) %>%
  pull(fy)

unpivot_data_global$`Tracking Link` <- unpivot_data_global$`Tracking Link`- 1/4
unpivot_data_global$`Tracking Link` <- gsub("[0-9]*\\ Q", 
                                            "Quarter ", 
                                            unpivot_data_global$`Tracking Link`, 
                                            perl = TRUE)

# Remove redundant columns and reorder
unpivot_data_global <- unpivot_data_global[-c(1:8, 10)]
unpivot_data_global <- unpivot_data_global[, c((ncol(unpivot_data_global) - 1),
                                               ncol(unpivot_data_global),
                                               2, 
                                               11, 
                                               12, 
                                               3:10, 
                                               13:(ncol(unpivot_data_global) - 2), 
                                               1)]

# Pivot to combine both LA columns, rename, then remove duplicates
unpivot_data_global <- unpivot_data_global %>% 
  pivot_longer(cols = 4:5, names_to = "extra", values_to = "LA") %>%
  filter(LA != "-") %>% 
  select(-extra)
unpivot_data_global <- unpivot_data_global[, c(1:3, 
                                               ncol(unpivot_data_global),
                                               4:(ncol(unpivot_data_global) - 1))]

# Code local authority name for councils completing survey without login
unpivot_data_global <- merge(unpivot_data_global, LA_names_dta)
unpivot_data_global$`Local Authority Name` <- unpivot_data_global$LA_names
unpivot_data_global <- unpivot_data_global %>% select(-LA_names)
unpivot_data_global <- unpivot_data_global[, c(2:4, 1, 5:ncol(unpivot_data_global))]

# Rename the columns containing the same questions so they match
# Rename comments columns

# Time Taken
colnames(unpivot_data_global)[c(26, 47, 70)] <- colnames(unpivot_data_global)[13]
colnames(unpivot_data_global)[14] <- "Time taken comments"
colnames(unpivot_data_global)[c(27, 48, 71)] <- colnames(unpivot_data_global)[14]

# Communication
colnames(unpivot_data_global)[c(28, 49, 72)] <- colnames(unpivot_data_global)[15]
colnames(unpivot_data_global)[16] <- "Communication comments"
colnames(unpivot_data_global)[c(29, 50, 73)] <- colnames(unpivot_data_global)[16]

# The column name of the question ("Quality of the information provided") is the same for the additional question council
# Therefore the column name is repeated and when it is read in it adds a number to the end
# Need to remove the number of change the names of each of these columns
colnames(unpivot_data_global)[17] <- gsub("\\...[1-9]*$", 
                                          "", 
                                          colnames(unpivot_data_global)[17],
                                          perl = TRUE)

colnames(unpivot_data_global)[c(30, 56, 74)] <- colnames(unpivot_data_global)[17]

colnames(unpivot_data_global)[18] <- gsub("\\...[1-9]*$", 
                                          "", 
                                          colnames(unpivot_data_global)[18],
                                          perl = TRUE)

colnames(unpivot_data_global)[c(34, 63, 75)] <- colnames(unpivot_data_global)[18]

colnames(unpivot_data_global)[c(35, 57, 76)] <- colnames(unpivot_data_global)[19]

colnames(unpivot_data_global)[20] <- "Information, staff, responsiveness comments"
colnames(unpivot_data_global)[c(36, 58, 77)] <- colnames(unpivot_data_global)[20]

# Treated fairly
colnames(unpivot_data_global)[c(37, 65, 78)] <- colnames(unpivot_data_global)[21]
colnames(unpivot_data_global)[22] <- "Treated fairly comments"
colnames(unpivot_data_global)[c(38, 66, 79)] <- colnames(unpivot_data_global)[22]

# Satisfaction
colnames(unpivot_data_global)[c(39, 67, 80)] <- colnames(unpivot_data_global)[23]
colnames(unpivot_data_global)[24] <- "Overall satisfaction comments"
colnames(unpivot_data_global)[c(40, 68, 81)] <- colnames(unpivot_data_global)[24]
colnames(unpivot_data_global)[c(41, 69,82)] <- colnames(unpivot_data_global)[25]

# Additional info ------------------------------------------------------------

# custom function for checking if vector is empty
isEmpty <- function(x) {
  return(length(x) == 0)
}

# Create popover text
KPO_popover_text<- paste("KPO4 is calculated by applying the following weightings:",
                         "Overall satisfaction - 50%",
                         "Communications and time taken - 12.5% each",
                         "Staff, information, responsiveness, fairness - 6.25% each.",
                         sep = "<br>")
report_popover_text <- paste("This page shows a summary of all", 
                             "results from the survey. Click the",
                             "Generate report button to download",
                             "a pdf report of these results.",
                             sep = "<br>")