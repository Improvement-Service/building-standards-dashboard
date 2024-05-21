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
# Formats todays date as a year and quarter value
# This formatting uses calender year values rather than financial so need
# to reduce by a quarter to format as financial years
crnt_date <- as.yearqtr(Sys.Date(), format = "%Y-%m-%d") -1/4
crnt_yr <- gsub("\\ ", "-", crnt_date, perl = TRUE)
# extracts just the year value - 1st year in the financial year
crnt_yr <- gsub("-Q[0-9]","", crnt_yr) %>% as.numeric(.) 
# gets the second year value - 2nd year in the financial year
crnt_yr2 <-  crnt_yr + 1
# extract just the last 2 digits of the 2nd financial year
crnt_yr2 <- substr(crnt_yr2, 3, 4) 
# adds a separator between the 2 years to format as a financial year
crnt_fin_yr <- paste(crnt_yr, crnt_yr2, sep = "/")
rm(crnt_yr2)
crnt_qtr <- gsub("[0-9]*\\ Q", "Quarter ", crnt_date, perl = TRUE)

# Create a variable for storing the previous financial year
prev_fin_yr <- crnt_yr - 1
crnt_yr <- substr(crnt_yr, 3, 4) 
prev_fin_yr <- paste(prev_fin_yr, crnt_yr, sep = "/")

# Unchanged data ---------------------------------------------------------

# This data is used in the data download page as this is to be presented in
# a wide un-pivoted format. In the server it is filtered to the selected council
# and is formatted. It keeps the additional questions for applicable councils.
fresh_dta <- read_csv("survey_data.csv", col_types = "c") %>% 
  select(-"Tracking Link") %>%
  replace_na(list("Local Authority Name" = "-"))
fresh_dta$`Ended date` <- as.Date(fresh_dta$`Ended date`, format = "%d/%m/%Y")

# An additional entry for "West Lothian;" was created causing an issue for their dashboard - fix this
fresh_dta[fresh_dta$`Local Authority Name` == "West Lothian;" &
             fresh_dta$`Q. Please select the local authority that your response relates to` == "10",
          "Q. Please select the local authority that your response relates to" ] <- "32"
fresh_dta[fresh_dta$`Q. Please select a local authority` == "33","Q. Please select a local authority" ] <- "32"

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
#pivot_dta$`Q. Please select a local authority` <- as.character(pivot_dta$`Q. Please select a local authority`)
#pivot_dta$`Q. Please select the local authority that your response relates to` <- as.character(pivot_dta$`Q. Please select the local authority that your response relates to`)

# An additional entry for "West Lothian;" was created causing an issue for their dashboard - fix this
pivot_dta[pivot_dta$`Local Authority Name` == "West Lothian;" &
            pivot_dta$`Q. Please select the local authority that your response relates to` == "10",
          "Q. Please select the local authority that your response relates to" ] <- "32"
pivot_dta[pivot_dta$`Q. Please select a local authority` == "33","Q. Please select a local authority" ] <- "32"

# Where the question has not been answered this needs to be changed to NA 
# These are currently "-" but need to be recoded to distinguish where the 
# question is skipped vs when it is not applicable to the council

pivot_dta <- pivot_dta %>%
  pivot_longer(cols = -(1:21), names_to = "col_names", values_to = "value") %>%
  # pivot to combine both LA columns, rename, then remove duplicates
  pivot_longer(cols = c(`Q. Please select a local authority`, 
                        `Q. Please select the local authority that your response relates to`
                        ), names_to = "extra", values_to = "LA") %>%
  filter(LA != "-") %>% 
  select(-extra)

# Code local authority name for councils completing survey without login
pivot_dta <- merge(pivot_dta, LA_names_dta)
pivot_dta$`Local Authority Name` <- pivot_dta$LA_names
pivot_dta <- pivot_dta %>% select(-LA_names) 

# Create a second data set with new column names
new_col_names_dta <- pivot_dta %>% 
  # These next two steps mean there is one council associated with each column name
  # For councils with additional questions, only that council will have non "-" 
  # responses so the council name will match up
  filter(value != "-") %>%
  distinct(col_names, .keep_all = TRUE) %>%
  mutate("new_col_names" = col_names) %>%
  # Where questions are duplicated numbers are added to column name - remove these
  mutate(new_col_names = str_remove_all(new_col_names, "\\.\\.\\.[:digit:]*$")) %>%
  # Add council name to additional questions so these can be referenced easily
  mutate(new_col_names = if_else(`Local Authority Name` %in% c("Angus",
                                                               "City of Edinburgh",
                                                               "North Lanarkshire",
                                                               "Orkney Islands",
                                                               "West Lothian"),
                                   paste0(new_col_names, "_", `Local Authority Name`),
                                   new_col_names)) %>%
  select(col_names, new_col_names)
 
# Match the new column names to the old ones 
pivot_dta <- merge(pivot_dta, new_col_names_dta) %>%
  select(-col_names) %>%
  # pivot back to wide format to continue tidying indicator columns
  pivot_wider(names_from = new_col_names, values_from = value)

# Replace blanks for respondents who completed Angus specific questions
pivot_dta <- pivot_dta %>% 
  mutate(across(contains("_Angus"),
                ~ ifelse(.=="-" & `Local Authority Name` == "Angus", 
                         NA, 
                         .)))

# Replace blanks for respondents who completed Edinburgh specific questions
pivot_dta <- pivot_dta %>% 
  mutate(across(contains("_City of Edinburgh"),
                ~ ifelse(.=="-" & `Local Authority Name` == "City of Edinburgh", 
                         NA, 
                         .)))

# Replace blanks for respondents who completed North Lanarkshire specific questions
pivot_dta <- pivot_dta %>% 
  mutate(across(contains("_North Lanarkshire"),
                ~ ifelse(.=="-" & `Local Authority Name` == "North Lanarkshire", 
                         NA, 
                         .)))

# Replace blanks for respondents who completed Orkney specific questions
pivot_dta <- pivot_dta %>% 
  mutate(across(contains("_Orkney Islands"),
                ~ ifelse(.=="-" & `Local Authority Name` == "Orkney Islands", 
                         NA, 
                         .)))

# For West Lothian questions replace blank values with NA
# (West Lothian had a few responses where respondents answered the standard
# question set rather than the specific West Lothian set, so need to account for
# both of these scenarios). If the response is "-" in both the West Lothian specific
# questions and the standard questions this is a genuine NA, but if it is only 
# "-" in that's because the question was skipped. Don't want to record as NA in 
# this scenario as respondent will be counted twice

# Replace blanks for respondents who completed West Lothian specific questions
pivot_dta <- pivot_dta %>% 
  mutate(`Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian` == "-" &
                   `Q3. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?` == "-", 
                 NA, 
                 `Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian`)) %>%
  mutate(`Q9. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q9. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?_West Lothian` == "-" &
                   `Q4. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?` == "-", 
                 NA, 
                 `Q9. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?_West Lothian`)) %>%
  mutate(`Q.1. Quality of the information provided_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.1. Quality of the information provided_West Lothian` == "-" &
                   `Q.1. Quality of the information provided` == "-", 
                 NA, 
                 `Q.1. Quality of the information provided_West Lothian`)) %>%
  mutate(`Q.5. Overall service offered by staff_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.5. Overall service offered by staff_West Lothian` == "-" &
                   `Q.2. Service offered by staff` == "-", 
                 NA, 
                 `Q.5. Overall service offered by staff_West Lothian`)) %>%
  mutate(`Q.6. Time taken to respond to any queries or issues raised_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.6. Time taken to respond to any queries or issues raised_West Lothian` == "-" &
                   `Q.3. Time taken to respond to any queries or issues raised` == "-", 
                 NA, 
                 `Q.6. Time taken to respond to any queries or issues raised_West Lothian`)) %>%
  mutate(`Q10. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q10. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?_West Lothian` == "-" &
                   `Q5. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?` == "-", 
                 NA, 
                 `Q10. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?_West Lothian`)) %>%
  # For the additional questions check whether other responses have been coded as NA as this suggests
  # a genuine NA response given the rules above
  mutate(`Q.2. Accuracy of the information provided as relevant to your needs_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.2. Accuracy of the information provided as relevant to your needs_West Lothian` == "-" &
                   is.na(`Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian`), 
                 NA, 
                 `Q.2. Accuracy of the information provided as relevant to your needs_West Lothian`)) %>%
  mutate(`Q.3. Professionalism in terms of the knowledge and skills of our staff_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.3. Professionalism in terms of the knowledge and skills of our staff_West Lothian` == "-" &
                   is.na(`Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian`), 
                 NA, 
                 `Q.3. Professionalism in terms of the knowledge and skills of our staff_West Lothian`)) %>%
  mutate(`Q.4. Attitude in terms of friendliness and helpfulness of our staff_West Lothian` = 
         if_else(`Local Authority Name` == "West Lothian" &
                   `Q.4. Attitude in terms of friendliness and helpfulness of our staff_West Lothian` == "-" &
                   is.na(`Q8. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?_West Lothian`), 
                 NA, 
                 `Q.4. Attitude in terms of friendliness and helpfulness of our staff_West Lothian`)) 

# Replace blanks for respondents who completed standard questions
pivot_dta <- pivot_dta %>% 
  mutate(across(c("Q3. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?",
                  "Q4. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?",
                  "Q.1. Quality of the information provided",
                  "Q.2. Service offered by staff",
                  "Q.3. Time taken to respond to any queries or issues raised",
                  "Q5. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?",
                  "Q6. Overall, how satisfied were you with the service provided by [question(16082428)][variable(la)] Building Standards?"
                  ),
                ~ ifelse(.=="-" & !`Local Authority Name` %in% c("City of Edinburgh", 
                                                                 "Orkney Islands", 
                                                                 "West Lothian"), 
                         NA, 
                         .)))

# Pivot indicator data 
pivot_dta <- pivot_dta %>% 
  pivot_longer(cols = -(1:20), 
               names_to = "Indicator", 
               values_to = "value")

# Remove the questions number prefix and council name suffix so duplicted questions
# can be matched
pivot_dta <- pivot_dta %>%
  mutate(Indicator = str_remove_all(Indicator, "^[^\\s]*\\s")) %>%
  mutate(Indicator = str_remove_all(Indicator, "_.*$"))

# Some of the standard questions are worded differently for the councils with 
# additional questions. Need to rename so these can be matched
pivot_dta$Indicator[pivot_dta$Indicator == "Overall service offered by staff"] <- "Service offered by staff"
pivot_dta$Indicator[pivot_dta$Indicator == "Overall, how would you rate the service offered by [question(16082428)][variable(la)] Council staff?"] <- "Service offered by staff"
pivot_dta$Indicator[pivot_dta$Indicator == "Time taken to respond to any queries or issues raised. Our target response times are 10 days for emails and 2 days for phone calls"] <- "Time taken to respond to any queries or issues raised"

# Remove additional questions
pivot_dta <- pivot_dta %>%
  filter(!Indicator %in% c("Accuracy of the information provided as relevant to your needs",
                           "Professionalism in terms of the knowledge and skills of our staff",
                           "Attitude in terms of friendliness and helpfulness of our staff",
                           "Did you find it easy to contact the officer/inspector/administrator you were looking for?",
                           "Easy to find",
                           "Understandable",
                           "Please provide further information about your answers:",
                           "Finally, if you are responding in relation to a Completion Certificate did your experience include a Remote Verification Inspection (RVI) whereby the inspection is via live or pre-recorded video?",
                           "How satisfied were you with the range of options provided by [question(16082428)][variable(la)] Council relating to inspections?",
                           "How satisfied were you with the building warrant approval process?",
                           "To what extent would you agree [question(16082428)][variable(la)] Council have used digital technology to make building standards processes easier for you (for example, around plan approval and site inspections)?",
                           "Staff were polite and courteous",
                           "Staff were helpful",
                           "Staff were efficient",
                           "Staff were knowledgeable"
                           ))

# Need to remove "-" blank values where questions were skipped
pivot_dta <- pivot_dta %>% 
  filter(value != "-")

# Add in columns with Quarter Info and Financial Year info
# Formats the ended date as a year and quarter value
pivot_dta$Quarter <- as.yearqtr(pivot_dta$`Ended date`, 
                                format = "%Y-%m-%d") 
# This formatting uses calender year values rather than financial so need
# to reduce by a quarter to format as financial years
pivot_dta$`Financial Year` <- pivot_dta$Quarter -1/4
pivot_dta$`Financial Year` <- gsub("\\ ", 
                                   "-", 
                                   pivot_dta$`Financial Year`, 
                                   perl = TRUE)

pivot_dta$`Financial Year` <- pivot_dta %>% 
  select(contains("Financial Year")) %>% 
  # extracts just the year value - 1st year in the financial year
  apply(2, function(x) gsub("-Q[0-9]", "", x)) %>% 
  as.numeric(.) %>%
  data.frame() %>% 
  # gets the second year value - 2nd year in the financial year
  mutate(nxt = .+ 1) %>% 
  # extract just the last 2 digits of the 2nd financial year
  mutate(nxt = substr(nxt, 3, 4)) %>% 
  # adds a separator between the 2 years to format as a financial year
  mutate(fy = paste(., nxt, sep = "/")) %>%
  pull(fy)

# Adds in column with quarter info
# Original formatting uses calender year values rather than financial so need
# to reduce by a quarter to format as financial years
pivot_dta$Quarter <- pivot_dta$Quarter- 1/4
pivot_dta$Quarter <- gsub("[0-9]*\\ Q", 
                          "Quarter ", 
                          pivot_dta$Quarter, 
                          perl = TRUE)

# Remove redundant columns and reorder
pivot_dta <- pivot_dta %>%
  select("Local Authority Name", 
         "Quarter", 
         "Financial Year",
         starts_with("Q1."),
         starts_with("Q2."),
         "Indicator",
         "value")

# Tidy up questions
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

dwnld_table_dta <- read_csv("survey_data.csv", col_types = "c") %>% 
  select(-"Tracking Link")
dwnld_table_dta[is.na(dwnld_table_dta$`Local Authority Name`), "Local Authority Name"] <- "-"
dwnld_table_dta$`Ended date` <- as.Date(dwnld_table_dta$`Ended date`, 
                                        format = "%d/%m/%Y")

# Need to code these as characters because when all responses are in the first column, 
# this is numeric and the other column is character, meaning when the code combines them later it won't work
dwnld_table_dta$`Q. Please select a local authority` <- as.character(dwnld_table_dta$`Q. Please select a local authority`)
dwnld_table_dta$`Q. Please select the local authority that your response relates to` <- as.character(dwnld_table_dta$`Q. Please select the local authority that your response relates to`)

##an additional entry for "West Lothian;" was created causing an issue for their dashboard - fix this
dwnld_table_dta[dwnld_table_dta$`Q. Please select the local authority that your response relates to` == "33","Q. Please select the local authority that your response relates to" ] <- "32"
dwnld_table_dta[dwnld_table_dta$`Q. Please select a local authority` == "33","Q. Please select a local authority" ] <- "32"


# Add in columns with Quarter Info and Financial Year info
# Formats the ended date as a year and quarter value
dwnld_table_dta$`Tracking Link` <- as.yearqtr(dwnld_table_dta$`Ended date`,
                                              format = "%Y-%m-%d") 
# This formatting uses calender year values rather than financial so need
# to reduce by a quarter to format as financial years
dwnld_table_dta$`Financial Year` <- dwnld_table_dta$`Tracking Link` - 1/4
dwnld_table_dta$`Financial Year` <- gsub("\\ ",
                                         "-", 
                                         dwnld_table_dta$`Financial Year`, 
                                         perl = TRUE)

dwnld_table_dta$`Financial Year` <- dwnld_table_dta %>% 
  select(contains("Financial Year")) %>% 
  # extracts just the year value - 1st year in the financial year
  apply(2, function(x) gsub("-Q[0-9]", "", x)) %>% 
  as.numeric(.) %>%
  data.frame() %>% 
  # gets the second year value - 2nd year in the financial year
  mutate(nxt = .+ 1) %>% 
  # extract just the last 2 digits of the 2nd financial year
  mutate(nxt = substr(nxt, 3, 4)) %>% 
  # adds a separator between the 2 years to format as a financial year
  mutate(fy = paste(., nxt, sep = "/")) %>%
  pull(fy)

# Adds in column with quarter info
# Original formatting uses calender year values rather than financial so need
# to reduce by a quarter to format as financial years
dwnld_table_dta$`Tracking Link` <- dwnld_table_dta$`Tracking Link`- 1/4
dwnld_table_dta$`Tracking Link` <- gsub("[0-9]*\\ Q", 
                                        "Quarter ", 
                                        dwnld_table_dta$`Tracking Link`, 
                                        perl = TRUE)

# Remove redundant columns and reorder
dwnld_table_dta <- dwnld_table_dta[-c(1:8, 10)]
dwnld_table_dta <- dwnld_table_dta[, c((ncol(dwnld_table_dta) - 1),
                                       ncol(dwnld_table_dta),
                                       2, 
                                       11, 
                                       12, 
                                       3:10, 
                                       13:(ncol(dwnld_table_dta) - 2), 
                                       1)]

# Pivot to combine both LA columns, rename, then remove duplicates
dwnld_table_dta <- dwnld_table_dta %>% 
  pivot_longer(cols = 4:5, names_to = "extra", values_to = "LA") %>%
  filter(LA != "-") %>% 
  select(-extra)
dwnld_table_dta <- dwnld_table_dta[, c(1:3, 
                                       ncol(dwnld_table_dta),
                                       4:(ncol(dwnld_table_dta) - 1))]

# Code local authority name for councils completing survey without login
dwnld_table_dta <- merge(dwnld_table_dta, LA_names_dta)
dwnld_table_dta$`Local Authority Name` <- dwnld_table_dta$LA_names
dwnld_table_dta <- dwnld_table_dta %>% select(-LA_names)
dwnld_table_dta <- dwnld_table_dta[, c(2:4, 1, 5:ncol(dwnld_table_dta))]

# Rename the columns containing the same questions so they match
# Rename comments columns

# Time Taken
colnames(dwnld_table_dta)[c(26, 47, 70)] <- colnames(dwnld_table_dta)[13]
colnames(dwnld_table_dta)[14] <- "Time taken comments"
colnames(dwnld_table_dta)[c(27, 48, 71)] <- colnames(dwnld_table_dta)[14]

# Communication
colnames(dwnld_table_dta)[c(28, 49, 72)] <- colnames(dwnld_table_dta)[15]
colnames(dwnld_table_dta)[16] <- "Communication comments"
colnames(dwnld_table_dta)[c(29, 50, 73)] <- colnames(dwnld_table_dta)[16]

# The column name of the question ("Quality of the information provided") is the same for the additional question council
# Therefore the column name is repeated and when it is read in it adds a number to the end
# Need to remove the number of change the names of each of these columns
colnames(dwnld_table_dta)[17] <- gsub("\\...[1-9]*$", 
                                      "", 
                                      colnames(dwnld_table_dta)[17],
                                      perl = TRUE)

colnames(dwnld_table_dta)[c(30, 56, 74)] <- colnames(dwnld_table_dta)[17]

colnames(dwnld_table_dta)[18] <- gsub("\\...[1-9]*$", 
                                      "", 
                                      colnames(dwnld_table_dta)[18],
                                      perl = TRUE)

colnames(dwnld_table_dta)[c(34, 63, 75)] <- colnames(dwnld_table_dta)[18]

colnames(dwnld_table_dta)[c(35, 57, 76)] <- colnames(dwnld_table_dta)[19]

colnames(dwnld_table_dta)[20] <- "Information, staff, responsiveness comments"
colnames(dwnld_table_dta)[c(36, 58, 77)] <- colnames(dwnld_table_dta)[20]

# Treated fairly
colnames(dwnld_table_dta)[c(37, 65, 78)] <- colnames(dwnld_table_dta)[21]
colnames(dwnld_table_dta)[22] <- "Treated fairly comments"
colnames(dwnld_table_dta)[c(38, 66, 79)] <- colnames(dwnld_table_dta)[22]

# Satisfaction
colnames(dwnld_table_dta)[c(39, 67, 80)] <- colnames(dwnld_table_dta)[23]
colnames(dwnld_table_dta)[24] <- "Overall satisfaction comments"
colnames(dwnld_table_dta)[c(40, 68, 81)] <- colnames(dwnld_table_dta)[24]
colnames(dwnld_table_dta)[c(41, 69,82)] <- colnames(dwnld_table_dta)[25]

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