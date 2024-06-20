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

# Clean Data ----------------------------------------------------

# This cleans the data so that any skipped questions (where it was for another council)
# are removed. In it's current for this data set is used in the server to create
# the data download file. It is formatted further in this script to create data 
# for the data download table in the app and to create the pivot data used in most 
# of the analysis

clean_data <- read_csv("survey_data.csv", col_types = "c") %>%
 select(-"Tracking Link")
clean_data[is.na(clean_data$`Local Authority Name`), "Local Authority Name"] <- "-"
clean_data$`Ended date` <- as.Date(clean_data$`Ended date`,
                                       format = "%d/%m/%Y")

# An additional entry for "West Lothian;" was created causing an issue for their dashboard - fix this
clean_data[clean_data$`Local Authority Name` == "West Lothian;" &
                 clean_data$`Q. Please select the local authority that your response relates to` == "10",
               "Q. Please select the local authority that your response relates to" ] <- "32"
clean_data[clean_data$`Q. Please select a local authority` == "33","Q. Please select a local authority" ] <- "32"
# Add a column to code where West Lothian respondents have answered standard questions
clean_data <- clean_data %>%
 mutate("wl_standard_q" = if_else(`Q. Please select the local authority that your response relates to` == "32",
                                  "Yes",
                                  "No"))

# pivot to combine both LA columns, rename, then remove duplicates
clean_data <- clean_data %>%
 pivot_longer(cols = c(`Q. Please select a local authority`,
                       `Q. Please select the local authority that your response relates to`),
              names_to = "extra", values_to = "LA") %>%
 filter(LA != "-") %>%
 select(-extra)

# Code local authority name for councils completing survey without login
clean_data <- merge(clean_data, LA_names_dta)
clean_data$`Local Authority Name` <- clean_data$LA_names
clean_data <- clean_data %>% select(-LA_names)

# For text columns "Please explain your answer" want to distinguish these by
# adding a prefix of the question they refer to'
# Create a new dataset for the column names
new_comment_names <- tibble("original_names" = colnames(clean_data))
# Add a column with the names lagged so that the question relevant question
# lines up with the "please explain your answer" response
new_comment_names <- new_comment_names %>%
 mutate("lagged_names" = lag(original_names)) %>%
 # Combine the question and please explain your answer into new name
 mutate("new_names" = if_else(grepl("Please explain your answer", original_names),
                              paste(lagged_names, original_names),
                              original_names))
# Add new names to dataset
colnames(clean_data) <- new_comment_names$new_names

# Add council names to columns
# Create a second data set with new column names
new_col_names_dta_2 <- clean_data %>%
 pivot_longer(cols = -c(`LA`:`Q2.4. Other (please specify):`, wl_standard_q),
              names_to = "col_names",
              values_to = "value") %>%
 # These next two steps mean there is one council associated with each column name
 # For councils with additional questions, only that council will have non "-"
 # responses so the council name will match up
 filter(value != "-") %>%
 distinct(col_names, .keep_all = TRUE) %>%
 mutate("new_col_names" = col_names) %>%
 # Add council name to additional questions so these can be referenced easily
 mutate(new_col_names = if_else(`Local Authority Name` %in% c("Angus",
                                                              "City of Edinburgh",
                                                              "North Lanarkshire",
                                                              "Orkney Islands",
                                                              "West Lothian"),
                                paste0(new_col_names, "_", `Local Authority Name`),
                                new_col_names)) %>%
 select(col_names, new_col_names)


clean_data <- clean_data %>%
 pivot_longer(cols = -c(`LA`:`Q2.4. Other (please specify):`, wl_standard_q),
              names_to = "col_names", values_to = "value")

# Match the new column names to the old ones
clean_data <- merge(clean_data, new_col_names_dta_2) %>%
 select(-col_names) %>%
 # Replace names for comments questions
 mutate(new_col_names = str_replace_all(new_col_names,
                                        c("Thinking of your engagement with \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised\\? Please explain your answer\\:" =
                                            "Time taken comments",
                                          "How would you rate the standard of communication provided by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards service following your initial contact or once your application had been submitted\\? Please explain your answer\\:" =
                                            "Communication comments",
                                          "Time taken to respond to any queries or issues raised Q\\.[1-9]\\. Please explain your answers\\:" =
                                            "Information, staff, responsiveness comments",
                                          "Time taken to respond to any queries or issues raised\\. Our target response times are 10 days for emails and 2 days for phone calls Q\\.[0-9]\\. Please explain your answers\\:" =
                                            "Information, staff, responsiveness comments",
                                          "Time taken to respond to any queries or issues raised Q\\.[1-9]\\. Please explain your answers\\:" =
                                            "Information, staff, responsiveness comments",
                                          "To what extent would you agree that you were treated fairly by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards\\? Please explain your answer\\:" =
                                            "Treated fairly comments",
                                          "Overall, how satisfied were you with the service provided by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards\\? Please explain your answer\\:" =
                                            "Overall satisfaction comments",
                                          "If you have any other comments about your experience, please use this space to leave these\\:\\sPlease note that we are unable to reply to specific cases, if you would like to discuss your experience further, please contact the Council directly\\." =
                                            "Other comments",
                                          "If you have any other comments about your experience, please use this space to leave these\\:\\r\\nPlease note that we are unable to reply to specific cases, if you would like to discuss your experience further, please contact the Council directly\\." =
                                            "Other comments",
                                          "If you have any other comments about your experience, please use this space to leave these\\:" =
                                            "Other comments"))) %>%
 # Remove question number from start of question
 mutate(new_col_names = str_remove_all(new_col_names, "^[^\\s]*\\s")) %>%
 # Remove numbers at end to note duplicate questions
 mutate(new_col_names = str_remove_all(new_col_names, "\\.\\.\\.[0-9]*")) %>%
 # Remove reference to unique link
 mutate(new_col_names = str_remove_all(new_col_names, " by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards")) %>%
 mutate(new_col_names = str_remove_all(new_col_names, " with \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards from beginning to end")) %>%
 # Some of the standard questions are worded differently for the councils with
 # additional questions. Need to rename so these can be matched
 mutate(new_col_names = str_replace_all(new_col_names,
                                        c("Overall service offered by staff" = "Service offered by staff",
                                          "Overall, how would you rate the service offered by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Council staff\\?" = "Service offered by staff",
                                          "Time taken to respond to any queries or issues raised\\. Our target response times are 10 days for emails and 2 days for phone calls" = "Time taken to respond to any queries or issues raised")))



# Where the question has not been answered this needs to be changed to NA
# These are currently "-" but need to be recoded to distinguish where the
# question is skipped vs when it is not applicable to the council

# Replace blanks for respondents who completed Angus specific questions
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "Angus" &
                          value == "-" &
                          grepl("_Angus", new_col_names),
                        NA,
                        value))

# Replace blanks for respondents who completed Edinburgh specific questions
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "City of Edinburgh" &
                          value == "-" &
                          grepl("_City of Edinburgh", new_col_names),
                        NA,
                        value))

# Replace blanks for respondents who completed North Lanarkshire specific questions
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "North Lanarkshire" &
                          value == "-" &
                          grepl("_North Lanarkshire", new_col_names),
                        NA,
                        value))

# Replace blanks for respondents who completed Orkney specific questions
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "Orkney Islands" &
                          value == "-" &
                          grepl("_Orkney Islands", new_col_names),
                        NA,
                        value))
# For West Lothian questions replace blank values with NA
# (West Lothian had a few responses where respondents answered the standard
# question set rather than the specific West Lothian set, so need to account for
# both of these scenarios). If the response is "-" in both the West Lothian specific
# questions and the standard questions this is a genuine NA, but if it is only
# "-" in that's because the question was skipped. Don't want to record as NA in
# this scenario as respondent will be counted twice
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "West Lothian" &
                          wl_standard_q == "No" &
                          value == "-" &
                          grepl("_West Lothian", new_col_names),
                        NA,
                        value))
# Replace blanks for respondents who completed standard questions
clean_data <- clean_data %>%
 mutate(value = if_else(!`Local Authority Name` %in% c("City of Edinburgh",
                                                       "Orkney Islands",
                                                       "West Lothian") &
                          value == "-" &
                          # This pulls out standard questions
                          !grepl("_Angus|_City of Edinburgh|_Orkney Islands|_North Lanarkshire|_West Lothian",
                                 new_col_names),
                        NA,
                        value))
# Need to account for West Lothian respondents who answered standard questions
clean_data <- clean_data %>%
 mutate(value = if_else(`Local Authority Name` == "West Lothian" &
                          wl_standard_q == "Yes" &
                          value == "-" &
                          # This pulls out standard questions
                          !grepl("_Angus|_City of Edinburgh|_Orkney Islands|_North Lanarkshire|_West Lothian",
                                 new_col_names),
                        NA,
                        value)) %>%
  # Add in columns with Quarter Info and Financial Year info
  # Formats the ended date as a year and quarter value
  mutate(Quarter = as.yearqtr(`Ended date`, format = "%Y-%m-%d")) %>%
  # This formatting uses calender year values rather than financial so need
  # to reduce by a quarter to format as financial years
  mutate("Financial Year" = Quarter -1/4) %>%
  mutate(`Financial Year` = str_replace(`Financial Year`, "\\ ", "-"))

clean_data$`Financial Year` <- clean_data %>% 
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
clean_data <- clean_data %>%
  mutate(Quarter = Quarter -1/4) %>%
  mutate(Quarter = str_replace(Quarter, "[0-9]*\\ Q", "Quarter ")) %>% 
  # NA values are coded as 5 by SmartSurvey - replace with NA
  mutate(value = str_replace(value, "5", NA_character_)) %>%
  mutate(new_col_names = str_remove_all(new_col_names, "_.*$")) %>%
  # Remove skipped values (-)
  filter(value != "-")

# Download Table Data ----------------------------------------------------

# This data set needs to be unpivoted but without the additional questions
# and the questions named the same

dwnld_table_dta <- clean_data %>% 
  # Remove additional questions
  filter(!new_col_names %in% c("Accuracy of the information provided as relevant to your needs",
                               "Attitude in terms of friendliness and helpfulness of our staff",
                               "Did you find it easy to contact the officer/inspector/administrator you were looking for?",
                               "Easy to find",
                               "Finally, if you are responding in relation to a Completion Certificate did your experience include a Remote Verification Inspection (RVI) whereby the inspection is via live or pre-recorded video?",
                               "How satisfied were you with the building warrant approval process?",
                               "How satisfied were you with the building warrant approval process? Please explain your answer:",
                               "How satisfied were you with the range of options provided by [question(16082428)][variable(la)] Council relating to inspections?",
                               "How satisfied were you with the range of options provided by [question(16082428)][variable(la)] Council relating to inspections? Please explain your answer:",
                               "Please provide further information about your answers:",
                               "Please rate your most recent experience with the service.",
                               "Professionalism in terms of the knowledge and skills of our staff",
                               "Service offered by staff Please explain your answers:",
                               "To what extent would you agree [question(16082428)][variable(la)] Council have used digital technology to make building standards processes easier for you (for example, around plan approval and site inspections)?",
                               "Staff were polite and courteous",
                               "Staff were helpful",
                               "Staff were efficient",
                               "Staff were knowledgeable",
                               "Understandable",
                               "use the comments box below to provide more information, including your preferred method for contacting the Council.")) %>%
  # Rename indicator names to format used in dashboard
  mutate(new_col_names = str_replace_all(new_col_names, 
                                         c("Thinking of your engagement, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised\\?" = "How satisfied were you with the time taken?",
                                           "How would you rate the standard of communication provided service following your initial contact or once your application had been submitted\\?" = "How would you rate the standard of communication?",
                                           "Time taken to respond to any queries or issues raised" = "Responsiveness to any queries or issues raised",
                                           "Overall, how satisfied were you with the service provided\\?" = "How satisfied were you overall?"))) %>%
  pivot_wider(names_from = new_col_names, values_from = value) %>%
  # Remove redundant columns and reorder
  select("Ended date",
         "Quarter", 
         "Financial Year",
         "Local Authority Name",
         starts_with("Q1."),
         starts_with("Q2."),
         contains("Time taken"),
         contains("Communication"),
         contains("Quality"),
         contains("Service offered"),
         contains("issues raised"),
         contains("Information"),
         contains("treated fairly"),
         contains("overall"),
         contains("other comments")) %>%
  # Tidy up other column names for selecting data later
  rename("Agent/Designer" = "Q1.1. Agent/Designer", 
         "Applicant" = "Q1.2. Applicant", 
         "Contractor" = "Q1.3. Contractor",
         "Other respondent" = "Q1.4. Other (please specify):", 
         "To discuss your proposal" = "Q2.1. To discuss your proposal before applying for a building warrant",
         "To make an application" = "Q2.2. To make an application for a building warrant", 
         "During construction" = "Q2.3. During construction, including submission of a completion certificate",
         "Other reason" = "Q2.4. Other (please specify):",
         "Submission date" = "Ended date") 

 # Pivoted Data --------------------------------------------------------------
 
 pivot_dta <- dwnld_table_dta %>%
   # Remove comments columns
   select(-`Submission date`, -contains("comments")) %>%
   # pivot to long format
   pivot_longer(`How satisfied were you with the time taken?`:`How satisfied were you overall?`,
                names_to = "Indicator",
                values_to = "value")
 
 # Recode "other" respondents and reasons so it doesn't show text value
 pivot_dta$`Other respondent`[pivot_dta$`Other respondent` != "0"] <- "1"
 pivot_dta$`Other reason`[pivot_dta$`Other reason` != "0"] <- "1"
 pivot_dta <- pivot_dta %>% 
   mutate(across(c(`Other respondent`, `Other reason`), ~as.numeric(.)))
   
 
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