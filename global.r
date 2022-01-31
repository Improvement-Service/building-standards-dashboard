library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(fy)
library(readxl)
library(shinyBS)
library(DT)


#create a list of local authorty names for use in the UI
LA_Names <- c("Aberdeen City", "Aberdeenshire","Angus", "Argyll and Bute" ,     
                "Clackmannanshire","Dumfries and Galloway", "Dundee City",  "East Ayrshire",      
                "East Dunbartonshire", "East Lothian", "East Renfrewshire","Edinburgh",     
                "Na h-Eileanan an Iar", "Falkirk" , "Fife", "Glasgow City",       
                "Highland", "Inverclyde","Midlothian","Moray",             
                "North Ayrshire" ,"North Lanarkshire" ,"Orkney Islands","Perth and Kinross" ,   
                "Renfrewshire", "Scottish Borders" ,"Shetland Islands" ,"South Ayrshire" ,    
                "South Lanarkshire" ,"Stirling","West Dunbartonshire", "West Lothian")

dta <- read_excel("DummyData.xlsx", col_types = "text") %>% select(!contains(c("Please explain your answer", "other comments"))) %>%
  pivot_longer(cols = 11:ncol(.), names_to = "Indicator", values_to ="value") %>%
  rename(LA  = "Q1. Please select a local authority") 
#remove question numbers from Indicator column and tidy up questions
dta$Indicator <- gsub("Q[\\.1-9]+\\s", "", dta$Indicator,perl = T)
dta$Indicator <- gsub(" by \\[question\\(15510346\\)\\] Building Standards", "", dta$Indicator)
dta$Indicator <- gsub(" with \\[question\\(15510346\\)\\] Building Standards from beginning to end", "", dta$Indicator)
dta[dta$Indicator == "How would you rate the standard of communication provided by the Building Standards service following your initial contact or once your application had been submitted?", "Indicator"] <- "How would you rate the standard of communication provided?"

##NA values are coded as 5 by SmartSurvey - replace with NA
dta$value <- replace(dta$value,dta$value == "5", NA)
#create data that is not pivoted for download
unpivot_data <-read_excel("DummyData.xlsx") %>%  rename(LA  = "Q1. Please select a local authority")
dl_all_data <- unpivot_data%>% filter(LA == "1")


##tidy up other" column names for selecting data later
dta <- dta %>% rename("Q2.4. Other respondent" = "Q2.4. Other (please specify):", 
               "Q3.4. Other reason" = "Q3.4. Other (please specify):") 

##Generate another dataframe with respondent types
resp_dta <- unpivot_data %>% group_by(LA) %>% select(1:10)%>%
  pivot_longer(cols = 3:10, names_to = "Question", values_to = "value")%>% 
  group_by(LA,Question) %>%
  count(value) %>%
  mutate(perc = n/sum(n))

##Tidy the respondent types and reasons
resp_dta$question_type <- ifelse(grepl("Q2", resp_dta$Question), "Type", "Reason")

##Remove question numbers
resp_dta$Question <- gsub("Q[\\.1-9]+\\s", "", resp_dta$Question,perl = T)

#custom function for checking if vector is empty
isEmpty <- function(x) {
  return(length(x)==0)
}

##create popover text
KPO_popover_text<-paste("KPO4 is calculated by applying the following weightings:",
              "Overall satisfaction - 50%",
              "Communications and time taken - 12.5% each",
              "Staff, information, responsiveness, fairness - 6.25% each.",
              sep = "<br>")

##For text analysis page - rename columns with comments
unpivot_data <- unpivot_data %>% rename("Time taken comments" = "Please explain your answer:...12",
                                      "Communication comments" =  "Please explain your answer:...14",
                                      "Information, staff, responsiveness comments" = "Q.4. Please explain your answers:",
                                      "Treated fairly comments" = "Please explain your answer:...20",
                                      "Overall satisfaction comments" = "Please explain your answer:...22") %>%
                rename("Q2.4. Other respondent" = "Q2.4. Other (please specify):", 
                      "Q3.4. Other reason" = "Q3.4. Other (please specify):") 
#tidy up question names
unpivot_data <- unpivot_data %>% rename("Q4. How satisfied were you with the time taken?" = "Q4. Thinking of your engagement with [question(15510346)] Building Standards from beginning to end, how satisfied were you with the time taken to complete the process?") %>%
  rename("Q5. How would you rate the standard of communication?" = "Q5. How would you rate the standard of communication provided by the Building Standards service following your initial contact or once your application had been submitted?") %>%
  rename("Q6. To what extent would you agree that you were treated fairly" = "Q6. To what extent would you agree that you were treated fairly by [question(15510346)] Building Standards?") %>%
  rename("Q7. How satisfied were you, overall?" = "Q7. Overall, how satisfied were you with the service provided by [question(15510346)] Building Standards?")

#recode responses for download and to show in table
unpivot_data$`Q4. How satisfied were you with the time taken?` <-  dplyr::recode(unpivot_data$`Q4. How satisfied were you with the time taken?`,"1" = "very satisfied",
                                                                                       "2" ="satisfied",
                                                                                       "3" = "dissatisfied",
                                                                                       "4" = "very dissatisfied")

unpivot_data$`Q5. How would you rate the standard of communication?` <-  dplyr::recode(unpivot_data$`Q5. How would you rate the standard of communication?`,"1" = "very good",
                                                                                          "2" ="good",
                                                                                          "3" = "poor",
                                                                                          "4" = "very poor")
unpivot_data$`Q.1. Quality of the information provided` <-  dplyr::recode(unpivot_data$`Q.1. Quality of the information provided`,"1" = "very good",
                                                                                       "2" ="good",
                                                                                       "3" = "poor",
                                                                                       "4" = "very poor")
unpivot_data$`Q.2. Service offered by staff` <-  dplyr::recode(unpivot_data$`Q.2. Service offered by staff`,"1" = "very good",
                                                                          "2" ="good",
                                                                          "3" = "poor",
                                                                          "4" = "very poor")
unpivot_data$`Q.3. Responsiveness to any queries or issues raised` <-  dplyr::recode(unpivot_data$`Q.3. Responsiveness to any queries or issues raised`,"1" = "very good",
                                                               "2" ="good",
                                                               "3" = "poor",
                                                               "4" = "very poor")
unpivot_data$`Q6. To what extent would you agree that you were treated fairly`<-  dplyr::recode(unpivot_data$`Q6. To what extent would you agree that you were treated fairly`,"1" = "very satisfied",
                                                                                 "2" ="satisfied",
                                                                                 "3" = "dissatisfied",
                                                                                 "4" = "very dissatisfied")
unpivot_data$`Q7. How satisfied were you, overall?`<-  dplyr::recode(unpivot_data$`Q7. How satisfied were you, overall?`,"1" = "very satisfied",
                                                                                                "2" ="satisfied",
                                                                                                "3" = "dissatisfied",
                                                                                                "4" = "very dissatisfied")


