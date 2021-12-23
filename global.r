library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(fy)
library(readxl)

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
unpivot_data <-read_excel("DummyData.xlsx") %>%  rename(LA  = "Q1. Please select a local authority")

##tidy up other" column names for selecting data later
dta <- dta %>% rename("Q2.4. Other respondent" = "Q2.4. Other (please specify):", 
               "Q3.4. Other reason" = "Q3.4. Other (please specify):") 

##Generate another dataframe with respondent types
resp_dta <- unpivot_data %>% group_by(LA) %>% select(1:10)%>%
  pivot_longer(cols = 3:10, names_to = "Question", values_to = "value")%>% 
  group_by(LA,Question) %>%
  count(value) %>%
  mutate(perc = n/sum(n))
resp_dta$question_type <- ifelse(grepl("Q2", resp_dta$Question), "Type", "Reason")
resp_dta$Question <- gsub("Q[\\.1-9]+\\s", "", resp_dta$Question,perl = T)

