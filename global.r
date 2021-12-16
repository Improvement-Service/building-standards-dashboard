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
unpivot_data <-read_excel("DummyData.xlsx")
