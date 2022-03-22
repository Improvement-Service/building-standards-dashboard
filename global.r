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

#create a list of local authorty names for use in the UI
LA_Names <- c("Aberdeen City", "Aberdeenshire","Angus", "Argyll and Bute" ,     
              "Clackmannanshire","Dumfries and Galloway", "Dundee City",  "East Ayrshire",      
              "East Dunbartonshire", "East Lothian", "East Renfrewshire","Edinburgh",     
              "Na h-Eileanan an Iar", "Falkirk" , "Fife", "Glasgow City",       
              "Highland", "Inverclyde","Midlothian","Moray",             
              "North Ayrshire" ,"North Lanarkshire" ,"Orkney Islands","Perth and Kinross" ,   
              "Renfrewshire", "Scottish Borders" ,"Shetland Islands" ,"South Ayrshire" ,    
              "South Lanarkshire" ,"Stirling","West Dunbartonshire", "West Lothian")

# Create a variable for storing council selection, this will need to be set up to match email addresses
council_fltr <- 1

# Create a variable for storing the current quarter
crnt_date <- as.yearqtr(Sys.Date(), format = "%Y-%m-%d")
fin_yr <- gsub("\\ ", "-", crnt_date, perl=T)
fin_yr <- qtr2fy(fin_yr)
crnt_qtr <- crnt_date + 3/4
crnt_qtr <- gsub("[0-9]*\\ Q", "Quarter ", crnt_qtr, perl = T)

####### Pivoted Data #######

dta <- read_excel("BSD Dummy Data - with add Q.xlsx", col_types = "text") %>% 
  select(!contains(c("Please explain your answer", "other comments", "Please use the comments box")))

# The question set is duplicated across columns to account for skip logic
# Rename the columns containing the same questions so they match
colnames(dta)[c(29,42,57)] <- colnames(dta)[22]
colnames(dta)[c(30,43,58)] <- colnames(dta)[23]
# The column name of the question ("Quality of the information provided") is the same for the additional question council
# Therefore the column name is repeated and when it is read in it adds a number to the end
# Need to remove the number of change the names of each of these columns
colnames(dta)[24] <- gsub("\\...[1-9]*$", "", colnames(dta)[24],perl = T)
colnames(dta)[c(31,48,59)] <- colnames(dta)[24]

colnames(dta)[25] <- gsub("\\...[1-9]*$", "", colnames(dta)[25],perl = T)
colnames(dta)[c(35,54,60)] <- colnames(dta)[25]

colnames(dta)[c(36,49,61)] <- colnames(dta)[26]
colnames(dta)[c(37,55,62)] <- colnames(dta)[27]
colnames(dta)[c(38,56,63)] <- colnames(dta)[28]

# Remove columns containing additional questions
dta <- dta[-c(32,33,34,39,40,41,44,45,46,47,50,51,52,53)]

# Pivot indicator data 
dta <- dta %>% pivot_longer(cols = 22:28, names_to = "Indicator", values_to ="value")

# As the columns were duplicated there are "-" blank values where the question would be skipped
# Need to remove these to get the complete data set
dta <- dta %>% filter(value != "-")

# Add in columns with Quarter Info and Financial Year info
dta$`Tracking Link` <- as.yearqtr(dta$`Ended date`, format = "%Y-%m-%d") 
dta$`Financial Year` <- dta$`Tracking Link`
dta$`Financial Year` <- gsub("\\ ", "-", dta$`Financial Year`, perl=T)
dta$`Financial Year` <- qtr2fy(dta$`Financial Year`)

dta$`Tracking Link` <- dta$`Tracking Link`+ 3/4
dta$`Tracking Link` <- gsub("[0-9]*\\ Q", "Quarter ", dta$`Tracking Link`, perl = T)

# Remove redundant columns and reorder
dta <- dta[-c(1:10)]
dta <- dta[,c(14,15,1,10,11,2:9,12:13)]

# pivot to combine both LA columns, rename, then remove duplicates
dta <- dta %>% pivot_longer(cols = 4:5, names_to = "extra", values_to ="LA") %>%
  filter(LA != "-") %>% select(-extra)
dta <- dta[,c(1:3, 14, 4:13)]

#remove question numbers from Indicator column and tidy up questions
dta$Indicator <- gsub("Q[\\.1-9]+\\s", "", dta$Indicator,perl = T)
dta$Indicator <- gsub(" by \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards", "", dta$Indicator)
dta$Indicator <- gsub(" with \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\] Building Standards from beginning to end", "", dta$Indicator)
dta[dta$Indicator == "How would you rate the standard of communication provided service following your initial contact or once your application had been submitted?", "Indicator"] <- "How would you rate the standard of communication provided?"
dta[dta$Indicator == "Thinking of your engagement, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?", "Indicator"] <- "Thinking of your engagement, how satisfied were you with the time taken to complete the process?"
dta[dta$Indicator == "Time taken to respond to any queries or issues raised", "Indicator"] <- "Responsiveness to any queries or issues raised"

##NA values are coded as 5 by SmartSurvey - replace with NA
dta$value <- replace(dta$value,dta$value == "5", NA)

##tidy up other" column names for selecting data later
dta <- dta %>% rename("Q1.4. Other respondent" = "Q1.4. Other (please specify):", 
                      "Q2.4. Other reason" = "Q2.4. Other (please specify):") 

###### Unpivoted data ########

#create data that is not pivoted for download
unpivot_data <- read_excel("BSD Dummy Data - with add Q.xlsx", col_types = "text") 

# Select columns based on council (ensures duplicate columns and questions from other councils are filtered out)
dl_all_data <- if(council_fltr == 12)
{unpivot_data[,c(7:21,79:91)]} else
  if(council_fltr == 22)
  {unpivot_data[,c(7:34,51:55)]}else
    if(council_fltr == 23)
    {unpivot_data[,c(7:21,56:78)]}else
      if(council_fltr == 32)
      {unpivot_data[,c(7:21,35:50)]}else
      {unpivot_data[,c(7:34)]}

# Add in column with Quarter Info
#dl_all_data$`Tracking Link` <- as.yearqtr(dl_all_data$`Ended date`, format = "%Y-%m-%d") + 3/4
#dl_all_data$`Tracking Link` <- gsub("[0-9]*\\ Q", "Quarter ", dl_all_data$`Tracking Link`, perl = T)

# Add in columns with Quarter Info and Financial Year info
dl_all_data$`Tracking Link` <- as.yearqtr(dl_all_data$`Ended date`, format = "%Y-%m-%d") 
dl_all_data$`Financial Year` <- dl_all_data$`Tracking Link`
dl_all_data$`Financial Year` <- gsub("\\ ", "-", dl_all_data$`Financial Year`, perl=T)
dl_all_data$`Financial Year` <- qtr2fy(dl_all_data$`Financial Year`)

dl_all_data$`Tracking Link` <- dl_all_data$`Tracking Link`+ 3/4
dl_all_data$`Tracking Link` <- gsub("[0-9]*\\ Q", "Quarter ", dl_all_data$`Tracking Link`, perl = T)


# Remove redundant columns and reorder
dl_all_data <- dl_all_data[-c(1:4)]
dl_all_data <- dl_all_data[,c((ncol(dl_all_data)-1),ncol(dl_all_data),1,10,11,2:9,12:(ncol(dl_all_data)-2))]

# pivot to combine both LA columns, rename, then remove duplicates
dl_all_data <- dl_all_data %>% pivot_longer(cols = 4:5, names_to = "extra", values_to ="LA") %>%
  filter(LA != "-") %>% select(-extra)
dl_all_data <- dl_all_data[,c(1:3,ncol(dl_all_data),4:(ncol(dl_all_data)-1))]  

######## Respondents and reasons data ############

# Generate another dataframe with respondent types
resp_dta <- dl_all_data %>% group_by(LA) %>% select(1:12)%>%
  pivot_longer(cols = 5:12, names_to = "Question", values_to = "value")%>% 
  group_by(LA,Question) %>%
  count(value) %>%
  mutate(perc = n/sum(n))

##Tidy the respondent types and reasons
resp_dta$question_type <- ifelse(grepl("Q1", resp_dta$Question), "Type", "Reason")

##Remove question numbers
resp_dta$Question <- gsub("Q[\\.1-9]+\\s", "", resp_dta$Question,perl = T)

##Filter to selected council
resp_dta <- resp_dta%>%filter(LA == council_fltr)

# Filter to council for dta download  
dl_all_data <- dl_all_data %>% filter(LA == council_fltr)


############### Data for data download table ############# 

# This data set needs to be unpivoted but without the additional questions and the questions named the same

# Add in columns with Quarter Info and Financial Year info
unpivot_data$`Tracking Link` <- as.yearqtr(unpivot_data$`Ended date`, format = "%Y-%m-%d") 
unpivot_data$`Financial Year` <- unpivot_data$`Tracking Link`
unpivot_data$`Financial Year` <- gsub("\\ ", "-", unpivot_data$`Financial Year`, perl=T)
unpivot_data$`Financial Year` <- qtr2fy(unpivot_data$`Financial Year`)

unpivot_data$`Tracking Link` <- unpivot_data$`Tracking Link`+ 3/4
unpivot_data$`Tracking Link` <- gsub("[0-9]*\\ Q", "Quarter ", unpivot_data$`Tracking Link`, perl = T)

# Remove redundant columns and reorder
unpivot_data <- unpivot_data[-c(1:10)]
unpivot_data <- unpivot_data[,c((ncol(unpivot_data)-1),ncol(unpivot_data),1,10,11,2:9,12:(ncol(unpivot_data)-2))]

# pivot to combine both LA columns, rename, then remove duplicates
unpivot_data <- unpivot_data %>% pivot_longer(cols = 4:5, names_to = "extra", values_to ="LA") %>%
  filter(LA != "-") %>% select(-extra)
unpivot_data <- unpivot_data[,c(1:3,ncol(unpivot_data),4:(ncol(unpivot_data)-1))]

# Rename the columns containing the same questions so they match
# Rename comments columns

#Time Taken
colnames(unpivot_data)[c(26,47,70)] <- colnames(unpivot_data)[13]
colnames(unpivot_data)[14] <- "Time taken comments"
colnames(unpivot_data)[c(27,48,71)] <- colnames(unpivot_data)[14]

# Communication
colnames(unpivot_data)[c(28,49,72)] <- colnames(unpivot_data)[15]
colnames(unpivot_data)[16] <- "Communication comments"
colnames(unpivot_data)[c(29,50,73)] <- colnames(unpivot_data)[16]


# The column name of the question ("Quality of the information provided") is the same for the additional question council
# Therefore the column name is repeated and when it is read in it adds a number to the end
# Need to remove the number of change the names of each of these columns
colnames(unpivot_data)[17] <- gsub("\\...[1-9]*$", "", colnames(unpivot_data)[17],perl = T)
colnames(unpivot_data)[c(30,56,74)] <- colnames(unpivot_data)[17]

colnames(unpivot_data)[18] <- gsub("\\...[1-9]*$", "", colnames(unpivot_data)[18],perl = T)
colnames(unpivot_data)[c(34,63,75)] <- colnames(unpivot_data)[18]

colnames(unpivot_data)[c(35,57,76)] <- colnames(unpivot_data)[19]

colnames(unpivot_data)[20] <- "Information, staff, responsiveness comments"
colnames(unpivot_data)[c(36,58,77)] <- colnames(unpivot_data)[20]

# Treated fairly
colnames(unpivot_data)[c(37,65,78)] <- colnames(unpivot_data)[21]
colnames(unpivot_data)[22] <- "Treated fairly comments"
colnames(unpivot_data)[c(38,66,79)] <- colnames(unpivot_data)[22]

# Satisfaction
colnames(unpivot_data)[c(39,67,80)] <- colnames(unpivot_data)[23]
colnames(unpivot_data)[24] <- "Overall satisfaction comments"
colnames(unpivot_data)[c(40,68,81)] <- colnames(unpivot_data)[24]


colnames(unpivot_data)[c(41,69,82)] <- colnames(unpivot_data)[25]

# Select columns based on council (ensures duplicate columns and additional questions are filtered out)
unpivot_data <- if(council_fltr == 12)
{unpivot_data[,c(1:12,70:82)]} else
  if(council_fltr == 23)
  {unpivot_data[,c(1:12,47:50,56:58,63,65:69)]}else
    if(council_fltr == 32)
    {unpivot_data[,c(1:12,26:30,34:41)]}else
    {unpivot_data[,c(1:25)]}

#tidy up question names
unpivot_data <- unpivot_data %>% rename("Q3. How satisfied were you with the time taken?" = "Q3. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?") %>%
  rename("Q4. How would you rate the standard of communication?" = "Q4. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?") %>%
  rename("Q.3. Responsiveness to any queries or issues raised" = "Q.3. Time taken to respond to any queries or issues raised") %>%
  rename("Q5. To what extent would you agree that you were treated fairly" = "Q5. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?") %>%
  rename("Q6. How satisfied were you, overall?" = "Q6. Overall, how satisfied were you with the service provided by [question(16082428)][variable(la)] Building Standards?")%>%
  rename("Q1.4. Other respondent" = "Q1.4. Other (please specify):") %>%
  rename("Q2.4. Other reason" = "Q2.4. Other (please specify):") 

#recode responses for download and to show in table
unpivot_data$`Q3. How satisfied were you with the time taken?` <-  dplyr::recode(
  unpivot_data$`Q3. How satisfied were you with the time taken?`,
  "1" = "very satisfied",
  "2" ="satisfied",
  "3" = "dissatisfied",
  "4" = "very dissatisfied",
  "5" = "NA"
)

unpivot_data$`Q4. How would you rate the standard of communication?` <-  dplyr::recode(
  unpivot_data$`Q4. How would you rate the standard of communication?`,
  "1" = "very good",
  "2" ="good",
  "3" = "poor",
  "4" = "very poor",
  "5" = "NA"
)

unpivot_data$`Q.1. Quality of the information provided` <-  dplyr::recode(
  unpivot_data$`Q.1. Quality of the information provided`,
  "1" = "very good",
  "2" ="good",
  "3" = "poor",
  "4" = "very poor",
  "5" = "NA"
)

unpivot_data$`Q.2. Service offered by staff` <-  dplyr::recode(
  unpivot_data$`Q.2. Service offered by staff`,
  "1" = "very good",
  "2" ="good",
  "3" = "poor",
  "4" = "very poor",
  "5" = "NA"
)

unpivot_data$`Q.3. Responsiveness to any queries or issues raised` <-  dplyr::recode(
  unpivot_data$`Q.3. Responsiveness to any queries or issues raised`,
  "1" = "very good",
  "2" ="good",
  "3" = "poor",
  "4" = "very poor",
  "5" = "NA"
)

unpivot_data$`Q5. To what extent would you agree that you were treated fairly`<-  dplyr::recode(
  unpivot_data$`Q5. To what extent would you agree that you were treated fairly`,
  "1" = "very satisfied",
  "2" ="satisfied",
  "3" = "dissatisfied",
  "4" = "very dissatisfied",
  "5" = "NA"
)

unpivot_data$`Q6. How satisfied were you, overall?`<-  dplyr::recode(
  unpivot_data$`Q6. How satisfied were you, overall?`,
  "1" = "very satisfied",
  "2" ="satisfied",
  "3" = "dissatisfied",
  "4" = "very dissatisfied",
  "5" = "NA"
)

# Filter this data for selected council
unpivot_data <- unpivot_data %>% filter(LA == council_fltr)

####### Additional info ############

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
