function(input, output, session) {
# Set up reactive council selection based on log in -------------------------
  user <- reactive({
    session$user
    })
  
  # LA selection generated for IS & SG users
  output$la_select <- renderUI({
    user <- user()
    if (grepl("improvementservice.org.uk|gov.scot", 
              user, 
              ignore.case = TRUE)) {
      selectizeInput(inputId = "LA_selection", 
                     label = "",
                     choices = LA_names, 
                     options = list(placeholder = "Select Your Local Authority",
                                    onInitialize = I('function() { this.setValue(""); }')
                                    )
                     )
    } else {
      return()
    }
    })
  
  # If statement to determine local authority filter.
  # Will use either LA name if LA log in or LA drop down selection if 
  # IS or SG
  local_authority <- reactive({
    user <- user()
    # Aberdeen City
    if (grepl("aberdeencity.gov.uk", user, ignore.case = TRUE)) {
      return("Aberdeen City")
    } else 
    # Aberdeenshire  
      if (grepl("aberdeenshire.gov.uk", user, ignore.case = TRUE)) {
        return("Aberdeenshire")
      } else 
    # Angus
        if (grepl("angus.gov.uk", user, ignore.case = TRUE)) {
          return("Angus")
        } else 
    # Argyll and Bute
          if(grepl("argyll-bute.gov.uk", user, ignore.case = TRUE)) {
            return("Argyll and Bute")
          } else 
    # City of Edinburgh    
            if(grepl("edinburgh.gov.uk", user, ignore.case = TRUE)){
              return("City of Edinburgh")
            } else 
    # Clackmannanshire
              if (grepl("clacks.gov.uk", user, ignore.case = TRUE)) {
                return("Clackmannanshire")
              } else 
    # Eilean Siar
                if (grepl("cne-siar.gov.uk", user, ignore.case = TRUE)) {
                  return("Eilean Siar")
                } else 
    # Dumfries and Galloway
                  if (grepl("dumgal.gov.uk", user, ignore.case = TRUE)) {
                    return("Dumfries and Galloway")
                  } else 
    # Dundee City          
                    if (grepl("dundeecity.gov.uk", user, ignore.case = TRUE)){
                      return("Dundee City")
                    } else
    # East Ayrshire  
                      if (grepl("east-ayrshire.gov.uk", user, ignore.case = TRUE)) {
                        return("East Ayrshire")
                      } else
    # East Dunbartonshire              
                        if (grepl("eastdunbarton.gov.uk", user, ignore.case = TRUE)) {
                          return("East Dunbartonshire")
                        } else 
    # East Lothian
                          if (grepl("eastlothian.gov.uk", user, ignore.case = TRUE)) {
                            return("East Lothian")
                          } else 
    # East Renfrewshire
                            if(grepl("@eastrenfrewshire.gov.uk", user,ignore.case = TRUE)){
                              return("East Renfrewshire")
                            } else
    # Falkirk
                             if (grepl("falkirk.gov.uk", user, ignore.case = TRUE)) {
                                return("Falkirk")
                              } else 
    # Fife
                                if (grepl("fife.gov.uk", user, ignore.case = TRUE)) {
                                  return("Fife")
                                } else 
    # Glasgow                         
                                  if (grepl("glasgow.gov.uk", user, ignore.case = TRUE)) {
                                    return("Glasgow")
                                  } else 
    # Highland
                                    if (grepl("highland.gov.uk", user,ignore.case = TRUE)) {
                                      return("Highland")
                                    } else 
    # Inverclyde
                                      if (grepl("inverclyde.gov.uk", user, ignore.case = TRUE)) {
                                        return("Inverclyde")
                                      } else 
    # Midlothian                              
                                        if (grepl("midlothian.gov.uk", user, ignore.case = TRUE)) {
                                          return("Midlothian")
                                        } else 
    # Moray
                                          if (grepl("moray.gov.uk", user, ignore.case = TRUE)) {
                                            return("Moray")
                                          } else 
    # North Ayrshire
                                            if (grepl("north-ayrshire.gov.uk", user, ignore.case = TRUE)) {
                                              return("North Ayrshire")
                                            } else 
    # North Lanarkshire
                                              if (grepl("northlan.gov.uk", user, ignore.case = TRUE)) {
                                                return("North Lanarkshire")
                                              } else 
    # Orkney Islands
                                                if (grepl("orkney.gov.uk", user, ignore.case = TRUE)) {
                                                  return("Orkney Islands")
                                                } else 
    # Perth and Kinross
                                                  if (grepl("pkc.gov.uk", user, ignore.case = TRUE)) {
                                                    return("Perth and Kinross")
                                                  } else 
    # Renfrewshire
                                                   if (grepl("@renfrewshire.gov.uk", 
                                                              user, ignore.case = TRUE, 
                                                              fixed = TRUE, 
                                                              perl = TRUE)) {
                                                      return("Renfrewshire")
                                                    } else 
    # Scottish Borders
                                                      if (grepl("scotborders.gov.uk", user, ignore.case = TRUE)) {
                                                        return("Scottish Borders")
                                                      } else 
    # Shetland Islands
                                                        if (grepl("shetland.gov.uk", user, ignore.case = TRUE)) {
                                                          return("Shetland Islands")
                                                        } else 
    # South Ayrshire
                                                          if (grepl("south-ayrshire.gov.uk", user, ignore.case = TRUE)) {
                                                            return("South Ayrshire")
                                                          } else 
    # South Lanarkshire
                                                            if (grepl("southlanarkshire.gov.uk", user, ignore.case = TRUE)) {
                                                              return("South Lanarkshire")
                                                            } else 
    # Stirling
                                                              if (grepl("stirling.gov.uk", user, ignore.case = TRUE)) {
                                                                return("Stirling")
                                                              } else 
    # West Dunbartonshire
                                                                if (grepl("west-dunbarton.gov.uk", user, ignore.case = TRUE)) {
                                                                  return("West Dunbartonshire")
                                                                } else 
    # West Lothian
                                                                  if (grepl("westlothian.gov.uk", user, ignore.case = TRUE)) {
                                                                    return("West Lothian")
                                                                  } else {
    # IS or SG                                                                
                                                                    input$LA_selection
                                                                    } 
    })
  
# Creates a heading with the selected council name
  output$LA_KPO4_Heading <- renderUI({
    la_name <- local_authority()
    h2(paste("KPO4 Performance", la_name, sep = " - "), 
       style = "margin-top:3px"
       )
    })
  
# Shows who the logged in user is     
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      usr <- session$user
      usr <- gsub("(@).+\\..+", "", usr, perl = TRUE)
      sidebarUserPanel(paste("Logged in as: ", usr, sep = "\n"),
                       subtitle = a(icon("sign-out"), 
                                    "Logout", 
                                    href="__logout__"
                                    )
      )
      }
    })

# Download Data - format data for download --------------------------------

# This data is used in the data download page as this is to be presented in
# a wide un-pivoted format. It keeps the additional questions for applicable
# councils. 
  
  # Select columns based on council (ensures duplicate columns and 
  # questions from other councils are filtered out)
  dl_all_data <- reactive({
    council_fltr = local_authority()
    dl_all_data <- if (council_fltr == "Angus") {
      fresh_dta[ ,c(7:34,92)]
    } else
      if (council_fltr == "City of Edinburgh") {
        fresh_dta[ ,c(7:21,79:91)]
      } else
        if (council_fltr == "North Lanarkshire") {
          fresh_dta[ ,c(7:34,51:55)] 
        } else
          if (council_fltr == "Orkney Islands") {
            fresh_dta[ ,c(7:21,56:78)]
          } else
            if (council_fltr == "West Lothian") {
              fresh_dta[ ,c(7:21,35:50)]
            } else {
              fresh_dta[,c(7:34)]
              }
  
    # Add in columns with Financial Year info
    # Formats the ended date as a year and quarter value
    dl_all_data$`Tracking Link` <- as.yearqtr(dl_all_data$`Ended date`, 
                                              format = "%Y-%m-%d"
                                              ) 
    # This formatting uses calender year values rather than financial so need
    # to reduce by a quarter to format as financial years
    dl_all_data$`Financial Year` <- dl_all_data$`Tracking Link`- 1/4
    dl_all_data$`Financial Year` <- gsub("\\ ", 
                                         "-", 
                                         dl_all_data$`Financial Year`, 
                                         perl = TRUE
                                         )
    dl_all_data$`Financial Year` <- dl_all_data %>% 
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
    dl_all_data$`Tracking Link` <- dl_all_data$`Tracking Link`- 1/4
    dl_all_data$`Tracking Link` <- gsub("[0-9]*\\ Q", 
                                        "Quarter ", 
                                        dl_all_data$`Tracking Link`, 
                                        perl = TRUE
                                        )
    
    # Remove redundant columns and reorder
    dl_all_data <- dl_all_data[-c(1, 2, 4)]
    dl_all_data <- dl_all_data[, c((ncol(dl_all_data) - 1),
                                   ncol(dl_all_data),
                                   2,
                                   11,
                                   12,
                                   3:10,
                                   13:(ncol(dl_all_data) - 2),
                                   1
                                   )
                               ]
    
    # Pivot to combine both LA columns, rename, then remove duplicates
    dl_all_data <- dl_all_data %>% 
      pivot_longer(cols = 4:5, names_to = "extra", values_to = "LA") %>%
      filter(LA != "-") %>% 
      select(-extra)
    dl_all_data <- dl_all_data[, c(1:3,
                                   ncol(dl_all_data),
                                   4:(ncol(dl_all_data) - 1)
                                   )
                               ]  
  
    # Code local authority name for councils completing survey without login
    dl_all_data <- merge(dl_all_data, LA_names_dta)
    dl_all_data$`Local Authority Name` <- dl_all_data$LA_names
    dl_all_data <- dl_all_data %>% select(-LA_names)
    dl_all_data <- dl_all_data[,c(2:4, 1, 5:ncol(dl_all_data))]
    dl_all_data
  })
  

# Respondent type & Reasons data ---------------------------------------------- 
  
  # Generate another dataframe with respondent types
  resp_dta <- reactive({
    dl_all_data <- dl_all_data()
    # remove the end date column
    dl_all_data <- dl_all_data[-26]
    council_fltr = local_authority()
    
    # Recode "other" respondents and reasons so it doesn't show the text response
    dl_all_data$`Q1.4. Other (please specify):`[dl_all_data$`Q1.4. Other (please specify):` != "0"] <- "1"
    dl_all_data$`Q2.4. Other (please specify):`[dl_all_data$`Q2.4. Other (please specify):` != "0"] <- "1"
    # Change these columns to numeric so they can be combined with the other columns
    dl_all_data$`Q1.4. Other (please specify):` <- as.numeric(dl_all_data$`Q1.4. Other (please specify):`)
    dl_all_data$`Q2.4. Other (please specify):` <- as.numeric(dl_all_data$`Q2.4. Other (please specify):`)

    # Calculates within each LA, the number of respondents answering yes to 
    # the different respondent options. Then calculates as a % of all responses
    # within the financial year.
    resp_dta <- dl_all_data %>% 
     group_by(`Local Authority Name`) %>% 
     select(1:12) %>%
     pivot_longer(cols = 5:12, names_to = "Question", values_to = "value") %>% 
     group_by(`Financial Year`,`Local Authority Name`, Question) %>%
     count(value) %>%
     mutate(perc = round((n/sum(n)) * 100, 1))
    
    # Differentiates questions by whether they ask about respondent types or reasons
    resp_dta$question_type <- ifelse(grepl("Q1", resp_dta$Question), 
                                     "Type", 
                                     "Reason"
                                     )
    # Remove question numbers
    resp_dta$Question <- gsub("Q[\\.1-9]+\\s", 
                              "", 
                              resp_dta$Question, 
                              perl = TRUE
                              )
    # Remove "(please specify):" from "Other" question value
    resp_dta$Question[resp_dta$Question == "Other (please specify):"] <- "Other"
    # Filter to selected council & current financial year
    resp_dta <- resp_dta %>% 
      filter(`Financial Year` == fin_yr & `Local Authority Name` == council_fltr)
    resp_dta
  })
  
# Finalise unpivot data ---------------------------------------------------
# This is used in the data download table and the respondent no. value box
  unpivot_data <- reactive({
    council_fltr <- local_authority()
    # Select columns based on council (ensures duplicate columns and 
    # additional questions are filtered out)
    unpivot_data <- if (council_fltr == "City of Edinburgh") {
      dwnld_table_dta[, c(1:12, 70:82, 84)]
    } else
      if (council_fltr == "Orkney Islands") {
        dwnld_table_dta[, c(1:12, 47:50, 56:58, 63, 65:69, 84)] 
      } else
        if (council_fltr == "West Lothian") {
          dwnld_table_dta[, c(1:12, 26:30, 34:41, 84)]
        } else
          if(council_fltr == "Angus") {
            dwnld_table_dta[, c(1:25, 83, 84)]
          } else {
            dwnld_table_dta[, c(1:25, 84)]
            }
  
   # Tidy up question names
    unpivot_data <- unpivot_data %>% 
      rename("Quarter" = "Tracking Link") %>%
      rename("Q3. How satisfied were you with the time taken?" = "Q3. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?") %>%
      rename("Q4. How would you rate the standard of communication?" = "Q4. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?") %>%
      rename("Q.3. Responsiveness to any queries or issues raised" = "Q.3. Time taken to respond to any queries or issues raised") %>%
      rename("Q5. To what extent would you agree that you were treated fairly" = "Q5. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?") %>%
      rename("Q6. How satisfied were you, overall?" = "Q6. Overall, how satisfied were you with the service provided by [question(16082428)][variable(la)] Building Standards?")%>%
      rename("Q1.4. Other respondent" = "Q1.4. Other (please specify):") %>%
      rename("Q2.4. Other reason" = "Q2.4. Other (please specify):") %>%
      rename("Submission date" = "Ended date") %>%
      mutate(across(contains(c("Q1.1. Agent/Designer", 
                               "Q1.2. Applicant", 
                               "Q1.3. Contractor",
                               "Q1.4. Other respondent",
                               "Q2.1. To discuss your proposal",
                               "Q2.2. To make an application", 
                               "Q2.3. During construction",
                               "Q2.4. Other reason"
                               )
                             ),
                    ~recode(., "1" = "Yes", "0" = "No")
                    )
             ) %>%
      select(-LA)
  
    # Recode responses for download and to show in table
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
    unpivot_data <- unpivot_data %>% 
      filter(`Local Authority Name` == council_fltr)
    unpivot_data
  })
  
# Create KPO4 Score Data ---------------------------------------------------
  
  # Calculated the KPO4 score for each response based on weighted responses
  KPOdta <- pivot_dta %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(KPO_weight = value - 1)
  # Sets multipliers for the different questions as they are weighted differently
  # Overall satisfaction makes up 50%
  # Communications and time taken each make up 12.5%
  # Staff, information, responsiveness and fairness each make up 6.25%
  KPOweights_multiplier <- outer(pivot_dta$Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?", 2)+
    outer(pivot_dta$Indicator == "How would you rate the standard of communication provided?", 2) +
    outer(pivot_dta$Indicator == "Overall, how satisfied were you with the service provided?", 8)
  KPOweights_multiplier <- replace(KPOweights_multiplier, 
                                   KPOweights_multiplier == 0, 
                                   1
                                   )
  # This calculates the weighted score
  KPOdta$KPO_weight <- KPOdta$KPO_weight * KPOweights_multiplier  
  
  # Get total and max values for Scotland by quarter and financial year
   
  # This calculates the highest possible weighted score available for each
  # response by multiplying the highest score by the weighting for that question
  scot_max <- pivot_dta %>% 
    mutate(maxAvailable = 4) %>% 
    mutate(value = as.numeric(value))
  scot_max$maxAvailable <- (scot_max$maxAvailable - 1) * KPOweights_multiplier 
  scot_max[is.na(scot_max$value), "maxAvailable"] <- NA
  # This calculates the actual weighted score
  scot_max$KPO4_weighted <- (scot_max$value - 1) * KPOweights_multiplier
  
  # Calculate KPO4 for quarter for all results   
  scot_max_sum <- scot_max %>% 
    group_by(`Tracking Link`, `Financial Year`) %>%
    summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
    group_by(`Financial Year`) %>%
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")
                        )
              ) %>%
    # Generate the KPO score (out of 10)    
    mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1))
  
  # Calculate KPO4 for quarter and financial year for selected local authority    
  la_max_sum <- reactive({
    council_fltr <- local_authority()
    la_max_sum <- scot_max %>% 
      filter(`Local Authority Name` == council_fltr) %>%     
      group_by(`Tracking Link`, `Financial Year`) %>%
      summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
      group_by(`Financial Year`) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.character), ~"Total")
                          )
                ) %>%
      # Generate the KPO score (out of 10)    
      mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1))
    })
  
# Create KPO4 download ---------------------------------------------------
  
  # Create data frame with KPO4 scores for all LA's 
  # (this is available for SG and IS to download)
  total_la_max_sum <- scot_max %>%      
    group_by(`Local Authority Name`,`Tracking Link`, `Financial Year`) %>%
    summarise(across(c(maxAvailable, KPO4_weighted),sum, na.rm = TRUE)) %>%
    group_by(`Local Authority Name`, `Financial Year`) %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")
                        )
              ) %>%
    # Generate the KPO score (out of 10)    
    mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1)) %>%
    rbind(scot_max_sum) %>%
    select(-maxAvailable, -KPO4_weighted) %>%
    rename(Area = `Local Authority Name`) %>%
    rename(Quarter = `Tracking Link`) %>%
    rename(`KPO4 Score` = KPO_score)
  
  total_la_max_sum$Quarter <- recode(total_la_max_sum$Quarter, 
                                     "Total" = "Year to Date"
                                     )
  total_la_max_sum$Area[is.na(total_la_max_sum$Area)] <- "Scotland"
  total_la_max_sum <- total_la_max_sum %>% arrange(`Financial Year`, Quarter)
  
  # Create downloadable file
  output$KPO_data_file <- downloadHandler(filename = paste("KPO4_Data", 
                                                           ".csv", 
                                                           sep = ""
                                                           ),
                                          content = function(file) {
                                            write.csv(total_la_max_sum, file)
                                            }
                                          )
  
  # Create conditionality to only show download button if IS or SG
  output$KPO_data_dl <- renderUI({
    user <- user()
    if(grepl("improvementservice.org.uk|gov.scot", 
             user, 
             ignore.case = TRUE
             )
       ) {
      downloadBttn("KPO_data_file", 
                   label = "Download KPO4 Data", 
                   style = "jelly", 
                   size = "sm"
                   )
      } else {
        return()
        }
    })
  
# Performance Overview Tab (Performance boxes) ------------------------------
  
 # Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    la_max_sum <- la_max_sum()
    # Sets traffic light colours based on KPO4 score
    kpo_colr <- ifelse(
      la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, 
                 "KPO_score"
                 ] > 7.5, 
      "green", 
      ifelse(la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, 
                        "KPO_score"
                        ] < 6.5, 
             "red", 
             "orange"
             )
      )
    
    valueBox(value = round(la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, 
                                      "KPO_score"
                                      ],
                           1
                           ), 
             paste("Council KPO4 YTD", fin_yr),
             icon = icon("chart-bar"), 
             color = kpo_colr
             )
    })
  
  # Create performance box for Scotland
  output$scotPerfBox<- renderValueBox({
    valueBox(value = round(scot_max_sum[scot_max_sum$`Tracking Link` =="Total" & scot_max_sum$`Financial Year` == fin_yr, 
                                        "KPO_score"],
                           1
                           ), 
             paste("Scotland Average KPO4 YTD", fin_yr), 
             icon = icon("times"), 
             color = "navy"
               )
    })
  
  # Create valuebox for number of responses 
  output$respBox <- renderValueBox({
    unpivot_data <- unpivot_data()
    # Counts the number of rows (responses) in the given quarter & financial year
    # data is already filtered to selected council
    valueBox(value = paste(nrow(filter(unpivot_data, 
                                       Quarter == crnt_qtr & `Financial Year` == fin_yr
                                       )
                                ), 
                           paste("Responses", 
                                 gsub("Quarter\\ ","Q",crnt_qtr, perl = TRUE)
                                 )
                           ),
             # Counts the rows (responses) for the full year
             subtitle = paste(nrow(filter(unpivot_data, `Financial Year` == fin_yr)),
                              paste("Year to Date", fin_yr)
                              ), 
             icon = icon("user-friends"), 
             color = "light-blue"
             )
    })
    
# Performance Overview tab (KPO4 bar plot) -----------------------------------
  
  # Create bar plot for overall performance
  output$ovrPerfBar <- renderPlotly({
    la_max_sum <- la_max_sum()
    #rename Total as year to date
    la_max_sum$`Tracking Link` <- recode(la_max_sum$`Tracking Link`, 
                                         "Total" = "YTD"
                                         )
    # Filter to only include the Quarters for current year
    la_max_sum <- la_max_sum %>% 
      filter(`Tracking Link` == "YTD" | (`Tracking Link` != "YTD" & `Financial Year` == fin_yr))
    # Add Financial year to quarter labels
    la_max_sum$`Tracking Link` <- gsub("Quarter\\ ",
                                       "Q",
                                       la_max_sum$`Tracking Link`, 
                                       perl = TRUE
                                       )
    la_max_sum$Label <- paste(la_max_sum$`Tracking Link`, 
                              la_max_sum$`Financial Year`, 
                              sep = " "
                              )
    # Store the number of YTD values to determine the colours for these bars
    YTD <- length(la_max_sum$`Tracking Link`[la_max_sum$`Tracking Link` == "YTD"])
    
    # Set colours for quarter by kpo4
    kpo_clrs <- la_max_sum %>% 
      filter(`Tracking Link` != "YTD") %>% 
      pull(KPO_score)
    clrs <- ifelse(kpo_clrs > 7.5, 
                   "forestgreen", 
                   ifelse(kpo_clrs < 6.5, 
                          "firebrick", 
                          "darkorange"
                          )
                   )
    
    p <- ggplot(data = la_max_sum) +
      geom_bar(aes(x = Label, 
                   y = KPO_score,
                   text = paste(paste("Quarter:", Label),
                                paste("KPO 4 Score", KPO_score),
                                sep = "\n"
                                )
                   ), 
               stat = "identity",
               position = "dodge", 
               fill = c(clrs, rep("grey13", YTD)), 
               width = 0.7, 
               colour = "black"
               ) +
      theme_classic() +
      scale_y_continuous(limits = c(0, 10), 
                         expand = expansion(mult = c(0, 0.1))
                         ) +
      ggtitle("KPO4 performance by quarter and YTD") +
      ylab("KPO 4 Score") +
      xlab("Response period") +
      theme(axis.text.x = element_text(size = 10),
            axis.title = element_text(size = 13)
            )
    
    ggplotly(p, tooltip = "text")
    })
    
# Performance Overview tab (respondent type & reason plots)------------------

  # Extract data for response type
  report_type_data <- reactive({
    resp_dta <- resp_dta()
    pc_resp_data <- resp_dta %>% 
      filter(., question_type == "Type" & value == 1)
    pc_resp_data
    })
  
  # This output is used twice, in the UI, but this is not allowed
  # therefore the output is assigned twice so that it can be called twice
  # using the different names. 
  # resp_type_graph_report is used in the report download tab
  # resp_type_graph_overview is used in the performance overview tab
  
  output$resp_type_graph_report <- output$resp_type_graph_overview <- renderPlotly({
    report_type_data <- report_type_data()
    plot <- ggplot(data = report_type_data) +
      geom_col(aes(x = Question, 
                   y = perc,
                   text = paste(paste("Respondent Type:", 
                                      report_type_data$Question
                                      ),
                                paste("% of responses:", 
                                      report_type_data$perc
                                      ),
                                sep = "\n"
                                )
                   ),
               fill = "cadetblue3", 
               colour = "black"
              ) +
      coord_flip() +
      theme_classic() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle("Respondent Type: YTD") +
      xlab("Respondent Type") +
      ylab("Percentage of Responses") +
      theme(plot.title = element_text(size = 12))
    
    ggplotly(plot, tooltip = "text")
    })
  
  # Extract data for response reason and shorten question labels
  report_reason_data <- reactive({
    resp_dta <- resp_dta()
    pc_resp_data <- resp_dta %>% 
      filter(., question_type == "Reason" & value == 1)
    pc_resp_data[pc_resp_data$Question == "During construction, including submission of a completion certificate", "Question"] <- "During construction" 
    pc_resp_data[pc_resp_data$Question == "To discuss your proposal before applying for a building warrant", "Question"] <- "Discuss proposal" 
    pc_resp_data[pc_resp_data$Question == "To make an application for a building warrant", "Question"] <- "Make application" 
    pc_resp_data
    })
  
  # This output is used twice, in the UI, but this is not allowed
  # therefore the output is assigned twice so that it can be called twice
  # using the different names. 
  # resp_reason_graph_report is used in the report download tab
  # resp_reason_graph_overview is used in the performance overview tab
  
  output$resp_reason_graph_report <- output$resp_reason_graph_overview <- renderPlotly({
    report_reason_data <- report_reason_data()
    plot <- ggplot(data = report_reason_data()) +
      geom_col(aes(x = Question, 
                   y = perc,
                   text = paste(paste("Reason:", 
                                      report_reason_data$Question
                                      ),
                                paste("% of responses:", 
                                      report_reason_data$perc
                                      ),
                                sep = "\n"
                                )
                   ),
               fill = "cadetblue3", 
               colour = "black"
              ) +
      coord_flip() +
      theme_classic() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle("Response Reason:YTD") +
      xlab("Reason") +
      ylab("Percentage of Responses") +
      theme(plot.title = element_text(size = 12))
    
    ggplotly(plot, tooltip = "text")
    })
    
# Questions results tab (Data filtering)--------------------------------
  
  # Create select button for financial year if there is more than 1 year available
  output$fin_yr <- renderUI({
    council_fltr <- local_authority()
    pivot_dta <- pivot_dta %>% filter(`Local Authority Name` == council_fltr) 
    years <- unique(pivot_dta$`Financial Year`)
    no_years <- length(unique(pivot_dta$`Financial Year`))
    if (no_years > 1) {
      selectizeInput(inputId = "fin_yr_selection", 
                     label = "Select financial year",
                     choices = years, 
                     selected = fin_yr
                     )
      } else {
        return()
        }
    })
  
  # Create filtered dataset from checkboxes
  qstn_dataset_filtered <- reactive({
    council_fltr <- local_authority()
    names(pivot_dta) <- gsub("Q[1-9\\.]+\\s", 
                             "", 
                             names(pivot_dta), 
                             perl = TRUE
                             )
    pivot_dta$`Tracking Link` <- as.factor(pivot_dta$`Tracking Link`)
    pivot_dta <- pivot_dta %>% filter(`Local Authority Name` == council_fltr)
    # Selected respondent type  
    slctn_respondent <- input$Qs_resp_input
    # Selected applicant reason using partial match
    slctn_reason <- names(select(pivot_dta, contains(input$Qs_reason_input)))
    # Filter to match responses with the selected respondent type & reason
    filter_data <- pivot_dta %>% 
      filter(if_any(slctn_respondent, ~. == 1)) %>%
      filter(if_any(slctn_reason, ~. ==1)) 
    # Filter to correct financial year - either current year or selected year
    no_years <- length(unique(pivot_dta$`Financial Year`))
    filter_data <- if (no_years > 1) {
      filter(filter_data, `Financial Year` == input$fin_yr_selection)
      } else {
        filter(filter_data, `Financial Year` == fin_yr)
        }
    filter_data
    })
    
# Questions Results tab (YTD plot)---------------------------------------
  
  # First graph for full breakdown of responses in year to date  
  
  output$YTDqstsPlot <- renderPlotly({
    filt_data <- qstn_dataset_filtered()
    # Filter data based on question selected & set responses as factors
   if (input$Qstn_tab2 == "All Questions") {
     qstnDta <- filt_data %>% filter(value != "-")
     qstnDta$value <- factor(qstnDta$value, levels = c(1, 2, 3, 4))
     } else {
       qstnDta <- filt_data %>% 
         filter(Indicator == input$Qstn_tab2) %>% 
         filter(value != "-")
       qstnDta$value <- factor(qstnDta$value, levels = c(1, 2, 3, 4))
     }
    # Calculate count for each response
    qstnDta <- qstnDta %>% count(value, .drop = FALSE)
    
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    if (input$Qstn_tab2 == "All Questions") {
      qstnDta$named_value <- recode(qstnDta$value, 
                                    "1" = "Very Good/Very Satisfied/Strongly Agree",
                                    "2" = "Good/Satisfied/Agree",
                                    "3" = "Poor/Dissatisfied/Disagree",
                                    "4" = "Very Poor/Very Dissatisfied/Strongly Disagree"
                                    )
      } else 
        if (input$Qstn_tab2 == "Overall, how satisfied were you with the service provided?"|input$Qstn_tab2 =="Thinking of your engagement, how satisfied were you with the time taken to complete the process?") {
          qstnDta$named_value <- recode(qstnDta$value, 
                                        "1" = "Very Satisfied",
                                        "2" =" Satisfied",
                                        "3" = "Dissatisfied",
                                        "4" = "Very Dissatisfied"
                                        )
          } else 
            if (input$Qstn_tab2 == "To what extent would you agree that you were treated fairly?") {
              qstnDta$named_value <- recode(qstnDta$value, 
                                            "1" = "Strongly Agree",
                                            "2" = "Agree",
                                            "3" = "Disagree",
                                            "4" = "Strongly Disagree"
                                            )
              } else {
                qstnDta$named_value <- recode(qstnDta$value, 
                                              "1" = "Very Good",
                                              "2" = "Good",
                                              "3" = "Poor",
                                              "4" = "Very Poor"
                                              )
                }
    
    # Generate barplot
    plot <- ggplot(data = qstnDta) +
      geom_bar(aes(x = reorder(named_value, as.numeric(value)), 
                   y = n, 
                   text = paste(paste0("Response: ", named_value), 
                                paste0("Number of Responses:", n),
                                sep = "\n"
                                )
                   ),
               stat= "identity",
               fill = "cadetblue3", 
               width = 0.7, 
               colour = "black"
               ) +
      ggtitle(input$Qstn_tab2) +
      xlab("Response") +
      ylab("Number of responses") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_classic()
    
    ggplotly(plot, tooltip = "text")
   })

# Questions Results tab (Summary by quarters plot)-------------------------

  # Second summary of percentage responses by quarter
  
  output$qrtsQsplot <- renderPlotly({
  # Filter dataset based on selected question & set responses as factors
    qstn_dataset_filtered <- qstn_dataset_filtered()
    qstn_dataset_filtered <- qstn_dataset_filtered %>% filter(value != "-")
    if(input$Qstn_tab2 == "All Questions") {  
      qstnDta <- qstn_dataset_filtered
      } else {
        qstnDta <- filter(qstn_dataset_filtered, Indicator == input$Qstn_tab2)
        }
    
    # Calculate count for each response
    qstnDta <- qstnDta %>% count(`Tracking Link`, value, .drop = FALSE)
    # Summarise the count per quarter
    qstnDta <- qstnDta %>% 
      group_by(`Tracking Link`) %>%
      mutate(total_responses = sum(n)) %>%
      ungroup() %>%
      mutate(`% of responses` = round((n/total_responses * 100), 1))
    
    # Remove NA for where there is no values for a quarter
    qstnDta <- qstnDta %>% filter(!is.na(value))
    
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    # Also need to set as factors to keep the responses in order within the bars
    qstnDta$Response <- qstnDta$value
    if (input$Qstn_tab2 == "All Questions") {
      qstnDta$Response <- recode(qstnDta$Response, 
                                 "1" = "Very Good/Very Satisfied/Strongly Agree",
                                 "2" = "Good/Satisfied/Agree",
                                 "3" = "Poor/Dissatisfied/Disagree",
                                 "4" = "Very Poor/Very Dissatisfied/Strongly Disagree"
                                 )
      qstnDta$Response <- factor(qstnDta$Response, 
                                 levels  = c("Very Good/Very Satisfied/Strongly Agree",
                                             "Good/Satisfied/Agree", 
                                             "Poor/Dissatisfied/Disagree",
                                             "Very Poor/Very Dissatisfied/Strongly Disagree"
                                             )
                                 )
      } else 
        if(input$Qstn_tab2 == "Overall, how satisfied were you with the service provided?"| input$Qstn_tab2 == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?") {
          qstnDta$Response <- recode(qstnDta$Response, 
                                     "1" = "Very Satisfied",
                                     "2" = "Satisfied",
                                     "3" = "Dissatisfied",
                                     "4" = "Very Dissatisfied"
                                     )
          qstnDta$Response <- factor(qstnDta$Response, 
                                     levels  = c("Very Satisfied",
                                                 "Satisfied",
                                                 "Dissatisfied",
                                                 "Very Dissatisfied"
                                                 )
                                     )
          } else 
            if (input$Qstn_tab2 == "To what extent would you agree that you were treated fairly?") {
              qstnDta$Response <- recode(qstnDta$Response, 
                                         "1" = "Strongly Agree",
                                         "2" = "Agree",
                                         "3" = "Disagree",
                                         "4" = "Strongly Disagree"
                                         )
              qstnDta$Response <- factor(qstnDta$Response, 
                                         levels  = c("Strongly Agree",
                                                     "Agree",
                                                     "Disagree",
                                                     "Strongly Disagree"
                                                     )
                                         )
              } else {
                qstnDta$Response <- recode(qstnDta$Response, 
                                           "1" = "Very Good",
                                           "2" = "Good",
                                           "3" = "Poor",
                                           "4" = "Very Poor"
                                           )
                qstnDta$Response <- factor(qstnDta$Response, 
                                           levels = c("Very Good", 
                                                      "Good", 
                                                      "Poor", 
                                                      "Very Poor"
                                                      )
                                           )
                }
    
    Labels <- levels(qstnDta$Response)
    # Rename in dataset to set what shows on hover labels
    qstnDta <- qstnDta %>% rename(Quarter = `Tracking Link`)
    
    # Generate bar plot
    plot <- ggplot(data = qstnDta ) +
      geom_bar(aes(x = Quarter, 
                   y = `% of responses`, 
                   fill = Response
                   ), 
               stat = "identity", 
               position = "stack",
               width = 0.7, 
               colour = "black"
              ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle(input$Qstn_tab2) +
      xlab("") +
      theme_classic() +
      scale_fill_manual(breaks = Labels, 
                        values = c("forestgreen", 
                                   "lightgreen", 
                                   "darkorange", 
                                   "firebrick"
                                   ),
                        name = "Responses"
                        )
    ggplotly(plot)
    })
   
# Report Download Tab (KPO4 YTD)------------------------------------------
  
  # Create data for KPO4 in report page - combine LA & Scot level data 
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  report_kpo_data <- reactive({
    la_max_sum <- la_max_sum()
    council_fltr <- local_authority()
    la_max_sum$id <- council_fltr
    scot_max_sum$id <- "Scotland" 
    all_kpo_dta <- rbind(scot_max_sum, la_max_sum)
    all_kpo_dta
    })
  
  # Generate plot for KPO4 in report page 
  output$reportKPO4Plot <- renderPlotly({
    all_kpo_data <- report_kpo_data()
    council_fltr <- local_authority()
    all_kpo_data <- all_kpo_data %>% filter(`Tracking Link` == "Total")
    # Set the council values as a factor so the data can be arranged to 
    # have the council first regardless of alphabetical order
    all_kpo_data$id <- factor(all_kpo_data$id, 
                              levels = c(council_fltr, "Scotland")
                              )
    all_kpo_data <- arrange(all_kpo_data, id)
    # Store number of years to determine the number of reps for colours of bars
    Years <- length(unique(all_kpo_data$`Financial Year`))
    
    plot <- ggplot(data = all_kpo_data) +
      geom_bar(aes(x = `Financial Year`, 
                   y = KPO_score, 
                   fill = id,
                   text = paste(`Financial Year`, 
                                id, 
                                paste("KPO 4 Score:", KPO_score), sep = "\n"
                                )
                   ), 
               stat = "identity",
               position = "dodge",
               width = 0.7, 
               colour = "black"
              ) +
      scale_y_continuous(limits = c(0, 10), 
                         expand = expansion(mult = c(0, 0.1))
                         ) +
      scale_fill_manual(values = rep(c("cadetblue3", "dimgrey"), Years), 
                        name = ""
                        ) + 
      ggtitle("KPO 4 score - Year to Date") +
      xlab("") +
      ylab("KPO 4 Score") +
      theme_classic()
    
    ggplotly(plot, tooltip = "text")
    })
  
  # Render text for KPO4 Overall performance  to year
  output$KPO4_text_report <- renderText({
    council_fltr <- local_authority()
    all_kpo_data <- report_kpo_data()
    # Store the number of financial years available for council
    Years <- all_kpo_data %>% filter(id == council_fltr)
    Years <- length(unique(Years$`Financial Year`))
    
    # Compare council KPO4 values with Scotland and target to create 
    # reactive text values
    
    all_kpo_data <- all_kpo_data %>% 
      filter(`Tracking Link` == "Total" & `Financial Year` == fin_yr)
    # Council KPO4
    KPO4_ytd <- all_kpo_data %>% 
      filter(id == council_fltr) %>% 
      pull(KPO_score)
    # Scotland KPO4
    scotAv_kpo4 <- all_kpo_data %>% 
      filter(id == "Scotland") %>% 
      pull(KPO_score)
    # Compares council KPO4 with target KPO4
    hilow_kpo4 <- ifelse(KPO4_ytd > 7.5, 
                         "higher than", 
                         ifelse(KPO4_ytd < 7.5, 
                                "lower than", 
                                "equal to"
                                )
                         )
    # Compares council KPO4 with Scotland KPO4
    abbel_kpo4 <- ifelse(KPO4_ytd > scotAv_kpo4, 
                         "higher than",
                         ifelse(KPO4_ytd < scotAv_kpo4, 
                                "lower than", 
                                "equal to"
                                )
                         )
    
    # Store value for previous financial year
    KPO4_prev <- report_kpo_data() %>% 
      filter(`Tracking Link` == "Total" & `Financial Year` == prev_fin_yr)
    KPO4_prev <- KPO4_prev %>% 
      filter(id == council_fltr) %>% 
      pull(KPO_score)
    
    # Create current KPO4 with previous year
    change_value <- KPO4_ytd - KPO4_prev
    change_text <- if_else(change_value < 0, "a decrease", "an increase")
    
    # Text for when there is only 1 financial year
    text_kpo <- paste0("This indicator summarises performance across all questions, with differential weightings based on importance. For ", 
                       council_fltr,
                       " in ",
                       fin_yr, 
                       " overall performance is at ", 
                       KPO4_ytd, 
                       " for the year to date. ", 
                       "This is ",
                       abbel_kpo4,
                       " the Scotland average of ", 
                       scotAv_kpo4,
                       " and ", 
                       hilow_kpo4,
                       " the target value of 7.5."
                       )
    
    # Text for when there are more than 1 financial years     
    text_multiple_kpo <- paste0("This indicator summarises performance across all questions, with differential weightings based on importance. For ", 
                                council_fltr,
                                " in ",
                                fin_yr, 
                                " overall performance is at ", 
                                KPO4_ytd, 
                                " for the year to date. This reflects ", 
                                change_text,
                                " of ", 
                                change_value, 
                                " points from the performance of ", 
                                KPO4_prev, 
                                " in ", 
                                prev_fin_yr, 
                                ". The year to date performance of ", 
                                council_fltr, 
                                " in ", 
                                fin_yr, 
                                " is ", 
                                abbel_kpo4,
                                " the Scotland average of ", 
                                scotAv_kpo4,
                                " and ", 
                                hilow_kpo4,
                                " the target value of 7.5."
                                )
    
    # Conditional statement to select text based on the number of financial years available    
    final_text <- ifelse(Years > 1, text_multiple_kpo, text_kpo)
    return(final_text)
    })
   
# Report download tab (respondent reason & type)----------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Text for respondent types 
  output$respondent_type_text_report <- renderText({
    resp_dta <- resp_dta()
    unpivot_data <- unpivot_data()
    council_fltr <- local_authority()
    
    # Filter data to respondent type
    resp_dta_filter <- resp_dta %>% filter(question_type == "Type")
    # Get total responses for referencing the percentage denominator
    resp_number <- resp_dta_filter %>%
      group_by(Question) %>%
      summarise_at(vars(`n`),sum)
    resp_number <- resp_number[1,2]
    # Create variables for percentages for different groups
    agent_perc <- resp_dta_filter[resp_dta_filter$Question == "Agent/Designer" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    appli_perc <- resp_dta_filter[resp_dta_filter$Question == "Applicant" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    contr_perc <- resp_dta_filter[resp_dta_filter$Question == "Contractor" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    other_perc <- resp_dta_filter[resp_dta_filter$Question == "Other" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    # If any are 0 then replace with "none"
    agent_perc <- ifelse(isEmpty(agent_perc), "none", paste0(agent_perc, "%"))
    appli_perc <- ifelse(isEmpty(appli_perc), "none", paste0(appli_perc, "%"))
    contr_perc <- ifelse(isEmpty(contr_perc), "none", paste0(contr_perc, "%"))
    other_perc <- ifelse(isEmpty(other_perc), "No respondents", paste0(other_perc, "%"))
    # Paste all text together
    txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, as well as their reason for contacting the Building Standards Service in ", 
                              council_fltr,
                              ". Of the ", 
                              resp_number, 
                              " respondents ", 
                              agent_perc, 
                              " were agents or designers, ", 
                              appli_perc, 
                              " were applicants and ", 
                              contr_perc, 
                              " were contractors. ", 
                              other_perc, 
                              " said they were an other respondent type."
                              )
    txt_respondents
    })
  
  # Text for respondent reason
  output$respondent_reason_text_report <- renderText({
    resp_dta <- resp_dta()
    unpivot_data <- unpivot_data()
    council_fltr <- local_authority()
    
    # Filter data to respondent reason
    resp_dta_filter <- resp_dta %>% filter(question_type == "Reason") 
    # Get total responses for referencing the percentage denominator
    resp_number <- resp_dta_filter %>%
      group_by(Question) %>%
      summarise_at(vars(`n`),sum)
    resp_number <- resp_number[1,2]
    # Calculate percentages for each response type
    discuss_perc <- resp_dta_filter[resp_dta_filter$Question == "To discuss your proposal before applying for a building warrant" & resp_dta_filter$value == 1 ,"perc"] %>% 
      pull(perc)
    appli_perc <- resp_dta_filter[resp_dta_filter$Question == "To make an application for a building warrant" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    constr_perc <- resp_dta_filter[resp_dta_filter$Question == "During construction, including submission of a completion certificate" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    other_perc <- resp_dta_filter[resp_dta_filter$Question == "Other" & resp_dta_filter$value == 1, "perc"] %>% 
      pull(perc)
    # If any are 0 then replace with "none"
    discuss_perc <- ifelse(isEmpty(discuss_perc), "none", paste0(discuss_perc, "%"))
    appli_perc <- ifelse(isEmpty(appli_perc), "none", paste0(appli_perc,"%"))
    constr_perc <- ifelse(isEmpty(constr_perc), "none", paste0(constr_perc,"%"))
    other_perc <- ifelse(isEmpty(other_perc), "No respondents", paste0(other_perc, "%"))
    # Paste all text together
    txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, as well as their reason for contacting the Building Standards Service in ", 
                              council_fltr,
                              ". Of the ", 
                              resp_number, 
                              " respondents ", 
                              discuss_perc, 
                              " contacted the local authority to discuss their proposal before applying for a building warrant, ",
                              appli_perc, 
                              " were making an application for a warrant and ", 
                              constr_perc, 
                              " contacted the service during construction. ",
                              other_perc, 
                              " contacted the service for some other reason."
                              )
    txt_respondents
    })
   
# Report download tab (KPO4 over time)-------------------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Create data for performance over time graph
  report_line_data <- reactive({
    la_max_sum <- la_max_sum()
    council_fltr <- local_authority()
    
    # Add area reference to KPO4 data
    scot_max_sum$LA <- "Scotland"
    la_max_sum$LA <- council_fltr
   
    # Combine Scotland & LA level KPO4 data & filter to exclude YTD values
    quarts_dta <- rbind(scot_max_sum, la_max_sum) %>% 
      filter(`Tracking Link` != "Total")
    quarts_dta
    })
  
  # Graph output for performance over time 
  output$ovrPerfLine <- renderPlotly({
    report_line_data <- report_line_data()
    council_fltr <- local_authority()
     
    # checks if there is more than one data point available for the council
    if (length(report_line_data$LA[report_line_data$LA == council_fltr]) > 1) {
      
      # TRUE = line Graph
      
      # Set the council values as a factor so the data can be arranged to have the 
      # council first regardless of alphabetical order
      report_line_data$LA <- factor(report_line_data$LA, 
                                    levels = c(council_fltr, "Scotland")
                                    )
      # Add Financial year to quarter labels
      report_line_data$`Tracking Link` <- gsub("Quarter\\ ",
                                               "Q",
                                               report_line_data$`Tracking Link`, 
                                               perl = TRUE
                                               )
      report_line_data$Label <- paste(report_line_data$`Tracking Link`, 
                                      report_line_data$`Financial Year`, 
                                      sep = " "
                                      )
      report_line_data$`Tracking Link` <- gsub("Q", 
                                               "", 
                                               report_line_data$`Tracking Link`, 
                                               perl = TRUE
                                               )
      # Arrange the data to set the order of colours
      report_line_data <- arrange(report_line_data,
                                  `Financial Year`, 
                                  `Tracking Link`, 
                                  LA
                                  )
      # Set the date labels as a factor to ensure they stay in order
      QLabels <- unique(report_line_data$Label)
      report_line_data$Label <- factor(report_line_data$Label, 
                                       levels = QLabels
                                       )
      
      plot <- ggplot(data = report_line_data) +
        geom_line(aes(x = Label, 
                      y = KPO_score, 
                      group = LA, 
                      colour = LA,
                      text = paste(LA,
                                   paste("Quarter:", Label),
                                   paste("KPO 4 Score:", KPO_score),
                                   sep = "\n"
                                   )
                      ),
                  lwd = 1
                  ) +
        scale_color_manual(values = c("cadetblue3", "dimgrey"), name = "") +
        ggtitle("KPO 4 score - over time") +
        ylim(0, 10) +
        xlab("") +
        ylab("KPO 4 Score") +
        theme_classic()
      ggplotly(plot, tooltip = "text")
      } else {
        
        # FALSE = bar graph
        
        # Pulls out quarter and financial year available for selected council
        # There may be more than one quarter for Scotland so need to filter so that's not included
        qrtr_available <- report_line_data$`Tracking Link`[report_line_data$LA == council_fltr]
        year_available <- report_line_data$`Financial Year`[report_line_data$LA == council_fltr]
        
        report_line_data <- report_line_data %>% 
          filter(`Tracking Link` == qrtr_available,
                 `Financial Year` == year_available
                 )
        # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
        report_line_data$LA <- factor(report_line_data$LA, 
                                      levels = c(council_fltr, "Scotland")
                                      )
        # Arrange the data and store order of colours
        report_line_data <- arrange(report_line_data, LA)
        
        plot <- ggplot(data = report_line_data) +
          geom_bar(aes(x = `Tracking Link`, 
                       y = KPO_score, 
                       fill = LA,
                       text = paste(LA,
                                    paste("Quarter:", `Tracking Link`),
                                    paste("KPO 4 Score:", KPO_score),
                                    sep = "\n"
                                    )
                       ), 
                   stat = "identity",
                   position = "dodge",
                   width = 0.7, 
                   colour = "black"
                   ) +
          scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
          scale_fill_manual(values = c("cadetblue3", "dimgrey"), name = "") + 
          ggtitle("KPO 4 score - over time") +
          xlab("") +
          ylab("KPO 4 Score") +
          theme_classic() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        ggplotly(plot, tooltip = "text")
        }
    })
  
  # Render text for quarter by quarter performance   
  output$quarter_text <- renderText({
    council_fltr <- local_authority()
    all_kpo_data <- report_kpo_data()
    # Filter to quarters, selected council and current financial year
    all_kpo_data <- all_kpo_data %>% 
      filter(`Tracking Link` != "Total",
             `Financial Year` == fin_yr,  
             id == council_fltr
             ) %>% 
      ungroup()
     
     # Set the quarter labels as a factor to ensure they stay in order
     QLabels <- unique(all_kpo_data$`Tracking Link`)
     all_kpo_data$`Tracking Link` <- factor(all_kpo_data$`Tracking Link`, 
                                            levels = QLabels
                                            )
     # Store the names of the quarters by position (can't just reference by
     # quarter number as some councils may have data missing) 
     # if there is a quarter missing the position will be empty
     first_Q <- all_kpo_data$`Tracking Link`[[1]]
     second_Q <- if (length(QLabels) > 1) {
       all_kpo_data$`Tracking Link`[[2]]
     } else {
        0
       }
     third_Q <- if (length(QLabels) > 2) {
       all_kpo_data$`Tracking Link`[[3]]
     } else {
         0
       }
     fourth_Q <- if (length(QLabels) > 3) {
       all_kpo_data$`Tracking Link`[[4]]
     } else {
         0
       }
     
     # Filter to get KPO for first quarter available
     Q1_kpo <- all_kpo_data %>% 
       filter(`Tracking Link` == first_Q) %>%
       select(KPO_score)
     # Render text for first quarter available
     Q1_text <- paste0("In ", 
                       first_Q, 
                       " performance for KPO 4 calculated across all responses for all questions was ",
                       Q1_kpo,
                       " for ", 
                       council_fltr,
                       ". "
                       )
     
     # Filter to get KPO for second quarter available
     Q2_kpo <- all_kpo_data %>% 
       filter(`Tracking Link` == second_Q) %>%
       select(KPO_score)
     # Compare second quarter and first quarter - ignore if error occurs
     comp_Q12 <- tryCatch({
       ifelse(Q2_kpo > Q1_kpo + 0.2, 
              "rose", 
              ifelse(Q2_kpo < Q1_kpo - 0.2, 
                     "fell", 
                     "stayed the same"
                     )
              )
       },
       error = function(error_message) {""}
       )
     # Render text for when there are 2 quarters           
     Q2_text <- paste0(Q1_text, 
                       "Performance then ", 
                       comp_Q12,
                       " in ", 
                       second_Q, 
                       " to stand at ", 
                       Q2_kpo, 
                       "."
                       )
     
     # Filter to get KPO for third quarter available
     Q3_kpo <- all_kpo_data %>% 
       filter(`Tracking Link` == third_Q) %>%
       select(KPO_score)
     # Compare third & second quarter available - ignore if error occurs
     comp_Q23 <- tryCatch({
       ifelse(Q3_kpo > Q2_kpo + 0.2, 
              "higher than", 
              ifelse(Q3_kpo < Q2_kpo - 0.2, 
                     "lower than", 
                     "the same as"
                     )
              )
       }, error = function(error_message) {""} 
       )
     # Render text for when there are 3 quarters             
     Q3_text <- paste0(Q2_text,
                       " In ", 
                       third_Q, 
                       " performance was ", 
                       comp_Q23,
                       " ", 
                       second_Q,
                       " at ", 
                       Q3_kpo, 
                       "."
                       )
     
     # Filter to get KPO for fourth quarter available
     Q4_kpo <- all_kpo_data %>% 
       filter(`Tracking Link` == fourth_Q) %>%
       select(KPO_score)
     # Compare fourth and third quarter available - ignore if error occurs
     comp_Q34 <- tryCatch({
       ifelse(Q4_kpo > Q3_kpo + 0.2, 
              "higher than", 
              ifelse(Q4_kpo < Q3_kpo - 0.2, 
                     "lower than", 
                     "the same as"
                     )
              )
       }, error = function(error_message) {""} 
       )
     # Render text for when there are four quarters                   
     Q4_text<- paste0(Q3_text, 
                      ". In ", 
                      fourth_Q,
                      " performance was ", 
                      comp_Q34,
                      " ",
                      third_Q,
                      " and stands at ", 
                      Q4_kpo, 
                      "."
                      )
     
     # Select what text to include based on the no. of quarters available
     main_text <- ifelse(length(QLabels) == 1, 
                         Q1_text, 
                         ifelse(length(QLabels) == 2, 
                                Q2_text, 
                                ifelse(length(QLabels) == 3, 
                                       Q3_text, 
                                       Q4_text
                                       )
                                )
                         )
     
     # If there is data for more than 1 financial year compare most 
     # recent quarter with same quarter in previous year
     extra_data <- report_kpo_data()
     # filter to current quarter and selected council
     extra_data <- extra_data %>% 
       filter(`Tracking Link` == crnt_qtr, 
              id == council_fltr
              )
     # Store values for current financial year and previous financial year
     first_fin_yr <- if (length(extra_data$`Tracking Link`) > 1) {
       filter(extra_data, `Financial Year` == prev_fin_yr) %>%
         ungroup() %>%
         select(KPO_score)
       } else {
         0
        }
     
     second_fin_yr <- if (length(extra_data$`Tracking Link`) > 1) {
       filter(extra_data, `Financial Year` == fin_yr) %>%
         ungroup() %>%
         select(KPO_score)
     } else {
         0
       }
     
     # Compare values and create text
     extra_comp <- tryCatch({
       ifelse(second_fin_yr > first_fin_yr + 0.2, 
              " higher than in ", 
              ifelse(second_fin_yr < first_fin_yr - 0.2, 
                     " lower than in ", 
                     " the same as in "
                     )
              )
       }, error = function(error_message) {""}
       )
     extra_comp_value <- ifelse(extra_comp == " the same as in ", 
                                "", 
                                second_fin_yr - first_fin_yr
                                )
     
     extra_text <- paste0("KPO 4 performance in ", 
                          crnt_qtr, 
                          " ", 
                          fin_yr, 
                          " was ", 
                          extra_comp_value, 
                          extra_comp,
                          crnt_qtr, 
                          " ",
                          prev_fin_yr,
                          "."
                          )
     
     # Select which text is shown based on the no. of years available
     final_text <- ifelse(length(unique(extra_data$`Financial Year`)) > 1,
                          paste(main_text, extra_text),
                          main_text
                          )
     final_text
     })
   
# Report Download tab (functions for individual questions)-------------------
  
  # Create a function for generating formatted data for individual questions
  
  format_qstn_dta <- function(question, 
                              named_value_1, 
                              named_value_2,
                              named_value_3, 
                              named_value_4
                              ) {
    council_fltr <- local_authority()
    # Filter pivot_dta to current financial year and remove missing values
    pivot_dta <- pivot_dta %>% filter(`Financial Year` == fin_yr) %>%
      filter(value != "-")
    # Set values as factor to keep in order
    pivot_dta$`value` <- factor(pivot_dta$`value`, levels = c(1, 2, 3, 4))
    # Calculate count for question and selected LA
    qstnDta_LA <- pivot_dta %>% 
      filter(Indicator == question) %>%
      filter(`Local Authority Name` == council_fltr) %>% 
      count(value, .drop = FALSE) %>%
      mutate(Selection = council_fltr)
    # Calculate count for question for all responses (Scotland) - bind LA count
    qstnDta <- pivot_dta %>% 
      filter(Indicator == question) %>%
      count(value, .drop = FALSE) %>%
      mutate(Selection = "Scotland") %>%
      rbind(qstnDta_LA )
    # Get response percentages for LA and Scotland 
    qstnDta <- qstnDta %>% 
      group_by(Selection) %>% 
      mutate(perc_resp = round((n / sum(n)) * 100, 1))
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    qstnDta$named_value <- recode(qstnDta$value, 
                                  "1" = named_value_1,
                                  "2" = named_value_2,
                                  "3" = named_value_3,
                                  "4" = named_value_4
                                  )
    # Set the council values as a factor so the data can be arranged to 
    # have the council first regardless of alphabetical order
    qstnDta$Selection <- factor(qstnDta$Selection, 
                                levels = c(council_fltr, "Scotland")
                                )
    # Arrange the data so the colours will be in order
    qstnDta <- arrange(qstnDta, value, Selection) 
    qstnDta
  } 
  
  # Create a function for generating plot for individual questions
  
  create_qstn_plot <- function(data, title) {
    plot <- ggplot(data = data) +
      geom_bar(aes(x = named_value,
                   y = perc_resp,
                   fill = Selection,
                   text = paste(Selection, 
                                paste("Response:", named_value), 
                                paste("% of Responses:", perc_resp),
                                sep = "\n"
                   )
      ), 
      stat = "identity", 
      position = "dodge",
      width = 0.7, 
      colour = "black"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = c("cadetblue3", "dimgrey"), name = "") +
      ggtitle(title) +
      xlab("Responses") +
      ylab("Percentage of Responses") +
      theme_classic()
    ggplotly(plot, tooltip = "text")
  }
  
  # Create a function for generating text for individual questions
 
  create_qstn_text <- function(data, 
                               question, 
                               named_value_1, 
                               named_value_2,
                               extra_text
                               ) {
    # Call data and split into Scotland and LA datasets
    council_fltr <- local_authority()
    qstnDta <- data
    qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
    qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
    # Get total percentage positive 
    total_good <- filter(qstnDta_LA, 
                         value %in% c(1,2) & Selection == council_fltr
                         ) %>% 
      pull(perc_resp) %>%
      sum()
    # If this is above 55% then overall is positive, 
    # If less than 45% negative, otherwise balanced
    pos_or_neg <- ifelse(total_good > 55, 
                         "mainly positive,", 
                         ifelse(total_good < 45, 
                                "mainly negative,", 
                                "balanced,"
                                )
                         )
    # Add "only" to the % positive if this is below 45
    if (total_good < 45) {
      total_good <- paste("only", total_good)
    } else {
      total_good <- total_good
    }
    
    # Get the name for the maximum value in LA dataset. If more than one 
    # paste these together
    max_name <- as.character(qstnDta_LA %>% 
                               filter(n == max(n)) %>% 
                               pull(named_value)
                             )
    if (length(max_name > 1)) {
      max_name <- paste(max_name, collapse = " & ")
    }
    # Get the percentage for the highest response and paste together if multiple
    max_perc <- qstnDta_LA %>% 
      filter(n == max(n)) %>% 
      pull(perc_resp)
    if (length(max_perc) > 1) {
      max_perc <- paste(paste(max_perc, collapse = " & "), 
                        "percent respectively."
                        )
      } else {
        max_perc <- paste0(max_perc, " percent.")
      }
    
    # Get second highest value
    sec_val <- sort(qstnDta_LA$n, partial = 3)[3]
    # Filter for second highest value's name
    sec_name <- qstnDta_LA %>%
      filter(n == sec_val) %>% 
      pull(named_value)
    if (length(sec_name) > 1) {
      sec_name <- paste(sec_name, collapse = " & ")
    }
    # Filter for second highest value's percentage
    sec_perc <- qstnDta_LA %>% 
      filter(n == sec_val) %>% 
      pull(perc_resp)
    if (length(sec_perc) > 1) {
      sec_perc <- paste(paste(sec_perc, collapse = " & "), 
                        "percent respectively."
                        )
      } else {
        sec_perc <- paste0(sec_perc, " percent.")
      }
    
    # Get most frequent response for Scotland
    scot_max_name <- as.character(qstnDta_scot %>% 
                                    filter(n == max(n)) %>% 
                                    pull(named_value)
                                  )
    if (length(scot_max_name) > 1) {
      scot_max_name <- paste(scot_max_name, collapse = " & ")
    }
    # Get percentage for most frequent Scotland level response
    scot_max_perc <- qstnDta_scot %>% 
      filter(n == max(n)) %>% 
      pull(perc_resp)
    if (length(scot_max_perc) > 1) {
      scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), 
                             "percent respectively."
                             )
      } else {  
        scot_max_perc <- paste0(scot_max_perc, " percent.")
        }
    
    # Paste the text together
    paste0("In this year to date for the question \"",
           question,
           "\" responses have been ",
           pos_or_neg, 
           " with ",
           total_good,
           " percent saying that ",
           extra_text,
           " ",
           named_value_1,
           " or ",
           named_value_2,
           ". The greatest proportion of respondents said ",
           extra_text,
           " ",
           max_name,
           " at ", 
           max_perc, 
           " This was followed by ", 
           sec_name, 
           " at ", 
           sec_perc,
           " For Scotland overall, most respondents said that ",
           extra_text,
           " ",
           scot_max_name,
           " at ", 
           scot_max_perc
     )
     }
  
# Report Download tab (Q1 - Time taken)--------------------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_time_data_report <- reactive({
    format_qstn_dta(question = "Thinking of your engagement, how satisfied were you with the time taken to complete the process?",
                    named_value_1 = "very satisfied",
                    named_value_2 = "satisfied",
                    named_value_3 = "dissatisfied",
                    named_value_4 = "very dissatisfied"
                    )
    })
  
  # Render plot 
  output$question_time_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_time_data_report(),
                     title = "Satisfaction with time taken - Year to Date"
                     )
    })
  
  # Render text
  output$question_time_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_time_data_report(),
                     question = "Thinking of your engagement, how satisfied were you with the time taken to complete the process?",
                     named_value_1 = "very satisfied",
                     named_value_2 = "satisfied",
                     extra_text = "they were"
                     )
    })
   
# Report download tab (Q2 - Standard of communication)-----------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
   
  # Call function to generate data to be used in graph and text
  question_comms_data_report <- reactive({
   format_qstn_dta(question = "How would you rate the standard of communication provided?",
                   named_value_1 = "very good",
                   named_value_2 = "good",
                   named_value_3 = "poor",
                   named_value_4 = "very poor"
                   )
    })
  
  # Render plot 
  output$question_comms_report <- renderPlotly({
   # Call function to create plot
   create_qstn_plot(data = question_comms_data_report(),
                    title = "Standard of communication - Year to Date"
                    )
    })
   
  # Render text
  output$question_comms_report_text <- renderText({
   # Call function to create text
   create_qstn_text(data = question_comms_data_report(),
                    question = "How would you rate the standard of communication provided?",
                    named_value_1 = "very good",
                    named_value_2 = "good",
                    extra_text = "it was"
                    )
    })

# Report download tab (Q3 - Quality of info)---------------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_info_data_report <- reactive({
    format_qstn_dta(question = "Quality of the information provided",
                    named_value_1 = "very good",
                    named_value_2 = "good",
                    named_value_3 = "poor",
                    named_value_4 = "very poor"
                    )
    })
  
  # Render plot 
  output$question_info_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_info_data_report(),
                     title = "Quality of information - Year to Date"
                     )
    })
  
  # Render text
  output$question_info_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_info_data_report(),
                     question = "How would you rate the quality of information provided?",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was"
                     )
    })
     
# Report download tab (Q4 - Service offered by staff) --------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_staff_data_report <- reactive({
    format_qstn_dta(question = "Service offered by staff",
                    named_value_1 = "very good",
                    named_value_2 = "good",
                    named_value_3 = "poor",
                    named_value_4 = "very poor"
                    )
    })
  
  # Render plot 
  output$question_staff_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_staff_data_report(),
                    title = "Service offered by staff - Year to Date"
                    )
    })
     
  # Render text
  output$question_staff_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_staff_data_report(),
                     question = "How would you rate the service offered by staff",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was"
                     )
    })
  
# Report download tab (Q5 - Responsiveness to queries/issues)---------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
     
  # Call function to generate data to be used in graph and text
  question_responsiveness_data_report <- reactive({
    format_qstn_dta(question = "Responsiveness to any queries or issues raised",
                    named_value_1 = "very good",
                    named_value_2 = "good",
                    named_value_3 = "poor",
                    named_value_4 = "very poor"
                    )
    })
  
  # Render plot 
  output$question_responsiveness_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_responsiveness_data_report(),
                     title = "Responsiveness to queries or issues - Year to Date"
                     )
    })
  
  # Render text
  output$question_responsiveness_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_responsiveness_data_report(),
                     question = "How would you rate the time taken to respond to any queries or issues raised?",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was"
                     )
    })
     
# Report download tab (Q6 - Treated fairly)-----------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_fairly_data_report <- reactive({
    format_qstn_dta(question = "To what extent would you agree that you were treated fairly?",
                    named_value_1 = "strongly agree",
                    named_value_2 = "agree",
                    named_value_3 = "disagree",
                    named_value_4 = "strongly disagree"
                    )
    })
  
  # Render plot 
  output$question_fair_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_fairly_data_report(),
                     title = "Would you agree you were treated fairly - Year to Date"
                     )
    })
  
  # Render text
  output$question_fair_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_fairly_data_report(),
                     question = "To what extent would you agree that you were treated fairly?",
                     named_value_1 = "strongly agree",
                     named_value_2 = "agree",
                     extra_text = "they"
                     )
    })
     
# Report download tab (Q7 - Overall satisfaction)---------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
     
  # Call function to generate data to be used in graph and text
  question_overall_data_report <- reactive({
   format_qstn_dta(question = "Overall, how satisfied were you with the service provided?",
                   named_value_1 = "very satisfied",
                   named_value_2 = "satisfed",
                   named_value_3 = "dissatisfied",
                   named_value_4 = "very dissatisfied"
                   )
    })
  
  # Render plot 
  output$question_overall_report <- renderPlotly({
   # Call function to create plot
   create_qstn_plot(data = question_overall_data_report(),
                    title = "Overall satisfaction - Year to Date"
                    )
    })    
     
  # Render text
  output$question_overall_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_overall_data_report(),
                     question = "Overall, how satisfied were you with the service provided?",
                     named_value_1 = "very satisfied",
                     named_value_2 = "satisfied",
                     extra_text = "they were"
                     )
    })
    
# Report download tab (Report download)-----------------------------------
  
  # Create pdf report
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(la = local_authority(),
                     kpo_data = report_kpo_data(),
                     type_data = report_type_data(),
                     reason_data = report_reason_data(),
                     respondent_data = resp_dta(),
                     line_data = report_line_data(),
                     time_data = question_time_data_report(),
                     comms_data = question_comms_data_report(),
                     info_data = question_info_data_report(),
                     staff_data = question_staff_data_report(),
                     responsive_data = question_responsiveness_data_report(),
                     fair_data = question_fairly_data_report(),
                     overall_data = question_overall_data_report()
                     )
      
      # Knit the markdown document, passing in the `params` list, and eval 
      # it in a child of the global environment (this isolates the code in 
      # the document from the code in this app).
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
      }
    )
     
# Data download tab (Data download button)-----------------------------------
  
  # Create excel download
  output$all_data_dl <- downloadHandler(
    filename = paste("All_Data", ".csv", sep = ""),
    content = function(file) {
      dl_all_data <- dl_all_data()
      council_fltr <- local_authority()
      # Reorder columns so that submission date moves to start
      dl_all_data <- dl_all_data %>% rename("Submission date" = "Ended date")
      dl_all_data <- dl_all_data[c(ncol(dl_all_data), 1:(ncol(dl_all_data) - 1))]
      
      # Recode all responses for the download from a number to text, remove LA column
      dl_all_data <- dl_all_data %>% 
        filter(`Local Authority Name` == council_fltr) %>%
        mutate(across(contains("how satisfied"),
                      ~recode(., 
                              "1" = "Very satisfied", 
                              "2"="Satisfied", 
                              "3" = "Dissatisfied",
                              "4" = "Very dissatisfied",  
                              "5" = "NA"
                              )
                      )
               ) %>%
        mutate(across(contains("would you rate"),
                      ~recode(.,
                              "1" = "Very good", 
                              "2"="Good", 
                              "3" = "Poor",
                              "4" = "Very poor",  
                              "5" = "NA"
                              )
                      )
               ) %>%
        mutate(across(contains(c("quality of the information",
                                 "accuracy of the information", 
                                 "respond to", 
                                 "by staff", 
                                 "our staff"
                                 )
                               ),
                      ~recode(.,
                              "1" = "Very good", 
                              "2"="Good", 
                              "3" = "Poor",
                              "4" = "Very poor",  
                              "5" = "NA"
                              )
                      )
               ) %>%
        mutate(across(contains(c("Staff were",
                                 "Easy to find", 
                                 "Understandable"
                                 )
                               ),
                      ~recode(.,
                              "1" = "Strongly agree", 
                              "2"="Agree", 
                              "3" = "Neither agree nor disagree",
                              "4" = "Disagree",  
                              "5" = "Strongly disagree", 
                              "6" = "NA"
                              )
                      )
               ) %>%
        mutate(across(contains("would you agree"),
                      ~recode(.,
                              "1" = "Strongly agree", 
                              "2"="Agree",
                              "3" = "Disagree",  
                              "4" = "Strongly disagree", 
                              "5"="NA"
                              )
                      )
               ) %>%
        mutate(across(contains("Did you find it easy to contact"),
                      ~recode(., 
                              "1" = "Yes, contact made straight away", 
                              "2" = "Yes, but took slightly longer than expected",
                              "3" = "No it wasn’t easy, but managed to contact the officer/inspector/administrator eventually"
                              )
                      )
               ) %>%
        mutate(across(contains("Finally"),
                      ~recode(., "1" = "Yes", "2" = "No","3" = "NA")
                      )
               ) %>%
        dplyr::rename("Quarter" = "Tracking Link") %>%
        mutate(across(contains(c("Q1.1. Agent/Designer", 
                                 "Q1.2. Applicant", 
                                 "Q1.3. Contractor", 
                                 "Other (please specify):",
                                 "Q2.1. To discuss your proposal",
                                 "Q2.2. To make an application", 
                                 "Q2.3. During construction"
                                 )
                               ),
                      ~recode(., "1" = "Yes", "0" = "No")
                      )
               ) %>%
        select(-LA)
      
      # Final tidy up of column names by removing SmartSurvey variable
      colnames(dl_all_data) <- gsub(" \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\]",
                                    "", 
                                    colnames(dl_all_data)
                                    )
      colnames(dl_all_data) <- gsub("\\...[1-9]*$", "",colnames(dl_all_data))
      write.csv(dl_all_data, file)
      }
    )
  
# Data download tab (Data table)--------------------------------------------    
  
  # Create data table for full dataset
  output$tableDisp <- DT::renderDataTable({
    unpivot_data <- unpivot_data()
    # Reorder columns so that submission date moves to start
    unpivot_data <- unpivot_data[c((ncol(unpivot_data)),
                                   1:(ncol(unpivot_data) - 1)
                                   )
                                 ]
    names(unpivot_data)[3:ncol(unpivot_data)] <- gsub("Q[1-9\\.]+\\s",
                                                      "",
                                                      names(unpivot_data)[3:ncol(unpivot_data)], 
                                                      perl = TRUE
                                                      )
    
    tbl <- datatable(unpivot_data, 
                     rownames = FALSE, 
                     class = "row-border",
                     escape = FALSE,
                     extensions = c("Scroller", "FixedColumns"), 
                     options = list(pageLength = 32, 
                                    scrollY = 250, 
                                    # Used to just show table
                                    dom = "t", 
                                    scrollX = TRUE, 
                                    fnDrawCallback = htmlwidgets::JS("function(){HTMLWidgets.staticRender();}"), 
                                    columnDefs = list(list(className = "dt-center", 
                                                           targets = "_all"
                                                           )
                                                      )
                                    )
                     )
    })

# Open Text tab-------------------------------------------------------------
  
  # Create table to show comments for selected question 
  output$cmnt_table <- DT::renderDataTable({
    unpivot_data <- unpivot_data()
    # Need to filter the data based on selections and recode answers
    names(unpivot_data)[3:ncol(unpivot_data)] <- gsub("Q[1-9\\.]+\\s",
                                                      "",
                                                      names(unpivot_data)[3:ncol(unpivot_data)], 
                                                      perl = TRUE
                                                      )
    unpivot_data$Quarter <- as.factor(unpivot_data$Quarter)
    # store selected respondent type  
    slctn_respondent <- input$cmnts_resp_input
    # Select selected respondent reason using partial match
    slctn_reason <- names(select(unpivot_data, 
                                 contains(input$cmnts_reason_input)
                                 )
                          )
    # Filter data to show comments for selected question, and respondents
    filter_data <- unpivot_data %>% 
      filter(if_any(slctn_respondent, ~ . == "Yes")) %>%
      filter(if_any(slctn_reason, ~.== "Yes")) %>%
      select(Quarter, `Financial Year`, contains(input$cmnts_slct))
    
    # Create datatable
    datatable(filter_data, 
              filter = "top",
              rownames = FALSE, 
              class = "row-border",
              escape = FALSE,
              extensions = c("Scroller", "FixedColumns")
              )
    })
  
# Closing bracket for opening function ---------------------------------------  
  }