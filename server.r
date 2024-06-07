function(input, output, session) {
  
  # Set up reactive council selection based on log in -------------------------
  user <- reactive({
    #session$user
    "cara.connachan@improvementservice.org.uk"
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
                                                                    # Validation - will show nice error message before LA is selected
                                                                    validate(need(input$LA_selection != "",
                                                                                  "Local Authority selection needed"))
                                                                    input$LA_selection
                                                                    } 
  })

  
  # Creates a heading with the selected council name
  output$LA_KPO4_Heading <- renderUI({
    h2(paste("KPO4 Performance", local_authority(), sep = " - "), 
       style = "margin-top:3px")
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
                                    href="__logout__"))
    }
  })

  # Financial Year Selection---------------------------------------------------
  
  # Create select button for financial year - will only show years where there is data available
  output$fin_yr <- renderUI({
    dta <- pivot_dta %>% filter(`Local Authority Name` == local_authority())
    # Will show a nice error message if there is no data for that council
    validate(need(row(dta) > 0,"No data available"))
    
    years <- sort(unique(dta$`Financial Year`), decreasing = TRUE)
    
    selectizeInput(inputId = "fin_yr_selection", 
                   label = "Select Financial Year",
                   choices = years, 
                   selected = crnt_fin_yr)
  })
  
  # Reactive expression to store financial year selected
  # Either selected year if more than 1 available, otherwise year available
  fin_yr <- reactive({
    dta <- pivot_dta %>% 
      filter(`Local Authority Name` == local_authority())
    # Will show a nice error message for any graphs that reference fin_yr 
    # if there is no data for that council
    validate(need(nrow(dta) > 0,"No data available"))
    
    years <- unique(dta$`Financial Year`)
    no_years <- length(unique(dta$`Financial Year`))
    fin_yr <- if (no_years > 1) {
      input$fin_yr_selection
    } else {
      years
    }
  })
  
  # Quarter Selection---------------------------------------------------
  
  # Create select button for quarter - default YTD
  output$qrtr <- renderUI({
    dta <- pivot_dta %>% 
      filter(`Financial Year` == fin_yr() &
               `Local Authority Name` == local_authority())
    # Will show a nice error message if there is no data for that council
    validate(need(row(dta) > 0,"No data available"))
    
    quarters <- unique(dta$Quarter)
    quarters <- sort(quarters)
    
    selectizeInput(inputId = "qrtr_selection", 
                   label = "Select Quarter",
                   choices = c("Year to Date", quarters), 
                   selected = "Year to Date")
  })
  
  # Reactive expression to store quarter selected
  # Either selected quarter if more than 1 available, otherwise quarter available
  qrtr <- reactive({
    dta <- pivot_dta %>% 
      filter(`Financial Year` == fin_yr() &
               `Local Authority Name` == local_authority())
    # Will show a nice error message for any graphs that reference qrtr
    # if there is no data for that council
    validate(need(nrow(dta) > 0,"No data available"))
    
    quarters <- unique(dta$Quarter)
    no_quarters <- length(unique(dta$Quarter))
    qrtr <- if (no_quarters < 2) {
      quarters
    } else {
      if (input$qrtr_selection == "Year to Date") {
        quarters
      } else {
        input$qrtr_selection
      }
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
    dl_all_data$Quarter <- as.yearqtr(dl_all_data$`Ended date`, 
                                              format = "%Y-%m-%d"
    ) 
    # This formatting uses calender year values rather than financial so need
    # to reduce by a quarter to format as financial years
    dl_all_data$`Financial Year` <- dl_all_data$Quarter- 1/4
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
    dl_all_data$Quarter <- dl_all_data$Quarter- 1/4
    dl_all_data$Quarter <- gsub("[0-9]*\\ Q", 
                                        "Quarter ", 
                                        dl_all_data$Quarter, 
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
    
    dta <- dwnld_table_dta
    # Recode "other" respondents and reasons so it doesn't show text value
    dta$`Other respondent`[dta$`Other respondent` != "0"] <- "1"
    dta$`Other reason`[dta$`Other reason` != "0"] <- "1"
    dta <- dta %>% 
      mutate(across(c(`Other respondent`, `Other reason`), ~as.numeric(.)))
    
    # Calculates within each LA, the number of respondents answering yes to 
    # the different respondent options. Then calculates as a % of all responses
    
    # Financial year responses
    fin_yr_dta <- dta %>%
      select(Quarter:`Other reason`) %>%
      pivot_longer(`Agent/Designer`:`Other reason`, 
                   names_to = "Question", 
                   values_to = "value") %>%
      group_by(`Financial Year`,`Local Authority Name`, Question) %>%
      count(value) %>%
      mutate(perc = round((n/sum(n)) * 100, 1))
    
    # Quarter responses
    qrtr_dta <- dta %>%
      select(Quarter:`Other reason`) %>%
      pivot_longer(`Agent/Designer`:`Other reason`, 
                   names_to = "Question", 
                   values_to = "value") %>%
      group_by(Quarter, `Financial Year`,`Local Authority Name`, Question) %>%
      count(value) %>%
      mutate(perc = round((n/sum(n)) * 100, 1))
    
    # Combine quarter and YTD data
    dta <- rbind(qrtr_dta, fin_yr_dta) %>%
      mutate(Quarter = replace_na(Quarter, "Year to Date")) %>%
      # Differentiates questions by whether they ask about respondent types or reasons
      mutate(question_type = if_else(Question %in% c("Agent/Designer",
                                                     "Applicant",
                                                     "Contractor",
                                                     "Other respondent"),
                                     "Type",
                                     "Reason"))
    
    # Filter to selected council & selected financial year
    dta <- dta %>%
      filter(Quarter == input$qrtr_selection & 
               `Financial Year` == fin_yr() & 
               `Local Authority Name` == local_authority())
    dta
  })
  
  # Finalise unpivot data ---------------------------------------------------
  
  # This is used in the data download table and the respondent no. value box
  unpivot_data <- reactive({
   dta <- dwnld_table_dta %>%
     # Recode responses for download and to show in table
      mutate(across(contains(c("Agent/Designer", 
                               "Applicant", 
                               "Contractor",
                               "Other respondent",
                               "To discuss your proposal",
                               "To make an application", 
                               "During construction",
                               "Other reason")),
                    ~str_replace_all(., c("1" = "Yes", "0" = "No")))) %>%
     mutate(across(c("How satisfied were you with the time taken?",
                     "How satisfied were you overall?"),
                     ~str_replace_all(.,
                                      c("1" = "Very satisfied",
                                        "2" ="Satisfied",
                                        "3" = "Dissatisfied",
                                        "4" = "Very dissatisfied")))) %>% 
     mutate(across(c("How would you rate the standard of communication?",
                     "Quality of the information provided",
                     "Service offered by staff",
                     "Responsiveness to any queries or issues raised"),
                     ~str_replace_all(.,
                                      c("1" = "Very good",
                                        "2" ="Good",
                                        "3" = "Poor",
                                        "4" = "Very poor")))) %>%
     mutate(`To what extent would you agree that you were treated fairly?` =
              str_replace_all(`To what extent would you agree that you were treated fairly?`,
                              c("1" = "Strongly agree",
                                "2" ="Agree",
                                "3" = "Disagree",
                                "4" = "Strongly disagree"))) %>%
     # Filter this data for selected council
     filter(`Local Authority Name` == local_authority())
   dta
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
  KPOweights_multiplier <- outer(pivot_dta$Indicator == "How satisfied were you with the time taken?", 2)+
    outer(pivot_dta$Indicator == "How would you rate the standard of communication?", 2) +
    outer(pivot_dta$Indicator == "How satisfied were you overall?", 8)
  KPOweights_multiplier <- replace(KPOweights_multiplier, 
                                   KPOweights_multiplier == 0, 
                                   1)
  # This calculates the weighted score
  KPOdta$KPO_weight <- KPOdta$KPO_weight * KPOweights_multiplier  
  
  # Get total and max values for Scotland by quarter and financial year
  
  # This calculates the highest possible weighted score available for each
  # response by multiplying the highest score by the weighting for that question
  scot_max <- pivot_dta %>% 
    mutate(maxAvailable = 4) %>% 
    mutate(value = as.numeric(value)) %>%
    mutate(maxAvailable = (maxAvailable - 1) * KPOweights_multiplier) %>%
    mutate(maxAvailable = replace(maxAvailable, is.na(value), NA)) %>%
    # This calculates the actual weighted score  
    mutate(KPO4_weighted = (value - 1) * KPOweights_multiplier)
  
  # Calculate KPO4 for quarter and financial year for all results   
  scot_max_sum <- scot_max %>% 
    group_by(Quarter, `Financial Year`) %>%
    summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
    group_by(`Financial Year`) %>%
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Year to Date"))) %>%
    # Generate the KPO score (out of 10)    
    mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1)) %>%
    ungroup()
  
  # Calculate KPO4 for quarter and financial year for selected local authority    
  la_max_sum <- reactive({
    dta <- scot_max %>% 
      filter(`Local Authority Name` == local_authority()) %>%     
      group_by(Quarter, `Financial Year`) %>%
      summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
      group_by(`Financial Year`) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.character), ~"Year to Date"))) %>%
      # Generate the KPO score (out of 10)    
      mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1)) %>%
      ungroup()
    # Will show a nice error message if there is no data for that council
    validate(need(nrow(dta) > 0, "No data available"))
    dta
  })
  
  # KPO4 per respondent type #
  
  # Function to get KPO by respondent type at Scotland level
  scot_resp_kpo <- function(resp_col){
    resp_col <- enquo(resp_col)
    dta <- scot_max %>% 
      filter(!!resp_col == 1) %>%
      group_by(Quarter,`Financial Year`) %>%
      summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
      ungroup() %>%
      group_by(`Financial Year`) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.character), ~"Year to Date")))  %>%
      # Generate the KPO score (out of 10)    
      mutate(KPO_score = (1 - KPO4_weighted/maxAvailable) * 10) %>%
      ungroup()
    
    return(dta)
  }
  
  # Calculate KPO4 for quarter for all results   
  scot_kpo_agent <- scot_resp_kpo(resp_col = `Agent/Designer`)
  scot_kpo_applicant <- scot_resp_kpo(resp_col = `Applicant`)
  scot_kpo_contractor <- scot_resp_kpo(resp_col = `Contractor`)
  scot_kpo_other <- scot_resp_kpo(resp_col = `Other respondent`)
  
  # Function to get KPO by respondent type at local authority level
  la_resp_kpo <- function(resp_col){
    resp_col <- enquo(resp_col)
    dta <- scot_max %>% 
      filter(!!resp_col == 1) %>%
      filter(`Local Authority Name` == local_authority()) %>%     
      group_by(Quarter, `Financial Year`) %>%
      summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>% 
      ungroup() %>%
      group_by(`Financial Year`) %>%
      bind_rows(summarise(.,across(where(is.numeric), sum),
                          across(where(is.character), ~"Year to Date")))  %>%
      # Generate the KPO score (out of 10)    
      mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1)) %>%
      ungroup()
  }
  
  # Calculate KPO4 for quarter for selected local authority    
  la_kpo_agent <- reactive({ la_resp_kpo(resp_col = `Agent/Designer`) })
  la_kpo_applicant <- reactive({ la_resp_kpo(resp_col = `Applicant`)  })
  la_kpo_contractor <- reactive({ la_resp_kpo(resp_col = `Contractor`) })
  la_kpo_other <- reactive({ la_resp_kpo(resp_col = `Other respondent`)  })
  
  # Create KPO4 download ---------------------------------------------------
  
  # Create data frame with KPO4 scores for all LA's 
  # (this is available for SG and IS to download)
  total_la_max_sum <- scot_max %>%      
    group_by(Quarter, `Financial Year`, `Local Authority Name`) %>%
    summarise(across(c(maxAvailable, KPO4_weighted), sum, na.rm = TRUE)) %>%
    group_by(`Local Authority Name`, `Financial Year`) %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~"Year to Date"))) %>%
    # Generate the KPO score (out of 10)    
    mutate(KPO_score = round((1 - KPO4_weighted/maxAvailable) * 10, 1)) %>%
    rbind(scot_max_sum) %>%
    select(-maxAvailable, -KPO4_weighted) %>%
    rename(Area = `Local Authority Name`) %>%
    rename(Quarter = Quarter) %>%
    rename(`KPO4 Score` = KPO_score) %>%
    mutate(Area = replace_na(Area, "Scotland")) %>%
    arrange(`Financial Year`, Quarter)
  
  # Create downloadable file
  output$KPO_data_file <- downloadHandler(filename = paste0("KPO4_Data", ".csv"),
  content = function(file) {
    write.csv(total_la_max_sum, file)
    })
  
  # Create conditionality to only show download button if IS or SG
  output$KPO_data_dl <- renderUI({
    if(grepl("improvementservice.org.uk|gov.scot", user(), ignore.case = TRUE)) {
      downloadBttn("KPO_data_file", 
                   label = "Download KPO4 Data", 
                   style = "jelly", 
                   size = "sm")
      } else {
        return()
        }
  })
  
  # Performance Overview Tab (Performance boxes) ------------------------------
  
  # Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    
    kpo_score <- la_max_sum() %>%
      filter(Quarter == input$qrtr_selection & `Financial Year` == fin_yr()) %>%
      pull(KPO_score)
    
    # Sets traffic light colours based on KPO4 score
    kpo_colr <- if_else(kpo_score > 7.5, 
                        "green",
                        if_else(kpo_score < 6.5,
                                "red",
                                "orange"))

    valueBox(value = round(kpo_score, 1), 
             paste("Council KPO4", input$qrtr_selection, fin_yr()),
             icon = icon("chart-bar"), 
             color = kpo_colr)
  })
  
  # Create performance box for Scotland
  output$scotPerfBox<- renderValueBox({
    # Will show a nice error message if there is no data for that council
    validate(need(input$qrtr_selection != "","No data available"))
    
    kpo_score <- scot_max_sum %>%
      filter(Quarter == input$qrtr_selection & `Financial Year` == fin_yr()) %>%
      pull(KPO_score)
    
    valueBox(value = round(kpo_score, 1), 
             paste("Scotland Average KPO4", input$qrtr_selection, fin_yr()), 
             icon = icon("times"), 
             color = "navy")
  })
  
  # Create valuebox for number of responses 
  output$respBox <- renderValueBox({
    dta <- dwnld_table_dta %>%
      filter(`Local Authority Name` == local_authority())
    # Will show a nice error message if there is no data for that council
    validate(need(input$qrtr_selection != "","No data available"))
    
    # Store number of responses in each quarter of the selected financial year
    q1_response <- dta %>%
      filter(Quarter == "Quarter 1" & `Financial Year` == fin_yr()) %>%
      nrow()
    q2_response <- dta %>%
      filter(Quarter == "Quarter 2" & `Financial Year` == fin_yr()) %>%
      nrow()
    q3_response <- dta %>%
      filter(Quarter == "Quarter 3" & `Financial Year` == fin_yr()) %>%
      nrow()
    q4_response <- dta %>%
      filter(Quarter == "Quarter 4" & `Financial Year` == fin_yr()) %>%
      nrow()
    full_yr_response <- dta %>%
      filter(`Financial Year` == fin_yr()) %>%
      nrow()
    
    # Counts the number of rows (responses) for the full year
    valueBox(value = tags$p(style = "font-size:18px; line-height:0px; margin-bottom:0px;",
                            paste(full_yr_response,
                                  "Responses Year to Date", 
                                  fin_yr())),
             subtitle = tags$p(style = "font-size:14px; line-height:1.1;",
                               # Uses response numbers for each of the quarters
                               HTML(sprintf("%s - Quarter 1<br/>%s - Quarter 2<br/>%s - Quarter 3<br/>%s - Quarter 4",
                                            q1_response, 
                                            q2_response, 
                                            q3_response, 
                                            q4_response))),
             icon = icon("user-friends"), 
             color = "light-blue")
    })
  
  # Performance Overview tab (KPO4 bar plot) -----------------------------------
  
  # Create function for creating KPO4 plot
  create_KPO4_plot <- function(dataset, 
                               plot_title,
                               title_width,
                               label_size,
                               text_size
                               ) {
    
    validate(need(nrow(dataset) > 0,"No respondents of this type"))
    
    # Rename Total as year to date
    dta <- dataset %>%
      mutate(Quarter = str_replace(Quarter, "Year to Date", "YTD")) %>%
      # Filter to only include the Quarters for current year
      filter(Quarter == "YTD" | 
               (Quarter != "YTD" & 
                  `Financial Year` == fin_yr())) %>%
      # Add Financial year to quarter labels
      mutate(Quarter = str_replace(Quarter, "Quarter ", "Q")) %>%
      mutate(Label = paste(Quarter, `Financial Year`))
    # Store the number of YTD values to determine the colours for these bars
    YTD <- dta %>%
      count(Quarter) %>%
      filter(Quarter == "YTD") %>%
      pull(n)
    
    # Set colours for quarter by kpo4
    kpo_clrs <- dta %>% 
      filter(Quarter != "YTD") %>% 
      pull(KPO_score)
    clrs <- if_else(kpo_clrs > 7.5, 
                   "palegreen3", 
                   if_else(kpo_clrs < 6.5, 
                          "tomato", 
                          "tan1"))
    
    p <- ggplot(data = dta) +
      geom_bar(aes(x = Label, 
                   y = KPO_score,
                   text = paste(paste("Quarter:", Label),
                                paste("KPO 4 Score", KPO_score),
                                sep = "\n")), 
      stat = "identity",
      position = "dodge", 
      fill = c(clrs, rep("dimgrey", YTD)), 
      width = 0.7, 
      colour = "black") +
      theme_classic() +
      # Span x axis labels over multiple lines
      scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
      scale_y_continuous(limits = c(0, 10), 
                         expand = expansion(mult = c(0, 0.1))) +
      ggtitle(str_wrap(plot_title, width = title_width)) +
      ylab("KPO 4 Score") +
      xlab("Response period") +
      theme(axis.text.x = element_text(size = text_size),
            axis.title = element_text(size = label_size),
            plot.title = element_text(size = label_size))
    
    ggplotly(p, tooltip = "text")
  }
  
  # Run function to create bar plot for overall performance
  output$ovrPerfBar <- renderPlotly({
    create_KPO4_plot(dataset = la_max_sum(), 
                     plot_title = "KPO4 Performance by Quarter and Year to Date",
                     title_width = 45,
                     label_size = 12,
                     text_size = 10)
  })
  
  # Performance Overview tab (respondent type & reason plots)------------------
  
  # Extract data for response type
  report_type_data <- reactive({
    dta <- resp_dta() %>% 
      filter(., question_type == "Type" & value == 1)
    dta
  })
  
  # This output is used twice, in the UI, but this is not allowed
  # therefore the output is assigned twice so that it can be called twice
  # using the different names. 
  # resp_type_graph_report is used in the report download tab
  # resp_type_graph_overview is used in the performance overview tab
  
  output$resp_type_graph_report <- output$resp_type_graph_overview <- renderPlotly({
    dta <- report_type_data()
    plot <- ggplot(data = dta) +
      geom_col(aes(x = fct_reorder(Question, desc(perc)),
                   y = perc,
                   text = paste(paste("Respondent Type:", 
                                      dta$Question),
                                paste("% of responses:", 
                                      dta$perc),
                                sep = "\n")),
               fill = "cadetblue3", 
               colour = "black") +
      coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme_classic() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle(paste("Respondent Type:\n", input$qrtr_selection, fin_yr())) +
      xlab("Respondent Type") +
      ylab("Percentage of Responses") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
    
    ggplotly(plot, tooltip = "text")
  })
  
  # Extract data for response reason
  report_reason_data <- reactive({
    dta <- resp_dta() %>% 
      filter(., question_type == "Reason" & value == 1)
    dta
  })
  
  # This output is used twice, in the UI, but this is not allowed
  # therefore the output is assigned twice so that it can be called twice
  # using the different names. 
  # resp_reason_graph_report is used in the report download tab
  # resp_reason_graph_overview is used in the performance overview tab
  
  output$resp_reason_graph_report <- output$resp_reason_graph_overview <- renderPlotly({
    dta <- report_reason_data()
    plot <- ggplot(data = dta) +
      geom_col(aes(x = fct_reorder(Question, desc(perc)),
                   y = perc,
                   text = paste(paste("Reason:", dta$Question),
                                paste("% of responses:", dta$perc),
                                sep = "\n")),
               fill = "cadetblue3", 
               colour = "black") +
      coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme_classic() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle(paste("Response Reason:\n", input$qrtr_selection, fin_yr())) +
      xlab("Reason") +
      ylab("Percentage of Responses") +
      theme(plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
    
    ggplotly(plot, tooltip = "text")
  })

# KPO4 by Respondent Graphs -------------------------------
  
  # Run function to create KPO4 plot for agents
  output$kpo_resp_graph_agent <- renderPlotly({
    create_KPO4_plot(dataset = la_kpo_agent(),
                     plot_title = "KPO4 by Quarter & Year to Date - Agent",
                     title_width = 31,
                     label_size = 10,
                     text_size = 8
                     )
  })
  
  # Run function to create KPO4 plot for applicants
  output$kpo_resp_graph_applicant <- renderPlotly({
    create_KPO4_plot(dataset = la_kpo_applicant(),
                     plot_title = "KPO4 by Quarter & Year to Date - Applicant",
                     title_width = 31,
                     label_size = 10,
                     text_size = 8
                     )
  })
  
  # Run function to create KPO4 plot for contractors
  output$kpo_resp_graph_contr <- renderPlotly({
    create_KPO4_plot(dataset = la_kpo_contractor(),
                     plot_title = "KPO4 by Quarter & Year to Date - Contractor",
                     title_width = 31,
                     label_size = 10,
                     text_size = 8
                     )
  })
  
  # Run function to create KPO4 plot for "other"
  output$kpo_resp_graph_other <- renderPlotly({
    create_KPO4_plot(dataset = la_kpo_other(),
                     plot_title = "KPO4 by Quarter & Year to Date - Other",
                     title_width = 31,
                     label_size = 10,
                     text_size = 8
                     )
  })
  
  # Questions results tab (Data filtering)--------------------------------
  
  # Create filtered dataset from checkboxes
  qstn_dataset_filtered <- reactive({
    pivot_dta$Quarter <- as.factor(pivot_dta$Quarter)
    # Selected respondent type and reason 
    slctn_respondent <- input$Qs_resp_input
    slctn_reason <- input$Qs_reason_input
    # Filter to match LA, selected respondent type & reason
    dta <- pivot_dta %>% 
      filter(`Local Authority Name` == local_authority()) %>%
      filter(if_any(slctn_respondent, ~. == 1)) %>%
      filter(if_any(slctn_reason, ~. == 1)) %>%
      # Filter to correct financial year - either current year or selected year
      filter(`Financial Year` == fin_yr())
    dta
  })

  # Questions Results tab (YTD plot)---------------------------------------
  
  # First graph for full breakdown of responses in year to date  
  
  output$YTDqstsPlot <- renderPlotly({
    # Filter data based on question selected & set responses as factors
    if (input$Qstn_tab2 == "All Questions") {
      dta <- qstn_dataset_filtered()
    } else {
      dta <- qstn_dataset_filtered() %>% 
        filter(Indicator == input$Qstn_tab2) 
    }
    # Calculate count for each response
    dta <- dta %>% 
      drop_na(value) %>%
      count(value) %>% 
      mutate(perc = round((n/sum(n)) * 100, 2))
    
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    if (input$Qstn_tab2 == "All Questions") {
      dta <- dta %>%
        mutate(named_value = recode(value, 
                                    "1" = "Very Good/Very Satisfied/Strongly Agree",
                                    "2" = "Good/Satisfied/Agree",
                                    "3" = "Poor/Dissatisfied/Disagree",
                                    "4" = "Very Poor/Very Dissatisfied/Strongly Disagree"))
    } else 
      if (input$Qstn_tab2 == "How satisfied were you overall?"|input$Qstn_tab2 == "How satisfied were you with the time taken?") {
        dta <- dta %>%
          mutate(named_value = recode(value, 
                                      "1" = "Very Satisfied",
                                      "2" =" Satisfied",
                                      "3" = "Dissatisfied",
                                      "4" = "Very Dissatisfied"))
      } else 
        if (input$Qstn_tab2 == "To what extent would you agree that you were treated fairly?") {
          dta <- dta %>%
            mutate(named_value = recode(value, 
                                        "1" = "Strongly Agree",
                                        "2" = "Agree",
                                        "3" = "Disagree",
                                        "4" = "Strongly Disagree"))
        } else {
          dta <- dta %>%
            mutate(named_value = recode(value, 
                                        "1" = "Very Good",
                                        "2" = "Good",
                                        "3" = "Poor",
                                        "4" = "Very Poor"))
        }
    
    # Format value as numeric to use in order
    dta$value <- as.numeric(dta$value)
    
    # Generate barplot
    plot <- ggplot(data = dta) +
      geom_bar(aes(x = fct_reorder(named_value, value), 
                   y = perc, 
                   text = paste(paste("Response:", named_value), 
                                paste0("Number of Responses ", 
                                       fin_yr(), 
                                       ": ", 
                                       perc,
                                       "%"),
                                sep = "\n")),
               stat= "identity",
               fill = "cadetblue3", 
               width = 0.7, 
               colour = "black") +
      ggtitle(str_wrap(input$Qstn_tab2, width = 60)) +
      xlab("Response") +
      ylab(paste("% of responses", fin_yr())) +
      # Span x axis labels over multiple lines
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_classic()
    
    ggplotly(plot, tooltip = "text")
  })
  
  # Questions Results tab (Summary by quarters plot)-------------------------
  
  # Second summary of percentage responses by quarter
  
  output$qrtsQsplot <- renderPlotly({
    # Filter dataset based on selected question & set responses as factors
    if(input$Qstn_tab2 == "All Questions") {  
      dta <- qstn_dataset_filtered()
    } else {
      dta <- qstn_dataset_filtered() %>%
        filter(Indicator == input$Qstn_tab2)
    }
    # Calculate count for each response
    dta <- dta %>% 
      drop_na(value) %>%
      count(Quarter, value)
    # Summarise the count per quarter
    dta <- dta %>% 
      group_by(Quarter) %>%
      mutate(total_responses = sum(n)) %>%
      ungroup() %>%
      mutate(`% of responses` = round((n/total_responses * 100), 1))
    
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    if (input$Qstn_tab2 == "All Questions") {
      dta <- dta %>%
        mutate(named_value = recode(value, 
                                    "1" = "Very Good/Very Satisfied/Strongly Agree",
                                    "2" = "Good/Satisfied/Agree",
                                    "3" = "Poor/Dissatisfied/Disagree",
                                    "4" = "Very Poor/Very Dissatisfied/Strongly Disagree"))
    } else 
      if (input$Qstn_tab2 == "How satisfied were you overall?"|input$Qstn_tab2 == "How satisfied were you with the time taken?") {
        dta <- dta %>%
          mutate(named_value = recode(value, 
                                      "1" = "Very Satisfied",
                                      "2" =" Satisfied",
                                      "3" = "Dissatisfied",
                                      "4" = "Very Dissatisfied"))
      } else 
        if (input$Qstn_tab2 == "To what extent would you agree that you were treated fairly?") {
          dta <- dta %>%
            mutate(named_value = recode(value, 
                                        "1" = "Strongly Agree",
                                        "2" = "Agree",
                                        "3" = "Disagree",
                                        "4" = "Strongly Disagree"))
        } else {
          dta <- dta %>%
            mutate(named_value = recode(value, 
                                        "1" = "Very Good",
                                        "2" = "Good",
                                        "3" = "Poor",
                                        "4" = "Very Poor"))
        }
    
    # Add Financial year to quarter labels
    dta <- dta %>%
      mutate(Quarter = str_replace(Quarter, "Quarter ", "Q")) %>%
      mutate(Quarter = paste(Quarter, fin_yr()))
    
    # Format value as numeric to use in order
    dta$value <- as.numeric(dta$value)
    
    # Generate bar plot
    plot <- ggplot(data = dta ) +
      geom_bar(aes(x = Quarter, 
                   y = `% of responses`, 
                   fill = fct_reorder(named_value, value), 
                   text = paste(paste("Quarter:", Quarter),
                                paste("% of responses:", `% of responses`),
                                paste("Response:", named_value),
                                sep = "\n")),
                   stat = "identity", 
                   position = "stack",
                   width = 0.7, 
                   colour = "black") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      ggtitle(str_wrap(input$Qstn_tab2, width = 60)) +
      xlab("") +
      ylab(paste("% of responses", fin_yr())) +
      theme_classic() +
      scale_fill_manual(values = c("forestgreen", 
                                   "lightgreen", 
                                   "darkorange", 
                                   "firebrick"),
                        name = "Responses")
    ggplotly(plot, tooltip = "text")
  })
  
# Questions Results Tab (Table with respondent number per Quarter)--------------------------------------
  output$resp_qrts <- renderTable({
    # Only want to count each respondent once, so only keep the question 
    # selected by user on page 2
    qst_ind <- if_else(input$Qstn_tab2 == "All Questions", 
                       "How satisfied were you overall?", 
                       input$Qstn_tab2
                       )
    dta <- qstn_dataset_filtered() %>% 
      filter(Indicator == qst_ind) %>% 
      drop_na(value) %>%
      # Calculate count per quarter
      count(Quarter) %>%
      add_row(Quarter = "Year to Date", n = sum(.$n)) %>%
      rename(Number = n)
  })
  
  # Report Download Tab (KPO4 YTD)------------------------------------------
  
  # Create data for KPO4 in report page - combine LA & Scot level data 
  # Note - data has to be created in a reactive function, separate from the 
  # plot function, so the data can be used in the markdown document
  
  report_kpo_data <- reactive({
    la_max_sum <- la_max_sum() %>%
      mutate(id = local_authority())
    scot_max_sum <- scot_max_sum %>%
      mutate(id = "Scotland")
    dta <- rbind(scot_max_sum, la_max_sum)
    dta
  })
  
  # Generate plot for KPO4 in report page 
  output$reportKPO4Plot <- renderPlotly({
    dta <- report_kpo_data() %>% 
      filter(Quarter == "Year to Date")
    # Set the council values as a factor so the data can be arranged to 
    # have the council first regardless of alphabetical order
    dta$id <- factor(dta$id, levels = c(local_authority(), "Scotland"))
    dta <- arrange(dta, id)
    # Store number of years to determine the number of reps for colours of bars
    Years <- length(unique(dta$`Financial Year`))
    
    plot <- ggplot(data = dta) +
      geom_bar(aes(x = `Financial Year`, 
                   y = KPO_score, 
                   fill = id,
                   text = paste(`Financial Year`, 
                                id, 
                                paste("KPO 4 Score:", KPO_score), sep = "\n")), 
               stat = "identity",
               position = "dodge",
               width = 0.7, 
               colour = "black") +
      scale_y_continuous(limits = c(0, 10), 
                         expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = rep(c("cadetblue3", "dimgrey"), Years), 
                        name = "") + 
      ggtitle("KPO 4 score - Year to Date") +
      xlab("") +
      ylab("KPO 4 Score") +
      theme_classic()
    
    ggplotly(plot, tooltip = "text")
  })
  
  # Render text for KPO4 Overall performance  to year
  output$KPO4_text_report <- renderText({
    Years <- report_kpo_data() %>%
      filter(id == local_authority()) %>%
      distinct(`Financial Year`) %>%
      arrange(`Financial Year`) %>%
      mutate("prev_year" = lag(`Financial Year`))
    # Filter to selected financial year
    dta <- report_kpo_data() %>%
      filter(Quarter == "Year to Date" & `Financial Year` == fin_yr())
    
    # Compare council KPO4 values with Scotland and target to create 
    # reactive text values
    # Council KPO4
    KPO4_ytd <- dta %>% 
      filter(id == local_authority()) %>% 
      pull(KPO_score)
    # Scotland KPO4
    scotAv_kpo4 <- dta %>% 
      filter(id == "Scotland") %>% 
      pull(KPO_score)
    # Compares council KPO4 with target KPO4
    hilow_kpo4 <- if_else(KPO4_ytd > 7.5, 
                          "higher than", 
                          if_else(KPO4_ytd < 7.5, 
                                  "lower than", 
                                  "equal to"))
    # Compares council KPO4 with Scotland KPO4
    abbel_kpo4 <- if_else(KPO4_ytd > scotAv_kpo4, 
                         "higher than",
                         if_else(KPO4_ytd < scotAv_kpo4, 
                                 "lower than", 
                                 "equal to"))
    
    # Store value for previous financial year if there is one
    prev_fin_yr <- Years %>%
      filter(`Financial Year` == fin_yr()) %>%
      pull(prev_year)
    KPO4_other <- report_kpo_data() %>%
      filter(Quarter == "Year to Date" & 
               `Financial Year` == prev_fin_yr & 
               id == local_authority()) %>%
      pull(KPO_score)
    
    # Create comparison of KPO4 for selected year with previous year available
    diff_value <- KPO4_ytd - KPO4_other
    diff_text <- if_else(diff_value < 0, "lower", "higher")
    diff_value <- round(abs(diff_value), 1)
    
    # Text for when there is only 1 financial year
    text_kpo <- paste0("This indicator summarises performance across all questions, with differential weightings based on importance. For ", 
                       local_authority(),
                       " in ",
                       fin_yr(), 
                       " overall performance is at ", 
                       KPO4_ytd, 
                       " for the year to date. ", 
                       "This is ",
                       abbel_kpo4,
                       " the Scotland average of ", 
                       scotAv_kpo4,
                       " and ", 
                       hilow_kpo4,
                       " the target value of 7.5.")
    
    # Text for when there are more than 1 financial years     
    text_multiple_kpo <- paste0("This indicator summarises performance across all questions, with differential weightings based on importance. For ", 
                                local_authority(),
                                " in ",
                                fin_yr(), 
                                " overall performance is at ", 
                                KPO4_ytd, 
                                " for the year to date. This is ", 
                                diff_value,
                                " points ",
                                diff_text,
                                " than the performance of ", 
                                KPO4_other, 
                                " in ", 
                                prev_fin_yr, 
                                ". The year to date performance of ", 
                                local_authority(), 
                                " in ", 
                                fin_yr(), 
                                " is ", 
                                abbel_kpo4,
                                " the Scotland average of ", 
                                scotAv_kpo4,
                                " and ", 
                                hilow_kpo4,
                                " the target value of 7.5.")
    
    # Conditional statement to select text based on whether there is a previous year to compare   
    final_text <- if_else(is.na(prev_fin_yr), text_kpo, text_multiple_kpo)
    return(final_text)
  })
  
  # Report download tab (respondent reason & type)----------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Text for respondent types 
  output$respondent_type_text_report <- renderText({
    
    # Filter data to respondent type
    dta <- resp_dta() %>% 
      filter(question_type == "Type")
    # Get total responses for referencing the percentage denominator
    resp_number <- dta %>%
      group_by(Question) %>%
      summarise("total_responses" = sum(n)) %>%
      distinct(total_responses) %>%
      pull(total_responses)
    # Create variables for percentages for different groups
    agent_perc <- dta %>%
      filter(Question == "Agent/Designer" & value == 1) %>%
      pull(perc)
    appli_perc <- dta %>%
      filter(Question == "Applicant" & value == 1) %>%
      pull(perc)
    contr_perc <- dta %>%
      filter(Question == "Contractor" & value == 1) %>%
      pull(perc)
    other_perc <- dta %>%
      filter(Question == "Other respondent" & value == 1) %>%
      pull(perc)
    # If any are 0 then replace with "none"
    agent_perc <- if_else(is_empty(agent_perc), "none", paste0(agent_perc, "%"))
    appli_perc <- if_else(is_empty(appli_perc), "none", paste0(appli_perc, "%"))
    contr_perc <- if_else(is_empty(contr_perc), "none", paste0(contr_perc, "%"))
    other_perc <- if_else(is_empty(other_perc), 
                          "No respondents", 
                          paste0(other_perc, "%"))
    # Paste all text together
    txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, as well as their reason for contacting the Building Standards Service in ", 
                              local_authority(),
                              ". Of the ", 
                              resp_number, 
                              " respondents in ",
                              input$qrtr_selection,
                              " ",
                              fin_yr(),
                              ", ",
                              agent_perc, 
                              " were agents or designers, ", 
                              appli_perc, 
                              " were applicants and ", 
                              contr_perc, 
                              " were contractors. ", 
                              other_perc, 
                              " said they were an other respondent type.")
    txt_respondents
  })
  
  # Text for respondent reason
  output$respondent_reason_text_report <- renderText({
    # Filter data to respondent reason
    dta <- resp_dta() %>% 
      filter(question_type == "Reason") 
    # Get total responses for referencing the percentage denominator
    resp_number <- dta %>%
      group_by(Question) %>%
      summarise("total_responses" = sum(n)) %>%
      distinct(total_responses) %>%
      pull(total_responses)
    # Calculate percentages for each response type
    discuss_perc <- dta %>%
      filter(Question == "To discuss your proposal" & value == 1) %>%
      pull(perc)
    appli_perc <- dta %>%
      filter(Question == "To make an application" & value == 1) %>%
      pull(perc)
    constr_perc <- dta %>%
      filter(Question == "During construction" & value == 1) %>%
      pull(perc)
    other_perc <- dta %>%
      filter(Question == "Other reason" & value == 1) %>%
      pull(perc)
    # If any are 0 then replace with "none"
    discuss_perc <- if_else(is_empty(discuss_perc), 
                            "none", 
                            paste0(discuss_perc, "%"))
    appli_perc <- if_else(is_empty(appli_perc), 
                          "none", 
                          paste0(appli_perc,"%"))
    constr_perc <- if_else(is_empty(constr_perc), 
                           "none", 
                           paste0(constr_perc,"%"))
    other_perc <- if_else(is_empty(other_perc), 
                          "No respondents", 
                          paste0(other_perc, "%"))
    # Paste all text together
    txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, as well as their reason for contacting the Building Standards Service in ", 
                              local_authority(),
                              ". Of the ", 
                              resp_number,
                              " respondents in ",
                              input$qrtr_selection,
                              " ",
                              fin_yr(),
                              ", ",
                              discuss_perc, 
                              " contacted the local authority to discuss their proposal before applying for a building warrant, ",
                              appli_perc, 
                              " were making an application for a warrant and ", 
                              constr_perc, 
                              " contacted the service during construction. ",
                              other_perc, 
                              " contacted the service for some other reason.")
    txt_respondents
  })
  
  # Report download tab (KPO4 over time)-------------------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Graph output for performance over time 
  output$ovrPerfLine <- renderPlotly({
    dta <- report_kpo_data() %>%
      filter(Quarter != "Year to Date")
    
    # Set the council values as a factor so the data can be arranged to have the 
    # council first regardless of alphabetical order
    dta$id <- factor(dta$id, levels = c(local_authority(), "Scotland"))
    # Add Financial year to quarter labels
    dta <- dta %>%
      mutate(Quarter = str_replace(Quarter, "Quarter ", "Q")) %>%
      mutate(Label = paste(Quarter, `Financial Year`))
    # # Arrange the data to set the order of colours
    dta <- dta %>%
      arrange(`Financial Year`, Quarter, id)
    # Set the date labels as a factor to ensure they stay in order
    dta$Label <- factor(dta$Label, levels = unique(dta$Label))
    
    plot <- ggplot(data = dta) +
      geom_line(aes(x = Label,
                    y = KPO_score, 
                    group = id, 
                    colour = id,
                    text = paste(id,
                                 paste("Quarter:", Label),
                                 paste("KPO 4 Score:", KPO_score),
                                 sep = "\n")),
                lwd = 1) +
      scale_color_manual(values = c("cadetblue3", "dimgrey"), name = "") +
      ggtitle("KPO 4 score - over time") +
      ylim(0, 10) +
      xlab("") +
      ylab("KPO 4 Score") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    ggplotly(plot, tooltip = "text")

  })
  
  # Render text for quarter by quarter performance   
  output$quarter_text <- renderText({
    
    # Filter data to selected council and quarters only
    dta <- report_kpo_data() %>%
      filter(Quarter != "Year to Date" & id == local_authority()) %>%
      # Add column with quarter & financial years combined
      mutate(Label = str_replace(Quarter, "Quarter ", "Q")) %>%
      mutate(Label = paste(Label, `Financial Year`))
    
    # Set value for current quarter - either quarter selected, or if YTD is
    # selected, use most recent quarter in the selected year
    current_quarter <- if(input$qrtr_selection == "Year to Date") {
      max_qrtr <- dta %>% 
        filter(`Financial Year` == fin_yr())
        max(max_qrtr$Quarter)
    } else {
      input$qrtr_selection
    }
                               
    # Store KPO score for currently selected quarter and year
    current_value <- dta %>%
      filter(Quarter == current_quarter &
               `Financial Year` == fin_yr()) %>%
      pull(KPO_score)
    
    # Find previous quarter name and value
    prev_quarter <- dta %>%
      arrange(`Financial Year`) %>%
      mutate("prev_quarter" = lag(Label)) %>%
      filter(Quarter == current_quarter & `Financial Year` == fin_yr()) %>%
      pull(prev_quarter)
    
    prev_quarter_value <- dta %>%
      filter(Label == prev_quarter) %>%
      pull(KPO_score)
    
    # Create comparison of KPO4 for selected quarter with previous quarter available
    quarter_diff <- current_value - prev_quarter_value
    quarter_diff_text <- if_else(quarter_diff < 0, "lower", "higher")
    quarter_diff <- round(abs(quarter_diff), 1)
    
    # Find equivalent quarter in previous year (if available)
    prev_year_quarter <- dta %>%
      filter(Quarter == current_quarter) %>%
      arrange(`Financial Year`) %>%
      mutate("prev_year_quarter" = lag(Label)) %>%
      filter(`Financial Year` == fin_yr()) %>%
      pull(prev_year_quarter)
    
    prev_year_quarter_value <- dta %>%
      filter(Label == prev_year_quarter) %>%
      pull(KPO_score)
    
    prev_year_quarter <- prev_year_quarter %>%
      str_replace(., "Q[0-9]\\ ", "")

    # Create comparison of KPO4 for selected quarter with equivalent quarter
    # from previous year (if available)
    year_quarter_diff <- current_value - prev_year_quarter_value
    year_quarter_diff_text <- if_else(year_quarter_diff < 0, "lower", "higher")
    year_quarter_diff <- round(abs(year_quarter_diff), 1)
    
    # Create standard text
    main_text <- paste0("In ", 
                        current_quarter, 
                        " ",
                        fin_yr(), 
                        " performance for KPO4, calculated across all responses for all questions, was ",
                        current_value,
                        " for ",
                        local_authority(),
                        ".")
    
    # Create text for previous quarter comparison
    prev_quarter_text <- paste0("This is ",
                                quarter_diff,
                                " points ",
                                quarter_diff_text,
                                " than the previous quarter performance of ",
                                prev_quarter_value,
                                " in ",
                                prev_quarter,
                                ".")
    
    # Create text for equivalent quarter in previous year comparison
    prev_year_quarter_text <- paste0("KPO4 performance in  ",
                                     current_quarter, 
                                     " ",
                                     fin_yr(), 
                                     " was ",
                                     year_quarter_diff,
                                     " points ",
                                     year_quarter_diff_text,
                                     " than the performance of ",
                                     prev_year_quarter_value,
                                     " in the same quarter in ",
                                     prev_year_quarter,
                                     ".")
    
    # Conditional statement to select text based on whether there is a previous year to compare   
    final_text <- if_else(is.na(prev_quarter) & is.na(prev_year_quarter), 
                          main_text,
                          if_else(!is.na(prev_quarter) & is.na(prev_year_quarter),
                                  paste(main_text, prev_quarter_text),
                                  paste(main_text, 
                                        prev_quarter_text, 
                                        prev_year_quarter_text)))
    return(final_text)
  })
  
  # Report Download tab (functions for individual questions)-------------------
  # Create a function for generating formatted data for individual questions
  
  format_qstn_dta <- function(question, 
                              named_value_1, 
                              named_value_2,
                              named_value_3, 
                              named_value_4) {
    # Filter pivot_dta to selected financial year, selected quarter and remove missing values
    dta <- pivot_dta %>% 
      filter(Quarter %in% qrtr() & 
               `Financial Year` == fin_yr() &
               Indicator == question) %>%
      filter(!is.na(value))
    # Set values as numeric to keep in order
    dta$value <- as.numeric(dta$value)
    # Calculate count for question and selected LA
    qstnDta_LA <- dta %>% 
      filter(`Local Authority Name` == local_authority()) %>% 
      count(value) %>%
      mutate(Selection = local_authority())
    # Calculate count for question for all responses (Scotland) - bind LA count
    qstnDta <- dta %>% 
      count(value) %>%
      mutate(Selection = "Scotland") %>%
      rbind(qstnDta_LA )
    # Get response percentages for LA and Scotland 
    qstnDta <- qstnDta %>% 
      group_by(Selection) %>% 
      mutate(perc_resp = round((n / sum(n)) * 100, 1))
    # Set labels for tickmarks to display on x axis
    # Different responses for different questions so needs to be set accordingly
    qstnDta <- qstnDta %>%
      mutate(named_value = str_replace_all(value, 
                                           c( "1" = named_value_1,
                                              "2" = named_value_2,
                                              "3" = named_value_3,
                                              "4" = named_value_4)))
    # Set the council values as a factor so the data can be arranged to 
    # have the council first regardless of alphabetical order
    qstnDta$Selection <- factor(qstnDta$Selection, 
                                levels = c(local_authority(), "Scotland"))
    # Arrange the data so the colours will be in order
    qstnDta <- qstnDta %>% 
      arrange(value, Selection) 
    qstnDta
  } 
  
  # Create a function for generating plot for individual questions
  
  create_qstn_plot <- function(data, title) {
    plot <- ggplot(data = data) +
      geom_bar(aes(x = fct_reorder(named_value, value),
                   y = perc_resp,
                   fill = Selection,
                   text = paste(Selection, 
                                paste("Response:", named_value), 
                                paste("% of Responses:", perc_resp),
                                sep = "\n")), 
               stat = "identity", 
               position = "dodge",
               width = 0.7, 
               colour = "black") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = c("cadetblue3", "dimgrey"), name = "") +
      ggtitle(paste(title, input$qrtr_selection, fin_yr())) +
      xlab("Responses") +
      ylab(paste("% of Responses", input$qrtr_selection, fin_yr())) +
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
    qstnDta_LA <- data %>%
      filter(Selection == local_authority())
    qstnDta_scot <- data %>%
      filter(Selection == "Scotland")
    # Get total percentage positive 
    total_good <- qstnDta_LA  %>%
      filter(value %in% c(1,2)) %>% 
      pull(perc_resp) %>%
      sum()
    # If this is above 55% then overall is positive, 
    # If less than 45% negative, otherwise balanced
    pos_or_neg <- if_else(total_good > 55, 
                          "mainly positive,", 
                          if_else(total_good < 45, 
                                  "mainly negative,", 
                                  "balanced,"))
    # Add "only" to the % positive if this is below 45
    if (total_good < 45) {
      total_good <- paste("only", total_good)
    } else {
      total_good <- total_good
    }
    
    # Get the name for the maximum value in LA dataset. If more than one 
    # paste these together
    max_name <- qstnDta_LA %>% 
      filter(perc_resp == max(perc_resp)) %>% 
      pull(named_value)
    # Get the percentage for the highest response and paste together if multiple
    max_perc <- qstnDta_LA %>% 
      filter(named_value %in% max_name) %>% 
      distinct(perc_resp) %>%
      pull(perc_resp)
    if (length(max_name) > 1) {
      max_perc <- paste(max_perc, "percent respectively.")
    } else {
      max_perc <- paste0(max_perc, " percent.")
    }
    if (length(max_name > 1)) {
      max_name <- paste(max_name, collapse = " & ")
    }
    # Get second highest value
    sec_val <- sort(unique(qstnDta_LA$perc_resp), decreasing = TRUE)[2]
    # Filter for second highest value's name
    sec_name <- qstnDta_LA %>%
      filter(perc_resp == sec_val) %>% 
      pull(named_value)
    # Create text, accounting for more than one same value
    if (length(sec_name) > 1) {
      sec_perc <- paste(sec_val, "percent respectively.")
    } else {
      sec_perc <- paste0(sec_val, " percent.")
    }
    if (length(sec_name) > 1) {
      sec_name <- paste(sec_name, collapse = " & ")
    }
    # Get most frequent response for Scotland
    scot_max_name <- qstnDta_scot %>% 
      filter(perc_resp == max(perc_resp)) %>% 
      pull(named_value)
    if (length(scot_max_name) > 1) {
      scot_max_name <- paste(scot_max_name, collapse = " & ")
    }
    # Get percentage for most frequent Scotland level response
    scot_max_perc <- qstnDta_scot %>% 
      filter(perc_resp == max(perc_resp)) %>% 
      distinct(perc_resp) %>%
      pull(perc_resp)
    if (length(scot_max_name) > 1) {
      scot_max_perc <- paste(scot_max_perc, "percent respectively.")
    } else {  
      scot_max_perc <- paste0(scot_max_perc, " percent.")
    }
    
    # Paste the text together
    main_text <- paste0("In ",
                       input$qrtr_selection,
                       " ",
                       fin_yr(),
                       ", in relation to the statement \"",
                       question,
                       "\" responses for ",
                       local_authority(),
                       " have been ",
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
                       scot_max_perc)
    
    # Text when all responses are the same
    other_text <- paste0("In ",
                        input$qrtr_selection,
                        " ",
                        fin_yr(),
                        ", in relation to the statement \"",
                        question,
                        "\" responses for ",
                        local_authority(),
                        " have been ",
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
                        " For Scotland overall, most respondents said that ",
                        extra_text,
                        " ",
                        scot_max_name,
                        " at ", 
                        scot_max_perc)
    
    # Test whether there's more than one different type of response
    text <- if_else(is.na(sec_val), other_text, main_text)
  }
  
  # Report Download tab (Q1 - Time taken)--------------------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_time_data_report <- reactive({
    format_qstn_dta(question = "How satisfied were you with the time taken?",
                    named_value_1 = "very satisfied",
                    named_value_2 = "satisfied",
                    named_value_3 = "dissatisfied",
                    named_value_4 = "very dissatisfied")
  })
  
  # Render plot 
  output$question_time_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_time_data_report(),
                     title = "Satisfaction with time taken -" )
  })
  
  # Render text
  output$question_time_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_time_data_report(),
                     question = "How satisfied were you with the time taken?",
                     named_value_1 = "very satisfied",
                     named_value_2 = "satisfied",
                     extra_text = "they were")
  })
  
  # Report download tab (Q2 - Standard of communication)-----------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_comms_data_report <- reactive({
    format_qstn_dta(question = "How would you rate the standard of communication?",
                    named_value_1 = "very good",
                    named_value_2 = "good",
                    named_value_3 = "poor",
                    named_value_4 = "very poor")
  })
  
  # Render plot 
  output$question_comms_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_comms_data_report(),
                     title = "Standard of communication -" )
  })
  
  # Render text
  output$question_comms_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_comms_data_report(),
                     question = "How would you rate the standard of communication?",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was")
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
                    named_value_4 = "very poor")
  })
  
  # Render plot 
  output$question_info_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_info_data_report(),
                     title = "Quality of information -")
  })
  # Render text
  output$question_info_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_info_data_report(),
                     question = "Quality of the information provided",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was")
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
                    named_value_4 = "very poor")
  })
  
  # Render plot 
  output$question_staff_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_staff_data_report(),
                     title = "Service offered by staff -")
  })
  
  # Render text
  output$question_staff_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_staff_data_report(),
                     question = "Service offered by staff",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was")
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
                    named_value_4 = "very poor")
  })
  
  # Render plot 
  output$question_responsiveness_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_responsiveness_data_report(),
                     title = "Responsiveness to queries or issues -")
  })
  
  # Render text
  output$question_responsiveness_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_responsiveness_data_report(),
                     question = "Responsiveness to any queries or issues raised",
                     named_value_1 = "very good",
                     named_value_2 = "good",
                     extra_text = "it was")
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
                    named_value_4 = "strongly disagree")
  })
  
  # Render plot 
  output$question_fair_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_fairly_data_report(),
                     title = "Would you agree you were treated fairly -")
  })
  
  # Render text
  output$question_fair_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_fairly_data_report(),
                     question = "To what extent would you agree that you were treated fairly?",
                     named_value_1 = "strongly agree",
                     named_value_2 = "agree",
                     extra_text = "they")
  })
  
  # Report download tab (Q7 - Overall satisfaction)---------------------------
  # Note - data has to be created in a reactive function, seperate from the 
  # plot function, so the data can be used in the markdown document
  
  # Call function to generate data to be used in graph and text
  question_overall_data_report <- reactive({
    format_qstn_dta(question = "How satisfied were you overall?",
                    named_value_1 = "very satisfied",
                    named_value_2 = "satisfed",
                    named_value_3 = "dissatisfied",
                    named_value_4 = "very dissatisfied")
  })
  
  # Render plot 
  output$question_overall_report <- renderPlotly({
    # Call function to create plot
    create_qstn_plot(data = question_overall_data_report(),
                     title = "Overall satisfaction -"
    )
  })    
  
  # Render text
  output$question_overall_report_text <- renderText({
    # Call function to create text
    create_qstn_text(data = question_overall_data_report(),
                     question = "How satisfied were you overall?",
                     named_value_1 = "very satisfied",
                     named_value_2 = "satisfied",
                     extra_text = "they were")
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
                     year = fin_yr(),
                     quarter = qrtr(),
                     named_qrtr = input$qrtr_selection,
                     kpo_data = report_kpo_data(),
                     type_data = report_type_data(),
                     reason_data = report_reason_data(),
                     respondent_data = resp_dta(),
                     time_data = question_time_data_report(),
                     comms_data = question_comms_data_report(),
                     info_data = question_info_data_report(),
                     staff_data = question_staff_data_report(),
                     responsive_data = question_responsiveness_data_report(),
                     fair_data = question_fairly_data_report(),
                     overall_data = question_overall_data_report())
      
      # Knit the markdown document, passing in the `params` list, and eval 
      # it in a child of the global environment (this isolates the code in 
      # the document from the code in this app).
      output <- rmarkdown::render(input = tempReport,
                                  output_format = "pdf_document",
                                  params = params, 
                                  envir = new.env(parent = globalenv()))
      file.copy(output, file, overwrite = TRUE)
    }
  )
  
  # Data download tab (Data download button)-----------------------------------
  # Create excel download
  output$all_data_dl <- downloadHandler(
    filename = paste0("All_Data", ".csv"),
    content = function(file) {
      dl_all_data <- dl_all_data()
      council_fltr <- local_authority()
      
      dl_all_data <- dl_all_data %>% arrange(desc(`Ended date`))
      
      # Reorder columns so that submission date moves to start
      dl_all_data <- dl_all_data %>% rename("Submission date" = "Ended date")
      dl_all_data <- dl_all_data[c(ncol(dl_all_data), 1:(ncol(dl_all_data) - 1))]
      
      # Recode all responses for the download from a number to text, remove LA column
      dl_all_data <- dl_all_data %>% 
        # Filter to selected financial year and selected quarter
        filter(Quarter %in% qrtr() & `Financial Year` == fin_yr() & `Local Authority Name` == council_fltr) %>%
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
    # Order by submission date
    unpivot_data <- unpivot_data %>% arrange(desc(`Submission date`))
    
    # Filter to selected year and quarter
    unpivot_data <- unpivot_data %>% 
      filter(Quarter %in% qrtr() & `Financial Year` == fin_yr())
    
    tbl <- datatable(unpivot_data,
                     rownames = FALSE,
                     escape = FALSE,
                     extensions = 'Scroller',
                     options = list(
                       # This shortens the text labels and makes them viewable
                       # by hovering instead
                       columnDefs = list(list(
                         targets = c(7,11,13,15,19,21,23,24),
                         render = JS(
                           "function(data, type, row, meta) {",
                           "return type === 'display' && data != null && data.length > 20 ?",
                           "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                           "}")
                       )
                       ),
                       
                       dom = "t",
                       deferRender = TRUE,
                       scrollY = "320px",
                       scrollX = TRUE,
                       scroller = TRUE
                     )
    )
  })
  
  # Open Text tab-------------------------------------------------------------
  
  # Create table to show comments for selected question 
  output$cmnt_table <- DT::renderDataTable({
    unpivot_data <- unpivot_data()
    # Need to recode "other" respondent and reason answers
    unpivot_data$`Other respondent`[unpivot_data$`Other respondent` != "No"] <- "Yes"
    unpivot_data$`Other reason`[unpivot_data$`Other reason` != "No"] <- "Yes"
    
    # Order by submission date
    unpivot_data <- unpivot_data %>% arrange(desc(`Submission date`))
    
    unpivot_data$Quarter <- as.factor(unpivot_data$Quarter)
    # store selected respondent type  
    slctn_respondent <- input$cmnts_resp_input
    # Select selected respondent reason using partial match
    slctn_reason <- input$cmnts_reason_input
    # Filter data to show comments for selected question, and respondents
    filter_data <- unpivot_data %>% 
      filter(if_any(slctn_respondent, ~ . == "Yes")) %>%
      filter(if_any(slctn_reason, ~.== "Yes")) %>%
      select(Quarter, `Financial Year`, contains(input$cmnts_slct))
    
    # Filter to selected year and quarter
    filter_data <- filter_data %>%
      filter(Quarter %in% qrtr() & `Financial Year` == fin_yr())
    
    # Create datatable
    datatable(filter_data, 
              filter = "top",
              rownames = FALSE, 
              class = "row-border",
              escape = FALSE,
              extensions = c("Scroller", "FixedColumns"),
              options = list(
                columnDefs = list(list(
                  # This shortens the text labels and makes them viewable
                  # by hovering instead
                  targets = 3,
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data != null && data.length > 100 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                    "}"))),
                dom = "t",
                deferRender = TRUE,
                scrollY = "280px",
                scroller = TRUE))
    })
  
  # Closing bracket for opening function ---------------------------------------  
}
