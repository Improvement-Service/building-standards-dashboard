function(input, output, session) {


###### Reactive global data #######  

#first, get the user  
  user <- reactive({
    "cara.connachan@improvementservice.org.uk"
  })
  
#generate ui drop down
output$la_select <- renderUI({
  user <- user()
  if(grepl("improvementservice.org.uk|gov.scot", user, ignore.case = T)){
    selectizeInput("LA_selection", "",
                   choices =LA_Names, options = list(placeholder = "Select Your Local Authority",
                                                     onInitialize = I('function() { this.setValue(""); }')))
  } else{
    return()
  }
  
  
})
  
  
##now filter data based on log in details  
    
  local_authority <- reactive({
    user <- user()
    #Aberdeen City
    if(grepl("aberdeencity.gov.uk", user,ignore.case = TRUE)){
      return("Aberdeen City")
    } else 
      #Aberdeenshire  
      if(grepl("aberdeenshire.gov.uk", user,ignore.case = TRUE)){
        return("Aberdeenshire")
      } else 
        #Angus
        if(grepl("angus.gov.uk", user,ignore.case = TRUE)){
          return("Angus")
        } else 
        #Argyll and Bute
          if(grepl("argyll-bute.gov.uk", user,ignore.case = TRUE)){
            return("Argyll and Bute")
          } else 
        #City of Edinburgh    
            if(grepl("edinburgh.gov.uk", user,ignore.case = TRUE)){
              return("City of Edinburgh")
            } else 
        #Clackmannanshire
              if(grepl("clacks.gov.uk", user,ignore.case = TRUE)){
                return("Clackmannanshire")
              } else 
          #Eilean Siar
                if(grepl("cne-siar.gov.uk", user,ignore.case = TRUE)){
                  return("Eilean Siar")
                } else 
          #Dumfries and Galloway
                  if(grepl("dumgal.gov.uk", user,ignore.case = TRUE)){
                    return("Dumfries and Galloway")
                  } else 
          ##Dundee City          
                    if(grepl("dundeecity.gov.uk", user,ignore.case = TRUE)){
                      return("Dundee City")
                    } else
          #East Ayrshire
                      if(grepl("east-ayrshire.gov.uk", user,ignore.case = TRUE)){
                        return("East Ayrshire")
                      } else
          #East Dunbartonshire              
                        if(grepl("eastdunbarton.gov.uk", user,ignore.case = TRUE)){
                          return("East Dunbartonshire")
                        } else 
         #East Lothian
                          if(grepl("eastlothian.gov.uk", user,ignore.case = TRUE)){
                            return("East Lothian")
                          } else 
        #Falkirk
                            if(grepl("falkirk.gov.uk", user,ignore.case = TRUE)){
                              return("Falkirk")
                            } else 
        #Fife
                              if(grepl("fife.gov.uk;", user,ignore.case = TRUE)){
                                return("Fife")
                              } else 
        #Glasgow                         
                                if(grepl("glasgow.gov.uk", user,ignore.case = TRUE)){
                                  return("Glasgow")
                                } else 
        #Highland
                                  if(grepl("highland.gov.uk", user,ignore.case = TRUE)){
                                    return("Highland")
                                  } else 
        #Inverclyde
                                    if(grepl("inverclyde.gov.uk", user,ignore.case = TRUE)){
                                      return("Inverclyde")
                                    } else 
        #Midlothian                              
                                      if(grepl("midlothian.gov.uk", user,ignore.case = TRUE)){
                                        return("Midlothian")
                                      } else 
        #Moray
                                        if(grepl("moray.gov.uk", user,ignore.case = TRUE)){
                                          return("Moray")
                                        } else 
        #North Ayrshire
                                          if(grepl("north-ayrshire.gov.uk", user,ignore.case = TRUE)){
                                            return("North Ayrshire")
                                          } else 
        #North Lanarkshire
                                            if(grepl("northlan.gov.uk", user,ignore.case = TRUE)){
                                              return("North Lanarkshire")
                                            } else 
        #Orkney Islands
                                              if(grepl("orkney.gov.uk", user,ignore.case = TRUE)){
                                                return("Orkney Islands")
                                              } else 
        #Perth and Kinross
                                                if(grepl("pkc.gov.uk", user,ignore.case = TRUE)){
                                                  return("Perth and Kinross")
                                                } else 
        #Renfrewshire
                                                  if(grepl("renfrewshire.gov.uk", user,ignore.case = TRUE)){
                                                    return("Renfrewshire")
                                                  } else 
        #Scottish Borders
                                                    if(grepl("scotborders.gov.uk", user,ignore.case = TRUE)){
                                                      return("Scottish Borders")
                                                    } else 
        #Shetland Islands
                                                      if(grepl("shetland.gov.uk", user,ignore.case = TRUE)){
                                                        return("Shetland Islands")
                                                      } else 
       #South Ayrshire
                                                        if(grepl("south-ayrshire.gov.uk", user,ignore.case = TRUE)){
                                                          return("South Ayrshire")
                                                        } else 
        #South Lanarkshire
                                                          if(grepl("southlanarkshire.gov.uk", user,ignore.case = TRUE)){
                                                            return("South Lanarkshire")
                                                          } else 
        #Stirling
                                                            if(grepl("stirling.gov.uk", user,ignore.case = TRUE)){
                                                              return("Stirling")
                                                            } else 
        #West Dunbartonshire
                                                              if(grepl("west-dunbarton.gov.uk", user,ignore.case = TRUE)){
                                                                return("West Dunbartonshire")
                                                              } else 
        #West Lothian
                                                                if(grepl("westlothian.gov.uk", user,ignore.case = TRUE)){
                                                                  return("West Lothian")
                                                                } else 
        #East Renfrewshire
                                                                  if(grepl("eastrenfrewshire.gov.uk", user,ignore.case = TRUE)){
                                                                    return("East Renfrewshire")
                                                                  } else{
                                                                    input$LA_selection
                                                                  } 
                                                                 
  })
  
##Return Council data is displayed for 
output$LA_KPO4_Heading <- renderUI({
  la_name <- local_authority()
  h2(paste("KPO4 Performance", la_name, sep = " - "),style = "margin-top:3px")
})
  
#### Download Data 
  
  # Select columns based on council (ensures duplicate columns and questions from other councils are filtered out)
  dl_all_data <- reactive({
  
  council_fltr = local_authority()
    
  dl_all_data <- if(council_fltr == "Angus")
  {fresh_dta[,c(7:34,92)]} else
    if(council_fltr == "City of Edinburgh")
      {fresh_dta[,c(7:21,79:91)]} else
        if(council_fltr == "North Lanarkshire")
          {fresh_dta[,c(7:34,51:55)]}else
            if(council_fltr == "Orkney Islands")
              {fresh_dta[,c(7:21,56:78)]}else
                if(council_fltr == "West Lothian")
                {fresh_dta[,c(7:21,35:50)]}else
                    {fresh_dta[,c(7:34)]}
  
  # Add in columns with Quarter Info and Financial Year info
  dl_all_data$`Tracking Link` <- as.yearqtr(dl_all_data$`Ended date`, format = "%Y-%m-%d") 
  dl_all_data$`Financial Year` <- dl_all_data$`Tracking Link`- 1/4
  dl_all_data$`Financial Year` <- gsub("\\ ", "-", dl_all_data$`Financial Year`, perl=T)
  dl_all_data$`Financial Year` <- dl_all_data %>% select(contains("Financial Year")) %>% apply(2, function(x) gsub("-Q[0-9]","",x))%>% as.numeric(.) %>%
    data.frame() %>% mutate(nxt = .+1) %>% mutate(nxt = substr(nxt,3,4)) %>% mutate(fy = paste(.,nxt, sep = "/")) %>%
    pull(fy)

  
  dl_all_data$`Tracking Link` <- dl_all_data$`Tracking Link`- 1/4
  dl_all_data$`Tracking Link` <- gsub("[0-9]*\\ Q", "Quarter ", dl_all_data$`Tracking Link`, perl = T)
  
  # Remove redundant columns and reorder
  dl_all_data <- dl_all_data[-c(1:4)]
  dl_all_data <- dl_all_data[,c((ncol(dl_all_data)-1),ncol(dl_all_data),1,10,11,2:9,12:(ncol(dl_all_data)-2))]
  
  # pivot to combine both LA columns, rename, then remove duplicates
  dl_all_data <- dl_all_data %>% pivot_longer(cols = 4:5, names_to = "extra", values_to ="LA") %>%
    filter(LA != "-") %>% select(-extra)
  dl_all_data <- dl_all_data[,c(1:3,ncol(dl_all_data),4:(ncol(dl_all_data)-1))]  
  
  # Code local authority name for councils completing survey without login
  dl_all_data <- merge(dl_all_data, LA_names_dta)
  dl_all_data$`Local Authority Name` <- dl_all_data$LA_Names
  dl_all_data <- dl_all_data %>% select(-LA_Names)
  dl_all_data <- dl_all_data[,c(2:4,1,5:ncol(dl_all_data))]
  dl_all_data
  })
  

##### Respondents and reasons data 
  
  # Generate another dataframe with respondent types
  
  resp_dta <- reactive({
    
    dl_all_data <- dl_all_data()
    council_fltr = local_authority()
    
    # Recode "other" respondents and reasons so it doesn't show text value
    dl_all_data$`Q1.4. Other (please specify):`[dl_all_data$`Q1.4. Other (please specify):` != "0"] <- "1"
    dl_all_data$`Q2.4. Other (please specify):`[dl_all_data$`Q2.4. Other (please specify):` != "0"] <- "1"
    # Change these columns to numeric so they can be combined with the other columns
    dl_all_data$`Q1.4. Other (please specify):` <- as.numeric(dl_all_data$`Q1.4. Other (please specify):`)
    dl_all_data$`Q2.4. Other (please specify):` <- as.numeric(dl_all_data$`Q2.4. Other (please specify):`)
    
    
 resp_dta <- dl_all_data %>% group_by(`Local Authority Name`) %>% select(1:12)%>%
    pivot_longer(cols = 5:12, names_to = "Question", values_to = "value")%>% 
    group_by(`Local Authority Name`,Question) %>%
    count(value) %>%
    mutate(perc = n/sum(n))
  
  ##Tidy the respondent types and reasons
  resp_dta$question_type <- ifelse(grepl("Q1", resp_dta$Question), "Type", "Reason")
  
  ##Remove question numbers
  resp_dta$Question <- gsub("Q[\\.1-9]+\\s", "", resp_dta$Question,perl = T)
  
  ##Remove "(please specify):"
  resp_dta$Question[resp_dta$Question == "Other (please specify):"] <- "Other"
  
  ##Filter to selected council
  resp_dta <- resp_dta%>%filter(`Local Authority Name` == council_fltr)
  
  

  resp_dta
  
  })
  
### Final reactive steps to create unpivot_data
  
  unpivot_data <- reactive({
  
    council_fltr <- local_authority()
    
  # Select columns based on council (ensures duplicate columns and additional questions are filtered out)
  unpivot_data <- if(council_fltr == "City of Edinburgh")
  {unpivot_data_global[,c(1:12,70:82)]} else
    if(council_fltr == "Orkney Islands")
    {unpivot_data_global[,c(1:12,47:50,56:58,63,65:69)]}else
      if(council_fltr == "West Lothian")
      {unpivot_data_global[,c(1:12,26:30,34:41)]}else
      {unpivot_data_global[,c(1:25)]}
  
  #tidy up question names
  unpivot_data <- unpivot_data %>% 
    rename("Quarter" = "Tracking Link") %>%
    rename("Q3. How satisfied were you with the time taken?" = "Q3. Thinking of your engagement with [question(16082428)][variable(la)] Building Standards from beginning to end, how satisfied were you that the time taken to deal with your application or enquiry met the timescales that you were promised?") %>%
    rename("Q4. How would you rate the standard of communication?" = "Q4. How would you rate the standard of communication provided by [question(16082428)][variable(la)] Building Standards service following your initial contact or once your application had been submitted?") %>%
    rename("Q.3. Responsiveness to any queries or issues raised" = "Q.3. Time taken to respond to any queries or issues raised") %>%
    rename("Q5. To what extent would you agree that you were treated fairly" = "Q5. To what extent would you agree that you were treated fairly by [question(16082428)][variable(la)] Building Standards?") %>%
    rename("Q6. How satisfied were you, overall?" = "Q6. Overall, how satisfied were you with the service provided by [question(16082428)][variable(la)] Building Standards?")%>%
    rename("Q1.4. Other respondent" = "Q1.4. Other (please specify):") %>%
    rename("Q2.4. Other reason" = "Q2.4. Other (please specify):") %>%
    mutate(across(contains(c("Q1.1. Agent/Designer", 
                             "Q1.2. Applicant", 
                             "Q1.3. Contractor",
                             "Q1.4. Other respondent",
                             "Q2.1. To discuss your proposal",
                             "Q2.2. To make an application", 
                             "Q2.3. During construction",
                             "Q2.4. Other reason")),
                  ~recode(., "1" = "Yes", 
                          "0" = "No"))) %>%
    select(-LA)
  
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
  unpivot_data <- unpivot_data %>% filter(`Local Authority Name` == council_fltr)
  
  unpivot_data
  })
  
##Create outputs for KPO4 Summary Page===========================  
  
##calculated the KPO4 score based on weighted responses
  KPOdta <- dta %>% mutate(value = as.numeric(value)) %>% mutate(KPO_weight = value-1)
  KPOweights_multiplier <- outer(dta$Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?",2)+
    outer(dta$Indicator == "How would you rate the standard of communication provided?",2) +
    outer(dta$Indicator == "Overall, how satisfied were you with the service provided?",8)
  KPOweights_multiplier <- replace(KPOweights_multiplier, KPOweights_multiplier==0, 1)
  
  KPOdta$KPO_weight <- KPOdta$KPO_weight * KPOweights_multiplier  
  
  ##get total and max values for Scotland and Selected LA by quarter
  scot_max <- dta %>% mutate(maxAvailable = 4) %>% mutate(value = as.numeric(value))
  scot_max$maxAvailable <- (scot_max$maxAvailable-1) * KPOweights_multiplier 
  scot_max[is.na(scot_max$value), "maxAvailable"] <- NA
  scot_max$KPO4_weighted <- (scot_max$value-1) * KPOweights_multiplier
  ##calculate KPO4 for quarter for all results   
  scot_max_sum <- scot_max %>% group_by(`Tracking Link`, `Financial Year`) %>%
    summarise(across(c(maxAvailable,KPO4_weighted),sum, na.rm = T)) %>% 
    group_by(`Financial Year`) %>%
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))  %>%
    ##generate the KPO score (out of 10)    
    mutate(KPO_score = round((1-KPO4_weighted/maxAvailable)*10,1))
  
  
  ##calculate KPO4 for quarter for selected local authority    
  la_max_sum <- reactive({
    
    council_fltr <- local_authority()
    
    la_max_sum <- scot_max %>% filter(`Local Authority Name` == council_fltr) %>%     
    group_by(`Tracking Link`, `Financial Year`) %>%
    summarise(across(c(maxAvailable,KPO4_weighted),sum, na.rm = T)) %>% 
    group_by(`Financial Year`) %>%
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))  %>%
  ##generate the KPO score (out of 10)    
    mutate(KPO_score = round((1-KPO4_weighted/maxAvailable)*10,1))
  })
  
  
  ##Create KPO4 download for all LA's
  
  total_la_max_sum <- scot_max %>%      
    group_by(`Local Authority Name`,`Tracking Link`, `Financial Year`) %>%
    summarise(across(c(maxAvailable,KPO4_weighted),sum, na.rm = T)) %>% 
    ##generate the KPO score (out of 10)    
    mutate(KPO_score = round((1-KPO4_weighted/maxAvailable)*10,1)) %>%
    rbind(scot_max_sum) %>%
    select(-maxAvailable, -KPO4_weighted) %>%
    rename(Area = `Local Authority Name`) %>%
    rename(Quarter = `Tracking Link`) %>%
    rename(`KPO4 Score` = KPO_score)
  
  total_la_max_sum$Quarter <- recode(total_la_max_sum$Quarter, "Total" = "Year to Date")
  
  total_la_max_sum$Area[is.na(total_la_max_sum$Area)] <- "Scotland"
  
  total_la_max_sum <- total_la_max_sum %>% arrange(`Financial Year`, Quarter)
  
  # Create download button for KPO 4 data
  output$KPO_data_file <- downloadHandler(
    filename = paste("KPO4_Data", ".csv", sep = ""),
    content = function(file) {
      write.csv(total_la_max_sum, file)
    }
  )
  
  # Create conditionality to only show download button if IS or SG
  output$KPO_data_dl <- renderUI({
    user <- user()
    if(grepl("improvementservice.org.uk|gov.scot", user, ignore.case = T)) {
      downloadBttn("KPO_data_file", label = "Download KPO4 Data", style = "jelly", size = "sm")
    }else{
      return()
    }
  })
  
#Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    la_max_sum <- la_max_sum()
    kpo_colr <- ifelse(
      la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, "KPO_score"] > 7.5, 
      "green", 
      ifelse(la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, "KPO_score"] < 6.5, 
             "red", 
             "orange"
             )
      )
    valueBox(
      value = round(la_max_sum[la_max_sum$`Tracking Link` =="Total" & la_max_sum$`Financial Year` == fin_yr, "KPO_score"],1), 
      "Council KPO4 YTD", 
      icon = icon("chart-bar"), 
      color = kpo_colr
    )
  })
#Create performance box for Scotland
  output$scotPerfBox<- renderValueBox({
   valueBox(
      value = round(scot_max_sum[scot_max_sum$`Tracking Link` =="Total" & scot_max_sum$`Financial Year` == fin_yr, "KPO_score"],1), 
      "Scotland Average", 
      icon = icon("times"), 
      color = "navy"
    )
  })
#Create responses valuebox
    output$respBox <- renderValueBox({
      unpivot_data <- unpivot_data()
      valueBox(
        value = paste(nrow(
          filter(unpivot_data, Quarter == crnt_qtr & `Financial Year` == fin_yr)
          ), 
          "Responses"
          ), 
        paste(nrow(
          filter(unpivot_data, `Financial Year` == fin_yr)
          ),
          "Year to Date"
          ), 
        icon = icon("user-friends"), 
        color = "light-blue"
      )
    })
    
##Create bar plot for overall performance
    output$ovrPerfBar <- renderPlotly({
      la_max_sum <- la_max_sum()
      
      #rename Total as year to date
      la_max_sum$`Tracking Link` <- recode(la_max_sum$`Tracking Link`, "Total" = "Year to Date")
      
      la_max_sum <- la_max_sum %>% filter(`Financial Year` == fin_yr)
     
        ##Set colours for quarter by kpo4
      kpo_clrs <- la_max_sum %>% 
        filter(`Tracking Link` != "Year to Date") %>% 
        pull(KPO_score)
      clrs <- ifelse(kpo_clrs >7.5, "forestgreen", ifelse(kpo_clrs <6.5, "firebrick", "darkorange"))
      
      p <- ggplot(data = la_max_sum) +
         geom_bar(aes(
           x = `Tracking Link`, 
           y = KPO_score,
           text = paste(
             paste("Quarter:", `Tracking Link`),
             paste("KPO 4 Score", KPO_score),
             sep = "\n"
           )
           ), 
           stat = "identity",
           position = "dodge", 
           fill = c(clrs, "grey13"), 
           width = 0.7, 
           colour = "black"
           ) +
         theme_classic() +
         scale_y_continuous(limits = c(0,10), expand = expansion(mult = c(0, 0.1)))+
         ggtitle("KPO4 performance by quarter and YTD")+
         ylab("KPO 4 Score") +
         xlab("Response period") +
         theme(axis.text.x = element_text(size = 12),
               axis.title = element_text(size = 13))
      ggplotly(p, tooltip = "text")
    })
    
##Create barplots for respondent types and reasons---
    
  ##n.b. I am using the same output twice, in the UI, but this is not allowed, so this
    #is the suggested workaround i.e. assign the output twice
    
    #Create data for response type
    report_type_data <- reactive({
      
      resp_dta <- resp_dta()
      
      
      pc_resp_data <- resp_dta %>% filter(., question_type == "Type" & value == 1)
      pc_resp_data$perc <- round(pc_resp_data$perc * 100, 1)
      pc_resp_data
    })
    
    output$resp_type_graph_report <- output$respDoughnut <- renderPlotly({
  ##select data    
      report_type_data <- report_type_data()
      
      pfig <- ggplot(data = report_type_data) +
        geom_col(aes(x = Question, y = perc,
                 text = paste(
                   paste("Respondent Type:", report_type_data$Question),
                   paste("% of responses:", report_type_data$perc),
                   sep = "\n"
                 )
                 ),
                 fill = "cadetblue3", 
                 colour = "black"
                 )+
        coord_flip() +
        theme_classic()+
        scale_y_continuous( expand = expansion(mult = c(0, 0.1)))+
        ggtitle("Respondent Type: YTD")+
        xlab("Respondent Type")+
        ylab("Percentage of Responses")+
        theme(plot.title = element_text(size = 12))
        
      ggplotly(pfig, tooltip = "text")
      
    })
    
## Create data for response reason
    report_reason_data <- reactive({
      
      resp_dta <- resp_dta()
      
      pc_resp_data <- resp_dta %>% filter(., question_type == "Reason" & value == 1)
      pc_resp_data[pc_resp_data$Question == "During construction, including submission of a completion certificate", "Question"] <-"During construction" 
      pc_resp_data[pc_resp_data$Question == "To discuss your proposal before applying for a building warrant", "Question"] <-"Discuss proposal" 
      pc_resp_data[pc_resp_data$Question == "To make an application for a building warrant", "Question"] <-"Make application" 
      
      pc_resp_data$perc <- round(pc_resp_data$perc * 100, 1)
      pc_resp_data
    })
    
    ##Create plot for respondent reason for contacting
    output$resp_reason_graph_report <- output$plotly_pie <- renderPlotly({
      ##select data   

      report_reason_data <- report_reason_data()
      
       pfig <- ggplot(data = report_reason_data()) +
        geom_col(aes(
          x = Question, 
          y = perc,
          text = paste(
            paste("Reason:", report_reason_data$Question),
            paste("% of responses:", report_reason_data$perc),
            sep = "\n"
          )
          ),fill = "cadetblue3", colour = "black")+
        coord_flip() +
        theme_classic()+
        scale_y_continuous( expand = expansion(mult = c(0, 0.1)))+
         ggtitle("Response Reason:YTD")+
         xlab("Reason")+
         ylab("Percentage of Responses")+
         theme(plot.title = element_text(size = 12))
      ggplotly(pfig, tooltip = "text")
    })
    
##Create graphs to display results by questions================================
  
    ##Create filtered dataset from checkboxes
    qstn_dataset_filtered <- reactive({
      council_fltr <- local_authority()
      
      names(dta) <- gsub("Q[1-9\\.]+\\s","",names(dta), perl = T)
      dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
      dta <- dta %>% filter(`Local Authority Name` == council_fltr)
    ##select applicant type  
      slctn_respondent <- input$Qs_resp_input
    ##select applicant reason using partial match
      slctn_reason <- names(select(dta, contains(input$Qs_reason_input)))
      filter_data <- dta %>% filter(if_any(slctn_respondent, ~ . == 1)) %>%
        filter(if_any(slctn_reason, ~.==1))
      filter_data
    })
    
    #First graph for full breakdown of responses in year to date  
   output$YTDqstsPlot <- renderPlotly({
     
     filt_data <- qstn_dataset_filtered()
     
    ##if a question is selected filter the dataset
   if(input$Qstn_tab2 == "All Questions"){
       qstnDta <- filt_data %>% filter(value != "-")
       qstnDta$value <- factor(qstnDta$value,levels = c(1,2,3,4))
     }else{
       qstnDta <-filter(filt_data, Indicator == input$Qstn_tab2) %>% filter(value != "-")
       qstnDta$value <- factor(qstnDta$value,levels = c(1,2,3,4))
            }
     qstnDta <- qstnDta %>% count(value, .drop = F)
     
     ##set  tickmarks to display on x axis
     if(input$Qstn_tab2 == "All Questions"){
     qstnDta$named_value <- recode(qstnDta$value, "1" = "Very good/very satisfied/strongly agree",
                                   "2" ="good/satisfied/agree",
                                   "3" = "poor/dissatisfied/disagree",
                                   "4" = "Very poor/very dissatisfied/strongly disagree")
     } else if(input$Qstn_tab2 =="Overall, how satisfied were you with the service provided?"|input$Qstn_tab2 =="Thinking of your engagement, how satisfied were you with the time taken to complete the process?"){
       qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                     "2" ="satisfied",
                                     "3" = "dissatisfied",
                                     "4" = "very dissatisfied")
     }else if(input$Qstn_tab2  =="To what extent would you agree that you were treated fairly?"){
       qstnDta$named_value <- recode(qstnDta$value, "1" = "strongly agree",
                                     "2" ="agree",
                                     "3" = "disagree",
                                     "4" = "strongly disagree")
     } else{
       qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                     "2" ="good",
                                     "3" = "poor",
                                     "4" = "very poor")
       qstnDta$named_value <- factor(qstnDta$named_value, levels  = c("very good", "good", "poor", "very poor"))
     }
  #generate barplot
     p <- ggplot(data = qstnDta) +
      geom_bar(aes(
        x = reorder(named_value, as.numeric(value)), 
        y = n, 
        text = paste(paste0("Response: ", named_value), paste0("Number of Responses:", n),sep = "\n")),
               stat= "identity",
               fill = "cadetblue3", 
               width = 0.7, 
               colour = "black"
               )+
       ggtitle(input$Qstn_tab2)+
       xlab("Response")+
       ylab("Number of responses")+
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       theme_classic()
    ggplotly(p, tooltip = "text")
   })
   
  #Second summary of %age Good/v.good by quarter
   output$qrtsQsplot <- renderPlotly({
  ##filter dataset based on selected question   
     qstn_dataset_filtered <- qstn_dataset_filtered()
     qstn_dataset_filtered <- qstn_dataset_filtered %>% filter(value != "-")
     if(input$Qstn_tab2 == "All Questions"){  
       qstnDta <- qstn_dataset_filtered
     }else{
         qstnDta <-filter(qstn_dataset_filtered, Indicator == input$Qstn_tab2)
       }
     
     qstnDta <- qstnDta %>%  count(`Tracking Link`,value, .drop =F)
     qstnDta <- qstnDta %>% 
       group_by(`Tracking Link`) %>%
       mutate(total_responses = sum(n)) %>%
       ungroup()%>%
       mutate(`% of responses` = round((n/total_responses*100),1))
     
     
     ##set labels for response groups
     qstnDta$Response <- qstnDta$value
     if(input$Qstn_tab2 == "All Questions"){
       qstnDta$Response<- recode(qstnDta$Response, "1" = "Very good/very satisfied/strongly agree",
                                     "2" ="good/satisfied/agree",
                                     "3" = "poor/dissatisfied/disagree",
                                     "4" = "Very poor/very dissatisfied/strongly disagree")
       qstnDta$Response <- factor(qstnDta$Response, levels  = c(
         "Very good/very satisfied/strongly agree",
         "good/satisfied/agree", 
         "poor/dissatisfied/disagree",
         "Very poor/very dissatisfied/strongly disagree"))
     } else if(input$Qstn_tab2 =="Overall, how satisfied were you with the service provided?"|input$Qstn_tab2 =="Thinking of your engagement, how satisfied were you with the time taken to complete the process?"){
      qstnDta$Response <- recode(qstnDta$Response, "1" = "very satisfied",
                                     "2" ="satisfied",
                                     "3" = "dissatisfied",
                                     "4" = "very dissatisfied")
      qstnDta$Response <- factor(qstnDta$Response, levels  = c("very satisfied",
                                                           "satisfied",
                                                           "dissatisfied",
                                                           "very dissatisfied"))
     }else if(input$Qstn_tab2  =="To what extent would you agree that you were treated fairly?"){
       qstnDta$Response <- recode(qstnDta$Response, "1" = "strongly agree",
                                     "2" ="agree",
                                     "3" = "disagree",
                                     "4" = "strongly disagree")
       qstnDta$Response <- factor(qstnDta$Response, levels  = c("strongly agree",
                                                            "agree",
                                                            "disagree",
                                                            "strongly disagree"))
     } else{
       qstnDta$Response <- recode(qstnDta$Response, "1" = "very good",
                                     "2" ="good",
                                     "3" = "poor",
                                     "4" = "very poor")
       qstnDta$Response <- factor(qstnDta$Response, levels  = c("very good", "good", "poor", "very poor"))
     }
     
     Labels <- levels(qstnDta$Response)
     
     #Change column titles in dataset to fix hover labels
     qstnDta <- qstnDta %>% rename(Quarter = `Tracking Link`)
    
     # Create plot  
     p <- ggplot(data = qstnDta ) +
         geom_bar(aes(x = Quarter, y = `% of responses`, fill = Response), 
                  stat= "identity", 
                  position = "stack",
                  width = 0.7, 
                  colour = "black"
                  ) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       ggtitle(input$Qstn_tab2)+
       xlab("")+
     #  ylab("Percentage of responses")+
         theme_classic() +
        scale_fill_manual(breaks = Labels, 
                         values = c("forestgreen", "lightgreen", "darkorange", "firebrick"),
                         name = "Responses"
                         )
     
       ggplotly(p)
   })
   
##Report page outputs =====================================

   #create data for kpo in report page   
   report_kpo_data <- reactive({
     la_max_sum <- la_max_sum()
     council_fltr <- local_authority()
     
     la_max_sum$id <- council_fltr
     scot_max_sum$id <- "Scotland" 
     
     all_kpo_dta <- rbind(scot_max_sum, la_max_sum)
     #Round figures
     all_kpo_dta$KPO_score <- round(all_kpo_dta$KPO_score,1)
     all_kpo_dta
   })
  #create plot for KPO in report page 
   output$reportKPO4Plot <- renderPlotly({
     all_kpo_data <- report_kpo_data()
     council_fltr <- local_authority()
     
     all_kpo_data <- all_kpo_data %>% filter(`Tracking Link` == "Total")
     
    # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     all_kpo_data$id <- factor(all_kpo_data$id, levels = c(council_fltr, "Scotland"))
     
     # arrange the data and store order of colours
     all_kpo_data <- arrange(all_kpo_data, id)
     
     p <- ggplot(data = all_kpo_data) +
       geom_bar(aes(
         x = `Tracking Link`, 
         y = KPO_score, 
         fill = id,
         text = paste("Year to date", id, paste("KPO 4 Score:", KPO_score),sep = "\n")), 
         stat = "identity",
         position = "dodge",
         width = 0.7, 
         colour = "black")+
       scale_y_continuous(limits = c(0,10), expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual(values = c("cadetblue3","dimgrey"), name = "")+ 
       ggtitle("KPO 4 score - Year to Date")+
       xlab("")+
       ylab("KPO 4 Score")+
       theme_classic()+
       theme(axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
     
     ggplotly(p, tooltip = "text")
     
     })
   
   
   ##Render text for KPO4 Overall perf to year
   output$KPO4_text_report <- renderText({
  ##Only select the full year data - 
    ##this will need to be updated when more than one year is available  
     council_fltr <- local_authority()
     all_kpo_data <- report_kpo_data() %>% filter(`Tracking Link` == "Total")
     local_auth <- council_fltr
     curr_year <- fin_yr
     KPO4_ytd <- all_kpo_data %>% filter(id == council_fltr) %>% pull(KPO_score)
     hilow_kpo4 <- ifelse(KPO4_ytd > 7.5, "higher than", ifelse(KPO4_ytd < 7.5, "lower than", "equal to"))
     scotAv_kpo4 <- all_kpo_data %>% filter(id == "Scotland") %>% pull(KPO_score)
     abbel_kpo4 <- ifelse(KPO4_ytd > scotAv_kpo4, "higher than",ifelse(KPO4_ytd < scotAv_kpo4, "lower than", "equal to"))
     
     text_kpo <- paste0("This indicator summarises performance across all questions, with differential
                       weightings based on importance. For ", local_auth," in ",curr_year, " overall
                       performance is at ", KPO4_ytd, " for the year to date. ", "This is ",abbel_kpo4,
                       " the Scotland average of ", scotAv_kpo4," and ", hilow_kpo4," the target value
                       of 7.5.")
     return(text_kpo)
   })
   
  ##Text for respondent types 
   output$respondent_type_text_report <- renderText({
     resp_dta <- resp_dta()
     council_fltr <- local_authority()
     
    # resp_dta_filter <- resp_dta %>% filter(`Local Authority Name` == council_fltr & question_type == "Type") 
     resp_dta_filter <- resp_dta %>% filter(question_type == "Type")
     #get total responses for using as percentage denominator
     resp_number <- resp_dta_filter %>% ungroup() %>% filter(Question == "Agent/Designer") %>% summarise_at(vars(`n`), sum) %>%
       select(`n`)
     #create variables for percentages for different groups
     agent_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Agent/Designer" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     appli_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Applicant" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     contr_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Contractor" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     other_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Other" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     #if any are 0 then replace with "none"
     agent_perc <-ifelse(isEmpty(agent_perc), "none", paste0(agent_perc,"%"))
     appli_perc <-ifelse(isEmpty(appli_perc), "none", paste0(appli_perc,"%"))
     contr_perc <-ifelse(isEmpty(contr_perc), "none", paste0(contr_perc,"%"))
     other_perc <- ifelse(isEmpty(other_perc), "No respondents", paste0(other_perc, "%"))
      #paste all text together
     txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, 
     as well as their reason for contacting the Building Standards Service in ", council_fltr,". ",
     "Of the ", resp_number, " respondents ", agent_perc, " were agents or designers, ", appli_perc, "
     were applicants and ", contr_perc, " were contractors. ", other_perc, " said they were an other respondent type.")
     txt_respondents
   })
   
   ##Text for respondent reason
   output$respondent_reason_text_report <- renderText({
     resp_dta <- resp_dta()
     council_fltr <- local_authority()
    
     #resp_dta_filter <- resp_dta %>% filter(`Local Authority Name` == council_fltr & question_type == "Reason") 
     resp_dta_filter <- resp_dta %>% filter(question_type == "Reason") ##filter by LA
     ##Get a total no. of respondents for working out percentages
     resp_number <- resp_dta_filter %>% ungroup() %>% filter(Question == "To make an application for a building warrant") %>% summarise_at(vars(`n`), sum) %>%
       select(`n`)
     #Calculate percentages for each response type
     discuss_perc <- round(resp_dta_filter[resp_dta_filter$Question == "To discuss your proposal before applying for a building warrant" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     appli_perc <- round(resp_dta_filter[resp_dta_filter$Question == "To make an application for a building warrant" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     constr_perc <- round(resp_dta_filter[resp_dta_filter$Question == "During construction, including submission of a completion certificate" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     other_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Other" & resp_dta_filter$value == 1 ,"perc"] *100, 1) %>% pull(perc)
     
     #if any are 0 then replace with "none"
     discuss_perc <-ifelse(isEmpty(discuss_perc), "none", paste0(discuss_perc,"%"))
     appli_perc <-ifelse(isEmpty(appli_perc), "none", paste0(appli_perc,"%"))
     constr_perc <-ifelse(isEmpty(constr_perc), "none", paste0(constr_perc,"%"))
     other_perc <- ifelse(isEmpty(other_perc), "No respondents", paste0(other_perc, "%"))
     
     #paste all text together
     txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, 
     as well as their reason for contacting the Building Standards Service in ", council_fltr,". ",
     "Of the ", resp_number, " respondents ", discuss_perc, " contacted the local authority to discuss their proposal before applying for a building warrant, ",
      appli_perc, " were making an application for a warrant and ", constr_perc, " contacted the service during construction. ",
     other_perc, " contacted the service for some other reason.")
     txt_respondents
   })
   
   
#Create data for performance over time graph
   report_line_data <- reactive({
     la_max_sum <- la_max_sum()
     council_fltr <- local_authority()
     
     scot_max_sum$LA <- "Scotland"
     la_max_sum$LA <- council_fltr
     
     quarts_dta <- rbind(scot_max_sum,la_max_sum) %>% filter(`Tracking Link` != "Total")
     quarts_dta$KPO_score <- round(quarts_dta$KPO_score,1)
     quarts_dta
   })
   
   
   #Graph output for performance over time 
   output$ovrPerfLine <- renderPlotly({
     
     report_line_data <- report_line_data()
     council_fltr <- local_authority()
     
     if(length(report_line_data$`Tracking Link`[report_line_data$LA == council_fltr]) > 1){
       
    # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
      report_line_data$LA <- factor(report_line_data$LA, levels = c(council_fltr, "Scotland"))
       
       # arrange the data and store order of colours
       report_line_data <- arrange(report_line_data,LA)

     plt <- ggplot(data = report_line_data) +
       geom_line(aes(
         x = `Tracking Link`, 
         y = KPO_score, 
         group = LA, 
         colour = LA,
         text = paste(
           LA,
           paste("Quarter:", `Tracking Link`),
           paste("KPO 4 Score:", KPO_score),
           sep = "\n"
         )
         ),
                lwd = 1)+
       scale_color_manual( 
            values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("KPO 4 score - over time")+
       ylim(0,10)+
       xlab("")+
       ylab("KPO 4 Score")+
       theme_classic()
     ggplotly(plt, tooltip = "text")
     
     } 
     else
    {
       # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
       report_line_data$LA <- factor(report_line_data$LA, levels = c(council_fltr, "Scotland"))
       
       # arrange the data and store order of colours
       report_line_data <- arrange(report_line_data,LA)
       
       plt <- ggplot(data = report_line_data) +
         geom_bar(aes(
           x = `Tracking Link`, 
           y = KPO_score, 
           fill = LA,
           text = paste(
             LA,
             paste("Quarter:", `Tracking Link`),
             paste("KPO 4 Score:", KPO_score),
             sep = "\n"
           )
           ), 
           stat = "identity",
           position = "dodge",
           width = 0.7, 
           colour = "black")+
         scale_y_continuous(limits = c(0,10), expand = c(0, 0))+
         scale_fill_manual(values = c("cadetblue3","dimgrey"), name = "")+ 
         ggtitle("KPO 4 score - over time")+
         xlab("")+
         ylab("KPO 4 Score")+
         theme_classic()+
         theme(axis.text.x=element_blank(),
               axis.ticks.x=element_blank())
       
       ggplotly(plt, tooltip = "text")
       
     }

   })
   
##render text for quarter by quarter performance   
   output$quarter_text <- renderText({
     
     council_fltr <- local_authority()
     #kpo data
     all_kpo_data <- report_kpo_data()
    #get the number of quarters for rendering the text
     no_quarts <- length(unique(dta$`Tracking Link`[dta$`Local Authority Name` == council_fltr]))
     

    #filter to get KPO for quarter 1
    Q1_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 1", id == council_fltr) %>%
      select(KPO_score)
    #render text for quarter 1
    Q1_text<- paste0("In Quarter 1 performance for KPO 4 calculated across all responses for all questions was ",
                      Q1_kpo,". ")
    #filter to get KPO for quarter 2
    Q2_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 2", id == council_fltr) %>%
      select(KPO_score)
    #compare quarter 2 and quarter 1
    comp_Q12 <- tryCatch({ifelse(Q2_kpo > Q1_kpo+0.2, "rose", ifelse(Q2_kpo < Q1_kpo-0.2, "fell", "stayed the same"))},error =function(error_message){""})
    #render text for quarter 2                   
    Q2_text<- paste0(Q1_text,"Performance then ", comp_Q12," in Quarter 2 to stand at ", Q2_kpo)
    
    #filter to get KPO for quarter 3
    Q3_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 3", id == council_fltr) %>%
      select(KPO_score)
    #compare quarter 3 and quarter 2 - ignore if error occurs
    comp_Q23 <- tryCatch({ifelse(Q3_kpo > Q2_kpo+0.2, "higher than", ifelse(Q3_kpo < Q2_kpo-0.2, "lower than", "the same as"))},error =function(error_message){""})
    #render text for quarter 3                   
    Q3_text<- paste0(Q2_text,". In Quarter 3 performance was ", comp_Q23," Quarter 2 at ", Q3_kpo)
    
    #filter to get KPO for quarter 4
    Q4_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 4", id == council_fltr) %>%
      select(KPO_score)
    #compare quarter 4 and quarter 3 - ignore if error occurs
    comp_Q34 <- tryCatch({ifelse(Q4_kpo > Q3_kpo+0.2, "higher than", ifelse(Q4_kpo < Q3_kpo-0.2, "lower than", "the same as"))},error =function(error_message){""})
    #render text for quarter 4                   
    Q4_text<- paste0(
      Q3_text, ". In Quarter 4 performance was ", comp_Q34," Quarter 3, and stands at ", Q4_kpo)
    
    
    final_text <- ifelse(no_quarts == 1, Q1_text, ifelse(no_quarts == 2, Q2_text, ifelse(no_quarts == 3, Q3_text, Q4_text)))
    final_text
   })
   
##create graph and text for Question 1 on report page=================

   ##generate data to be used in graph and text
   question_time_data_report <- reactive({
     council_fltr <- local_authority()
     ##filter dataset based on selected question   
     dta <- dta %>% filter(value != "-")
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)

     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100,1))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                   "2" ="satisfied",
                                   "3" = "dissatisfied",
                                   "4" = "very dissatisfied")
     
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
      qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))

    # arrange the data so the colours will be in order
      qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
     
   })
   
   output$question_time_report <- renderPlotly({
     qstnDta <- question_time_data_report() 
     
    #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
       #  x = reorder(named_value, as.numeric(value)), 
         x = named_value,
         y = perc_resp,
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
       ), 
       stat= "identity", 
       position = "dodge",
       width = 0.7, 
       colour = "black")+
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual(values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("Satisfaction with time taken - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")
     
   })
   
  
   # satisfaction with time taken text
   output$question_time_report_text <- renderText({
     #load data and split into Scotland and LA datasets
     council_fltr <- local_authority()
     qstnDta <- question_time_data_report() 
     qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
     qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
     #get total percentage good or very good 
     total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
       sum()
     #if this is above 55% then overall is positive, otherwise negative/balances
     pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
     
     # add "only" to the % positive if this is below 45
     if(total_good < 45) {
       total_good <- paste("only", total_good)
     } else{
       total_good <- total_good
     }
     
      #get the name for the maximum value in LA dataset. If more than one paste these together
     max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
     if(length(max_name >1)){
       max_name <- paste(max_name, collapse = " & ")
     }
     #Get the pecentage for the highest response and paste together if multiple
     max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
     if(length(max_perc) >1){
       max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
     } else{
       max_perc <- paste0(max_perc, " percent.")
     }
     
     #Gte second highest value
     sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
     #Filter for second highest value's name
     sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
     if(length(sec_name) >1){
       sec_name <- paste(sec_name, collapse = " & ")
     }
     
     #Filter for second highest value's value
     sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
     if(length(sec_perc) >1){
       sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
     }else{
       sec_perc <- paste0(sec_perc, " percent.")
     }
     
     #get most frequent response for Scotland
     scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
     
     if(length(scot_max_name) >1){
       scot_max_name <- paste(scot_max_name, collapse = " & ")
     }
     #get percentage for most frequent Scotland level response
     scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
     if(length(scot_max_perc) >1){
       scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
     } else{
       scot_max_perc <- paste0(scot_max_perc, " percent.")
     }
     
     #Paste it all together!
     
     paste("In this year to date for the question \"Thinking of your engagement, how satisfied were you with the time taken to complete the process?\" responses have been",
           pos_or_neg, "with",total_good,"percent saying that they were very satisfied or satisfied. The greatest proportion of respondents said they were", max_name,
           "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
           "For Scotland overall, most respondents said that they were",scot_max_name,
           "at", scot_max_perc)
   })
   
  ##create graph and text for Question 2 on report page========
   ##generate data to be used in graph and text
   question_comms_data_report <- reactive({
     council_fltr <- local_authority()
     ##filter dataset based on selected question   
     dta <- dta %>% filter(value != "-")
     ##filter dataset based on selected question   
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "How would you rate the standard of communication provided?") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "How would you rate the standard of communication provided?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100,1))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- as.character(recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor"))
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta

   })
   
     #create a graph
     output$question_comms_report <- renderPlotly({
       qstnDta <- question_comms_data_report()
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black") +
       scale_fill_manual( 
         values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("Standard of communication - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       theme_classic()
     ggplotly(p, tooltip = "text")
   })
     
     # satisfaction with comms taken text
     output$question_comms_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <- question_comms_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question \"How would you rate the standard of communication provided?\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that it was good or very good. The greatest proportion of respondents said they felt it was", max_name,
             "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that communication was",scot_max_name,
             "at", scot_max_perc)
     })
  
   ##create graph and text for Question 3 on report page==========
     ##generate data to be used in graph and text
     question_info_data_report <- reactive({
       council_fltr <- local_authority()
       dta <- dta %>% filter(value != "-")
     ##filter dataset based on selected question   
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Quality of the information provided") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Quality of the information provided") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100,1))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
    })
     output$question_info_report <- renderPlotly({
       qstnDta <- question_info_data_report()
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black"
         ) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual( 
         values = c("cadetblue3","dimgrey"), name = "")+
       ggtitle("Quality of information - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")
     
   })
     
     # satisfaction with info text
     output$question_info_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <- question_info_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question \"Quality of the information provided\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that it was good or very good. The greatest proportion of respondents said they felt it was", max_name,
             "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that the information they received was",scot_max_name,
             "at", scot_max_perc)
     })
     
   ##create graph and text for Question 4 on report page=============
     question_staff_data_report <- reactive({
       council_fltr <- local_authority()
       dta <- dta %>% filter(value != "-")
     ##filter dataset based on selected question   
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Service offered by staff") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Service offered by staff") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100,1))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
      })
     
     #create a graph
     output$question_staff_report <- renderPlotly({
       qstnDta <- question_staff_data_report() 
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black"
         ) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual( 
         values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("Service offered by staff - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")

   })
     
     # satisfaction with staff text
     output$question_staff_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <- question_staff_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question for how they would rate the \"Service offered by staff\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that it was good or very good. The greatest proportion of respondents said they felt it was", max_name,
             "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that the service received was",scot_max_name,
             "at", scot_max_perc)
     })
     
  ##create graph and text for Question 5 on report page-------------------
   
     question_responsiveness_data_report <- reactive({
       council_fltr <- local_authority()
       dta <- dta %>% filter(value != "-")
      ##filter dataset based on selected question   
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Responsiveness to any queries or issues raised") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Responsiveness to any queries or issues raised") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100),1)
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
      })
     
     #create a graph
     output$question_responsiveness_report <- renderPlotly({
       qstnDta <-  question_responsiveness_data_report() 
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black"
         ) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual( 
         values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("Responsiveness to queries or issues - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")

      })
     
     # satisfaction with responsiveness text
     output$question_responsiveness_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <-  question_responsiveness_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question on how they would rate the \"Time taken to respond to any queries or issues raised\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that it was good or very good. The greatest proportion of respondents said they felt it was", max_name,
             "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that responsiveness was",scot_max_name,
             "at", scot_max_perc)
     })
     
   ##create graph and text for Question 6 on report page---------
     question_fairly_data_report <- reactive({
       council_fltr <- local_authority()
       dta <- dta %>% filter(value != "-")
     ##filter dataset based on selected question   
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "To what extent would you agree that you were treated fairly?"   ) %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "To what extent would you agree that you were treated fairly?"   ) %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100),1)
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "strongly agree",
                                   "2" ="agree",
                                   "3" = "disagree",
                                   "4" = "strongly disagree")
    
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
     
     })
     #create a graph
     output$question_fair_report <- renderPlotly({
       qstnDta <- question_fairly_data_report()
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black"
         )+
       scale_fill_manual( 
         values = c("cadetblue3", "dimgrey"), name = "")+
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
        ggtitle("Would you agree you were treated fairly - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")
        })
     
     # satisfaction with responsiveness text
     output$question_fair_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <-  question_fairly_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question \"To what extent would you agree that you were treated fairly?\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that they agree or strongly agree. The greatest proportion of respondents said they", max_name,
             "with the statement at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that they",scot_max_name,
             "at", scot_max_perc)
     })
     
   ##create graph for Question 7 on report page===========
     question_overall_data_report <- reactive({
       council_fltr <- local_authority()
     ##filter dataset based on selected question   
     dta <- dta %>% filter(value != "-")
     dta$`value` <- factor(dta$`value`, levels = c(1,2,3,4))
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Overall, how satisfied were you with the service provided?") %>%
       filter(`Local Authority Name` == council_fltr) %>% count(value, .drop =F) %>%
       mutate(Selection = council_fltr)
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Overall, how satisfied were you with the service provided?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = round((n/sum(n))*100),1)
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                   "2" ="satisfied",
                                   "3" = "dissatisfied",
                                   "4" = "very dissatisfied")
     
     # Set the council values as a factor so the data can be arranged to have the council first regardless of alphabetical order
     qstnDta$Selection <- factor(qstnDta$Selection, levels = c(council_fltr, "Scotland"))
     
     # arrange the data so the colours will be in order
     qstnDta <- arrange(qstnDta, value, Selection) 
     qstnDta
     
     })
     
     #create a graph
     output$question_overall_report <- renderPlotly({
       qstnDta <- question_overall_data_report() 
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(
         x = reorder(named_value, as.numeric(value)), 
         y = perc_resp, 
         fill = Selection,
         text = paste(
           Selection, 
           paste("Response:", named_value), 
           paste("% of Responses:", perc_resp),
           sep = "\n")
         ), 
         stat= "identity", 
         position = "dodge",
         width = 0.7, 
         colour = "black"
         ) +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
       scale_fill_manual( 
         values = c("cadetblue3", "dimgrey"), name = "")+
       ggtitle("Overall satisfaction - Year to Date")+
       xlab("Responses")+
       ylab("Percentage of Responses")+
       theme_classic()
     ggplotly(p, tooltip = "text")
      })
     
     # satisfaction with responsiveness text
     output$question_overall_report_text <- renderText({
       council_fltr <- local_authority()
       #load data and split into Scotland and LA datasets
       qstnDta <-  question_overall_data_report() 
       qstnDta_LA <- qstnDta %>% filter(Selection == council_fltr)
       qstnDta_scot<- qstnDta %>% filter(Selection == "Scotland")
       #get total percentage good or very good 
       total_good <- filter(qstnDta_LA, value %in% c(1,2) & Selection == council_fltr) %>% pull(perc_resp) %>%
         sum()
       #if this is above 55% then overall is positive, otherwise negative/balances
       pos_or_neg <- ifelse(total_good > 55, "mainly positive,", ifelse(total_good < 45, "mainly negative,", "balanced,"))
       
       # add "only" to the % positive if this is below 45
       if(total_good < 45) {
         total_good <- paste("only", total_good)
       } else{
         total_good <- total_good
       }
       
       #get the name for the maximum value in LA dataset. If more than one paste these together
       max_name <- as.character(qstnDta_LA %>% filter(n == max(n)) %>% pull(named_value))
       if(length(max_name >1)){
         max_name <- paste(max_name, collapse = " & ")
       }
       #Get the pecentage for the highest response and paste together if multiple
       max_perc <- qstnDta_LA %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(max_perc) >1){
         max_perc <- paste(paste(max_perc, collapse = " & "), "percent respectively.")
       } else{
         max_perc <- paste0(max_perc, " percent.")
       }
       
       #Gte second highest value
       sec_val <- sort(qstnDta_LA$n, partial= 3)[3]
       #Filter for second highest value's name
       sec_name <- qstnDta_LA %>% filter(n == sec_val) %>% pull(named_value)
       if(length(sec_name) >1){
         sec_name <- paste(sec_name, collapse = " & ")
       }
       
       #Filter for second highest value's value
       sec_perc <- qstnDta_LA %>% filter(n == sec_val) %>% pull(perc_resp)
       if(length(sec_perc) >1){
         sec_perc <- paste(paste(sec_perc, collapse = " & "), "percent respectively.")
       }else{
         sec_perc <- paste0(sec_perc, " percent.")
       }
       
       #get most frequent response for Scotland
       scot_max_name <- as.character(qstnDta_scot %>% filter(n == max(n)) %>% pull(named_value))
       
       if(length(scot_max_name) >1){
         scot_max_name <- paste(scot_max_name, collapse = " & ")
       }
       #get percentage for most frequent Scotland level response
       scot_max_perc <- qstnDta_scot %>% filter(n == max(n)) %>% pull(perc_resp)
       if(length(scot_max_perc) >1){
         scot_max_perc <- paste(paste(scot_max_perc, collapse = " & "), "percent respectively.")
       } else{
         scot_max_perc <- paste0(scot_max_perc, " percent.")
       }
       
       #Paste it all together!
       
       paste("In this year to date for the question \"Overall, how satisfied were you with the service provided?\" responses have been",
             pos_or_neg, "with",total_good,"percent saying that they were very satisfied or satisfied. The greatest proportion of respondents said they felt ", max_name,
             "at", max_perc, "This was followed by", sec_name, "at", sec_perc,
             "For Scotland overall, most respondents said that they were",scot_max_name,
             "at", scot_max_perc)
     })
     
##Generate download from report page-------------
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
         
     
         # Knit the document, passing in the `params` list, and eval it in a
         # child of the global environment (this isolates the code in the document
         # from the code in this app).
         rmarkdown::render(tempReport, output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
         )
       }
     )
     
  ##Generate Data download to Excel====================
     output$all_data_dl <- downloadHandler(
       filename = paste("All_Data", ".csv", sep = ""),
       content = function(file) {
         dl_all_data <- dl_all_data()
         council_fltr <- local_authority()
         
         ##recode all responses for the download from a number to text, remove LA column
         dl_all_data <- dl_all_data %>% filter(`Local Authority Name` == council_fltr) %>%
           mutate(across(contains("how satisfied"), 
                         ~recode(.,"1" = "Very satisfied", "2"="Satisfied", "3" = "Dissatisfied",
                                 "4" = "Very dissatisfied",  "5" = "NA"))) %>%
           mutate(across(contains("would you rate"),
                         ~recode(.,"1" = "Very good", "2"="Good", "3" = "Poor",
                                 "4" = "Very poor",  "5" = "NA"))) %>%
           mutate(across(contains(c("quality of the information","accuracy of the information", "respond to", "by staff", "our staff")),
                         ~recode(.,"1" = "Very good", "2"="Good", "3" = "Poor",
                                 "4" = "Very poor",  "5" = "NA"))) %>%
           mutate(across(contains(c("Staff were","Easy to find", "Understandable")),
                         ~recode(.,"1" = "Strongly agree", "2"="Agree", "3" = "Neither agree nor disagree",
                                 "4" = "Disagree",  "5" = "Strongly disagree", "6" = "NA"))) %>%
           mutate(across(contains("would you agree"),
                         ~recode(.,"1" = "Strongly agree", "2"="Agree",
                                 "3" = "Disagree",  "4" = "Strongly disagree", "5"="NA"))) %>%
           mutate(across(contains("Did you find it easy to contact"),
                         ~recode(., "1" = "Yes, contact made straight away", 
                                 "2" = "Yes, but took slightly longer than expected",
                                 "3" = "No it wasn’t easy, but managed to contact the officer/inspector/administrator eventually"))) %>%
           mutate(across(contains("Finally"),
                         ~recode(., "1" = "Yes", 
                                 "2" = "No",
                                 "3" = "NA"))) %>%
           dplyr::rename("Quarter" = "Tracking Link") %>%
           mutate(across(contains(c("Q1.1. Agent/Designer", 
                                    "Q1.2. Applicant", 
                                    "Q1.3. Contractor", 
                                    "Other (please specify):",
                                   "Q2.1. To discuss your proposal",
                                    "Q2.2. To make an application", 
                                   "Q2.3. During construction")),
                         ~recode(., "1" = "Yes", 
                                 "0" = "No"))) %>%
           select(-LA)
         
         #final tidy up of column names by removing SmartSurvey variable
         colnames(dl_all_data) <- gsub(" \\[question\\(16082428\\)\\]\\[variable\\(la\\)\\]", "", colnames(dl_all_data))
         colnames(dl_all_data) <- gsub("\\...[1-9]*$", "",colnames(dl_all_data))
         write.csv(dl_all_data, file)
       }
     )
##create table with all data to explore     
     output$tableDisp <- DT::renderDataTable({
       unpivot_data <- unpivot_data()
       names(unpivot_data)[3:ncol(unpivot_data)] <- gsub("Q[1-9\\.]+\\s","",names(unpivot_data)[3:ncol(unpivot_data)], perl = T)
       tbl <- datatable(unpivot_data, rownames = FALSE, class = "row-border",escape = F,extensions = c("Scroller", "FixedColumns"), 
                        options = list(pageLength = 32, scrollY = 250, dom = "t", 
                                       scrollX = TRUE, 
                                       #fixedColumns = list(leftColumns = 1),
                                       fnDrawCallback  = htmlwidgets::JS(
                                         "function(){
                                         HTMLWidgets.staticRender();
     }"
                  ), columnDefs = list(list(className = "dt-center", targets = "_all"))))
     })

##create table to show comments for selected question 
     output$cmnt_table <- DT::renderDataTable({
       unpivot_data <- unpivot_data()
       ##need to filter the data based on selections and recode answers
       names(unpivot_data)[3:ncol(unpivot_data)] <- gsub("Q[1-9\\.]+\\s","",names(unpivot_data)[3:ncol(unpivot_data)], perl = T)
       unpivot_data$Quarter <- as.factor(unpivot_data$Quarter)
       ##select applicant type  
       slctn_respondent <- input$cmnts_resp_input
       ##select applicant reason using partial match
       slctn_reason <- names(select(unpivot_data, contains(input$cmnts_reason_input)))
       
     #  slct_qstns <- ifelse(input$cmnts_slct == "Information, staff, and responsiveness", c("information", "staff", "responsiveness"), input$cmnts_slct)
       
       filter_data <- unpivot_data %>% filter(if_any(slctn_respondent, ~ . == "Yes")) %>%
         filter(if_any(slctn_reason, ~.== "Yes")) %>%
         select(contains(input$cmnts_slct))
       datatable(filter_data, filter = "top",rownames = FALSE, class = "row-border",escape = F,extensions = c("Scroller", "FixedColumns"))
       
     })
     
##log in details     
     output$userpanel <- renderUI({
       # session$user is non-NULL only in authenticated sessions
       if (!is.null(session$user)) {
         usr <- session$user
         usr <- gsub("(@).+\\..+", "", usr, perl = T)
         sidebarUserPanel(
           paste("Logged in as: ", usr, sep = "\n"),
           subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
       }
     })
 }
