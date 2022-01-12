server <- function(input, output) {
  
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
  scot_max_sum <- scot_max %>% group_by(`Tracking Link`) %>%
    summarise(across(c(maxAvailable,KPO4_weighted),sum, na.rm = T)) %>% 
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))  %>%
    mutate(KPO_score = 1-KPO4_weighted/maxAvailable)
  
  
  ##calculate KPO4 for quarter for selected local authority    
  la_max_sum <- reactive({
    
    ##filter KPO data by local authority name
    LA_num <- match(input$LA_selection, LA_Names)
    
    la_max_sum <- scot_max %>% filter(LA == 1) %>%          ##Filter using input for LA
    group_by(`Tracking Link`) %>%
    summarise(across(c(maxAvailable,KPO4_weighted),sum, na.rm = T)) %>% 
    bind_rows(summarise(.,across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))  %>%
    mutate(KPO_score = 1-KPO4_weighted/maxAvailable)
  })
  
#Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    la_max_sum <- la_max_sum()
    valueBox(
      value = round(la_max_sum[la_max_sum$`Tracking Link` =="Total", "KPO_score"],2), "Council KPO4 YTD", icon = icon("chart-bar"), color = "red"
    )
  })
#Create performance box for Scotland
  output$scotPerfBox<- renderInfoBox({
    infoBox(
      value = round(scot_max_sum[scot_max_sum$`Tracking Link` =="Total", "KPO_score"],2), "Scotland Average", icon = icon("times"), color = "blue"
    )
  })
#Create responses valuebox
    output$respBox <- renderValueBox({
      valueBox(
        value = paste(nrow(filter(unpivot_data, `Tracking Link` == "Quarter 1")), "Responses"), paste(nrow(unpivot_data),"Year to Date"), icon = icon("user-friends"), color = "green"
      )
    })
    
##Create bar plot for overall performance
    output$ovrPerfBar <- renderPlot({
      la_max_sum <- la_max_sum()
      
       ggplot(data = la_max_sum) +
         geom_bar(aes(x = `Tracking Link`, y = KPO_score), stat = "identity",
                  position = "dodge")
    })
    
##Create doughnut for respondent types and reasons
    
  ##n.b. I am using the same output twice, in the UI, but this is not allowed, so this
    #is the suggested workaround i.e. assign the output twice
    output$respDoughnut_report <- output$respDoughnut <- renderPlotly({
  ##select data    
      pc_resp_data <- resp_dta %>% filter(., question_type == "Type" & value == 1)
      pfig <- ggplot(data = pc_resp_data) +
        geom_col(aes(x = Question, y = perc))+
        coord_flip()
      ggplotly(pfig)
      
    })
    
##Alternative - create a pie chart in plotly
    output$plotly_pie <- renderPlotly({
      ##select data   
      pc_resp_data <- resp_dta %>% filter(., question_type == "Reason" & value == 1)
      pc_resp_data$Question <- gsub(" for a building warrant","",pc_resp_data$Question)
      pc_resp_data[pc_resp_data$Question == "During construction, including submission of a completion certificate", "Question"] <-"During construction" 
      pfig <- ggplot(data = pc_resp_data) +
        geom_col(aes(x = Question, y = perc))+
        coord_flip()
      ggplotly(pfig)
    })
    
##Create graphs to display results by questions================================
  
    ##Create filtered dataset from checkboxes
    qstn_dataset_filtered <- reactive({
      names(dta) <- gsub("Q[1-9\\.]+\\s","",names(dta), perl = T)
      dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
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
  #generate basic barplot
     p <- ggplot(data = qstnDta) +
      geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = n), stat= "identity")
    ggplotly(p)
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
     qstnDta <- qstnDta %>% group_by(value) %>% summarise(., across(n, sum)) %>% mutate(`Tracking Link` = "YTD") %>%
       select(3,1,2) %>%
       bind_rows(qstnDta) %>%
       group_by(`Tracking Link`) %>%
       mutate(total_responses = sum(n)) %>%
       ungroup()%>%
       mutate(percentage_responses = n/total_responses)
   
       p <- ggplot(data = qstnDta ) +
         geom_bar(aes(x = `Tracking Link`, y = percentage_responses), stat= "identity", position = "dodge")
       ggplotly(p)
   })
   
##Report page outputs =====================================
   
   report_kpo_data <- reactive({
     la_max_sum <- la_max_sum()
     
     la_max_sum$id <- "local authority"
     scot_max_sum$id <- "Scotland" 
     
     all_kpo_dta <- rbind(scot_max_sum, la_max_sum)
     all_kpo_dta
   })
   
   output$reportKPO4Plot <- renderPlotly({
     all_kpo_data <- report_kpo_data()
     all_kpo_data <- all_kpo_data %>% filter(`Tracking Link` == "Total")
     ggplot(data = all_kpo_data) +
       geom_bar(aes(x = `Tracking Link`, y = KPO_score, fill = id), stat = "identity",
                position = "dodge")   
     })
   
   
   ##Render text for KPO4 Overall perf to year
   output$KPO4_text <- renderText({
     all_kpo_data <- report_kpo_data()
     local_auth <- "Aberdeen City" ##will need to select LA based on log in details
     curr_year <- yr2fy(2022)
     KPO4_ytd <- all_kpo_data %>% filter(id == "local authority") %>% select(KPO_score)
     hilow_kpo4 <- ifelse(KPO4_ytd > 7.5, "higher", "lower")
     scotAv_kpo4 <- all_kpo_data %>% filter(id == "Scotland") %>% select(KPO_score)
     abbel_kpo4 <- ifelse(KPO4_ytd > scotAv_kpo4, "higher", "lower")
     
     text_kpo <- paste("This indicator summarises performance across all questions, with differential
                       weightings based on importance. For", local_auth,"in",curr_year, "overall
                       performance is at", KPO4_ytd, "for the year to date.", "This is", hilow_kpo4,
                       "than the Scotland average of", scotAv_kpo4,"and", abbel_kpo4,"than the target value
                       of 7.5.")
     text_kpo
   })
   
   output$respondent_text_report <- renderText({
     local_auth <- "Aberdeen City" ##will need to select LA based on log in details
     resp_dta_filter <- resp_dta %>% filter(LA == 1) ##filter by LA
     resp_number <- resp_dta_filter %>% ungroup() %>% filter(Question == "Agent/Designer") %>% summarise_at(vars(`n`), sum) %>%
       select(`n`)
     agent_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Agent/Designer" & resp_dta_filter$value == 1 ,"perc"] *100, 2)
     appli_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Applicant" & resp_dta_filter$value == 1 ,"perc"] *100, 2)
     contr_perc <- round(resp_dta_filter[resp_dta_filter$Question == "Contractor" & resp_dta_filter$value == 1 ,"perc"] *100, 2)
    
     txt_respondents <- paste0("Respondents were asked to provide details on the type of respondent they were, 
     as well as their reason for contacting the Building Standards Service in", local_auth,".",
     "Of the ", resp_number, " respondents ", agent_perc, "% were agents or designers,", appli_perc, "%
     were applicants and ", contr_perc, "% were contractors.")
     txt_respondents
   })
   
   output$ovrPerfLine <- renderPlotly({
     la_max_sum <- la_max_sum()
     scot_max_sum$LA <- "Scotland"
     la_max_sum$LA <- "Aberdeen City"
     
     quarts_dta <- rbind(scot_max_sum,la_max_sum) %>% filter(`Tracking Link` != "Total")
     
     plt <- ggplot(data = quarts_dta) +
       geom_line(aes(x = `Tracking Link`, y = KPO_score, group = LA, colour = LA),
                lwd = 1)
     ggplotly(plt)
   })
##render text for quarter by quarter performance   
   output$quarter_text <- renderText({
    #get the number of quarters for rendering the text
     no_quarts <- length(unique(dta$`Tracking Link`))
    #kpo data
     all_kpo_data <- report_kpo_data()
    #filter to get KPO for quarter 1
    Q1_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 1", id == "local authority") %>%
      select(KPO_score)
    #render text for quarter 1
    Q1_text<- paste0("In Quarter 1 performance for KPO 4 calculated across all responses for all questions was ",
                      Q1_kpo,". ")
    #filter to get KPO for quarter 2
    Q2_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 2", id == "local authority") %>%
      select(KPO_score)
    #compare quarter 2 and quarter 1
    comp_Q12 <- ifelse(Q2_kpo > Q1_kpo+0.2, "rose", ifelse(Q2_kpo < Q1_kpo-0.2, "fell", "stayed the same"))
    #render text for quarter 2                   
    Q2_text<- paste0(Q1_text,"Performance then ", comp_Q12,
            "in Quarter 2 to stand at", Q2_kpo)
    
    #filter to get KPO for quarter 3
    Q3_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 3", id == "local authority") %>%
      select(KPO_score)
    #compare quarter 3 and quarter 2 - ignore if error occurs
    comp_Q23 <- tryCatch({ifelse(Q3_kpo > Q3_kpo+0.2, "higher than", ifelse(Q3_kpo < Q3_kpo-0.2, "lower than", "the same as"))},error =function(error_message){""})
    #render text for quarter 3                   
    Q3_text<- paste0(Q1_text, Q2_text,"In Quarter 3 performance was ", comp_Q23,
                     " Quarter 2 at", Q3_kpo)
    
    #filter to get KPO for quarter 4
    Q4_kpo <- all_kpo_data %>% filter(`Tracking Link` == "Quarter 4", id == "local authority") %>%
      select(KPO_score)
    #compare quarter 4 and quarter 3 - ignore if error occurs
    comp_Q34 <- tryCatch({ifelse(Q4_kpo > Q4_kpo+0.2, "higher than", ifelse(Q4_kpo < Q4_kpo-0.2, "lower than", "the same as"))},error =function(error_message){""})
    #render text for quarter 4                   
    Q3_text<- paste0(Q1_text, Q2_text, Q3_text, "In Quarter 4 performance is ", comp_Q34,
                     " Quarter 3, and stands at", Q4_kpo)
    
    final_text <- ifelse(no_quarts == 1, Q1_text, ifelse(no_quarts == 2, Q2_text, ifelse(no_quarts == 3, Q3_text, Q4_text)))
    
   })
   
##create graph for Question 1 on report page
   output$question_time_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Thinking of your engagement, how satisfied were you with the time taken to complete the process?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
    #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
    #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                   "2" ="satisfied",
                                   "3" = "dissatisfied",
                                   "4" = "very dissatisfied")
    #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   
   ##create graph for Question 2 on report page
   output$question_comms_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "How would you rate the standard of communication provided?") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "How would you rate the standard of communication provided?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
  
   ##create graph for Question 3 on report page
   output$question_info_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Quality of the information provided") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Quality of the information provided") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   ##create graph for Question 4 on report page
   output$question_staff_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Service offered by staff") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Service offered by staff") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   ##create graph for Question 5 on report page
   output$question_responsiveness_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Responsiveness to any queries or issues raised") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Responsiveness to any queries or issues raised") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                   "2" ="good",
                                   "3" = "poor",
                                   "4" = "very poor")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   ##create graph for Question 6 on report page
   output$question_fair_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "To what extent would you agree that you were treated fairly?"   ) %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "To what extent would you agree that you were treated fairly?"   ) %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "strongly agree",
                                   "2" ="agree",
                                   "3" = "disagree",
                                   "4" = "strongly disagree")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   ##create graph for Question 7 on report page
   output$question_overall_report <- renderPlot({
     ##filter dataset based on selected question   
     dta$`Tracking Link` <- as.factor(dta$`Tracking Link`)
     #filter by local authority and question and count no. responses
     qstnDta_LA <- dta %>% filter(Indicator == "Overall, how satisfied were you with the service provided?") %>%
       filter(LA == "1") %>% count(value, .drop =F) %>%
       mutate(Selection = "LA")
     
     #get all data for this question and count no. responses, bind LA count
     qstnDta <- dta %>% filter(Indicator == "Overall, how satisfied were you with the service provided?") %>%
       count(value, .drop =F) %>%
       mutate(Selection = "Scotland") %>%
       rbind(qstnDta_LA )
     #Get percentage of responses for LA and Scotland 
     qstnDta <- qstnDta %>% group_by(Selection) %>% mutate(perc_resp = n/sum(n))
     #Recode the values for this question to be shown on tickmarks in x axis 
     qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                   "2" ="satisfied",
                                   "3" = "dissatisfied",
                                   "4" = "very dissatisfied")
     #create a graph
     p <- ggplot(data = qstnDta ) +
       geom_bar(aes(x = reorder(named_value, as.numeric(value)), y = perc_resp, fill =Selection), stat= "identity", position = "dodge")
     p
     
   })
   }
