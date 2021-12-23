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
  
  
  ##calculate KPO4 for quearter for selected local authority    
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
    output$respDoughnut <- renderPlotly({
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
   if(input$Qstn_tab2 == "All Questions"){
       qstnDta <- filt_data %>% filter(value != "-")
     }else{
       qstnDta <-filter(filt_data, Indicator == input$Qstn_tab2) %>% filter(value != "-")
       qstnDta$value <- factor(qstnDta$value,levels = c(1,2,3,4))
            }
     qstnDta <- qstnDta %>% count(value, .drop = F)
     
     if(input$Qstn_tab2 == "All Questions"){
     qstnDta$named_value <- recode(qstnDta$value, "1" = "Very good/very satisfied/strongly agree",
                                   "2" ="good/satisfied/agree",
                                   "3" = "poor/dissatisfied/disagree",
                                   "4" = "Very poor/very dissatisfied/strongly disagree")
     } else if(input$Qstn_tab2 == "Quality of the information provided"|input$Qstn_tab2 =="Overall, how satisfied were you with the service provided?"){
       qstnDta$named_value <- recode(qstnDta$value, "1" = "very satisfied",
                                     "2" ="satisfied",
                                     "3" = "dissatisfied",
                                     "4" = "very dissatisfied")
     } else{
       qstnDta$named_value <- recode(qstnDta$value, "1" = "very good",
                                     "2" ="good",
                                     "3" = "poor",
                                     "4" = "very poor")
       qstnDta$named_value <- factor(qstnDta$named_value, levels  = c("very good", "good", "poor", "very poor"))
     }
  
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
   
   output$reportKPO4Plot <- renderPlotly({
     p <- ggplot(data = dta) +
       geom_bar(aes(x = Council, y = Quarter), stat= "identity") +
       theme_bw()
     ggplotly(p)     
     })
   
   
   ##Render text for KPO4 Overall perf to year
   output$KPO4_text <- renderText({
     local_auth <- "Aberdeen"
     curr_year <- yr2fy(2022)
     KPO4_ytd <- 8
     hilow_kpo4 <- ifelse(KPO4_ytd > 7.5, "higher", "lower")
     scotAv_kpo4 <- 7.8
     abbel_kpo4 <- ifelse(KPO4_ytd > scotAv_kpo4, "higher", "lower")
     
     text_kpo <- paste("This indicator summarises performance across all questions, with differential
                       weightings based on importance. For", local_auth,"in",curr_year, "overall
                       performance is at", KPO4_ytd, "for the year to date.", "This is", hilow_kpo4,
                       "than the Scotland average of", scotAv_kpo4,"and", abbel_kpo4,"than the target value
                       of 7.5.")
     text_kpo
   })
   
   output$reportRespondents <- renderPlotly({
     p <- ggplot(data = dta) +
       geom_bar(aes(x = Council, y = Quarter), stat= "identity") +
       theme_bw()
     ggplotly(p)     
   })
}
