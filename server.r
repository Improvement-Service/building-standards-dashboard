server <- function(input, output) {
  
##Create outputs for KPO4 Summary Page===========================  
  
##calculated the KPO4 score based on weighted responses
  KPOdta <- dta %>% mutate(value = as.numeric(value)) %>% mutate(KPO_weight = value-1)
  KPOweights_multiplier <- outer(dta$Indicator == "Q4. Thinking of your engagement with [question(15510346)] Building Standards from beginning to end, how satisfied were you with the time taken to complete the process?",2)+
    outer(dta$Indicator == "Q5. How would you rate the standard of communication provided by the Building Standards service following your initial contact or once your application had been submitted?",2) +
    outer(dta$Indicator == "Q7. Overall, how satisfied were you with the service provided by [question(15510346)] Building Standards?",8)
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
    output$respDoughnut <- renderPlot({
  ##create data    
      data <- data.frame(
        category=c("A", "B", "C"),
        count=c(10, 60, 30)
      )
      # Compute percentages
      data$fraction = data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax = cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin = c(0, head(data$ymax, n=-1))
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
        xlim(c(2, 4)) +
        theme_void()
      
    })
    
##Alternative - create a pie chart in plotly
    output$plotly_pie <- renderPlotly({
     pfig <- plot_ly(data = dta,labels = ~Council, values = ~`Q1. test`, margin = c(0,0,0,0), autosize = F) %>% 
       add_pie(hole = 0.6)
     pfig <- pfig %>% layout(title = "Test",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                             )
     pfig
    })
    
##Create graphs to display results by questions================================
  #First graph for full breakdown of responses in year to date  
   output$YTDqstsPlot <- renderPlotly({
   if(input$Qstn_tab2 == "All Questions"){
       qstnDta <- dta
     }else{
       qstnDta <-filter(dta, Indicator == input$Qstn_tab2)
    }
     qstnDta <- qstnDta %>% count(value)
     
     p <- ggplot(data = qstnDta) +
      geom_bar(aes(x = value, y = n), stat= "identity")
    ggplotly(p)
   })
   #Second summary of %age Good/v.good by quarter
   output$qrtsQsplot <- renderPlotly({
       p <- ggplot(data = dta) +
         geom_bar(aes(x = Council, y = Quarter), stat= "identity")
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
