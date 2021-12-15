server <- function(input, output) {
  

  
#Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    laAv <- dta %>% filter(Council == input$LA_selection) %>% summarise(CncPerf = round(mean(`Q1. test`, na.rm = T),1))
    valueBox(
      value = laAv$CncPerf, "Council Satisfaction YTD", icon = icon("chart-bar"), color = "red"
    )
  })
#Create performance box for Scotland
  output$scotPerfBox<- renderInfoBox({
    scotAv<- round(mean(dta$`Q1. test`, na.rm = T),1) ##calculate Scotland average
    infoBox(
      value = scotAv, "Scotland Average", icon = icon("times"), color = "blue"
    )
  })
#Create responses valuebox
    output$respBox <- renderValueBox({
      valueBox(
        value = paste("500", "Responses"), paste(500+300,"Year to Date"), icon = icon("user-friends"), color = "green"
      )
    })
    
##Create bar plot for overall performance
    output$ovrPerfBar <- renderPlot(
       ggplot(data = dta) +
         geom_bar(aes(x = Quarter, y = `Q1. test`, fill = Council), stat = "identity",
                  position = "dodge")
    )
    
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
