server <- function(input, output) {
  

  
#Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    laAv <- dta %>% filter(Council == input$LA_selection) %>% summarise(CncPerf = round(mean(`Q1. test`, na.rm = T),1))
    valueBox(
      value = laAv$CncPerf, "Council Satisfaction YTD", icon = icon("chart-bar"), color = "red"
    )
  })
#Create performance box for Scotland
  output$scotPerfBox<- renderValueBox({
    scotAv<- round(mean(dta$`Q1. test`, na.rm = T),1) ##calculate Scotland average
    valueBox(
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
    
##Create graph to display results by questions
   output$qstsPlot <- renderPlotly({
    p <- ggplot(data = dta) +
      geom_bar(aes(x = Council, y = Quarter), stat= "identity")
    ggplotly(p)
   })
}
