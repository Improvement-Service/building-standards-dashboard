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
    scotAv<- round(mean(dta$`Q1. test`, na.rm = T),1) ##calculate Scotland aerage
    valueBox(
      value = scotAv, "Scotland Average", icon = icon("times"), color = "blue"
    )
  })
#Create responses valuebox
    output$respBox <- renderValueBox({
      valueBox(
        value = paste("500", "Responses"), "xxx Year to Date", icon = icon("user-friends"), color = "green"
      )
    })
    
##Create bar plot
    output$ovrPerfBar <- renderPlot(
       ggplot(data = dta) +
         geom_bar(aes(x = Quarter, y = `Q1. test`, fill = Council), stat = "identity",
                  position = "dodge")
    )
}
