server <- function(input, output) {
  
#Create performance box for selected Council  
  output$performanceBox <- renderValueBox({
    valueBox(
      value = "8.1", "Council Satisfaction YTD", icon = icon("chart-bar"), color = "red"
    )
  })
#Create performance box for Scotland
  output$scotPerfBox<- renderValueBox({
    valueBox(
      value = "8.2", "Scotland Average", icon = icon("times"), color = "blue"
    )
  })
#Create responses valuebox
    output$respBox <- renderValueBox({
      valueBox(
        value = paste("500", "Responses"), "xxx Year to Date", icon = icon("user-friends"), color = "green"
      )
    })
}
