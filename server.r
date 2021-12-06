server <- function(input, output) {
  
  output$performanceBox <- renderValueBox({
    valueBox(
      value = "8.2", "Out of 10", icon = icon("chart-bar"), color = "blue"
    )
  })
}
