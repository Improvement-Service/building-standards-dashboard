ui <- dashboardPage(
  dashboardHeader(title = "National Customer Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Overview", tabName="PrfOvr", icon = icon("dashboard")),
      menuItem("Question Results", tabName = "Qstns", icon = icon("question-circle")),
      menuItem("Report Download", tabName = "RptDl", icon = icon("chart-area")),
      menuItem("Data Download", tabName = "DtDl", icon = icon("download")),
      menuItem("Open Text", tabName = "OpTxt", icon = icon("comments"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PrfOvr",
              h2("KPO4 Performance"), ###add something to say date most recently updated?
              valueBoxOutput("performanceBox")), 
      tabItem(tabName = "Qstns",
              h2("Performance by Question"))
    )
    
  )
)