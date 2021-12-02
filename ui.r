ui <- dashboardPage(
  dashboardHeader(title = "National Customer Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Overview", tabName="PrfmncOvrvw", icon = icon("dashboard")),
      menuItem("Question Results", tabName = "Qstns", icon = icon("question-circle"))
    )
  ),
  dashboardBody()
)