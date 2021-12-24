ui <- dashboardPage(
  dashboardHeader(title = "National Customer Survey Dashboard"),
 
   dashboardSidebar(
    selectizeInput("LA_selection", "",
                   choices =LA_Names, options = list(placeholder = "Select Your Local Authority",
                                             onInitialize = I('function() { this.setValue(""); }'))),
    
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
              h2(paste("KPO4 Performance", "Date last updated:", format(Sys.time(), "%d %b %Y")),style = "margin-top:3px"), ###add something to say date most recently updated?
              fluidRow(
                valueBoxOutput("performanceBox"), #performance for council
                infoBoxOutput("scotPerfBox"),   #Scotland average performance
                valueBoxOutput("respBox")
                ),
              fluidRow(
                box(width = 8,
                  plotOutput("ovrPerfBar")
                  ),
                tabBox(width = 4,
                       title = "Respondents", 
                       id = "RespOverViewTabs",
                       tabPanel("Type",plotlyOutput("respDoughnut")),
                       tabPanel("Reason", plotlyOutput("plotly_pie"))
                )
               )
              ), 
      tabItem(tabName = "Qstns",
             # h2("Performance by Question", style = "margin-top:3px"),
              fluidRow(
                column(4,
              selectInput("Qstn_tab2", label = "Select Question",
                          choices = c("All Questions", unique(dta$Indicator)),
                          selected = "All Questions"
                          )),
              column(4,
              prettyCheckboxGroup(
                inputId = "Qs_resp_input",
                label = "Respondent", 
                choices = c("Agent/Designer", "Applicant", "Contractor", "Other respondent"),
                selected = c("Agent/Designer", "Applicant", "Contractor", "Other respondent"),
                inline = TRUE,
                icon = icon("check"),
                status = "danger",
                animation = "rotate"
                )),
              column(4,
              prettyCheckboxGroup(
                inputId = "Qs_reason_input",
                label = "Reasons", 
                choices = c("To discuss your proposal", "Make an application", "During construction", "Other reason"),
                selected = c("To discuss your proposal", "Make an application", "During construction", "Other reason"),
                inline = TRUE,
                icon = icon("check"),
                status = "info",
                animation = "pulse"
              ))),
              tabBox(width = 12,
                    tabPanel("Year to Date",plotlyOutput("YTDqstsPlot")),
                    tabPanel("Summary by Quarter", plotlyOutput("qrtsQsplot")))
               ),
      tabItem(tabName = "RptDl",
            #  h2("Report Download", style = "margin-top:3px"),
              fluidRow(
                box(width = 8, plotlyOutput("reportKPO4Plot")),
                box(width = 4,textOutput("KPO4_text"))
              ),
              fluidRow(
                box(width = 8, plotlyOutput("respDoughnut_report")),
                box(width = 4,textOutput("respondent_text_report"))
               ),
            fluidRow(
              box(width = 8, plotlyOutput("ovrPerfLine")),
              box(width = 4, "text")
            )
              )
    )
    
  )
)
