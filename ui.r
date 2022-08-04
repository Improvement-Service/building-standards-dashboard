ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "National Customer Survey Dashboard",
                  titleWidth = 400),
 
   dashboardSidebar(
    uiOutput("la_select"),
    uiOutput("KPO_data_dl"),
    sidebarMenu(
      menuItem("Performance Overview", tabName="PrfOvr", icon = icon("dashboard")),
      menuItem("Question Results", tabName = "Qstns", icon = icon("question-circle")),
      menuItem("Report Download", tabName = "RptDl", icon = icon("chart-area")),
      menuItem("Data Download", tabName = "DtDl", icon = icon("download")),
      menuItem("Open Text", tabName = "OpTxt", icon = icon("comments")),
      uiOutput("userpanel"),
      tags$footer(a("Contact us", href = "mailto:research@improvementservice.org.uk"), style = "position:fixed; bottom:0; margin-left:2px")
      
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "PrfOvr",
              uiOutput("LA_KPO4_Heading"),
              fluidRow(
                valueBoxOutput("performanceBox"), #performance for council
                bsPopover("performanceBox", title = "KPO4 Weightings" ,
                         content = KPO_popover_text,
                          "right", trigger = "hover"),
                valueBoxOutput("scotPerfBox")%>%withSpinner(),   #Scotland average performance
                valueBoxOutput("respBox")
                ),
              fluidRow(
                box(width = 8,height = "66vh",
                  plotlyOutput("ovrPerfBar", height = "60vh")%>%withSpinner()
                  ),
                tabBox(width = 4,height = "66vh",
                       title = "Respondents", 
                       id = "RespOverViewTabs",
                       tabPanel("Type",plotlyOutput("respDoughnut",height = "60vh")%>%withSpinner()),
                       tabPanel("Reason", plotlyOutput("plotly_pie",height = "60vh"))
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
              tabBox(width = 12, height = "66vh",
                    tabPanel("Year to Date",plotlyOutput("YTDqstsPlot", height = "60vh")),
                    tabPanel("Summary by Quarter", plotlyOutput("qrtsQsplot", height = "60vh")))
               ),
      tabItem(tabName = "RptDl",
            #  h2("Report Download", style = "margin-top:3px"),
            div(style = "margin-bottom: 5px; display: inline",downloadBttn("report", "Generate report")),
            div(style = "float:right", bsButton("q1_pop", label = "", icon = icon("question"), style = "info", size = "extra-small")),
            bsPopover("q1_pop",title = "About This Page" ,
                      content = report_popover_text,
                      "left", trigger = "hover"),
              fluidRow(style = "margin-top:10px",
                box(width = 8, plotlyOutput("reportKPO4Plot")%>%withSpinner()),
                box(width = 4,style = "font-size:18px",textOutput("KPO4_text_report")%>%withSpinner())
              ),
              fluidRow(
                box(width = 8, plotlyOutput("resp_type_graph_report")%>%withSpinner()),
                box(width = 4,style = "font-size:18px",textOutput("respondent_type_text_report")%>%withSpinner())
               ),
            fluidRow(
              box(width = 8, plotlyOutput("resp_reason_graph_report")%>%withSpinner()),
              box(width = 4,style = "font-size:18px",textOutput("respondent_reason_text_report")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("ovrPerfLine")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("quarter_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_time_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_time_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_comms_report")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("question_comms_report_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_info_report")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("question_info_report_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_staff_report")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("question_staff_report_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_responsiveness_report")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("question_responsiveness_report_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_fair_report")%>%withSpinner()),
              box(width = 4, style = "font-size:18px",textOutput("question_fair_report_text")%>%withSpinner())
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_overall_report")%>%withSpinner()),
              box(width = 4,  style = "font-size:18px", textOutput("question_overall_report_text")%>%withSpinner())
            )
              ),
      tabItem(tabName = "DtDl",
              div(style = "margin-bottom: 5px",downloadBttn("all_data_dl", label = "Download all data", style = "jelly")),
              box(div(DT::dataTableOutput("tableDisp", height = "75vh"),style = "font-size:80%; line-height:75%; width:100%; padding-left:0px"),
                  width = 12, height = "80vh")
              
              ),
      tabItem(tabName = "OpTxt",
              fluidRow(
              column(4,selectInput("cmnts_slct", "Select comments",c("Time taken", "Communication",
                                                            "Information", "Staff", "Responsiveness",
                                                            "Treated fairly",
                                                            "Overall",
                                                            "Other comments"))),
              column(4,prettyCheckboxGroup(
                inputId = "cmnts_resp_input",
                label = "Respondent", 
                choices = c("Agent/Designer", "Applicant", "Contractor", "Other respondent"),
                selected = c("Agent/Designer", "Applicant", "Contractor", "Other respondent"),
                inline = TRUE,
                icon = icon("check"),
                status = "danger",
                animation = "rotate"
              )),
              column(4,prettyCheckboxGroup(
                inputId = "cmnts_reason_input",
                label = "Reasons", 
                choices = c("To discuss your proposal", "Make an application", "During construction", "Other reason"),
                selected = c("To discuss your proposal", "Make an application", "During construction", "Other reason"),
                inline = TRUE,
                icon = icon("check"),
                status = "info",
                animation = "pulse"
              )),
              box(DT::dataTableOutput("cmnt_table"), width = 12, height = "70vh")
              )
      )
    )
    
  )
)
