ui <- dashboardPage(skin = "blue",
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
      menuItem("Open Text", tabName = "OpTxt", icon = icon("comments")),
      tags$footer(a("Contact us", href = "mailto:research@improvementservice.org.uk"), style = "position:fixed; bottom:0; margin-left:2px")
      
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "PrfOvr",
              h2(paste("KPO4 Performance", "Date last updated:", format(Sys.time(), "%d %b %Y")),style = "margin-top:3px"), ###add something to say date most recently updated?
              "logged in as", textOutput("user2"),
              fluidRow(
                valueBoxOutput("performanceBox"), #performance for council
                bsPopover("performanceBox", title = "KPO4 Weightings" ,
                         content = KPO_popover_text,
                          "right", trigger = "click"),
                valueBoxOutput("scotPerfBox"),   #Scotland average performance
                valueBoxOutput("respBox")
                ),
              fluidRow(
                box(width = 8,
                  plotlyOutput("ovrPerfBar")
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
            downloadBttn("report", "Generate report"),
              fluidRow(
                box(width = 8, plotlyOutput("reportKPO4Plot")),
                box(width = 4,style = "font-size:18px",textOutput("KPO4_text_report"))
              ),
              fluidRow(
                box(width = 8, plotlyOutput("resp_type_graph_report")),
                box(width = 4,style = "font-size:18px",textOutput("respondent_type_text_report"))
               ),
            fluidRow(
              box(width = 8, plotlyOutput("resp_reason_graph_report")),
              box(width = 4,style = "font-size:18px",textOutput("respondent_reason_text_report"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("ovrPerfLine")),
              box(width = 4, style = "font-size:18px",textOutput("quarter_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_time_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_time_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_comms_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_comms_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_info_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_info_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_staff_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_staff_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_responsiveness_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_responsiveness_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_fair_report")),
              box(width = 4, style = "font-size:18px",textOutput("question_fair_report_text"))
            ),
            fluidRow(
              box(width = 8, plotlyOutput("question_overall_report")),
              box(width = 4,  style = "font-size:18px", textOutput("question_overall_report_text"))
            )
              ),
      tabItem(tabName = "DtDl",
              downloadBttn("all_data_dl", label = "Download all data", style = "jelly"),
              box(div(DT::dataTableOutput("tableDisp"),style = "font-size:80%; line-height:75%; width:160%; padding-left:0px"),width = 12)
              
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
              box(DT::dataTableOutput("cmnt_table"), width = 12)
              )
      )
    )
    
  )
)

##Add shiny manager authenitcation
#ui <- secure_app(ui)