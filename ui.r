ui <- dashboardPage(skin = "blue",                      

                    # Dashboard header ---------------------------------------------------------                    
                    dashboardHeader(title = "National Customer Survey Dashboard",
                                    titleWidth = 400),
                    # Dashboard Sidebar --------------------------------------------------------
                    # KPO4 download button (IS & SG only)
                    dashboardSidebar(uiOutput("KPO_data_dl"),
                                     # LA selection (IS & SG only)
                                     uiOutput("la_select"),
                                     # Financial Year selection.
                                     # Will only show years available
                                     uiOutput("fin_yr"),
                                     # Quarter selection.
                                     # Will only show quarters available in selected year
                                     uiOutput("qrtr"),
                                     sidebarMenu(menuItem("Performance Overview", 
                                                          tabName = "PrfOvr", 
                                                          icon = icon("dashboard")
                                     ),
                                     menuItem("Annual KPO4 by Respondent",
                                              tabName = "KPO_resp",
                                              icon = icon("user")
                                     ),
                                     menuItem("Question Results", 
                                              tabName = "Qstns", 
                                              icon = icon("question-circle")
                                     ),
                                     menuItem("Report Download", 
                                              tabName = "RptDl", 
                                              icon = icon("chart-area")
                                     ),
                                     menuItem("Data Download", 
                                              tabName = "DtDl", 
                                              icon = icon("download")
                                     ),
                                     menuItem("Open Text", 
                                              tabName = "OpTxt", 
                                              icon = icon("comments")
                                     ),
                                     menuItem("Version History",
                                              tabName = "version",
                                              icon = icon("clone")
                                     ),
                                     uiOutput("userpanel"),
                                     tags$footer(a("Contact us", 
                                                   href = "mailto:research@improvementservice.org.uk"
                                     ), 
                                     style = "position:fixed; bottom:0; margin-left:2px"
                                     )
                                     )
                    ),
                    # Dashboard body ----------------------------------------------------------
                    dashboardBody(tabItems(
                      # Performance Overview Tab ----------------------------------------------- 
                      tabItem(tabName = "PrfOvr",
                              uiOutput("LA_KPO4_Heading"),
                              # KPO4 Performance for council
                              fluidRow(valueBoxOutput("performanceBox"), 
                                       bsPopover("performanceBox", 
                                                 title = "KPO4 Weightings",
                                                 content = KPO_popover_text,
                                                 "right", 
                                                 trigger = "hover"
                                       ),

                                       # KPO4 performance for Scotland average
                                       valueBoxOutput("scotPerfBox") %>%
                                         withSpinner(),   
                                       # Responses in quarter and YTD
                                       valueBoxOutput("respBox")
                              ),
                              # KPO4 performance graph
                              fluidRow(box(width = 8,
                                           height = "66vh",
                                           plotlyOutput("ovrPerfBar", 
                                                        height = "60vh"
                                                        ) %>%
                                             withSpinner()
                                           ),
                                       # Respondents overview tabs
                                       tabBox(width = 4,
                                              height = "66vh",
                                              title = "Respondents", 
                                              id = "RespOverViewTabs",
                                              # Respondents type graph
                                              tabPanel("Type",
                                                       plotlyOutput("resp_type_graph_overview",
                                                                    height = "55vh"
                                                                    ) %>%
                                                         withSpinner()
                                                       ),
                                              # Respondents reason graph
                                              tabPanel("Reason", 
                                                       plotlyOutput("resp_reason_graph_overview",
                                                                    height = "55vh"
                                                                    )
                                                       )
                                              )
                                       )
                              ), 

# KPO4 by Respondent Tab ------------------------------------
                      tabItem(tabName  = "KPO_resp",
                              fluidRow(box(width = 6, plotlyOutput("kpo_resp_graph_agent")),
                                       box(width = 6, plotlyOutput("kpo_resp_graph_contr"))
                                       ),
                              fluidRow(box(width = 6, plotlyOutput("kpo_resp_graph_applicant")),
                                       box(width = 6, plotlyOutput("kpo_resp_graph_other"))
                              )
                      ),
# Question Results Tab -------------------------------------------------------
                      tabItem(tabName = "Qstns",
                              fluidRow(column(3,
                                              # Question drop down
                                              selectInput(inputId = "Qstn_tab2", 
                                                          label = "Select Question",
                                                          choices = c("All Questions", 
                                                                      unique(pivot_dta$Indicator)
                                                                      ),
                                                          selected = "All Questions"
                                                          )
                                               ),
                                       column(4,
                                              # Respondent type selection
                                              prettyCheckboxGroup(inputId = "Qs_resp_input",
                                                                  label = "Respondent", 
                                                                  choices = c("Agent/Designer", 
                                                                              "Applicant", 
                                                                              "Contractor", 
                                                                              "Other respondent"
                                                                              ),
                                                                  selected = c("Agent/Designer", 
                                                                               "Applicant", 
                                                                               "Contractor", 
                                                                               "Other respondent"
                                                                               ),
                                                                  inline = TRUE,
                                                                  icon = icon("check"),
                                                                  status = "danger",
                                                                  animation = "rotate"
                                                                  )
                                              ),
                                       column(5,
                                              # Respondent reason selection
                                              prettyCheckboxGroup(inputId = "Qs_reason_input",
                                                                  label = "Reasons", 
                                                                  choices = c("To discuss your proposal", 
                                                                              "To make an application", 
                                                                              "During construction", 
                                                                              "Other reason"
                                                                              ),
                                                                  selected = c("To discuss your proposal", 
                                                                               "To make an application", 
                                                                               "During construction", 
                                                                               "Other reason"
                                                                               ),
                                                                  inline = TRUE,
                                                                  icon = icon("check"),
                                                                  status = "info",
                                                                  animation = "pulse"
                                                                  )
                                              )
                                       ),
                              # Response overview graphs tabs
                              fluidRow(
                                column(9,
                                  tabBox(width = 12, 
                                     height = "66vh",
                                     # YTD graph
                                     tabPanel("Year to Date",
                                              plotlyOutput("YTDqstsPlot", 
                                                           height = "60vh"
                                                           )
                                              ),
                                     # Summary by quarter graph
                                     tabPanel("Summary by Quarter", 
                                              plotlyOutput("qrtsQsplot", 
                                                           height = "60vh"
                                                           )#,
 
                                              )
                                     )
                                  ),
                                column(3,
                                       box(title = "Respondents by Quarter",
                                           width = 12,
                                           tableOutput("resp_qrts")
                                       )
                                )
                              )
                              ),
# Report Download Tab -----------------------------------------------------
                      tabItem(tabName = "RptDl",
                              # Report download button
                              div(style = "margin-bottom: 5px; display: inline",
                                  downloadBttn("report", "Generate report")
                                  ),
                              # About this page button
                              div(style = "float:right", 
                                  bsButton("q1_pop", 
                                           label = "", 
                                           icon = icon("question"), 
                                           style = "info", 
                                           size = "extra-small"
                                           )
                                  ),
                              # About this page popover content
                              bsPopover("q1_pop",
                                        title = "About This Page" ,
                                        content = report_popover_text,
                                        "left", 
                                        trigger = "hover"
                                        ),
                              fluidRow(style = "margin-top:10px",
                                       # KPO4 YTD graph
                                       box(width = 8, 
                                           plotlyOutput("reportKPO4Plot") %>%
                                             withSpinner()
                                           ),
                                       # KPO4 YTD Text
                                       box(width = 4,
                                           style = "font-size:18px",
                                           textOutput("KPO4_text_report") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Respondent type graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("resp_type_graph_report") %>%
                                             withSpinner()
                                           ),
                                       # Respondent type text
                                       box(width = 4,
                                           style = "font-size:18px",
                                           textOutput("respondent_type_text_report") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Respondent reason graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("resp_reason_graph_report") %>%
                                             withSpinner()
                                           ),
                                       # Respondent reason text
                                       box(width = 4,
                                           style = "font-size:18px",
                                           textOutput("respondent_reason_text_report") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # KPO4 over time graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("ovrPerfLine") %>%
                                             withSpinner()
                                           ),
                                       # KPO4 over time text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("quarter_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q1 - Satisfaction with time graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_time_report") %>%
                                             withSpinner()
                                           ),
                                       # Q1 - Satisfaction with time text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_time_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q2 - Standard of communication graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_comms_report") %>%
                                             withSpinner()
                                           ),
                                       # Q2 - Standard of communication text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_comms_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                       # Q3 - Quality of information graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_info_report") %>%
                                             withSpinner()
                                           ),
                                       # Q3 - Quality of information text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_info_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q4 - Service offered by staff graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_staff_report") %>%
                                             withSpinner()
                                           ),
                                       # Q4 - Service offered by staff text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_staff_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q5 - Responsiveness to queries or issues graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_responsiveness_report") %>%
                                             withSpinner()
                                           ),
                                       # Q5 - Responsiveness to queries or issues text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_responsiveness_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q6 - Treated fairly graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_fair_report") %>%
                                             withSpinner()
                                           ),
                                       # Q6 - Treated fairly text
                                       box(width = 4, 
                                           style = "font-size:18px",
                                           textOutput("question_fair_report_text") %>%
                                             withSpinner()
                                           )
                                       ),
                                      # Q7 - Overall satisfaction graph
                              fluidRow(box(width = 8, 
                                           plotlyOutput("question_overall_report") %>%
                                             withSpinner()
                                           ),
                                       # Q7 - Overall satisfaction text
                                       box(width = 4,  
                                           style = "font-size:18px", 
                                           textOutput("question_overall_report_text") %>%
                                             withSpinner()
                                           )
                                       )
                              ),
# Data Download Tab ----------------------------------------------------------
                      tabItem(tabName = "DtDl",
                              div(style = "margin-bottom: 5px",
                                  # Data download button
                                  downloadBttn("all_data_dl", 
                                               label = "Download all data", 
                                               style = "jelly"
                                               )
                                  ),
                                      # Data table
                              box(div(DT::dataTableOutput("tableDisp", 
                                                          height = "75vh"
                                                          ),
                                      style = "font-size:80%; line-height:75%; width:100%; padding-left:0px"
                                      ),
                                  width = 12, height = "80vh"
                                  )
                              ),
# Open Text Tab ---------------------------------------------------------------
                      tabItem(tabName = "OpTxt",
                              fluidRow(column(4,
                                              # Comment selection drop down
                                              selectInput(inputId = "cmnts_slct", 
                                                          label = "Select comments",
                                                          choices = c("Time taken", 
                                                                      "Communication",
                                                                      "Information", 
                                                                      "Staff", 
                                                                      "Responsiveness",
                                                                      "Treated fairly",
                                                                      "Overall",
                                                                      "Other comments"
                                                                      )
                                                          )
                                              ),
                                       column(4,
                                              # Respondent type selection
                                              prettyCheckboxGroup(inputId = "cmnts_resp_input",
                                                                  label = "Respondent", 
                                                                  choices = c("Agent/Designer", 
                                                                              "Applicant", 
                                                                              "Contractor", 
                                                                              "Other respondent"
                                                                              ),
                                                                  selected = c("Agent/Designer", 
                                                                               "Applicant", 
                                                                               "Contractor", 
                                                                               "Other respondent"
                                                                               ),
                                                                  inline = TRUE,
                                                                  icon = icon("check"),
                                                                  status = "danger",
                                                                  animation = "rotate"
                                                                  )
                                              ),
                                       column(4,
                                              # Respondent reason selection
                                              prettyCheckboxGroup(inputId = "cmnts_reason_input",
                                                                  label = "Reasons", 
                                                                  choices = c("To discuss your proposal", 
                                                                              "To make an application", 
                                                                              "During construction", 
                                                                              "Other reason"
                                                                              ),
                                                                  selected = c("To discuss your proposal", 
                                                                               "To make an application", 
                                                                               "During construction", 
                                                                               "Other reason"
                                                                               ),
                                                                  inline = TRUE,
                                                                  icon = icon("check"),
                                                                  status = "info",
                                                                  animation = "pulse"
                                                                  )
                                              ),
                                       # Comments Data Table
                                       box(DT::dataTableOutput("cmnt_table"), 
                                           width = 12, 
                                           height = "70vh"
                                           )
                                       )
                              ),
# Version History Tab ---------------------------------------------------------
                      tabItem(tabName = "version",
                              h1("Version History"),
                              tags$br(),
                              h4(div(tags$li(tags$b("Version 2.2 - March 2023"), 
                                             tags$br(),
                                             "- Dropdown added to sidebar to allow selection of quarters. This filters the data throughout the tool to match the selection.",
                                             tags$br(),
                                             "- New tab ", 
                                             tags$i("Annual KPO4 by Respondent"),
                                             "added.",
                                             tags$br(),
                                             "- Table with respondent numbers per quarter added to ",
                                             tags$i("Question Results"),
                                             "tab. "
                                             )
                                     )
                                 ),
                              tags$br(),
                              h4(div(tags$li(tags$b("Version 2.1 - February 2023"),
                                             tags$br(),
                                             " - Dropdown added to sidebar to allow selection of financial year when more than one is available. This filters the data throughout the tool to match the selection."
                                             )
                                     )
                                 ),
                              tags$br(),
                              h4(div(tags$li(tags$b("Version 1.2 - August 2022"), 
                                             tags$br(),
                                             " - Submission dates added to data download and data table on ",
                                             tags$i("Data Download"),
                                             "tab."
                                             )
                                     )
                                 ),
                              tags$br(),
                              h4(div(tags$li(tags$b("Version 1.1 - April 2022"),
                                             tags$br(),
                                             " - Initial publication of tool."
                                             )
                                     )
                                 )
                              )
# Closing bracket for Tab Items in dashboard body
                                  )
# Closing bracket for dashboard body
                                  )
# Closing bracket for dashboard page
                    )
