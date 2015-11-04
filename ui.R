library(shiny)
library(shinydashboard)
library(dygraphs)

options( shiny.maxRequestSize = 300*1024*1024 )

dashboardPage(
  dashboardHeader( title = "Nar Report" ),
  dashboardSidebar(
    sidebarMenu(
      menuItem( "Inputs", tabName = "load", icon = icon("folder-open") ),
      hr(), h5("Review charts"),
      menuItem("SP", tabName = "sp", icon = icon("area-chart") ),
      menuItem("LUNs", tabName = "lun", icon = icon("area-chart") ),
      menuItem("Disks", tabName = "disk", icon = icon("area-chart") ),
      menuItem("Hosts", tabName = "host", icon = icon("area-chart") ),
      hr(), menuItem("Detailed charts", tabName = "detailed", icon = icon("line-chart") ),
      menuItem("Relations", tabName = "relations", icon = icon("pie-chart") ),
      menuItem("Summary", tabName = "summary", icon = icon("table") ),
      hr(), menuItem( "Report", tabName = "report", icon = icon("save") ),
      hr(), menuItem( "Feedback", icon = icon("envelope"),
                      tags$a( href = "mailto:vzaigrin@yandex.ru?subject=Nar%20Report", "Feedback" )
                    )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem( tabName = "load",
        fluidRow(
          box( title = "Instructions", status = "info", width = 12, solidHeader = TRUE,
               "Upload CSV file (up to 300MB), extracted from some NAR files from one disk array.",
               br(), br(),
               "Because of the limited resources available to this application, ",
               "we analyze only the basic objects (SP, Host LUN, Disks) and parameters ",
               "(Utilization, Throughput, Bandwidth, Read/Write Size, Response Time, Service Time and Queue Length).",
               br(), br(),
               "To reduce the size of the csv file it's better to use this command:",
               br(), br(),
               code("naviseccli analyzer -archivedump -data file1.nar ... filen.nar "),
               code( "-object s,hl,d -format on,pt,oan,co,u,tt,rio,wio,tb,rb,wb,rs,ws,rt,st,ql "),
               code( "-join -overwrite y -out nar.csv" ),
               br(), br(),
               "After uploading file please take some time for processing."
          )
        ),      
        fluidRow(
          box( title = "Inputs", status = "warning", solidHeader = TRUE,
               fileInput( "file", label = "Choose CSV file",
                         accept = c( 'text/csv','text/comma-separated-values,text/plain', '.csv' )
               )
          ),
          box( title = "Results", status = "success", solidHeader = TRUE, uiOutput("ui")
          )
        )
      ),
      tabItem( tabName = "sp",
        fluidRow(
          box( title = "Utilization", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "sp.util", click = "sp.util.click" ),
               verbatimTextOutput( "sp.util.info" )
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "sp.tt", click = "sp.tt.click" ),
               verbatimTextOutput( "sp.tt.info" )
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "sp.trw", click = "sp.trw.click" ),
               verbatimTextOutput( "sp.trw.info" )
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "sp.bt", click = "sp.bt.click" ),
               verbatimTextOutput( "sp.bt.info" )
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "sp.brw", click = "sp.brw.click" ),
               verbatimTextOutput( "sp.brw.info" )
          ),
          box( title = "Read and Write Size", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "sp.srw", click = "sp.srw.click" ),
               verbatimTextOutput( "sp.srw.info" )
          )
        )
      ),
      tabItem( tabName = "lun",
        fluidRow(
          box( title = "Histogram of total throughput by 95th percentile",
               status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "lun.hist", hover = "lun.hist.hover" ),
               verbatimTextOutput( "lun.hist.info" )
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "lun.tt", click = "lun.tt.click" ),
               verbatimTextOutput( "lun.tt.info" )
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "lun.trw", click = "lun.trw.click" ),
               verbatimTextOutput( "lun.trw.info" )
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "lun.bt", click = "lun.bt.click" ),
               verbatimTextOutput( "lun.bt.info" )
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "lun.brw", click = "lun.brw.click" ),
               verbatimTextOutput( "lun.brw.info" )
          ),
          box( title = "Read and Write Size", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "lun.srw", click = "lun.srw.click" ),
               verbatimTextOutput( "lun.srw.info" )
          )
        )
      ),
      tabItem( tabName = "disk",
        fluidRow(
          box( title = "Disks Heatmap", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "disk.heat", hover = "disk.hist.hover" ),
               verbatimTextOutput( "disk.hist.info" )
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "disk.tt" , click = "disk.tt.click" ),
               verbatimTextOutput( "disk.tt.info" )
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "disk.trw", click = "disk.trw.click" ),
               verbatimTextOutput( "disk.trw.info" )
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "disk.bt", click = "disk.bt.click" ),
               verbatimTextOutput( "disk.bt.info" )
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "disk.brw", click = "disk.brw.click" ),
               verbatimTextOutput( "disk.brw.info" )
          )
        )
      ),
      tabItem( tabName = "host",
        fluidRow(
          box( title = "Histogram of total throughput by 95th percentile",
               status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "host.hist", hover = "host.hist.hover" ),
               verbatimTextOutput( "host.hist.info" )
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "host.tt", click = "host.tt.click" ),
               verbatimTextOutput( "host.tt.info" )
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "host.trw", click = "host.trw.click" ),
               verbatimTextOutput( "host.trw.info" )
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput( "host.bt", click = "host.bt.click" ),
               verbatimTextOutput( "host.bt.info" )
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "host.brw", click = "host.brw.click" ),
               verbatimTextOutput( "host.brw.info" )
          ),
          box( title = "Read and Write Size", status = "primary", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               plotOutput( "host.srw", click = "host.srw.click" ),
               verbatimTextOutput( "host.srw.info" )
          )
        )
      ),
      tabItem( tabName = "detailed",
        fluidRow(
          box( title = "Select component and objects", status = "warning", width = 12,
               solidHeader = TRUE, collapsible = TRUE,
               radioButtons( "det.component", label = "Select component",
                             choices = list( "SP" = 1, "LUNs" = 2, "Disks" = 3, "Hosts" = 4 ),
                             selected = 1
               ),
               selectInput( "det.list", label = "Select objects", choices = list(),
                            multiple = TRUE )
          ),     
          box( title = "Utilization", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.util")
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.tt")
          ),
          box( title = "Read Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.tr")
          ),
          box( title = "Write Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.tw")
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.bt")
          ),
          box( title = "Read Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.br")
          ),
          box( title = "Write Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.bw")
          ),
          box( title = "Read Size", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.sr")
          ),
          box( title = "Write Size", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.sw")
          ),
          box( title = "Response Time", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.rt")
          ),
          box( title = "Service Time", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.st")
          ),
          box( title = "Queue Length", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, dygraphOutput("det.ql")
          )
        )
      ),      
      tabItem( tabName = "relations",
        fluidRow(
          box( title = "Components", status = "warning", solidHeader = TRUE,
               collapsible = TRUE,
               radioButtons( "rel.component", label = h3("Select component"),
                             choices = list( "SP" = 1, "LUNs" = 2, "Disks" = 3, "Hosts" = 4 ),
                             selected = 2
               )
          ),     
          box( title = "Parameters", status = "warning", solidHeader = TRUE,
               collapsible = TRUE,
               selectInput( "rel.x", label = "x", 
                            choices = list( "Utilization" = "util",
                                            "Throughput" = "iops",
                                            "Read Throughput" = "riops",
                                            "Write Throughput" = "wiops",
                                            "Bandwidth" = "bw",
                                            "Read Bandwidth" = "rbw",
                                            "Write Bandwidth" = "wbw",
                                            "Read Size" = "rsize",
                                            "Write Size" = "wsize",
                                            "Response Time" = "rt",
                                            "Service Time" = "st",
                                            "Queue Length" = "ql"
                            ), selected = "iops"
               ),
               selectInput( "rel.y", label = "y", 
                            choices = list( "Utilization" = "util",
                                            "Throughput" = "iops",
                                            "Read Throughput" = "riops",
                                            "Write Throughput" = "wiops",
                                            "Bandwidth" = "bw",
                                            "Read Bandwidth" = "rbw",
                                            "Write Bandwidth" = "wbw",
                                            "Read Size" = "rsize",
                                            "Write Size" = "wsize",
                                            "Response Time" = "rt",
                                            "Service Time" = "st",
                                            "Queue Length" = "ql"
                            ), selected = "rt"
               ),
               selectInput( "rel.size", label = "Size", 
                            choices = list( "Utilization" = "util",
                                            "Throughput" = "iops",
                                            "Read Throughput" = "riops",
                                            "Write Throughput" = "wiops",
                                            "Bandwidth" = "bw",
                                            "Read Bandwidth" = "rbw",
                                            "Write Bandwidth" = "wbw",
                                            "Read Size" = "rsize",
                                            "Write Size" = "wsize",
                                            "Response Time" = "rt",
                                            "Service Time" = "st",
                                            "Queue Length" = "ql"
                            ), selected = "ql"
               ),
               selectInput( "rel.colour", label = "Color", 
                            choices = list( "Utilization" = "util",
                                            "Throughput" = "iops",
                                            "Read Throughput" = "riops",
                                            "Write Throughput" = "wiops",
                                            "Bandwidth" = "bw",
                                            "Read Bandwidth" = "rbw",
                                            "Write Bandwidth" = "wbw",
                                            "Read Size" = "rsize",
                                            "Write Size" = "wsize",
                                            "Response Time" = "rt",
                                            "Service Time" = "st",
                                            "Queue Length" = "ql"
                                          ), selected = "util"
               )  
          ),     
          box( title = "Relations", status = "primary", width = 12, solidHeader = TRUE,
               plotOutput( "rel.plot", click = "rel.click" ),
               "Brush to select points", br(),
               verbatimTextOutput( "rel.info" )
          )
        )  
      ),          
      tabItem( tabName = "summary",
        fluidRow(
          box( title = "Components", status = "warning", solidHeader = TRUE,
               collapsible = TRUE,
               radioButtons( "summary.component", label = h3("Select component"),
                             choices = list( "SP" = 1, "LUNs" = 2, "Disks" = 3, "Hosts" = 4 ),
                             selected = 1 )
          ),     
          box( title = "Summary", status = "primary", width = 12, solidHeader = TRUE,
               dataTableOutput( "summary" )
          )
        )  
      ),          
      tabItem( tabName = "report",
        fluidRow(
          box( title = "Report", status = "primary", width = 12, solidHeader = TRUE,
               "Download a full report in pdf format.", br(),
               "Generating report takes about 5 - 10 min. Please be patient.", br(), br(),
               downloadButton( 'downloadReport', label = "Download" )
          )
        )
      )
    )
  )
)
