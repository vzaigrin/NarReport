library(shiny)
library(shinydashboard)
library(DiagrammeR)

options( shiny.maxRequestSize = 500*1024^2 )

dashboardPage(
  dashboardHeader( title = "Nar Report" ),
  dashboardSidebar(
    sidebarMenu(
      menuItem( "Inputs", tabName = "load", icon = icon("folder-open") ),
      dateRangeInput( 'dateRange', label = 'Select date range:', format = "mm/dd/yyyy" ),
      hr(), h5("Configuration"),
      menuItem("Diagram", tabName = "diagram", icon = icon("image") ),
      menuItem("Data", tabName = "configuration", icon = icon("gears") ),
      hr(), h5("Plots"),
      menuItem("SP", tabName = "sp", icon = icon("area-chart") ),
      menuItem("Ports", tabName = "port", icon = icon("area-chart") ),
      menuItem("LUNs", tabName = "lun", icon = icon("area-chart") ),
      menuItem("Disks", tabName = "disk", icon = icon("area-chart") ),
      menuItem("Hosts", tabName = "host", icon = icon("area-chart") ),
      hr(),
      menuItem("Relations", tabName = "relations", icon = icon("pie-chart") ),
      hr(),
      menuItem("Summary", tabName = "summary", icon = icon("table") ),
      hr(),
      menuItem( "Report", tabName = "report", icon = icon("save") ),
      hr()
    )
  ),
  dashboardBody(
    tabItems(
      tabItem( tabName = "load",
        fluidRow(
          box( title = "Instructions", status = "info", width = 12, solidHeader = TRUE,
               HTML("<UL>
                     <LI>Upload CSV file (up to 300MB), extracted from some NAR files from one disk array.
                     <br>
                     Because of the limited resources available to this application,
                     we analyze only the basic objects (SP, Ports, LUNs, Disks) and parameters
                     (Utilization, Throughput, Bandwidth, Read/Write Size, Response Time, Service Time and Queue Length).
                     <br>
                     To reduce the size of the csv file it's better to use this command:
                     <br>
                     <code>naviseccli analyzer -archivedump -data file1.nar ... filen.nar 
                           -object s,p,al,d -format on,pt,oan,co,u,tt,rio,wio,tb,rb,wb,rs,ws,rt,st,ql
                           -join -overwrite y -out nar.csv</code>
                     <br>
                     </LI>
                     <LI>
                     Upload configuration file in XML format extracted from NAR file by the command:
                     <code>naviseccli analyzer -archivedump -config filen.nar -xml -overwrite y -out config.xml</code>
                     </LI>
                     <LI>
                     Upload relations file in XML format extracted from NAR file by the command:
                     <code>naviseccli analyzer -archivedump -rel filen.nar -xml -overwrite y -out rel.xml</code>
                     </LI>
                     <LI>
                     After uploading all files please take some time for processing.
                     </LI>
                     </UL>") 
          )
        ),      
        fluidRow(
          box( title = "Input", status = "warning", solidHeader = TRUE,
               fileInput( "file1", label = "Choose CSV file",
                         accept = c( 'text/csv','text/comma-separated-values,text/plain', '.csv' )
               ),
               fileInput( "file2", label = "Choose config file",
                          accept = c( 'text/xml', '.xml' )
               ),
               fileInput( "file3", label = "Choose rel file",
                          accept = c( 'text/xml', '.xml' )
               )
          ),
          box( title = "Results", status = "success", solidHeader = TRUE, uiOutput("ui")
          )
        )
      ),
      tabItem( tabName = "diagram",
               fluidRow(
                 box( title = "Diagram", status = "primary", width = 12, height = 20100, solidHeader = TRUE,
                      grVizOutput('diagram', width = "100%", height = 20000 )
                 )
               )
      ),
      tabItem( tabName = "configuration",
               fluidRow(
                 box( title = "Component", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                      radioButtons( "conf.component", label = h3("Select component"),
                                    choices = list( "Hosts" = 1, "LUNs" = 2, "Pools/RG" = 3, "Disks" = 4  ),
                                    selected = 1
                      )
                 ),     
                 box( title = "Element", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                      selectInput( "conf.element", label = h3("Select element"), choices = list() )
                 )     
               ),   
               fluidRow(
                 box( title = "Host/Lun/Pool Configuration", status = "primary", width = 12, solidHeader = TRUE,
                      dataTableOutput("lun.config")
                 )
               ),
               fluidRow(
                 box( title = "Pool/Disk Configuration", status = "primary", width = 12, solidHeader = TRUE,
                      dataTableOutput("pool.config")
                 )
               )
      ),          
      tabItem( tabName = "sp",
        fluidRow(
          box( title = "Utilization", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.util")
          ),
          box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.tt")
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.trw")
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.bt")
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.brw")
          ),
          box( title = "Read and Write Size", status = "primary", width = 12, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("sp.srw")
          )
        )
      ),
      tabItem( tabName = "port",
               fluidRow(
                 box( title = "Total Throughput", status = "primary", width = 12, solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("port.tt" )
                 ),
                 box( title = "Read and Write Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("port.trw", height = 1000)
                 ),
                 box( title = "Total Bandwidth", status = "primary", width = 12, solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("port.bt" )
                 ),
                 box( title = "Read and Write Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("port.brw", height = 1000)
                 ),
                 box( title = "Read and Write Size", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("port.srw", height = 1000)
                 )
               )
      ),
      tabItem( tabName = "lun",
        fluidRow(
          box( title = "Histogram of total throughput by 95th percentile",
               status = "primary", width = 12, height = 660, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.hist", height = 600)
          ),
          box( title = "Total Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.tt", height = 1000)
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.trw", height = 1000)
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.bt", height = 1000)
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.brw", height = 1000)
          ),
          box( title = "Read and Write Size", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("lun.srw", height = 1000)
          )
        )
      ),
      tabItem( tabName = "disk",
        fluidRow(
          box( title = "Disks Heatmap", status = "primary", width = 12, height = 770, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("disk.heat", height = 600, click = "dhm.click"),
               br(), verbatimTextOutput("dhm.clickinfo")
          ),
          box( title = "Total Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("disk.tt", height = 1000)
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("disk.trw", height = 1000)
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("disk.bt", height = 1000)
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("disk.brw", height = 1000)
          )
        )
      ),
      tabItem( tabName = "host",
        fluidRow(
          box( title = "Histogram of total throughput by 95th percentile",
               status = "primary", width = 12, height = 660, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.hist", height = 600)
          ),
          box( title = "Total Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.tt", height = 1000)
          ),
          box( title = "Read and Write Throughput", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.trw", height = 1000)
          ),
          box( title = "Total Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.bt", height = 1000)
          ),
          box( title = "Read and Write Bandwidth", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.brw", height = 1000)
          ),
          box( title = "Read and Write Size", status = "primary", width = 12, height = 1060, solidHeader = TRUE,
               collapsible = TRUE, plotOutput("host.srw", height = 1000)
          )
        )
      ),
      tabItem( tabName = "relations",
        fluidRow(
          box( title = "Components", status = "warning", solidHeader = TRUE, collapsible = TRUE,
               radioButtons( "rel.component", label = h3("Select component"),
                             choices = list( "SP" = 1, "LUNs" = 3, "Disks" = 4, "Hosts" = 5 ),
                             selected = 3
               )
          ),     
          box( title = "Parameters", status = "warning", solidHeader = TRUE, collapsible = TRUE,
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
               plotOutput( "rel.plot", brush = "rel.brush" ),
               "Brush to select points", br(),
               verbatimTextOutput("rel.info")
          )
        )  
      ),          
      tabItem( tabName = "summary",
        fluidRow(
          box( title = "Components", status = "warning", solidHeader = TRUE, collapsible = TRUE,
               radioButtons( "summary.component", label = h3("Select component"),
                             choices = list( "SP" = 1, "Port" = 2, "LUNs" = 3, "Disks" = 4, "Hosts" = 5 ),
                             selected = 1 )
          ),     
          box( title = "Summary", status = "primary", width = 12, solidHeader = TRUE,
               dataTableOutput("summary")
          )
        )  
      ),          
      tabItem( tabName = "report",
        fluidRow(
          box( title = "Report", status = "primary", width = 12, solidHeader = TRUE,
               "Download a full report in pdf format.", br(),
               "Generating report takes about 15 - 20 min. Please be patient.", br(), br(),
               downloadButton( 'downloadReport', label = "Download" )
          )
        )
      )
    )
  )
)
