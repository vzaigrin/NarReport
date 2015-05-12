library(shiny)
library(data.table)
library(stringr)
library(dygraphs)


options(shiny.maxRequestSize=300*1024^2)

shinyUI(fluidPage(
    
    titlePanel("Visualisation data from NAR files"),
    helpText("Upload CSV file (up to 100MB), extracted from some NAR files from one disk array."),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file",label=h3("Choose CSV file"),accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
            hr(),
            radioButtons("component",label=h3("Select component"),choices=list("SP"=1,"LUNs"=2,"Disks"=3),selected=1),
            hr(),
            h3("Options for Plots"),
            radioButtons("type",label=h4("Select type"),choices=list("total"=1,"read"=2,"write"=3),selected=1),
            checkboxGroupInput("object",label=h4("Select object"),choices=list("SP A"="SP A","SP B"="SP B"),selected="SP A"),
            hr(),
            h3("Options for Combinations"),
            radioButtons("stat",label=h4("Select stat"),choices=list("mean"=1,"95th"=2,"max"=3),selected=2)
        ),

        mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            verbatimTextOutput("values"),
            tabsetPanel(type="tabs",
                tabPanel("Plot",
                         dygraphOutput("dygraph1"),
                         dygraphOutput("dygraph2"),
                         dygraphOutput("dygraph3"),
                         dygraphOutput("dygraph4")
                        ),
                tabPanel("Combinations",
                         plotOutput("util_iops"),
                         plotOutput("abql_rt"),
                         plotOutput("band_iops"),
                         plotOutput("write_read")
                        ),
                tabPanel("Summary",dataTableOutput("summary")),
                tabPanel("Table",dataTableOutput("table"))
            )
        )
    )
))
