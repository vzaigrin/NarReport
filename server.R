library(shiny)
library(rmarkdown)
library("data.table")
library("stringr")
library("ggplot2")
library("grid")
library("scales")
library("XML")

data2sum <- function( dt ) {
  
  s1 <- dt[, list( "mean", round( mean(util), digits = 2 ),
                     round( mean(iops), digits = 2 ),
                     round( mean(riops), digits = 2 ),
                     round( mean(wiops), digits = 2 ),
                     round( mean(bw), digits = 2 ),
                     round( mean(rbw), digits = 2 ),
                     round( mean(wbw), digits = 2 ),
                     round( mean(rsize), digits = 2 ),
                     round( mean(wsize), digits = 2 ),
                     round( mean(rt), digits = 2 ),
                     round( mean(st), digits = 2 ),
                     round( mean(ql), digits = 2 )
  ), by = list( object, name, owner ) ]
  
  s2 <- dt[, list( "95th", round( quantile( util, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( iops, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( riops, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( wiops, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( bw, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( rbw, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( wbw, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( rsize, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( wsize, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( rt, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( st, c(0.95), na.rm = TRUE ), digits = 2 ),
                     round( quantile( ql, c(0.95), na.rm = TRUE ), digits = 2 )
  ), by = list( object, name, owner ) ]
  
  s3 <- dt[, list( "max", round( max(util), digits = 2 ),
                     round( max(iops), digits = 2 ),
                     round( max(riops), digits = 2 ),
                     round( max(wiops), digits = 2 ),
                     round( max(bw), digits = 2 ),
                     round( max(rbw), digits = 2 ),
                     round( max(wbw), digits = 2 ),
                     round( max(rsize), digits = 2 ),
                     round( max(wsize), digits = 2 ),
                     round( max(rt), digits = 2 ),
                     round( max(st), digits = 2 ),
                     round( max(ql), digits = 2 )
  ), by = list( object, name, owner ) ]
  
  sum <- rbind( s1, s2, s3 )
  setnames( sum, c( "object", "name", "owner", "stat", "util", "iops", "riops", "wiops", "bw", "rbw", "wbw", "rsize", "wsize", "rt", "st", "ql" ) )
  
  return( sum )
}

data2type <- function( dt ) {
  
  tt <- dt[, list( object, date, name, owner, iops, bw ) ]
  tt$size <- 0
  tt$type <- "total"
  
  rt <- dt[, list( object, date, name, owner, riops, rbw, rsize ) ]
  setnames( rt, c( "object", "date", "name", "owner", "iops", "bw", "size" ) )
  rt$type <- "read"
  
  wt <- dt[, list( object, date, name, owner, wiops, wbw, wsize ) ]
  setnames( wt, c( "object", "date", "name", "owner", "iops", "bw", "size" ) )
  wt$type <- "write"
  
  nt <- rbind( tt, rt, wt )
  
  return( nt )
}


shinyServer( function(input, output, session) {

  datafile <- reactive({
    if( is.null(input$file1) )  return(NULL)
    input$file1
  })

  configfile <- reactive({
    if( is.null(input$file2) )  return(NULL)
    input$file2
  })

  relfile <- reactive({
    if( is.null(input$file3) )  return(NULL)
    input$file3
  })

  config.doc <- reactive({
    if( is.null(input$file2) )  return(NULL)
    config.doc <- xmlParse( configfile()$datapath )
    return(config.doc)
  })
    
  rel.doc <- reactive({
    if( is.null(input$file3) )  return(NULL)
    rel.doc <- xmlParse( relfile()$datapath )
    return(rel.doc)
  })

  array.name <-reactive({
    if( is.null(input$file2) )  return(NULL)
    array.name <- xpathSApply( config.doc(), "//object[@type='Subsystem']//value[@type='Name']", xmlValue )
    return(array.name)
  })
  
  array.sn <- reactive({
    if( is.null(input$file2) )  return(NULL)
    array.sn <- xpathSApply( config.doc(), "//object[@type='Subsystem']//value[@type='Array Serial Number']", xmlValue )
    return(array.sn)    
  }) 
  
  rawdata <- reactive({
    if( is.null(input$file1) )  return(NULL)
    rawdata <- read.csv( datafile()$datapath, stringsAsFactor = FALSE )
    rawdata <- rawdata[ rawdata$Owner.Array.Name == array.name(), ]
    return(rawdata)
  })
  
  data <- reactive({
    if( is.null(input$file1) )  return(NULL)
    raw <- rawdata()
    data <- data.table( object = raw$Object.Name,
                        date = as.POSIXct( strptime( raw$Poll.Time,"%m/%d/%Y %H:%M:%S" ) ),
                        owner = raw$Current.Owner,
                        util = raw$Utilization...,
                        iops = raw$Total.Throughput..IO.s.,
                        riops = raw$Read.Throughput..IO.s.,
                        wiops = raw$Write.Throughput..IO.s.,
                        bw = raw$Total.Bandwidth..MB.s.,
                        rbw = raw$Read.Bandwidth..MB.s.,
                        wbw = raw$Write.Bandwidth..MB.s.,
                        rsize = raw$Read.Size..KB.,
                        wsize = raw$Write.Size..KB.,
                        rt = raw$Response.Time..ms,
                        st = raw$Service.Time..ms,
                        ql = raw$Queue.Length
    )
    data <- data[ as.Date(date) %between% input$dateRange, ]
    return(data)
  })
  
  observe({
    if( is.null(input$file1) )  return(NULL)
    rdt <- rawdata()
    dstart <- unlist(strsplit(head(rdt$Poll.Time,1)," "))[1]
    dend <- unlist(strsplit(tail(rdt$Poll.Time,1)," "))[1]
    updateDateRangeInput( session, "dateRange", start = dstart, end = dend, min = dstart, max = dend )
  })
  
  observe({
    if( is.null(input$file1) )  return(NULL)
    switch( input$conf.component,
            "1" = updateSelectInput( session, "conf.element", choices = levels(as.factor(lun.hosts()$host)) ),
            "2" = updateSelectInput( session, "conf.element", choices = levels(as.factor(lun.hosts()$name)) ),
            "3" = updateSelectInput( session, "conf.element", choices = levels(as.factor(disk.pools()$pool)) ),
            "4" = updateSelectInput( session, "conf.element", choices = levels(as.factor(disk.pools()$disk)) )
    )
  })
  
  sp.data <- reactive({
    if( is.null(input$file1) )  return(NULL)
    dt <- data()
    sp.data <- dt[ object == "SP A" | object == "SP B", ]
    sp.data$name <- sp.data$object
    return(sp.data)
  })
  
  sp.sum <- reactive({
    if( is.null(input$file1) )  return(NULL)
    sp.sum <- data2sum( sp.data() )
    return(sp.sum)
  })
  
  sp.type <- reactive({
    if( is.null(input$file1) )  return(NULL)
    sp.type <- data2type( sp.data() )
    return(sp.type)
  })

  port.data <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    dt <- data()
    rd <- rel.doc()
    port.data <- dt[ grepl( "Port ([0-9]+)", object ), ]
    port.data$wwn = str_replace_all( port.data$object, "Port ([0-9]+) \\[FC; ([0-9A-F:]+) \\]", "\\2" )
    spaports <- xpathSApply( rd, "//object[@name='SP A']/object[@type='Port']", xmlAttrs )[2,]
    spbports <- xpathSApply( rd, "//object[@name='SP B']/object[@type='Port']", xmlAttrs )[2,]
    ports <- rbindlist( list(
      data.table(
        name = str_replace_all( spaports, "Port ([0-9]+) \\[FC; ([0-9A-F:]+) \\]", "A\\1" ),
        wwn = str_replace_all( spaports, "Port ([0-9]+) \\[FC; ([0-9A-F:]+) \\]", "\\2" )
      ),
      data.table(
        name = str_replace_all( spbports, "Port ([0-9]+) \\[FC; ([0-9A-F:]+) \\]", "B\\1" ),
        wwn = str_replace_all( spbports, "Port ([0-9]+) \\[FC; ([0-9A-F:]+) \\]", "\\2" )
      )
    ))
    port.data <- merge( port.data, ports, by='wwn', all = TRUE )
    port.data$object <- port.data$name
    port.data[ grepl( "A([0-9]+)", object), ]$owner <- "SPA"
    port.data[ grepl( "B([0-9]+)", object), ]$owner <- "SPB"
    return(port.data)
  })
  
  port.sum <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    port.sum <- data2sum( port.data() )
    return(port.sum)
  })
  
  port.type <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    port.type <- data2type( port.data() )
    port.type$object <- str_replace_all( port.type$object, "([AB])([0-9]+)", "\\2")
    return(port.type)
  })
  
  lun.owners <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    rd <- rel.doc()
    lun.owners <- rbindlist( lapply( c(
      xpathSApply( rd, "//object[@type='RAID Group']",xmlGetAttr,'name' ),
      xpathSApply( rd, "//object[@type='Pool']",xmlGetAttr,'name' ) ),
      function(x) data.table( lun = xpathSApply( rd,
                                                 paste("//object[@name='",x,"']/object[@type='Pool Public LUN' or
                                                       @type='Public RaidGroup LUN' or @type='Private RaidGroup LUN']",
                                                       sep=""),
                                                 xmlGetAttr, 'name' ),
                              owner = x )
    ))
    lun.owners$name <- str_replace_all( lun.owners$lun, "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\1" )
    lun.owners$object <- str_replace_all( lun.owners$lun, "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\2" )
    lun.owners$owner <- str_replace_all( lun.owners$owner, "(Raid Group) ([0-9]+)", "RG \\2" )
    return(lun.owners)
  })
  
  lun.pool.host <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    lun.pools <- lun.owners()
    lun.pools$lun <- NULL
    setnames( lun.pools, c( "pool", "name", "lun" ) )
    lun.pool.host <- merge( lun.pools, lun.hosts(), by=c('lun','name'), all = FALSE )
    return(lun.pool.host)
  })

  lun.data <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    lun.data <- data()[ grepl( "([[:print:]]+) \\[([0-9]+);", object ), ]
    lun.data$name <- str_replace_all( lun.data$object, "([[:print:]]+) \\[([0-9]+)([[:print:]]+)","\\1" )
    lun.data$object <- str_replace_all( lun.data$object, "([[:print:]]+) \\[([0-9]+)([[:print:]]+)","\\2" )
    lun.data$sp <- paste( "SP", lun.data$owner )
    lun.data <- merge( lun.data, lun.owners(), by='object', all = TRUE )
    lun.data$owner <- lun.data$owner.y
    lun.data$name <- lun.data$name.y
    lun.data$owner.x <- NULL
    lun.data$owner.y <- NULL
    lun.data$name.x <- NULL
    lun.data$name.y <- NULL
    lun.data <- lun.data[ !is.na(date), ]
    return(lun.data)
  })

  lun.sum <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    lun.sum <- data2sum( lun.data() )
    return(lun.sum)
  })
  
  lun.type <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    lun.type <- data2type( lun.data() )
    return(lun.type)
  })
  
  disk.owners <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    rd <- rel.doc()
    disk.owners <- rbindlist( lapply( c(
      xpathSApply( rd, "//object[@type='RAID Group']",xmlGetAttr,'name' ),
      xpathSApply( rd, "//object[@type='Pool']",xmlGetAttr,'name' ) ),
      function(x) data.table( disk = xpathSApply( rd, paste("//object[@name='",x,"']/object[@type='Disk']", sep=""),
                                                  xmlGetAttr, 'name' ),
                              owner = x )
    ))
    disk.owners$name <- disk.owners$disk
    disk.owners$object <- str_replace_all( disk.owners$name, "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)", "\\1_\\2_\\3" )
    disk.owners$owner <- str_replace_all( disk.owners$owner, "(Raid Group) ([0-9]+)", "RG \\2" )
    return(disk.owners)
  })
  
  disk.pools <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    disk.pools <- disk.owners()
    disk.pools$disk <- NULL
    disk.pools$name <- NULL
    setnames( disk.pools, c( "pool", "disk" ) )
    return(disk.pools)
  })
  
  disk.data <- reactive({
    if( is.null(input$file1) | is.null(input$file2) | is.null(input$file3) )  return(NULL)
    dt <- data()
    cd <- config.doc()
    disk.data <- dt[ grepl( "Bus [0-9]", object ), ]
    disk.data$name <- disk.data$object
    disk.data$bus <- str_replace_all( disk.data$name,
                                      "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)",
                                      "\\1" )
    disk.data$object <- str_replace_all( disk.data$name,
                                         "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)",
                                         "\\1_\\2_\\3" )
    disk.config <- as.data.table( cbind(
      xpathSApply( cd, "//object[@type='Disk']//value[@type='Name']", xmlValue ),
      xpathSApply( cd, "//object[@type='Disk']//value[@type='Drive Type']", xmlValue )
    ))
    names(disk.config) <- c("name","type")
    disk.config$object <- str_replace_all( disk.config$name, "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)", "\\1_\\2_\\3" )
    disk.co <- merge( disk.config, disk.owners(), by = "object", all = TRUE )
    disk.co$name.x <- NULL
    disk.co$name.y <-NULL
    disk.co$disk <- NULL
    disk.data <- merge( disk.data, disk.co, by = "object", all = TRUE )
    disk.data$owner <- disk.data$owner.y
    disk.data$owner.x <- NULL
    disk.data$owner.y <- NULL
    disk.data$name <- disk.data$type
    return(disk.data)
  })
  
  disk.sum <- reactive({
    if( is.null(input$file1) | is.null(input$file2) | is.null(input$file3) )  return(NULL)
    disk.sum <- data2sum( disk.data() )
    return(disk.sum)
  })
  
  disk.type <- reactive({
    if( is.null(input$file1) | is.null(input$file2) | is.null(input$file3) )  return(NULL)
    dd <- disk.data()
    dd$owner <- dd$bus
    disk.type <- data2type( dd )
    return(disk.type)
  })
  
  lun.hosts <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    rd <- rel.doc()
    lun.hosts <- rbindlist( lapply( c(
      xpathSApply( rd, "//object[@type='SP']//object[@type='Public RaidGroup LUN']", xmlGetAttr, 'name' ),
      xpathSApply( rd, "//object[@type='SP']//object[@type='Pool Public LUN']", xmlGetAttr, 'name' )),
      function(x) data.table(
        lun = x,
        host = xpathSApply( rd, paste( "//object[@type='SP']//object[@name='",x,"']/object[@type='Host']", sep="" ),
                            xmlGetAttr, 'name' )
      )
    ))
    lun.hosts <- lun.hosts[ host != 'NULL', ]
    lun.hosts$lun.name <- str_replace_all( lun.hosts$lun, "([[:print:]]+) \\[([0-9]+)([[:print:]]+)","\\1" )
    lun.hosts$lun <- str_replace_all( lun.hosts$lun, "([[:print:]]+) \\[([0-9]+)([[:print:]]+)","\\2" )
    lun.hosts$host <- unlist(lun.hosts$host)
    setnames(lun.hosts, c("lun", "host", "name"))
    return(lun.hosts)
  })
  
  host.data <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    host.data <- data()[ grepl( "([[:print:]]+) \\[([0-9]+);", object ), ]
    host.data$lun <- str_replace_all( host.data$object, "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)", "\\2" )
    host.data$lun.name <- str_replace_all( host.data$object, "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)", "\\1" )
    host.data <- rbindlist( lapply( levels(as.factor(lun.hosts()$host)),
                                    function(x) cbind(host.data[ lun %in% lun.hosts()[ host == x, lun]], host=x) ) )
    host.data$object <- host.data$lun
    host.data$name <- host.data$lun.name
    host.data$owner <- host.data$host
    host.data$lun <- NULL
    host.data$lun.name <- NULL
    host.data$host <- NULL
    return(host.data)
  })
  
  host.sum <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    host.sum <- data2sum( host.data() )
    return(host.sum)
  })
  
  host.type <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    host.type <- data2type( host.data() )
    return(host.type)
  })
  
  hds.sum <- reactive({
    if( is.null(input$file1) | is.null(input$file3) )  return(NULL)
    hds <- host.data()[, list( mean(util), sum(iops), sum(riops), sum(wiops),
                               sum(bw), sum(rbw), sum(wbw), mean(rsize), mean(wsize),
                               mean(rt), mean(st), mean(ql) ), by = list( owner, date ) ]
    setnames( hds, c( "object", "date", "util", "iops", "riops", "wiops",
                      "bw", "rbw", "wbw", "rsize", "wsize", "rt", "st", "ql") )
    hds$name <- hds$object
    hds$owner <- ""
    hds.sum <- data2sum( hds )
    return(hds.sum)
  })

  output$ui <- renderUI({
    if( is.null(input$file1) | is.null(input$file2) | is.null(input$file3) )  return()
    fluidPage(
      "Performance analysis of the array ", strong( array.name() ), ", S/N: ", strong( array.sn() ),
      br(), "Data taken from ", strong( head( rawdata()$Poll.Time, 1 ) ),
      " to ", strong( tail( rawdata()$Poll.Time, 1 ) )
    )
  })
  
  output$downloadReport <- downloadHandler(
    filename = 'report.pdf',
    content = function( file ) {
      if( is.null(input$file1) )  return(NULL)
      rmarkdown::render( 'report.Rmd', params = list( file1 = datafile()$datapath,
                                                      file2 = configfile()$datapath,
                                                      file3 = relfile()$datapath ),
                         output_format = 'pdf_document', output_file = file )
    }
  )
  
  output$sp.util <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.data(), aes( x = date, y = util, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Utilization %" ) +
      ggtitle( "Utilization on SPA and SPB with 95th percentile" ) + 
      geom_line( y = quantile( sp.data()[ object == "SP A", ]$util, c(0.95) ),
                 colour = "#FF6666", size = 1 ) + 
      geom_line( y = quantile( sp.data()[ object == "SP B", ]$util, c(0.95) ),
                 colour = "cyan3", size = 1 ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$sp.tt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.type()[ type == "total", ], aes( x = date, y = iops ) ) +
      geom_area ( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput on SPA and SPB with 95th percentile" ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP A", ]$iops, c(0.95) ),
                 colour = "#FF6666", size = 1 ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP B", ]$iops, c(0.95) ),
                 colour = "cyan3", size = 1 ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$sp.trw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.type(), aes( x = date, y = iops, colour = type ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total, Read and Write Throughput on both SP" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) +
      facet_grid( object ~ ., scales = "free" ) + 
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
    
  })
  
  output$sp.bt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "MB/s" ) +
      ggtitle( "Total Bandwidth on SPA and SPB with 95th percentile" ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP A", ]$bw, c(0.95) ),
                 colour = "#FF6666", size = 1 ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP B", ]$bw, c(0.95) ),
                 colour = "cyan3", size = 1 ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })  
  
  output$sp.brw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.type(), aes( x = date, y = bw, colour = type ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) + 
      ggtitle( "Total, Read and Write Bandwidth on both SP" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) + 
      facet_grid( object ~ ., scales = "free" ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$sp.srw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = sp.type()[ type == "read" | type == "write" ],
            aes( x = date, y = size, colour = type ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) + 
      ggtitle( "Read and Write Size on both SP" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( object ~ ., scales = "free" ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })

  output$port.tt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = port.type()[ type == "total", ], aes( x = date, y = iops ) ) +
      geom_area ( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput on front-end ports" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) ) +
      facet_grid( . ~ owner, scales = "free" )
  })
  
  output$port.trw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = port.type(), aes( x = date, y = iops, colour = type ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total, Read and Write Throughput on front-end ports" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) +
      facet_grid( object ~ owner, scales = "free" ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$port.bt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = port.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "MB/s" ) +
      ggtitle( "Total Bandwidth on front-end port" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) ) +
      facet_grid( . ~ owner, scales = "free" )
  })  
  
  output$port.brw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = port.type(), aes( x = date, y = bw, colour = type ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) + 
      ggtitle( "Total, Read and Write Bandwidth on front-end port" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) + 
      facet_grid( object ~ owner, scales = "free" ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$port.srw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = port.type()[ type == "read" | type == "write" ],
            aes( x = date, y = size, colour = type ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) + 
      ggtitle( "Read and Write Size on front-end ports" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( object ~ owner, scales = "free" ) +
      theme( legend.title = element_blank(), strip.text.y = element_text(angle = 0) )
  })
  
  output$lun.hist <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ls <- lun.sum()[ stat == "95th", ]
    ls <- ls[ order( ls$iops, decreasing = TRUE ), ]
    ls$n <- c( 1 : dim(ls)[1] )
    ggplot( data = ls, aes( n, iops ) ) +
      geom_bar( stat = "identity", aes( fill = owner ) ) +
      ylab( "IOPS" ) + xlab( "LUNs" ) + 
      ggtitle( "Total IOPS on all LUNs by 95th percentile" ) +
      scale_x_discrete( limits = c( 1 : dim(ls)[1] ), label = function(x) { paste( ls$object, ": ", ls[ x, ]$name ) } ) +
      theme( axis.text.x = element_text( angle = 90, size = rel(0.3) ),
             legend.title = element_blank() )
  })

  output$lun.tt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = lun.type()[ type == "total", ], aes( x = date, y = iops, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') +
      xlab("") + ylab( "IOPS" ) + ggtitle( "Total Throughput of all LUNs" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) + 
      theme( legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$lun.trw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ], aes( x = date, y = iops, colour = object ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Read and Write Throughput of all LUNs" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  
  
  output$lun.bt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = lun.type()[ type == "total", ], aes( x = date, y = bw, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') +
      xlab("") + ylab( "MB/s" ) + ggtitle( "Total Bandwidth of all LUNs" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) + 
      theme( legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  
  
  output$lun.brw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ], aes( x = date, y = bw, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') + xlab("") +
      ylab( "MB/s" ) + ggtitle( "Read and Write Bandwidth of all LUNs on SP A and SP B" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$lun.srw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ], aes( x = date, y = size, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) +
      ggtitle( "Read and Write Size of all LUNs on both SP" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  

  dhm <- reactive({
    if( is.null(input$file1) )  return(NULL)
    dhm <- disk.sum()[ stat == "95th", .( object, owner, util ) ]
    dhm$dae <- str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\1_\\2" )
    dhm$disk <- as.numeric( str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\3" ) )
    return(dhm)
  })
    
  output$disk.heat <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    dhm <- disk.sum()[ stat == "95th", .( object, owner, util ) ]
    dhm$dae <- str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\1_\\2" )
    dhm$disk <- as.numeric( str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\3" ) )
    dhm[ is.na(dhm$owner), ]$owner <- ""
    ggplot( data = dhm, aes( disk, dae ) ) + geom_tile( aes( fill = util ), colour = "black" ) +
      scale_fill_gradient( low = "green", high = "red", limits=c( 0, 100 ) ) + 
      xlab( "Disk" ) + ylab( "DAE" ) +
      ggtitle( "Disks Utilization Hetamap by 95th percentile" ) +
      scale_x_discrete( limits = c( 0 : max( dhm$disk ) ) ) +
      theme_classic() + theme( legend.title = element_blank() ) + geom_text( aes( label = owner ), size = 2.8 )
  })

  output$disk.tt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = disk.type()[ type == "total", ], aes( x = date, y = iops, colour = object ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) +
      theme( legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$disk.trw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = disk.type()[ type == "read" | type == "write", ],
            aes( x = date, y = iops, colour = object ) ) + geom_line() +
      xlab("") + ylab( "IOPS" ) + 
      ggtitle( "Read and Write Throughput of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  

  output$disk.bt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = disk.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object ), position = 'stack') +
      xlab("") + ylab( "MB/s" ) + ggtitle( "Total Bandwidth of all Disks by Buses" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$disk.brw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = disk.type()[ type == "read" | type == "write", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_line() +
      xlab("") + ylab( "MB/s" ) +
      ggtitle( "Read and Write Bandwidth of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  

  output$host.hist <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    hs <- hds.sum()[ stat == "95th", ]
    hs <- hs[ order( hs$iops, decreasing = TRUE ), ]
    hs$n <- c( 1 : dim(hs)[1] )
    ggplot( data = hs, aes( n, iops ) ) +
      geom_bar( stat = "identity", fill="blue" ) +
      ylab( "IOPS" ) + xlab( "" ) + 
      ggtitle( "Total IOPS on all hosts by 95th percentile" ) +
      scale_x_discrete( limits = c( 1 : dim(hs)[1] ), label = hs$object ) +
      theme( axis.text.x = element_text( angle = 90, size = 10, face="bold" ),
             legend.title = element_blank() )
  })    

  output$host.tt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = host.type()[ type == "total", ], aes( x = date, y = iops, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') +
      xlab("") + ylab( "IOPS" ) + ggtitle( "Total Throughput of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free"  ) + 
      theme( legend.position = "none", strip.text.y = element_text(angle = 0) )
  })

  output$host.trw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write", ],
            aes( x = date, y = iops, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') +
      xlab("") + ylab( "IOPS" ) + ggtitle( "Read and Write Throughput of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  

  output$host.bt <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = host.type()[ type == "total", ], aes( x = date, y = bw, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') +
      xlab("") + ylab( "MB/s" ) + ggtitle( "Total Bandwidth of all hosts" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) + 
      theme( legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$host.brw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack') + xlab("") +
      ylab( "MB/s" ) + ggtitle( "Read and Write Bandwidth of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })
  
  output$host.srw <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write" ],
            aes( x = date, y = size, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) +
      ggtitle( "Read and Write Size of all LUNs on both SP" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none", strip.text.y = element_text(angle = 0) )
  })  
  
  output$summary <- renderDataTable({
    if( is.null(input$file1) )  return(NULL)
    switch( input$summary.component,
            "1" = sp.sum(),
            "2" = port.sum(),
            "3" = lun.sum(),
            "4" = disk.sum(),
            "5" = hds.sum()
    )
  }) 
  
  output$lun.config <- renderDataTable({
    if( is.null(input$file1) )  return(NULL)
    switch( input$conf.component,
            "1" = lun.pool.host()[ host == input$conf.element, ],
            "2" = lun.pool.host()[ name == input$conf.element, ],
            "3" = lun.pool.host()[ pool == input$conf.element, ],
            "4" = lun.pool.host()[ pool %in% disk.pools()[ disk == input$conf.element, ]$pool, ]
    )
  })
  
  output$pool.config <- renderDataTable({
    if( is.null(input$file1) )  return(NULL)
    switch( input$conf.component,
            "1" = disk.pools()[ pool %in% lun.pool.host()[ host == input$conf.element, ]$pool, ],
            "2" = disk.pools()[ pool %in% lun.pool.host()[ name == input$conf.element, ]$pool, ],
            "3" = disk.pools()[ pool == input$conf.element, ],
            "4" = disk.pools()[ disk == input$conf.element, ]
    )
  })
  
  output$pool.disk <- renderDataTable({
    if( is.null(input$file1) )  return(NULL)
    disk.pools <- disk.owners()
    disk.pools$disk <- NULL
    disk.pools$name <- NULL
    setnames( disk.pools, c( "pool", "disk" ) )
    disk.pools
  })
  
  rel.data <- reactive({
    if( is.null(input$file1) )  return(NULL)
    dt <- switch( input$rel.component,
                   "1" = sp.sum()[ stat == "95th", ],
                   "3" = lun.sum()[ stat == "95th", ],
                   "4" = disk.sum()[ stat == "95th", ],
                   "5" = hds.sum()[ stat == "95th", ]
    )
    return(dt)
  })

  output$rel.plot <- renderPlot({
    if( is.null(input$file1) )  return(NULL)
    ggplot( data = rel.data(), aes_string( x = input$rel.x, y = input$rel.y ) ) + 
      geom_point( aes_string( size = input$rel.size, colour = input$rel.colour ) ) + 
      scale_color_gradient( low = "green", high = "red" )
  })
  
  output$rel.info <- renderPrint({
    if( is.null(input$file1) )  return(NULL)
    brushedPoints( as.data.frame( rel.data() ), input$rel.brush )
  })

  output$dhm.clickinfo <- renderPrint({
    if( is.null(input$file1) )  return(NULL)
    disk.sum()[ object == dhm()[ dae == levels(as.factor(dhm()$dae))[as.integer(input$dhm.click$y+0.5)] &
                                 disk == as.integer(input$dhm.click$x+0.5), object ], ]
  })
  
  
})
