library(shiny)
library(rmarkdown)
library(xts)
library(dygraphs)
library(data.table)
library(stringr)
library(ggplot2)
library(grid)
library(scales)

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
    if( is.null( input$file ) )  return(NULL)
    input$file
  })
  
  rawdata <- reactive({
    if( is.null( input$file ) )  return(NULL)
    rawdata <- read.csv( datafile()$datapath, stringsAsFactor = FALSE )
    arrayname <- rawdata$Owner.Array.Name[1]
    rawdata <- rawdata[ rawdata$Owner.Array.Name == arrayname, ]
    return(rawdata)
  })

  data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    raw <- rawdata()
    data <- data.table( object = raw$Object.Name,
                        date = as.POSIXct( strptime( raw$Poll.Time, "%m/%d/%Y %H:%M:%S" ) ),
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
    return(data)
  })
  
  sp.data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    dt <- data()
    sp.data <- dt[ object == "SP A" | object == "SP B", ]
    sp.data$name <- sp.data$object
    return(sp.data)
  })
  
  sp.sum <- reactive({
    if( is.null( input$file ) )  return(NULL)
    sp.sum <- data2sum( sp.data() )
    return(sp.sum)
  })
  
  sp.type <- reactive({
    if( is.null( input$file ) )  return(NULL)
    sp.type <- data2type( sp.data() )
    return(sp.type)
  })
  
  lun.data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    dt <- data()
    lun.data <- dt[ grepl( "([[:print:]]+) \\[([0-9]+);", object ), ]
    lun.data$name <- str_replace_all( lun.data$object,
                                      "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\1" )
    lun.data$object <- str_replace_all( lun.data$object,
                                        "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\2" )
    lun.data$owner <- paste( "SP", lun.data$owner )
    return(lun.data)
  })

  lun.sum <- reactive({
    if( is.null( input$file ) )  return(NULL)
    lun.sum <- data2sum( lun.data() )
    return(lun.sum)
  })
  
  lun.type <- reactive({
    if( is.null( input$file ) )  return(NULL)
    lun.type <- data2type( lun.data() )
    return(lun.type)
  })
  
  disk.data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    dt <- data()
    disk.data <- dt[ grepl( "Bus [0-9]", object ), ]
    disk.data$name <- disk.data$object
    disk.data$owner <- str_replace_all( disk.data$name,
                                        "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)",
                                        "\\1" )
    disk.data$object <- str_replace_all( disk.data$name,
                                         "Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)",
                                         "\\1_\\2_\\3" )
    return(disk.data)
  })
  
  disk.sum <- reactive({
    if( is.null( input$file ) )  return(NULL)
    disk.sum <- data2sum( disk.data() )
    return(disk.sum)
  })
  
  disk.type <- reactive({
    if( is.null( input$file ) )  return(NULL)
    disk.type <- data2type( disk.data() )
    return(disk.type)
  })
  
  host.data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    dt <- data()
    host.data <- dt[ grepl( "([[:print:]]+) \\[([0-9]+);", object ), ]
    host.data$lun <-  str_replace_all( host.data$object,
                                       "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\2" )
    host.data$lun.name <- str_replace_all( host.data$object,
                                           "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\1" )
    host.data$object <- str_replace_all( host.data$object,
                                         "([[:print:]]+) \\[([0-9]+); ([[:print:]]+)\\]",
                                         "\\3" )
    host.data$object <- str_replace_all( host.data$object,"RAID ([0-9_]+)", "" )
    host.data$object <- str_replace_all( host.data$object,"^; ", "" )
    host.data <- host.data[ object != "", ]
    host.data <- host.data[, list( mean(util), sum(iops), sum(riops), sum(wiops),
                                 sum(bw), sum(rbw), sum(wbw), mean(rsize), mean(wsize),
                                 mean(rt), mean(st), mean(ql) ), by = list( object, date ) ]
    setnames( host.data, c( "object", "date", "util", "iops", "riops", "wiops",
                          "bw", "rbw", "wbw", "rsize", "wsize", "rt", "st", "ql") )
    host.data$name <- host.data$object
    host.data$owner <- ""
    return(host.data)
  })
  
  host.sum <- reactive({
    if( is.null( input$file ) )  return(NULL)
    host.sum <- data2sum( host.data() )
    return(host.sum)
  })
  
  host.type <- reactive({
    if( is.null( input$file ) )  return(NULL)
    host.type <- data2type( host.data() )
    return(host.type)
  })

  output$ui <- renderUI({
    if( is.null( input$file ) )  return()
    fluidPage(
      "Performance analysis of the array ", strong( rawdata()$Owner.Array.Name[1] ),
      br(), "Data taken from ", strong( head( rawdata()$Poll.Time, 1 ) ),
      " to ", strong( tail( rawdata()$Poll.Time, 1 ) )
    )
  })
  
  output$downloadReport <- downloadHandler(
    filename = 'report.pdf',
    content = function( file ) {
      if( is.null( input$file ) )  return(NULL)
      rmarkdown::render( 'report.Rmd', params = list( file = datafile()$datapath ),
                         output_format = 'pdf_document', output_file = file )
    }
  )
  
  output$sp.util <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.data(), aes( x = date, y = util, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Utilization %" ) +
      ggtitle( "Utilization on SPA and SPB with 95th percentile" ) + 
      geom_line( y = quantile( sp.data()[ object == "SP A", ]$util, c(0.95) ),
                 colour = "#FF6666", size = 1 ) + 
      geom_line( y = quantile( sp.data()[ object == "SP B", ]$util, c(0.95) ),
                 colour = "cyan3", size = 1 ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank() )
    
  })
  
  output$sp.util.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.util.click ) )  return(NULL)
    ds <- nearPoints( sp.data()[, .( object, date, util) ], input$sp.util.click )
    ds[ order( ds$util, decreasing = TRUE ), ]
  })
  
  output$sp.tt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.type()[ type == "total", ], aes( x = date, y = iops ) ) +
      geom_area ( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput on SPA and SPB with 95th percentile" ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP A", ]$iops, c(0.95) ),
                 colour = "#FF6666", size = 1 ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP B", ]$iops, c(0.95) ),
                 colour = "cyan3", size = 1 ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank() )
  })

  output$sp.tt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.tt.click ) )  return(NULL)
    dt <- sp.type()[ type == "total", ]
    selected <- abs( dt$date - as.POSIXct( input$sp.tt.click$x, origin = "1970-01-01" ) ) < 5
    ds <- dt[ selected, .( object, date, iops ) ]
    ds[ order( ds$iops, decreasing = TRUE ), ]
  })
  
  output$sp.trw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.type(), aes( x = date, y = iops, colour = type ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total, Read and Write Throughput on both SP" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) +
      facet_grid( object ~ ., scales = "free" ) + theme( legend.title = element_blank() )
  })
  
  output$sp.trw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.trw.click ) )  return(NULL)
    ds <- nearPoints( sp.type()[, .( object, date, iops, type ) ], input$sp.trw.click )
    ds[ order( ds$iops, decreasing = TRUE ), ]
  })
  
  output$sp.bt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "MB/s" ) +
      ggtitle( "Total Bandwidth on SPA and SPB with 95th percentile" ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP A", ]$bw, c(0.95) ),
                 colour = "#FF6666", size = 1 ) +
      geom_line( y = quantile( sp.type()[ type == "total" & object == "SP B", ]$bw, c(0.95) ),
                 colour = "cyan3", size = 1 ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank() )
  })  
  
  output$sp.bt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.bt.click ) )  return(NULL)
    dt <- sp.type()[ type == "total", ]
    selected <- abs( dt$date - as.POSIXct( input$sp.bt.click$x, origin = "1970-01-01" ) ) < 5
    ds <- dt[ selected, .( object, date, bw ) ]
    ds[ order( ds$bw, decreasing = TRUE ), ]
  })
  
  output$sp.brw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.type(), aes( x = date, y = bw, colour = type ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) + 
      ggtitle( "Total, Read and Write Bandwidth on both SP" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      scale_color_manual( values = c( "#FF6666", "black", "cyan3" ) ) + 
      facet_grid( object ~ ., scales = "free" ) + theme( legend.title = element_blank() )
  })
  
  output$sp.brw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.brw.click ) )  return(NULL)
    ds <- nearPoints( sp.type()[, .( object, date, bw, type ) ], input$sp.brw.click )
    ds[ order( ds$bw, decreasing = TRUE ), ]
  })
  
  output$sp.srw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = sp.type()[ type == "read" | type == "write" ],
            aes( x = date, y = size, colour = type ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) + 
      ggtitle( "Read and Write Size on both SP" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( object ~ ., scales = "free" ) +
      theme( legend.title = element_blank() )
  })
  
  output$sp.srw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$sp.srw.click ) )  return(NULL)
    ds <- nearPoints( sp.type()[, .( object, date, size, type ) ], input$sp.srw.click )
    ds[ order( ds$size, decreasing = TRUE ), ]
  })
  
  output$lun.hist <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ls <- lun.sum()[ stat == "95th", ]
    ls <- ls[ order( ls$iops, decreasing = TRUE ), ]
    ls$n <- c( 1 : dim(ls)[1] )
    ggplot( data = ls, aes( n, iops ) ) +
      geom_bar( stat = "identity", aes( fill = owner ) ) +
      ylab( "IOPS" ) + xlab( "LUNs" ) + 
      ggtitle( "Total IOPS on all LUNs by 95th percentile" ) +
      scale_x_discrete( limits = c( 1 : dim(ls)[1] ),
                        label = function(x) { paste( ls$object, ": ", ls[ x, ]$name ) } ) +
      theme( axis.text.x = element_text( angle = 90, size = rel(0.3) ),
             legend.title = element_blank() )
  })

  output$lun.hist.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.hist.hover ) )  return(NULL)
    ls <- lun.sum()[ stat == "95th", ]
    ls <- ls[ order( ls$iops, decreasing = TRUE ), ]
    ls[ input$lun.hist.hover$x, .( object, name, iops ) ]
  })
  
  output$lun.tt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = lun.type()[ type == "total", ],
            aes( x = date, y = iops, colour = object ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput of LUNs on SP A and SP B" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free"  ) + 
      theme( legend.position = "none" )
  })
  
  output$lun.tt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.tt.click ) )  return(NULL)
    ds <- nearPoints( lun.type(), input$lun.tt.click )
    ds <- ds[ order( ds$iops, decreasing = TRUE ), .( object, name, date, iops, type ) ]
    ds[ type == "total", ]
  })
  
  output$lun.trw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ],
            aes( x = date, y = iops, colour = object ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Read and Write Throughput of LUNs on SP A and SP B" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  
  
  output$lun.trw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.trw.click ) )  return(NULL)
    ds <- nearPoints( lun.type(), input$lun.trw.click )
    ds <- ds[ order( ds$iops, decreasing = TRUE ), .( object, name, date, iops, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$lun.bt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = lun.type()[ type == "total", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) +
      ggtitle( "Total Bandwidth of all LUNs on SP A and SP B" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) + 
      theme( legend.position = "none" )
  })  
  
  output$lun.bt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.bt.click ) )  return(NULL)
    ds <- nearPoints( lun.type(), input$lun.bt.click )
    ds <- ds[ order( ds$bw, decreasing = TRUE ), .( object, name, date, bw, type ) ]
    ds[ type == "total", ]
  })
  
  output$lun.brw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) +
      ggtitle( "Read and Write Bandwidth of all LUNs on SP A and SP B" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })
  
  output$lun.brw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.brw.click ) )  return(NULL)
    ds <- nearPoints( lun.type(), input$lun.brw.click )
    ds <- ds[ order( ds$bw, decreasing = TRUE ), .( object, name, date, bw, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$lun.srw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = lun.type()[ type == "read" | type == "write", ],
            aes( x = date, y = size, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) +
      ggtitle( "Read and Write Size of all LUNs on both SP" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  

  output$lun.srw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$lun.srw.click ) )  return(NULL)
    ds <- nearPoints( lun.type(), input$lun.srw.click )
    ds <- ds[ order( ds$size, decreasing = TRUE ), .( object, name, date, size, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$disk.heat <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    dhm <- disk.sum()[ stat == "95th", .( object, util ) ]
    dhm$dae <- str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\1_\\2" )
    dhm$disk <- as.numeric( str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\3" ) )
    ggplot( dhm, aes( disk, dae ) ) + geom_tile( aes( fill = util ), colour = "black" ) +
      scale_fill_gradient( low = "green", high = "red", limits=c( 0, 100 ) ) + 
      xlab( "Disk" ) + ylab( "DAE" ) +
      ggtitle( "Disks Utilization Heatmap by 95th percentile" ) +
      scale_x_discrete( limits = c( 0 : max( dhm$disk ) ) ) +
      theme_classic() + theme( legend.title = element_blank() )
  })

  output$disk.hist.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$disk.hist.hover ) )  return(NULL)
    dhm <- disk.sum()[ stat == "95th", .( object, util, iops ) ]
    dhm$dae <- str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\1_\\2" )
    dhm$disk <- as.numeric( str_replace_all( dhm$object, "([0-9]+)_([0-9]+)_([0-9]+)", "\\3" ) )
    hdisk <- round( input$disk.hist.hover$x, 0 )
    hdae <- sort(unique(dhm$dae))[ round( input$disk.hist.hover$y, 0 ) ]
    dhm[ disk == hdisk & dae == hdae, .(object, util, iops) ]
  })
  
  output$disk.tt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = disk.type()[ type == "total", ],
            aes( x = date, y = iops, colour = object ) ) +
      geom_line() + xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) +
      theme( legend.position = "none" )
  })
  
  output$disk.tt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$disk.tt.click ) )  return(NULL)
    ds <- nearPoints( disk.type(), input$disk.tt.click )
    ds <- ds[ order( ds$iops, decreasing = TRUE ), .( object, date, iops, type ) ]
    ds[ type == "total", ]
  })
  
  output$disk.trw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = disk.type()[ type == "read" | type == "write", ],
            aes( x = date, y = iops, colour = object ) ) + geom_line() +
      xlab("") + ylab( "IOPS" ) + 
      ggtitle( "Read and Write Throughput of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  

  output$disk.trw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$disk.trw.click ) )  return(NULL)
    ds <- nearPoints( disk.type(), input$disk.trw.click )
    ds <- ds[ order( ds$iops, decreasing = TRUE ), .( object, date, iops, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$disk.bt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = disk.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object ), position = 'stack') +
      xlab("") + ylab( "MB/s" ) + ggtitle( "Total Bandwidth of all Disks by Buses" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ ., scales = "free" ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })
  
  output$disk.bt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$disk.bt.click ) )  return(NULL)
    dt <- disk.type()[ type == "total", ]
    selected <- abs( dt$date - as.POSIXct( input$disk.bt.click$x, origin = "1970-01-01" ) ) < 5
    ds <- dt[ selected & owner == input$disk.bt.click$panelvar1, .( object, date, bw ) ]
    ds[ order( ds$bw, decreasing = TRUE ), ]
  })
  
  output$disk.brw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = disk.type()[ type == "read" | type == "write", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_line() +
      xlab("") + ylab( "MB/s" ) +
      ggtitle( "Read and Write Bandwidth of all Disks by Buses" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( owner ~ type, scales = "free" ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  

  output$disk.brw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$disk.brw.click ) )  return(NULL)
    ds <- nearPoints( disk.type(), input$disk.brw.click )
    ds <- ds[ order( ds$bw, decreasing = TRUE ), .( object, date, bw, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$host.hist <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    hs <- host.sum()[ stat == "95th", ]
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

  output$host.hist.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.hist.hover ) )  return(NULL)
    hs <- host.sum()[ stat == "95th", ]
    hs <- hs[ order( hs$iops, decreasing = TRUE ), ]
    hs[ input$host.hist.hover$x, .( object, iops ) ]
  })
  
  output$host.tt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = host.type()[ type == "total", ], aes( x = date, y = iops ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "IOPS" ) +
      ggtitle( "Total Throughput of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), legend.key.size = unit( 1, "lines" ) )
  })

  output$host.tt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.tt.click ) )  return(NULL)
    dt <- host.type()[ type == "total", ]
    selected <- abs( dt$date - as.POSIXct( input$host.tt.click$x, origin = "1970-01-01" ) ) < 5
    ds <- dt[ selected, .( object, date, iops ) ]
    ds[ order( ds$iops, decreasing = TRUE ), ]
  })
  
  output$host.trw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write", ],
            aes( x = date, y = iops, colour = object ) ) + geom_line() +
      xlab("") + ylab( "IOPS" ) + 
      ggtitle( "Read and Write Throughput of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( ~ type ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  

  output$host.trw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.trw.click ) )  return(NULL)
    ds <- nearPoints( host.type(), input$host.trw.click )
    ds <- ds[ order( ds$iops, decreasing = TRUE ), .( object, date, iops, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$host.bt <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = host.type()[ type == "total", ], aes( x = date, y = bw ) ) +
      geom_area( aes( colour = object, fill = object ), position = 'stack' ) +
      xlab("") + ylab( "MB/s" ) + 
      ggtitle( "Total Bandwidth of all hosts" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      theme( legend.title = element_blank(), legend.key.size = unit( 1, "lines" ) )
  })
  
  output$host.bt.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.bt.click ) )  return(NULL)
    dt <- host.type()[ type == "total", ]
    selected <- abs( dt$date - as.POSIXct( input$host.bt.click$x, origin = "1970-01-01" ) ) < 5
    ds <- dt[ selected, .( object, date, bw ) ]
    ds[ order( ds$bw, decreasing = TRUE ), ]
  })
  
  output$host.brw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write", ],
            aes( x = date, y = bw, colour = object ) ) +
      geom_line() + xlab("") + ylab( "MB/s" ) + 
      ggtitle( "Read and Write Bandwidth of all hosts" ) + 
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( ~ type ) + 
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })
  
  output$host.brw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.brw.click ) )  return(NULL)
    ds <- nearPoints( host.type(), input$host.brw.click )
    ds <- ds[ order( ds$bw, decreasing = TRUE ), .( object, date, bw, type ) ]
    ds[ type == "read" | type == "write", ]
  })
  
  output$host.srw <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = host.type()[ type == "read" | type == "write" ],
            aes( x = date, y = size, colour = object ) ) +
      geom_line() + xlab("") + ylab( "Size, KB" ) +
      ggtitle( "Read and Write Size of all hosts" ) +
      scale_x_datetime( labels = date_format( "%m/%d %H:%m" ) ) +
      facet_grid( ~ type ) +
      theme( axis.text.x = element_text( angle = 90 ), legend.position = "none" )
  })  

  output$host.srw.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$host.srw.click ) )  return(NULL)
    ds <- nearPoints( host.type(), input$host.srw.click )
    ds <- ds[ order( ds$size, decreasing = TRUE ), .( object, date, size, type ) ]
    ds[ type == "read" | type == "write", ]
  })

  components <- reactive({ input$det.component })
  objects <- reactive({ input$det.list })
  
  observe({
    if( is.null( input$file ) )  return(NULL)
    dt <- switch( input$det.component,
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    ch <- list()
    ch[ unique( dt[ order( dt$object ), name ] ) ] <- unique( dt[ order( dt$object ), object ] )
    updateSelectInput( session, "det.list", choices = ch,
                       selected = unique( dt[ order( dt$object ), object ] )[1]
    )
  })
  
  output$det.util <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, util ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Utilization", ylab = "%") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.tt <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, iops ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Total Throughput", ylab = "IOPS") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.tr <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, riops ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Read Throughput", ylab = "IOPS") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.tw <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, wiops ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Write Throughput", ylab = "IOPS") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.bt <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, bw ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Total Bandwidth", ylab = "MB/s") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.br <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, rbw ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Read Bandwidth", ylab = "MB/s") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.bw <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, wbw ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Write Bandwidth", ylab = "MB/s") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.sr <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, rsize ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Read Size", ylab = "KB") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.sw <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, wsize ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Write Size", ylab = "KB") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.rt <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, rt ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Response Time", ylab = "ms") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.st <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, st ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Service Time", ylab = "ms") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$det.ql <- renderDygraph({
    if( is.null( input$file ) )  return(NULL)
    if( length(objects()) == 0 )  return(NULL)
    dt <- switch( isolate(components() ),
                  "1" = sp.data(),
                  "2" = lun.data(),
                  "3" = disk.data(),
                  "4" = host.data()
    )
    s1 <- NULL
    for ( x in objects() )
      s1 <- cbind( s1, xts( dt[ object == x, ql ],
                            order.by = dt[ object == x, date ] ) )
    names(s1) <- objects()
    dygraph( s1, main = "Queue Length", ylab = "") %>%
      dyOptions( colors = RColorBrewer::brewer.pal( 8, "Set2" ) ) %>%
      dyRangeSelector()
  })
  
  output$summary <- renderDataTable({
    switch( input$summary.component,
            "1" = sp.sum(),
            "2" = lun.sum(),
            "3" = disk.sum(),
            "4" = host.sum()
    )
  })  
  
  rel.data <- reactive({
    if( is.null( input$file ) )  return(NULL)
    dt <- switch( input$rel.component,
                   "1" = sp.sum()[ stat == "95th", ],
                   "2" = lun.sum()[ stat == "95th", ],
                   "3" = disk.sum()[ stat == "95th", ],
                   "4" = host.sum()[ stat == "95th", ]
    )
    return(dt)
  })

  output$rel.plot <- renderPlot({
    if( is.null( input$file ) )  return(NULL)
    ggplot( data = rel.data(), aes_string( x = input$rel.x, y = input$rel.y ) ) + 
      geom_point( aes_string( size = input$rel.size, colour = input$rel.colour ) ) + 
      scale_color_gradient( low = "green", high = "red" )
  })
  
  output$rel.info <- renderPrint({
    if( is.null( input$file ) )  return(NULL)
    if( is.null( input$rel.click ) )  return(NULL)
    nearPoints( as.data.frame( rel.data() ), input$rel.click )
  })

})
