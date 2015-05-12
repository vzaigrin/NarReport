library(shiny)
library(data.table)
library(stringr)
library(xts)
library(dygraphs)
library(ggplot2)

labname<-data.table(l=c("1","2","3"),lab=c("Total","Read","Write"))
setkey(labname,"l")

shinyServer(function(input, output, session) {
    
    file<-reactive({
        if(is.null(input$file))  return(NULL)
        input$file
    })
    
    components<-reactive({input$component})
    types<-reactive({input$type})
    objects<-reactive({input$object})
    stats<-reactive({input$stat})
    
    rawdata <- reactive({
        if(is.null(file()))  return(NULL)
        data.file<-file()
        rawdata<-read.csv(data.file$datapath,stringsAsFactor=FALSE)
        arrayname<-rawdata$Owner.Array.Name[1]
        rawdata<-rawdata[rawdata$Owner.Array.Name==arrayname,]
        return(rawdata)
    })
    
    arrayname <- reactive({
        if(is.null(file()))  return(NULL)
        arrayname<-rawdata()$Owner.Array.Name[1]
    })
    
    data <- reactive({
        if(is.null(file()))  return(NULL)
        raw<-rawdata()
        data<-data.table(object=raw$Object.Name,date=as.POSIXct(strptime(raw$Poll.Time,"%m/%d/%Y %H:%M:%S")),owner=raw$Current.Owner,util=raw$Utilization...,ql=raw$Queue.Length,rt=raw$Response.Time..ms,bw=raw$Total.Bandwidth..MB.s.,iops=raw$Total.Throughput..IO.s.,rbw=raw$Read.Bandwidth..MB.s.,rsize=raw$Read.Size..KB.,riops=raw$Read.Throughput..IO.s.,wbw=raw$Write.Bandwidth..MB.s.,wsize=raw$Write.Size..KB.,wiops=raw$Write.Throughput..IO.s.,abql=raw$Average.Busy.Queue.Length,st=raw$Service.Time..ms.)
        return(data)
    })
    
    sps <- reactive({
        dt<-data()
        sp<-dt[object=="SP A"|object=="SP B",]
        sp$lo<-sp$object
        return(sp)
    })
    
    luns <- reactive({
        dt<-data()
        luns<-dt[grepl("([[:print:]]+) \\[([0-9]+);",object),]
        luns$lo<-str_replace_all(luns$object,"([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\2")
        luns$object<-str_replace_all(luns$object,"([[:print:]]+) \\[([0-9]+); ([[:print:]]+)","\\1")
        return(luns)
    })
    
    disks <- reactive({
        dt<-data()
        disks<-dt[grepl("Bus [0-9]",object),]
        disks$lo<-str_replace_all(disks$object,"Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)","\\1_\\2_\\3")
        disks$bus<-str_replace_all(disks$object,"Bus ([0-9]) Enclosure ([0-9]) Disk ([0-9]+)","\\1")
        return(disks)
    })
    
    ssum <- reactive({
        if(is.null(file()))  return(NULL)
        dt<-sps()
        s1<-dt[,list("mean",round(mean(util),digits=2),round(mean(ql),digits=2),round(mean(rt),digits=2),round(mean(bw),digits=2),round(mean(iops),digits=2),round(mean(rbw),digits=2),round(mean(rsize),digits=2),round(mean(riops),digits=2),round(mean(wbw),digits=2),round(mean(wsize),digits=2),round(mean(wiops),digits=2),round(mean(abql),digits=2),round(mean(st),digits=2)),by=lo]
        s2<-dt[,list("95th",round(quantile(util,c(0.95),na.rm=TRUE),digits=2),round(quantile(ql,c(0.95),na.rm=TRUE),digits=2),round(quantile(rt,c(0.95),na.rm=TRUE),digits=2),round(quantile(bw,c(0.95),na.rm=TRUE),digits=2),round(quantile(iops,c(0.95),na.rm=TRUE),digits=2),round(quantile(rbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(rsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(riops,c(0.95),na.rm=TRUE),digits=2),round(quantile(wbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(wsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(wiops,c(0.95),na.rm=TRUE),digits=2),round(quantile(abql,c(0.95),na.rm=TRUE),digits=2),round(quantile(st,c(0.95),na.rm=TRUE),digits=2)),by=lo]
        s3<-dt[,list("max",round(max(util),digits=2),round(max(ql),digits=2),round(max(rt),digits=2),round(max(bw),digits=2),round(max(iops),digits=2),round(max(rbw),digits=2),round(max(rsize),digits=2),round(max(riops),digits=2),round(max(wbw),digits=2),round(max(wsize),digits=2),round(max(wiops),digits=2),round(max(abql),digits=2),round(max(st),digits=2)),by=lo]
        dsum<-rbind(s1,s2,s3)
        setnames(dsum,c("object","stat","util","ql","rt","bw","iops","rbw","rsize","riops","wbw","wsize","wiops","abql","st"))
        return(dsum)
    })
    
    lsum <- reactive({
        if(is.null(file()))  return(NULL)
        dt<-luns()
        s1<-dt[,list("mean",round(mean(util),digits=2),round(mean(ql),digits=2),round(mean(rt),digits=2),round(mean(bw),digits=2),round(mean(iops),digits=2),round(mean(rbw),digits=2),round(mean(rsize),digits=2),round(mean(riops),digits=2),round(mean(wbw),digits=2),round(mean(wsize),digits=2),round(mean(wiops),digits=2),round(mean(abql),digits=2),round(mean(st),digits=2)),by=lo]
        s2<-dt[,list("95th",round(quantile(util,c(0.95),na.rm=TRUE),digits=2),round(quantile(ql,c(0.95),na.rm=TRUE),digits=2),round(quantile(rt,c(0.95),na.rm=TRUE),digits=2),round(quantile(bw,c(0.95),na.rm=TRUE),digits=2),round(quantile(iops,c(0.95),na.rm=TRUE),digits=2),round(quantile(rbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(rsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(riops,c(0.95),na.rm=TRUE),digits=2),round(quantile(wbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(wsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(wiops,c(0.95),na.rm=TRUE),digits=2),round(quantile(abql,c(0.95),na.rm=TRUE),digits=2),round(quantile(st,c(0.95),na.rm=TRUE),digits=2)),by=lo]
        s3<-dt[,list("max",round(max(util),digits=2),round(max(ql),digits=2),round(max(rt),digits=2),round(max(bw),digits=2),round(max(iops),digits=2),round(max(rbw),digits=2),round(max(rsize),digits=2),round(max(riops),digits=2),round(max(wbw),digits=2),round(max(wsize),digits=2),round(max(wiops),digits=2),round(max(abql),digits=2),round(max(st),digits=2)),by=lo]
        dsum<-rbind(s1,s2,s3)
        setnames(dsum,c("object","stat","util","ql","rt","bw","iops","rbw","rsize","riops","wbw","wsize","wiops","abql","st"))
        return(dsum)
    })
    
    dsum <- reactive({
        if(is.null(file()))  return(NULL)
        dt<-disks()
        s1<-dt[,list("mean",round(mean(util),digits=2),round(mean(ql),digits=2),round(mean(rt),digits=2),round(mean(bw),digits=2),round(mean(iops),digits=2),round(mean(rbw),digits=2),round(mean(rsize),digits=2),round(mean(riops),digits=2),round(mean(wbw),digits=2),round(mean(wsize),digits=2),round(mean(wiops),digits=2),round(mean(abql),digits=2),round(mean(st),digits=2)),by=lo]
        s2<-dt[,list("95th",round(quantile(util,c(0.95),na.rm=TRUE),digits=2),round(quantile(ql,c(0.95),na.rm=TRUE),digits=2),round(quantile(rt,c(0.95),na.rm=TRUE),digits=2),round(quantile(bw,c(0.95),na.rm=TRUE),digits=2),round(quantile(iops,c(0.95),na.rm=TRUE),digits=2),round(quantile(rbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(rsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(riops,c(0.95),na.rm=TRUE),digits=2),round(quantile(wbw,c(0.95),na.rm=TRUE),digits=2),round(quantile(wsize,c(0.95),na.rm=TRUE),digits=2),round(quantile(wiops,c(0.95),na.rm=TRUE),digits=2),round(quantile(abql,c(0.95),na.rm=TRUE),digits=2),round(quantile(st,c(0.95),na.rm=TRUE),digits=2)),by=lo]
        s3<-dt[,list("max",round(max(util),digits=2),round(max(ql),digits=2),round(max(rt),digits=2),round(max(bw),digits=2),round(max(iops),digits=2),round(max(rbw),digits=2),round(max(rsize),digits=2),round(max(riops),digits=2),round(max(wbw),digits=2),round(max(wsize),digits=2),round(max(wiops),digits=2),round(max(abql),digits=2),round(max(st),digits=2)),by=lo]
        dsum<-rbind(s1,s2,s3)
        setnames(dsum,c("object","stat","util","ql","rt","bw","iops","rbw","rsize","riops","wbw","wsize","wiops","abql","st"))
        return(dsum)
    })
    
    observe({
        if(is.null(file()))  return(NULL)
        dt<-switch(components(),
                   "1"=sps(),
                   "2"=luns(),
                   "3"=disks()
                   )
        ch<-list()
        ch[unique(dt$object)]<-unique(dt$object)
        updateCheckboxGroupInput(session,"object",choices=ch,selected=ch[1])
    })
    
    output$dygraph1 <- renderDygraph({
        if(is.null(file()))  return(NULL)
        if(length(objects())==0)  return(NULL)
        tval<-switch(types(),
                     "1"="iops",
                     "2"="riops",
                     "3"="wiops"
                     )
        dt<-switch(isolate(components()),
                   "1"=sps(),
                   "2"=luns(),
                   "3"=disks()
                   )
        s1<-NULL
        for ( x in objects())
            s1<-cbind(s1,xts(dt[object==x,tval,with=FALSE],order.by=dt[object==x,date]))
        names(s1)<-objects()
        dygraph(s1,main=paste(labname[input$type]$lab," Throughput"),ylab="IOPS") %>%
            dyOptions(colors=RColorBrewer::brewer.pal(8,"Set2")) %>%
            dyRangeSelector()
    })
    
    output$dygraph2 <- renderDygraph({
        if(is.null(file()))  return(NULL)
        if(length(objects())==0) return(NULL)
        tval<-switch(types(),
                     "1"="bw",
                     "2"="rbw",
                     "3"="wbw"
        )
        dt<-switch(isolate(components()),
                   "1"=sps(),
                   "2"=luns(),
                   "3"=disks()
        )
        s2<-NULL
        for ( x in objects())
            s2<-cbind(s2,xts(dt[object==x,tval,with=FALSE],order.by=dt[object==x,date]))
        names(s2)<-objects()
        dygraph(s2,main=paste(labname[input$type]$lab," Bandwidth"),ylab="MB/s") %>%
            dyOptions(colors=RColorBrewer::brewer.pal(8,"Set2")) %>%
            dyRangeSelector()
    })
    
    output$dygraph3 <- renderDygraph({
        if(is.null(file()))  return(NULL)
        if(length(objects())==0) return(NULL)
        dt<-switch(isolate(components()),
                   "1"=sps(),
                   "2"=luns(),
                   "3"=disks()
        )
        s3<-NULL
        for ( x in objects())
            s3<-cbind(s3,xts(dt[object==x,rt],order.by=dt[object==x,date]))
        names(s3)<-objects()
        dygraph(s3,main="Response time",ylab="ms") %>%
            dyOptions(colors=RColorBrewer::brewer.pal(8,"Set2")) %>%
            dyRangeSelector()
    })
    
    output$dygraph4 <- renderDygraph({
        if(is.null(file()))  return(NULL)
        if(length(objects())==0) return(NULL)
        dt<-switch(isolate(components()),
                   "1"=sps(),
                   "2"=luns(),
                   "3"=disks()
        )
        s3<-NULL
        for ( x in objects())
            s3<-cbind(s3,xts(dt[object==x,util],order.by=dt[object==x,date]))
        names(s3)<-objects()
        dygraph(s3,main="Utilization",ylab="%") %>%
            dyOptions(colors=RColorBrewer::brewer.pal(8,"Set2")) %>%
            dyRangeSelector()
    })
    
    output$util_iops <- renderPlot({
        ds<-switch(components(),
                   "1"=ssum(),
                   "2"=lsum(),
                   "3"=dsum()
        )
        sval<-switch(stats(),
                     "1"="mean",
                     "2"="95th",
                     "3"="max"
        )
        ggplot(data=ds[stat==sval,],aes(iops,util,label=object)) + 
            geom_point(aes(size=rt,color=abql)) + 
            geom_text(hjust=-0.1,vjust=-0.1,size=3) + 
            scale_color_gradient(low="green", high="red") + 
            xlab("IOPS")+ylab("Utilization") + 
            ggtitle("Utilization vs IOPS by Response Time and ABQL")
    })
    
    output$abql_rt <- renderPlot({
        ds<-switch(components(),
                   "1"=ssum(),
                   "2"=lsum(),
                   "3"=dsum()
        )
        sval<-switch(stats(),
                     "1"="mean",
                     "2"="95th",
                     "3"="max"
        )
        ggplot(data=ds[stat==sval,],aes(rt,abql,label=object)) +
            geom_point(aes(size=iops,color=util)) + 
            geom_text(hjust=-0.1,vjust=-0.1,size=3) + 
            scale_color_gradient(low="green", high="red") +
            xlab("Response Time") +
            ylab("ABQL") +
            ggtitle("ABQL vs Response Time by Utilization and IOPS")
        
    })
    
    output$band_iops <- renderPlot({
        ds<-switch(components(),
                   "1"=ssum(),
                   "2"=lsum(),
                   "3"=dsum()
        )
        sval<-switch(stats(),
                     "1"="mean",
                     "2"="95th",
                     "3"="max"
        )
        ggplot(data=ds[stat==sval,],aes(iops,bw,label=object)) +
            geom_point(aes(size=rt,color=util)) +
            scale_color_gradient(low="green",high="red") +
            geom_text(hjust=-0.1,vjust=-0.1,size=3) +
            xlab("IOPS") +
            ylab("MB/s")+ggtitle("Bandwidth vs IOPS by Utilization and Response Time")
    })
    
    output$write_read <- renderPlot({
        ds<-switch(components(),
                   "1"=ssum(),
                   "2"=lsum(),
                   "3"=dsum()
        )
        sval<-switch(stats(),
                     "1"="mean",
                     "2"="95th",
                     "3"="max"
        )
        ggplot(data=ds[stat==sval,],aes(riops,wiops,label=object)) +
            geom_point(aes(color=util,size=rt)) +
            scale_color_gradient(low="green",high="red") +
            geom_text(hjust=-0.1,vjust=-0.1,size=3) +
            xlab("Read IOPS") +
            ylab("Write IOPS") +
            ggtitle("Write IOPS vs Read IOPS by Utilization and Response Time")
    })
    
    output$summary <- renderDataTable({
        switch(components(),
               "1"=ssum(),
               "2"=lsum(),
               "3"=dsum()
        )
    })
    
    output$table <- renderDataTable({
        switch(components(),
               "1"=sps(),
               "2"=luns(),
               "3"=disks()
        )
    })
    
    output$values <- renderText({
        if(!is.null(file()))
            paste("Array name ..",arrayname(),"   File size ..",file()["size"],sep=" ")
    })
})
