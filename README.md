# NarReport
NarReport is a Shiny application for analyzing and visualization Nar files from EMC VNX and CLARiiON disks arrays

## Description
This applications takes CSV file, extracted from some Nar files from one disk array and performs the next tasks:
* Plots review charts for Throughput (total, read or write), Bandwidth (total, read or write), Response Time and Utilization for all SP, LUNs, Disks or Hosts.
* Plots detailed charts for selected SP, LUNs, Disks or Hosts.
* Plots combinations charts by 95th percentile statistics.
* Displays Summary Table for SP, LUNs or Disks with mean, 95th percentile and max statistics.
* Generate report in PDF format.

## Installation
No installation required.
Application is available here: (https://vzaigrin.shinyapps.io/NarReport/)

## Usage Instructions

1. Generate csv file from nar files by this command:
```
naviseccli analyzer -archivedump -data file1.nar ... filen.nar  -object s,hl,d -format on,pt,oan,co,u,tt,rio,wio,tb,rb,wb,rs,ws,rt,st,ql  -join -overwrite y -out nar.csv
```
2. Upload csv file.
3. See charts and tables.
