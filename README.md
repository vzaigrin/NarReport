# NarReport
NarReport is a Shiny application for analyzing and visualization Nar files from EMC VNX and CLARiiON disk arrays

## Description
This applications takes CSV, configuration and relation files, extracted from some Nar files from one disk array and performs the next tasks:
* Show relationship between hosts, LUNs, Pools/RGs and disks.
* Plots review charts for Throughput (total, read or write), Bandwidth (total, read or write), Response Time and Utilization for all SP, Ports, LUNs, Disks and Hosts.
* Plots combinations charts by 95th percentile statistics.
* Displays Summary Table for SP, Ports, LUNs, Disks and Hosts with mean, 95th percentile and max statistics.
* Generate report in PDF format.

## Installation
No installation required.
Application is available here: (https://vzaigrin.shinyapps.io/NarReport/)

But it runs much faster on local computer in the RStudio or in local Shiny server (https://www.rstudio.com/products/shiny/shiny-server).

## Usage Instructions

1. Generate csv file from nar files by this command:

   ```
  naviseccli analyzer -archivedump -data file1.nar ... filen.nar  -object s,p,al,d -format on,pt,oan,co,u,tt,rio,wio,tb,rb,wb,rs,ws,rt,st,ql  -join -overwrite y -out nar.csv
  ```
2. Upload csv file.
3. Generate configuration file by this command:

   ```
   naviseccli analyzer -archivedump -config filen.nar -xml -overwrite y -out config.xml
   ```
4. Upload configuration file
5. Generate relations file by this command:

   ```
   naviseccli analyzer -archivedump -rel filen.nar -xml -overwrite y -out rel.xml
   ```
6. Upload relations file
7. See charts and tables.
