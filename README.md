# NarVisualisation
NarVisualisation is a Shiny application for visualisation Nar files from EMC VNX disks arrays

This applications takes CSV file, extracted from some Nar files from one disk array and performs the next tasks:
* Plots regular charts for Throughput (total, read or write), Bandwidth (total, read or write), Response Time and Utilization for SP, LUNs or Disks
* Plots combinations charts for SP, LUNs or Disks by mean, 95th percentile or max statistics:
  + Utilization vs IOPS by Response Time and ABQL
  + ABQL vs Response Time by Utilization and IOPS
  + Bandwidth vs IOPS by Utilization and Response Time
  + Wtite IOPS vs Read IOPS by Utilization and Response Time
* Displays Summary Table for SP, LUNs or Disks with mean, 95th percentile and max statistics
* Displays Table with all values  for SP, LUNs or Disks

Application is available here: (https://vzaigrin.shinyapps.io/NarVisualisation/)
