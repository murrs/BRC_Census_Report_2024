makeTableData <- function(plotDat, format = c("percent", "proportion", "dollar"),
                          digits = 3, 
                          confInt = c("interval", "plusminus", "none")){
  plotDat <- plotDat[order(plotDat$year, plotDat$labels),]
  nYears <- length(unique(plotDat$year))
  nLabels <- length(unique(plotDat$labels))
  cells <- apply(plotDat, 1, format_table_entry, format = format, 
                 digits = digits, confInt = confInt)
  
  outTable <- as.table(matrix(cells, nrow = nLabels, ncol = nYears))
  rownames(outTable) <- unique(plotDat$labels)
  colnames(outTable) <- unique(plotDat$year)
  return(outTable)
}
