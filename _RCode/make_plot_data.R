makePlotData <- function(varName, varNameTable, designs, years, levels,
                         labels = NULL, labelOrder = NULL, groups = NULL){
  #TODO: check inputs for errors and create helpful error messages
  
  #Get number of years to loop over
  nYears <- length(years)
  
  #If a character vector is supplied for levels, repeat as a list for number
  # of years supplied
  if(is.character(levels)){
    levels <- rep(list(levels), times = nYears)
  }
  
  #If groups is just a numeric vector identifying groups, format as a list
  if((!is.list(groups)) & (!is.null(groups))){
    if(is.numeric(groups)){
      ngroups <- max(groups)
      groups <- lapply(1:ngroups, function(i, groups){which(groups == i)},
                       groups = groups)
    }
    else{
      stop("groups should be a list or numeric vector.")
    }
  }
  
  #Order years, levels, and designs by decreasing years to match the 
  #  varname lookup
  yearOrder <- order(years, decreasing = TRUE)
  years <- years[yearOrder]
  designs <- designs[yearOrder]
  levels <- levels[yearOrder]
  
  #Get variable names from the crosswalk
  #Determine which years to pull
  yearsToUse <- lapply(years, function(yr, nms){
    grepl(yr, nms)
  }, nms = names(varNameTable))
  yearsToUse <- do.call(rbind, yearsToUse)
  yearsToUse <- apply(yearsToUse, 2, any)
  
  #Grab names.
  varNames <- as.vector(varNameTable[question == varName, ..yearsToUse])
  #Remove any write-in questions
  writeInIndex <- unique(unlist(lapply(varNames, function(x){
    which(grepl("writeIn", x))})))
  if(any(writeInIndex > 0)){
    varNames <- lapply(varNames, function(x, indx){x[-indx]}, indx = writeInIndex)
  }
  
  
  #For questions where only one answer may be selected
  #Check if varNames is not a list and convert to list
  singleSelection <- length(varNames[[1]]) == 1
  if(!is.list(varNames)){
    if(singleSelection){
      varNames <- rep(varNames, times = nYears)
    }
    varNames <- as.list(varNames)
  }
  #Get all estimates
  outDat <- lapply(1:nYears, makePlotDataByYearVarLevel, 
                   varNames = varNames, designs = designs, 
                   years = years, levels = levels, groups = groups)
  #Rbind into a single data.table
  outDat <- do.call(rbind, outDat)
  #Remove row names
  rownames(outDat) <- NULL
  
  #If label order is not provided just use 1:numberOfLevels
  if(is.null(labelOrder)){
    if(singleSelection){
      labelOrder <- 1:length(levels[[1]]) 
    }
    else{
      labelOrder <- 1:length(varNames[[1]])
    }
  }
  
  
  #If labels are not provided create labels based on varnames for 
  #  multi-selection questions or based on the most recent year of levels
  #  for single-selection questions
  if(is.null(labels)){
    #Assign labels apppropriately depending on if label order is numeric or
    #  character.
    if(is.character(labelOrder)){
      if(singleSelection){
        outDat$labels <- factor(levels[[1]], levels = labelOrder)
      }
      else{
        outDat$labels <- factor(varNames, levels = labelOrder)
      }
    }
    else if(is.numeric(labelOrder)){
      if(singleSelection){
        outDat$labels <- factor(levels[[1]], levels = levels[[1]][labelOrder])
      }
      else{
        outDat$labels <- factor(varNames, levels = varNames[labelOrder])
      }
    }
    return(outDat)
  }
  else{
    #If everything is given assign labels appropriately with the given ordering
    if(is.character(labelOrder)){
      outDat$labels <- factor(rep(labels, nYears), levels = labelOrder)
    }
    else if(is.numeric(labelOrder)){
      outDat$labels <- factor(rep(labels, nYears), 
                              levels = labels[labelOrder])
    }
  }
  return(outDat)
}
