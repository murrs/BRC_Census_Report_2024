formulaVarLevel <- function(level, varName){
  if(is.character(level)){
    out <- paste0("(", varName, " == ", "\"", level, "\"", ")")
  }
  else if(is.logical(level)){
    if(level){
      out <- varName
    }
    else{
      out <- paste0("!", varName)
    }
  }
  return(out)
}

#TODO: makePlotDataByLevel() and makePlotDataByGroup() can be simplified and 
#        combined into a single function

#TODO: check to make sure level is a level of the variable.  If not give an
#        error informing user to check data cleaning script for an error

makePlotDataByLevel <- function(level, design, varName, year){
  if(varName == ""){
    out <- data.frame(est = NA,
                      lower = NA,
                      upper = NA,
                      level = level,
                      year = year)
    return(out)
  }
  if(is.character(level)){
    if(grepl("_qual", varName)){
      f <- as.formula(paste0("~", varName, "!=", "\"\""))
    }
    else{
      f <- as.formula(paste0("~", varName, " == ", "\"", level, "\""))
    }
  }
  else if(is.logical(level)){
    if(level){
      f <- as.formula(paste0("~", varName))
    }
    else{
      f <- as.formula(paste0("~", "!", varName))
    }
  }
  else if(level == ".+"){
    f <- as.formula(paste0("~", varName, "!=", "\"\""))
  }
  ciOut <- svyciprop(f, design = design)
  out <- data.frame(est = as.numeric(ciOut),
                    lower = attr(ciOut, "ci")[1],
                    upper = attr(ciOut, "ci")[2],
                    level = level,
                    year = year)
  return(out)
}

#TODO: The grouping currently does not work for multiple selection questions

makePlotDataByGroup <- function(group, design, varName, year, levels){
  if(is.numeric(group)){
    levelsInGroup <- levels[group]
  }
  else if(is.character(group)){
    levelsInGroup <- levels[levels %in% group]
  }
  fpieces <- lapply(levelsInGroup, formulaVarLevel, varName = varName)
  fpieces <- do.call(paste, c(fpieces, list(sep = " | ")))
  f <- as.formula(paste("~", fpieces))
  ciOut <- svyciprop(f, design = design)
  data.frame(est = as.numeric(ciOut),
             lower = attr(ciOut, "ci")[1],
             upper = attr(ciOut, "ci")[2],
             level = do.call(paste, as.list(levelsInGroup)),
             year = year)
}

makePlotDataByVarLevel <- function(varName, design, year, levels, groups){
  if(is.null(groups)){
    ciDatList <- lapply(levels, makePlotDataByLevel, design = design, 
                        varName = varName, year = year)
  }
  else{
    ciDatList <- lapply(groups, makePlotDataByGroup, design = design,
                        varName = varName, year = year, levels = levels)
  }
  
  do.call(rbind, ciDatList)
}


makePlotDataByYearVarLevel <- function(i, varNames, designs, years, levels,
                                       groups){
  
  ciDatList <- lapply(varNames[[i]], makePlotDataByVarLevel, 
                      design = designs[[i]], levels = levels[[i]], 
                      year = years[i], groups = groups)
  do.call(rbind, ciDatList)
}

format_table_entry <- function(x, format = c("percent", "proportion"),
                               digits = 3, 
                               confInt = c("interval", "plusminus", "none")){
  x <- list(est = as.numeric(x[which(names(x) == "est")]),
            lower = as.numeric(x[which(names(x) == "lower")]),
            upper = as.numeric(x[which(names(x) == "upper")]))
  if(is.na(x$est)){
    return("--")
  }
  
  lessThanPoint1 <- x$est < 0.001
  if(confInt[1] == "interval"){
    if(any(format == "percent")){
      #floating decimal format
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%", " (", 
                           sprintf(fdf, round(x$lower, digits) * 100), "%, ", 
                           sprintf(fdf, round(x$upper, digits) * 100), "%)")
      outEntries[lessThanPoint1] <- "< 0.1% (--, --)"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits)), " (", 
                           sprintf(fdf, round(x$lower, digits)), ", ", 
                           sprintf(fdf, round(x$upper, digits)), ")")
      outEntries[lessThanPoint1] <- "< 0.001 (--, --)"
    }
    else if(format == "dollar"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- paste0("$", sprintf(fdf, round(x$est, digits)), " (", 
                           "$", sprintf(fdf, round(x$lower, digits)), ", ", 
                           "$", sprintf(fdf, round(x$upper, digits)), ")")
    }
    else{
      stop("Invalid format")
    }
  }
  else if(confInt[1] == "plusminus"){
    if(any(format == "percent")){
      #floating decimal format
      fdf <- paste0("%.", digits - 2, "f")
      plusminus = round(max(c(x$est - x$lower, x$upper - x$est)), digits)
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%", " ± ", 
                           sprintf(fdf, plusminus * 100), "%")
      outEntries[lessThanPoint1] <- "< 0.1%"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      plusminus = round(max(c(x$est - x$lower, x$upper - x$est)), digits)
      outEntries <- paste0(sprintf(fdf, round(x$est, digits)), " ± ", 
                           sprintf(fdf, plusminus))
      outEntries[lessThanPoint1] <- "< 0.001"
    }
    else if(format == "dollar"){
      fdf <- paste0("%.", digits, "f")
      plusminus = round(max(c(x$est - x$lower, x$upper - x$est)), digits)
      outEntries <- paste0("$", sprintf(fdf, round(x$est, digits)), " ± ", 
                           "$", sprintf(fdf, plusminus))
    }
    else{
      stop("Invalid format")
    }
  }
  else if(confInt[1] == "none"){
    if(any(format == "percent")){
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%")
      outEntries[lessThanPoint1] <- "< 0.1%"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- sprintf(fdf, round(x$est, digits))
      outEntries[lessThanPoint1] <- "< 0.001"
    }
    else if(format == "dollar"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- paste0("$", sprintf(fdf, round(x$est, digits)))
    }
    else{
      stop("Invalid format")
    }
  }
  else{stop("Invalid confInt format")}
  
  return(linebreak(outEntries))
}


customPlotDat <- function(levels, design, incomeVarName, year, label){
  f <- as.formula(paste0("~ I(", incomeVarName, " %in% c(\"", 
                         do.call(paste, c(as.list(levels), 
                                          list(sep = "\",\""))),"\"))"))
  ciout <- svyciprop(f, design = design)
  data.frame(est = as.numeric(ciout), lower = attr(ciout, "ci")[1],
             upper = attr(ciout, "ci")[2], level = label, 
             year = year, labels = label)
}



weighted.quantile <- function(x, w, p, alpha = 0.95, na.rm = TRUE){
  if(any(is.na(x)) & na.rm){
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  if (any(is.na(w)))
    w <- rep(1 / length(x), length(x))
  
  indexes <- order(x)
  x <- x[indexes]
  w <- w[indexes]
  
  outList <- lapply(p, function(p, x, w, alpha){
    nw <- sum(w)^2 / sum(w^2) # Kish's effective sample size
    a <- p * (nw + 1)
    b <- (1 - p) * (nw + 1)
    
    cdfs.probs <- cumsum(c(0, w / sum(w)))
    cdfs <- pbeta(cdfs.probs, a, b)
    W <- tail(cdfs, -1) - head(cdfs, -1)
    
    c1 <- sum(W * x)
    c2 <- sum(W * x^2)
    se <- sqrt(c2 - c1^2)
    estimation <- c1
    margin <- se * qt(1 - (1 - alpha) / 2, df = nw - 1)
    
    out <- data.frame(est = estimation, se = se, lower = estimation - margin, 
                      upper = estimation + margin)
    return(out)
  }, x = x, w = w, alpha = alpha)
  
  out <- do.call(rbind, outList)
  rownames(out) <- paste0(p * 100, "%")
  return(out)
}

weighted.sd <- function(x, w, trim = NA, na.rm = TRUE){
  if(any(is.na(x)) & na.rm){
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  
  if(!all(is.na(trim))){
    if(length(trim) == 1){
      trimQuant <- weighted.quantile(x = x, w = w, p = c(trim, 1 - trim), 
                                     na.rm = na.rm)$est
      trimQuant <- trimQuant[order(trimQuant)]
      w <- w[x > trimQuant[1] & x < trimQuant[2]]
      x <- x[x > trimQuant[1] & x < trimQuant[2]]
    }
    else if(length(trim) == 2){
      if(is.na(trim[1])){
        trimQuant <- weighted.quantile(x = x, w = w, p = trim[2], 
                                       na.rm = na.rm)$est
        w <- w[x < trimQuant]
        x <- x[x < trimQuant]
      }
      else if(is.na(trim[2])){
        trimQuant <- weighted.quantile(x = x, w = w, p = trim[1], 
                                       na.rm = na.rm)$est
        w <- w[x > trimQuant]
        x <- x[x > trimQuant]
      }
      else{
        trimQuant <- weighted.quantile(x = x, w = w, p = trim, 
                                       na.rm = na.rm)$est
        trimQuant <- trimQuant[order(trimQuant)]
        w <- w[x > trimQuant[1] & x < trimQuant[2]]
        x <- x[x > trimQuant[1] & x < trimQuant[2]]
      }
    }
    else{stop("trim must be a numeric vector of length 1 or 2")}
  }
  n = length(w)
  xWbar = sum(x * w) / sum(w)
  wbar = sum(w) / n
  out = n / ((n-1) * sum(w)^2) * 
    (sum((w * x - wbar * xWbar)^2) -
       2 * xWbar * sum((w - wbar) * (w * x - wbar * xWbar)) + 
       xWbar^2 * sum((w - wbar)^2))
  return(out)
}

weighted.mean2 <- function(x, w, trim = NA, na.rm = TRUE, alpha = 0.05){
  if(any(is.na(x)) & na.rm){
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  if(!all(is.na(trim))){
    if(length(trim) == 1){
      trimQuant <- weighted.quantile(x = x, w = w, p = c(trim, 1 - trim), 
                                     na.rm = na.rm)$est
      trimQuant <- trimQuant[order(trimQuant)]
      w <- w[x > trimQuant[1] & x < trimQuant[2]]
      x <- x[x > trimQuant[1] & x < trimQuant[2]]
    }
    else if(length(trim) == 2){
      if(is.na(trim[1])){
        trimQuant <- weighted.quantile(x = x, w = w, p = trim[2], 
                                       na.rm = na.rm)$est
        w <- w[x < trimQuant]
        x <- x[x < trimQuant]
      }
      else if(is.na(trim[2])){
        trimQuant <- weighted.quantile(x = x, w = w, p = trim[1], 
                                       na.rm = na.rm)$est
        w <- w[x > trimQuant]
        x <- x[x > trimQuant]
      }
      else{
        trimQuant <- weighted.quantile(x = x, w = w, p = trim, 
                                       na.rm = na.rm)$est
        trimQuant <- trimQuant[order(trimQuant)]
        w <- w[x > trimQuant[1] & x < trimQuant[2]]
        x <- x[x > trimQuant[1] & x < trimQuant[2]]
      }
    }
    else{stop("trim must be a numeric vector of length 1 or 2")}
  }
  mn <- sum(x * w) / sum(w)
  se <- weighted.sd(x, w, trim = NA, na.rm = na.rm)
  lower <- NULL
  upper <- NULL
  if(!is.na(alpha)){
    lower = mn - qnorm(1 - alpha / 2) * se
    upper = mn + qnorm(1 - alpha / 2) * se
  }
  return(c(wmean = mn, se = se, lowerCI = lower, upperCI = upper))
}
