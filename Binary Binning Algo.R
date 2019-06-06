
### Create Bin Function
CreateBinTable <- function(x, y, UseBinNum = TRUE, NumOfBins = -1, Breaks = -999){
  ### Checking Data Integrity
  if(UseBinNum && NumOfBins < 0){
    stop('Need a positive number for bins if table is to bin on a given number')
  }
  
  if(!UseBinNum && Breaks[1] == -999){
    stop('Need a vector of breakpoints to create a set of custom bins')
  }
  
  if(length(x) != length(y)){
    stop('x and y vectors need to be the same length')
  }
  
  ### Organizing and Sorting Bin Info
  Data <- cbind(as.matrix(as.numeric(x)), as.matrix(as.numeric(y)))
  
  if(length(which(is.na(Data))) >0){
    Data <- Data[-which(is.na(Data)),]
  }
  colnames(Data) <- c('x', 'y')
  
  if(UseBinNum){
    Breaks    <- unique(signif(quantile(Data[,'x'], probs = c(1:(NumOfBins-1)) / NumOfBins), 6))
  }
  
  BinObs <- list()
  for(i in 1:(length(Breaks) + 1)){
    if(i == 1){
      Names        <- paste0("x <= ", Breaks[i])
      BinObs[[i]] <- Data[which(Data[,'x'] <= Breaks[i]),]
    } else if(i == length(Breaks) + 1){
      Names       <- append(Names, paste0(Breaks[i - 1], " < x"))
      BinObs[[i]] <- Data[which(Data[,'x'] > Breaks[i - 1]),] 
    } else{
      Names <- append(Names, paste0(Breaks[i - 1], "< x <= ", Breaks[i]))
      BinObs[[i]] <- Data[intersect(which(Data[,'x'] <= Breaks[i]), which(Data[,'x'] > Breaks[i - 1])),] 
    }
    if(is.null(nrow(BinObs[[i]]))){ BinObs[[i]] <- t(data.matrix(BinObs[[i]]))}
  }
  names(BinObs) <- Names
  
  ### Creation of Bin Table
  BinTable <- matrix(nrow = length(BinObs), ncol = 8)
  colnames(BinTable) <- c('BinMin', 'BinMax', 'TotalObs', 'AffObs', 'NegObs', 'IndObs', 'IV', 'WOE')
  rownames(BinTable) <- Names
  
  for(i in 1:length(BinObs)){
    BinTable[i,'BinMin']   <- ifelse(i == 1, min(x, na.rm=TRUE), Breaks[i-1])
    BinTable[i,'BinMax']   <- ifelse(i == length(BinObs), max(x, na.rm=TRUE), Breaks[i])
    
    BinTable[i,'TotalObs'] <- nrow(BinObs[[i]])
    BinTable[i,'AffObs']   <- ifelse(length(which(BinObs[[i]][,'y'] == 1)) > 0, length(BinObs[[i]][which(BinObs[[i]][,'y'] == 1),'y']), 0)
    BinTable[i,'NegObs']   <- ifelse(length(which(BinObs[[i]][,'y'] == 0)) > 0, length(BinObs[[i]][which(BinObs[[i]][,'y'] == 0),'y']), 0)
    BinTable[i,'IndObs']   <- ifelse(length(which(BinObs[[i]][,'y'] == 2)) > 0, length(BinObs[[i]][which(BinObs[[i]][,'y'] == 2),'y']), 0)    
  }
  BinTable[,'WOE'] <- log((BinTable[,'AffObs'] / sum(BinTable[,'AffObs'])) / (BinTable[,'NegObs'] / sum(BinTable[,'NegObs'])))
  BinTable[,'IV']  <- ((BinTable[,'AffObs'] / sum(BinTable[,'AffObs'])) - (BinTable[,'NegObs'] / sum(BinTable[,'NegObs']))) * BinTable[,'WOE']
  
  return(BinTable)
}


### Bin Bust
BinBust <- function(x, y, initLevels, Break_Threshold, Squelch_Threshold, MinObs){
  
  varBin    <- CreateBinTable(x, y, UseBinNum = TRUE, NumOfBins = initLevels)
  varBin    <- BinSquelch(varBin, Squelch_Threshold)$varBin
  varBinIV  <- sum(varBin[,'IV'])
  varBreaks <- append(as.numeric(varBin[,'BinMin']), max(as.numeric(varBin[,'BinMax'])))
  
  curvarBin   <- varBin
  curvarBinIV <- varBinIV
  curBreaks   <- varBreaks[-c(1, length(varBreaks))]
  
  ### Determine Breaks
  for(i in 1:nrow(varBin)){
    Exit      <- FALSE
    BreakBins <- 2
    while(!Exit){
      BinSeq <- seq(from = varBreaks[i], to = varBreaks[i + 1], length.out = BreakBins + 1)
      if(i == 1){
        tmpBreaks <- c(BinSeq[-c(1,length(BinSeq))], curBreaks)
      }else if(i == nrow(varBin)){
        tmpBreaks <- c(curBreaks, BinSeq[-c(1,length(BinSeq))])
      }else{
        tmpBreaks <- c(curBreaks[which(curBreaks < varBreaks[i])], BinSeq, curBreaks[which(curBreaks > varBreaks[i + 1])])
      }
      
      tmpvarBin <- CreateBinTable(x, y, UseBinNum = FALSE, Breaks = tmpBreaks)
      tmpvarBinIV <- sum(tmpvarBin[,'IV'])
      
      infBinCheck <- sum(tmpvarBin[intersect(which(tmpvarBin[,'BinMin'] >= first(BinSeq)), which(tmpvarBin[,'BinMax'] <= last(BinSeq))), 'IV'])
      
      if(tmpvarBinIV - varBinIV >= Break_Threshold && min(tmpvarBin[,"TotalObs"]) >= MinObs && infBinCheck != Inf && !Exit){
        
        curvarBin   <- tmpvarBin
        curvarBinIV <- tmpvarBinIV
        curBreaks   <- tmpBreaks
        
        BreakBins <- BreakBins * 2
        
      }else{
        Exit <- TRUE
      }
    }
  }
  
  Output <- list(varBin = curvarBin, breaks = curBreaks)
  return(Output)
}

### BinSquelch
BinSquelch <- function(varBin, Squelch_Threshold){
  curBreaks<- varBin[,c('BinMin','BinMax')]
  orgBreaks<- curBreaks
  
  if(nrow(varBin) == 1){ return(list(varBin = varBin, breaks = NA)) }
  
  for(i in 2:nrow(varBin)){
    if(abs(varBin[i-1,"IV"] - varBin[i,'IV']) <= Squelch_Threshold || varBin[i - 1,'IV'] %in% c('Inf', NaN) || varBin[i,'IV'] %in% c('Inf', NaN)){
      varBin[i, 'TotalObs'] <- sum(varBin[(i-1):i, "TotalObs"])
      varBin[i, 'AffObs']   <- sum(varBin[(i-1):i, "AffObs"])
      varBin[i, 'NegObs']   <- sum(varBin[(i-1):i, "NegObs"])
      varBin[i, 'IndObs']   <- sum(varBin[(i-1):i, "IndObs"])
      
      varBin[i,'WOE'] <- log((varBin[i,'AffObs'] / sum(varBin[,'AffObs'])) / (varBin[i,'NegObs'] / sum(varBin[,'NegObs'])))
      varBin[i,'IV']  <- ((varBin[i,'AffObs'] / sum(varBin[,'AffObs'])) - (varBin[i,'NegObs'] / sum(varBin[,'NegObs']))) * varBin[i,'WOE']
      
      varBin[i,'BinMin'] <- varBin[(i-1),'BinMin']
      
      varBin[(i-1), 'TotalObs'] <- varBin[(i-1), 'AffObs']   <- varBin[(i-1), 'NegObs']   <- varBin[(i-1), 'IndObs']   <- 0
      varBin[(i-1),'WOE'] <- varBin[(i-1),'IV']  <- varBin[(i-1),'BinMin'] <- varBin[(i-1),'BinMax'] <- 0
    }
  }
  
  if(length(which(varBin[,'TotalObs'] == 0)) > 0){
    FinalBins <- varBin[-which(varBin[,'TotalObs'] == 0),]
    if(is.numeric(FinalBins) && !is.matrix(FinalBins)){ FinalBins <- t(as.matrix(FinalBins))}
  }else{
    FinalBins <- varBin
  }
  
  rownames(FinalBins) <- paste0(signif(FinalBins[,'BinMin'], 6), " < x <= ", signif(FinalBins[,'BinMax'], 6))
  FinalBreaks <- as.numeric(FinalBins[-1,'BinMin'])
  
  Output <- list(varBin = FinalBins, breaks = FinalBreaks)
  return(Output)
}

