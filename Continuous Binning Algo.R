### Pretend Data
BestBrackets <- as.matrix(sort(rnorm(5000, mean = 0, sd = 3)))
BestBrackets <- cbind(BestBrackets, as.matrix(rep(0, 5000)))
BestBrackets[1000:2000, 2] <- BestBrackets[3200:3400, 2] <- BestBrackets[1:4, 2] <- 1
BestBrackets[2495:2505, 2] <- 2

### Required Inputs

BT   <- 0.001
MO   <- 30
ST   <- 0.0005

### Create Bin Function
CreateBinTable <- function(x, y, z = NULL, UseBinNum = TRUE, NumOfBins, Breaks = -999){
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
  
  if(is.null(z)){
  ### Organizing and Sorting Bin Info
    Data <- cbind(as.matrix(as.numeric(x)), as.matrix(as.numeric(y)))
    colnames(Data) <- c('x', 'y')
    y_bar <- mean(Data[,'y'])
    var   <- var(Data[, 'y'])
   
  } else{
    Data <- cbind(as.matrix(as.numeric(x)), as.matrix(as.numeric(y)), as.matrix(as.numeric(z)))
    colnames(Data) <- c('x', 'y', 'z')
    y_bar <- weighted.mean(Data[,'y'], Data[,'z'])
    var <- sum(Data[,'z']*(Data[,'y'] - y_bar)^2)/sum(Data[,'z'])
  }
  
  if(UseBinNum){
    Breaks    <- unique(signif(quantile(Data[,'x'], probs = c(1:(NumOfBins-1)) / NumOfBins), 6))
  }
  
  Model.Data <- as.data.table(Data)
  Model.Data[, tag := character(.N)]
  
  BinObs <- list()
  for(i in 1:(length(Breaks) + 1)){
    if(i == 1){
      Names        <- paste0("x <= ", Breaks[i])
      BinObs[[i]] <- Data[which(Data[,'x'] <= Breaks[i]),] 
      Model.Data$tag[x <= Breaks[i]] <- Names[i]
      
    } else if(i == length(Breaks) + 1){
      Names       <- append(Names, paste0(Breaks[i - 1], " < x"))
      BinObs[[i]] <- Data[which(Data[,'x'] > Breaks[i - 1]),] 
      Model.Data$tag[x > Breaks[i-1]] <- Names[i] 
    } else{
      Names <- append(Names, paste0(Breaks[i - 1], "< x <= ", Breaks[i]))
      BinObs[[i]] <- Data[intersect(which(Data[,'x'] <= Breaks[i]), which(Data[,'x'] > Breaks[i - 1])),]
      Model.Data$tag[x <= Breaks[i] & x > Breaks[i - 1]] <- Names[i]
    }
  }
  names(BinObs) <- Names

  ### Creation of Bin Table
  BinTable <- matrix(nrow = length(BinObs), ncol = 5)
  colnames(BinTable) <- c('BinMin', 'BinMax' , 'TotalObs', 'WPct', 'NMean')
  rownames(BinTable) <- Names
  
  
  for(i in 1:length(BinObs)){
   
    BinTable[i,'BinMin']    <- min(BinObs[[i]][,'x'])
    BinTable[i,'BinMax']    <- max(BinObs[[i]][,'x'])
    BinTable[i,'TotalObs']  <- nrow(BinObs[[i]])
    BinTable[i,'WPct']      <- sum(BinObs[[i]][,'z'])/sum(Data[,'z'])
    
    if(is.null(weights)){
      
    BinTable[i, 'NMean']   <- (mean(BinObs[[i]][,'y']) - y_bar)/sqrt(var)
    }else{
    BinTable[i, 'NMean']   <- (weighted.mean(BinObs[[i]][,'y'],BinObs[[i]][,'z']) - y_bar)/sqrt(var)
    }
  }
  
  test <- as.data.frame(model.matrix(~., Model.Data)[,-(1)])
  
  
  reg.fit <- lm(y~. - x - z, weights = z, data = as.data.frame(test))
  r2 <- summary(reg.fit)$r.squared
  test$y_hat <- predict(reg.fit, newdata=test)
  test$resid <- residuals(reg.fit, newdata = test)
  
  
  test$tag <- Model.Data$tag
  
  ##create the bin R2 numbers for each level
  R2Table <- as.data.table(names(BinObs))
  R2Table[, levelR2 := numeric(.N)]
  
  rss <- sum(test$z * (test$resid)^2)
  mss <- sum(test$z*(test$y_hat - y_bar)^2)

  for(i in 1:nrow(R2Table)){
    BucketRowIDs <- which(test$tag == names(BinObs)[i])
    
    partial_mss <- sum(test$z[BucketRowIDs]*(test$y_hat[BucketRowIDs] - y_bar)^2)
    
    R2Table$levelR2[i] <- partial_mss / (mss + rss)
    
  }
  
  BinTable <- cbind(BinTable,R2Table$levelR2)
  colnames(BinTable) <- c('BinMin', 'BinMax' , 'TotalObs', 'WPct', 'NMean', 'R2')
  
  return(BinTable)
}


####  NEEDS TO BE MODIFIED FROM THIS POINT BELOW!

### Bin Bust
BinBust <- function(x, y, z, initLevels, Break_Threshold, MinObs){
  
  varBin    <-  CreateBinTable(x, y, z,UseBinNum = TRUE, NumOfBins = initLevels)
  varBinR2  <- sum(varBin[,'R2'])
  varBreaks <- append(as.numeric(varBin[,'BinMin']), max(as.numeric(varBin[,'BinMax'])))
  
  curvarBin   <- varBin
  curvarBinR2 <- varBinR2
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
      
      tmpvarBin <- CreateBinTable(x, y, z, UseBinNum = FALSE, Breaks = tmpBreaks)
      tmpvarBinR2 <- sum(tmpvarBin[,'R2'])
      
      infBinCheck <- sum(tmpvarBin[intersect(which(tmpvarBin[,'BinMin'] >= head(BinSeq,1)), which(tmpvarBin[,'BinMax'] <=tail(BinSeq,1))), 'R2'])
      
      if(tmpvarBinR2 - varBinR2 >= Break_Threshold && min(tmpvarBin[,"TotalObs"]) >= MinObs && infBinCheck != Inf && !Exit){
        curvarBin   <- tmpvarBin
        curvarBinR2 <- tmpvarBinR2
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
BinSquelch <- function(x,y,z,varBin, Squelch_Threshold){
  varBin <- Busted$varBin
  curBreaks<- varBin[,c('BinMin','BinMax')]
  orgBreaks<- curBreaks
  
  for(i in 2:nrow(varBin)){
    if(abs(varBin[i-1,"R2"] - varBin[i,'R2']) <= Squelch_Threshold || is.na(varBin[i,'R2'])){
      varBin[i, 'TotalObs'] <- sum(varBin[(i-1):i, "TotalObs"])
      varBin[i, 'WPct']   <- sum(varBin[(i-1):i, "WPct"])
     
      
      varBin[i,'BinMin'] <- varBin[(i-1),'BinMin']
      
      varBin[(i-1), 'TotalObs'] <- 0
      
    }
  }
  
  if(length(which(varBin[,'TotalObs'] == 0) > 0)){
    FinalBins <- varBin[-which(varBin[,'TotalObs'] == 0),]
  }else{
    FinalBins <- varBin
  }
  
  FinalBreaks <- FinalBreaks <- as.numeric(FinalBins[-1,'BinMin'])
  FinalBins   <- CreateBinTable(x, y, z, UseBinNum = FALSE, Breaks = FinalBreaks)
  
  Output <- list(varBin = FinalBins, breaks = FinalBreaks)
  return(Output)
}

Busted     <- BinBust(x = x, y = y, z = z, initLevels = 40, Break_Threshold = BT, MinObs = MO)
Squelched  <- BinSquelch(x = x, y = y, z = z, varBin = Busted$varBin, ST)

Squelched$varBin