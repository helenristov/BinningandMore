Contracts <- 'EDU6.U7'
Type <- 'TRS'

E <- as.POSIXct(paste0(Sys.Date() - 1 - 7 * 0, " ", getContractTimes(Contracts)$a.close))
S <- as.POSIXct(paste0(as.Date(E - 3600 * 24 * 7 * 8), " ", getContractTimes(Contracts)$a.open))

if(Type == 'MDR'){
  MT <- getContractMT(Contracts, TT_MinTick = TRUE)
  data <- Pull.MDR.Data(Contracts, S, E)[[1]]
  
  data <- cbind(cbind(data, 
                      ifelse(data[,'AP1'] - data[,'BP1'] > MT * 1.01, (data[,'AP1'] + data[,'BP1']) / 2, 
                             (data[,'BP1'] * data[,'AQ1'] + data[,'AP1'] * data[,'BQ1']) / (data[,'AQ1'] + data[,'BQ1']) )),
                ifelse(data[,'AP1'] - data[,'BP1'] > MT * 1.01, (data[,'AP1'] + data[,'BP1']) / 2, 
                       (data[,'BP1'] * data[,'AC1'] + data[,'AP1'] * data[,'BC1']) / (data[,'AC1'] + data[,'BC1']) ))
  colnames(data)[ncol(data)-1] <- 'WMP.Q'
  colnames(data)[ncol(data)  ] <- 'WMP.C'
  
  data <- cbind(cbind(data, (data[,'AP1'] - data[,'WMP.Q']) / MT), (data[,'AP1'] - data[,'WMP.C']) / MT)
  colnames(data)[ncol(data)-1] <- 'd.WMP.Q'
  colnames(data)[ncol(data)  ] <- 'd.WMP.C'
  
  tmp.data <- data[which(!is.na(data[,'TradedPrice']),)]
  tmp.data <- cbind(tmp.data, ifelse(tmp.data[,'TradedPrice'] <= tmp.data[,'BP1'], 0, ifelse(tmp.data[,'TradedPrice'] >= tmp.data[,'AP1'], 1, 2)))
  colnames(tmp.data)[ncol(tmp.data)] <- 'Buy.Sell'
  
  tmp.data <- cbind(tmp.data, 0.7 * tmp.data[,'d.WMP.Q'] + 0.3 * tmp.data[,'d.WMP.C'])
  colnames(tmp.data)[ncol(tmp.data)] <- 'd.WMP.M'
}else{
  MT <- getContractMT(Contracts)
  data <- Pull.TRS.Data(Contracts, S, E)[[1]]
  
  data <- cbind(data, 
                ifelse(data[,'BestAsk'] - data[,'BestBid'] > MT * 1.01, (data[,'BestAsk'] + data[,'BestBid']) / 2, 
                       (data[,'BestBid'] * data[,'AskQty'] + data[,'BestAsk'] * data[,'BidQty']) / (data[,'AskQty'] + data[,'BidQty']) ))
  colnames(data)[ncol(data)] <- 'WMP.Q'
  
  data <- cbind(data, (data[,'BestAsk'] - data[,'WMP.Q']) / MT)
  colnames(data)[ncol(data)] <- 'd.WMP.M'
  
  tmp.data <- data[which(!is.na(data[,'TradedPrice']),)]
  tmp.data <- cbind(tmp.data, ifelse(tmp.data[,'TradedPrice'] <= tmp.data[,'BestBid'], 0, ifelse(tmp.data[,'TradedPrice'] >= tmp.data[,'BestAsk'], 1, 2)))
  colnames(tmp.data)[ncol(tmp.data)] <- 'Buy.Sell'
}
source('/research/Binning Algos/Binary BS.R')

Bins <- CreateBinTable(tmp.data[,'d.WMP.M'], tmp.data[,'Buy.Sell'], NumOfBins = 20)
print(Bins)

data2examine <- tmp.data[which(tmp.data[,'Buy.Sell'] != 2),]

Bins <- BinBust(data2examine[,'d.WMP.M'], data2examine[,'Buy.Sell'], initLevels = 10, Break_Threshold = 0.1, Squelch_Threshold = 0.2, MinObs = 30)
print(Bins$varBin)
