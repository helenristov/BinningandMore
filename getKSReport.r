
#'@title Generate Standard KS Report
#'
#'@description Generate a standard gains table and report statistics including KS, ROC, IV and D.
#'
#'@details The KS Report function will execute and output the gains table and statistics that are computed discretely on this table.  User inputs include specification of the number of bins, the report score, the target variables which must contain only values of 0,1,2 where 0 and 1 are customizable to be Good or Bad, and 2 is indeterminant.  The target variable cannot contain missing values; it is suggested that the user pre-process the data so that missing are grouped into indeterminant or just exclude.  
#'
#'	The standard gains table is reported on these three levels: Good, Bad, and Indeterminant and can be outputted to a csv file if \code(file) is populated.  The discrete computation of KS, ROC, IV and D statistics are computed using gains table inData.  Those statistics are also written to the console and printed to the csv.
#'
#'
#'@param data The data that will be examined.  Enter a .sas, .csv, or data.frame object.
#'@param file The location of the csv file where the output will be stored.  Set to "" to avoid printing to a csv file and just print to console.
#'@param score The report score.
#'@param target The dependent variable.
#'@param numOfIntervals The number of intervals or bins over which the KS, ROC, IV, and D statisticswill be computed.
#'@param weights A weight variable.  In none set to \code(NULL)
#'@param filterLogical A logical expression that will be used to subset the data before computationof the report statistics\code(NULL)
#'@param cumulation Specifies the direction over which the bad and goods are accumulated.  Enter \code(CUMUP) to accumulate from the lowest score range to the highest.  Enter \code(CUMDOWN) to accumulate the \code(target) levels from the highest score range to the lowest.
#'@param direction The direction the gains table is reported.  Enter \code(UP) to report the score from low to high.  Enter \code(DOWN) to report from high to low.
#'@param reverseSchema The assignment of good and bad in your data.  \code(FALSE) sets the evaluation for the levels of the \code(target) variable : (0 = Bad, 1 = Good, and 2 = Indeterminant).  \code(TRUE) sets the evaluation for (1 = Bad, 0 = Good, and 2 = Indeterminant).
#'
#'@example \dontrun{
#'
#'## set parameters for KS Report
#'data           <- '/shared/shape_tier3/bu_fs/transient/UserDir/Data.csv'
#'file           <- '/shared/shape_tier3/bu_fs/transient/UserDir/Report.csv'
#'score          <- 'scaledScore'
#'target         <- 'BGI_12'
#'numOfIntervals <- 20
#'weights        <- 'rpt_wgt12'
#'filterLogical  <- NULL
#'direction      <- 'UP'  ## report UP or DOWN
#'reverseSchema  <- FALSE ## reverseSchema = FALSE -> Bad = 0, Good = 1
#'                           reverseSchema = TRUE  -> Bad = 1, Good = 0
#'cumulation     <- 'CUMDOWN'
#'
#'getKSReport(data, file, target, weights, score, numOfIntervals, filterLogical, cumulation, reverseSchema)
#'}
#'
#'@author Helen Ristov
#'
#'@export
#'

getKSReport(data, file, weights = NULL, score, numOfIntervals, filterLogical = NULL, cumulation = 'CUMUP', direction = 'DOWN', reverseSchema = FALSE){
	
	## Read the data
	if("data.frame" %in% class(data)){
		inData <- data
	}else if(substr(data, nchar(data) - 3, nchar(data)) %in% c(".sas", ".csv")){
		inData <- rxImport(data)
	}else{
		stop("This function requires a data.frame object, csv file or sas data")
	}

	##error handling
	if(!toupper(score) %in% toupper(names(inData))){
		stop("score is not found in the data")
	}
	
	if(!toupper(target) %in% toupper(names(inData))){
		stop("target is not found in the data")
	}
	
	if(!is.null(weights)){
		if(!(toupper(weights) %in% toupper(names(inData)))){
			stop("weights is not found in the data and is not set to NULL")
		}
	}
	
	if(!(toupper(direction) %in% c('UP','DOWN'))){
		stop("direction must be specified as UP or DOWN")
	}
	
	if(!(toupper(cumulation) %in% c('CUMUP','CUMDOWN'))){
		stop("cumulation must be specified as UP or DOWN")
	}
	
	## Subsetting inData to Handle Inputted Filter Logic
	if(!is.null(filterLogical)){
		inData <- subset(inData, eval(parse(text=filterLogical)))
	}
	
	## Set the bad definition according to the targetScheme
	inData$Bad <- inData$Good <- inData$Indeterminant = 0
	inData$Tot <- 1
	
	## coercing the target into a character field
	inData[,target] <- as.character(inData[,target])
	
	#target.Values <- unique(inData[,target])
	#target.Values <- target.Values[!target.Values %in% c("1","0","2")]  ## determine other bad tags that are not (0,1,2)
	
	badRows  <- which(inData[target] == ifelse(reverseSchema, '1', '0'))  #values of 1 are assigned to bad for reverseSchema == TRUE and values of 0 are assigned to bad for reverseSchema == FALSE
	goodRows <- which(inData[target] == ifelse(reverseSchema, '0', '1'))  #values of 0 are assigned to good for reverseSchema == TRUE and values of 1 are assigned to good for reverseSchema == FALSE
	indRows  <- which(inData[target] == '2')  #values of 2 are assigned to indeterminant
		
	## Assignment of Bad, Good, and Indeterminant fields
	inData$Bad[badRows  ] <- 1
	inData$Good[goodRows] <- 1
	inData$Ind[indRows  ] <- 1
	
	if(!is.null(weights){
		inData$BadWgt <- inData$GoodWgt <- inData$IndWgt <- 0
		inData$BadWgt[badRows  ] <- inData[badRows ,weights]
		inData$GoodWgt[goodRows] <- inData[goodRows,weights]
		inData$IndWgt[indRows  ] <- inData[indRows ,weights]
	}
	
	Buckets <- c(0:numOfIntervals) / numOfIntervals
	
	Quantiles <- quantile(inData[score], probs = Buckets, na.rm = TRUE)
	quantileData <- data.frame(id = names(Quantiles), values = Quantiles)
	
	if(length(which(quantileData[,'values'] == max(quantileData[,'values']))) > 1){
		stop("Not enough variance in the score, lower the number of bins")
	}
	
	## creation of the gains table to be outputted into a csv file
	GainsTable <- data.frame(Score.Range = c(1:numOfIntervals), Totals = NA, Cum.Tot = NA, Bad = NA, Bad.Rate = NA, Cuml.Bad = NA, Good = NA, Good.Rate = NA, Cuml.Good = NA, Ind = NA, Ind.Rate = NA, Cuml.Ind = NA, KS = NA, Avg.Score = NA)
	
	for(i in 1:numOfIntervals){
		if(sum(as.double(inData[,'score']))%%1 != 0){
			if(i == 1){
				TempinData <- subset(inData, inData[score] >= quantileData$values[i] & inData[score] <= quantileData$values[i+1])
				GainsTable$Score.Range[i] <- paste(round(quantileData$values[i], digits = 4), "-", round(quantileData$values[i=1], digits = 4), sep = " ")
			}else{
				TempinData <- subset(inData, inData[score] >  quantileData$values[i] & inData[score] <= quantileData$values[i+1])
				GainsTable$Score.Range[i] <- paste(round(quantileData$values[i], digits = 4), "-", round(quantileData$values[i=1], digits = 4), sep = " ")
			}
		}else{
			if(i == 1){
				TempinData <- subset(inData, inData[score] >= quantileData$values[i] & inData[score] <= quantileData$values[i+1])
				GainsTable$Score.Range[i] <- paste(quantileData$values[i], "-", quantileData$values[i=1], sep = " ")
			}else{
				TempinData <- subset(inData, inData[score] >  quantileData$values[i] & inData[score] <= quantileData$values[i+1])
				GainsTable$Score.Range[i] <- paste(quantileData$values[i], "-", quantileData$values[i=1], sep = " ")
			}
		}
		
		if(is.null(weights)){
			GainsTable$Bad[i]       <- sum(TempinData$Bad)
			GainsTable$Good[i]      <- sum(TempinData$Good)
			GainsTable$Ind[i]       <- sum(TempinData$Ind)
			GainsTable$Totals[i]    <- sum(TempinData$Tot)
			GainsTable$Avg.Score[i] <- colMeans(TempinData[score], na.rm = TRUE)
		}else{
			GainsTable$Bad[i]       <- sum(TempinData$BadWgt)
			GainsTable$Good[i]      <- sum(TempinData$GoodWgt)
			GainsTable$Ind[i]       <- sum(TempinData$IndWgt)
			GainsTable$Totals[i]    <- GainsTable$Bad[i] + GainsTable$Good[i] + GainsTable$Ind[i]
			GainsTable$Avg.Score[i] <- colMeans(TempinData[score], na.rm = TRUE)
		}
	}
	
	GainsTable$Bad.Rate  <- (GainsTable$Bad  / GainsTable$Totals)
	GainsTable$Good.Rate <- (GainsTable$Good / GainsTable$Totals)
	GainsTable$Ind.Rate  <- (GainsTable$Ind  / GainsTable$Totals)
	
	if(toupper(cumulation) == 'CUMUP'){
		GainsTable$Cuml.Tot  <- cumsum(GainsTable$Totals) / cumsum(GainsTable$Totals)[length(cumsum(GainsTable$Totals))]
		GainsTable$Cuml.Bad  <- cumsum(GainsTable$Bad   ) / cumsum(GainsTable$Bad   )[length(cumsum(GainsTable$Bad   ))]
		GainsTable$Cuml.Good <- cumsum(GainsTable$Good  ) / cumsum(GainsTable$Good  )[length(cumsum(GainsTable$Good  ))]
		GainsTable$Cuml.Ind  <- cumsum(GainsTable$Ind   ) / cumsum(GainsTable$Ind   )[length(cumsum(GainsTable$Ind   ))]
	}	
	
	if(toupper(cumulation) == 'CUMDOWN'){
		GainsTable$Cuml.Tot  <- cumsum(GainsTable$Totals[order(-1:-nrow(GainsTable))]) / cumsum(GainsTable$Totals)[length(cumsum(GainsTable$Totals))]
		GainsTable$Cuml.Bad  <- cumsum(GainsTable$Bad   [order(-1:-nrow(GainsTable))]) / cumsum(GainsTable$Bad   )[length(cumsum(GainsTable$Bad   ))]
		GainsTable$Cuml.Good <- cumsum(GainsTable$Good  [order(-1:-nrow(GainsTable))]) / cumsum(GainsTable$Good  )[length(cumsum(GainsTable$Good  ))]
		GainsTable$Cuml.Ind  <- cumsum(GainsTable$Ind   [order(-1:-nrow(GainsTable))]) / cumsum(GainsTable$Ind   )[length(cumsum(GainsTable$Ind   ))]
		
		GainsTable$Cuml.Tot  <- GainsTable$Cuml.Tot[ order(-1:-nrow(GainsTable))]
		GainsTable$Cuml.Bad  <- GainsTable$Cuml.Bad[ order(-1:-nrow(GainsTable))]
		GainsTable$Cuml.Good <- GainsTable$Cuml.Good[order(-1:-nrow(GainsTable))]
		GainsTable$Cuml.Ind  <- GainsTable$Cuml.Ind[ order(-1:-nrow(GainsTable))]
	}
	
	GainsTable$KS <- abs(GainsTable$Bad - GainsTable$Good)
	GainsTable$IV <- (GainsTable$Good/sum(GainsTable$Good) - GainsTable$Bad/sum(GainsTable$Bad)) * log((GainsTable$Good / GainsTable$Bad) / (sum(GainsTable$Good) / sum(GainsTable$Bad)))
	
	##  STATS -- Calculate Gini, ROC, IV, D, KS
	MeanGood <- mean(subset(inData, Good == 1)[,score])
	MeanBad  <- mean(subset(inData, Bad  == 1)[,score])
	VarGood  <- mean(subset(inData, Good == 1)[,score])
	VarBad   <- mean(subset(inData, Bad  == 1)[,score])

	if(!is.null(weights)){
		GoodSet <- subset(inData, Good == 1)
		BadSet  <- subset(inData, Good == 1)
		
		WgtdGood <- sum(GoodSet[,score] * GoodSet[,weights]) / sum(GoodSet$GoodWgt)
		WgtdBad  <- sum(BadSet[ ,score] * BadSet[ ,weights]) / sum(BadSet$BadWgt)
		
		WgtdGoodVar <- sum(GoodSet$GoodWgt * (GoodSet[,score] - WgtdGood)^2) / (sum(GoodSet$GoodWgt) - 1)
		WgtdBadVar  <- sum(BadSet$BadWgt   * (BadSet[, score] - WgtdBad )^2) / (sum(BadSet$BadWgt)   - 1)
		
		D <- round((WgtdGood - WgtdBad) / sqrt(0.5 * (WgtdGoodVAr + WgtdBadVar)), digits = 4)
	}else{
		D <- round((MeanGood - MeanBad) / sqrt(0.5 * (GoodVAr + BadVar)), digits = 4)
	}

	IV <- round(sum(GainsTable$IV), digits = 4)
	
	## Gini Calculation -- Using the Trapezoid Rule
	base1     <- c(0, GainsTable$Cuml.Bad[-length(GainsTable$Cuml.Bad)])
	base2     <- GainsTable$Bad
	incrGood  <- diff(c(0, GainsTable$Cuml.Good))
	TrapAreas <- (base1 + base2) * incrGood * 0.5
	Gini      <- round((sum(TrapAreas) - 0.5) * 2, digits = 4)
	
	if(toupper(cumulation) == 'CUMDOWN'){
		Gini <- round(1 + Gini, digits = 4)
	}
	
	ROC <- round((Gini + 1) / 2, digits = 4)
	KS  <- round(max(GainsTable$KS), digits = 4)
	
	## Change to report either UP or DOWN
	if(toupper(direction) == "DOWN"){
		GainsTable <- GainsTable[order(-1:-nrow(GainsTable)),]
	}else if(!toupper(direction) == "UP"){
		stop("direction parameter can only be 'UP' or 'DOWN'.")
	}
	
	## Cleanup the table by setting the precision before outputting to a csv file
	GainsTable$Bad       <- round(GainsTable$Bad   , digits = 0)
	GainsTable$Good      <- round(GainsTable$Good  , digits = 0)
	GainsTable$Ind       <- round(GainsTable$Ind   , digits = 0)
	GainsTable$Totals    <- round(GainsTable$Totals, digits = 0)
	GainsTable$Bad.Rate  <- round(100 * GainsTable$Bad.Rate , digits = 2)
	GainsTable$Good.Rate <- round(100 * GainsTable$Good.Rate, digits = 2)
	GainsTable$Ind.Rate  <- round(100 * GainsTable$Ind.Rate , digits = 2)
	GainsTable$Cuml.Tot  <- round(100 * GainsTable$Cuml.Tot , digits = 2)
	GainsTable$Cuml.Bad  <- round(100 * GainsTable$Cuml.Bad , digits = 2)
	GainsTable$Cuml.Good <- round(100 * GainsTable$Cuml.Good, digits = 2)
	GainsTable$Cuml.Ind  <- round(100 * GainsTable$Cuml.Ind , digits = 2)
	GainsTable$KS        <- round(GainsTable$KS, digits = 4)
	GainsTable$IV        <- round(GainsTable$IV, digits = 4)
	GainsTable$Avg.Score <- round(GainsTable$Avg.Score, digits = 0)
	
	if(!file == ""){
		write.table(GainsTable, file = file, sep= ",", row.names = FALSE)
		write.table(" "       , file = file, sep= ",", row.names = FALSE  , col.names = FALSE, append = TRUE)
		write.table(KS        , file = file, sep= ",", col.names = "KS"   , row.names = FALSE, append = TRUE)
		write.table(IV        , file = file, sep= ",", col.names = "IV"   , row.names = FALSE, append = TRUE)
		write.table(D         , file = file, sep= ",", col.names = "D"    , row.names = FALSE, append = TRUE)
		write.table(Gini      , file = file, sep= ",", col.names = "Gini" , row.names = FALSE, append = TRUE)
		write.table(ROC       , file = file, sep= ",", col.names = "ROC"  , row.names = FALSE, append = TRUE)
	}
	
	return(list("KS" = KS, "Gini" = Gini, "ROC" = ROC, "IV" = IV, "D" = D, "GainsTable" = GainsTable))
}




