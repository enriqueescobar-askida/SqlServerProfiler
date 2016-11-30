library(xlsx);
#' Title  TraceXmlXlsToTibble
#'
#' @param filePath 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
TraceXmlXlsToTibble <- function(filePath = "") {

  if (is.null(filePath)) {

    return(NULL);
  } else {
    if (is.list(filePath)) {
      filePath <- unlist(filePath);
    }
    xlsFile <- xlsx::read.xlsx2(filePath,
                                sheetIndex = 1,
                                as.data.frame = TRUE,
                                header = FALSE);
      
    return(tibble::as_data_frame(xlsFile[-2]));
  }
}
#' Title  IsConvertibleToDate
#'
#' @param anObject 
#'
#' @return logical
#' @export TBD
#'
#' @examples TBD
IsConvertibleToDate <- function(anObject = "") {
  if (is.null(anObject) || is.na(anObject) || is.nan(anObject)) {
  
      return(FALSE);
  } else {
    d <- try( as.Date( as.POSIXct(anObject), #as.character(anObject)
                       format = "%d-%m-%Y %H:%M:%S" ) );
    
    if ( class(d) == "try-error" || is.na(d) ) {
      
      return(FALSE);
    } else {
      
      return(TRUE);
    }
  }
}
#' Title  IsEvenInteger
#'
#' @param aInt 
#'
#' @return logical
#' @export TBD
#'
#' @examples TBD
IsEvenInteger <- function(aInt = 0) {
  
  boo <- FALSE;
  
  if (is.integer(aInt)) {
    
    boo <- (aInt %% 2) == 0;
  } else {
    
    boo <- FALSE;
  }
  
  return(boo);
}
library(data.table);
#' Title  TrimXmlXlsToTibble
#'
#' @param aTibble 
#'
#' @return data.frame
#' @export TBD
#'
#' @examples TBD
TrimXmlXlsToTibble <- function(aTibble = NULL) {
  nbLines <- nrow(aTibble);
  aDataFr <- data.frame(NULL);
  count <- 0;
  
  for (aLine in seq(nbLines))
  {
    if (IsConvertibleToDate(aTibble[[aLine,]])) {
    } else {
      count <- count + 1;
      aString <- as.character(aTibble[[aLine,]]);
      print(aString);
      aDataFr <- rbind(aDataFr, aString);
    }
  }
  colnames(aDataFr) <- c("SqlTrace");
  aDataFr <- data.frame(lapply(aDataFr, as.character), stringsAsFactors = FALSE);
  print(count);
  
  return(tibble::as_data_frame(aDataFr));
}
#' Title  ScreenXmlXlsFiles
#'
#' @param fileList 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
ScreenXmlXlsFiles <- function(fileList = list()){
  
  if (is.null(fileList)) {
    
    return(NULL);
  } else {
    singleDataFrame <- NULL;
    
    if (length(fileList) < 2) {
      oneFile <- unlist(fileList[[1]]);
      singleDataFrame <- TraceXmlXlsToTibble(oneFile);
    } else {
      
      for (singleFile in fileList) {
        print(unlist(singleFile));
        aDf <- TraceXmlXlsToTibble(singleFile);
        singleDataFrame <- rbind(singleDataFrame, aDf);
      }
    }
    singleDataFrame <- data.frame(lapply(singleDataFrame, as.character), stringsAsFactors = FALSE);
    colnames(singleDataFrame) <- c("SqlTrace");
    
    return(tibble::as_data_frame(singleDataFrame));
  }
}
#' Title  TraceXmlXlsToCsv
#'
#' @param aTibble 
#' @param filePath 
#'
#' @return logical
#' @export TBD
#'
#' @examples TBD
TraceXmlXlsToCsv <- function(aTibble = data.frame(NULL), filePath = ""){
  
  if (is.null(aTibble)) {
    
    return(FALSE);
  } else {
    aTibble <- aTibble[!grepl("^2016", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("-- network protocol", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("SET TRANSACTION ISOLATION LEVEL READ", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("SET NOCOUNT ON", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("exec sp_cursorfetch ", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("exec sp_cursorclose ", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("exec sp_reset_connection", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("exec sp_cursoroption ", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("exec sp_cursor ", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("set implicit_transactions on", aTibble$SqlTrace), ];
    aTibble <- aTibble[!grepl("set implicit_transactions off", aTibble$SqlTrace), ];
    write.csv(aTibble, file = "filePath");
    
    return(TRUE);
  }
}

ScreenTxtFiles <- function(fileList = list()) {
  
  for (singleFile in fileList) {
    print(singleFile);
    singleText <- readLines(singleFile);
    corpusWords <- Corpus(VectorSource(singleText));
    # inspect(corpusWords);
    termDocMatrixSortDesc <- sort(rowSums(as.matrix(TermDocumentMatrix(corpusWords))),
                                  decreasing  = TRUE);
    termDocDataFrameSortDesc <- data.frame(word = names(termDocMatrixSortDesc),
                                           freq = termDocMatrixSortDesc);
    termDocDataFrameSortDesc <- tibble::as_data_frame(termDocDataFrameSortDesc);
    print(head(termDocDataFrameSortDesc));
    singleWordcloudPng <- gsub(".txt", ".wordcloud.png", singleFile);
    WordcloudToPng(termDocDataFrameSortDesc, singleWordcloudPng);
    # WorcloudToBarplot(termDocDataFrameSortDesc);
    singleWordcloudFreq <- gsub(".txt", ".freq.csv", singleFile);
    print(singleWordcloudFreq);
    write.csv2(termDocDataFrameSortDesc, file = singleWordcloudFreq, row.names = FALSE);
  }
}

library(tm);
library(wordcloud);
#' Title
#'
#' @param dataFrameSortDesc
#' @param pngFilePath
#' @export TBD
#'
#' @examples TBD
WordcloudToPng <- function(dataFrameSortDesc = NULL, pngFilePath = ""){
  
  if (is.null(dataFrameSortDesc)) {
    
    return(FALSE);
  } else {
    # graph
    png(filename = pngFilePath, width = 800, height = 800);
    wordcloud(words = dataFrameSortDesc$word,
              freq = dataFrameSortDesc$freq,
              min.freq = 1,
              max.words = 200,
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"));
    dev.off();
    
    return(TRUE);
  }
}
#' Title  WorcloudToBarplot
#'
#' @param aSortedDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
WorcloudToBarplot <- function(aSortedDataFrame = NULL){
  
  if (is.null(aSortedDataFrame)) {
    
    return(NULL);
  } else {
    aDataFrame <- data.frame(NULL);
    
    if (nrow(aSortedDataFrame) > 40) {
      aDataFrame <- head(aSortedDataFrame, 40);
    } else {
      aDataFrame <- aSortedDataFrame;
    }
    
    aBarplot <- barplot(aDataFrame$freq,
                        las = 2,
                        names.arg = aDataFrame$word,
                        col = "lightblue",
                        main = "Most frequent words",
                        ylab = "Word frequencies");
    
    return(aBarplot);
  }
}
#' Title  XlsToDataFrameSingleRow
#'
#' @param xlsFile 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
XlsToDataFrameSingleRow <- function(xlsFile = ""){
  aDF <- NULL;
  
  if (!is.null(xlsFile)) {
    aDF <- xlsx::read.xlsx2(xlsFile,
                                sheetIndex = 1,
                                as.data.frame = TRUE,
                                header = TRUE);
    aDF <- head(aDF, 1);
    aDF <- tibble::as_data_frame(aDF);
  }
  
  return(aDF);
}
#' Title  XlsToDataFrame
#'
#' @param xlsFile 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
XlsToDataFrame <- function(xlsFile = ""){
  aDF <- NULL;
  
  if (!is.null(xlsFile)) {
    aDF <- xlsx::read.xlsx2(xlsFile,
                            sheetIndex = 1,
                            as.data.frame = TRUE,
                            header = TRUE);
    aDF <- tibble::as_data_frame(aDF);
  }
  
  return(aDF);
}
#' Title  XlsFileToPng
#'
#' @param xlsFilePath 
#' @param dbName 
#' @param chartName 
#'
#' @return character
#' @export TBD
#'
#' @examples TBD
XlsFileToPng <- function(xlsFilePath = "", dbName = "", chartName = ""){
  # extension time
  # timeNow <- format(Sys.time(), "-%Y-%m-%d_%Hh%M");
  # timeExt <- paste0(timeNow, chartName, ".png");
  timeExt <- paste0(chartName, ".png");
  # png
  filePath <- gsub(".xls", timeExt, xlsFilePath);
  #filePath <- gsub("SQL/", paste0("../../ODBC/", dbName, "-", Sys.getenv("ASKIDA_ENV"), "-"), filePath);
  
  return(filePath);
}
#' Title  ObjectListGroupFunctionsDataFrame
#'
#' @param objectDataFrame
#' @param functionFilter
#'
#' @return data.frame
#' @export TBD
#'
#' @examples TBD
ObjectListGroupFunctionsDataFrame <- function(objectDataFrame, functionFilter="Function"){
  colnames <- colnames(objectDataFrame);
  # separate functions
  functionsDataFrame <- subset(objectDataFrame, grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  # sum functions
  functionsDataFrame <- data.frame(functionFilter, sum(functionsDataFrame[[2]]));
  colnames(functionsDataFrame) <- colnames;
  # remove functions
  objectDataFrame <- subset(objectDataFrame, !grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  # add sum functions
  objectDataFrame <- rbind(objectDataFrame, functionsDataFrame);
  
  return(tibble::as_data_frame(objectDataFrame));
}
#' Title  SummarizeAllDBFunctionDataFrame
#'
#' @param objectDataFrame
#' @param functionFilter
#'
#' @return data.frame
#' @export TBD
#'
#' @examples TBD
SummarizeAllDBFunctionDataFrame <- function(objectDataFrame, functionFilter="Function"){
  colnames <- colnames(objectDataFrame);
  # separate functions
  functionsDataFrame <- subset(objectDataFrame, grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  colnames <- c("FunctionTypes", "FunctionCount");
  colnames(functionsDataFrame) <- colnames;
  
  return(tibble::as_data_frame(functionsDataFrame));
}
#' Title  DataFrameFromColumns
#'
#' @param aDataFrame
#' @param colName1
#' @param colName2
#' @param colName3
#'
#' @return data.frame
#' @export TBD
#'
#' @examples TBD
DataFrameFromColumns <- function(aDataFrame = NULL,
                                 colName1 = "",
                                 colName2 = "",
                                 colName3 = ""){
  columnList <- c(colName1, colName2, colName3);
  write(columnList, stdout());
  indexList <- which(colnames(aDataFrame) %in% columnList);
  write(indexList, stdout());
  
  return(tibble::as_data_frame(aDataFrame[indexList]));
}
#' Title  DataFrameWithoutWithTotal
#'
#' @param intWithout
#' @param intWith
#' @param intTotal
#' @param OutputType
#'
#' @return data.frame
#' @export TBD
#'
#' @examples TBD
DataFrameWithoutWithTotal <- function(intWithout = 1,
                                      intWith = 1,
                                      intTotal = 2,
                                      OutputType = ""){
  aDataFrame <- data.frame(NULL);
  aDataFrame[1, 1] <- intWithout;
  aDataFrame[1, 2] <- intWith;
  aDataFrame[1, 3] <- intTotal;
  colnames(aDataFrame) <- c(paste0(OutputType,"WithoutParameters"),
                            paste0(OutputType,"WithParameters"),
                            paste0(OutputType,"Total"));
  
  return(tibble::as_data_frame(aDataFrame));
}
#' Title  ConstraintToTableNameFrequency
#'
#' @param aDataFrame 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
ConstraintToTableNameFrequency <- function(aDataFrame = NULL) {
  aDF <- data.frame(NULL);
  
  if (!is.null(aDataFrame)) {
    aDF <- tibble::as_data_frame(sort(table(aDataFrame$TableName), decreasing = TRUE));
    colnames(aDF) <- c("TableName", "ConstraintCount");
  }
  
  return(aDF);
}
library(ggplot2);
library(gridExtra);
#' Title  GgplotToPng
#'
#' @param pngFilePath
#'
#' @export TBD
#'
#' @examples TBD
GgplotToPng <- function(pngFilePath = "", barplot = NULL) {
  
  if (is.na(barplot)) {
    
    return(FALSE);
  } else {
    write(paste0("Saving to path : ", pngFilePath), stdout());
    ggsave(filename = pngFilePath, plot = barplot, dpi = 100);
    
    return(TRUE);
  }
}
#' Title  TwoColumnDataFrameToBarlot
#'
#' @param aDataFrame 
#' @param mainTitle 
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
TwoColumnDataFrameToBarlot <- function(aDataFrame = NULL, mainTitle = "") {
  
  if (is.null(aDataFrame)) {
    
    return(NULL);
  } else {
    write(paste0("Column name: ", colnames(aDataFrame)), stdout());
    aDataFrame <- head(aDataFrame, 50);
    # titles
    xTitle <- colnames(aDataFrame)[1];
    yTitle <- colnames(aDataFrame)[2];
    colnames(aDataFrame) <- NULL;
    names(aDataFrame)[1] <- "X";
    names(aDataFrame)[2] <- "Y";
    # graph
    barplot <- ggplot(aDataFrame,
                      aes(x = factor(X), y = Y)) +
                geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
                xlab(xTitle) +
                ylab(yTitle) +
                ggtitle(mainTitle) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5));
    
    return(barplot);
  }
}
#' Title  DBUsageDataFrameToBarplot
#'
#' @param usageDataFrame
#' @param pngFilePath
#'
#' @return ggplot
#' @export TBD
#'
#' @examples TBD
DBUsageDataFrameToBarplot <- function(usageDataFrame = NULL) {
  
  if (is.null(usageDataFrame)) {
    
    return(NULL);
  } else {
    write(paste0("Column name: ", colnames(usageDataFrame)), stdout());
    # titles
    xTitle <- colnames(usageDataFrame)[2];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Usage List count";
    # graph
    barplot <- ggplot(usageDataFrame,
                      aes(x = factor(DBName), y = DBBufferMB)) +
                geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
                xlab(xTitle) +
                ylab(yTitle) +
                ggtitle(mainTitle) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5));
    
    return(barplot);
  }
}
#' Title  DBUsageDataFrameToPiechart
#'
#' @param usageDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBUsageDataFrameToPiechart <- function(usageDataFrame = NULL) {
  
  if (is.null(usageDataFrame)) {
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(usageDataFrame)), stdout());
    PercentList <- round(usageDataFrame$DBBufferMB / sum(usageDataFrame$DBBufferMB) * 100, digits = 1);
    labelList <- paste0(usageDataFrame$DBName," ",PercentList, "%");
    ColorList <- heat.colors(length(PercentList));
    # titles
    xTitle <- colnames(usageDataFrame)[2];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Usage List Piechart";
    # graph
    piePlot <- ggplot(usageDataFrame, aes(x = factor(1), y = PercentList, fill = labelList)) +
      # make stacked bar chart with black border
      geom_bar(stat = "identity", color = "grey80", width = 1) +
      # add the percents to the interior of the chart
      #geom_text(aes(x = 1.5, y = PercentList, label = labelList), size = 4) +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle) +
      coord_polar(theta = "y");
    
    return(piePlot);
  }
}
#' Title  GenericPiechartFromTwoColumnDataFrame
#'
#' @param aDataFrame 
#' @param mainTitle 
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
GenericPiechartFromTwoColumnDataFrame <- function(aDataFrame = NULL, mainTitle = NULL) {
  aPierchart <- NULL;
  
  if (!is.null(aDataFrame) & (ncol(aDataFrame) == 2)) {
    write(paste0("Column name: ", colnames(aDataFrame)), stdout());
    # titles
    xTitle <- colnames(aDataFrame)[1];
    names(aDataFrame)[1] <- "X";
    yTitle <- colnames(aDataFrame)[2];
    names(aDataFrame)[2] <- "Y";
    write(paste0("Column name: ", colnames(aDataFrame)), stdout());
    mainTitle <- paste0(mainTitle, " PieChart");
    # lists
    PercentList <- round(aDataFrame$Y / sum(aDataFrame$Y) * 100, digits = 1);
    labelList <- paste0(aDataFrame$X, " ", PercentList, "%");
    write(paste0("labelList: ", aDataFrame), stdout());
    ColorList <- heat.colors(length(PercentList));
    # graph
    aPierchart <- ggplot(aDataFrame,
                         aes(x = factor(1), y = PercentList, fill = labelList)) +
                  # make stacked bar chart with black border
                  geom_bar(stat = "identity", color = "grey80", width = 1) +
                  #geom_text(aes(x = 1.5, y = PercentList, label = labelList), size = 4) +
                  ggtitle(mainTitle) +
                  xlab(xTitle) +
                  ylab(yTitle) +
                  coord_polar(theta = "y");
  }
  
  return(aPierchart);
}
#' Title  DBObjectDataFrameToBarplot
#'
#' @param objectDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples  TBD
DBObjectDataFrameToBarplot <- function(objectDataFrame = NULL) {
  
  if (is.null(objectDataFrame)) {
    write("+ ERROR : Empty data.frame", stderr());
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(objectDataFrame)), stdout());
    # titles
    xTitle <- colnames(objectDataFrame)[1];
    yTitle <- colnames(objectDataFrame)[-1];
    mainTitle <- "ObjectList count";
    # graph
    barplot <- ggplot(objectDataFrame,
                      aes(x = factor(ObjectName), y = ObjectCount)) +
      geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle);
    
    return(barplot);
  }
}
#' Title DBFunctionDataFrameToBarplot
#'
#' @param functionDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBFunctionDataFrameToBarplot <- function(functionDataFrame = NULL) {
  
  if (is.null(functionDataFrame)) {
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(functionDataFrame)), stdout());
    PercentList <- round(functionDataFrame$FunctionCount / sum(functionDataFrame$FunctionCount) * 100);
    FunctionTypeList <- paste0(functionDataFrame$FunctionTypes," ",PercentList, "%");
    # titles
    xTitle <- colnames(functionDataFrame)[1];
    yTitle <- colnames(functionDataFrame)[-1];
    mainTitle <- "Function Type List Barplot";
    # graph
    aBarplot <- ggplot(functionDataFrame,
                       aes(x = "", y = FunctionCount, fill = FunctionTypeList)) +
      labs(fill = xTitle) +
      geom_bar(width = 1, stat = "identity") +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle);
    
    return(aBarplot);
  }
}
#' Title  DBFunctionDataFrameToPiechart
#'
#' @param functionDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBFunctionDataFrameToPiechart <- function(functionDataFrame = NULL) {
  
  if (is.null(functionDataFrame)) {
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(functionDataFrame)), stdout());
    PercentList <- round(functionDataFrame$FunctionCount / sum(functionDataFrame$FunctionCount) * 100);
    FunctionTypeList <- paste0(functionDataFrame$FunctionTypes," ",PercentList, "%");
    ColorList <- heat.colors(length(PercentList));
    # titles
    xTitle <- colnames(functionDataFrame)[1];
    yTitle <- colnames(functionDataFrame)[-1];
    mainTitle <- "Function Type List Piechart";
    # graph
    piePlot <- ggplot(functionDataFrame,
                      aes(x = factor(1), y = PercentList, fill = FunctionTypeList)) +
      # make stacked bar chart with black border
      geom_bar(stat = "identity", color = "grey80", width = 1) +
      # add the percents to the interior of the chart
      #geom_text(aes(x = 1.5, y = PercentList, label = FunctionTypes), size = 4) +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle) +
      coord_polar(theta = "y");
    
    return(piePlot);
  }
}
#' Title  StoredProcWithoutWithTotalDFToBarplot
#'
#' @param woWithTotalDataFrame
#' @param OutputType
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
StoredProcWithoutWithTotalDFToBarplot <- function(woWithTotalDataFrame = NULL,
                                                  OutputType = "") {
  if (is.null(woWithTotalDataFrame)) {
    
    return(NULL);
  }else{
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- tibble::as_data_frame(as.data.frame(woWithTotalDataFrame));
    woWithTotalDataFrame;
    # check colname
    write(paste0("Column name: ", colnames(woWithTotalDataFrame)), stdout());
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[1];
    yTitle <- colnames(woWithTotalDataFrame)[-1];
    mainTitle <- paste0(OutputType, " Type List Barplot");
    # graph
    aBarplot <- ggplot(woWithTotalDataFrame,
                       aes(x = "", y = StoredProcCount, fill = StoredProcTypes)) +
      labs(fill = xTitle) +
      geom_bar(width = 1, stat = "identity") +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle);
    
    return(aBarplot);
  }
}
#' Title  DBStoreProcDataFrameToDensityplot
#'
#' @param storeProcDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBStoreProcDataFrameToDensityplot <- function(storeProcDataFrame = NULL) {
  
  if (is.null(storeProcDataFrame)) {
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(storeProcDataFrame)), stdout());
    # titles
    xTitle <- "Number of parameters";
    mainTitle <- "Densityplot for parameter distribution";
    # graph
    aDensityplot <- ggplot(storeProcDataFrame,
                           aes(NbParameters, fill = ProcedureType)) +
      geom_density(alpha = 0.6) +
      labs(title = mainTitle, x = xTitle);
    
    return(aDensityplot);
  }
}
#' Title  StoredProcWithoutWithTotalDFToPiechart
#'
#' @param woWithTotalDataFrame
#' @param OutputType
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
StoredProcWithoutWithTotalDFToPiechart <- function(woWithTotalDataFrame = NULL,
                                                   OutputType = "") {
  if (is.null(woWithTotalDataFrame)) {
    
    return(NULL);
  } else {
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- tibble::as_data_frame(as.data.frame(woWithTotalDataFrame));
    woWithTotalDataFrame[[2]] <- as.numeric(as.character(woWithTotalDataFrame[[2]]));
    #
    write(paste0("Column name: ", colnames(woWithTotalDataFrame)), stdout());
    PercentList <- round(woWithTotalDataFrame[[2]] / sum(woWithTotalDataFrame[[2]]) * 100, digits = 1);
    labelList <- paste0(woWithTotalDataFrame[[1]], " ", PercentList, "%");
    ColorList <- heat.colors(length(PercentList));
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[2];
    yTitle <- colnames(rev(woWithTotalDataFrame)[1]);
    mainTitle <- paste0(OutputType, " List Piechart");
    # graph
    piePlot <- ggplot(woWithTotalDataFrame, aes(x = factor(1), y = PercentList, fill = labelList)) +
      # make stacked bar chart with black border
      geom_bar(stat = "identity", color = "grey80", width = 1) +
      # add the percents to the interior of the chart
      # geom_text(aes(x = 1.5, y = PercentList, label = labelList), size = 4) +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle) +
      coord_polar(theta = "y");
    
    return(piePlot);
  }
}
#' Title  DBStoreProcDataFrameToBoxplot
#'
#' @param storeProcDataFrame 
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBStoreProcDataFrameToBoxplot <- function(storeProcDataFrame = NULL) {
  if (is.null(storeProcDataFrame)) {
    
    return(NULL);
  }else{
    write(paste0("Column name: ", colnames(storeProcDataFrame)), stdout());
    # titles
    xTitle <- "Procedure type";
    yTitle <- "Number of parameters";
    mainTitle <- "Boxplot for parameter distribution";
    # graph
    aBoxplot <- ggplot(storeProcDataFrame,
                       aes(x = ProcedureType, y = NbParameters)) +
      geom_boxplot(aes(fill = ProcedureType)) +
      geom_jitter() +
      # + geom_point(aes(colour = factor(type_desc)), size=4)
      scale_y_continuous(breaks = seq(0, 12, 1.0)) +
      labs(title = mainTitle, x = xTitle, y = yTitle);
    
    return(aBoxplot);
  }
}
#' Title  DBRowCountFrameToBarplot
#'
#' @param usageDataFrame 
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBRowCountFrameToBarplot <- function(usageDataFrame = NULL) {
  
  if (is.null(usageDataFrame)) {
    
    return(NULL);
  }else{
    write(colnames(usageDataFrame), stdout());
    # titles
    xTitle <- colnames(usageDataFrame)[1];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Row List count";
    # graph
    barplot <- ggplot(usageDataFrame, aes(x = factor(TableRows), y = RowRepeats)) +
      ##barplot <- ggplot(usageDataFrame, aes(x = factor(TableRows), y = sqrt(RowRepeats))) +
      geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
      ##scale_y_sqrt(paste0("Square root of ", yTitle)) +
      ggtitle(mainTitle) +
      xlab(xTitle) +
      ylab(yTitle) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5));
    
    return(barplot);
  }
}
#' Title  FunctionWithoutWithTotalDFToPiechart
#'
#' @param woWithTotalDataFrame 
#' @param OutputType 
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
FunctionWithoutWithTotalDFToPiechart <- function(woWithTotalDataFrame = NULL,
                                                OutputType = "") {
  
  if (is.null(woWithTotalDataFrame)) {
    
    return(NULL);
  } else {
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- tibble::as_data_frame(woWithTotalDataFrame);
    woWithTotalDataFrame[[2]] <- as.numeric(woWithTotalDataFrame[[2]]);
    # check colname
    write(colnames(woWithTotalDataFrame), stdout());
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[1];
    yTitle <- colnames(woWithTotalDataFrame)[2];
    mainTitle <- paste0(OutputType, " Type List Piechart");
    PercentList <- round(woWithTotalDataFrame[[2]] / sum(woWithTotalDataFrame[[2]]) * 100, digits = 1);
    labelList <- paste0(woWithTotalDataFrame[[1]], " ", PercentList, "%");
    ColorList <- heat.colors(length(PercentList));
    # graph
    piePlot <- ggplot(woWithTotalDataFrame,
                      aes(x = factor(1), y = PercentList, fill = labelList)) +
                # make stacked bar chart with black border
                geom_bar(stat = "identity", color = "grey80", width = 1) +
                # add the percents to the interior of the chart
                # geom_text(aes(x = 1.5, y = PercentList, label = labelList), size = 4) +
                ggtitle(mainTitle) +
                xlab(xTitle) +
                ylab(yTitle) +
                coord_polar(theta = "y");
    
    return(piePlot);
  }
}
#' Title  FunctionWithoutWithTotalDFToBarplot
#'
#' @param woWithTotalDataFrame
#' @param OutputType
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
FunctionWithoutWithTotalDFToBarplot <- function(woWithTotalDataFrame = NULL,
                                                OutputType = "") {
  
  if (is.null(woWithTotalDataFrame)) {
    
    return(NULL);
  }else{
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- as.data.frame(woWithTotalDataFrame);
    # check colname
    write(colnames(woWithTotalDataFrame), stdout());
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[1];
    yTitle <- colnames(woWithTotalDataFrame)[-1];
    mainTitle <- paste0(OutputType, " Type List Barplot");
    # graph
    aBarplot <- ggplot(woWithTotalDataFrame,
                       aes(x = "", y = FnCount, fill = FnTypes)) +
                labs(fill = xTitle) +
                geom_bar(width = 1, stat = "identity") +
                ggtitle(mainTitle) +
                xlab(xTitle) +
                ylab(yTitle);
    
    return(aBarplot);
  }
}
#' Title  DBFunctionDataFrameToBoxplot
#'
#' @param functionDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBFunctionDataFrameToBoxplot <- function(functionDataFrame = NULL) {
  
  if (is.null(functionDataFrame)) {
    
    return(NULL);
  }else{
    write(colnames(functionDataFrame), stdout());
    # titles
    xTitle <- "Function type";
    yTitle <- "Number of parameters";
    mainTitle <- "Boxplot for parameter distribution";
    # graph
    aBoxplot <- ggplot(functionDataFrame,
                       aes(x = FunctionType, y = NbParameters)) +
      geom_boxplot(aes(fill = FunctionType)) +
      geom_jitter() +
      # + geom_point(aes(colour = factor(type_desc)), size=4)
      scale_y_continuous(breaks = seq(0, 12, 1.0)) +
      labs(title = mainTitle, x = xTitle, y = yTitle);
    
    return(aBoxplot);
  }
}
#' Title  DBFunctionDataFrameToDensityplot
#'
#' @param functionDataFrame
#'
#' @return ggplot2
#' @export TBD
#'
#' @examples TBD
DBFunctionDataFrameToDensityplot <- function(functionDataFrame = NULL) {
  
  if (is.null(functionDataFrame)) {
    
    return(NULL);
  }else{
    write(colnames(functionDataFrame), stdout());
    # titles
    xTitle <- "Number of parameters";
    mainTitle <- "Densityplot for parameter distribution";
    # graph
    aDensityplot <- ggplot(functionDataFrame,
                           aes(NbParameters, fill = FunctionType)) +
      geom_density(alpha = 0.6) +
      labs(title = mainTitle, x = xTitle);
    
    return(aDensityplot);
  }
}

