#' Title  ColumnDataFrameToFrequencyTable
#'
#' @param aDataFrame 
#' @param columnName 
#'
#' @return table
#' @export TBD
#'
#' @examples TBD
ColumnDataFrameToFrequencyTable <- function(aDataFrame = NULL, columnName = NULL) {
  tableFrequency <- NULL;
  
  if (!is.null(aDataFrame) & !is.null(columnName)) {
    tableFrequency <- table(aDataFrame[columnName]);
  }
  
  return(tableFrequency);
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
# import tibble
source("Lib/tibble.Util.R");
### import wordcloud
source("Lib/wordcloud.Util.R");
### import xlsx
source("Lib/xlsx.Util.R");
### ggplot2
source("Lib/ggplot2.Util.R");
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
#' Title  ScreenTxtFiles
#'
#' @param fileList
#' @export TBD
#'
#' @examples TBD
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
