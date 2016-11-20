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
      count<-count+1;
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
    aTibble <- aTibble[!grepl("^2016", aTibble$SqlTrace),];
    write.csv(aTibble, file = "filePath");
    
    return(TRUE);
  }
}
