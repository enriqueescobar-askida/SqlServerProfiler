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
TrimXmlXlsToTibble <- function(aTibble = NULL) {

  if (IsEvenInteger(nrow(aTibble))) {
   
     return(aTibble);
  } else {
  nbLines <- nrow(aTibble);

    if (IsConvertibleToDate(aTibble[[nbLines,]])) {
      print("mockway");
      #return(aTibble[-1,]);
    } else {
      print("gremlin");
      #return(aTibble);
    }
  #     return(aTibble[-1,]);
  #   }
  }
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
    
    return(tibble::as_data_frame(singleDataFrame));
  }
}
TraceXmlXlsToCharacter <- function(aTibble = NULL) {
  
  if (is.null(aTibble)) {
    
    return(NULL);
  } else {
    nbLines <- nrow(aTibble);
    aDF <- data.frame(NULL);
    lastDF <- data.frame(NULL);
    
    canConvert <- FALSE;
    lastConvert <- FALSE;
    lastSql <- "";
    lastDate <- Sys.Date();
    
    for (rowl in seq(nbLines)) {
      canConvert <- IsConvertibleToDate(aTibble[[rowl,]]);
      if (!canConvert) {
        lastDF$TimeStamp <- NULL;
        lastDF$Sql <- aTibble[[rowl,]];
        aDF <- rbind(lastDF);
      }
    }
  }
}
