# import
library(tibble);
###
### functions
###
#' Title  FrequencyTableToTibble
#'
#' @param aTable 
#' @param aTitle 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
FrequencyTableToTibble <- function(aTable = NULL, aTitle = NULL) {
  aDataFrame <- NULL
  
  if (!is.null(aTable) & is.table(aTable)) {
    aDataFrame <- tibble::as_data_frame(aTable);
  }
  if (!is.null(aTitle)) {
    names(aDataFrame)[1] <- paste0(aTitle, " List");
    names(aDataFrame)[2] <- paste0(aTitle, " Count");
  } else {
    colnames(aDataFrame) <- paste0(colnames(aDataFrame), " List");
  }
  print(aDataFrame);
  
  return(aDataFrame)
}
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

