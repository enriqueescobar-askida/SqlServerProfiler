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
    
    if (length(fileList) < 2) {
      oneFile <- unlist(fileList[[1]]);
      
      return(TraceXmlXlsToTibble(oneFile));
    } else {
      singleDataFrame <- NULL;
      
      for (singleFile in fileList) {
        print(unlist(singleFile));
        aDf <- TraceXmlXlsToTibble(singleFile);
        singleDataFrame <- rbind(singleDataFrame, aDf);
      }
      
      return(tibble::as_data_frame(singleDataFrame));
    }
  }
}
