



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

