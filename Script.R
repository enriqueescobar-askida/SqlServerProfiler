#setwd("C:\\Users\\Admin\\Desktop"); #USERPROFILE
library(xlsx);
xlsFile <- xlsx::read.xlsx2("Data/00 Scripts start.xls",
                           sheetIndex = 1,
                           as.data.frame = TRUE,
                           header = FALSE);
xlsFile <- tibble::as_data_frame(xlsFile);
xlsFile <- xlsFile[-2];
library(XLConnect);
workBoo <- loadWorkbook("Data/00 Scripts start.xls");
xlsFile <- readWorksheet(workBoo, sheet = "Feuil2", header = FALSE);
xlsFile <- tibble::as_data_frame(xlsFile);
xlsFile <- xlsFile[-2];
library(readxl);
xlsFile <- readxl::read_excel("Data/00 Scripts start.xls", col_names = FALSE);
xlsFile <- xlsFile[-2];
