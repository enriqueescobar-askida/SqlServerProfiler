#setwd("C:\\Users\\Admin\\Desktop"); #USERPROFILE
#setwd('D:/Disk_X/axon/SqlServerProfiler');
setwd("E:/Disk_X/SqlServerProfiler/");
source('Lib/SqlServerProfiler.Util.R');
fileList0 <- list("Data/00 Scripts start.new.xls");
fileList1 <- list("Data/01 Agent Niveau 1 - Evelyne made mistake and logged out.new.xls");
fileList2 <- list("Data/02 Agent N1 Complete Agent Web Services engage.new.xls");
fileList3 <- list("Data/03 Agent N1 complete.new.1of5.xls",
                  "Data/03 Agent N1 complete.new.2of5.xls",
                  "Data/03 Agent N1 complete.new.3of5.xls",
                  "Data/03 Agent N1 complete.new.4of5.xls",
                  "Data/03 Agent N1 complete.new.5of5.xls");
fileList4 <- list("Data/04 Agent N2 complete.new.1of3.xls",
                  "Data/04 Agent N2 complete.new.2of3.xls",
                  "Data/04 Agent N2 complete.new.3of3.xls");
fileList5 <- list("Data/05 Sup SAC.new.xls");

xlsxTibble0 <- ScreenXmlXlsFiles(fileList0);
TraceXmlXlsToCsv(xlsxTibble0, filePath = "Data/00 Scripts start.new.csv");
rm(xlsxTibble0);
xlsxTibble1 <- ScreenXmlXlsFiles(fileList1);
TraceXmlXlsToCsv(xlsxTibble1, filePath = "Data/01 Agent Niveau 1 - Evelyne made mistake and logged out.new.csv");
rm(xlsxTibble1);
xlsxTibble2 <- ScreenXmlXlsFiles(fileList2);
TraceXmlXlsToCsv(xlsxTibble2, filePath = "Data/02 Agent N1 Complete Agent Web Services engage.new.csv");
rm(xlsxTibble2);
xlsxTibble3 <- ScreenXmlXlsFiles(fileList3);
TraceXmlXlsToCsv(xlsxTibble3, filePath = "Data/03 Agent N1 complete.new.csv");
rm(xlsxTibble3);
xlsxTibble4 <- ScreenXmlXlsFiles(fileList4);
TraceXmlXlsToCsv(xlsxTibble4, filePath = "Data/04 Agent N2 complete.new.csv");
rm(xlsxTibble4);
xlsxTibble5 <- ScreenXmlXlsFiles(fileList5);
TraceXmlXlsToCsv(xlsxTibble5, filePath = "Data/05 Sup SAC.new.csv");
rm(xlsxTibble5);
