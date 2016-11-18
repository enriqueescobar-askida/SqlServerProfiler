#setwd("C:\\Users\\Admin\\Desktop"); #USERPROFILE
setwd('D:/Disk_X/axon/SqlServerProfiler');
source('Lib/SqlServerProfiler.Util.R');
fileList0 <- list("Data/00 Scripts start.xls");
fileList1 <- list("Data/01 Agent Niveau 1 - Evelyne made mistake and logged out.xls");
fileList2 <- list("Data/02 Agent N1 Complete Agent Web Services engage.xls");
fileList3 <- list("Data/03 Agent N1 complete.new.1of5.xls",
                  "Data/03 Agent N1 complete.new.2of5.xls",
                  "Data/03 Agent N1 complete.new.3of5.xls",
                  "Data/03 Agent N1 complete.new.4of5.xls",
                  "Data/03 Agent N1 complete.new.5of5.xls");
fileList4 <- list("Data/04 Agent N2 complete.new.1of3.xls",
                  "Data/04 Agent N2 complete.new.2of3.xls",
                  "Data/04 Agent N2 complete.new.3of3.xls");
fileList5 <- list("Data/05 Sup SAC.xls");

xlsxTibble0 <- ScreenXmlXlsFiles(fileList0);
# rm(xlsxTibble0);
# xlsxTibble1 <- ScreenXmlXlsFiles(fileList1);
# rm(xlsxTibble1);
# xlsxTibble2 <- ScreenXmlXlsFiles(fileList2);
# rm(xlsxTibble2);
# xlsxTibble3 <- ScreenXmlXlsFiles(fileList3);
# rm(xlsxTibble3);
# xlsxTibble4 <- ScreenXmlXlsFiles(fileList4);
# rm(xlsxTibble4);
# xlsxTibble5 <- ScreenXmlXlsFiles(fileList5);
# rm(xlsxTibble5);
