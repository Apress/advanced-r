library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(Hmisc)
library(foreign)
library(xlsx)
options(width = 70) # only 70 characters per line


##Help Functions
?'+'

help("+")

#System
Sys.Date()
Sys.time()
Sys.timezone()


getwd()
file.exists("ch02_text.txt")
file.exists("NOSUCHFILE.FAKE")
file.exists("C:/Users/Matt/Google Drive/Books/Apress_AdvancedR/Apress_AdvancedR_Proposal.docx")

file.access("ch02_text.txt", mode = 0)
file.access("ch02_text.txt", mode = 1)
file.access("ch02_text.txt", mode = 2)
file.access("ch02_text.txt", mode = 4)

file.info("ch02_text.txt", "chapter01.R")

newTime<-Sys.time()-20
newTime

file.info("ch02_text.txt")
Sys.setFileTime("ch02_text.txt",newTime)
file.info("ch02_text.txt")


file.create("ch02_created.docx", showWarnings = TRUE)
file.remove("ch02_created.docx")
file.remove("NOSUCHFILE.FAKE")


Sys.time()
file.copy("ch02_text.txt", "ch02_copy.txt", overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
file.info("ch02_copy.txt")
file.rename("ch02_copy.txt", "ch02.txt")


file.append("ch02_text.txt", "ch02.txt")
file.append("ch02_pp.pptx", "ch02_pp2.pptx")

dir.create("folder1")
#####################################################

##IO
countiesILCSV<-read.table("Ch02/Counties_in_Illinois.csv", header = TRUE, sep = ",")
View(countiesILCSV)

##stata
rscfpData <- read.dta("Ch02/rscfp2013.dta") 
View(rscfpData)

##SPSS
countiesILSPSS <- spss.get("Ch02/Counties_in_Illinois.sav")
head(countiesILSPSS)

##Excel
countiesILExcel <- read.xlsx("Ch02/Counties_in_Illinois.xlsx", sheetName = "Counties_in_Illinois") 

##Excel Output
write.xlsx(countiesILExcel, "Ch02/Output1.xlsx") 

##SPSS Output
write.foreign(countiesILExcel, "Ch02/Output2.txt", "Ch02/Output2.sps",   package="SPSS") 

#Stata Output
write.dta(countiesILExcel, "Ch02/Output3.dta") 

##capture Console Output to File
sink("Ch02/Output4.txt", append = TRUE, split = TRUE)
x <- 10
xSquared <- x^2
x
xSquared
unlink("Ch02/Output4.txt")


#Debugging

debug()
traceback()
browser()

