


CollegeData <- read.delim("chapter22_shiny/Ch22_Data.txt", header = TRUE, sep = "\t")

y<-unlist(CollegeData[1+1,2:6])
barplot(y)

y2 <- as.numeric("2")+1
y2

pbi<-read.csv("chapter22_shiny/pbi.csv", sep= ",")

biserial.cor(pbi[[5]], pbi[[4]], level = 2)

