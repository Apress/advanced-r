
sink("Output21.txt", append = TRUE, split = TRUE)
x <- seq(1,100000,1)
head(x)
forTime<- proc.time()
xCube <- NULL
for (i in 1:length(x) ) {
  xCube[i] = x[i]^3
}
forTime <- proc.time()-forTime
head(xCube)
forTime
unlink("Output21.txt")