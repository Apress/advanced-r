library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(ggplot2)
options(width = 70) # only 70 characters per line

########################################################################
##                                                                    ##
##            Loops, Flow Control, and *apply functions               ##
##                                                                    ##
########################################################################


##a demonstration of the for loop and why it may not be always ideal.

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

rm(xCube)
forTime2<- proc.time()
xCube <- vector(mode = "numeric", length = length(x))
for (i in 1:length(x) ) {
  xCube[i] = x[i]^3
}
forTime2 <- proc.time()-forTime2
head(xCube)
forTime2


xCube <- NULL
vTime <- proc.time()
xCube <- x^3
vTime <- proc.time()-vTime
head(xCube, -99994)
vTime



#rnorm

for (i in 1:5 ) {
  x <- rnorm(5,4,2)
  print(x)
  print(paste0("Xbar: ",mean(x)))
  print(paste0("StdDev: ",sd(x)))
}

##while
y <- 3
while(mean(y)< 6){y <- rnorm(5, mean = 4.0, sd = 2)}
mean(y)
y


##If & If/Else

if(y>6){
  boxplot(y)
}

else{
  histogram(y)
}

if(mean(y)>6){
  boxplot(y)
}

else{
  histogram(y)
}

if(mean(y)>6){
  boxplot(y)
}else{
  histogram(y)
}

##break and next
i <- 0
while(i < 5){
  i <- i + 1
  if(i ==2) {next()}
  print(i)
}


z <- NULL
i <-1
repeat{
  z <- rnorm(1,4,2)
  i <- i + 1
  if(z > 10){break}
}
z
i


###############*apply functions

xL <- 1:5

## returns a list
lapply(xL, is.integer)
## simplifies list to a vector
sapply(xL, is.integer)

## returns a vector, but had to specify the type of value (i.e., single value and logical)
vapply(xL, is.integer, FUN.VALUE = NA)
## what happens when the function does not return the correct type of value
## expected logical because by default NA is a logical so R expected the same type
## but it got a double
vapply(xL, mean, FUN.VALUE = NA)

## here is an example where it expects each result to be a vector of length 2
## but it got a vector of length 1, correct type, wrong length
vapply(xL, is.integer, FUN.VALUE = c(NA, NA))

## make more sense with real example
yL <- list(
  a = 1:5,
  b = c("6", "7", "8"),
  c = c(9:10))
yL
yL$b[2]
is.character(yL$b[2])
is.integer(yL$b[2])
## works, may be what we want, may not be
lapply(yL, summary)
## by directly specifying the expected output, get an error when it fails
## could trigger us to examine what is wrong with yL
vapply(yL, summary, c(Min. = 0, `1st Qu.` = 0, Median = 0, Mean = 0, `3rd Qu.` = 0, Max. = 0))


## tapply is useful for ragged arrays
## and when there is an index variable
head(mtcars[, c("mpg", "cyl")])

barplot(table(mtcars$cyl))

#for further viewing
tapply(mtcars$mpg, mtcars$cyl, FUN = function(x) x)

## simplifies by default
tapply(
  X = mtcars$mpg,
  INDEX = mtcars$cyl,
  FUN = mean)


## without simplifying returns a list like lapply()
tapply(
  X = mtcars$mpg,
  INDEX = mtcars$cyl,
  FUN = mean,
  simplify = FALSE)




## for a "generic" use, or using lapply()/sapply() like a for loop
## use an anonymous function
## because sapply() simplifies, the result is a 5 x 32 matrix
## because there are 32 elements in mtcars$mpg

##not doing this yet because this is before we introduce functions.  Leaving it in the source code.
sapply(1:length(mtcars$mpg), function(i) {
  rnorm(5, mtcars$mpg[i], 2)
})


## apply() is good for data frames, matrices, and arrays

## get the SD by column
apply(mtcars, MARGIN = 2, FUN = sd)

## get the SD by row
apply(mtcars, MARGIN = 1, FUN = sd)


##this is also clever, but we have not yet introduced functions!
## can use "fancier" functions.  For example
## to set "extreme" values to missing
## just one value set to missing
## but apply() looped through every column in mtcars applying our function
apply(mtcars, MARGIN = 2, FUN = function(x) {
  limit <- mean(x) + sd(x) * 3
  ifelse(abs(x) > limit, NA, x)
})


## mapply() is a multivariate version
## a bit trickier to follow and ordered differently
## as the function comes FIRST rather than in the middle or toward the end

par(mfrow = c(2, 1))
mapply(plot,
       mtcars[, c("mpg", "hp")],
       mtcars[, c("disp", "qsec")])

## is equivalent to
plot(mtcars[, "mpg"], mtcars[, "disp"])
plot(mtcars[, "hp"], mtcars[, "qsec"])


## rapply() is a recursive version of lapply() with slightly different arguments

## here is some imaginary data
## notice the structure, which includes lists within lists
zL <- list(
  a = list(1, 2, 3:5),
  b = list(8:12))
zL

## what if we try lapply() on it?
## not good because lapply() passes each element to mean() and mean() cannot handle lists!
lapply(zL, FUN = mean)

## we could make lapply() work with an inner loop
## but this is lots of work, and what if the nesting was even deeper?
lapply(zL, FUN = function(x) lapply(x, FUN = mean))

rapply(zL, f = mean)
rapply(zL, f = mean, how = "list")


## last eapply() is for environments
## environments, users don't normally worry about environments
## all the objects we have created have been in what is known as the Global Environment
environment()

## we can see all the objects names using ls()
ls()

## what if we wanted to see the class of all objects in our environment?
eapply(env = environment(), FUN = class)

####################
