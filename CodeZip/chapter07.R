## load checkpoint and required packages
library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(devEMF)
library(data.table)
options(width = 70) # only 70 characters per line


################################################################################
##                                                                            ##
##                                data.table intro                            ##
##                                                                            ##
################################################################################

## options to reduce number of rows printed
## for the book
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R

iris$Species <- as.character(iris$Species)
diris <- as.data.table(iris)

## prints just a few starting and ending rows
diris

tables()

sapply(diris, class)

## checking data for extra rows / duplicates
# nrow(diris)
# ncol(diris)
# dim(diris)

################################################################################
##                                                                            ##
##                                Keys and Sorting                            ##
##                                                                            ##
################################################################################

## keys in data.table take time to create
## but are very fast to use after that
## they also result in the data.table being sorted by key

## add a key (data.table object, and name of variable(s) to use to key)
haskey(diris)
setkey(diris, Species)
key(diris)
haskey(diris)


## alternately, a data.table can be sorted using the order() function
diris <- diris[order(Sepal.Length)]
diris
## ordering removes the key
haskey(diris)


## order can use multiple variables
## and use - to sort in decreasing order
diris <- diris[order(Sepal.Length, -Sepal.Width)]
diris


setkey(diris, Species)
diris[49:52]

anyDuplicated(diris)
## check for duplicates by Sepal.Length
anyDuplicated(diris$Sepal.Length)

## when no variable specified, based on the key
table(duplicated(diris)) ## 3 species, our key variable
## also get only unique keys
unique(diris)

################################################################################
##                                                                            ##
##                             Selecting & Subsetting                         ##
##                                                                            ##
################################################################################

#### Selecting Rows ####

## select rows 1 to 5
## does not have to be sequential rows
diris[1:5]

## select all but rows 1 to 5
diris[-(1:148)]


## once you have added a key, you can select
## rows by key name
diris["setosa"]

## select all rows where keys do not match
diris[!"setosa"]

## can select rows using logical indexing
diris[Sepal.Length < 5]

## and can use mutliple logical arguments combining using & or |
diris[Sepal.Length < 5 & Petal.Width < .2]

diris[Sepal.Length == 4.3 | Sepal.Length == 4.4]

## %in% operator is shorter way for multiple matches
diris[Sepal.Length %in% c(4.3, 4.4)]

interest <- c(4.3, 4.4)
diris[Sepal.Length %in% interest]

## not in
## useful for excluding outliers or other data points
diris[!Sepal.Length %in% interest]


#### Selecting Columns ####

## so far we've grabbed all columns

## can select multiple columns
## .() in data.table is a shorthand for list()
diris[, .(Sepal.Length, Sepal.Width)]

## select one column (still as a data.table)
diris[, .(Sepal.Length)]


x <- "example"
x
"x"


diris[, 1, with = FALSE]

diris[, "Sepal.Length", with = FALSE]

## useful if you have a variable with variable names or positions
v <- "Sepal.Length"
diris[, v, with = FALSE]

diris[, v]
diris[, 1]

## extract a column as a vector (each of the below only works for a single column)
head(diris[["Sepal.Length"]]) # easy if you have a R variable
head(diris[[v]]) # easy if you have a R variable
head(diris[, Sepal.Length]) # easy to type
head(diris$Sepal.Length) # easy to type

## extract all except column(s)
diris[1, -"Sepal.Length", with=FALSE]
diris[1, !"Sepal.Length", with=FALSE]
diris[1, -c("Sepal.Length", "Petal.Width"), with=FALSE]
diris[1, -v, with=FALSE]


################################################################################
##                                                                            ##
##                       Variable Renaming and Ordering                       ##
##                                                                            ##
################################################################################

## the variable names can be accessed as usual
names(diris)
colnames(diris)

## to avoid copying (and thus being memory inefficient)
## data.table changes variable names by reference using setnames()

## data, old variable name(s), new variable name(s)
setnames(diris, old = "Sepal.Length", new = "SepalLength")
names(diris)

## can select old variables by position/number rather than name
setnames(diris, old = 1, new = "SepalL")
names(diris)

## you can reorder the variables in a dataset using setcolorder()
setcolorder(diris, c("SepalL", "Petal.Length", "Sepal.Width", "Petal.Width", "Species"))
diris

## can re-order by column number as well
setcolorder(diris, c(5, 1, 3, 2, 4))
diris

## for either names or number positions can reorder by an R object
v <- c(1, 3, 2, 4, 5)
setcolorder(diris, v)
diris

################################################################################
##                                                                            ##
##                  Computing on Data and Creating Variables                  ##
##                                                                            ##
################################################################################

## re-create data.table based on original iris data
diris <- as.data.table(iris)
setkey(diris, Species)

## create a new variable using the := operator
diris[, V0 := 0]
diris

## delete a variable by setting it to NULL
## diris2 <- diris[, -SepalL] # make a copy of data less column (slower)
diris[, Sepal.Length := NULL]
diris

## create multiple variables using a named vector and a list
diris[, c("X1", "X2") := .(1L, 2L)]
diris

## one of the main benefits of data.table is that it is easy
## to compute using variables from the dataset
diris[, V := Petal.Length * Petal.Width]
diris

## remove V
diris[, c("V", "V0") := NULL]
## assign V for only rows where the key (Species) is "setosa"
diris["setosa", V := Petal.Length * Petal.Width]
unique(diris)

## in addition to creating a variable
## you can also change a variable
diris["virginica", V := sqrt(Petal.Length * Petal.Width)]


## you can also compute without creating a variable
diris[, mean(Sepal.Width)]
## or return it as a named data.table object
diris[, .(M = mean(Sepal.Width))]

## computing in a list allows multiple operations
diris[, .(M = mean(Sepal.Width),
          SD = sd(Sepal.Width))]

## computing can be done on subsets easily
diris["virginica", .(M = mean(Sepal.Width))]

## computing can also be done by another variable
diris[, .(M = mean(Sepal.Width)), by = Species]

## can make multiple variables during computing
diris[, .(M1 = mean(Sepal.Width),
          M2 = mean(Petal.Width)), by = Species]

## by(diris, diris$Species, FUN = function(d)
##   data.frame(M1 = mean(d$Sepal.Width),
##              M2 = mean(d$Petal.Width)))


## another useful thing is that operations can be chained together
diris[, .(M1 = mean(Sepal.Width),
          M2 = mean(Petal.Width)), by = Species][,
  .(r = cor(M1, M2))]


## variables can be created by a variable as well
## create new variable that is whether a petal width
## is greater or smaller than the median petal width
## *for that species*
diris[, MedPW := Petal.Width > median(Petal.Width), by = Species]
diris

## you can perform calculations by multiple variables
## by passing a list of variables to the by argument
diris[, .(M1 = mean(Sepal.Width),
          M2 = mean(Petal.Width)),
      by = .(Species, MedPW)]


diris[, .(M1 = mean(Sepal.Width),
          M2 = mean(Petal.Width)),
      by = .(Species, MedPW)][,
  .(r = cor(M1, M2)), by = MedPW]


################################################################################
##                                                                            ##
##                           Merging and Reshaping Data                       ##
##                                                                            ##
################################################################################

#### different kinds of merges

## can distinguish merges based on:
### (1) whether it is: (a) one to one,
###     (b) one to many / many to one, or
###     (c) many to many and
### (2) what to do for cases/rows that do not match:
###     (a) keep only cases/rows that match both datasets,
###     (b) keep cases/rows that match the "x" dataset,
###     (c) keep cases/rows that match the "y" dataset
###     (d) keep all cases/rows from either dataset
## for data.table, merging uses the key variable by default


## first make two variable data.table
## with 3 observations per species
## also keys the data using keyby instead of just by
diris <- diris[, .(Sepal.Width = Sepal.Width[1:3]), keyby = Species]

## make dataset with only 1 observation per sepcies
diris2 <- unique(diris)

## make another data table
## that contains information about each species
## with repeated species
dalt1 <- data.table(
  Species = c("setosa", "setosa", "versicolor", "versicolor",
              "virginica", "virginica", "other", "other"),
  Type = c("wide", "wide", "wide", "wide",
           "narrow", "narrow", "moderate", "moderate"),
  MedPW = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
setkey(dalt1, Species)


## make a data table with only unique species (no repeats)
dalt2 <- unique(dalt1)[1:2]


diris
diris2
dalt1
dalt2

## one to one merge, only keeping cases/rows that match both
merge(diris2, dalt2)

## one to one merge, keeping all cases/rows from "x"
merge(diris2, dalt2, all.x = TRUE)

## one to one merge, keeping all cases/rows from "y"
merge(diris2, dalt2, all.y = TRUE)

## one to one merge, keeping all cases/rows from both
merge(diris2, dalt2, all = TRUE)


## many to one / one to many merge, only keeping cases/rows that match both
merge(diris, dalt2)

## many to one / one to many merge, keeping all cases/rows from "y"
merge(diris, dalt2, all.y = TRUE)

## many to one / one to many merge, keeping all cases/rows from both datasets
## probably not desirable in this case, as "other" only gets one row where most have 3
merge(diris, dalt2, all = TRUE)


### many to many
## since there are multiple matches, it makes all possible combinations
## only keeping cases/rows that match in both datasets
## by default gives an error, but if you know this is what you want, use
## allow.cartesian = TRUE
merge(diris, dalt1, allow.cartesian = TRUE)

## since there are multiple matches, it makes all possible combinations
## keeping all cases/rows from both datasets
merge(diris, dalt1, all = TRUE, allow.cartesian = TRUE)



## can also have keys based on multiple columns
d2key1 <- data.table(
  ID1 = c(1, 1, 2, 2),
  ID2 = c(1, 2, 1, 2),
  X = letters[1:4])

d2key2 <- data.table(
  ID1 = c(1, 1, 2, 2),
  ID2 = c(1, 2, 1, 2),
  Y = LETTERS[1:4])

d2key1
d2key2

setkey(d2key1, ID1)
setkey(d2key2, ID1)

## made all combinations of each ID1
## also appends .x and .y to indicate ID2 comes from the x or y dataset,
## because the same variable exists in both datasets
merge(d2key1, d2key2)


## but this is not really a many to many merge
## really one to one merge, but with a multi column key
setkey(d2key1, ID1, ID2)
setkey(d2key2, ID1, ID2)

## now get a simple one-to-one merge because of the proper keying
## in most cases, apparently "many to many" merges can be made one to one
## through encofrcement of a unique key
##(e.g., if you have timepoints within subjects
## than you may make a key based on both subject ID and time,
##so merging matches the appropriate
## time point within the appropriate subject
merge(d2key1, d2key2)


##### Reshaping Long #####

## reshaping generally works better if there is a unique ID per row

diris <- as.data.table(iris)
## in data.table .N is a special way to refer to the number of rows
## in the data.table, so 1:.N creates integers from 1 to number of rows
diris[, ID := 1:.N]
setkey(diris, ID)


## Wickham, H. (2007). Reshaping data with the reshape package. Journal of Statistical Software, 21(12), 1-20.

## to reshape long we will "melt" the data.table
## measure.vars are repeatedly measured variables
## or otherwise variables you wish to stack in the long dataset
## the variable.name is the name of the variable that will indicate
## which stack a row belongs to
## value.name can specify the names for the newly stacked variables
## and id.vars are variables that should not be reshaped or stacked
## and instead are simply repeated
diris.long <- melt(diris,
  measure.vars = list(
    c("Sepal.Length", "Sepal.Width"),
    c("Petal.Length", "Petal.Width")),
  variable.name = "Type",
  value.name = c("Sepal", "Petal"),
  id.vars = c("ID", "Species"))
diris.long

diris.long[, Type := factor(Type, levels = 1:2,
                            labels = c("Length", "Width"))]
diris.long

## what happens by default if you use melt
## and only specify those variables that should not be melted
diris.long2 <- melt(diris, id.vars = c("ID", "Species"))
diris.long2

##### Reshaping Wide #####

## the dangers of not having a unique ID
## multiple values must get aggregated to fit in one cell
dcast(diris.long2, Species ~ variable)

diris.wide2 <- dcast(diris.long2, ID + Species ~ variable)
diris.wide2

## value.var is for when there are multiple
## long variables to be reshaped to wide
## sep is what is used to combine the names from
## value.var with the names in the variable indicating
## repeated measures, Type
diris.wide <- dcast(diris.long, ID + Species ~ Type,
      value.var = list("Sepal", "Petal"),
      sep = ".")


## check that datasets are equal
all.equal(diris.wide2, diris.wide)
