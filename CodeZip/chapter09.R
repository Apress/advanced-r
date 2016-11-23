library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(dplyr)
library(tibble)
library(stringdist)
#library(data.table)


##########################################################################
##                                                                      ##
##            Other Tools for Data Managment                            ##
##                                                                      ##
#########################################################################



################################################################################
##                                                                            ##
##                                   dplyr intro                              ##
##                                                                            ##
################################################################################

## options to reduce number of rows printed
## for the book
## called tibble because dplyr has separated out its data storage format, called tibbles,
## into a separate tibble package, that is installed as a dependency of dplyr
options(stringsAsFactors = FALSE,
        tibble.print_max = 20, ## if over 20 rows
        tibble.print_min = 5, ## print first 5
        digits = 2) ## reduce digits printed by R

iris$Species <- as.character(iris$Species)
diris <- as_tibble(iris)

## prints first five rows
diris

## we can use sapply() to get a vector of the classes
## but note that for what was printed, it already indicated some data
## were "dbl" for doubles or what R calls numeric, and Species is "chr"
## for character
sapply(diris, class)

## checking data for extra rows / duplicates
# nrow(diris)
# ncol(diris)
# dim(diris)

## dplyr mindset is organized around focused verbs that accomplish some action.
## These are functions and typically only accomplish a narrow task.
## Unlike data.table, dplyr is "functional" in that everything returns a copy of the data
## It does not modify the data in place in memory as data.table does.
## However, it is still pretty fast, because many of the functions are optimized written in C++
## and because it does not make unnecessary copies.


################################################################################
##                                                                            ##
##                                     Sorting                                ##
##                                                                            ##
################################################################################

## To sort data, we use the dplyr arrange() function
diris <- arrange(diris, Sepal.Length)
diris

## you can arrange by multiple variables
## and use the function desc() to arrange in decreasing order

diris <- arrange(diris, Sepal.Length, desc(Sepal.Width))
diris


anyDuplicated(diris)
## check for duplicates by Sepal.Length
anyDuplicated(diris$Sepal.Length)


## when no variable specified, based on all variables
table(duplicated(diris)) ##
table(duplicated(diris$Sepal.Length))

## get distinct values for the overall data
distinct(diris)
## or jsut for specific variables
distinct(diris, Sepal.Length)
distinct(diris, Sepal.Length, Sepal.Width)

################################################################################
##                                                                            ##
##                             Selecting & Subsetting                         ##
##                                                                            ##
################################################################################

#### Selecting Rows ####

## to select rows by position, we use the function slice()
## select rows 1 to 5
## does not have to be sequential rows
slice(diris, 1:5)

## select all but rows 1 to 5
slice(diris, -(1:145))

## to select rows by a variable use logical indexing
## combined with the function filter()
filter(diris, Species == "setosa")

## select all rows where keys do not match
filter(diris, Species != "setosa")

## can select rows using logical indexing on numeric values
filter(diris, Sepal.Length < 5)

## and can use mutliple logical arguments combining using & or |
filter(iris, Sepal.Length < 5 & Petal.Width > .2)

filter(diris, Sepal.Length == 4.3 | Sepal.Width != 4.4)

## %in% operator is shorter way for multiple matches
filter(diris, Sepal.Length %in% c(4.3, 4.4))

interest <- c(4.3, 4.4)
filter(diris, Sepal.Length %in% interest)

## not in
## useful for excluding outliers or other data points
filter(diris, !Sepal.Length %in% interest)


## you can pass multiple arguments, which are equivalent to &
## can be helpful for breaking up code
filter(diris,
       Sepal.Length == 4.3 | Sepal.Length == 4.4,
       Petal.Width < .2)


#### Selecting Columns ####

## so far we've grabbed all columns

## can select multiple columns
## this is accomplished using the select() function
select(diris, Sepal.Length, Sepal.Width)

## select one column (still  a tibble)
select(diris, Sepal.Length)

## can select based on position
select(diris, 1, 5, 2)

## to select using character strings, somewhat more programmatic,
## use select_() noting the underscore
select_(diris, "Sepal.Length", "Sepal.Width")


## useful if you have a variable with variable names or positions
v <- c("Sepal.Length", "Sepal.Width")
select_(diris, v)

select_(diris, .dots = v)


## or use the function one_of()
v <- c("Sepal.Length", "Sepal.Width")
select(diris, one_of(v))

## if using tibbles, other more standard approaches also work
## however, these are not guaranteed to translate as smoothly throughout
## the dplyr experience
diris[, v]
diris[, 1]

## extract a column as a vector (each of the below only works for a single column)
head(diris[["Sepal.Length"]]) # easy if you have a R variable
head(diris[[v]]) # easy if you have a R variable
head(diris$Sepal.Length) # easy to type

## extract all except column(s)
select(diris, -Sepal.Length)
select(diris, -1)
select(diris, -Sepal.Length, -Petal.Width)

ex <- paste0("-", v)
select_(diris, ex)

## or use one_of() again
select(diris, -one_of(v))


## there are also some convenience fucntions to help choose variables
## without typing them all out
## these include:
starts_with()
ends_with()
contains()
matches() ## uses regular expressions
## they all default to looking in the current variable list
## and ignore case by default, although this can be turned off
select(diris, starts_with("s"))
select(diris, starts_with("s", ignore.case = FALSE))

################################################################################
##                                                                            ##
##                       Variable Renaming and Ordering                       ##
##                                                                            ##
################################################################################

## the variable names can be accessed as usual
names(diris)
colnames(diris)

## to avoid copying (and thus being memory inefficient)
## data.table changes variable names by reference using rename()

## data, new name = old name
diris <- rename(diris, SepalLength = Sepal.Length)
names(diris)

## you can reorder the variables in a dataset using select()
diris <- select(diris,
                SepalLength, Petal.Length, Sepal.Width, Petal.Width, Species)
diris

## can re-order by column number as well
diris <- select(diris, c(5, 1, 3, 2, 4))
diris

## for either names or number positions can reorder by an R object
v <- c(1, 3, 2, 4, 5)
## use select_() and pass the multiple numbers as a list to the .dots argument
diris <- select_(diris, .dots = as.list(v))
diris

#### TODO ####
################################################################################
##                                                                            ##
##                  Computing on Data and Creating Variables                  ##
##                                                                            ##
################################################################################

## re-create data.table based on original iris data
diris <- as_tibble(iris)

## create a new variable using the mutate function
#diris <- mutate(diris, V0 = 0)
#diris

## "delete" a variable by selecting all columns but that one
#diris <- select(diris, -Sepal.Length)
#diris

## create multiple variables as separate arguments in mutate()
diris <- mutate(diris, V0 = 0, X1 = 1L, X2 = 2L)
diris

## remove excess columns
diris <- select(diris, -V0, -X1, - X2)

## using mutate() can easily compute using variables from the dataset
diris <- mutate(diris, V = Petal.Length * Petal.Width)
diris

## assign V for only rows where the key (Species) is "setosa"
## we use if_else() from dplyr
diris <- mutate(diris, V2 = if_else(Species == "setosa",
                                   Petal.Length * Petal.Width, NA_real_))

## in addition to creating a variable
## you can also change a variable
diris <- mutate(diris, V2 = if_else(Species == "virginica",
                                   sqrt(Petal.Length * Petal.Width), V2))

## highlight just a few rows
slice(diris, c(1, 51, 101))

## you can also compute without creating a variable using summarise
summarise(diris, mean(Sepal.Width))

## computing in summarise allows multiple operations
summarise(diris,
          M = mean(Sepal.Width),
          SD = sd(Sepal.Width))

## computing can be done on subsets
## to do this, we will chain together several functions
## this could be accomplished through nested functions
## or a more readable format is using the %>% operator
diris %>%
  filter(Species == "virginica") %>%
  summarise(M = mean(Sepal.Width))

## computing can also be done by another variable
## to do this, we use the group_by() function
diris %>%
  group_by(Species) %>%
  summarise(M = mean(Sepal.Width))

## can make multiple variables during computing
diris %>%
  group_by(Species) %>%
  summarise(
    M1 = mean(Sepal.Width),
    M2 = mean(Petal.Width))


## another useful thing is that operations can be chained together
##  indefinitely
diris %>%
  group_by(Species) %>%
  summarise(
    M1 = mean(Sepal.Width),
    M2 = mean(Petal.Width)) %>%
  summarise(r = cor(M1, M2))


#diris <- as_tibble(iris)
## variables can be created by a variable as well
## create new variable that is whether a petal width
## is greater or smaller than the median petal width
## *for that species*
diris <- diris %>%
  group_by(Species) %>%
  mutate(MedPW = Petal.Width > median(Petal.Width))
diris

## we can group by multiple variables easily
diris %>%
  group_by(Species, MedPW) %>%
  summarise(
    M1 = mean(Sepal.Width),
    M2 = mean(Petal.Width))

## and continuing the operation chain...
diris %>%
  group_by(Species, MedPW) %>%
  summarise(
    M1 = mean(Sepal.Width),
    M2 = mean(Petal.Width)) %>%
  group_by(MedPW) %>%
  summarise(r = cor(M1, M2))

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


## first make two variable data.table
## with 3 observations per species
## also keys the data using keyby instead of just by
diris <- diris %>%
  group_by(Species) %>%
  select(Species, Sepal.Width) %>%
  slice(1:3)

## make dataset with only 1 observation per sepcies
diris2 <- slice(diris, c(1, 4, 7))

## make another tibble
## that contains information about each species
## with repeated species
dalt1 <- tibble(
  Species = c("setosa", "setosa", "versicolor", "versicolor",
              "virginica", "virginica", "other", "other"),
  Type = c("wide", "wide", "wide", "wide",
           "narrow", "narrow", "moderate", "moderate"),
  MedPW = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))


## make a tibble with only unique species (no repeats)
dalt2 <- slice(dalt1, c(1, 3))


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

##joins
left_join(diris2,dalt2)
right_join(diris2,dalt2)
inner_join(diris2, dalt2)
anti_join(diris2,dalt2)




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
merge(diris, dalt1)

## since there are multiple matches, it makes all possible combinations
## keeping all cases/rows from both datasets
merge(diris, dalt1, all = TRUE)





## can also merge by multiple columns
d2key1 <- tibble(
  ID1 = c(1, 1, 2, 2),
  ID2 = c(1, 2, 1, 2),
  X = letters[1:4])

d2key2 <- tibble(
  ID1 = c(1, 1, 2, 2),
  ID2 = c(1, 2, 1, 2),
  Y = LETTERS[1:4])

d2key1
d2key2

## made all combinations of each ID1
## also appends .x and .y to indicate ID2 comes from the x or y dataset,
## because the same variable exists in both datasets
merge(d2key1, d2key2, by = "ID1")


## but this is not really a many to many merge
## really one to one merge, but with a multi column key
## now get a simple one-to-one merge because of the proper keying
## in most cases, apparently "many to many" merges can be made one to one
## through encofrcement of a unique key
##(e.g., if you have timepoints within subjects
## than you may make a key based on both subject ID and time,
##so merging matches the appropriate
## time point within the appropriate subject
merge(d2key1, d2key2, by = c("ID1", "ID2"))


##### Reshaping Long #####

## reshaping generally works better if there is a unique ID per row
diris <- as_tibble(iris)

## in dplyr n() is a special way to refer to the number of rows
## in the data.table, so 1:.N creates integers from 1 to number of rows
diris <- mutate(diris, ID = 1:n())
diris

# to reshape long we will use the reshape() function from base R
# varying are repeatedly measured variables
# or otherwise variables you wish to stack in the long dataset
# the timevar is the name of the variable that will indicate
# which stack a row belongs to
# v.names can specify the names for the newly stacked variables
# and idvar is the ID variable
# note any variables not listed in varying will be assumed to be not varying
# and will simply be repeated
diris.long <- as_tibble(reshape(as.data.frame(diris),
  varying = list(
    c("Sepal.Length", "Sepal.Width"),
    c("Petal.Length", "Petal.Width")),
  timevar = "Type",
  v.names = c("Sepal", "Petal"),
  idvar = c("ID"),
  direction = "long"))

diris.long

## recode Type to be clearer
diris.long <- mutate(diris.long, Type = factor(Type, levels = 1:2,
                            labels = c("Length", "Width")))
diris.long


##### Reshaping Wide #####
diris.wide2 <- as_tibble(reshape(as.data.frame(diris.long),
                                 v.names = c("Sepal", "Petal"),
                                 timevar = "Type",
                                 idvar = "ID",
                                 ids = diris.long$ID,
                                 direction = "wide"))
diris.wide2


## note that reshape() is part of base R and not a very dplyr way of doing things
## however, it is considerably more powerful and versatile.  Hence we decided to show case
## how to use it rather than more dplyr type approaches
