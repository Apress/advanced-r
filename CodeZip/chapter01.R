library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(Hmisc)
library(foreign)
library(xlsx)
options(width = 70) # only 70 characters per line


########################################################################
##                                                                    ##
##            Programming Basics                                      ##
##                                                                    ##
########################################################################



## Classes of R objects

TRUE ## logical
FALSE ## logical
42L ## integer
1.5 ## double numeric
2+3i ## complex number
"a" ## character


## Missing values are special
## NA represnts a missing value
## but can make different classes of missing
NA ## logical
NA_integer_ ## integer
NA_real_ ## double / numeric
NA_character_ ## character
NA_complex_ ## complex

## factors are a special kind of object
## not so useful for general programming, but used a lot
## for statistics.  A factor variable indicates that a variable
## should be treated discretely.  Factors are stored as integers
## with labels to indicate the original value
factor(1:3)
factor(c("a", "b", "c"))
factor(letters[1:3])


## Data structures

## vector
c(1, 2, 3)

## scalar is just a vector of length one
c(1)
## matrix is a vector with dimensions
matrix(c(1:6), nrow = 3, ncol = 2)

## vectors and matrices can only have one type of data (e.g., integer, logical, etc.)

## list is a vector of objects
## lists can have different type of objects in each element
list(
  c("a"),
  c(1, 2, 3),
  matrix(c(1:6), nrow = 3, ncol = 2)
  )

## data frames are special type of lists
## where each element of the list is identical in length
data.frame(
  1:3,
  4:6)

## using non equal length objects causes problems
data.frame(
  1:3,
  4:5)

data.frame( 1:3, letters[1:3])
## due to far superior speed, we will make use of
## data table objects in R, from the data.table package
## data tables are very similar to data frames, but are designed to
## be more memory efficient and faster.  We will show some things with data frames
## as well as data tables because although we recommend data tables,
## to work with R many other people's code will include data frames, and indeed
## data tables inherit many methods from data frames
data.table( 1:3, 4:6)

#####################################################
## base operators and functions

#### assignment operator
x <- 5
## assignment can also be done using =
y = 3
x
y
is.integer(x)
is.double(y)
is.vector(x)


## but = are used within functions to set arguments,
## which are not assigned objects in the same way
## so generally recommend using the more "formal" <-

#### Accessing

## once an object is assigned, we can access particular
## elements using brackets.  Note R starts indexing at 1.

x <- c("a", "b", "c")
x[1]
is.vector(x)
is.vector(x[1])
is.character(x[1])

## for matrices we use two indices, one for rows, second for columns
## blank grabs all
x2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
x2 ## print to see full matrix
x2[1, 2] ## row 1, column 2
x2[1, ] ## all row 1
x2[, 1] ## all column 1

## can also grab several at once
x2[c(1, 2), ] ## rows 1 and 2
x2[c(1, 3), ] ## rows 1 and 3

## can drop one element using negative values
x[-2] ## drop element two
x2[, -2] ## drop column two
x2[-1, ] ## drop row 1


is.vector(x2)
is.matrix(x2)


## for lists using a single bracket
## returns a list with one element
y <- list(
  c("a"),
  c(1:3))

y[1]
is.vector(y[1])
is.list(y[1])
is.character(y[1])

## using double bracket returns the object within that
## element of the list, nothing more
y[[1]]
is.vector(y[[1]])
is.list(y[[1]])
is.character(y[[1]])


## can chain brackets togeter
y[[2]][3] ## second element of the list, third element of the vector

## brackets almost always work, but depending on the type of object
## there may be some additional ways to access components
## named data frames and lists can use $
x3 <- data.frame(
  A = 1:3,
  B = 4:6)

y2 <- list(
  C = c("a"),
  D = c(1, 2, 3))

x3$A
y2$C

is.list(x3)
is.list(y2)
is.matrix(x3)
is.matrix(y2)

## these are equivalent to

x3[["A"]]
y2[["C"]]


## because of their special nature (i.e., all elements equal length)
## data frames and data tables can be indexed similarly to matrices
x3[1, 1]
x3[1, ]
x3[, 1]

## Generally any named object can be indexed using the names
## rather than the positional numbers
x3[1, "A"]
x3[, "A"]

## and this is true for both column and row names
rownames(x3) <- c("first", "second", "third")

x3["second", "B"]

## data tables use a slightly different approach
## selecting rows works almost identically
## but selecting columns does not require quotes
x4 <- data.table(
  A = 1:3,
  B = 4:6)

x4[1, ]
x4[, A]
x4[1, A]

## select multiple by name without quotes using .()
x4[1:2, .(A, B)]

## if you need to use quotes, use the option with = FALSE
x4[1, "A", with = FALSE]

## although not generally used, these accessing operators
## are actually functions.  To use as a regular function, need special
## ` notation
`[`(x, 1)
`[`(x3, "second", "A")

## a similar approach works for double brackets
`[[`(y, 2)

######### Checking data types and type coercion
## generally, you can check whether an object is of a particular class
## using the is.classtype() function

is.integer(x)
is.numeric(x)
is.character(x)
is.logical(x)
is.complex(x)
is.atomic(x)
is.factor(x)
is.data.frame(x3)
is.matrix(x2)
is.list(y)

## you can also specially check whether a value is missing
## this is NOT done using regular logical comparions
NA == NA ## does not work
is.na(NA) ## works

## a general, flexible approach for other types of objects
## is to use the inherits() to test whether an object inherits
## from a class
inherits(x3, "data.frame")
inherits(x2, "matrix")

## inherits() is helpful when no is.class() function exists, which
## can occur for R packages that define specific classes outside of the core ones
## presented so far

## type coercion generally works similarly, but
## uses as.desiredtype()
as.integer(3.8)
as.character(3)
as.numeric(3)
as.complex(3)
as.factor(3)
as.matrix(3)
as.data.frame(3)
as.list(3)

## sometimes this may have unintended consequences
as.logical("a") ## NA no warning
as.logical(3) ## TRUE, no warning
as.numeric("a") ## NA with a warning



####################MATH################################
###### Comparisons and logicals
4 > 4
4 >= 4
4 < 4
4 <= 4
4 == 4
4 != 4

## note that due to representation error with floating point numbers
## you rarely want to do exact numeric comparions
## instead, often check that things are close within a tolerance
all.equal(1, 1.00000002, tolerance = .00001)

TRUE | FALSE
FALSE | TRUE
TRUE & TRUE
TRUE & FALSE
## also these are vectorized

1:3 >= 3:1
c(TRUE, TRUE) | c(TRUE, FALSE)
c(TRUE, TRUE) & c(TRUE, FALSE)

## for cases where you only want a single response
## such as for if else flow control
## can use && or ||, which stop evaluating after they confirm what it is
## for example
W
TRUE | W
## BUT
TRUE || W
W || TRUE
FALSE & W
FALSE && W

##also


## these are not vectorized and just use the first element
c(TRUE, TRUE) || c(TRUE, FALSE)
c(TRUE, TRUE) && c(TRUE, FALSE)

## two additional useful functions are
any(c(TRUE, FALSE, FALSE))
all(c(TRUE, FALSE, TRUE))
all(c(TRUE, TRUE, TRUE))


####################################################
## mathematical operators and functions
## there are many basic mathematical operators and functions available
3 + 3
3 - 3
3 * 3
3 / 3
(-27) ^ (1/3)
4 %/% .7
4 %% .3
## for details, see
?Arithmetic

sqrt(3)
abs(-3)
exp(1)
log(2.71)

cos(3.1415) ## cosine
## for more of these trig functions, see
?Trig

## matrix
## scalar operations
x2
x2 * 3
x2 + 3
## matrix multiplication
x2 %*% matrix(c(1, 1), 2)
## transpose
t(x2)
## cross product
t(x2) %*% x2
## easier cross product
crossprod(x2)

## transpose cross product
x2 %*% t(x2)
## easier transpose cross product
tcrossprod(x2)
